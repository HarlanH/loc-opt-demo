

library(knitr)
opts_chunk$set(fig.width=7, fig.height=5, echo=TRUE)

options(stringsAsFactors=FALSE)

library(ggplot2)
library(zipcode)
library(plyr)
library(compiler)
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(mapproj))
suppressPackageStartupMessages(library(DEoptim))



dat <- read.csv('DC2_Survey_Locations.csv')

dat <- subset(dat, Home_ZIP > 20000 & Home_ZIP < 30000 & 
                  (is.na(Work_ZIP) | (Work_ZIP > 20000 & Work_ZIP < 30000)))

dat <- mutate(dat,
              Home_ZIP = as.character(Home_ZIP),
              Work_ZIP = as.character(Work_ZIP))

names(dat) <- tolower(names(dat))

data(zipcode)
home_zip <- zipcode
names(home_zip) <- paste0("home_", names(home_zip))
work_zip <- zipcode
names(work_zip) <- paste0("work_", names(work_zip))

dat <- join(dat, home_zip, by='home_zip')
dat <- join(dat, work_zip, by='work_zip')

head(dat)




# get the most common work zip code
simple_mode <- names(which.max(table(dat$work_zip)))
# then look up in the previously-pulled list of work zip codes
simple_mode <- unlist(work_zip[work_zip$work_zip==simple_mode, 
                               c('work_latitude', 'work_longitude')])

# median's simpler...
simple_median <- c(median(dat$work_latitude, na.rm=TRUE),
                   median(dat$work_longitude, na.rm=TRUE))

# trim 10% from each edge of the work locaitons, then mean of that
simple_trim_mean <- c(mean(dat$work_latitude, na.rm=TRUE, trim=.1),
                      mean(dat$work_longitude, na.rm=TRUE, trim=.1))

sum_work_locs <- data.frame(type=c('Modal', 'Median', 'Trimmed Mean'),
                       lon=c(simple_mode[[2]],
                             simple_median[[2]],
                             simple_trim_mean[[2]]),
                       lat=c(simple_mode[[1]],
                             simple_median[[1]],
                             simple_trim_mean[[1]]))

print(sum_work_locs)



# this is slow, especially on a crowded network, so only pull the
# maps once... (knitr has caching, but can be a bit tricky to set up...)
maps_fn = "dc_maps.Rdata"
if (file.exists(maps_fn)) {
    load(maps_fn)
} else {
    # 9 is zoomed pretty far out; 13 is pretty far in
    dc.maps <- llply(9:13,
        function(z) get_map(location=c(lon=-77.034,lat=38.901), 
                             zoom=z, source='osm', color='bw'))
    # cache this in case the server's down...
    save(dc.maps, file=maps_fn)
}



# Plot the locations on a mostly-zoomed-in map
p <- ggmap(dc.maps[[4]], extent='device') + 
    geom_point(data=sum_work_locs, aes(x=lon, y=lat, color=type), size=6) +
    scale_color_brewer("Type", type='qual') +
    ggtitle("Work Location Simple Optima")
print(p)



#distance in kilometers between two long/lat positions (from "fossil" package)
earth.dist <- function (long1, lat1, long2, lat2) 
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
}
earth.dist.o <- cmpfun(earth.dist) # make it a bit faster



# df must have cols work_longitude and work latitude
# lon and lat must be scalars
# scale just scales the units
# x=2 is cartesian distance
p2p_cost <- function(df, lon, lat, scale=100,x=2) {
    sum((earth.dist.o(df$work_longitude, df$work_latitude, lon, lat)/scale)^x, na.rm=TRUE)
}

# How good or bad are recent locations, for optimizing vs people's work
# locations?
## Recent locations funger hall, microsoft and google
funger <- c(-77.048943, 38.899316)
microsoft <- c( -77.086387, 38.962345)
google <- c(-77.027485, 38.902138)

p2p_cost_funger <- p2p_cost(dat, funger[[1]], funger[[2]], scale=100)
p2p_cost_microsoft <- p2p_cost(dat, microsoft[[1]], microsoft[[2]], scale=100)
p2p_cost_google <- p2p_cost(dat, google[[1]], google[[2]], scale=100)

# map that
recent_locs <- data.frame(type=c('Funger', 'Microsoft', 'Google'),
                       lon=c(funger[[1]], microsoft[[1]], google[[1]]),
                       lat=c(funger[[2]], microsoft[[2]], google[[2]]),
                          cost=c(p2p_cost_funger, p2p_cost_microsoft,
                                 p2p_cost_google))
print(recent_locs)

p <- ggmap(dc.maps[[4]], extent='device') + 
    geom_point(data=recent_locs, aes(x=lon, y=lat, color=cost), size=6) +
    geom_text(data=recent_locs, aes(x=lon, y=lat, color=cost, label=type), size=6, vjust=-1) +
    scale_color_continuous("Cost", low='blue', high='red') +
    ggtitle("Single-Point Cost of Recent Locations")
print(p)



# p-norm computation. p = 2 is cartesian
dist <- function(a,b,c,d, p=2) {
    (abs(a-b)^p + abs(c-d)^p)^(1/p)
}
dist.o <- cmpfun(dist)

# distance from lon/lat to the df with four column names specified in cols
# as (end1_x, end1_y, end2_x, end2_y), where end2 may be missing.
# returns a vector of costs.
p2ls_cost_v <- function(df, cols, lon, lat, p=2, km_per_degree=69.11) {
    stopifnot(length(cols) == 4)
    stopifnot(all(cols %in% names(df)))
    stopifnot(length(lon) == 1)
    stopifnot(length(lat) == 1)
    
    #http://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
    
    # convert from lat/lon to flat coordinates, using the lat/lon ratio at the point in question
    param_x = lon * km_per_degree * cos(lat)
    param_y = lat * km_per_degree
    home_x = df[[cols[[1]]]] * km_per_degree * cos(lat); home_y = df[[cols[[2]]]] * km_per_degree
    work_x = df[[cols[[3]]]] * km_per_degree * cos(lat); work_y = df[[cols[[4]]]] * km_per_degree
    
    # first, get the home distances
    home_dists <- dist.o(home_x, param_x, home_y, param_y, p)
    
    # then, get the distances to the line segment, which may be NA
    # length of the segment (squared)
    l2 <- (home_x - work_x)^2 + (home_y - work_y)^2
    # position of closest point on line
    t <- (param_x - home_x) * (work_x - home_x) + 
         (param_y - home_y) * (work_y - home_y)
    t <- t / l2
    proj_x <- home_x + t * (work_x - home_x)
    proj_y <- home_y + t * (work_y - home_y)
    seg_dists<- ifelse(t < 0, 
                       home_dists,
                       ifelse(t > 1,
                              dist(work_x, param_x, work_y, param_y, p),
                              dist(proj_x, param_x, proj_y, param_y, p)))

    ifelse(is.na(seg_dists), home_dists, seg_dists)
}
p2ls_cost_v.o <- cmpfun(p2ls_cost_v)

# wrapper function for convenience
p2ls_cost <- function(df, lon, lat, p=2) {
    sum(p2ls_cost_v.o(df, c("home_longitude", "home_latitude", "work_longitude", "work_latitude"), lon, lat, p))
}



# non-DRY :(
p2ls_cost_funger <- p2ls_cost(dat, funger[[1]], funger[[2]])
p2ls_cost_microsoft <- p2ls_cost(dat, microsoft[[1]], microsoft[[2]])
p2ls_cost_google <- p2ls_cost(dat, google[[1]], google[[2]])

recent_locs_p2ls <- data.frame(type=c('Funger', 'Microsoft', 'Google'),
                       lon=c(funger[[1]], microsoft[[1]], google[[1]]),
                       lat=c(funger[[2]], microsoft[[2]], google[[2]]),
                          cost=c(p2ls_cost_funger, p2ls_cost_microsoft,
                                 p2ls_cost_google))

p <- ggmap(dc.maps[[4]], extent='device') + 
    geom_point(data=recent_locs_p2ls, aes(x=lon, y=lat, color=cost), size=6) +
    geom_text(data=recent_locs_p2ls, aes(x=lon, y=lat, color=cost, label=type), size=6, vjust=-1) +
    scale_color_continuous("Cost", low='blue', high='red') +
    ggtitle("Location-to-Commute Cost of Recent Locations")
print(p)

print(recent_locs_p2ls)




# global call to p2ls_cost()
make_map <- function(map) {
    bb = attr(map, "bb")
    grid <- expand.grid(lat=seq(from=bb$ll.lat, to=bb$ur.lat, length.out=41),
                        lon=seq(from=bb$ll.lon, to=bb$ur.lon, length.out=41))
    grid <- adply(grid, 1, 
                  function(rr) data.frame(cost=p2ls_cost(dat, rr$lon, rr$lat)))
    
    ggmap(map, extent='device') + 
        geom_tile(data=grid, aes(x=lon, y=lat, fill=cost, alpha=1/cost)) +
        scale_fill_gradientn("Cost", colours=rainbow(5)[5:1], trans='log') +
        scale_alpha_continuous("Cost", range=c(.1,.5)) +
        ggtitle("Distance from Commute") +
        theme(legend.position='none')
}
for (map in dc.maps[c(1,3,5)]) {
    p <- make_map(map) # handy to keep one around for later
    print(p)
}



# wrap for optim()
p2ls_cost_opt <- function(params) {
    p2ls_cost(dat, params[[1]], params[[2]])
}

starting_points = list(google, microsoft, funger)
# call optim, which defaults to Nelder-Mead/Simplex
one_loc <- llply(starting_points, 
                 function(sp) optim(sp, p2ls_cost_opt, 
                                    control=list(trace=1)))
print(laply(one_loc, function(ol) ol$par)) # all the same!

# reuse the last map...
single_p2ls_optim = list(x=one_loc[[1]]$par[[1]], 
                         y=one_loc[[1]]$par[[2]])
p + annotate('point', x=single_p2ls_optim$x, y=single_p2ls_optim$y, size=6, color='white')



colnames <- c("home_longitude", "home_latitude",
          "work_longitude", "work_latitude")
p2lsN_cost <- function(dat, latlons, p=2) {
    # for each pair of latlons, calc p2ls_cost
    costs <- laply(1:(length(latlons)/2), 
                   function(i) p2ls_cost_v.o(dat, 
                                               colnames, 
                                               latlons[[i*2-1]], latlons[[i*2]], 
                                               p))
    sum(aaply(costs, 2, min))
}
p2lsN_cost.o <- cmpfun(p2lsN_cost)
p2lsN_cost_opt <- function(latlons) {
    p2lsN_cost.o(dat, latlons)
}

# get cost for two example triples of locations
tri1_cost <- p2lsN_cost_opt(c(google, microsoft, funger))
tri2_cost <- p2lsN_cost_opt(c(google, microsoft, single_p2ls_optim))



# bit of a hack from the geom_polygon docs to construct polygons of different colors
ids <- factor(c(1,2))
values <- data.frame(id=ids, value=c(tri1_cost, tri2_cost))
positions <- data.frame(id = rep(ids, each=3),
                        x=c(google[[1]], microsoft[[1]], funger[[1]], google[[1]],
                            microsoft[[1]], single_p2ls_optim[[1]]),
                        y=c(google[[2]], microsoft[[2]], funger[[2]], google[[2]],
                            microsoft[[2]], single_p2ls_optim[[2]]))
datapoly <- merge(values, positions, by="id")

# blue is better than red...
make_map(dc.maps[[4]]) + 
    geom_polygon(data=datapoly, aes(x=x,y=y,color=value,group=id), alpha=.1,
                 size=2) +
    scale_color_continuous(low="blue", high="red")




# constrain the points to be not too far from DC
bigbox <- attr(dc.maps[[3]], "bb")
lower_box <- rep(c(bigbox$ll.lon, bigbox$ll.lat), 3)
upper_box <- rep(c(bigbox$ur.lon, bigbox$ur.lat), 3)




# this takes 5-10 minutes for 20 runs... find a random seed that lets us demo
# the problem faster...
set.seed(1) # gets one bad local optimum, and 3 variations of a good one
nruns=4

starting_points = rlply(nruns, function(i) unlist(sample(list(google, microsoft, funger), 3)) + rnorm(6, sd=.1))
three_loc <- llply(starting_points, 
                   function(sp) optim(sp, cmpfun(p2lsN_cost_opt), 
                                      method="L-BFGS-B",
                                      lower=lower_box, upper=upper_box,
                                      control=list(trace=1)))




three_loc_pars <- laply(three_loc, function(ol) ol$par)
print(three_loc_pars)

# turn these into a set of triangles and plot them
ids <- factor(1:nruns)
values <- data.frame(id=ids, value=laply(three_loc, function(x) x$value))
positions <- data.frame(id = rep(ids, each=3),
                        x=as.vector(t(three_loc_pars[,c(1,3,5)])),
                        y=as.vector(t(three_loc_pars[,c(2,4,6)])))
datapoly <- merge(values, positions, by="id")
make_map(dc.maps[[2]]) + 
    geom_polygon(data=datapoly, aes(x=x,y=y,color=value,group=id), alpha=.1, size=2) +
    scale_color_continuous(low="blue", high="red")



# Could use the historical points as starting points, but it turns out not to
# be a good enough location to matter much, so letting the algorithm pick
# random points in the bounding box instead.

# This takes a few minutes. 
# NP should be at least 60, itermax seems to converge around 100 or so,
# and strategy doesn't seem to make much difference for this decision
# surface.
three_loc_de <- DEoptim(cmpfun(p2lsN_cost_opt), lower=lower_box, upper=upper_box,
                     control=list(NP=100, itermax=300, trace=FALSE, 
                                  strategy=2, c=.5, VTR=362, reltol=.0001, steptol=25))
# get a copy of coffee...
# Note that not using parallel because running on a Mac in an R console...



# DEoptim bug makes plot.DEoptim not work right -- do it by hand
iter_values <- three_loc_de$member$bestvalit[1:three_loc_de$optim$iter]
qplot(seq_along(iter_values), iter_values)

# will show both the historical 3-location cost (Funger + Microsoft + Google)
# as well as the theoretical best 3-location solution
ids <- factor(c("Past", "Optimized"))
values <- data.frame(id=ids, value=c(tri1_cost, three_loc_de$optim$bestval))
positions <- data.frame(id = rep(ids, each=3),
                        x=c(google[[1]], microsoft[[1]], funger[[1]], three_loc_de$optim$bestmem[c(1,3,5)]),
                        y=c(google[[2]], microsoft[[2]], funger[[2]], three_loc_de$optim$bestmem[c(2, 4, 6)]))

datapoly <- merge(values, positions, by="id")
make_map(dc.maps[[3]]) + geom_polygon(data=datapoly, aes(x=x,y=y,color=value,group=id), alpha=.1, size=2) +
    scale_color_continuous(low="blue", high="red")

make_map(dc.maps[[4]]) + geom_point(data=datapoly, aes(x=x,y=y,color=value), size=5) +
    scale_color_continuous(low="blue", high="red")

make_map(dc.maps[[5]]) + geom_point(data=datapoly, aes(x=x,y=y,color=value), size=5) +
    scale_color_continuous(low="blue", high="red")

print(datapoly)


