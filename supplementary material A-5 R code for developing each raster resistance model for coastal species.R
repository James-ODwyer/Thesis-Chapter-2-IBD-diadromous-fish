#Supplementary material A-5 R code for developing each raster resistance model.

#An investigation of genetic connectivity shines light on the relative roles of isolation by distance and oceanic currents in three diadromous fish species


#netcdf files loading them
# ncparce requires a complete function to be read in in addition to the packages
# My data doesn't have speed
# Speed can be calculated from u velocity and v velocity together]
# direction can be calculated from u and v velocities as well
# Direction is given by the equation (180/3.14) * Atn2([u],[v])
# speed is given by the equation Sqr([u]^2 + [v]^2)


library(ncdf4)
library(rgeos)
library(raster)

library(chron)
library(RColorBrewer)
library(lattice)




# Let's get a map with it 

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

library('rnaturalearthhires')
world <- ne_countries(scale = "large", returnclass = "sf")

sitemap <- st_crop(world, world$geometry,xmin=140.2,xmax=150.2,ymin=-37.80, ymax=-42.00)
plot(sitemap$geometry)


# get map of Aus
library(devtools)
library(sf)
library(ggplot2)
library(rworldmap)
library(tmap) 
library(tmaptools)

#file_URL <- "C:/Users/18088076/Dropbox/PhD/Chapters/common galaxias/landscape information/IMOS_aggregation_20200723T002748Z/IMOS_aggregation_20200723T002748Z.nc"
file_URL <- "C:/Users/18088076/Dropbox/PhD/Chapters/common galaxias/landscape information/IMOS_aggregation_coastal_sp/IMOS_aggregation_20200903T004418Z.nc"
#file_URL <- "D:/Dropbox/PhD/Chapters/common galaxias/landscape information/IMOS_aggregation_coastal_sp/IMOS_aggregation_20200903T004418Z.nc"
file_URL <- "C:/Users/18088076/Dropbox/PhD/Chapters/common galaxias/landscape information/IMOS_aggregation_coastal_sp/IMOS_aggregation_20200903T004418Z.nc"

#run ncParse function before running this line (see bottom of page)
dataset <- ncParse(file_URL)
dataset$variables


lat <- dataset$dimensions$LATITUDE$data
lon <- dataset$dimensions$LONGITUDE$data
udata <- dataset$variables$UCUR$data
vdata <- dataset$variables$VCUR$data
date <- dataset$dimensions$TIME$data
speed <- sqrt((udata^2)+(vdata^2))
direction <- ((180/3.14) * atan2(udata,vdata))       # Oceanographic Convention: (180/3.14) * Atn2([u],[v]) 
# Direction is given as geographic based. e.g. north = 0 degrees, East 90, south 180/-180, West -90


# can now pull out any time period from each of the variables and create a separate raster for that time period
#e.g.
dat1 <-list() 
dat1$x <- dataset$dimensions$LONGITUDE$data
dat1$y <- dataset$dimensions$LATITUDE$data
dat1$z <- speed[,,5]  # select sea surface speed values for the 5th time value.
dat1$direction <- direction[,,5]

dat1$z <- speed[,,5]  # select sea surface speed values for the 5th time value.
dat1$direction <- direction[,,5]



### Now let's see the variance in the data. starting month to month with January
# What I need to do is get all the values for the same cell across each date 
# from there I will have vectors of values spanning time with which to calculate mean/variance
# can then plot each cells total points 

# Now to look at species specific times

# Tupong breed between late may to August
# Tupong stay at sea for approximately 4 months
# So they come back from late September to December
# Give an average spawning time of around 30th June and a return time of october 30th. push out to november 14th for 2 weeks
# want the average from 30/6/2016 to 14/11. the range of times is taken based roughly off (Bice et al. 2018; P. Zampatti et al. 2010)

# Now common galaxias 
# breed in Autumn and return 4-6 months later. 
# more specific than Autumn can't be found. They have a wide range of breeding. Mid April for breeding 
# end of September return 15/4/2016 - 30/9/2016.The range of times is taken based off
#(McDowall et al. 1994)



# Now Australian grayling
# Spawning varies with location but around Vic it's late Autumn (ranges from Summer to Winter) and varies based on water flow
# marine phase can be up to 6 months  and return around November.
# larvae take up to 3 weeks to drift downstream to ocean which is added to the late may count
# 14/6/2016 - 1/12/2016. The range of times is taken based off (Webb et al. 2018)



# Next steps. 
# Have average raster data for current and direction. 
# need to create cost matricies. 
# Absolute matrix will not work for currents which is because currents are unidirectional resistance matricies
# Solution 1. Create pop specific resistance matricies 
# Therefore will need resistance matrix * npops == 10 for CG, 7 for TP, 8 for AG.
# need to also decide whether to do least cost pathing or a more circuitscape based approach of average long pathing
# Whether or not they stick to the coast will impact the pathing choice to
# If they stick to the coast then an average cost isn't as approapriate 
# If they drift out into the strait then both the coast and deeper make more sense and therefore a average pathing is more approapriate
# This feels like a separate model all to itself 
# 
# Model plans:
#
# Model 1. IBD model. No current resistance. Larvae can go anywhere in water. 
# Model 2  IBD model. No current resistance but larvae stick to the coast and therefore further out is higher resistance
# Model 3. IBD model. No current resistance, larvae do not stick to the coast, instead preferring deeper water
# Model 4. IBR model. Current and direction resistance. Larvae can go anywhere in water
# Model 5. IBR model. Current and direction resistance. Larvae stick to the coast and therefore further out is higher resistance
# Model 6. IBR model. Current and direction resistance. Larvae do not stick to the coast, preferring deeper water

# Based on the models and the larvae average cost pathing seems like the best cost path method

# Models 1-3 are absolute resistance models so each will require only 1 raster each.
# Models 4-6 are relative resistance models so will require npop() resistance rasters per species
# Therefore in total across all models
# CG = approximately 30 rasters
# TP = approximately 24 rasters
# AG = approximately 27 rasters



# Start with Tupong
#30/6-14/11 

# fish out the dates of each year which are what I want

#2000
date[91]
date[160]
#2001
date[274]
date[342]
#2002
date[456]
date[525]
#2003
date[639]
date[707]
#2004
date[822]
date[890]
#2005
date[1004]
date[1073]
#2006
date[1187]
date[1255]
#2007
date[1369]
date[1438]
#2008
date[1552]
date[1621]
#2009
date[1735]
date[1803]
#2010
date[1917]
date[1986]
#2011
date[2190]
date[2327]
#2012
date[2556]
date[2693]
#2013
date[2921]
date[3058]
#2014
date[3286]
date[3423]
#2015
date[3651]
date[3788]
#2016
date[4017]
date[4145]

#

# double check it all worked
dim(speed)
speed[1,1,c(91:160, 274:342, 456:525, 639:707, 822:890, 1004:1073, 1187:1255, 1369:1438 , 1552:1621, 1735:1803, 1917:1986, 2190:2327, 
            2556:2693, 2921:3058, 2386:3423, 3651:3788, 4017:4145)]

# Tupong first
# generate mtrix for avg + var +sd to be stored
matrixavgsTP <- matrix(nrow = 63,ncol=26)
matrixvarsTP <- matrix(nrow = 63,ncol=26)
matrixsdTP <- matrix(nrow =63,ncol=26)



# loop through all the values per distinct XY cell in the raster to generate avg var and sd for the desired date ranges
for (j in (c(1:26))) {
  
  for ( i in (c(1:63))) {
    
    vectorvals <- speed[i,j,c(91:160, 274:342, 456:525, 639:707, 822:890, 1004:1073, 1187:1255, 1369:1438 , 1552:1621, 1735:1803, 1917:1986, 2190:2327, 
                              2556:2693, 2921:3058, 2386:3423, 3651:3788, 4017:4145)]
    
    matrixavgsTP[i,j] <- mean(vectorvals)
    matrixvarsTP[i,j] <- var(vectorvals)
    matrixsdTP[i,j] <- sd(vectorvals)
    
  }
  
  
}

rastercurrentavgTP <- raster( matrixavgsTP, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentavgTP <- t(rastercurrentavgTP)
rastercurrentavgTP2 <- flip(rastercurrentavgTP, 2)

par(mar = rep(3, 4))
plot(rastercurrentavgTP2)

rastercurrentvarTP <- raster( matrixvarsTP, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentvarTP <- t(rastercurrentvarTP)
rastercurrentvarTP2 <- flip(rastercurrentvarTP, 2)

par(mar = rep(3, 4))
plot(rastercurrentvarTP2)


rastercurrentsdTP <- raster( matrixsdTP, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentsdTP <- t(rastercurrentsdTP)
rastercurrentsdTP2 <- flip(rastercurrentsdTP, 2)

par(mar = rep(3, 4))
plot(rastercurrentsdTP2)


# Have now developed a series of current strength maps 
# Now direction





matrixdiravgsTP <- matrix(nrow = 63,ncol=26)
matrixdirvarsTP <- matrix(nrow = 63,ncol=26)
matrixdirsdTP <- matrix(nrow =63,ncol=26)




for (j in (c(1:26))) {
  
  for ( i in (c(1:63))) {
    
    vectorvals <- direction[i,j,c(91:160, 274:342, 456:525, 639:707, 822:890, 1004:1073, 1187:1255, 1369:1438 , 1552:1621, 1735:1803, 1917:1986, 2190:2327, 
                                       2556:2693, 2921:3058, 2386:3423, 3651:3788, 4017:4145)]
    
    
    matrixdiravgsTP[i,j] <- mean(vectorvals)
    matrixdirvarsTP[i,j] <- var(vectorvals)
    matrixdirsdTP[i,j] <- sd(vectorvals)
    
  }
  
  
}

rasterdirectionavgTP <- raster(matrixdiravgsTP, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionavgTP <- t(rasterdirectionavgTP)
rasterdirectionavgTP2 <- flip(rasterdirectionavgTP, 2)

par(mar = rep(3, 4))
plot(rasterdirectionavgTP2)

rasterdirectionvarTP <- raster( matrixdirvarsTP, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionvarTP <- t(rasterdirectionvarTP)
rasterdirectionvarTP2 <- flip(rasterdirectionvarTP, 2)

par(mar = rep(3, 4))
plot(rasterdirectionvarTP2)


rasterdirectionsdTP <- raster( matrixdirsdTP, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionsdTP <- t(rasterdirectionsdTP)
rasterdirectionsdTP2 <- flip(rasterdirectionsdTP, 2)

par(mar = rep(3, 4))
plot(rasterdirectionsdTP2)



# Model 1. IBD model. No current resistance. Larvae can go anywhere in water. 
#Current and direction are not valuble. Resistance raster = distance raster
setwd("C:/Users/18088076/Dropbox/PhD/Chapters/common galaxias/2017 samples DaRT")

studysite<- st_crop(world, world$geometry,xmin=139.0,xmax=151.4,ymin=-37.20, ymax=-42.20)
#studysite <- st_crop(world, c(xmin=140.2, ymin=-37.80, xmax=150.2, ymax=-42.00))
tupong_sites <- read.csv("Tupong_coords.csv")

colnames(tupong_sites) <- c("Pop", "Lat", "Lon")

TPpoints <- SpatialPoints( tupong_sites[,2:3])

par(mar = rep(2, 4))

plot(studysite$geometry)
plot(rastercurrentavgTP2, add=T)
plot(TPpoints, add=T)



#Model 1. All water= resistance 1 all land = resistance 100

rastercurrentavgTP2@data@values

rasterwatermodel1TP <- rastercurrentavgTP2

rasterwatermodel1TP@data@values[rasterwatermodel1TP@data@values <= 10] <- 1
rasterwatermodel1TP@data@values[is.na(rasterwatermodel1TP@data@values)] <- 5


plot(rasterwatermodel1TP)

library(gdistance)
tr <- transition( 1/rasterwatermodel1TP, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
TPpoints[1]

path.1 <- shortestPath( tr, TPpoints[4], TPpoints[6], output="SpatialLines")
plot( rasterwatermodel1TP , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( TPpoints,pch=16, col="red")

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist[1:7,1:7] 

#Success! Model 1 = worked! 

pwfsttable <- read.table("pwfstTP10000bootordered.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a LM of the data and use that to determine the AIC? 
# Do for each method?
model1TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model1TP)

summary(model1TP)

AIC(model1TP)
#-127.6346
cor(df$Genetic_Distance, df$Slope_Distance)
#0.7564
#P-value
#7.245e-05
#Rsquared 0.5722



# Model 2. Larvae stick to the coast. Resistance matrix needs to increase as distance moves from the coastline

# Problem. How do I measure the coastline. Can't use straight lat long as that is uneven
# Solution. 
#  create grid of central points across all the land of my study site area (use world map subset)
#  subset and make a new  grid to only include the intersection of my subset site area using st_intersection 
#  make a 3rd grid which is the difference between grid one and the union of grid 2/
#  Convert studysite  into multilinestrings 
# create distance between studysitemap and grid3 
# convert distance to dataframe
#  create extent for coordinates of grid
#  convert df to an sf object with extent
#  turn sf into raster


studysite2<- st_crop(world, world$geometry,xmin=138.0,xmax=152.4,ymin=-35.20, ymax=-42.20)

plot(studysite2$geometry)
?st_make_grid()
grid <- st_make_grid(st_as_sfc(st_bbox(studysite2)), cellsize = 0.1, what = "centers")
grid <- st_transform(grid, crs =crs(studysite2))


plot(grid)
plot(studysite2$geometry)

grid2 <- st_intersection(grid, studysite2$geometry)
grid2
plot(grid2)
grid3 <-  st_difference(grid, st_union(grid2))
class(grid3)
plot(grid3)

#plot(grid)
#plot(studysite$geometry, add=T)
#plot(rastercurrentavgTP2, add=T)
#plot(TPpoints, add=T)

studysite2 <- st_cast(studysite2, "MULTILINESTRING")
dist <- st_distance(studysite2, grid3)

class(grid3)
head(dist)

df <- data.frame(dist = as.vector(dist),
                 st_coordinates(grid3))

col_dist <- brewer.pal(11, "RdGy")
ggplot(df, aes(X, Y, fill = dist))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (degrees)")+ #legend name
  theme_void()+ #map theme
  theme(legend.position = "bottom")

crs(studysite2)

ext <- extent(as(grid3, "Spatial"))
r <- raster(resolution = res(rasterwatermodel1TP), ext = ext, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs, +datum=WGS84")

dist_sf <- st_as_sf(df, coords = c("X", "Y")) %>%
  st_set_crs(4326)

dist_raster <- rasterize(dist_sf, r, "dist", fun = mean)



plot(dist_raster)

#Raster works perfectly as of now
res(dist_raster)
res(rasterwatermodel1TP)


#1km as the range for value increases?

plot(rasterwatermodel1TP)
rasterwatermodel2TP <-rasterwatermodel1TP
dist_raster_reduced <- crop(dist_raster, extent(rasterwatermodel1TP), snap='near')
rasterwatermodel2TP <- crop(rasterwatermodel1TP, extent(dist_raster_reduced), snap='near')
dist_raster_reduced

plot(rasterwatermodel2TP)
plot(dist_raster_reduced, add=T)

# need to change all values of rasterwatermodel so that as distance increases (based on dist_raster) the new water model increases
# for loop

plot(studysite$geometry)
plot(dist_raster)

plot(dist_raster_reduced, add=T)
dist_raster_reduced@data@values
length(dist_raster_reduced@data@values)

rasterwatermodel2_2TP <- rasterwatermodel2TP
rasterwatermodel2TP@data@values
length(rasterwatermodel2_2TP@data@values)
#paramaters of loop to create new model resistance raster. 
#Nas of the distance raster = land. 
# if distance raster value= NA water model = 5*max
# if resistance raster value <1000 model =1 
# if resistance raster value < 2000 model = 2 etc

for (i in (c(1:length(rasterwatermodel2TP@data@values)))){
  

  
  if ((rasterwatermodel2TP@data@values[i] == 5)) {
    
    rasterwatermodel2_2TP@data@values[i] <- 300
    
  } else  {
    
    rasterwatermodel2_2TP@data@values[i] <- (dist_raster_reduced@data@values[i]/500)
    rasterwatermodel2_2TP@data@values[i] <- rasterwatermodel2_2TP@data@values[i]+10
  }
  
}

min(dist_raster_reduced@data@values)
min(rasterwatermodel2_2TP@data@values)
plot(rasterwatermodel2_2TP)



#success
# now to quantify


tr <- transition( 1/rasterwatermodel2_2TP, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
TPpoints[1]

path.1 <- shortestPath( tr, TPpoints[1], TPpoints[3], output="SpatialLines")
plot( rasterwatermodel2_2TP , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( TPpoints,pch=16, col="red")

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist[1:7,1:7] 


pwfsttable <- read.table("pwfstTP10000bootordered.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?


cor(df$Genetic_Distance, df$Slope_Distance)
#0.7522814
model2TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model2TP)
# Pvalue 8.334e-05
summary(model2TP)
#R squared 
#0.5661
AIC(model2TP)
#-127.6346









# Model 3. IBD model. No current resistance, larvae do not stick to the coast, instead preferring deeper water




rasterwatermodel3TP <-rasterwatermodel1TP
dist_raster_reduced <- crop(dist_raster, extent(rasterwatermodel1TP), snap='near')
rasterwatermodel3TP <- crop(rasterwatermodel1TP, extent(dist_raster_reduced), snap='near')
dist_raster_reduced
rasterwatermodel3_2TP <- rasterwatermodel3TP
rasterwatermodel3TP@data@values
length(rasterwatermodel3_2TP@data@values)

#paramaters of loop to create new model resistance raster. 
#Nas of the distance raster = land. 
# if distance raster value= NA water model = 9999999
# if resistance raster value <1000 model =1 
# if resistance raster value < 2000 model = 2 etc

for (i in (c(1:length(rasterwatermodel3TP@data@values)))){
  
  
  
  if ((rasterwatermodel3TP@data@values[i] == 5)) {
    
    rasterwatermodel3_2TP@data@values[i] <- 500
    
  } else  {
    
    rasterwatermodel3_2TP@data@values[i] <- (800000/dist_raster_reduced@data@values[i])
    rasterwatermodel3_2TP@data@values[i] <- rasterwatermodel3_2TP@data@values[i]+10
  }
  
}




min(dist_raster_reduced@data@values)
min(rasterwatermodel3_2TP@data@values)
plot(rasterwatermodel3_2TP)


for (i in (c(1:length(rasterwatermodel3TP@data@values)))){
  
  
  if (rasterwatermodel3_2TP@data@values[i] > 501) {
    
    rasterwatermodel3_2TP@data@values[i] =50
    
  }
}

plot(rasterwatermodel3_2TP)

#success Model 3 is complete. Now quantify


tr <- transition( 1/rasterwatermodel3_2TP, transitionFunction = mean, directions = 4)
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
TPpoints[1]

path.1 <- shortestPath( tr, TPpoints[1], TPpoints[3], output="SpatialLines")
plot( rasterwatermodel3_2TP , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( TPpoints,pch=16, col="red")

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist[1:7,1:7] 


pwfsttable <- read.table("pwfstTP10000bootordered.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?


cor(df$Genetic_Distance, df$Slope_Distance)
#0.4407
model3TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model3TP)
# Pvalue 0.04578
summary(model3TP)
#R squared 
#0.1938
AIC(model3TP)
#-114.3261






# Now model 4. 
# Model 4. IBR model. Current and direction resistance. Larvae can go anywhere in water

# This model will require the addition of currents and direction which means it will be location specific
# Initial thoughts ifelse commands based on relative XY of site to raster?


rasterwatermodel1TP
plot(rasterwatermodel1TP)

rastercurrentavgTP2
plot(rastercurrentavgTP2)

rasterdirectionavgTP2
plot(rasterdirectionavgTP2)
# change raste rto same size
rastercurrentavgTP4 <-rastercurrentavgTP2
dist_raster_reduced4 <- crop(dist_raster, extent(rastercurrentavgTP4), snap='near')
rastercurrentavgTP4 <- crop(rastercurrentavgTP2, extent(dist_raster_reduced), snap='near')

rasterdirectionavgTP4 <-rasterdirectionavgTP2
dist_raster_reduced4 <- crop(dist_raster, extent(rasterdirectionavgTP4), snap='near')
rasterdirectionavgTP4 <- crop(rasterdirectionavgTP2, extent(dist_raster_reduced), snap='near')

rasterwatermodel4TP <- rasterwatermodel1TP
dist_raster_reduced4 <- crop(dist_raster, extent(rasterwatermodel4TP), snap='near')
rasterwatermodel4TP <- crop(rasterwatermodel1TP, extent(dist_raster_reduced), snap='near')


xFromCell(rasterwatermodel4TP, (3))
rasterwatermodel4TP@data@attributes
View(rasterwatermodel4TP)


# Can mess with the variables so that current is the primary driver when strong vs not the primary driver. 

raster_list <- list()

for (j in(c(1:length(TPpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4TP@data@values)))) {
    
    #if the Lon coordinate of the site is to the left of the raster pixel
    
    if(TPpoints@coords[j,1] < xFromCell(rasterwatermodel4TP, (i))) {
      
      if(!is.na(rasterdirectionavgTP4@data@values[i])) {
        
        if (rasterdirectionavgTP4@data@values[i] > 45 & rasterdirectionavgTP4@data@values[i] < 135){
          
          rasterwatermodel4TP@data@values[i] <- (45 - 100*rastercurrentavgTP4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgTP4@data@values[i] > 135 & rasterdirectionavgTP4@data@values[i] < 180) {
          
          rasterwatermodel4TP@data@values[i] <- (45 + 50*rastercurrentavgTP4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgTP4@data@values[i] > 0 & rasterdirectionavgTP4@data@values[i] < 45) {
          
          rasterwatermodel4TP@data@values[i] <- (45 - 50*rastercurrentavgTP4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgTP4@data@values[i] > -135 & rasterdirectionavgTP4@data@values[i] < -45) {
          
          rasterwatermodel4TP@data@values[i] <- (45 + 100*rastercurrentavgTP4@data@values[i]) 
          
        }
        else if (rasterdirectionavgTP4@data@values[i] > -180 & rasterdirectionavgTP4@data@values[i] < -135) {
          
          rasterwatermodel4TP@data@values[i] <- (45 + 50*rastercurrentavgTP4@data@values[i]) 
          
        }
        else if (rasterdirectionavgTP4@data@values[i] > -45 & rasterdirectionavgTP4@data@values[i] < 0) {
          
          rasterwatermodel4TP@data@values[i] <- (45 - 50*rastercurrentavgTP4@data@values[i]) 
          
        }
        
        else {
          
          rasterwatermodel4TP@data@values[i] <- (45 + 100*rastercurrentavgTP4@data@values[i])
          
        }  
      } 
      else {
        
        rasterwatermodel4TP@data@values[i] <- 150 
        
      }
    }
    
    #if the Lon coordinate of the site is to the right of the raster pixel
    else if (TPpoints@coords[j,1] >= xFromCell(rasterwatermodel4TP, (i))) {
      
      if (!is.na(rasterdirectionavgTP4@data@values[i])) {
        if (rasterdirectionavgTP4@data@values[i] > 45 & rasterdirectionavgTP4@data@values[i] < 135){
          
          rasterwatermodel4TP@data@values[i] <- (45 + 100*rastercurrentavgTP4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgTP4@data@values[i] > 135 & rasterdirectionavgTP4@data@values[i] < 180) {
          
          rasterwatermodel4TP@data@values[i] <- (45 + 50*rastercurrentavgTP4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgTP4@data@values[i] > 0 & rasterdirectionavgTP4@data@values[i] < 45) {
          
          rasterwatermodel4TP@data@values[i] <- (45 - 50*rastercurrentavgTP4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgTP4@data@values[i] > -135 & rasterdirectionavgTP4@data@values[i] < -45) {
          
          rasterwatermodel4TP@data@values[i] <- (45 - 100*rastercurrentavgTP4@data@values[i]) 
          
        }
        else if (rasterdirectionavgTP4@data@values[i] > -180 & rasterdirectionavgTP4@data@values[i] < -135) {
          
          rasterwatermodel4TP@data@values[i] <- (45 + 50*rastercurrentavgTP4@data@values[i]) 
          
        }
        else if (rasterdirectionavgTP4@data@values[i] > -45 & rasterdirectionavgTP4@data@values[i] < 0) {
          
          rasterwatermodel4TP@data@values[i] <- (45 - 50*rastercurrentavgTP4@data@values[i]) 
          
        } 
      }
      else {
        
        rasterwatermodel4TP@data@values[i] <- 150
        
      }
    }
    
    # if NA
    else {
      rasterwatermodel4TP@data@values[i] <- 150
      
    }
    
  }

  raster_list[[j]] <- rasterwatermodel4TP
   
  
   
}

  rasterwatermodel4TPpop1 <- raster_list[[1]]
  rasterwatermodel4TPpop2 <- raster_list[[2]]
  rasterwatermodel4TPpop3 <- raster_list[[3]]
  rasterwatermodel4TPpop4 <- raster_list[[4]]
  rasterwatermodel4TPpop5 <- raster_list[[5]]
  rasterwatermodel4TPpop6 <- raster_list[[6]]
  rasterwatermodel4TPpop7 <- raster_list[[7]]
  plot(rasterwatermodel4TPpop6)
  
  plot(rastercurrentavgTP2)
  plot(rasterdirectionavgTP2)


  
  
# Now not sure how to best do this at this point

# Need to combine the ecological distances for each pop for each raster into a single e distance to
# use as the matrix to plot fst against
  # Can I have uneven dissimilarity matricies? The cost one way is different to the other way? 
  # Should the costs be averaged?
  # pairwise fst is by definition averaged.

  #pop1
  tr <- transition( 1/rasterwatermodel4TPpop1, transitionFunction = mean, directions = 4 )
  tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
  
  eDist <- costDistance( tr, TPpoints )
  eDist <- as.matrix( eDist )
  rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
  eDist[1:7,1:7] 
  
  eDist2 <- eDist
  eDist2[1,] <- eDist[1,]
  #pop2
  tr <- transition( 1/rasterwatermodel4TPpop2, transitionFunction = mean, directions = 4 )
  tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
  
  eDist <- costDistance( tr, TPpoints )
  eDist <- as.matrix( eDist )
  rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
  
  eDist2[2,] <- eDist[2,]
  #pop3
  tr <- transition( 1/rasterwatermodel4TPpop3, transitionFunction = mean, directions = 4 )
  tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
  
  eDist <- costDistance( tr, TPpoints )
  eDist <- as.matrix( eDist )
  rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
  eDist2[3,] <- eDist[3,]

  #pop4
  tr <- transition( 1/rasterwatermodel4TPpop4, transitionFunction = mean, directions = 4 )
  tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
  
  eDist <- costDistance( tr, TPpoints )
  eDist <- as.matrix( eDist )
  rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
  eDist2[4,] <- eDist[4,]
  
#pop5
  tr <- transition( 1/rasterwatermodel4TPpop5, transitionFunction = mean, directions = 4 )
  tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
  
  eDist <- costDistance( tr, TPpoints )
  eDist <- as.matrix( eDist )
  rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
  eDist2[5,] <- eDist[5,]
  
  #pop6
  tr <- transition( 1/rasterwatermodel4TPpop6, transitionFunction = mean, directions = 4 )
  tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
  
  eDist <- costDistance( tr, TPpoints )
  eDist <- as.matrix( eDist )
  rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
  eDist2[6,] <- eDist[6,]
  
  #pop7 
  tr <- transition( 1/rasterwatermodel4TPpop7, transitionFunction = mean, directions = 4 )
  tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
  
  eDist <- costDistance( tr, TPpoints )
  eDist <- as.matrix( eDist )
  rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
  eDist2[7,] <- eDist[7,]
  
  
  # done 
  eDist2
  
  
  #Now to run the correlation 


  
vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
  vector3 <- vector()
  eDist3 <- eDist2
  for (j in(c (1:length(vector1)))) {
    vector1 <- eDist2[j,]
    vector2 <- eDist2[,j]
    
  for( i in c(1:length(vector1))) {
  vector3[i] <- (vector1[i]+vector2[i])/2
   
  }
  eDist3[j,] <- vector3
  }  
    
    #Fst table.
  pwfsttable <- read.table("pwfstTP10000bootordered.txt",header=TRUE)
  rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")
  
  df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                    Slope_Distance = eDist3[ lower.tri(eDist3)])
  df <- df[ !is.infinite(df$Slope_Distance),]
  
  ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
    stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")
  
  cor(df$Genetic_Distance, df$Slope_Distance)
  # 0.7355285
model4TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model4TP)
# Pvalue 0.000145
summary(model4TP)
#R squared 
#0.541
AIC(model4TP)
#-126.1542



  
  
  #relative migration rates
  pwfsttable <- read.table("divmigrateTPordered2.txt",header=TRUE)
  rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")
  
  df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                    Slope_Distance = eDist2[ lower.tri(eDist2)])
  df <- df[ !is.infinite(df$Slope_Distance),]
  df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                     Slope_Distance = eDist2[ upper.tri(eDist2)])
  df2 <- df[ !is.infinite(df$Slope_Distance),]
  df3 <- (rbind(df,df2))
  ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
    stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")
  cor(df$Genetic_Distance, df$Slope_Distance)
#0.03319099
model4_2TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model4_2TP)
# Pvalue 0.8864
summary(model4_2TP)
#R squared 
#0.001102
AIC(model4_2TP)
#5.073055

  
  
  # Run a GLM of the data and use that to determine the AIC? 
  # Do for each method?
  
  
  #Fst 0.7355
  # Migration rates -0.03319099

  
  
  
  
  
  # Now Model 5. 
  #Model 5. IBR model. Current and direction resistance. Larvae stick to the coast and therefore further out is higher resistance
  # So the model 4 rasters are pretty good. Can I just use them and apply a new distance raster/ factor based on model 2?
  
  rasterwatermodel2_2TP
  
  rasterwatermodel2_2TP@data@values
  
  plot(rasterwatermodel2_2TP)
  rasterwatermodel4TPpop1@data@values
 raster_listmodel5 <- raster_list
  
  for (j in(c(1:length(TPpoints@coords[,1])))) {
    for (i in (c(1:length(rasterwatermodel4TP@data@values)))) {
      
      raster_listmodel5[[j]]@data@values[i] <- (raster_list[[j]]@data@values[i] + (0.2*rasterwatermodel2_2TP@data@values[i])) 
      
      if (raster_listmodel5[[j]]@data@values[i] > 200) {
        
        raster_listmodel5[[j]]@data@values[i] <- 200
      }
      
      
      else {
        raster_listmodel5[[j]]@data@values[i] <- raster_listmodel5[[j]]@data@values[i]
        
      }
    }
    
  }


 
 rasterwatermodel5TPpop1 <- raster_listmodel5[[1]]
 rasterwatermodel5TPpop2 <- raster_listmodel5[[2]]
 rasterwatermodel5TPpop3 <- raster_listmodel5[[3]]
 rasterwatermodel5TPpop4 <- raster_listmodel5[[4]]
 rasterwatermodel5TPpop5 <- raster_listmodel5[[5]]
 rasterwatermodel5TPpop6 <- raster_listmodel5[[6]]
 rasterwatermodel5TPpop7 <- raster_listmodel5[[7]]
 
plot(rasterwatermodel4TPpop1)
plot(rasterwatermodel5TPpop1)
plot(rasterwatermodel2_2TP)
rasterwatermodel4TPpop7@data@values
rasterwatermodel5TPpop7@data@values
 
#pop1
tr <- transition( 1/rasterwatermodel5TPpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel5TPpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel5TPpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel5TPpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel5TPpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel5TPpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel5TPpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 


# Currently this just brings back the lower triangle but that is not correct as directional flow is assymetrical 


#try average. 

vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstTP10000bootordered.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.750512

model5TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model5TP)
# Pvalue 8.885e-05
summary(model5TP)
#R squared 
#0.5633
AIC(model5TP)
#-127.1984


#relative migration rates
pwfsttable <- read.table("divmigrateTPordered.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")
cor(df$Genetic_Distance, df$Slope_Distance)

model5_2TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model5_2TP)
# Pvalue 0.8959
summary(model5_2TP)
#R squared 
#0.0009244
AIC(model5_2TP)
#5.076781


# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?



 
 
# Now Model 6. 
## Model 6. IBR model. Current and direction resistance. Larvae do not stick to the coast, preferring deeper water
# So the model 4 rasters are pretty good. Can I just use them and apply a new distance raster/ factor based on model 3?


rasterwatermodel3_2TP

rasterwatermodel3_2TP@data@values

plot(rasterwatermodel3_2TP)
rasterwatermodel4TPpop1@data@values
raster_listmodel6 <- raster_list

for (j in(c(1:length(TPpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4TP@data@values)))) {
    
    raster_listmodel6[[j]]@data@values[i] <- (raster_list[[j]]@data@values[i] + (0.5*rasterwatermodel3_2TP@data@values[i])) 
    
    if (raster_listmodel6[[j]]@data@values[i] == 400) {
      
      raster_listmodel6[[j]]@data@values[i] <- 200
    }
    
    else if (raster_listmodel5[[j]]@data@values[i] > 600) {
      
      raster_listmodel6[[j]]@data@values[i] <- 150
    }
    
    
    else {
      raster_listmodel6[[j]]@data@values[i] <- raster_listmodel6[[j]]@data@values[i]
      
    }
  }
  
}



rasterwatermodel6TPpop1 <- raster_listmodel6[[1]]
rasterwatermodel6TPpop2 <- raster_listmodel6[[2]]
rasterwatermodel6TPpop3 <- raster_listmodel6[[3]]
rasterwatermodel6TPpop4 <- raster_listmodel6[[4]]
rasterwatermodel6TPpop5 <- raster_listmodel6[[5]]
rasterwatermodel6TPpop6 <- raster_listmodel6[[6]]
rasterwatermodel6TPpop7 <- raster_listmodel6[[7]]

 
 

plot(rasterwatermodel4TPpop1)
plot(rasterwatermodel6TPpop1)
plot(rasterwatermodel3_2TP)
rasterwatermodel4TPpop7@data@values
rasterwatermodel6TPpop7@data@values

#pop1
tr <- transition( 1/rasterwatermodel6TPpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel6TPpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel6TPpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel6TPpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel6TPpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel6TPpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel6TPpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, TPpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 
pwfsttable <- read.table("pwfstTP10000bootordered.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

# Currently this just brings back the lower triangle but that is not correct as directional flow is assymetrical 


#try average. 

vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstTP10000bootordered.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.7019404
model6TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model6TP)
# Pvalue 0.00039
summary(model6TP)
#R squared 
#0.4927
AIC(model6TP)
#-124.0538

#relative migration rates
pwfsttable <- read.table("divmigrateTPordered2.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")
cor(df$Genetic_Distance, df$Slope_Distance)
#0.03674232
model6_2TP <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model6_2TP)
# Pvalue 0.8744
summary(model6_2TP)
#R squared 
#0.00135
AIC(model6_2TP)
#5.067833


 
#Model 1
# Correlation =0.7564
# P-value = 7.245e-05
# Rsquared = 0.5722
# AIC = -127.6346

#Model 2 
# Correlation =0.7523974
# P-value = 8.334e-05
# Rsquared = 0.5661
# AIC = -127.3351


# Model 3
# Correlation = 0.4407
# P-value = 0.04578
# Rsquared = 0.1938
# AIC = -114.3261


# Model 4
# Fst
# Correlation = 0.7355
# P-value = 0.000145
# Rsquared = 0.541
# AIC -126.1542
# MIGRATION 
# Correlation = 0.03319099
# P-value = 8.885e-05
# Rsquared = 0.001102
# AIC 5.073055

# Model 5
# Fst
# Correlation = 0.7551547
# P-value = 0.0001251
# Rsquared = 0.5633
# AIC -127.1984

#Migration
# Correlation = 0.02384243
# P-value = 0.9183
# Rsquared = 0.0005685
# AIC 5.084261

#Model 6 
# Fst
# Correlation = 0.7019404
# P-value = 0.00039
# Rsquared = 0.4927
# AIC -124.0538

#Migration
# Correlation = 0.03674232
# P-value = 0.8744
# Rsquared = 0.00135
# AIC 5.067833


# There isn't really much of a difference between the 6 models tested, with th exception of 2 and 6 which were lower
# suggesting that the larvae do not preferentially move far from the coast
#All (except 2) yielded greater correlations than that seen in a straight Mantel IBD test.
#Tupong appear to be primarily structured via distance 

# There is still the issue with assymetrical differences between sites being averaged out. I really need an assymetrical measure
# of gene flow to use for my correlation with the resistance matrix.
# An alternative is to sub out the Fst values obtained and replace them with a form of relative migration rates?
# All I have in this regard is the Bayessass which isn't the best. 
# Can look into measures from divMigrate to
# If this works I can substitute these relative migration rates in and
# 1. see if a full triangle works instead of lower triangle correllation I have been doing
# or 2. Duplicate the rows with new pops being "river_2" to signify the other direction of flow
# then sub the correct values in and then run a correllation with 2X points 


# Other points. Nm? or D? What is the optimum solution to measure migration rates?

# After doing exactly that, I have shown that there doesn't appear to be strong assymmetrical patterns of gene flow. No sig diffs between
# migrating pops were detected with divMigrate, and when substituting the migration rates found instead of Fst values
# we see no correlation with increasing raster resistance driven by oceanic conditions.








# Now lets do Australian Grayling


##########
##########

##### 
# Spawning varies with location but around Vic it's late Autumn (ranges from Summer to Winter) and varies based on water flow
# marine phase can be up to 6 months  and return around November.
# larvae take up to 3 weeks to drift downstream to ocean which is added to the late may count
# 14/6/2016 - 1/12/2016



#2000
date[84]
date[169]
#2001
date[266]
date[351]
#2002
date[449]
date[534]
#2003
date[631]
date[716]
#2004
date[814]
date[899]
#2005
date[996]
date[1082]
#2006
date[1179]
date[1264]
#2007
date[1361]
date[1447]
#2008
date[1545]
date[1630]
#2009
date[1727]
date[1812]
#2010
date[1910]
date[1994]
#2011
date[2174]
date[2344]
#2012
date[2527]
date[2710]
#2013
date[2905]
date[3075]
#2014
date[3270]
date[3440]
#2015
date[3635]
date[3805]
#2016
date[4001]
date[4162]

#
dim(speed)
speed[1,1,c(84:169, 266:351, 449:534, 631:716, 814:899, 996:1082, 1179:1264, 1361:1447 , 1545:1630, 1727:1812, 1910:1994, 2174:2344, 
            2527:2710, 2905:3075, 3270:3423, 3635:3805, 4001:4162)]

# AG 

matrixavgsAG <- matrix(nrow = 63,ncol=26)
matrixvarsAG <- matrix(nrow = 63,ncol=26)
matrixsdAG <- matrix(nrow =63,ncol=26)




for (j in (c(1:26))) {
  
  for ( i in (c(1:63))) {
    
    vectorvals <- speed[i,j,c(84:169, 266:351, 449:534, 631:716, 814:899, 996:1082, 1179:1264, 1361:1447 , 1545:1630, 1727:1812, 1910:1994, 2174:2344, 
                              2527:2710, 2905:3075, 3270:3423, 3635:3805, 4001:4162)]
    
    matrixavgsAG[i,j] <- mean(vectorvals)
    matrixvarsAG[i,j] <- var(vectorvals)
    matrixsdAG[i,j] <- sd(vectorvals)
    
  }
  
  
}

rastercurrentavgAG <- raster( matrixavgsAG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentavgAG <- t(rastercurrentavgAG)
rastercurrentavgAG2 <- flip(rastercurrentavgAG, 2)

par(mar = rep(3, 4))
plot(rastercurrentavgAG2)

rastercurrentvarAG <- raster( matrixvarsAG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentvarAG <- t(rastercurrentvarAG)
rastercurrentvarAG2 <- flip(rastercurrentvarAG, 2)

par(mar = rep(3, 4))
plot(rastercurrentvarAG2)


rastercurrentsdAG <- raster( matrixsdAG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentsdAG <- t(rastercurrentsdAG)
rastercurrentsdAG2 <- flip(rastercurrentsdAG, 2)

par(mar = rep(3, 4))
plot(rastercurrentsdAG2)


#
# Now direction





matrixdiravgsAG <- matrix(nrow = 63,ncol=26)
matrixdirvarsAG <- matrix(nrow = 63,ncol=26)
matrixdirsdAG <- matrix(nrow =63,ncol=26)




for (j in (c(1:26))) {
  
  for ( i in (c(1:63))) {
    
    vectorvals <- direction[i,j,c(84:169, 266:351, 449:534, 631:716, 814:899, 996:1082, 1179:1264, 1361:1447 , 1545:1630, 1727:1812, 1910:1994, 2174:2344, 
                                  2527:2710, 2905:3075, 3270:3423, 3635:3805, 4001:4162)]
    
    
    matrixdiravgsAG[i,j] <- mean(vectorvals)
    matrixdirvarsAG[i,j] <- var(vectorvals)
    matrixdirsdAG[i,j] <- sd(vectorvals)
    
  }
  
  
}

rasterdirectionavgAG <- raster(matrixdiravgsAG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionavgAG <- t(rasterdirectionavgAG)
rasterdirectionavgAG2 <- flip(rasterdirectionavgAG, 2)

par(mar = rep(3, 4))
plot(rasterdirectionavgAG2)

rasterdirectionvarAG <- raster( matrixdirvarsAG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionvarAG <- t(rasterdirectionvarAG)
rasterdirectionvarAG2 <- flip(rasterdirectionvarAG, 2)

par(mar = rep(3, 4))
plot(rasterdirectionvarAG2)


rasterdirectionsdAG <- raster( matrixdirsdAG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionsdAG <- t(rasterdirectionsdAG)
rasterdirectionsdAG2 <- flip(rasterdirectionsdAG, 2)

par(mar = rep(3, 4))
plot(rasterdirectionsdAG2)



# Model 1. IBD model. No current resistance. Larvae can go anywhere in water. 
#Current and direction are not valuble. Resistance raster = distance raster
setwd("C:/Users/18088076/Dropbox/PhD/Chapters/common galaxias/2017 samples DaRT")

studysite<- st_crop(world, world$geometry,xmin=139.0,xmax=151.4,ymin=-37.20, ymax=-42.20)
#studysite <- st_crop(world, c(xmin=140.2, ymin=-37.80, xmax=150.2, ymax=-42.00))
AG_sites <- read.csv("Grayling_coords.csv")

colnames(AG_sites) <- c("Pop", "Lat", "Lon")

AGpoints <- SpatialPoints( AG_sites[,2:3])

AGpoint_geo <- st_as_sf(AG_sites, 
                      coords = c(x = "Lat", y = "Lon"), 
                      crs = 4326)

AGpoint_geo2 <- st_transform(AGpoint_geo, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")


#hull <- gConvexHull(TPpoints)
#hull_plus_buffer <- gBuffer(hull,width=.2)


par(mar = rep(2, 4))

plot(studysite$geometry)
plot(rastercurrentavgAG2, add=T)
plot(AGpoint_geo2, add=T)



#Model 1. All water= resistance 1 all land = resistance 100

rastercurrentavgAG2@data@values

rasterwatermodel1AG <- rastercurrentavgAG2

rasterwatermodel1AG@data@values[rasterwatermodel1AG@data@values <= 10] <- 1
rasterwatermodel1AG@data@values[is.na(rasterwatermodel1AG@data@values)] <- 5


plot(rasterwatermodel1AG)

library(gdistance)
tr <- transition( 1/rasterwatermodel1AG, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
AGpoints[1]

path.1 <- shortestPath( tr, AGpoints[1], AGpoints[3], output="SpatialLines")
plot( rasterwatermodel1AG , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( AGpoints,pch=16, col="red")

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist[1:7,1:7] 

#Success! Model 1 = worked! 

pwfsttable <- read.table("pwfstAG3plateordered.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?

cor(df$Genetic_Distance, df$Slope_Distance)
#0.6059082
model1AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model1AG)
# Pvalue 0.0036
summary(model1AG)
#R squared 
#0.3671
AIC(model1AG)
#-131.2601



# Model 2. Larvae stick to the coast. Resistance matrix needs to increase as distance moves from the coastline


studysite2<- st_crop(world, world$geometry,xmin=138.0,xmax=152.4,ymin=-35.20, ymax=-42.20)

plot(studysite2$geometry)

grid <- st_make_grid(st_as_sfc(st_bbox(studysite2)), cellsize = 0.1, what = "centers")
grid <- st_transform(grid, crs =crs(studysite2))


plot(grid)
plot(studysite2$geometry)

grid2 <- st_intersection(grid, studysite2$geometry)
grid2
plot(grid2)
grid3 <-  st_difference(grid, st_union(grid2))
class(grid3)
plot(grid3)

#plot(grid)
#plot(studysite$geometry, add=T)
#plot(rastercurrentavgTP2, add=T)
#plot(TPpoints, add=T)

studysite2 <- st_cast(studysite2, "MULTILINESTRING")
dist <- st_distance(studysite2, grid3)

class(grid3)
head(dist)

df <- data.frame(dist = as.vector(dist),
                 st_coordinates(grid3))

col_dist <- brewer.pal(11, "RdGy")
ggplot(df, aes(X, Y, fill = dist))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (degrees)")+ #legend name
  theme_void()+ #map theme
  theme(legend.position = "bottom")

crs(studysite2)

ext <- extent(as(grid3, "Spatial"))
r <- raster(resolution = res(rasterwatermodel1AG), ext = ext, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs, +datum=WGS84")

dist_sf <- st_as_sf(df, coords = c("X", "Y")) %>%
  st_set_crs(4326)

dist_raster <- rasterize(dist_sf, r, "dist", fun = mean)



plot(dist_raster)

#Raster works perfectly as of now
res(dist_raster)
res(rasterwatermodel1AG)


#1km as the range for value increases?

plot(rasterwatermodel1AG)
rasterwatermodel2AG <-rasterwatermodel1AG
dist_raster_reduced <- crop(dist_raster, extent(rasterwatermodel1AG), snap='near')
rasterwatermodel2AG <- crop(rasterwatermodel1AG, extent(dist_raster_reduced), snap='near')
dist_raster_reduced
?crs()
plot(rasterwatermodel2AG)
plot(dist_raster_reduced, add=T)

# need to change all values of rasterwatermodel so that as distance increases (based on dist_raster) the new water model increases
# for loop

plot(studysite$geometry)
plot(dist_raster)

plot(dist_raster_reduced, add=T)
dist_raster_reduced@data@values
length(dist_raster_reduced@data@values)

rasterwatermodel2_2AG <- rasterwatermodel2AG
rasterwatermodel2AG@data@values
length(rasterwatermodel2_2AG@data@values)
#paramaters of loop to create new model resistance raster. 
#Nas of the distance raster = land. 
# if distance raster value= NA water model = 5*max
# if resistance raster value <1000 model =1 
# if resistance raster value < 2000 model = 2 etc

for (i in (c(1:length(rasterwatermodel2AG@data@values)))){
  
  
  
  if ((rasterwatermodel2AG@data@values[i] == 5)) {
    
    rasterwatermodel2_2AG@data@values[i] <- 500
    
  } else  {
    
    rasterwatermodel2_2AG@data@values[i] <- (dist_raster_reduced@data@values[i]/500)
    rasterwatermodel2_2AG@data@values[i] <- rasterwatermodel2_2AG@data@values[i]+10
  }
  
}

min(dist_raster_reduced@data@values)
min(rasterwatermodel2_2AG@data@values)
plot(rasterwatermodel2_2AG)



#success
# now to quantify


tr <- transition( 1/rasterwatermodel2_2AG, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
AGpoints[1]

path.1 <- shortestPath( tr, AGpoints[1], AGpoints[3], output="SpatialLines")
plot( rasterwatermodel2_2AG , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( AGpoints,pch=16, col="red")

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist[1:7,1:7] 


pwfsttable <- read.table("pwfstAG3plateordered.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?

#0.6745
cor(df$Genetic_Distance, df$Slope_Distance)
#0.6745
model2AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model2AG)
# Pvalue 0.0007977
summary(model2AG)
#R squared 
#0.455
AIC(model2AG)
#-134.3975









# Model 3. IBD model. No current resistance, larvae do not stick to the coast, instead preferring deeper water

# Grayling
# AG


rasterwatermodel3AG <-rasterwatermodel1AG
dist_raster_reduced <- crop(dist_raster, extent(rasterwatermodel1AG), snap='near')
rasterwatermodel3AG <- crop(rasterwatermodel1AG, extent(dist_raster_reduced), snap='near')
dist_raster_reduced
rasterwatermodel3_2AG <- rasterwatermodel3AG
rasterwatermodel3AG@data@values
length(rasterwatermodel3_2AG@data@values)

#paramaters of loop to create new model resistance raster. 
#Nas of the distance raster = land. 
# if distance raster value= NA water model = 9999999
# if resistance raster value <1000 model =1 
# if resistance raster value < 2000 model = 2 etc

for (i in (c(1:length(rasterwatermodel3AG@data@values)))){
  
  
  
  if ((rasterwatermodel3AG@data@values[i] == 5)) {
    
    rasterwatermodel3_2AG@data@values[i] <- 500
    
  } else  {
    
    rasterwatermodel3_2AG@data@values[i] <- (800000/dist_raster_reduced@data@values[i])
    rasterwatermodel3_2AG@data@values[i] <- rasterwatermodel3_2AG@data@values[i]+10
  }
  
}




min(dist_raster_reduced@data@values)
min(rasterwatermodel3_2AG@data@values)
plot(rasterwatermodel3_2AG)


for (i in (c(1:length(rasterwatermodel3AG@data@values)))){
  
  
  if (rasterwatermodel3_2AG@data@values[i] > 501) {
    
    rasterwatermodel3_2AG@data@values[i] =50
    
  }
}

plot(rasterwatermodel3_2AG)

#success Model 3 is complete. Now quantify


tr <- transition( 1/rasterwatermodel3_2AG, transitionFunction = mean, directions = 4)
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
AGpoints[1]

path.1 <- shortestPath( tr, AGpoints[1], AGpoints[3], output="SpatialLines")
plot( rasterwatermodel3_2AG , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( AGpoints,pch=16, col="red")

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist[1:7,1:7] 

pwfsttable <- read.table("pwfstAG3plateordered.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?

cor(df$Genetic_Distance, df$Slope_Distance)
#0.3833762
model3AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model3AG)
# Pvalue 0.08625
summary(model3AG)
#R squared 
#0.147
AIC(model3AG)
#-124.9913


# Now model 4. 
# Model 4. IBR model. Current and direction resistance. Larvae can go anywhere in water

# This model will require the addition of currents and direction which means it will be location specific
# Initial thoughts ifelse commands based on relative XY of site to raster?


rasterwatermodel1AG
plot(rasterwatermodel1AG)

rastercurrentavgAG2
plot(rastercurrentavgAG2)

rasterdirectionavgAG2
plot(rasterdirectionavgAG2)
# change raster to same size
rastercurrentavgAG4 <-rastercurrentavgAG2
dist_raster_reduced4 <- crop(dist_raster, extent(rastercurrentavgAG4), snap='near')
rastercurrentavgAG4 <- crop(rastercurrentavgAG2, extent(dist_raster_reduced), snap='near')

rasterdirectionavgAG4 <-rasterdirectionavgAG2
dist_raster_reduced4 <- crop(dist_raster, extent(rasterdirectionavgAG4), snap='near')
rasterdirectionavgAG4 <- crop(rasterdirectionavgAG2, extent(dist_raster_reduced), snap='near')

rasterwatermodel4AG <- rasterwatermodel1AG
dist_raster_reduced4 <- crop(dist_raster, extent(rasterwatermodel4AG), snap='near')
rasterwatermodel4AG <- crop(rasterwatermodel1AG, extent(dist_raster_reduced), snap='near')


#xFromCell(rasterwatermodel4AG, (3))
#rasterwatermodel4AG@data@attributes
#View(rasterwatermodel4AG)


# Can mess with the variables so that current is the primary driver when strong vs not the primary driver. 

raster_list <- list()

for (j in(c(1:length(AGpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4AG@data@values)))) {
    
    #if the Lon coordinate of the site is to the left of the raster pixel
    
    if(AGpoints@coords[j,1] < xFromCell(rasterwatermodel4AG, (i))) {
      
      if(!is.na(rasterdirectionavgAG4@data@values[i])) {
        
        if (rasterdirectionavgAG4@data@values[i] > 45 & rasterdirectionavgAG4@data@values[i] < 135){
          
          rasterwatermodel4AG@data@values[i] <- (45 - 100*rastercurrentavgAG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgAG4@data@values[i] > 135 & rasterdirectionavgAG4@data@values[i] < 180) {
          
          rasterwatermodel4AG@data@values[i] <- (45 + 50*rastercurrentavgAG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgAG4@data@values[i] > 0 & rasterdirectionavgAG4@data@values[i] < 45) {
          
          rasterwatermodel4AG@data@values[i] <- (45 - 50*rastercurrentavgAG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgAG4@data@values[i] > -135 & rasterdirectionavgAG4@data@values[i] < -45) {
          
          rasterwatermodel4AG@data@values[i] <- (45 + 100*rastercurrentavgAG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgAG4@data@values[i] > -180 & rasterdirectionavgAG4@data@values[i] < -135) {
          
          rasterwatermodel4AG@data@values[i] <- (45 + 50*rastercurrentavgAG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgAG4@data@values[i] > -45 & rasterdirectionavgAG4@data@values[i] < 0) {
          
          rasterwatermodel4AG@data@values[i] <- (45 - 50*rastercurrentavgAG4@data@values[i]) 
          
        }
        
        else {
          
          rasterwatermodel4AG@data@values[i] <- (45 + 100*rastercurrentavgAG4@data@values[i])
          
        }  
      } 
      else {
        
        rasterwatermodel4AG@data@values[i] <- 150 
        
      }
    }
    
    #if the Lon coordinate of the site is to the right of the raster pixel
    else if (AGpoints@coords[j,1] >= xFromCell(rasterwatermodel4AG, (i))) {
      
      if (!is.na(rasterdirectionavgAG4@data@values[i])) {
        if (rasterdirectionavgAG4@data@values[i] > 45 & rasterdirectionavgAG4@data@values[i] < 135){
          
          rasterwatermodel4AG@data@values[i] <- (45 + 100*rastercurrentavgAG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgAG4@data@values[i] > 135 & rasterdirectionavgAG4@data@values[i] < 180) {
          
          rasterwatermodel4AG@data@values[i] <- (45 + 50*rastercurrentavgAG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgAG4@data@values[i] > 0 & rasterdirectionavgAG4@data@values[i] < 45) {
          
          rasterwatermodel4AG@data@values[i] <- (45 - 50*rastercurrentavgAG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgAG4@data@values[i] > -135 & rasterdirectionavgAG4@data@values[i] < -45) {
          
          rasterwatermodel4AG@data@values[i] <- (45 - 100*rastercurrentavgAG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgAG4@data@values[i] > -180 & rasterdirectionavgAG4@data@values[i] < -135) {
          
          rasterwatermodel4AG@data@values[i] <- (45 + 50*rastercurrentavgAG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgAG4@data@values[i] > -45 & rasterdirectionavgAG4@data@values[i] < 0) {
          
          rasterwatermodel4AG@data@values[i] <- (45 - 50*rastercurrentavgAG4@data@values[i]) 
          
        } 
      }
      else {
        
        rasterwatermodel4AG@data@values[i] <- 150
        
      }
    }
    
    # if NA
    else {
      rasterwatermodel4AG@data@values[i] <- 150
      
    }
    
  }
  
  raster_list[[j]] <- rasterwatermodel4AG
  
  
  
}

rasterwatermodel4AGpop1 <- raster_list[[1]]
rasterwatermodel4AGpop2 <- raster_list[[2]]
rasterwatermodel4AGpop3 <- raster_list[[3]]
rasterwatermodel4AGpop4 <- raster_list[[4]]
rasterwatermodel4AGpop5 <- raster_list[[5]]
rasterwatermodel4AGpop6 <- raster_list[[6]]
rasterwatermodel4AGpop7 <- raster_list[[7]]
plot(rasterwatermodel4AGpop2)

plot(rastercurrentavgAG2)
plot(rasterdirectionavgAG2)




# Now not sure how to best do this at this point

# Need to combine the ecological distances for each pop for each raster into a single e distance to
# use as the matrix to plot fst against
# Can I have uneven dissimilarity matricies? The cost one way is different to the other way? 
# Should the costs be averaged?
# pairwise fst is by definition averaged.

#pop1
tr <- transition( 1/rasterwatermodel4AGpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel4AGpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel4AGpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel4AGpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel4AGpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel4AGpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel4AGpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 


vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstAG3plateordered.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)

cor(df$Genetic_Distance, df$Slope_Distance)
#0.6318646
model4AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model4AG)
# Pvalue 0.002121
summary(model4AG)
#R squared 
#0.3993
AIC(model4AG)
#-132.3542


#relative migration rates
pwfsttable <- read.table("divmigrateAGordered2.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.3154773
model4_2AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model4_2AG)
# Pvalue 0.1636
summary(model4_2AG)
#R squared 
#0.09953
AIC(model4_2AG)
#-4.919449


# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?


#Fst 0.6318646
# 0.3154773






# Now Model 5. 
#Model 5. IBR model. Current and direction resistance. Larvae stick to the coast and therefore further out is higher resistance
# So the model 4 rasters are pretty good. Can I just use them and apply a new distance raster/ factor based on model 2?

rasterwatermodel2_2AG

rasterwatermodel2_2AG@data@values

plot(rasterwatermodel2_2AG)
rasterwatermodel4AGpop1@data@values
raster_listmodel5 <- raster_list

for (j in(c(1:length(AGpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4AG@data@values)))) {
    
    raster_listmodel5[[j]]@data@values[i] <- (raster_list[[j]]@data@values[i] + (0.2*rasterwatermodel2_2AG@data@values[i])) 
    
    if (raster_listmodel5[[j]]@data@values[i] > 200) {
      
      raster_listmodel5[[j]]@data@values[i] <- 200
    }
    
    
    else {
      raster_listmodel5[[j]]@data@values[i] <- raster_listmodel5[[j]]@data@values[i]
      
    }
  }
  
}



rasterwatermodel5AGpop1 <- raster_listmodel5[[1]]
rasterwatermodel5AGpop2 <- raster_listmodel5[[2]]
rasterwatermodel5AGpop3 <- raster_listmodel5[[3]]
rasterwatermodel5AGpop4 <- raster_listmodel5[[4]]
rasterwatermodel5AGpop5 <- raster_listmodel5[[5]]
rasterwatermodel5AGpop6 <- raster_listmodel5[[6]]
rasterwatermodel5AGpop7 <- raster_listmodel5[[7]]

plot(rasterwatermodel4AGpop1)
plot(rasterwatermodel5AGpop6)
plot(rasterwatermodel2_2AG)
rasterwatermodel4AGpop7@data@values
rasterwatermodel5AGpop7@data@values

#pop1
tr <- transition( 1/rasterwatermodel5AGpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel5AGpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel5AGpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel5AGpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel5AGpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel5AGpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel5AGpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 


# Currently this just brings back the lower triangle but that is not correct as directional flow is assymetrical 


#try average. 

vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstAG3plateordered.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.6315248

model5AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model5AG)
# Pvalue 0.002137
summary(model5AG)
#R squared 
#0.3988
AIC(model5AG)
#-132.3392



#relative migration rates
pwfsttable <- read.table("divmigrateAGordered2.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.3140548

model5_2AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model5_2AG)
# Pvalue 0.1656
summary(model5_2AG)
#R squared 
#0.09863
AIC(model5_2AG)
#-4.940323

#Fst 0.6315248
# Migration rates 0.3140548


# Now Model 6. 
## Model 6. IBR model. Current and direction resistance. Larvae do not stick to the coast, preferring deeper water
# So the model 4 rasters are pretty good. Can I just use them and apply a new distance raster/ factor based on model 3?


rasterwatermodel3_2AG

rasterwatermodel3_2AG@data@values

plot(rasterwatermodel3_2AG)
rasterwatermodel4AGpop1@data@values
raster_listmodel6 <- raster_list

for (j in(c(1:length(AGpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4AG@data@values)))) {
    
    raster_listmodel6[[j]]@data@values[i] <- (raster_list[[j]]@data@values[i] + (0.5*rasterwatermodel3_2AG@data@values[i])) 
    
    if (raster_listmodel6[[j]]@data@values[i] == 400) {
      
      raster_listmodel6[[j]]@data@values[i] <- 200
    }
    
    else if (raster_listmodel5[[j]]@data@values[i] > 600) {
      
      raster_listmodel6[[j]]@data@values[i] <- 150
    }
    
    
    else {
      raster_listmodel6[[j]]@data@values[i] <- raster_listmodel6[[j]]@data@values[i]
      
    }
  }
  
}



rasterwatermodel6AGpop1 <- raster_listmodel6[[1]]
rasterwatermodel6AGpop2 <- raster_listmodel6[[2]]
rasterwatermodel6AGpop3 <- raster_listmodel6[[3]]
rasterwatermodel6AGpop4 <- raster_listmodel6[[4]]
rasterwatermodel6AGpop5 <- raster_listmodel6[[5]]
rasterwatermodel6AGpop6 <- raster_listmodel6[[6]]
rasterwatermodel6AGpop7 <- raster_listmodel6[[7]]




plot(rasterwatermodel4AGpop1)
plot(rasterwatermodel6AGpop1)
plot(rasterwatermodel3_2AG)
rasterwatermodel4AGpop7@data@values
rasterwatermodel6AGpop7@data@values

#pop1
tr <- transition( 1/rasterwatermodel6AGpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel6AGpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel6AGpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel6AGpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel6AGpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel6AGpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel6AGpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, AGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- AG_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 


#try average. 

vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstAG3plateordered.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.5865211
model6AG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model6AG)
# Pvalue 0.005196
summary(model6AG)
#R squared 
#0.344
AIC(model6AG)
#-130.5067


#relative migration rates
pwfsttable <- read.table("divmigrateAGordered2.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","5", "6", "7", "8", "9")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")
cor(df$Genetic_Distance, df$Slope_Distance)
#0.2559859
model6_2AG <- lm(Genetic_Distance~Slope_Distance,data=df)

anova(model6_2AG)
# Pvalue 0.2627
summary(model6_2AG)
#R squared 
#0.06553
AIC(model6_2AG)
#-5.697697



#Model 1
# Correlation =0.6059082
# P-value = 0.0036
# Rsquared = 0.3671
# AIC = -131.2601

#Model 2
# Correlation =0.6745
# P-value = 0.0007977
# Rsquared = 0.455
# AIC = -134.3975

#Model 3
# Correlation =0.3833762
# P-value = 0.08625
# Rsquared = 0.147
# AIC = -124.9913

#Model 4
#Fst 
# Correlation =0.6318646
# P-value = 0.002121
# Rsquared = 0.3993
# AIC = -132.3542

# migration rates 
# Correlation =0.3154773
# P-value = 0.1636
# Rsquared = 0.09953
# AIC = 4.919449

#Model 5
#Fst
# Correlation =0.6315248
# P-value = 0.002137
# Rsquared = 0.3988
# AIC = -132.3392

# migration rates
# Correlation =0.3140548
# P-value = 0.1656
# Rsquared = 0.09863
# AIC = 4.940323
#Model 6
#Fst
# Correlation = 0.5865211
# P-value = 0.005196
# Rsquared = 0.344
# AIC = -130.5067

#migration rates
# Correlation = 0.2559859
# P-value = 0.2627
# Rsquared = 0.06553
# AIC = 5.697697

#########################



######################



#
#


# And now finally Common galaxias


##########
##########

##### 
# breed in Autumn and return 4-6 months later. 
# more specific than Autumn can't be found. They have a wide range of breeding. Mid April for breeding 
# end of September return 15/4/2016 - 30/9/2016








#2000
date[54]
date[137]
#2001
date[236]
date[320]
#2002
date[433]
date[503]
#2003
date[601]
date[685]
#2004
date[784]
date[868]
#2005
date[966]
date[1050]
#2006
date[1149]
date[1233]
#2007
date[1331]
date[1416]
#2008
date[1514]
date[1599]
#2009
date[1697]
date[1781]
#2010
date[1880]
date[1963]
#2011
date[2114]
date[2282]
#2012
date[2480]
date[2648]
#2013
date[2845]
date[3013]
#2014
date[3210]
date[3378]
#2015
date[3575]
date[3743]
#2016
date[3941]
date[4106]

#
dim(speed)
speed[1,1,c(54:137, 236:320, 433:503, 601:685, 784:868, 966:1050, 1149:1233, 1331:1416 , 1514:1599, 1697:1781, 1880:1963, 2114:2282, 
            2480:2648, 2845:3013, 3210:3378, 3575:3743, 3941:4106)]

# CG 

matrixavgsCG <- matrix(nrow = 63,ncol=26)
matrixvarsCG <- matrix(nrow = 63,ncol=26)
matrixsdCG <- matrix(nrow =63,ncol=26)




for (j in (c(1:26))) {
  
  for ( i in (c(1:63))) {
    
    vectorvals <- speed[i,j,c(54:137, 236:320, 433:503, 601:685, 784:868, 966:1050, 1149:1233, 1331:1416 , 1514:1599, 1697:1781, 1880:1963, 2114:2282, 
                               2480:2648, 2845:3013, 3210:3378, 3575:3743, 3941:4106)]
    
    matrixavgsCG[i,j] <- mean(vectorvals)
    matrixvarsCG[i,j] <- var(vectorvals)
    matrixsdCG[i,j] <- sd(vectorvals)
    
  }
  
  
}

rastercurrentavgCG <- raster( matrixavgsCG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentavgCG <- t(rastercurrentavgCG)
rastercurrentavgCG2 <- flip(rastercurrentavgCG, 2)

par(mar = rep(3, 4))
plot(rastercurrentavgCG2)

rastercurrentvarCG <- raster( matrixvarsCG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentvarCG <- t(rastercurrentvarCG)
rastercurrentvarCG2 <- flip(rastercurrentvarCG, 2)

par(mar = rep(3, 4))
plot(rastercurrentvarCG2)


rastercurrentsdCG <- raster( matrixsdCG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rastercurrentsdCG <- t(rastercurrentsdCG)
rastercurrentsdCG2 <- flip(rastercurrentsdCG, 2)

par(mar = rep(3, 4))
plot(rastercurrentsdCG2)


#
# Now direction





matrixdiravgsCG <- matrix(nrow = 63,ncol=26)
matrixdirvarsCG <- matrix(nrow = 63,ncol=26)
matrixdirsdCG <- matrix(nrow =63,ncol=26)




for (j in (c(1:26))) {
  
  for ( i in (c(1:63))) {
    
    vectorvals <- direction[i,j,c(54:137, 236:320, 433:503, 601:685, 784:868, 966:1050, 1149:1233, 1331:1416 , 1514:1599, 1697:1781, 1880:1963, 2114:2282, 
                                  2480:2648, 2845:3013, 3210:3378, 3575:3743, 3941:4106)]
    
    
    matrixdiravgsCG[i,j] <- mean(vectorvals)
    matrixdirvarsCG[i,j] <- var(vectorvals)
    matrixdirsdCG[i,j] <- sd(vectorvals)
    
  }
  
  
}

rasterdirectionavgCG <- raster(matrixdiravgsCG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionavgCG <- t(rasterdirectionavgCG)
rasterdirectionavgCG2 <- flip(rasterdirectionavgCG, 2)

par(mar = rep(3, 4))
plot(rasterdirectionavgCG2)

rasterdirectionvarCG <- raster( matrixdirvarsCG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionvarCG <- t(rasterdirectionvarCG)
rasterdirectionvarCG2 <- flip(rasterdirectionvarCG, 2)

par(mar = rep(3, 4))
plot(rasterdirectionvarCG2)


rasterdirectionsdCG <- raster( matrixdirsdCG, xmn = range( dat1[[2]])[1], xmx = range( dat1[[2]])[2], ymn = range( dat1[[1]])[1], ymx = range( dat1[[1]])[2])

rasterdirectionsdCG <- t(rasterdirectionsdCG)
rasterdirectionsdCG2 <- flip(rasterdirectionsdCG, 2)

par(mar = rep(3, 4))
plot(rasterdirectionsdCG2)



# Model 1. IBD model. No current resistance. Larvae can go anywhere in water. 
#Current and direction are not valuble. Resistance raster = distance raster
setwd("C:/Users/18088076/Dropbox/PhD/Chapters/common galaxias/2017 samples DaRT")

studysite<- st_crop(world, world$geometry,xmin=139.0,xmax=151.4,ymin=-37.20, ymax=-42.20)
#studysite <- st_crop(world, c(xmin=140.2, ymin=-37.80, xmax=150.2, ymax=-42.00))
CG_sites <- read.csv("CG_coords.csv")

colnames(CG_sites) <- c("Pop", "Lat", "Lon")

CGpoints <- SpatialPoints( CG_sites[,2:3])

CGpoint_geo <- st_as_sf(CG_sites, 
                        coords = c(x = "Lat", y = "Lon"), 
                        crs = 4326)

CGpoint_geo2 <- st_transform(CGpoint_geo, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")


#hull <- gConvexHull(TPpoints)
#hull_plus_buffer <- gBuffer(hull,width=.2)


par(mar = rep(2, 4))

plot(studysite$geometry)
plot(rastercurrentavgCG2, add=T)
plot(CGpoint_geo2, add=T)



#Model 1. All water= resistance 1 all land = resistance 10

rastercurrentavgCG2@data@values

rasterwatermodel1CG <- rastercurrentavgCG2

rasterwatermodel1CG@data@values[rasterwatermodel1CG@data@values <= 10] <- 1
rasterwatermodel1CG@data@values[is.na(rasterwatermodel1CG@data@values)] <- 5


plot(rasterwatermodel1CG)

library(gdistance)
tr <- transition( 1/rasterwatermodel1CG, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
CGpoints[1]

path.1 <- shortestPath( tr, CGpoints[1], CGpoints[3], output="SpatialLines")
plot( rasterwatermodel1CG , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( CGpoints,pch=16, col="red")

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- tupong_sites$Pop
eDist[1:7,1:7] 

#Success! Model 1 = worked! 

pwfsttable <- read.table("pwfstCG10000bootfixedcovar.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?


cor(df$Genetic_Distance, df$Slope_Distance)
#0.07831227

cor(df$Genetic_Distance, df$Slope_Distance)
#0.6042422
model1CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model1CG)
# Pvalue 0.6091
summary(model1CG)
#R squared 
#0.006133
AIC(model1CG)
#-428.8433

# Model 2. Larvae stick to the coast. Resistance matrix needs to increase as distance moves from the coastline


studysite2<- st_crop(world, world$geometry,xmin=138.0,xmax=152.4,ymin=-35.20, ymax=-42.20)

plot(studysite2$geometry)

grid <- st_make_grid(st_as_sfc(st_bbox(studysite2)), cellsize = 0.1, what = "centers")
grid <- st_transform(grid, crs =crs(studysite2))


plot(grid)
plot(studysite2$geometry)

grid2 <- st_intersection(grid, studysite2$geometry)
grid2
plot(grid2)
grid3 <-  st_difference(grid, st_union(grid2))
class(grid3)
plot(grid3)

#plot(grid)
#plot(studysite$geometry, add=T)
#plot(rastercurrentavgTP2, add=T)
#plot(TPpoints, add=T)

studysite2 <- st_cast(studysite2, "MULTILINESTRING")
dist <- st_distance(studysite2, grid3)

class(grid3)
head(dist)

df <- data.frame(dist = as.vector(dist),
                 st_coordinates(grid3))

col_dist <- brewer.pal(11, "RdGy")
ggplot(df, aes(X, Y, fill = dist))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (degrees)")+ #legend name
  theme_void()+ #map theme
  theme(legend.position = "bottom")

crs(studysite2)

ext <- extent(as(grid3, "Spatial"))
r <- raster(resolution = res(rasterwatermodel1CG), ext = ext, crs = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs, +datum=WGS84")

dist_sf <- st_as_sf(df, coords = c("X", "Y")) %>%
  st_set_crs(4326)

dist_raster <- rasterize(dist_sf, r, "dist", fun = mean)



plot(dist_raster)

#Raster works perfectly as of now
res(dist_raster)
res(rasterwatermodel1CG)


#1km as the range for value increases?

plot(rasterwatermodel1CG)
rasterwatermodel2CG <-rasterwatermodel1CG
dist_raster_reduced <- crop(dist_raster, extent(rasterwatermodel1CG), snap='near')
rasterwatermodel2CG <- crop(rasterwatermodel1CG, extent(dist_raster_reduced), snap='near')
dist_raster_reduced
?crs()
plot(rasterwatermodel2CG)
plot(dist_raster_reduced, add=T)

# need to change all values of rasterwatermodel so that as distance increases (based on dist_raster) the new water model increases
# for loop

plot(studysite$geometry)
plot(dist_raster)

plot(dist_raster_reduced, add=T)
dist_raster_reduced@data@values
length(dist_raster_reduced@data@values)

rasterwatermodel2_2CG <- rasterwatermodel2CG
rasterwatermodel2CG@data@values
length(rasterwatermodel2_2CG@data@values)
#paramaters of loop to create new model resistance raster. 
#Nas of the distance raster = land. 
# if distance raster value= NA water model = 5*max
# if resistance raster value <1000 model =1 
# if resistance raster value < 2000 model = 2 etc

for (i in (c(1:length(rasterwatermodel2CG@data@values)))){
  
  
  
  if ((rasterwatermodel2CG@data@values[i] == 5)) {
    
    rasterwatermodel2_2CG@data@values[i] <- 500
    
  } else  {
    
    rasterwatermodel2_2CG@data@values[i] <- (dist_raster_reduced@data@values[i]/500)
    rasterwatermodel2_2CG@data@values[i] <- rasterwatermodel2_2CG@data@values[i]+10
  }
  
}

min(dist_raster_reduced@data@values)
min(rasterwatermodel2_2CG@data@values)
plot(rasterwatermodel2_2CG)



#success
# now to quantify


tr <- transition( 1/rasterwatermodel2_2CG, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
CGpoints[1]

path.1 <- shortestPath( tr, CGpoints[1], CGpoints[3], output="SpatialLines")
plot( rasterwatermodel2_2CG , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( CGpoints,pch=16, col="red")

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist[1:7,1:7] 


pwfsttable <- read.table("pwfstCG10000bootfixedcovar.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.1041885
model2CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model2CG)
# Pvalue 0.4958
summary(model2CG)
#R squared 
#0.01086
AIC(model2CG)
#-429.0576
# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?








# Model 3. IBD model. No current resistance, larvae do not stick to the coast, instead preferring deeper water

# Grayling
# CG


rasterwatermodel3CG <-rasterwatermodel1CG
dist_raster_reduced <- crop(dist_raster, extent(rasterwatermodel1CG), snap='near')
rasterwatermodel3CG <- crop(rasterwatermodel1CG, extent(dist_raster_reduced), snap='near')
dist_raster_reduced
rasterwatermodel3_2CG <- rasterwatermodel3CG
rasterwatermodel3CG@data@values
length(rasterwatermodel3_2CG@data@values)

#paramaters of loop to create new model resistance raster. 
#Nas of the distance raster = land. 
# if distance raster value= NA water model = 9999999
# if resistance raster value <1000 model =1 
# if resistance raster value < 2000 model = 2 etc

for (i in (c(1:length(rasterwatermodel3CG@data@values)))){
  
  
  
  if ((rasterwatermodel3CG@data@values[i] == 5)) {
    
    rasterwatermodel3_2CG@data@values[i] <- 500
    
  } else  {
    
    rasterwatermodel3_2CG@data@values[i] <- (800000/dist_raster_reduced@data@values[i])
    rasterwatermodel3_2CG@data@values[i] <- rasterwatermodel3_2CG@data@values[i]+10
  }
  
}




min(dist_raster_reduced@data@values)
min(rasterwatermodel3_2CG@data@values)
plot(rasterwatermodel3_2CG)


for (i in (c(1:length(rasterwatermodel3CG@data@values)))){
  
  
  if (rasterwatermodel3_2CG@data@values[i] > 501) {
    
    rasterwatermodel3_2CG@data@values[i] =50
    
  }
}

plot(rasterwatermodel3_2CG)

#success Model 3 is complete. Now quantify


tr <- transition( 1/rasterwatermodel3_2CG, transitionFunction = mean, directions = 4)
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)
tr
CGpoints[1]

path.1 <- shortestPath( tr, CGpoints[1], CGpoints[3], output="SpatialLines")
plot( rasterwatermodel3_2CG , xlab="Longitude", ylab="Latitude")
lines( path.1, col="red")
points( CGpoints,pch=16, col="red")

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist[1:7,1:7] 
pwfsttable <- read.table("pwfstCG10000bootfixedcovar.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")

# edist and pwfst are correctly formatted for a dissimilarity test


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist[ lower.tri(eDist)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?

cor(df$Genetic_Distance, df$Slope_Distance)
#0.1398613
model3CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model3CG)
# Pvalue 0.3595
summary(model3CG)
#R squared 
#0.01956
AIC(model3CG)
#-429.4554


# Now model 4. 
# Model 4. IBR model. Current and direction resistance. Larvae can go anywhere in water

# This model will require the addition of currents and direction which means it will be location specific
# Initial thoughts ifelse commands based on relative XY of site to raster?


rasterwatermodel1CG
plot(rasterwatermodel1CG)

rastercurrentavgCG2
plot(rastercurrentavgCG2)

rasterdirectionavgCG2
plot(rasterdirectionavgCG2)
# change raster to same size
rastercurrentavgCG4 <-rastercurrentavgCG2
dist_raster_reduced4 <- crop(dist_raster, extent(rastercurrentavgCG4), snap='near')
rastercurrentavgCG4 <- crop(rastercurrentavgCG2, extent(dist_raster_reduced), snap='near')

rasterdirectionavgCG4 <-rasterdirectionavgCG2
dist_raster_reduced4 <- crop(dist_raster, extent(rasterdirectionavgCG4), snap='near')
rasterdirectionavgCG4 <- crop(rasterdirectionavgCG2, extent(dist_raster_reduced), snap='near')

rasterwatermodel4CG <- rasterwatermodel1CG
dist_raster_reduced4 <- crop(dist_raster, extent(rasterwatermodel4CG), snap='near')
rasterwatermodel4CG <- crop(rasterwatermodel1CG, extent(dist_raster_reduced), snap='near')


#xFromCell(rasterwatermodel4CG, (3))
#rasterwatermodel4CG@data@attributes
#View(rasterwatermodel4CG)


# Can mess with the variables so that current is the primary driver when strong vs not the primary driver. 

raster_list <- list()

for (j in(c(1:length(CGpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4CG@data@values)))) {
    
    #if the Lon coordinate of the site is to the left of the raster pixel
    
    if(CGpoints@coords[j,1] < xFromCell(rasterwatermodel4CG, (i))) {
      
      if(!is.na(rasterdirectionavgCG4@data@values[i])) {
        
        if (rasterdirectionavgCG4@data@values[i] > 45 & rasterdirectionavgCG4@data@values[i] < 135){
          
          rasterwatermodel4CG@data@values[i] <- (45 - 100*rastercurrentavgCG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgCG4@data@values[i] > 135 & rasterdirectionavgCG4@data@values[i] < 180) {
          
          rasterwatermodel4CG@data@values[i] <- (45 + 50*rastercurrentavgCG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgCG4@data@values[i] > 0 & rasterdirectionavgCG4@data@values[i] < 45) {
          
          rasterwatermodel4CG@data@values[i] <- (45 - 50*rastercurrentavgCG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgCG4@data@values[i] > -135 & rasterdirectionavgCG4@data@values[i] < -45) {
          
          rasterwatermodel4CG@data@values[i] <- (45 + 100*rastercurrentavgCG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgCG4@data@values[i] > -180 & rasterdirectionavgCG4@data@values[i] < -135) {
          
          rasterwatermodel4CG@data@values[i] <- (45 + 50*rastercurrentavgCG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgCG4@data@values[i] > -45 & rasterdirectionavgCG4@data@values[i] < 0) {
          
          rasterwatermodel4CG@data@values[i] <- (45 - 50*rastercurrentavgCG4@data@values[i]) 
          
        }
        
        else {
          
          rasterwatermodel4CG@data@values[i] <- (45 + 100*rastercurrentavgCG4@data@values[i])
          
        }  
      } 
      else {
        
        rasterwatermodel4CG@data@values[i] <- 150 
        
      }
    }
    
    #if the Lon coordinate of the site is to the right of the raster pixel
    else if (CGpoints@coords[j,1] >= xFromCell(rasterwatermodel4CG, (i))) {
      
      if (!is.na(rasterdirectionavgCG4@data@values[i])) {
        if (rasterdirectionavgCG4@data@values[i] > 45 & rasterdirectionavgCG4@data@values[i] < 135){
          
          rasterwatermodel4CG@data@values[i] <- (45 + 100*rastercurrentavgCG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgCG4@data@values[i] > 135 & rasterdirectionavgCG4@data@values[i] < 180) {
          
          rasterwatermodel4CG@data@values[i] <- (45 + 50*rastercurrentavgCG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgCG4@data@values[i] > 0 & rasterdirectionavgCG4@data@values[i] < 45) {
          
          rasterwatermodel4CG@data@values[i] <- (45 - 50*rastercurrentavgCG4@data@values[i]) 
          
        }
        
        else if (rasterdirectionavgCG4@data@values[i] > -135 & rasterdirectionavgCG4@data@values[i] < -45) {
          
          rasterwatermodel4CG@data@values[i] <- (45 - 100*rastercurrentavgCG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgCG4@data@values[i] > -180 & rasterdirectionavgCG4@data@values[i] < -135) {
          
          rasterwatermodel4CG@data@values[i] <- (45 + 50*rastercurrentavgCG4@data@values[i]) 
          
        }
        else if (rasterdirectionavgCG4@data@values[i] > -45 & rasterdirectionavgCG4@data@values[i] < 0) {
          
          rasterwatermodel4CG@data@values[i] <- (45 - 50*rastercurrentavgCG4@data@values[i]) 
          
        } 
      }
      else {
        
        rasterwatermodel4CG@data@values[i] <- 150
        
      }
    }
    
    # if NA
    else {
      rasterwatermodel4CG@data@values[i] <- 150
      
    }
    
  }
  
  raster_list[[j]] <- rasterwatermodel4CG
  
  
  
}

rasterwatermodel4CGpop1 <- raster_list[[1]]
rasterwatermodel4CGpop2 <- raster_list[[2]]
rasterwatermodel4CGpop3 <- raster_list[[3]]
rasterwatermodel4CGpop4 <- raster_list[[4]]
rasterwatermodel4CGpop5 <- raster_list[[5]]
rasterwatermodel4CGpop6 <- raster_list[[6]]
rasterwatermodel4CGpop7 <- raster_list[[7]]
plot(rasterwatermodel4CGpop2)

plot(rastercurrentavgCG2)
plot(rasterdirectionavgCG2)




# Now not sure how to best do this at this point

# Need to combine the ecological distances for each pop for each raster into a single e distance to
# use as the matrix to plot fst CGainst
# Can I have uneven dissimilarity matricies? The cost one way is different to the other way? 
# Should the costs be averaged?
# pairwise fst is by definition averCGed.

#pop1
tr <- transition( 1/rasterwatermodel4CGpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel4CGpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel4CGpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel4CGpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel4CGpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel4CGpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel4CGpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 



vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstCG10000bootfixedcovar.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
# 0.06860341
model4CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model4CG)
# Pvalue 0.6543
summary(model4CG)
#R squared 
#0.004706
AIC(model4CG)
#-428.7787

#relative migration rates
pwfsttable <- read.table("divmigrateCGordered2.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.05844662
model4_2CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model4_2CG)
# Pvalue 0.7029
summary(model4_2CG)
#R squared 
#0.003416
AIC(model4_2CG)
#-40.90902


# Now Model 5. 
#Model 5. IBR model. Current and direction resistance. Larvae stick to the coast and therefore further out is higher resistance
# So the model 4 rasters are pretty good. Can I just use them and apply a new distance raster/ factor based on model 2?

rasterwatermodel2_2CG

rasterwatermodel2_2CG@data@values

plot(rasterwatermodel2_2CG)
rasterwatermodel4CGpop1@data@values
raster_listmodel5 <- raster_list

for (j in(c(1:length(CGpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4CG@data@values)))) {
    
    raster_listmodel5[[j]]@data@values[i] <- (raster_list[[j]]@data@values[i] + (0.2*rasterwatermodel2_2CG@data@values[i])) 
    
    if (raster_listmodel5[[j]]@data@values[i] > 200) {
      
      raster_listmodel5[[j]]@data@values[i] <- 200
    }
    
    
    else {
      raster_listmodel5[[j]]@data@values[i] <- raster_listmodel5[[j]]@data@values[i]
      
    }
  }
  
}



rasterwatermodel5CGpop1 <- raster_listmodel5[[1]]
rasterwatermodel5CGpop2 <- raster_listmodel5[[2]]
rasterwatermodel5CGpop3 <- raster_listmodel5[[3]]
rasterwatermodel5CGpop4 <- raster_listmodel5[[4]]
rasterwatermodel5CGpop5 <- raster_listmodel5[[5]]
rasterwatermodel5CGpop6 <- raster_listmodel5[[6]]
rasterwatermodel5CGpop7 <- raster_listmodel5[[7]]

plot(rasterwatermodel4CGpop1)
plot(rasterwatermodel5CGpop6)
plot(rasterwatermodel2_2CG)
rasterwatermodel4CGpop7@data@values
rasterwatermodel5CGpop7@data@values

#pop1
tr <- transition( 1/rasterwatermodel5CGpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel5CGpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel5CGpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel5CGpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel5CGpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel5CGpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel5CGpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 


# Currently this just brings back the lower triangle but that is not correct as directional flow is assymetrical 


#try average. 

vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstCG10000bootfixedcovar.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.08312044
model5CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model5CG)
# Pvalue 0.5872
summary(model5CG)
#R squared 
#0.006909
AIC(model5CG)
#-428.8784

#relative migration rates
pwfsttable <- read.table("divmigrateCGordered2.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")
cor(df$Genetic_Distance, df$Slope_Distance)
#0.0895136
model5_2CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model5_2CG)
# Pvalue 0.5587
summary(model5_2CG)
#R squared 
#0.008013
AIC(model5_2CG)
#-41.11706
# Run a GLM of the data and use that to determine the AIC? 
# Do for each method?


#Fst 0.08312044
# Migration rates 0.0895136


# Now Model 6. 
## Model 6. IBR model. Current and direction resistance. Larvae do not stick to the coast, preferring deeper water
# So the model 4 rasters are pretty good. Can I just use them and apply a new distance raster/ factor based on model 3?


rasterwatermodel3_2CG

rasterwatermodel3_2CG@data@values

plot(rasterwatermodel3_2CG)
rasterwatermodel4CGpop1@data@values
raster_listmodel6 <- raster_list

for (j in(c(1:length(CGpoints@coords[,1])))) {
  for (i in (c(1:length(rasterwatermodel4CG@data@values)))) {
    
    raster_listmodel6[[j]]@data@values[i] <- (raster_list[[j]]@data@values[i] + (0.5*rasterwatermodel3_2CG@data@values[i])) 
    
    if (raster_listmodel6[[j]]@data@values[i] == 400) {
      
      raster_listmodel6[[j]]@data@values[i] <- 200
    }
    
    else if (raster_listmodel5[[j]]@data@values[i] > 600) {
      
      raster_listmodel6[[j]]@data@values[i] <- 150
    }
    
    
    else {
      raster_listmodel6[[j]]@data@values[i] <- raster_listmodel6[[j]]@data@values[i]
      
    }
  }
  
}



rasterwatermodel6CGpop1 <- raster_listmodel6[[1]]
rasterwatermodel6CGpop2 <- raster_listmodel6[[2]]
rasterwatermodel6CGpop3 <- raster_listmodel6[[3]]
rasterwatermodel6CGpop4 <- raster_listmodel6[[4]]
rasterwatermodel6CGpop5 <- raster_listmodel6[[5]]
rasterwatermodel6CGpop6 <- raster_listmodel6[[6]]
rasterwatermodel6CGpop7 <- raster_listmodel6[[7]]




plot(rasterwatermodel4CGpop1)
plot(rasterwatermodel6CGpop1)
plot(rasterwatermodel3_2CG)
rasterwatermodel4CGpop7@data@values
rasterwatermodel6CGpop7@data@values

#pop1
tr <- transition( 1/rasterwatermodel6CGpop1, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist[1:7,1:7] 

eDist2 <- eDist
eDist2[1,] <- eDist[1,]
#pop2
tr <- transition( 1/rasterwatermodel6CGpop2, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop

eDist2[2,] <- eDist[2,]
#pop3
tr <- transition( 1/rasterwatermodel6CGpop3, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[3,] <- eDist[3,]

#pop4
tr <- transition( 1/rasterwatermodel6CGpop4, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[4,] <- eDist[4,]

#pop5
tr <- transition( 1/rasterwatermodel6CGpop5, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[5,] <- eDist[5,]

#pop6
tr <- transition( 1/rasterwatermodel6CGpop6, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[6,] <- eDist[6,]

#pop7 
tr <- transition( 1/rasterwatermodel6CGpop7, transitionFunction = mean, directions = 4 )
tr <- geoCorrection( tr, type="c", multpl=FALSE, scl=FALSE)

eDist <- costDistance( tr, CGpoints )
eDist <- as.matrix( eDist )
rownames(eDist) <- colnames(eDist) <- CG_sites$Pop
eDist2[7,] <- eDist[7,]


# done 
eDist2


#Now to run the correlation 


#try average. 

vector1 <- as.vector(eDist2[1,])
vector2 <- as.vector(eDist2[1,])
vector3 <- vector()
eDist3 <- eDist2
for (j in(c (1:length(vector1)))) {
  vector1 <- eDist2[j,]
  vector2 <- eDist2[,j]
  
  for( i in c(1:length(vector1))) {
    vector3[i] <- (vector1[i]+vector2[i])/2
    
  }
  eDist3[j,] <- vector3
}  

#Fst table.
pwfsttable <- read.table("pwfstCG10000bootfixedcovar.txt",header=TRUE)

rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")


df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist3[ lower.tri(eDist3)])
df <- df[ !is.infinite(df$Slope_Distance),]

ggplot(df,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")

cor(df$Genetic_Distance, df$Slope_Distance)
#0.05913078

model6CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model6CG)
# Pvalue 0.6996
summary(model6CG)
#R squared 
#0.003496
AIC(model6CG)
#-428.7241

#relative migration rates

pwfsttable <- read.table("divmigrateCGordered2.txt",header=TRUE)
rownames(pwfsttable) <- colnames(pwfsttable) <- c("1","2","3", "4", "5", "6", "7","8","9","10")

df <- data.frame( Genetic_Distance = pwfsttable[lower.tri(pwfsttable)],
                  Slope_Distance = eDist2[ lower.tri(eDist2)])
df <- df[ !is.infinite(df$Slope_Distance),]
df2 <- data.frame( Genetic_Distance = pwfsttable[upper.tri(pwfsttable)],
                   Slope_Distance = eDist2[ upper.tri(eDist2)])
df2 <- df[ !is.infinite(df$Slope_Distance),]
df3 <- (rbind(df,df2))
ggplot(df3,aes(x=Slope_Distance,y=Genetic_Distance)) + geom_point() + 
  stat_smooth(method=lm, formula = y ~ x) + xlab("Least Cost Distance (Slope Categories)") + ylab("Neis Distance")
cor(df$Genetic_Distance, df$Slope_Distance)
#0.1257076
model6_2CG <- lm(Genetic_Distance~Slope_Distance,data=df)
anova(model6_2CG)
# Pvalue 0.4106
summary(model6_2CG)
#R squared 
#0.0158
AIC(model6_2CG)
#-41.47182

#Model 1
# Correlation =0.07831227
# P-value = 0.6091
# Rsquared = 0.006133
# AIC = -428.8433
#Model 2
# Correlation =0.1041885
# P-value = 0.4958
# Rsquared = 0.01086
# AIC = -429.0576
#Model 3
# Correlation =0.1398613
# P-value = 0.3595
# Rsquared = 0.01956
# AIC = -429.4554
#Model 4
#Fst
# Correlation = 0.07121561
# P-value = 0.6543
# Rsquared = 0.004706
# AIC = -428.7787
# migration rates
# Correlation = 0.05844662
# P-value = 0.7029
# Rsquared = 0.003416
# AIC = -40.90902
#Model 5
#Fst
# Correlation = 0.08312044
# P-value = 0.5872
# Rsquared = 0.006909
# AIC = -428.8784
# migration rates
# Correlation = 0.0895136
# P-value = 0.5587
# Rsquared = 0.008013
# AIC = -41.11706
#Model 6
#Fst
# Correlation = 0.05913078
# P-value = 0.6996
# Rsquared = 0.003496
# AIC = -428.7241
#migration rates
# Correlation = 0.1257076
# P-value = 0.4106
# Rsquared = 0.0158
# AIC = -41.47182


#



#










# summary of all models and species

#TP

 
#Model 1
# Correlation =0.7564
# P-value = 7.245e-05
# Rsquared = 0.5722
# AIC = -127.6346

#Model 2 
# Correlation =0.7523974
# P-value = 8.334e-05
# Rsquared = 0.5661
# AIC = -127.3351


# Model 3
# Correlation = 0.4407
# P-value = 0.04578
# Rsquared = 0.1938
# AIC = -114.3261


# Model 4
# Fst
# Correlation = 0.7355
# P-value = 0.000145
# Rsquared = 0.541
# AIC -126.1542
# MIGRATION 
# Correlation = 0.03319099
# P-value = 0.8959
# Rsquared = 0.001102
# AIC 5.073055

# Model 5
# Fst
# Correlation = 0.7551547
# P-value = 0.0001251
# Rsquared = 0.5633
# AIC -127.1984

#Migration
# Correlation = 0.02384243
# P-value = 0.9183
# Rsquared = 0.0005685
# AIC 5.084261

#Model 6 
# Fst
# Correlation = 0.7019404
# P-value = 0.00039
# Rsquared = 0.4927
# AIC -124.0538

#Migration
# Correlation = 0.03674232
# P-value = 0.8744
# Rsquared = 0.00135
# AIC 5.067833


#AG 
#Model 1
# Correlation =0.6059082
# P-value = 0.0036
# Rsquared = 0.3671
# AIC = -131.2601

#Model 2
# Correlation =0.6745
# P-value = 0.0007977
# Rsquared = 0.455
# AIC = -134.3975

#Model 3
# Correlation =0.3833762
# P-value = 0.08625
# Rsquared = 0.147
# AIC = -124.9913

#Model 4
#Fst 
# Correlation =0.6318646
# P-value = 0.002121
# Rsquared = 0.3993
# AIC = -132.3542

# migration rates 
# Correlation =0.3154773
# P-value = 0.1636
# Rsquared = 0.09953
# AIC = 4.919449

#Model 5
#Fst
# Correlation =0.6315248
# P-value = 0.002137
# Rsquared = 0.3988
# AIC = -132.3392

# migration rates
# Correlation =0.3140548
# P-value = 0.1656
# Rsquared = 0.09863
# AIC = 4.940323
#Model 6
#Fst
# Correlation = 0.5865211
# P-value = 0.005196
# Rsquared = 0.344
# AIC = -130.5067

#migration rates
# Correlation = 0.2559859
# P-value = 0.2627
# Rsquared = 0.06553
# AIC = 5.697697


# CG
#Model 1
# Correlation =0.07831227
# P-value = 0.6091
# Rsquared = 0.006133
# AIC = -428.8433
#Model 2
# Correlation =0.1041885
# P-value = 0.4958
# Rsquared = 0.01086
# AIC = -429.0576
#Model 3
# Correlation =0.1398613
# P-value = 0.3595
# Rsquared = 0.01956
# AIC = -429.4554
#Model 4
#Fst
# Correlation = 0.07121561
# P-value = 0.6543
# Rsquared = 0.004706
# AIC = -428.7787
# migration rates
# Correlation = 0.05844662
# P-value = 0.7029
# Rsquared = 0.003416
# AIC = -40.90902
#Model 5
#Fst
# Correlation = 0.08312044
# P-value = 0.5872
# Rsquared = 0.006909
# AIC = -428.8784
# migration rates
# Correlation = 0.0895136
# P-value = 0.5587
# Rsquared = 0.008013
# AIC = -41.11706
#Model 6
#Fst
# Correlation = 0.05913078
# P-value = 0.6996
# Rsquared = 0.003496
# AIC = -428.7241
#migration rates
# Correlation = 0.1257076
# P-value = 0.4106
# Rsquared = 0.0158
# AIC = -41.47182







rm(speed,tr,TPpoints,udata,vdata,vector1,vector2,vector3,vectorvals,AGpoint_geo,AG_sites,AGpoint_geo2,AGpoints,CG_sites,CGpoint_geo,CGpoint_geo2,CGpoints,world,df,df2,df3,dist_sf,eDist,eDist2,eDist3,matrixavgsAG,matrixavgsCG,matrixavgsTP,matrixdiravgsAG,matrixdiravgsCG,matrixdiravgsTP,matrixdirsdAG,matrixdirsdCG,matrixdirsdTP,matrixdirvarsAG,matrixdirvarsCG,matrixdirvarsTP,matrixsdAG,matrixsdCG,matrixsdTP,pwfsttable,sitemap,studysite,studysite2,dist_raster,dist_raster_reduced, dist_raster_reduced4,grid,grid2,grid3,i,j,lat,lon,path.1,r,raster_list,raster_listmodel5,raster_listmodel6)
rm(matrixvarsAG,matrixvarsCG,matrixvarsTP,tupong_sites,col_dist,dat1,dataset,date,direction,dist,ext,file_URL,rastercurrentavgAG,rastercurrentavgAG2,rastercurrentavgAG4,rastercurrentavgCG,rastercurrentavgCG2,rastercurrentavgCG4,rastercurrentavgTP,rastercurrentavgTP2,rastercurrentavgTP4,rastercurrentsdAG,rastercurrentsdAG2,rastercurrentsdCG,rastercurrentsdCG2,rastercurrentsdTP,rastercurrentsdTP2)
rm(rastercurrentvarAG,rastercurrentvarAG2,rastercurrentvarCG,rastercurrentvarCG2,rastercurrentvarTP,rastercurrentvarTP2,rasterdirectionavgAG,rasterdirectionavgAG2,rasterdirectionavgAG4,rasterdirectionavgCG,rasterdirectionavgCG2,rasterdirectionavgCG4,rasterdirectionavgTP,rasterdirectionavgTP2,rasterdirectionavgTP4,rasterdirectionsdAG,rasterdirectionsdAG2,rasterdirectionsdCG,rasterdirectionsdCG2,rasterdirectionsdTP,rasterdirectionsdTP2,rasterdirectionvarAG,rasterdirectionvarAG2,rasterdirectionvarCG,rastercurrentsdCG2,rasterdirectionvarTP,rasterdirectionvarTP2)
rm(rasterdirectionvarCG2,ncParse)

#

#




#



ncParse <- function( inputFileName, parserOption, variables){
#
# ncParse {ncdf4}
#
# 
# Description
#
# Retrieves all information stored in a NetCDF file.
#
#
# Usage
#
# ncParse( inputFileName, parserOption = NA, variables = NA)
#
# 
# Arguments
#
# inputFileName : OPeNDAP URL or local address of the NetCDF file.
# parserOption : Character string indicating whether to retrieve the entire content of the NetCDF file or 
# only the metadata. If parserOption = 'all' or NA or is omitted (default) then the parser retrieves the entire file, 
# if parserOption = 'metadata' then the parser retrieves metadata only.
# variables :  Character string indicating whether to parse metadata (and data if parserOption = 'all' or NA or omitted) 
# for all variables (if variables = NA or is omitted, default) or only a specified set of variables (e.g. c('TEMP','PSAL')). 
#
#
# Details
#
# The ncParse function is the core of the "IMOS user code library". This function parses a NetCDF file from a 
# local address or an OPeNDAP URL, and harvests its entire 
# content into the workspace.
#
#
# Value
# 
# Returns a list of three sub-lists containing all the information stored in the original NetCDF file. 
# The 'metadata' sub-list stores all the global attributes of the NetCDF file, the 'dimensions' sub-list stores all the 
# information regarding the different dimensions of the NetCDF file, the 'variables' sub-list stores all the 
# data and attributes information of the NetCDF file. 
#
#
# Author
#
# Dr. Xavier Hoenner, IMOS/eMII
# email: xavier.hoenner@utas.edu.au
# Website: http://imos.org.au/  
# Apr 2013; Last revision: 24-Apr-2013
# Copyright 2013 IMOS
# The script is distributed under the terms of the GNUv3 General Public License
#
# 
# References
# 
# The R software is freely available for all operating systems at: http://cran.r-project.org
# The 'ncdf4' package required to run the ncParse function can be downloaded at: http://cirrus.ucsd.edu/~pierce/ncdf/
#
#
# See also
# 
# Additional information about the procedures used to create this NetCDF parser can be found at: 
# https://github.com/aodn/imos_user_code_library/blob/master/IMOS_user_code_library.pdf
#
#
# Example
#
# Parse all data and metadata
# dataset <- ncParse ( '/path/to/netcdfFile.nc' , parserOption = 'all')
#
# Parse data and metadata for both PSAL and TEMP only
# dataset <- ncParse ( '/path/to/netcdfFile.nc', variables = c( 'PSAL', 'TEMP'))
#    
# Parse metadata only for PSAL.
# dataset <- ncParse ( '/path/to/netcdfFile.nc', parserOption = 'metadata', variables = 'PSAL')

ncdf <- nc_open( inputFileName, write=FALSE, readunlim=TRUE, verbose=FALSE)
stopifnot( class( ncdf) == "ncdf4")
if ( missing( parserOption) == TRUE) parserOption <- "all"
if ( is.na( parserOption) == TRUE) parserOption <- "all"
if ( missing( variables) == TRUE) variables <- NA

##### Extract dimension names
varinfos <- list()
for ( i in 1:ncdf$ndims){
	varinfos[[ncdf$dim[[i]]$name]] <- list( id = ncdf$dim[[i]]$id)
}
dimnames <- data.frame( names( varinfos), rep( "Dimension", length( names( varinfos))))
colnames(dimnames) <- c( "Name", "Type")

##### Extract variable names
nvars <- ncdf$nvars
for ( i in 1:ncdf$nvars){
	varinfos[[ncdf$var[[i]]$name]] <- list( natts = ncdf$var[[i]]$natts, dimids = ncdf$var[[i]]$dimids)
}
qcvars <- c( grep( "_quality_control", names( varinfos)), grep( "_QC", names( varinfos)))
histqc <- which( ( names( varinfos)) == "HISTORY_QCTEST")
if ( length( histqc) > 0) qcvars <- qcvars[-which(( qcvars) == histqc)]
vars <- c( 1:nvars)[-qcvars]
varnames <- data.frame( names( varinfos), rep( "Variable", length( names( varinfos))))
varnames[, 2] <- as.character( varnames[, 2])
if( length( qcvars) > 0) varnames[qcvars, 2] <- "QC_Variable"
colnames( varnames) <- c( "Name", "Type")

dimvarqcnames <- rbind( dimnames, varnames)
if ( length( which(( duplicated( dimvarqcnames[, 1])) == TRUE)) > 0) dimvarqcnames <- dimvarqcnames[-which(( duplicated( dimvarqcnames[, 1])) == TRUE),]
dimvar <- if( length( which(( dimvarqcnames[, 2]) == "QC_Variable")) == 0) dimvar <- dimvarqcnames else dimvar <- dimvarqcnames[-which(( dimvarqcnames[, 2]) == "QC_Variable"),]
dimvarqcnames <- as.character( dimvarqcnames[, 1])
dimvarnames <- as.character( dimvar$Name)
dimvartype <- as.character( dimvar$Type)

## Determine which variables to parse
if ( is.na( variables[1]) == FALSE){
    if( is.character( variables) == FALSE) stop( "variables value invalid, variables must be a character string")
	for ( i in 1:length( variables)){
		if( length( which(( dimvarnames) == variables[i])) == 0) stop( paste( "Variable ", variables[i], " is not listed as a variable of this NetCDF file"))
		if( dimvartype[which(( dimvarnames) == variables[i])] == "Dimension") stop( paste( "Variable ", variables[i], " is a dimension but not a variable of this NetCDF file"))
		}
	varsel <- which(( dimvarnames) == variables[1])
	if ( length( variables) > 1) {
		for ( i in 2:length( variables)){
			varsel <- c( varsel, which(( dimvarnames) == variables[i]))
		}
	}
	dimid <- varinfos[[ which(( names( varinfos)) == dimvarnames[ varsel[1]])]]$dimids
	dimid <- dimid[order( dimid)]
	dimsel <- ncdf$dim[dimid[1]+1][[1]]$name
	for ( i in 1:length( varsel)){
	dimid <- varinfos[[which(( names( varinfos)) == dimvarnames[varsel[i]])]]$dimids
	dimid <- dimid[order( dimid)]
	dimsel <- c( dimsel, ncdf$dim[dimid[1]+1][[1]]$name)
		if( length( dimid)>1) {
			for ( j in 2:length( dimid)){
				dimsel <- c( dimsel, ncdf$dim[dimid[j]+1][[1]]$name)
			}
		}
	}
	dimsel <- dimsel[-which(( duplicated( dimsel)) == TRUE)]
	dimselid <- which(( dimvarnames) == dimsel[1])
	if ( length( dimsel)>1) {
		for ( i in 2:length( dimsel)){
			dimselid <- c( dimselid, which(( dimvarnames) == dimsel[i]))
		}
	}
	dimvarnames <- dimvarnames[c( dimselid, varsel)]
	dimvartype <- dimvartype[c( dimselid, varsel)]
}

dataset <- list()
##### Extract global attributes
dataset$metadata <- list( netcdf_filename = ncdf$filename)
if ( ncdf$natts > 0) {
	for ( i in 1:ncdf$natts){
		dataset$metadata[[names( summary( ncatt_get( ncdf, 0))[, 1])[i]]] <- ncatt_get( ncdf, 0)[[i]]
	}
	}

for ( v in 1:length( dimvarnames)){
	if( length( ncatt_get( ncdf, dimvarnames[v])) > 0) {
	data <- ncvar_get( ncdf, dimvarnames[v])
	natts <- varinfos[[which(( names( varinfos)) == dimvarnames[v])]]$natts
	dimid <- varinfos[[which(( names( varinfos)) == dimvarnames[v])]]$dimids
	if( length( dimid) > 0){
	dimid <- dimid[order( dimid)]
	dimension <- ncdf$dim[dimid[1]+1][[1]]$name
	if( length( dimid) > 1) {
		for ( j in 2:length( dimid)){
			dimension <- c( dimension, ncdf$dim[dimid[j]+1][[1]]$name)
		}
	}}} else {
		natts <- 0
		if( length(varinfos[[which(( names( varinfos)) == dimvarnames[v])]]$dimids) > 0) data <- ncvar_get( ncdf, dimvarnames[v]) else data <- ncdf$dim[[v]]$len
		if( length(varinfos[[which(( names( varinfos)) == dimvarnames[v])]]$dimids) > 0) dimid <- varinfos[[which(( names( varinfos)) == dimvarnames[v])]]$dimids
		if( length(varinfos[[which(( names( varinfos)) == dimvarnames[v])]]$dimids) > 0) dimension <- ncdf$dim[dimid[1]+1][[1]]$name
		}

	##### Convert time values into dates
	if ( ( dimvarnames[v] == "TIME" | dimvarnames[v] == "time" | dimvarnames[v] == "JULD" | dimvarnames[v] == "JULD_LOCATION") == TRUE && (length( grep( "days", ncatt_get( ncdf, dimvarnames[v], "units"))) == 1) == TRUE) unit <- 3600*24 else
	if ( (dimvarnames[v] == "TIME" | dimvarnames[v] == "time" | dimvarnames[v] == "JULD" | dimvarnames[v] == "JULD_LOCATION") == TRUE && (length( grep( "hours", ncatt_get( ncdf, dimvarnames[v], "units"))) == 1) == TRUE) unit <- 3600 else unit <- 1
	if ( dimvarnames[v] == "TIME" | dimvarnames[v] == "time" | dimvarnames[v] == "JULD" | dimvarnames[v] == "JULD_LOCATION" ) data <- as.POSIXlt( data*unit, origin=strsplit(ncatt_get( ncdf, dimvarnames[v], "units")$value,split=' ')[[1]][3], tz="UTC")
	##### Extract data from dimensions and variables
	if ( parserOption != "metadata" && parserOption != "all") stop( "parserOption value invalid")
	if ( parserOption == "metadata" && dimvartype[v] == "Dimension") dataset$dimensions[[dimvarnames[v]]] <- list()
	if ( parserOption == "metadata" && dimvartype[v] == "Variable") dataset$variables[[dimvarnames[v]]] <- list( dimensions = dimension)
	if ( parserOption == "all" && dimvartype[v] == "Dimension") dataset$dimensions[[dimvarnames[v]]] <- list( data = data)
	if ( parserOption == "all" && dimvartype[v] == "Variable") dataset$variables[[dimvarnames[v]]] <- list(dimensions = dimension, data = data)
	if ( length( ncatt_get( ncdf, dimvarnames[v])) > 0) { for ( j in 1:length( ncatt_get( ncdf, dimvarnames[v]))){
		if ( dimvartype[v] == "Dimension") dataset$dimensions[[dimvarnames[v]]][[dimnames( summary( ncatt_get( ncdf, dimvarnames[v])))[[1]][j]]] <- ncatt_get( ncdf, dimvarnames[v])[[j]]
		if ( dimvartype[v] == "Variable") dataset$variables[[dimvarnames[v]]][[dimnames( summary( ncatt_get( ncdf, dimvarnames[v])))[[1]][j]]] <- ncatt_get( ncdf, dimvarnames[v])[[j]]
	}}
	
	##### Extract QC data from dimensions and variables
	qcvar_id <- c( which(( dimvarqcnames) == paste( dimvarnames[v], "_quality_control", sep="")), which(( dimvarqcnames) == paste( dimvarnames[v], "_QC", sep="")))
	if ( length( qcvar_id) > 0) {
		qcdata <- ncvar_get( ncdf, dimvarqcnames[qcvar_id])
		nqcatts <- varinfos[[which(( names( varinfos)) == dimvarqcnames[qcvar_id])]]$natts
		if ( parserOption == "all" && dimvartype[v] == "Dimension") dataset$dimensions[[dimvarnames[v]]][[paste( "flags", sep="")]] <- qcdata
		if ( parserOption == "all" && dimvartype[v] == "Variable") dataset$variables[[dimvarnames[v]]][[paste( "flags", sep="")]] <- qcdata
		for ( j in 1:nqcatts){
			if ( dimvartype[v] == "Dimension") dataset$dimensions[[dimvarnames[v]]][[paste( "flags", "_", dimnames( summary( ncatt_get( ncdf, dimvarqcnames[qcvar_id])))[[1]][j], sep="")]] <- ncatt_get( ncdf, dimvarqcnames[qcvar_id])[[j]]
			if ( dimvartype[v] == "Variable") dataset$variables[[dimvarnames[v]]][[paste( "flags", "_", dimnames( summary( ncatt_get( ncdf, dimvarqcnames[qcvar_id])))[[1]][j], sep="")]] <- ncatt_get( ncdf, dimvarqcnames[qcvar_id])[[j]]
		}}

}
# you need to close the connection _before_ you return the dataset otherwise R's con stack gets full and crashes.
# - Dirk Slawinski, 08/06/2014
#return( dataset)
if ( length( grep( "http", inputFileName)) == 1) nc_close( ncdf)
return( dataset)
}
