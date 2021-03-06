###################################################
#Clusters for batching pickups and drop offs
###################################################

# source Terry's functions

source('~/CS/Parcel_Hack_2019/Code/distance.r', echo=TRUE)


# Get data from DB

# install.packages("RMySQL")

require(RMySQL)

mydb <-  dbConnect(MySQL(), user='ucfnhke', password='baleyakuli', dbname='ucfnhke', host='dev.spatialdatacapture.org')

# dbSendQuery builds the query string
# fetch executes the query
# dbClearResults() frees memory resources associated with the query, not clear that its diferent from rm()

names_query_4 <- dbGetQuery(mydb, paste("SHOW TABLES;"))
print(names_query_4)

# sample <- dbGetQuery(mydb, "SELECT * FROM ph_pings LIMIT 10")

table_headers <- dbGetQuery(mydb, paste("SHOW columns FROM ph_data;"))
print(table_headers)

jobs_data <- dbSendQuery(mydb, paste("SELECT JOB_ID, COLLECTION_LATITUDE, COLLECTION_LONGITUDE, DELIVERY_LATITUDE, DELIVERY_LONGITUDE FROM PH_Jobs"))
# jobs_data <- dbSendQuery(mydb, paste("SELECT JOB_ID, COLLECTION_LATITUDE, COLLECTION_LONGITUDE, DELIVERY_LATITUDE, DELIVERY_LONGITUDE FROM PH_Jobs"))
data <- fetch(jobs_data, n = -1)



dbClearResult(jobs_data)

# test_query = dbSendQuery(mydb, paste("SELECT * FROM cities"))
# results = dbFetch(test_query, n=10)
# dbClearResult(test_query)

# or?
# test_query_1 = dbGetQuery(mydb, paste("SELECT * FROM cities"))
# rm(test_query_1)

# info about database


# results_4 = dbFetch(names_query_4, n = -1)
# dbClearResult(names_query_4)

# names_query_5 = dbGetQuery(mydb, paste("Select * FROM cities"))

######################################################################

# convert to BNG

# Done with SP....
require(rgdal)
require(sp)
# require(sf)


# convert collection coords

# Use sf instead? 
# unreasonably conplex because sf builds the lat and long column into a list that then 
# needs to be disaggregated
# data_1 <- st_as_sf(data, coords = c('COLLECTION_LONGITUDE','COLLECTION_LATITUDE'), crs = 4326)
# data_1 <- st_transform(data_1, crs = 27700)
# data_1 <- as.data.frame(data_1)



coordinates(data) <- c('COLLECTION_LONGITUDE', 'COLLECTION_LATITUDE')
#set current CRS
proj4string(data) <- CRS("+init=epsg:4326")
# BNG Coordinates
CRS.new <- CRS("+init=epsg:27700")
# Convert
data <- spTransform(data,CRS.new)
#Convert back to dataframe
data <- as.data.frame(data)


# convert delivery coords
coordinates(data) <- c('DELIVERY_LONGITUDE', 'DELIVERY_LATITUDE')
proj4string(data) <- CRS("+init=epsg:4326")
data <- spTransform(data,CRS.new)
data <- as.data.frame(data)

# Rename as easting and northing instead of lat long

names(data)[names(data) == 'COLLECTION_LONGITUDE'] <- 'COLLECTION_NORTHING'
names(data)[names(data) == 'COLLECTION_LATITUDE'] <- 'COLLECTION_EASTING'
names(data)[names(data) == 'DELIVERY_LONGITUDE'] <- 'DELIVERY_NORTHING'
names(data)[names(data) == 'DELIVERY_LATITUDE'] <- 'DELIVERY_EASTING'



######################################################################
# Do Clustering analysis

require(stats)

# don't need to scale because BNG grid is square
# my_data = scale(results_4)

data_subset <- data.frame(data$COLLECTION_NORTHING,data$COLLECTION_EASTING, data$DELIVERY_NORTHING, data$DELIVERY_EASTING)

distance_matrix <- dist(data_subset, method = "euclidean")

fit <- hclust(distance_matrix, method="ward.D2")

# plot(fit)

###########################################################################
###########################################################################
# Change this line to get different number of clusters
groups <- cutree(fit, k = 400, h = NULL)         # k is number of clusters, h is radius of clusters

# rect.hclust(fit, k = 5, border = "red")

###############################################################

# Add Cluster ID attribute to dataframe

# require(tidyverse)

data <- data_holder
data$Group_ID <- groups

data_1 <- data.frame(data$JOB_ID, data$COLLECTION_NORTHING, data$COLLECTION_EASTING, data$Group_ID)
data_2 <- data.frame(data$JOB_ID,data$DELIVERY_NORTHING,data$DELIVERY_EASTING,data$Group_ID)

names(data_1)[names(data_1) == 'data.COLLECTION_NORTHING'] <- 'NORTHING'
names(data_1)[names(data_1) == 'data.COLLECTION_EASTING'] <- 'EASTING'
data_1$type <- rep("origin",nrow(data_1))
names(data_2)[names(data_2) == 'data.DELIVERY_NORTHING'] <- 'NORTHING'
names(data_2)[names(data_2) == 'data.DELIVERY_EASTING'] <- 'EASTING'
data_2$type <- rep("destination",nrow(data_2))
data <- rbind(data_1, data_2)

names(data)[names(data) == 'data.Group_ID'] <- 'cluster_id'

# write.csv(data, file = "Clustered_Data_Set.csv")

###################################################################################
require(sf)
data <- st_as_sf(data, coords = c("NORTHING", "EASTING"), crs = "+init=epsg:27700")

distances_400 <-  cluster_distance(data)

count <- c(10,20,50,75,100,125,150,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100)
dist <- c(mean(distances),mean(distances_1),mean(distances_2),mean(distances_3),mean(distances_4),mean(distances_5),
          mean(distances_6),mean(distances_7),mean(distances_8),mean(distances_400),mean(distances_9),mean(distances_10),mean(distances_11),
          mean(distances_12),mean(distances_13),mean(distances_14),mean(distances_15),mean(distances_16),mean(distances_17),
          mean(distances_18),mean(distances_19),mean(distances_20),mean(distances_21),mean(distances_22),mean(distances_23),
          mean(distances_24),mean(distances_25))

count <- c(600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100)
dist <- c(mean(distances_10),mean(distances_11),
          mean(distances_12),mean(distances_13),mean(distances_14),mean(distances_15),mean(distances_16),mean(distances_17),
          mean(distances_18),mean(distances_19),mean(distances_20),mean(distances_21),mean(distances_22),mean(distances_23),
          mean(distances_24),mean(distances_25))

frame <- data.frame(count, dist)
frame$distance_km <- frame$dist/1000
frame_subset <- data.frame(frame$count, frame$distance_km)
names(frame_subset)[names(frame_subset) == 'frame.count'] <- 'Number of Couriers'
names(frame_subset)[names(frame_subset) == 'frame.distance_km'] <- 'Average Straightline Distance Travelled'
plot(frame_subset)
