library(RMySQL)
library(leaflet)
library(tmap)
library(tmaptools)
library(sp)
library(maptools)
library(dbplyr)
library(dplyr)
library(sf)

mydb = dbConnect(MySQL(), user='ucfnhke', password='baleyakuli', dbname='ucfnhke', host='dev.spatialdatacapture.org')
job_id = "'1237','1257','510983','511154','511370','511748','512270','512379'"
job_id = "'1237','1257','1582','1996','2674','2849','3008','3118','3353','3904','4297','4910','5351','5834','5920','6006','6092','6265','6955','7147','7492','7650','7934','8165','8363','8665','9068','9109','9351','9611','9796','10028','10728','10769','10810','11181','11376','11494','11781','11877','11916','12439','12465','12574','12600','12808','13179','13445','13562','13749','13827','14043','14088','14132','14667','14947','15496','15657','15871','15915','16106','16112','16456','16501','16764','16795','17023','17055','17399','17433','17563','17729','17893','18742','18920','19173','19332','19516','20180','20306','20574','20758','21014','21043','21950','22249','22601','22688','22695','23001','23461','23973','24439','24682','24940','25293','25404','25505'"
job_id = "'1237','1257','1582','1996','2674','2849','3008','3118','3353','3904','4297','4910','5351','5834','5920','6006','6092','6265','6955','7147','7492','7650','7934','8165','8363','8665','9068','9109','9351','9611','9796','10028','10728','10769','10810','11181','11376','11494','11781','11877','11916','12439','12465','12574','12600','12808','13179','13445'"
dates = "'2018-10-29' AND '2018-10-30'"


ping <- GetData(rs,dates, job_id)
lines <- GetLines(ping)
origin <- GetOrigin(ping)
destination <- GetDestination(ping)
lastping <-GetLastPing(ping, dates,job_id)

colors <-  data.frame(rainbow(length(unlist(strsplit(job_id,",")))))
colnames(colors) <- "Color"
lines <- SpatialLinesDataFrame(lines, colors )

tm <- tm_shape(lines)+
  tm_lines(col="Color") +
  tm_shape(origin) +
  tm_dots(col="red")+
  tm_shape(destination) +
  tm_dots(col="blue",size=0.20) +
  tm_shape(lastping) +
  tm_dots(col="green")

tmap_leaflet(tm)

#### FUNCTIONS ----
points_to_line <- function(data, long, lat, id_field = NULL,sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0(as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

GetData <-  function(rs,dates,job_id){
  rs = dbSendQuery(mydb, paste("SELECT * FROM ph_pings pings where (CREATED_AT BETWEEN ",dates,") and JOB_ID IN (",job_id,");"))
  ping = fetch(rs, n=-1)
}

GetLines <- function(data){
  
  pings = ping[c(2,12,20,21)]
  
  lines <- points_to_line(data = pings, 
                          long = "PING_LONGITUDE", 
                          lat = "PING_LATITUDE",
                          id_field = "JOB_ID",
                          sort_field = "PING_TIMESTAMP")
  
  #proj4string(lines) <- CRS("+proj=longlat +datum=WGS84")
}

GetOrigin <- function(data){
  origins = ping[!duplicated(ping$JOB_ID),c(2,16,17)]
  origin <- st_as_sf(x = origins, 
                     coords = c("COLLECTION_LONGITUDE", "COLLECTION_LATITUDE"),
                     crs = "+proj=longlat +datum=WGS84")
}

GetDestination <- function(data){
  destinations = ping[!duplicated(ping$JOB_ID),c(2,18,19)]
  destination <- st_as_sf(x = destinations, 
                          coords = c("DELIVERY_LONGITUDE", "DELIVERY_LATITUDE"),
                          crs = "+proj=longlat +datum=WGS84")
}

GetLastPing <- function(data,dates,job_id){
  rs = dbSendQuery(mydb, paste("select max(PING_TIMESTAMP)as Last from ucfnhke.ph_pings 
                   where (CREATED_AT BETWEEN",dates,") and JOB_ID IN (",job_id,")
                   group by JOB_ID"))
  
  lastping = as.character(unlist(fetch(rs, n=-1)))
  
  lastpings <-  filter(data, PING_TIMESTAMP %in% lastping)
  
  lastpingdf <- st_as_sf(x = lastpings, 
                      coords = c("PING_LONGITUDE", "PING_LATITUDE"),
                     crs = "+proj=longlat +datum=WGS84")
  
}

#dbDisconnect(mydb)


