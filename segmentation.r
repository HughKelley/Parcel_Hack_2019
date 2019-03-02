#### segmentation

euclid_distance <- function (a_northing,a_easting,b_northing,b_easting){
  northing <- abs(a_northing-b_northing)
  easting <- abs(a_easting-b_easting)
  return(sqrt(northing^2 +easting^2))
}


nearest <- function(a_northing,a_easting,b_northing_list,b_easting_list){
  d <- 1e99
  g <- NA
  for (i in 1:length(b_northing_list)){
    e <- euclid_distance(a_northing,a_easting,b_northing_list[i],b_easting_list[i])
    if (e < d){
      d <- e
      g <- i
    }
  }
  return(c(b_northing_list[g],b_easting_list[g]))
}

segment <- function(collection_northing,collection_easting,delivery_northing,delivery_easting,hub_northing_list,hub_easting_list){
  seg1 <- nearest(collection_northing,collection_easting,c(delivery_northing,hub_northing_list),c(delivery_easting,hub_easting_list))
  seg3 <- nearest(delivery_northing,delivery_easting,c(seg1[1],hub_northing_list),c(seg1[2],hub_easting_list))
  return(c(seg1,seg3))
  }
data2[1,]

hub <- data.frame(rbind(cbind(NORTHING=530000,EASTING=182000),cbind(NORTHING=520000,EASTING=181000))  )
data2 <- cbind(data,SEGMENT_ID=NA)
for (i in 1:length(data$JOB_ID)){
  t <- data2[i,]
  s <- segment(t$COLLECTION_NORTHING,t$COLLECTION_EASTING,t$DELIVERY_NORTHING,t$DELIVERY_EASTING,hub$NORTHING,hub$EASTING)
  r1 <- t
  r1$DELIVERY_NORTHING <- s[1]
  r1$DELIVERY_EASTING <- s[2]
  r1$SEGMENT_ID=1
  r2 <- t
  r2$COLLECTION_NORTHING <- s[1]
  r2$COLLECTION_EASTING <- s[2]
  r2$DELIVERY_NORTHING <- s[3]
  r2$DELIVERY_EASTING <- s[4]
  r2$SEGMENT_ID=2
  r3 <- t
  r3$COLLECTION_NORTHING <- s[3]
  r3$COLLECTION_EASTING <- s[4]
  r3$SEGMENT_ID=3
  r <- rbind(r1,r2,r3)
  data2 <- rbind(data2,r)  
}
