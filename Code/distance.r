
estimate_distance <- function (start_points,end_points)  {
  
  require(sf)
  start_centroid <- st_centroid(start_points)
  end_centroid <- st_centroid(end_points)
  start_stem_distance <- st_distance(start_points,start_centroid,tolerance=10) %>% sum()
  end_stem_distance<- st_distance(end_points,end_centroid,tolerance=10) %>% sum()
  mid_distance <- st_distance(start_centroid,end_centroid,tolerance=10)
  
  return (start_stem_distance+mid_distance+end_stem_distance)
}


cluster_distance <- function(sf) {
  
  require(sf)
  require(dplyr)
  clusters <- unique(sf$cluster_id)
  distance <- rep(NA, length(clusters))     # create list for distances
  
  for(i in 1:length(clusters)) {
    
    points <- filter(sf,cluster_id==clusters[i])    # throws error when it tries to coerce geometry column to a double
    
    origin_points <- filter(points,type=="origin")
    destination_points <- filter(points,type=="destination")
    
    distance[i] <- estimate_distance(origin_points,destination_points)
  }
  
  return (cbind(clusters,distance))
}

#  error message to be debugged is:
# Error in data.matrix(data) : 
#   (list) object cannot be coerced to type 'double'
# In addition: Warning message:
#   In data.matrix(data) : NAs introduced by coercion


