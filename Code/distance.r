
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
  clusters <- unique(sf$cluster_id)
  distance <- rep(NA, length(clusters))     # create list for distances
  
  print("for loop 1")
  
  for(i in 1:length(clusters)) {
    
    print(i)
    print(sf$cluster_id)
    points <- filter(sf,cluster_id==clusters[i])    # throws error when it tries to coerce geometry column to a double
    # points <- sf[sf$cluster_id == clusters[1]]    # when using current subset of data from Arthur
    print('a')
    origin_points <- filter(points,type=="origin")
    print('b')
    destination_points <- filter(points,type=="destination")
    print('c')
    distance[i] <- estimate_distance(origin_points,destination_points)
  }
  
  return (cbind(clusters,distance))
}

#  error message to be debugged is:
# Error in data.matrix(data) : 
#   (list) object cannot be coerced to type 'double'
# In addition: Warning message:
#   In data.matrix(data) : NAs introduced by coercion


