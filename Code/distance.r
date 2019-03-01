
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
  distance <- rep(NA, length(clusters))
  for(i in 1:length(clusters)) {
    points <- filter(sf,cluster_id==clusters[i])
    origin_points <- filter(points,type=="origin")
    destination_points <- filter(points,type=="destination")
    distance[i] <- estimate_distance(origin_points,destination_points)
  }
  return (cbind(clusters,distance))
}
