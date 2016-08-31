FiltersOutCountries <- function(world) {
  ## filter out the southern half
  ## those countries, whose maximum point is under the equator should be cut out simply
  ## those who meet the equador once, should be cut there
  
  world$long[world$long > 50] <- 50
  world$long[world$long < -30] <- -30
  world$lat[world$lat < 30] <- 30
  
  summary_of_countries <- ddply(.data = world, .variables = "region", summarize,
                               maxLat = max(lat), 
                               minLat = min(lat),
                               minLong = min(long),
                               maxLong = max(long))
  removable_countries <- c(summary_of_countries$maxLat == summary_of_countries$minLat |
                             summary_of_countries$minLong == summary_of_countries$maxLong)
  world <- world[!(world$region %in% removable_countries), ]
  # 
  # bottom <- minlat_of_countries$region[minlat_of_countries$minLat < 30]
  # left <- minlong_of_countries$region[minlong_of_countries$minLong < -50]
  # right <- maxlong_of_countries$region[maxlong_of_countries$maxLong > 75]
  # 
  # world <- ddply(world, .variables =  "region", .fun =  function(df){
  #   if(unique(df$region) %in% bottom) {
  #     df$lat[df$lat < 30] <- 30
  #     #browser()
  #     df
  #   } else if(unique(df$region) %in% left) {
  #     df$lat[df$long < -50] <- -50
  #     #browser()
  #     df
  #   } else if(unique(df$region) %in% right) {
  #     df$long[df$long > 75] <- 75
  #     #browser()
  #     df
  #   } else {
  #     df
  #   }
  # })
  world
}