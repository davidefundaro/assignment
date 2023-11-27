# Area from Geo sheet:

GeoData <- read_excel("C:/Users/davide.fundaro/OneDrive - iQera Italia SpA/Desktop/Repository/Data/Metadata_2022_12_21.xlsx", sheet = "Geo")


# find the area from the city:
create_area_city <- function(table, key) {
  for (j in 1:length(key)) {                 
    if (is.na(key[j])) {
      table$area[j] <- NA  
    } else  {
      matches_city <- subset(GeoData, city==key[j]) 
      if(nrow(matches_city)>0){
        table$area[j]  <- matches_city$area
      } else {
        table$area[j]  <- NA 
      }
    }
  }
  return(table)
}


# find the area from the city and province:
create_area_city_prov <- function(table, key1, key2) {
  for (j in 1:length(key1)) {                 
    if (is.na(key1[j]) || is.na(key2[j])) {
      table$area[j] <- NA  
    } else {
      matching_row <- subset(GeoData, city == toupper(key1[j]) & province == toupper(key2[j]))
      if (nrow(matching_row) > 0) {
        table$area[j] <- matching_row$area
      } else {
        table$area[j] <- NA
      }
    }
  }
  return(table)
}
