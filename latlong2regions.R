rm(list = ls(all=TRUE))
library(raster)
library(dplyr)
library(sp)
library(rworldmap)
library(countrycode)

latlong2regions <- function(data) {
      
      # Make function to derive country name from the coordinates
      coords2country = function(points)
      {  
            countriesSP <- rworldmap::getMap(resolution='low')
            pointsSP = sp::SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
            indices = sp::over(pointsSP, countriesSP)
            indices$ADMIN  
      }
      
      # Get country names from the coordinates, convert to 3 character ISO names
      data <- test %>% 
            # Ensure that coordinates are in numeric format, and are converted to numeric without alteration
            dplyr::mutate(Longitude = as.numeric(as.character(Longitude)), Latitude = as.numeric(as.character(Latitude))) %>%
            # Replace NA values with 0, to avoid error in coords2country
            dplyr::mutate(Longitude = ifelse(is.na(Longitude), 0, Longitude),
                   Latitude = ifelse(is.na(Latitude), 0, Latitude)) %>% 
            dplyr::mutate(country = coords2country(.)) %>% 
            dplyr::mutate(ISO3 = countrycode::countrycode(country, "country.name","iso3c"))
      
      # Get spatial polygons for all countries
      allac2 <- do.call("bind", lapply(as.character(unique(data$ISO3[!is.na(data$ISO3)])), 
                                       function(x) raster::getData('GADM', country=x, level=2)))

      # Convert the points to SPDF
      sp::coordinates(data) <- ~Longitude + Latitude
      
      # Assign CRS to 'data', using the allac2 projection
      sp::proj4string(data) <- sp::proj4string(allac2)
      
      # Retrieve attributes from 'allac2', that are overlaying points in 'data'
      points_with_zones <- sp::over(data, allac2) %>% 
            as.data.frame() %>% 
            # Add coordinates from'data' bnack to the new attribute table
            cbind(., as.data.frame(data)) %>% 
            # Rename some columns to make them more readable
            dplyr::rename(Country = NAME_0, County = NAME_1, Region = NAME_2) %>% 
            # Select only necessary columns
            dplyr::select(Country, County, Region, Longitude, Latitude) %>% 
            # Replace zeroes in coordinates with NA
            dplyr::mutate(Longitude = ifelse(Longitude == 0, NA, Longitude),
                   Latitude = ifelse(Latitude == 0, NA, Latitude))
      
      # Remove all the downloaded .rds files
      unlink(list.files(pattern = "\\.rds$"))
      
      return(points_with_zones)
}
