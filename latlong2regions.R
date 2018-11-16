rm(list = ls(all=TRUE))
library(raster)
library(dplyr)
library(sp)
library(rworldmap)
library(countrycode)
test <- read.csv("test.csv")

latlong2regions <- function(data) {
      # Make function to derive country name from the coordinates
      coords2country = function(points)
      {  
            countriesSP <- rworldmap::getMap(resolution='low')
            pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
            indices = sp::over(pointsSP, countriesSP)
            indices$ADMIN  
      }
      
      # Get country names from the coordinates, convert to 3 character ISO names
      data <- data %>% 
            dplyr::mutate(country = coords2country(.)) %>% 
            dplyr::mutate(ISO = countrycode(country, "country.name","iso3c"))
      
      # Get spatial polygons for all countries
      allac2 <- do.call("bind", lapply(as.character(unique(data$ISO)), 
                                       function(x) raster::getData('GADM', country=x, level=2)))

      # Convert the points to SPDF
      coordinates(data) <- ~Longitude + Latitude
      
      # Assign CRS to 'data', using the allac2 projection
      proj4string(data) <- proj4string(allac2)
      
      # Retrieve attributes from 'allac2', that are overlaying points in 'data'
      points_with_zones <- sp::over(data, allac2) %>% 
            as.data.frame() %>% 
            # Add coordinates from'data' bnack to the new attribute table
            merge(., as.data.frame(data), by = "ISO") %>% 
            # Rename some columns to make them more readable
            dplyr::rename(Country = NAME_0, County = NAME_1, Region = NAME_2) %>% 
            # Select only necessary columns
            dplyr::select(Country, County, Region, Longitude, Latitude)
            
}

latlong2regions(test)
