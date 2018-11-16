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
            dplyr::mutate(country = coords2country(.)) %>% 
            dplyr::mutate(ISO3 = countrycode::countrycode(country, "country.name","iso3c"))
      
      # Get spatial polygons for all countries
      allac2 <- do.call("bind", lapply(as.character(unique(data$ISO3)), 
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
            dplyr::select(Country, County, Region, Longitude, Latitude)
      
      # Remove all the downloaded .rds files
      unlink(list.files(pattern = "\\.rds$"))
      
      return(points_with_zones)
}

test <- read.csv("SampleData.csv")
new <- latlong2regions(test)
