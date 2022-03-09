#script for a webmap of Cheshire East wards population density, with railway stations 
#plotted over the top 

#clear environment

rm(list = ls())

#install.packages("leaflet")
#install.packages("rgdal")

library(leaflet)
library(rgdal)
library(geojsonio)
library(dplyr)
library(sf)
library(RColorBrewer)

#all_wards <- st_read("C:\\Users\\james.thompson\\Documents\\Wards_(December_2021)_GB_BFC.geojson") 
#st_transform(4326) # safety of unprojected CRS

che_west <- st_read("https://martinjc.github.io/UK-GeoJSON/json/eng/wards_by_lad/topo_E06000050.json") 
#st_transform(4326) # safety of unprojected CRS

che_east <- st_read("https://martinjc.github.io/UK-GeoJSON/json/eng/wards_by_lad/topo_E06000049.json")

joblocs <- st_read("C:\\Users\\james.thompson\\Documents\\stations_che_east.geojson") %>% 
  st_transform(4326) # safety of unprojected CRS   

#join cheshire east data with the csv data which has everything in it 

csv_che_east <- st_read("C:\\Users\\james.thompson\\Documents\\cheshire_east_ward_profile_data.csv")


che_east[5] <- csv_che_east[10]           #col 5 of che east will contain our desired data (pop density).

#first attempt at choropleth map with these wards 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = che_east,
              stroke = TRUE,
              weight = 0.5,
              color = "#37B1ED",
              opacity = 1,
              fillColor = "#37B1ED",
              fillOpacity = 0.5)

#m

#extra numerical data from popl. col of che_east 

pop_den_only = as.numeric(che_east[[5]])
quantile(pop_den_only)

#create population map for these wards 

LSOABins <- c(0, 1, 2, 5, 10, 20,30,50)         #loosely based on quantile estimate
LSOAPal <- colorBin("YlGnBu", bins = LSOABins)  #uses bins defined above


#set pop-up content
che_east$popup <- paste("<strong>",che_east$WD13NM,"</strong>", "</br>", 
                        che_east$WD13CD, "</br>",
                        "Population Density (per hectare): ", prettyNum(che_east$WD13NMW, big.mark = ","))


#add to interactive map

m2 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = che_east,
              stroke = TRUE,
              weight = 0.2,
              color = "ABABAB",
              smoothFactor = 0.3,    #added this in 
              opacity = 0.9,
              fillColor = ~LSOAPal(as.numeric(che_east$WD13NMW)),
              fillOpacity = 0.8,
              popup = ~popup,   #adds a popup
              highlightOptions = highlightOptions(color = "#E2068A", weight = 1.5, 
                                                  fillOpacity = 0.5),    #adds a hover effect
              group = "Popl Density")%>%    #add groups
  
  addCircleMarkers(data = joblocs, radius = 2, stroke = TRUE, color = "#424242", weight = 1, fillOpacity = 1, fillColor ="#FDFDFD", group = "Stations")%>% 
  
  addLegend("bottomright",opacity = 1,
            colors =c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
            title = "Population Density</br>per hectare",
            labels= c("< 1","2 - 5","5 - 10","10 - 20","20 - 30", "30 - 40", "> 40"))%>%     #adds a legend
  
  addProviderTiles(providers$Esri.WorldImagery, group = "Basemap - aerial") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Basemap - greyscale") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Basemap - dark") %>%
  
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Level 1",
    onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
  
  addLayersControl(
    overlayGroups = c("Popl Density"),
    baseGroups = c("Basemap - dark","Basemap - greyscale","Basemap - aerial"),
    options = layersControlOptions(collapsed = TRUE))              

m2


