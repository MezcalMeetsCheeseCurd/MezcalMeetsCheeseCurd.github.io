library(googlesheets)
library(tidyverse)
library(jsonlite)
library(leaflet)
library(htmlwidgets)
#devtools::install_github("dkahle/ggmap")
library(geonames)
library(httr)
library(ggmap)
library(leaflet)
library(htmltools)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(viridis)


options("ggmap")

## read recs sheet
gs_ls("OaxacaRecs")

OaxRec <- gs_title("OaxacaRecs")

OaxDF <- OaxRec %>% gs_read("Sheet1")

## geocode

register_google(key = googleApi,account_type = "standard" )

addsplit[a]

options(geonamesUsername="cjschwantesll")

latlongGN <- GNsearch(name = na.omit(OaxDF$Address), maxRows = 1 )

geocodeQueryCheck()

length(na.omit(OaxDF$Address))


df_test <- geocode(location = na.omit(OaxDF$Address),output = "latlon")

OaxDF[!is.na(OaxDF$Address),c(5,6)] <- df_test[,c(2,1)]

OaxDF$X8 <- paste0("<h3><a href=", OaxDF$Website,">",OaxDF$Name,"</a></h3>","<hr>",
                   "<i>",OaxDF$Category,"</i>", "<hr>",
                   "<p>",OaxDF$Description,"</p>")



## create leaflet 

mLat <- mean(OaxDF$Lat,na.rm = T)
mLong <-  mean(OaxDF$Long,na.rm = T)

OaxDF$Category <- gsub(pattern = "drinks",replacement = "Drinks",x = OaxDF$Category)

ColPal <- viridis(n = length(unique(OaxDF$Category)),alpha = 0)



#because we cant use Hex colors for some reason (dumb)
ColorsMarkers <-c("red", "darkred","orange", "blue","cadetblue", "lightblue", "purple")

ColDf <- tibble(Category = unique(OaxDF$Category), Color = ColorsMarkers)

OaxDF <- left_join(x = OaxDF,y = ColDf,"Category")


icons <- awesomeIcons(
  icon = 'coffee',
  iconColor = "#440154",
  library = 'fa',
  markerColor = 
)


unique(OaxDF$Category)



iconCustom <- awesomeIconList(
  
  Breakfast = makeAwesomeIcon(icon = "coffee",library = 'fa',markerColor = "red",iconColor = "#ffffff"),
  Dinner = makeAwesomeIcon(icon = "cutlery",library = 'fa',markerColor = "darkred",iconColor = "#ffffff"),
  Market = makeAwesomeIcon(icon = "shopping-basket",library = 'fa',markerColor = "orange",iconColor = "#ffffff"),
  Drinks = makeAwesomeIcon(icon = "glass",library = 'fa',markerColor = "blue",iconColor = "#ffffff"), 
  Historical = makeAwesomeIcon(icon = "globe",library = 'fa',markerColor = "cadetblue",iconColor = "#ffffff"),
  Museum = makeAwesomeIcon(icon = "university",library = 'fa',markerColor = "darkpurple",iconColor = "#ffffff"),
  Nature = makeAwesomeIcon(icon = "tree",library = 'fa',markerColor = "cadetblue",iconColor = "#ffffff"),
  Wedding = makeAwesomeIcon(icon = "heart",library = 'fa',markerColor = "purple",iconColor = "#ffffff")
)

m <- leaflet(data = OaxDF) %>% 
  setView(lat = 17.065556,lng = -96.723056,zoom = 15) %>% 
  addTiles(urlTemplate = 'http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>% 
addAwesomeMarkers(~Long, ~Lat, icon = ~iconCustom[Category], popup = ~X8,label = ~htmlEscape(Name)) 

create.d

saveWidget(m,file = "./recs_map.html",selfcontained = T)

## would like to create json for leaflet map




