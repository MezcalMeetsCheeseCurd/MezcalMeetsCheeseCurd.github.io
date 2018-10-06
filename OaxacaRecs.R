library(googlesheets)
library(tidyverse)
library(jsonlite)
library(leaflet)
library(htmlwidgets)
devtools::install_github("dkahle/ggmap")
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

OaxDF[!is.na(OaxDF$Address),c('Lat','Long')] <- df_test[,c(2,1)]

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

OaxDF$Category <- gsub(pattern = "Ice Cream",replacement = "Ice-Cream",x = OaxDF$Category)

iconCustom <- awesomeIconList(
  
  Breakfast = makeAwesomeIcon(icon = "coffee",library = 'fa',markerColor = "red",iconColor = "#ffffff"),
  Dinner = makeAwesomeIcon(icon = "cutlery",library = 'fa',markerColor = "darkred",iconColor = "#ffffff"),
  Market = makeAwesomeIcon(icon = "shopping-basket",library = 'fa',markerColor = "orange",iconColor = "#ffffff"),
  Drinks = makeAwesomeIcon(icon = "glass",library = 'fa',markerColor = "blue",iconColor = "#ffffff"), 
  Historical = makeAwesomeIcon(icon = "globe",library = 'fa',markerColor = "cadetblue",iconColor = "#ffffff"),
  Museum = makeAwesomeIcon(icon = "university",library = 'fa',markerColor = "darkpurple",iconColor = "#ffffff"),
  Nature = makeAwesomeIcon(icon = "tree",library = 'fa',markerColor = "cadetblue",iconColor = "#ffffff"),
  Wedding = makeAwesomeIcon(icon = "heart",library = 'fa',markerColor = "purple",iconColor = "#ffffff"),
  Chocolate = makeAwesomeIcon(icon = "heart",library = 'fa',markerColor = "black",iconColor = "#ffffff"),
  "Ice-Cream" = makeAwesomeIcon(icon = "smile-o",library = 'fa',markerColor = "lightblue",iconColor = "#ffffff")
  )

m <- leaflet(data = OaxDF) %>% 
  setView(lat = 17.065556,lng = -96.723056,zoom = 15) %>% 
  addTiles(urlTemplate = 'http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png',options= providerTileOptions(detectRetina = T )) %>% 
addAwesomeMarkers(~Long, ~Lat, icon = ~iconCustom[Category], popup = ~X8,label = ~htmlEscape(Name)) 

saveWidget(m,file = "./recs_map.html",selfcontained = T)

## would like to create json for leaflet map

## numbered map

OaxRex <- OaxDF %>% 
  filter(!is.na(ListNumber))

dir.create("./NumIcons")

PNGPATHS <- list()

ColPal <- viridis(n = max(OaxRex$ListNumber),alpha = 1,option = "C")

for(i in 1:length(OaxRex$Name)) { 
pathPng <- paste0("./NumIcons/",i,".png")
PNGPATHS[[i]] <- pathPng
  
ggplot()+
  annotate(geom = "segment", x = 1,xend = 1,y = .25,yend = .75, color = ColPal[i], size = 2) +
  annotate(geom = "point",x = 1,y = .75,size = 30,color = ColPal[i] ) +
  annotate(geom = "text",x = 1, y = .75, label = as.character(i),size = 14, color = "white") +
 # ylim(c(0,.6)) +
  theme_void() +
  coord_fixed(4,xlim = c(0,2),ylim = c(0.0,1))
  
  ggsave(filename = pathPng,device = "png",bg = "transparent",units = "in", width = 1,height = 2)
  
}



aptha <- unlist(PNGPATHS)

iconWidth <- 40
iconHeight <- 80

NumIcons <- iconList(
  "1" = makeIcon(aptha[1], iconWidth = iconWidth, iconHeight = iconHeight),
  "2" = makeIcon(aptha[2], iconWidth = iconWidth, iconHeight = iconHeight),
  "3" = makeIcon(aptha[3], iconWidth = iconWidth, iconHeight = iconHeight),
  "4" = makeIcon(aptha[4], iconWidth = iconWidth, iconHeight = iconHeight),
  "5" = makeIcon(aptha[5], iconWidth = iconWidth, iconHeight = iconHeight),
  "6" = makeIcon(aptha[6], iconWidth = iconWidth, iconHeight = iconHeight),
  "7" = makeIcon(aptha[7], iconWidth = iconWidth, iconHeight = iconHeight),
  "8" = makeIcon(aptha[8], iconWidth = iconWidth, iconHeight = iconHeight),
  "9" = makeIcon(aptha[9], iconWidth = iconWidth, iconHeight = iconHeight),
  "10" = makeIcon(aptha[10], iconWidth = iconWidth, iconHeight = iconHeight),
  "11" = makeIcon(aptha[11], iconWidth = iconWidth, iconHeight = iconHeight),
  "12" = makeIcon(aptha[12], iconWidth = iconWidth, iconHeight = iconHeight),
  "13" = makeIcon(aptha[13], iconWidth = iconWidth, iconHeight = iconHeight),
  "14" = makeIcon(aptha[14], iconWidth = iconWidth, iconHeight = iconHeight),
  "15" = makeIcon(aptha[15], iconWidth = iconWidth, iconHeight = iconHeight),
  "16" = makeIcon(aptha[16], iconWidth = iconWidth, iconHeight = iconHeight),
  "17" = makeIcon(aptha[17], iconWidth = iconWidth, iconHeight = iconHeight),
  "18" = makeIcon(aptha[18], iconWidth = iconWidth, iconHeight = iconHeight),
  "19" = makeIcon(aptha[19], iconWidth = iconWidth, iconHeight = iconHeight),
  "20" = makeIcon(aptha[20], iconWidth = iconWidth, iconHeight = iconHeight),
  "21" = makeIcon(aptha[21], iconWidth = iconWidth, iconHeight = iconHeight)
)

tileOptions()
r <- leaflet(data = OaxRex, options = leafletOptions(zoomDelta = 0.25, zoomSnap= 0.25)) %>% 
  setView(lat = 17.065556,lng = -96.723056,zoom = 15) %>% 
  addProviderTiles(providers$Stamen.Toner,options = providerTileOptions(detectRetina = T)) %>% 
  addMarkers(~Long, ~Lat, icon = ~NumIcons[ListNumber], popup = ~X8,label = ~htmlEscape(Name)) 

r

saveWidget(r,file = "./rexNum_map.html",selfcontained = T)
