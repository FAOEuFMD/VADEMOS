# maps
mapdata <-subset(data, Year == 2020)
mapdata<- subset(mapdata, Species== 'LR')

url<- "https://api.mapbox.com/styles/v1/pilipili/cl4h4ht3r001015t5xf2cuu25/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoicGlsaXBpbGkiLCJhIjoiY2wyOWdka2JwMGd5NjNrbzg1NXRndW1mbiJ9.Msfezuof3dDDJJ7xNwe-qQ"

#polygons
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="TM_WORLD_BORDERS_SIMPL-0.3.zip")
unzip('TM_WORLD_BORDERS_SIMPL-0.3.zip')

world <-st_read('TM_WORLD_BORDERS_SIMPL-0.3.shp') %>% as_Spatial()
world <-merge(world, mapdata, by.x='ISO3', by.y='Country_code')

palpool<- colorFactor('viridis', domain = world$Pool, na.color='grey' )
pcpcolors= c('#fd030e','#ef8125','#fccc19','#4f8e32','#15592b')
palpcp<- colorFactor(pcpcolors, domain = c(0,1,2,3,4), na.color='grey' )
#serocolors= c('#fd030e','#ef8125','#fccc19','#4f8e32','#15592b')
palsero<- colorFactor('magma', domain = world$Serotype, na.color='grey' )
palout<- colorFactor('black', domain = data$TotalOutbreaks, na.color= NA)



map<-leaflet(world) %>%
  setView(15, 46, zoom = 3) %>%
  
  addTiles(urlTemplate = url) %>%
  #addPolygons( weight=0, fill = FALSE)%>%
  addPolygons(stroke= FALSE, fillOpacity= 1, color= ~palpool (Pool), group= 'Pool') %>%
  addPolygons(stroke= FALSE, fillOpacity= 1, color= ~palpcp (PCP), group= 'PCP')%>%
  addPolygons(stroke= FALSE, fillOpacity= 1, color= ~palsero (Serotype), group= 'Serotype')%>%
  #addHeatmap(data=data, lng= ~LON, lat= ~LAT, blur= 3, radius= 20, color = ~palout(TotalOutbreaks), fillColor = ~palout(TotalOutbreaks), group= 'Outbreaks')%>%
  addCircles(data=data, lng= ~LON, lat= ~LAT, color = ~palout(TotalOutbreaks), fillColor = ~palout(TotalOutbreaks), 
              radius = data$TotalOutbreaks, popup = paste('Outbreaks', ~TotalOutbreaks), label = ~TotalOutbreaks, group='Outbreaks')%>%
  

  addLayersControl (baseGroups = c('PCP (default)', 'Pool', 'Serotype' ),
    overlayGroups= c('Outbreaks', 'Results'), 
                    options= layersControlOptions(collapsed=FALSE))%>%
  
  addLegend(map, position= 'bottomright', pal=palpcp, opacity= 1, title= 'PCP Stage', group= 'PCP')%>%
  addLegend(map, position= 'bottomright', pal=palpool, opacity= 1, title= 'Pool', group= 'Pool')
  

  
#   addLegend(data= 'bottomright', group='', pal=pal, values= - nombre columna, title= '', opacity= 0.8 )
#   
# 
map



