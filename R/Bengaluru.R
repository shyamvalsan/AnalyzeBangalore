library(rgdal)
library(leaflet)
library(htmlwidgets)
library(webshot)

blr <- readOGR("data", "BBMP-Wards")
blr_wards <- read.csv("data/blr_wards.csv", sep=",", dec=".")

# Converting special characters to numeric
blr_wards[,18] <- as.numeric(gsub(",", "", blr_wards[,18]))
blr_wards[,19] <- as.numeric(gsub("%", "", blr_wards[,19]))

# Add column with 2001 population value
blr_wards[,20] <- as.integer((100*blr_wards$Population2011)/(blr_wards$DecadalPopnGrowthRate + 100))
names(blr_wards)[20] <- "Population2001"

# Add column with 2021 population value
blr_wards[,21] <- as.integer(((blr_wards$DecadalPopnGrowthRate*blr_wards$Population2011)/100)+blr_wards$Population2011)
names(blr_wards)[21] <- "Population2021"

# 2001 choropleth

palettePopulation <- colorNumeric("OrRd",domain = c(0,10000,15000,20000,25000,30000,50000,60000,70000,80000,90000,100000))
popupPopulation <- paste0(blr_wards$WardName,":",blr_wards$Population2001)

choropleth2001 <- leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(minZoom = 10, maxZoom = 16)) %>%
  addPolygons(data = blr, fillColor = ~palettePopulation(blr_wards$Population2001),
              fillOpacity = 0.6, color = "darkgrey", weight = 1.5, 
              popup = popupPopulation, group = "Population") %>%
  addLegend(position = 'topright', 
            pal = palettePopulation,
            values = blr_wards$Population2001,
            opacity = 0.6,    
            title = "Population") %>%
  addLabelOnlyMarkers(
    lng = 77.45, lat = 13.18,
    label = "Bangalore Population 2001",
    labelOptions = labelOptions(noHide = T,textsize = "20px"))

saveWidget(choropleth2001, 'choropleth2001.html', selfcontained = FALSE)
webshot('choropleth2001.html', file="images/Blr2001.png", cliprect = 'viewport')

# 2011 choropleth 

palettePopulation <- colorNumeric("OrRd",domain = blr_wards$Population2011)
popupPopulation <- paste0(blr_wards$WardName,":",blr_wards$Population2011)

choropleth2011 <- leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(minZoom = 10, maxZoom = 16)) %>%
  addPolygons(data = blr, fillColor = ~palettePopulation(blr_wards$Population2011),
              fillOpacity = 0.6, color = "darkgrey", weight = 1.5,
              popup = popupPopulation, group = "Population") %>%
  addLegend(position = 'topright',
            pal = palettePopulation,
            values = blr_wards$Population2011,
            opacity = 0.6,
            title = "Population") %>%
  addLabelOnlyMarkers(
    lng = 77.45, lat = 13.18,
    label = "Bangalore Population 2011",
    labelOptions = labelOptions(noHide = T,textsize = "20px"))

saveWidget(choropleth2011, 'choropleth2011.html', selfcontained = FALSE)
webshot('choropleth2011.html', file="images/Blr2011.png", cliprect = 'viewport')

# 2021 choropleth 

palettePopulation <- colorNumeric("Reds",domain = blr_wards$Population2021)
popupPopulation <- paste0(blr_wards$WardName,":",blr_wards$Population2021)

choropleth2021 <- leaflet() %>%
  addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(minZoom = 10, maxZoom = 16)) %>%
  addPolygons(data = blr, fillColor = ~palettePopulation(blr_wards$Population2021),
              fillOpacity = 0.6, color = "darkgrey", weight = 1.5,
              popup = popupPopulation, group = "Population") %>%
  addLegend(position = 'topright',
            pal = palettePopulation,
            values = blr_wards$Population2021,
            opacity = 0.6,
            title = "Population") %>%
  addLabelOnlyMarkers(
    lng = 77.45, lat = 13.18,
    label = "Bangalore Population 2021",
    labelOptions = labelOptions(noHide = T,textsize = "20px"))

saveWidget(choropleth2021, 'choropleth2021.html', selfcontained = FALSE)
webshot('choropleth2021.html', file="images/Blr2021.png", cliprect = 'viewport')

system("convert images/blr*.png -delay 3 -loop 0 images/blr_population.gif")
system("rm -rf choropleth20*")