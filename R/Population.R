library(rgdal)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(ggplot2)

blr <- readOGR("data", "BBMP-Wards")
blr_wards <- read.csv("data/blr_wards.csv", sep=",", dec=".")

# Converting special characters to numeric
blr_wards[,18] <- as.numeric(gsub(",", "", blr_wards[,18]))
blr_wards[,19] <- as.numeric(gsub("%", "", blr_wards[,19]))

# Use splinefun to interpolate and extrapolate the population of each ward from 2001 to 2021 
# As reference we use the population from 2011 and the decadal growth rate of each ward from 2001-2011
# Comparing the results of this formula to the United Nations estimates show that they align very closely
for (ward in 1:length(blr_wards[,3])) {
  range <- c(2001:2011)
  for (j in 1:21) {
    population <- seq(as.integer((100*blr_wards[ward,3])/(blr_wards[ward,19] + 100)),blr_wards[ward,3], length.out=11)
    func = splinefun(x=range, y=population, method="fmm",  ties = mean)
    blr_wards[ward,(j+19)] <- as.integer(func(seq(2000+j,2000+j,1)))
    names(blr_wards)[j+19] <- paste0("pop",2000+j,"")
  }  
}

colorrange <- c(15000,30000,50000,80000,125000,163000)
palettePopulation <- colorNumeric("Reds",domain = colorrange)

for (year in 2001:2021) {
  colname <- paste0("pop",year,"")
  labelname <- paste("Bangalore population",year)
  popupPopulation <- paste0(blr_wards$WardName,":",blr_wards[,colname])
  name <- paste("pop", year, sep="")
  html_name <- paste(name, ".html", sep="")
  png_name <- paste(name, ".png", sep="")
  png_absolute_name <- paste("images/", png_name, sep="")
  
  img <- leaflet() %>% 
    addProviderTiles("Esri.WorldGrayCanvas", 
                     options = tileOptions(minZoom = 10, maxZoom = 16)) %>%
    addPolygons(data = blr, fillColor = ~palettePopulation(blr_wards[,colname]),
                fillOpacity = 0.6, color = "darkgrey", weight = 1.5, 
                popup = popupPopulation, group = "Population") %>%
    addLegend(position = 'topright', 
              pal = palettePopulation,
              values = colorrange,
              opacity = 0.6,    
              title = "Population") %>%
    addLabelOnlyMarkers(
      lng = 77.47, lat = 13.18,
      label = labelname,
      labelOptions = labelOptions(noHide = T,textsize = "20px"))
  
  saveWidget(img, html_name, selfcontained = FALSE)
  webshot(html_name, file=png_absolute_name, cliprect = 'viewport')
}

system("rm -rf pop*.html")
system("rm -rf pop*_files")
system("convert -delay 50 images/pop*.png -loop 0 images/blr_population.gif")
