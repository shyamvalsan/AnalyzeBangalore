
library(plotly)

data <- read.csv("data/top30_pop.csv")

data <- data[order(data$continent, data$city),]
slope <- 2.666051223553066e-05
data$size <- sqrt(data$pop * slope)
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

p1 <- plot_ly(data, x = ~growthrate, y = ~sexratio, color = ~continent, size = ~size, colors = colors,
             type = 'scatter', mode = 'markers', sizes = c(min(data$size), max(data$size)),
             marker = list(symbol = 'circle', sizemode = 'radius',
                           line = list(width = 2, color = '#FFFFFF')),
             text = ~paste(city, '<br>Population:', pop, '<br>Growth Rate:', growthrate,'<br>Sex Ratio:', sexratio)) %>%
  layout(title = 'Male-Female Ratio vs Population Growth Rate - Top 30 Urban Agglomerations - 2015',
         xaxis = list(title = 'Growth Rate',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 4.5),
                      zerolinewidth = 1,
                      ticklen = 1,
                      gridwidth = 2),
         yaxis = list(title = 'Males per thousand Females',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(800, 1200),
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')


colors <- c('#C61951', '#FF7070', '#965F8A', '#1972A4', '#4AC6B7')

p2 <- plot_ly(data, x = ~growthrate, y = ~sexratio, color = ~sexratio, size = ~size, colors = colors,
        type = 'scatter', mode = 'markers', sizes = c(min(data$size), max(data$size)),
        marker = list(symbol = 'circle', sizemode = 'radius',
                      line = list(width = 2, color = '#FFFFFF')),
        text = ~paste(city, '<br>Population:', pop, '<br>Growth Rate:', growthrate,'<br>Sex Ratio:', sexratio)) %>%
  layout(title = 'Sex Ratio vs Population Growth Rate - Top 30 Urban Agglomerations - 2015',
         xaxis = list(title = 'Growth Rate',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(0, 4.5),
                      zerolinewidth = 1,
                      ticklen = 1,
                      gridwidth = 2),
         yaxis = list(title = 'Sex Ratio (Males per thousand Females)',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(800, 1200),
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')
