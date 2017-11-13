library(ggplot2)

blr_growth_rate <- read.csv("data/BangaloreGrowthRate.csv")

png(filename="images/growthrate.png")

ggplot(blr_growth_rate,aes(x = Time,y = GrowthRate )) +
  geom_point(aes(colour = GrowthRate)) +
  scale_colour_gradient2(low = "blue", mid = "green" , high = "red", midpoint = 3.5) + 
  geom_smooth(colour = "blue",size = 0.9) +
  scale_y_continuous(limits = c(1,7), breaks = seq(1,7,1)) +
  scale_x_continuous(limits = c(1955,2030), breaks = seq(1960,2030,10)) +
  ggtitle ("Average Annual Rate of Change of Population in Bangalore") +
  xlab("Year") +  ylab ("Average Growth Rate %")

dev.off()