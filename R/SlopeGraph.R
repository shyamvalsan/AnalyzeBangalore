library(RColorBrewer)
library(scales)

pop <- read.csv("data/ward_popt.csv")
x.locs <- c(0.075,0.92)
y.locs<- c(0,1,5)
plot(c(0,1),c(0,1),type='n',axes=F,xlab='',ylab='') 

pop[,1]<-as.character(pop[,1]) 
listOfWards<-ncol(pop)-1
ranking<-2:ncol(pop)
pop[,ranking]<-rescale(as.matrix(pop[,ranking]),c(5,25))  #rescale so line sizes make sense

#reorder column ranking 
first.ord<-order(pop[1,ranking],decreasing=T)
pop[,ranking]<-pop[,ranking][,first.ord]
names(pop)[ranking]<-names(pop)[ranking][first.ord]

#list population of wards in decreasing order for each date
columns<-vector('list',nrow(pop))
names(columns)<-pop[,1]
for(val in 1:nrow(pop)){
  tmp<-t(pop[val,ranking])
  tmp<-tmp[order(tmp,decreasing=T),,drop=F]
  columns[[val]]<-tmp
}

# x and y co-ordinates 
x_plot<-rep(seq(x.locs[1],x.locs[2],length=length(columns)),each=listOfWards)
x_plot<-split(x_plot,x_plot)
y_plot<-rev(seq(y.locs[1],y.locs[2],length=listOfWards+1))[-1]

# get line colors
line_colors<-alpha(colorRampPalette(c("red","yellow","green"))(listOfWards),0.7)

for(val in 1:(length(columns)-1)){
  temp_plot<-columns[c(val,val+1)]
  x_tmp<-x_plot[c(val,val+1)]
  rowtxt <- row.names(temp_plot[[1]])
  text(x_tmp[[1]],y_plot,rowtxt)
  
  if(val == length(columns)-1){
    rowtxt <- row.names(temp_plot[[2]])
    text(x_tmp[[2]],y_plot,rowtxt)
    heading<-substitute(bold(x),list(x=names(temp_plot)[2]))
    text(unique(x_tmp[[2]]),y.locs[2],heading)
  }	
  
  heading<-substitute(bold(x),list(x=names(temp_plot)[1]))  
  text(unique(x_tmp[[1]]),y.locs[2],heading)
  
  sort_names<-match(row.names(temp_plot[[1]]),row.names(temp_plot[[2]]))
  line_width<-temp_plot[[1]][,1]
  
  segments(x_tmp[[1]], y_plot, x_tmp[[2]], y_plot[sort_names], col=line_colors, lwd=line_width)
  
  sort_colors<-match(row.names(temp_plot[[2]]),row.names(temp_plot[[1]]))
  line_colors<-line_colors[sort_colors]
}