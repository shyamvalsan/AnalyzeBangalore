library(RColorBrewer)
library(scales)

x <- read.csv("data/ward_popt.csv")
x.locs <- c(0.075,0.92)
y.locs<- c(0,1,5)

x[,1]<-as.character(x[,1]) 
listOfWards<-ncol(x)-1
ranking<-2:ncol(x)
x[,ranking]<-rescale(as.matrix(x[,ranking]),c(5,25))  #rescale so line sizes make sense

#reorder column ranking 
first.ord<-order(x[1,ranking],decreasing=T)
x[,ranking]<-x[,ranking][,first.ord]
names(x)[ranking]<-names(x)[ranking][first.ord]

#list population of wards in decreasing order for each date
columns<-vector('list',nrow(x))
names(columns)<-x[,1]
for(val in 1:nrow(x)){
  tmp<-t(x[val,ranking])
  tmp<-tmp[order(tmp,decreasing=T),,drop=F]
  columns[[val]]<-tmp
}

#initiate plot object
plot(c(0,1),c(0,1),type='n',axes=F,xlab='',ylab='') 

#x locations
x.vals<-rep(seq(x.locs[1],x.locs[2],length=length(columns)),each=listOfWards)
x.vals<-split(x.vals,x.vals)

#y locations, rearranged in loop, exception if dates are plotted
y.vals<-rev(seq(y.locs[1],y.locs[2],length=listOfWards+1))[-1]

#get line colors  ``  
line_colors<-alpha(colorRampPalette(c("red","yellow","green"))(listOfWards),0.7)

for(val in 1:(length(columns)-1)){
  
  #temp data to plot
  temp_plot<-columns[c(val,val+1)]
  x_tmp<-x.vals[c(val,val+1)]
  
  #plot temp text for column, remove spp if rnks
  rowtxt <- row.names(temp_plot[[1]])
  text(x_tmp[[1]],y.vals,rowtxt)
  
  if(val == length(columns)-1){
    rowtxt <- row.names(temp_plot[[2]])
    text(x_tmp[[2]],y.vals,rowtxt)
    heading<-substitute(bold(x),list(x=names(temp_plot)[2]))
    text(unique(x_tmp[[2]]),y.locs[2],heading)
  }	
  
  heading<-substitute(bold(x),list(x=names(temp_plot)[1]))  
  text(unique(x_tmp[[1]]),y.locs[2],heading)
  
  sort_names<-match(row.names(temp_plot[[1]]),row.names(temp_plot[[2]]))
  
  #if no line rescale, use first element of rs.ln
  line_width<-temp_plot[[1]][,1]
  
  #for lines
  segments(x_tmp[[1]], y.vals, x_tmp[[2]], y.vals[sort_names], col=line_colors, lwd=line_width)
  
  #resort color vector for next column
  sort_colors<-match(row.names(temp_plot[[2]]),row.names(temp_plot[[1]]))
  line_colors<-line_colors[sort_colors]
}