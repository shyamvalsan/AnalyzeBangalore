library(RColorBrewer)
library(scales)

x <- read.csv("data/ward_popt.csv")
x.locs <- c(0.075,0.92)
y.locs<- c(0,1,5)

x[,1]<-as.character(x[,1]) 
listOfWards<-ncol(x)-1
ranking<-2:ncol(x)
rankingOriginal<-x[,ranking]
x[,ranking]<-rescale(as.matrix(x[,ranking]),c(5,25))  #rescale so line sizes make sense

#reorder column ranking 
first.ord<-order(x[1,ranking],decreasing=T)
x[,ranking]<-x[,ranking][,first.ord]
names(x)[ranking]<-names(x)[ranking][first.ord]

#list population of wards in decreasing order for each date
data_col<-vector('list',nrow(x))
names(data_col)<-x[,1]
for(val in 1:nrow(x)){
  tmp<-t(x[val,ranking])
  tmp<-tmp[order(tmp,decreasing=T),,drop=F]
  data_col[[val]]<-tmp
}

#initiate plot object
plot(dim.x,dim.y,type='n',axes=F,xlab='',ylab='') 

#x locations
x.vals<-rep(seq(x.locs[1],x.locs[2],length=length(data_col)),each=listOfWards)
x.vals<-split(x.vals,x.vals)

#y locations, rearranged in loop, exception if dates are plotted
y.vals<-rev(seq(y.locs[1],y.locs[2],length=listOfWards+1))[-1]

#get line colors  ``  
line.cols<-alpha(colorRampPalette(c("red","yellow","green"))(listOfWards),0.7)

for(val in 1:(length(data_col)-1)){
  
  #temp data to plot
  plt.tmp<-data_col[c(val,val+1)]
  x.tmp<-x.vals[c(val,val+1)]
  
  #plot temp text for column, remove spp if rnks
  rowtxt <- row.names(plt.tmp[[1]])
  text(x.tmp[[1]],y.vals,rowtxt)
  
  if(val == length(data_col)-1){
    rowtxt <- row.names(plt.tmp[[2]])
    text(x.tmp[[2]],y.vals,rowtxt)
    dt.txt<-substitute(bold(x),list(x=names(plt.tmp)[2]))
    text(unique(x.tmp[[2]]),y.locs[2],dt.txt)
  }	
  
  dt.txt<-substitute(bold(x),list(x=names(plt.tmp)[1]))  
  text(unique(x.tmp[[1]]),y.locs[2],dt.txt)
  
  srt.ln.y<-match(row.names(plt.tmp[[1]]),row.names(plt.tmp[[2]]))
  
  #if no line rescale, use first element of rs.ln
  lwd.val<-plt.tmp[[1]][,1]
  
  #vector for species selection of line segments
  sel.sp<-rep(T,listOfWards)
  
  #for lines
  segments(x.tmp[[1]][sel.sp], y.vals[sel.sp], x.tmp[[2]][sel.sp],
    y.vals[srt.ln.y][sel.sp], col=line.cols[sel.sp], lwd=lwd.val[sel.sp])
  
  #resort color vector for next column
  srt.cl.y<-match(row.names(plt.tmp[[2]]),row.names(plt.tmp[[1]]))
  line.cols<-line.cols[srt.cl.y]
}