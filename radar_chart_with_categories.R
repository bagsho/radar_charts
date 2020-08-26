library(readxl)
library(fmsb)

senaryolar<-get_data()
create_bau_chart(senaryolar)
create_scenarios_chart(senaryolar)

# 4'lü grafik nasýl oluþturulacak bilemiyorum.
# çünkü pie sonrasý yeni bir çizim alaný oluþturuyordum.
# þimdi diðer hücreye geçiyor.





# defined functions

# The radar chart for bau
create_bau_chart <- function(senaryolar){
  png(filename = "bau_category.png",width=5, height=5, units="in", res=1450) 
  par(oma=c(0,0,0,0))
  par(mar=c(2,2,2,2))
  
  # Simple Pie Chart
  pie_slices<-c(6,4,2)
  pie_col<-c("#FFF0F5","#F0FFF0","#FAFAD2")
  lbls <- c("Economic\nProsperity", "Mobility\nTrends", "Social\nAspects")
  #pie(slices, labels = lbls, main="Pie Chart of Countries")
  pie(pie_slices,col= pie_col,init.angle=285, labels = lbls,border = "navy",lty=2)
  par(new=TRUE)
  par(oma=c(2,2,2,2))
  par(mar=c(4,4,4,4))
  
  radarchart(senaryolar[1:3,],
             axistype=1,
             axislabcol="grey",
             caxislabels=c("0","25","50","75","100"),
             calcex=0.75,
             sep=5,
             cglcol = "grey",
             cglty=1,
             vlcex = 0.4,
             palcex = 1,
             pcol= 2,
             pty = 32, 
             plty = 1,
             plwd = 6,
             title = row.names(senaryolar[3,])
  )
  dev.off()
}

# Produce multiple plots ----
create_scenarios_chart<- function(senaryolar){

  opar <- par() 
  
  pie_slices<-c(6,4,2)
  pie_col<-c("#FFF0F5","#F0FFF0","#FAFAD2")
  lbls <- c("", "", "")
  

  png(filename = "scenarios_categories.png",width=5, height=5, units="in", res=1450) 

  par(oma=c(0,0,0,0))
  par(mar=c(1,0, 1.5, 0))
  par(mfrow=c(2,2))

  
  # Iterate through the data, producing a radar-chart for each line
  for (i in 4:7) {toplot <- rbind(senaryolar[1:3,],senaryolar[i,])

  pie(pie_slices,col= pie_col,init.angle=285, labels = lbls,border = "navy",lty=2)
  par(new=TRUE)
  par(oma=c(0,0,0,0))
  par(mar=c(1,0, 1.5, 0))
  radarchart(toplot,
             sep=5,
             cglcol = "grey",
             cglty=1,
             vlcex = 0.4,
             palcex = 1,
             #dolgu rengi
             pfcol = c("#99979980",NA),
             #çizgi rengi
             pcol= c(NA,2),
             pty = 32, 
             plty = 1,
             plwd = c(3,3),
             title = row.names(senaryolar[i,])
  )
  }
  dev.off()
  
  # restore standard par settings
  par <- par(opar) 
}

#import data from excel
get_data <- function(){
  temp <- read_excel("C:/danýþmanlýk/arup/reports/m4.1/senaryolar.xlsx",sheet = "dimensions")
  senaryolar<-as.data.frame(t(temp))
  names(senaryolar)<-senaryolar[1,]
  senaryolar<-senaryolar[2:6,]
  # Specify columns you want to change
  i<-c(1:12) 
  # Specify own function within apply
  senaryolar[ , i] <- apply(senaryolar[ , i], 2,            
                            function(x) as.numeric(as.character(x)))
  # remove unnecessary data
  rm(temp,i)
  # To use the fmsb package, I have to add 2 lines to the dataframe: the max (2nd row) and min (1st row) of each topic to show on the plot!
  senaryolar<-rbind(rep(100,12),rep(0,12),senaryolar)
  rownames(senaryolar)[1]<-"min scale"
  rownames(senaryolar)[2]<-"max scale"
  
  names(senaryolar)[1]<-"Local Decision\nMaking"
  names(senaryolar)[2]<-"Economic\nProsperity"
  names(senaryolar)[3]<-"Mobility\nDemand"
  names(senaryolar)[4]<-"Private\nVehicle Choice"
  names(senaryolar)[5]<-"Public Transport\nChoice"
  names(senaryolar)[6]<-"Active Transport\nChoice"
  names(senaryolar)[7]<-"Land Use\nCompactness"
  names(senaryolar)[8]<-"Funding\nLevel"
  names(senaryolar)[9]<-"Funding\nEfficiency"
  names(senaryolar)[10]<-"Social\nInclusion"
  names(senaryolar)[11]<-"Level of\nEquality"
  names(senaryolar)[12]<-"Resilience to\nExternal Factors"
  return(senaryolar)
}