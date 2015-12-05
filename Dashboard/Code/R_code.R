
#**********Libraries***************
library(ggplot2)
library(lubridate)

#**********Loading Data***************
setwd("C:/Users/Innovaccer/Desktop/prism/")
d1=read.csv("quandl.csv")
d2=read.csv("ustre.csv")

#**********Transforming the Data***************
d2$X10.yr=as.numeric(d2$X10.yr)
d1$DATE=as.Date(d1$DATE)
d2$Date=as.Date(d2$Date,"%m-%d-%y")

#**********Merging The Two Data Set ***************
m=merge(x=d1,y=d2,by.x ='DATE',by.y = 'Date' ,all = F)

#**********Monthly Linear Regression***************
monthly<-matrix(nrow=10, ncol=12)
for(j in 0:9){ 
  for(i in 1:12){
  m1=m[year(m$DATE)==2005+j & month(m$DATE)==i,]
  fit=lm(data = m1,formula =m1$X10.yr~m1$VALUE,na.action = na.omit)
  monthly[j+1,i]<-fit$coefficients[2]
  }
}
colnames(monthly)<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
rownames(monthly)<-c(2005:2014)
write.csv(t(monthly),"monthly.csv",row.names = T)
#**********Yearly Linear Regression***************
yearly<-c()
for(j in 0:9){ 
     m1=m[year(m$DATE)==2005+j,]
    fit=lm(data = m1,formula = m1$X10.yr~m1$VALUE,na.action = na.omit)
    print(nrow(m1))
    yearly[j+1]<-fit$coefficients[2]
}
write.csv(yearly,"yearly.csv",row.names = T)

#**********Analysis on Whole Data  
fit<-lm(m$X10.yr~m$VALUE,na.action=na.omit)
plot(x=m[,2],y=m[,3],xlab = "USD 3 month LIBOR  ", ylab = "10 year Treasury yield curve rate ",col="orange", pch = 10, cex = .7, font.lab=2 )
abline(coef = fit$coefficients,col=2,lwd=2,lty="dashed")
title(main = "Scatter Plot With Linear Regression Line (Whole Data)")

