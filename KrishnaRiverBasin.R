#loading required libraries
library(ggplot2)
library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(sf)
library(dplyr)
library(leaflet)
library(htmltools)
library(rnaturalearth)
library(leaflet.extras)
library(tmap)
library(RColorBrewer)
library(cartography)
library(mapproj)
library(spatial)
library(gganimate)
library(viridis)
library(hrbrthemes)
library(plotly)
library(htmlwidgets)
library(xts)
library(forecast)
library(tseries)
library(lubridate)
library(zoo)
library(chron)
library(car)
library(rstatix)
library(pbkrtest)
library(ggpubr)
library(tidyquant)
###############################################################################################
#ssp245
#1
#Access CM2
#TMaxData
#reading the dataset
tmax1<-read.table("F:/2 month project/Krishna/Krishna/ACCESS-CM2/ssp245/TMaxData",
                              quote="\"", comment.char="")
#subsetting the dataset
values1<-tmax1 %>% slice(3:31413)
#View(values1)
#finding the means of each row
means1<-rowMeans(values1[4:373])
#View(means1)
#adding the column to the dataset
values1$average<-means1
#view(values1)
#finding sums year-wise
sumyears1<-c()
x1=1
c1=1
for (i in (2015:2101)){
  sum1<-0
  for (j in (1:365)){
    if (values1$V1[c1]==i){
      sum1<-sum1+values1$average[c1]
    }
    c1<-c1+1
  }
  sumyears1[x1]<-sum1
  x1<-x1+1
}
#View(sumyears1)
#finding average year-wise
tmaxvalues1<-c()
y1=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues1[y1]<-sumyears1[y1]/366
  }
  else{
    tmaxvalues1[y1]<-sumyears1[y1]/365
  }
  y1<-y1+1
}
#View(tmaxvalues1)
#converting values to time-series
tstmax1<- ts(tmaxvalues1,start=2015,frequency=1)
#plotting the data
#plot(tstmax1)

################################################################################################
#Access-CM2
#TMinData
#reading the dataset
tmin1<-read.table("F:/2 month project/Krishna/Krishna/ACCESS-CM2/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values2<-tmin1 %>% slice(3:31413)
#View(values2)
#finding the means of each row
means2<-rowMeans(values2[4:373])
#View(means2)
#adding the column to the dataset
values2$average<-means2
#view(values2)
#finding sums year-wise
sumyears2<-c()
x2=1
c2=1
for (i in (2015:2101)){
  sum2<-0
  for (j in (1:365)){
    if (values2$V1[c2]==i){
      sum2<-sum2+values2$average[c2]
    }
    c2<-c2+1
  }
  sumyears2[x2]<-sum2
  x2<-x2+1
}
#View(sumyears2)
#finding average year-wise
tminvalues1<-c()
y2=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues1[y2]<-sumyears2[y2]/366
  }
  else{
    tminvalues1[y2]<-sumyears2[y2]/365
  }
  y2<-y2+1
}
#View(tminvalues1)
#converting values to time-series
tstmin1<- ts(tminvalues1,start=2015,frequency=1)
#plotting the data
#plot(tstmin1)
#finding dtr
dtr1<-tmaxvalues1-tminvalues1
#View(dtr1)
#converting values to time-series
tsdtr1<- ts(dtr1,start=2015,frequency=1)
#plotting the data
plot(tsdtr1)

################################################################################
#Access ESM1-5
#2
#TMaxData
#reading the dataset
tmax2<-read.table("F:/2 month project/Krishna/Krishna/ACCESS-ESM1-5/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values3<-tmax2 %>% slice(3:31413)
#View(values3)
#finding the means of each row
means3<-rowMeans(values3[4:373])
#View(means3)
#adding the column to the dataset
values3$average<-means3
#view(values3)
#finding sums year-wise
sumyears3<-c()
x3=1
c3=1
for (i in (2015:2101)){
  sum3<-0
  for (j in (1:365)){
    if (values3$V1[c3]==i){
      sum3<-sum3+values3$average[c3]
    }
    c3<-c3+1
  }
  sumyears3[x3]<-sum3
  x3<-x3+1
}
#View(sumyears3)
#finding average year-wise
tmaxvalues2<-c()
y3=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues2[y3]<-sumyears3[y3]/366
  }
  else{
    tmaxvalues2[y3]<-sumyears3[y3]/365
  }
  y3<-y3+1
}
#View(tmaxvalues2)
#converting values to time-series
tstmax2<- ts(tmaxvalues2,start=2015,frequency=1)
#plotting the data
#plot(tstmax2)




################################################################################################
#Access-ESM1-5
#TMinData
#reading the dataset
tmin2<-read.table("F:/2 month project/Krishna/Krishna/ACCESS-ESM1-5/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values4<-tmin2 %>% slice(3:31413)
#View(values4)
#finding the means of each row
means4<-rowMeans(values4[4:373])
#View(means4)
#adding the column to the dataset
values4$average<-means4
#view(values4)
#finding sums year-wise
sumyears4<-c()
x4=1
c4=1
for (i in (2015:2101)){
  sum4<-0
  for (j in (1:365)){
    if (values4$V1[c4]==i){
      sum4<-sum4+values4$average[c4]
    }
    c4<-c4+1
  }
  sumyears4[x4]<-sum4
  x4<-x4+1
}
#View(sumyears4)
#finding average year-wise
tminvalues2<-c()
y4=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues2[y4]<-sumyears4[y4]/366
  }
  else{
    tminvalues2[y4]<-sumyears4[y4]/365
  }
  y4<-y4+1
}
#View(tminvalues2)
#converting values to time-series
tstmin2<- ts(tminvalues2,start=2015,frequency=1)
#plotting the data
#plot(tstmin2)
#finding dtr
dtr2<-tmaxvalues2-tminvalues2
#View(dtr2)
#converting values to time-series
tsdtr2<- ts(dtr2,start=2015,frequency=1)
#plotting the data
plot(tsdtr2)


#########################################################################
#BCC-CSM2-MR
#3
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#########################################################################
#CanESM5
#4
#TMaxData
#reading the dataset
tmax4<-read.table("F:/2 month project/Krishna/Krishna/CanESM5/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values7<-tmax4 %>% slice(3:31413)
#View(values7)
#finding the means of each row
means7<-rowMeans(values7[4:373])
#View(means7)
#adding the column to the dataset
values7$average<-means7
#view(values7)
#finding sums year-wise
sumyears7<-c()
x7=1
c7=1
for (i in (2015:2101)){
  sum7<-0
  for (j in (1:365)){
    if (values7$V1[c7]==i){
      sum7<-sum7+values7$average[c7]
    }
    c7<-c7+1
  }
  sumyears7[x7]<-sum7
  x7<-x7+1
}
#View(sumyears7)
#finding average year-wise
tmaxvalues4<-c()
y7=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues4[y7]<-sumyears7[y7]/366
  }
  else{
    tmaxvalues4[y7]<-sumyears7[y7]/365
  }
  y7<-y7+1
}
#View(tmaxvalues4)
#converting values to time-series
tstmax4<- ts(tmaxvalues4,start=2015,frequency=1)
#plotting the data
#plot(tstmax4)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin4<-read.table("F:/2 month project/Krishna/Krishna/CanESM5/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values8<-tmin4 %>% slice(3:31413)
#View(values8)
#finding the means of each row
means8<-rowMeans(values8[4:373])
#View(means8)
#adding the column to the dataset
values8$average<-means8
#view(values8)
#finding sums year-wise
sumyears8<-c()
x8=1
c8=1
for (i in (2015:2101)){
  sum8<-0
  for (j in (1:365)){
    if (values8$V1[c8]==i){
      sum8<-sum8+values8$average[c8]
    }
    c8<-c8+1
  }
  sumyears8[x8]<-sum8
  x8<-x8+1
}
#View(sumyears8)
#finding average year-wise
tminvalues4<-c()
y8=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues4[y8]<-sumyears8[y8]/366
  }
  else{
    tminvalues4[y8]<-sumyears8[y8]/365
  }
  y8<-y8+1
}
#View(tminvalues4)
#converting values to time-series
tstmin4<- ts(tminvalues4,start=2015,frequency=1)
#plotting the data
#plot(tstmin4)
#finding dtr
dtr4<-tmaxvalues4-tminvalues4
#View(dtr4)
#converting values to time-series
tsdtr4<- ts(dtr4,start=2015,frequency=1)
#plotting the data
plot(tsdtr4)


#########################################################################
#5
#########################################################################
#EC-Earth3
#TMaxData
#reading the dataset
tmax5<-read.table("F:/2 month project/Krishna/Krishna/EC-Earth3/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values9<-tmax5 %>% slice(3:31413)
#View(values9)
#finding the means of each row
means9<-rowMeans(values9[4:373])
#View(means9)
#adding the column to the dataset
values9$average<-means9
#view(values9)
#finding sums year-wise
sumyears9<-c()
x9=1
c9=1
for (i in (2015:2101)){
  sum9<-0
  for (j in (1:365)){
    if (values9$V1[c9]==i){
      sum9<-sum9+values9$average[c9]
    }
    c9<-c9+1
  }
  sumyears9[x9]<-sum9
  x9<-x9+1
}
#View(sumyears9)
#finding average year-wise
tmaxvalues5<-c()
y9=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues5[y9]<-sumyears9[y9]/366
  }
  else{
    tmaxvalues5[y9]<-sumyears9[y9]/365
  }
  y9<-y9+1
}
#View(tmaxvalues5)
#converting values to time-series
tstmax5<- ts(tmaxvalues5,start=2015,frequency=1)
#plotting the data
#plot(tstmax5)




################################################################################################
#EC-Earth3
#TMinData
#reading the dataset
tmin5<-read.table("F:/2 month project/Krishna/Krishna/EC-Earth3/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values10<-tmin5 %>% slice(3:31413)
#View(values10)
#finding the means of each row
means10<-rowMeans(values10[4:373])
#View(means10)
#adding the column to the dataset
values10$average<-means10
#view(values10)
#finding sums year-wise
sumyears10<-c()
x10=1
c10=1
for (i in (2015:2101)){
  sum10<-0
  for (j in (1:365)){
    if (values10$V1[c10]==i){
      sum10<-sum10+values10$average[c10]
    }
    c10<-c10+1
  }
  sumyears10[x10]<-sum10
  x10<-x10+1
}
#View(sumyears10)
#finding average year-wise
tminvalues5<-c()
y10=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues5[y10]<-sumyears10[y10]/366
  }
  else{
    tminvalues5[y10]<-sumyears10[y10]/365
  }
  y10<-y10+1
}
#View(tminvalues5)
#converting values to time-series
tstmin5<- ts(tminvalues5,start=2015,frequency=1)
#plotting the data
#plot(tstmin5)
#finding dtr
dtr5<-tmaxvalues5-tminvalues5
#View(dtr5)
#converting values to time-series
tsdtr5<- ts(dtr5,start=2015,frequency=1)
#plotting the data
plot(tsdtr5)


#########################################################################
#6
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#7
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#8
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#9
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#10
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#11
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#12
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
#13
#########################################################################
#BCC-CSM2-MR
#TMaxData
#reading the dataset
tmax3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMaxData",
                  quote="\"", comment.char="")
#subsetting the dataset
values5<-tmax3 %>% slice(3:31413)
#View(values5)
#finding the means of each row
means5<-rowMeans(values5[4:373])
#View(means5)
#adding the column to the dataset
values5$average<-means5
#view(values5)
#finding sums year-wise
sumyears5<-c()
x5=1
c5=1
for (i in (2015:2101)){
  sum5<-0
  for (j in (1:365)){
    if (values5$V1[c5]==i){
      sum5<-sum5+values5$average[c5]
    }
    c5<-c5+1
  }
  sumyears5[x5]<-sum5
  x5<-x5+1
}
#View(sumyears5)
#finding average year-wise
tmaxvalues3<-c()
y5=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tmaxvalues3[y5]<-sumyears5[y5]/366
  }
  else{
    tmaxvalues3[y5]<-sumyears5[y5]/365
  }
  y5<-y5+1
}
#View(tmaxvalues3)
#converting values to time-series
tstmax3<- ts(tmaxvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmax3)




################################################################################################
#BCC-CSM2-MR
#TMinData
#reading the dataset
tmin3<-read.table("F:/2 month project/Krishna/Krishna/BCC-CSM2-MR/ssp245/TMinData",
                  quote="\"", comment.char="")
#subsetting the dataset
values6<-tmin3 %>% slice(3:31413)
#View(values6)
#finding the means of each row
means6<-rowMeans(values6[4:373])
#View(means6)
#adding the column to the dataset
values6$average<-means6
#view(values6)
#finding sums year-wise
sumyears6<-c()
x6=1
c6=1
for (i in (2015:2101)){
  sum6<-0
  for (j in (1:365)){
    if (values6$V1[c6]==i){
      sum6<-sum6+values6$average[c6]
    }
    c6<-c6+1
  }
  sumyears6[x6]<-sum6
  x6<-x6+1
}
#View(sumyears6)
#finding average year-wise
tminvalues3<-c()
y6=1
for (i in 2015:2100){
  if (i%%4==0 || i%%100==0){
    tminvalues3[y6]<-sumyears6[y6]/366
  }
  else{
    tminvalues3[y6]<-sumyears6[y6]/365
  }
  y6<-y6+1
}
#View(tminvalues3)
#converting values to time-series
tstmin3<- ts(tminvalues3,start=2015,frequency=1)
#plotting the data
#plot(tstmin3)
#finding dtr
dtr3<-tmaxvalues3-tminvalues3
#View(dtr3)
#converting values to time-series
tsdtr3<- ts(dtr3,start=2015,frequency=1)
#plotting the data
plot(tsdtr3)


#########################################################################
View(values1)
sumpremon<-c()
xx=1
cx=1
for (i in (2021:2101)){
  sumx<-0
  for (j in (1:365)){
    if (values1$V2[cx]==3 || values1$V2[cx]==4 || values1$V2[cx]==5){
      sumx<-sumx+values1$average[cx]
    }
    cx<-cx+1
  }
  sumpremon[xx]<-sumx
  xx<-xx+1
}
View(sumpremon)
