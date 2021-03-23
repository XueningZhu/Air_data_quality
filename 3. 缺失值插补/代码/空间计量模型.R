
#数据建模
dataclean1 <- function(file){
  filename = paste("C:/Users/yuyu/Desktop",file,sep = "/")
  data <- read.csv(filename,sep = ",",header = TRUE,encoding = "UTF-8")
  np = dim(data)
  n = np[1]
  p = np[2]
  
  #删除缺失值较多的列
  na_flag1 <- apply(is.na(data),2, sum)
  na_ff <- rep(0,length(na_flag1))
  for (i in 1:length(na_flag1)){
    na_ff[i] = na_flag1[i]/n
  }
  
  data1 <- data[,which(na_ff <= 0.1)]
  
}


dataclean2 <- function(x){
  #删除有缺失的行
  na_flag2 <- apply(is.na(x),1, sum)
  data2 <- x[which(na_flag2== 0),]
}



datapm25 = dataclean1("pm2.5_20182.csv")




##空间计量模型
##加载程序包
#install.packages("raster")
#install.packages("sp")
#install.packages("rgdal")
#install.packages("gstat")
library(sp)
library(spData)
library(sf)
library(spdep)
library(maptools)
library(Matrix)


#空间距离矩阵
library(geosphere)
station = read.csv("C:/Users/yuyu/Desktop/station18.csv",sep = ",",header = FALSE,encoding = "UTF-8")
View(station)
aa = station[-1,]
aa = as.numeric(as.matrix(aa))
bb = aa[1:21]
cc = aa[22:42]
dd = c(bb[1],cc[1])
ee = c(bb[2],cc[2])
ff = rbind(dd,ee)
xy = ff
for (i in 3:21){
  gg = c(bb[i],cc[i])
  xy = rbind(xy,gg)
}
#xy

dismatrix =distm(xy)

xx =  matrix(0,21,21)

for (i in 1:21){
  for (j in 1:21){
    xx[i,j] = 10000*(1/(dismatrix[i,j]))
  }
  xx[i,i] = 0
}


#莫兰检验
listw2 <- mat2listw(xx,style = "W")

#listw = mat2listw(dismatrix,style = "W")

datanew = read.csv("C:/Users/yuyu/Desktop/datanew.csv",sep = ",",header = TRUE,encoding = "GB2312")

seclectf = function(i){
  for (j in (1:21)){
    a = datanew[(i + (j - 1)*1470),]
    datasec = rbind(datasec,a)
  }
  
}

datasec = NULL
for (j in (1:21)){
  a = datanew[(5 + (j - 1)*1470),]
  datasec = rbind(datasec,a)
}


SAR <-
  lagsarlm(
    datasec$PM2.5 ~ CO + SO2 +NO2 + O3 ,
    data = datasec,
    listw2,
    method = "eigen",
    quiet = FALSE
  )

SAR2 <-
  lagsarlm(
    datasec$PM2.5 ~ CO + SO2 +NO2 + O3 ,
    data = datasec,
    listw2,
    method = "eigen",
    type = "lag",
    quiet = FALSE
  )

summary(SAR2)

SEM <-
  errorsarlm(
    datasec$PM2.5 ~ CO + SO2 +NO2 + O3,
    data = datasec,
    listw2,
    method = "eigen",
    quiet = FALSE
  )
summary(SEM)

SLX <-
  lmSLX(datasec$PM2.5 ~ CO + SO2 +NO2 + O3, data =datasec, listw = listw2)
summary(SLX)

SAC <-
  sacsarlm(datasec$PM2.5 ~ CO + SO2 +NO2 + O3, data =datasec, listw = listw2)
summary(SAC)


SDM <-
  lagsarlm(
    datasec$PM2.5 ~ CO + SO2 +NO2 + O3,
    data =datasec,
    listw = listw2,
    method = "eigen",
    quiet = FALSE,
    type = "mixed"
  )
summary(SDM)

SDEM <-
  errorsarlm(
    datasec$PM2.5 ~ CO + SO2 +NO2 + O3,
    data =datasec,
    listw = listw2,
    method = "eigen",
    quiet = FALSE,
    etype = "emixed"
  )
summary(SDEM)

GNS <-
  sacsarlm(datasec$PM2.5 ~ CO + SO2 +NO2 + O3,
           data =datasec,
           listw = listw2,
           type = "sacmixed")
summary(GNS)


OLS = lm(datasec$PM2.5 ~ CO + SO2 +NO2 + O3,
         data =datasec)

lmols = lm.morantest(OLS,listw = listw)

lm1 = errorsarlm(form = 东四pm2.5~.,data = data000,listw = listw0,etype = "error")
data0 = as.data.frame(lapply(data0,as.numeric))
mor = NULL
for (i in 1:4000){
  data101 = as.vector(t(data0[i,]))
  aa = moran.test(data101,listw2,randomisation = FALSE,alternative = "two.sided")
  mor[i] = aa$p.value
}

mor
#p < 0.05,拒绝原假设，存在空间相关性

juzhen = read.csv("C:/Users/yuyu/Desktop/juzhen.csv",sep = ",",header = TRUE,encoding = "GB2312")
row.names(juzhen) <- juzhen[, 1]
jz = as.matrix(juzhen[,-1])
#空间滞后变量
library(splm)
datanew1 = NULL
for (j in (1:21)){
  x = 1 + (j - 1)*1470
  y = x+20
  a = datanew[(x:y),]
  datanew1 = rbind(datanew1,a)
}

GM=spgm(PM2.5 ~ CO + SO2 +NO2 + O3,data=datanew1,listw = jz,moments = "fullweights",spatial.error = TRUE) #空间面板数据的广义矩估计
summary(GM)
fm=PM2.5 ~ CO + SO2 +NO2 + O3#空间面板极大似然估计
#fixed effects panel with spatial errors
Fespaterr=spml(fm,data=datanew1,listw=mat2listw(jz),model="within",spatial.error="b",hess=FALSE)
summary(Fespaterr)
#伴有随机效应和序列误差相关的空间面板模型的极大似然估计，命令speml
#random effects panel with spatial lag and serial error correlation
#optimization method set to “BFGS”
Sarsrmod<-spreml(fm,data=datanew1,w=jz,error="sr",lag=TRUE,method="BFGS")
summary(Sarsrmod)
#模型检验
#bsjktest,Baltigi,Song,Jung,and Koh LM test for spatial panels
bsjktest(fm,data=datanew1,listw=jz,test="C.1")
bsktest(fm,data=datanew1,listw=mat2listw(jz),test="LM1",standardize = TRUE)

#伴有随机效应和序列误差相关的空间面板模型的极大似然估计，命令speml
#random effects panel with spatial lag and serial error correlation
#optimization method set to “BFGS”
Sarsrmod<-spreml(fm,data=datanew1,w=jz,error="sr",lag=TRUE,method="BFGS")
summary(Sarsrmod)
#模型检验
#bsjktest，Baltigi,Song,Jung,and Koh LM test for spatial panels
bsjktest(fm,data=datanew1,listw=jz,test="C.1")
#bsktest，Baltigi,Song and Koh LM test for spatial panels
bsktest(fm,data=datanew1,listw=mat2listw(jz),test="LM1",standardize = TRUE)
#Covariance extractor method for splm objects
sarremod=spml(fm,data=datanew1,listw=mat2listw(jz),model = "random",lag = TRUE,spatial.error = "none")

