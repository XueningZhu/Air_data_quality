#毕业论文#
#聚类
data180 = read.csv("C:/Users/yuyu/Desktop/pm2.5_20181.csv",sep = ",",header = TRUE,encoding = "UTF-8")
#install.packages("mice")
#library(lattice)
#library(mice)
#md.pattern(data18)
data18a = read.csv("C:/Users/yuyu/Desktop/co_20181.csv",sep = ",",header = TRUE,encoding = "UTF-8")
data18b = read.csv("C:/Users/yuyu/Desktop/so2_20181.csv",sep = ",",header = TRUE,encoding = "UTF-8")
data18c = read.csv("C:/Users/yuyu/Desktop/no2_20181.csv",sep = ",",header = TRUE,encoding = "UTF-8")
data18d = read.csv("C:/Users/yuyu/Desktop/o3_20181.csv",sep = ",",header = TRUE,encoding = "UTF-8")
#data180 =data180[-1,]
#data18a =data18a[-1,]
#data18b =data18b[-1,]
#data18c =data18c[-1,]
#data18d =data18d[-1,]

data18 = cbind(data180,data18a,data18b,data18c,data18d)

np = dim(data18)
n = np[1]
p = np[2]

data181 = data18[,-(1:3)]

for (i in 1:(p-3)){
  
}

#删除缺失值较多的列

na_flag1 <- apply(is.na(data181),2, sum)

na_ff <- rep(0,length(na_flag1))

for (i in 1:length(na_flag1)){
  na_ff[i] = na_flag1[i]/n
}

data182 <- data181[,which(na_ff <= 0.2)]

#删除有缺失的行
na_flag2 <- apply(is.na(data182),1, sum)
data183 <- na.omit(data182)
setwd("C:/Users/yuyu/Desktop/")
write.table(data183,"data20.csv",row.names=FALSE,col.names=TRUE,sep=",")

#聚类
#install.packages("dbscan")
library(dbscan)
data11 = t(data183)

a = kNN(data11, k=5, sort = TRUE, search = "kdtree", bucketSize = 10, splitRule = "suggest", approx = 0)
nn = kNN(data11,k=5)

kNNdistplot(data11, k = 3)
res <- dbscan(data11, eps = 1250, minPts = 5)
res

type = as.numeric(res$cluster)

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



datapm25 = dataclean1("pm2.5_20181.csv")
dataco = dataclean1("co_20181.csv")
dataso2 = dataclean1("so2_20181.csv")
datano2 = dataclean1("no2_20181.csv")
datao3 = dataclean1("o3_20181.csv")

datafinal = cbind(datapm25,dataco[,-(1:3)],dataso2[,-(1:3)],datano2[,-(1:3)],datao3[,-(1:3)])

data0 <- dataclean2(datafinal)

data0 <- data0[,-3]


#选择1聚类下的建模
lei1 = rep(type,5)
data1 = data0[,-(1:2)]
data2 = rbind(data1,lei1)

nm = dim(data2)
n = nm[1]
m = nm[2]

data3 = data2[-n,which(lei1 == 1)] #lei1
data4 = data2[-n,which(lei1 == 0)] #lei0

#简单回归
lm1 = lm(data = data3,data3$东四pm2.5~.)

library(carData)
library(car)

vif(lm1)

#修正多重共线性-逐步回归
lm2 = step(lm1,direction="backward")
vif(lm2)

#岭回归
library(MASS)
lm3 <- lm.ridge(data3$东四pm2.5 ~ ., lambda = seq(0, 150, length = 151), data = data3, model = TRUE)
lm3$lambda[which.min(lm3$GCV)]
lm3$coef[which.min(lm3$GCV)]

lm5 <- lm.ridge(data3$东四pm2.5 ~ ., lambda = 6, data = data3, model = TRUE)

#install.packages("ridge")
library(ridge)
lm4 <- linearRidge(data3$东四pm2.5 ~ ., data = data3)
summary(lm4)


library(lars)
x = as.matrix(data3[,-1])
y = as.matrix(data3[,1])
lm6 = lars(x, y, type = "lar")
plot(lm6)
summary(lm6)

#选那六个
lm7 = lm(data3$东四pm2.5~data3$农展馆pm2.5+data3$奥体中心pm2.5+data3$前门pm2.5+data3$万寿西宫pm2.5+data3$官园pm2.5+data3$西直门北pm2.5)
lm7


####第二次建模
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



datapm25 = dataclean1("pm2.5_20181.csv")
dataco = dataclean1("co_20181.csv")
dataso2 = dataclean1("so2_20181.csv")
datano2 = dataclean1("no2_20181.csv")
datao3 = dataclean1("o3_20181.csv")

datafinal = cbind(datapm25,dataco[,-(1:3)],dataso2[,-(1:3)],datano2[,-(1:3)],datao3[,-(1:3)])

#时间序列建模
datapm25_dongsi <- datapm25[,(1:4)]
nads <- apply(is.na(datapm25_dongsi),1, sum)



#密云水库

datapm25_mysk <- cbind(datapm25[,(1:3)],datapm25[,28])
nads <- apply(is.na(datapm25_mysk),1, sum)

#选出时间段
datatime <- function(x){
  p = length(x)
  nn = NULL
  for (i in 1:p){
    if (x[i] == 1)
      nn[i] = i
  }
  m1 = na.omit(nn)
  nm = c(0,m1)

  q = length(nm)
  m2 = NULL
  for (i in 1:(q-1)){
    m2[i] = nm[i+1] - nm[i]
  }
  nmm = m2
  j = which.max(nmm)
  max = nm[j+1] -1
  min = nm[j] + 1
  datat = datafinal[(min:max),]
}

datadst = datatime(nads)

library("e1071")
library("tseries")
library("zoo")
library("forecast")


abc = as.integer(seq(from = 28,by = 34,to = 168))
data1 = datadst[,abc]

zhihou <- function(data){
  np = dim(data)
  n = np[1]
  np = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-1,])
  yt1 = data.frame(y[-n,])
  x = data[-n,-1]
  
  newdata = cbind(yt,yt1,x)
}

datadst10 = zhihou(data1)

datadst1 = datadst10[(1:450),]
datadst2 = datadst10[(450:500),]


###回归
lmds1 = lm(datadst1$y..1...~.,data = datadst1)
lmds2 = lm(datadst1$y..1...~y..n...,data = datadst1)
lmds3 = lm(datadst1$y..1...~密云水库CO+密云水库SO2+密云水库NO2+密云水库O3,data = datadst1)

summary(lmds1)
summary(lmds2)
summary(lmds3)


mmse <- function(data,m,f){
  np = dim(data)
  n = np[1]
  p = np[2]
  
  set.seed(100)
  a = as.numeric(sample(n,m))
  
  y1 = data[a,1]
  
  x = data[a,-1]
  y2 = predict(f,x)
  
  b = NULL
  
  for(i in (1:length(y1))){
    b[i] = (y1[i] - y2[i])^2
  }
  
  mmse = sum(b)/m
  
}

ce1 = mmse(datadst2,1,lmds1)
ce2 = mmse(datadst2,2,lmds1)
ce3 = mmse(datadst2,3,lmds1)
ce4 = mmse(datadst2,4,lmds1)
ce5 = mmse(datadst2,5,lmds1)
ce6 = mmse(datadst2,12,lmds1)
ce6 = mmse(datadst2,25,lmds1)
ce1
ce2
ce3
ce4
ce5
ce6





ce12 = mmse(datadst2,1,lmds2)
ce22 = mmse(datadst2,2,lmds2)
ce32 = mmse(datadst2,3,lmds2)
ce42 = mmse(datadst2,4,lmds2)
ce52 = mmse(datadst2,5,lmds2)
ce62 = mmse(datadst2,12,lmds2)
ce72 = mmse(datadst2,25,lmds2)





ce12 = mmse(datadst2,1,lmds3)
ce22 = mmse(datadst2,2,lmds3)
ce32 = mmse(datadst2,3,lmds3)
ce42 = mmse(datadst2,4,lmds3)
ce52 = mmse(datadst2,5,lmds3)
ce62 = mmse(datadst2,12,lmds3)
ce72 = mmse(datadst2,25,lmds3)

ce12 
ce22
ce32 
ce42 
ce52 
ce62 
ce72 







library(carData)
library(car)

vif(lmds1)

lmds2 = lm(datadst1$y..1...~datadst1$东四CO+datadst1$东四SO2+datadst1$东四NO2+datadst1$东四O3,data = datadst1)
summary(lmds2)
vif(lmds2)





