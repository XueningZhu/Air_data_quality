#shijian

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

zhihou <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-1,])
  yt1 = data.frame(y[-n,])
  x = data[-n,-1]
  
  newdata = cbind(yt,yt1,x)
}

library("e1071")
library("tseries")
library("zoo")
library("forecast")
library(carData)
library(car)


time = c(9,15,16,21,22,23,24,26,28,28,30,31) #12个离群点

#北部新区
datapm25_1 <- cbind(datapm25[,(1:3)],datapm25[,11])
nads <- apply(is.na(datapm25_1),1, sum)
databb= datatime(nads)
abc = as.integer(seq(from = 11,by = 34,to = 168))
data1 = databb[,abc]

data10 = zhihou(data1)

data101 = data10[(1:400),]
data102 = data10[(401:450),]


lmbb1 = lm(data101$y..1...~data101$y..n...,data = data101)
lmbb2 = lm(data101$y..1...~.,data = data101)
lmbb3 = lm(data101$y..1...~北部新区CO+北部新区SO2+北部新区NO2+北部新区O3, data = data101)

summary(lmbb1)
summary(lmbb3)
summary(lmbb2)

vif(lmbb1)
vif(lmbb2)
vif(lmbb3)

cc1 = NULL
cc2 = NULL
cc3 = NULL

for (i in (1:5)){
  c1 = mmse(data102,i,lmbb1)
  cc1 = c(cc1,c1)
  c2 = mmse(data102,i,lmbb2)
  cc2 = c(cc2,c2)
  c3 = mmse(data102,i,lmbb3)
  cc3 = c(cc3,c3)
}

c3 = mmse(data102,25,lmbb2)


#大兴
datapm25_2 <- cbind(datapm25[,(1:3)],datapm25[,16])
nads <- apply(is.na(datapm25_2),1, sum)
datadx= datatime(nads)
abc = as.integer(seq(from = 16,by = 34,to = 168))
data2 = datadx[,abc]

data20 = zhihou(data2)

data201 = data20[(1:400),]
data202 = data20[(401:450),]


lmdx1 = lm(data201$y..1...~data201$y..n...,data = data201)
lmdx2 = lm(data201$y..1...~.,data = data201)
lmdx3 = lm(data201$y..1...~data201$大兴CO+data201$大兴SO2+data201$大兴NO2+data201$大兴O3, data = data201)

summary(lmdx1)
summary(lmdx3)
summary(lmdx2)

#亦庄
datapm25_3 <- cbind(datapm25[,(1:3)],datapm25[,17])
nads <- apply(is.na(datapm25_3),1, sum)
datayz= datatime(nads)
abc = as.integer(seq(from = 17,by = 34,to = 168))
data3 = datayz[,abc]

data30 = zhihou(data3)

data301 = data30[(1:400),]
data302 = data30[(401:450),]


lmyz1 = lm(data301$y..1...~data301$y..n...,data = data301)
lmyz2 = lm(data301$y..1...~.,data = data301)
lmyz3 = lm(data301$y..1...~data301$亦庄CO+data301$亦庄SO2+data301$亦庄NO2+data301$亦庄O3, data = data301)

summary(lmyz1)
summary(lmyz3)
summary(lmyz2)

#平谷
datapm25_4 <- cbind(datapm25[,(1:3)],datapm25[,22])
nads <- apply(is.na(datapm25_4),1, sum)
datapg = datatime(nads)
abc = as.integer(seq(from = 22,by = 34,to = 168))
data4 = datapg[,abc]

data40 = zhihou(data4)

data401 = data40[(1:400),]
data402 = data40[(401:450),]


lmpg1 = lm(data401$y..1...~data401$y..n...,data = data401)
lmpg2 = lm(data401$y..1...~.,data = data401)
lmpg3 = lm(data401$y..1...~data401$平谷CO+data401$平谷SO2+data401$平谷NO2+data401$平谷O3, data = data401)

summary(lmpg1)
summary(lmpg3)
summary(lmpg2)


#怀柔
datapm25_5 <- cbind(datapm25[,(1:3)],datapm25[,23])
nads <- apply(is.na(datapm25_5),1, sum)
datahr = datatime(nads)
abc = as.integer(seq(from = 23,by = 34,to = 168))
data5 = datahr[,abc]

data50 = zhihou(data5)

data501 = data50[(1:400),]
data502 = data50[(401:450),]


lmhr1 = lm(data501$y..1...~data501$y..n...,data = data501)
lmhr2 = lm(data501$y..1...~.,data = data501)
lmhr3 = lm(data501$y..1...~data501$怀柔CO+data501$怀柔SO2+data501$怀柔NO2+data501$怀柔O3, data = data501)

summary(lmhr1)
summary(lmhr3)
summary(lmhr2)

#密云
datapm25_6 <- cbind(datapm25[,(1:3)],datapm25[,24])
nads <- apply(is.na(datapm25_6),1, sum)
datamy = datatime(nads)
abc = as.integer(seq(from = 24,by = 34,to = 168))
data6 = datamy[,abc]

data60 = zhihou(data6)

data601 = data60[(1:400),]
data602 = data60[(401:450),]


lmmy1 = lm(data601$y..1...~data601$y..n...,data = data601)
lmmy2 = lm(data601$y..1...~.,data = data601)
lmmy3 = lm(data601$y..1...~data601$密云CO+data601$密云SO2+data601$密云NO2+data601$密云O3, data = data601)

summary(lmmy1)
summary(lmmy3)
summary(lmmy2)


#延庆
datapm25_7 <- cbind(datapm25[,(1:3)],datapm25[,25])
nads <- apply(is.na(datapm25_7),1, sum)
datayq = datatime(nads)
abc = as.integer(seq(from = 25,by = 34,to = 168))
data7 = datayq[,abc]

data70 = zhihou(data7)

data701 = data70[(1:400),]
data702 = data70[(401:450),]


lmyq1 = lm(data701$y..1...~data701$y..n...,data = data701)
lmyq2 = lm(data701$y..1...~.,data = data701)
lmyq3 = lm(data701$y..1...~data701$延庆CO+data701$延庆SO2+data701$延庆NO2+data701$延庆O3, data = data701)

summary(lmyq1)
summary(lmyq3)
summary(lmyq2)

#八达岭
datapm25_8 <- cbind(datapm25[,(1:3)],datapm25[,27])
nads <- apply(is.na(datapm25_8),1, sum)
databdl = datatime(nads)
abc = as.integer(seq(from = 27,by = 34,to = 168))
data8= databdl[,abc]

data80 = zhihou(data8)

data801 = data80[(1:300),]
data802 = data80[(301:321),]


lmbdl1 = lm(data801$y..1...~data801$y..n...,data = data801)
lmbdl2 = lm(data801$y..1...~.,data = data801)
lmbdl3 = lm(data801$y..1...~data801$八达岭CO+data801$八达岭SO2+data801$八达岭NO2+data801$八达岭O3, data = data801)

summary(lmbdl1)
summary(lmbdl3)
summary(lmbdl2)

#东高村
datapm25_9 <- cbind(datapm25[,(1:3)],datapm25[,29])
nads <- apply(is.na(datapm25_9),1, sum)
datadgc = datatime(nads)
abc = as.integer(seq(from = 29,by = 34,to = 168))
data9= datadgc[,abc]

data90 = zhihou(data9)

data901 = data90[(1:300),]
data902 = data90[(301:321),]


lmdgc1 = lm(data901$y..1...~data901$y..n...,data = data901)
lmdgc2 = lm(data901$y..1...~.,data = data901)
lmdgc3 = lm(data901$y..1...~data901$东高村CO+data901$东高村SO2+data901$东高村NO2+data901$东高村O3, data = data901)

summary(lmdgc1)
summary(lmdgc3)
summary(lmdgc2)


#永乐店
datapm25_10 <- cbind(datapm25[,(1:3)],datapm25[,30])
nads <- apply(is.na(datapm25_10),1, sum)
datayld = datatime(nads)
abc = as.integer(seq(from = 30,by = 34,to = 168))
data10= datayld[,abc]

data100 = zhihou(data10)

data1001 = data100[(1:400),]
data1002 = data100[(401:450),]


lmyld1 = lm(data1001$y..1...~data1001$y..n...,data = data1001)
lmyld2 = lm(data1001$y..1...~.,data = data1001)
lmyld3 = lm(data1001$y..1...~data1001$永乐店CO+data1001$永乐店SO2+data1001$永乐店NO2+data1001$永乐店O3, data = data1001)

summary(lmyld1)
summary(lmyld3)
summary(lmyld2)


#榆垡
datapm25_11 <- cbind(datapm25[,(1:3)],datapm25[,31])
nads <- apply(is.na(datapm25_11),1, sum)
datayf = datatime(nads)
abc = as.integer(seq(from = 31,by = 34,to = 168))
data11= datayf[,abc]

data110 = zhihou(data11)

data1101 = data110[(1:350),]
data1102 = data110[(351:400),]


lmyf1 = lm(data1101$y..1...~data1101$y..n...,data = data1101)
lmyf2 = lm(data1101$y..1...~.,data = data1101)
lmyf3 = lm(data1101$y..1...~data1101$榆垡CO+data1101$榆垡SO2+data1101$榆垡NO2+data1101$榆垡O3, data = data1101)

summary(lmyf1)
summary(lmyf3)
summary(lmyf2)

#琉璃河
datapm25_12 <- cbind(datapm25[,(1:3)],datapm25[,32])
nads <- apply(is.na(datapm25_12),1, sum)
datallh = datatime(nads)
abc = as.integer(seq(from = 32,by = 34,to = 168))
data12 = datallh[,abc]

data120 = zhihou(data12)

data1201 = data120[(1:200),]
data1202 = data120[(200:250),]


lmllh1 = lm(data1201$y..1...~data1201$y..n...,data = data1201)
lmllh2 = lm(data1201$y..1...~.,data = data1201)
lmllh3 = lm(data1201$y..1...~data1201$琉璃河CO+data1201$琉璃河SO2+data1201$琉璃河NO2+data1201$琉璃河O3, data = data1201)

summary(lmllh1)
summary(lmllh3)
summary(lmllh2)


#滞后2-10期
#bb

datapm25_mysk <- cbind(datapm25[,(1:3)],datapm25[,28])
nads <- apply(is.na(datapm25_mysk),1, sum)
datadst = datatime(nads)
abc = as.integer(seq(from = 28,by = 34,to = 168))
data1 = datadst[,abc]


zhihou2 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:2),])
  yt1 = data.frame(y[-((n-1):n),])
  x = data[-((n-1):n),-1]
  newdata = cbind(yt,yt1,x)
}

data10_2 = zhihou2(data1)

data101_2 = data10_2[(1:400),]
data102_2 = data10_2[(401:450),]


lmbb1_2 = lm(data101_2$y...1.2....~.,data = data101_2)

summary(lmbb1_2)
cc1 = NULL
cc2 = NULL
cc3 = NULL
for (i in (1:5)){
  c1 = mmse(data102_2,i,lmbb1_2)
  cc1 = c(cc1,c1)
  c2 = mmse(data102_3,i,lmbb1_3)
  cc2 = c(cc2,c2)
  c3 = mmse(data102_4,i,lmbb1_4)
  cc3 = c(cc3,c3)
}
i = 25
c1 = mmse(data102_2,i,lmbb1_2)
c2 = mmse(data102_3,i,lmbb1_3)
c3 = mmse(data102_4,i,lmbb1_4)
c1
c2
c3

zhihou3 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:3),])
  yt1 = data.frame(y[-((n-2):n),])
  x = data[-((n-2):n),-1]
  newdata = cbind(yt,yt1,x)
}

data10_3 = zhihou3(data1)

data101_3 = data10_3[(1:400),]
data102_3 = data10_3[(401:450),]


lmbb1_3 = lm(data101_3$y...1.3....~.,data = data101_3)

summary(lmbb1_3)


zhihou4 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:4),])
  yt1 = data.frame(y[-((n-3):n),])
  x = data[-((n-3):n),-1]
  newdata = cbind(yt,yt1,x)
}

data10_4 = zhihou4(data1)

data101_4 = data10_4[(1:400),]
data102_4 = data10_4[(401:450),]


lmbb1_4 = lm(data101_4$y...1.4....~.,data = data101_4)

summary(lmbb1_4)


#dx

data20_2 = zhihou2(data2)
data20_3 = zhihou3(data2)
data20_4 = zhihou4(data2)

data201_2 = data20_2[(1:400),]
data202_2 = data20_2[(401:450),]

data201_3 = data20_3[(1:400),]
data202_3 = data20_3[(401:450),]

data201_4 = data20_4[(1:400),]
data202_4 = data20_4[(401:450),]


lmdx2_2 = lm(data201_2$y...1.2....~.,data = data201_2)
lmdx2_3 = lm(data201_3$y...1.3....~.,data = data201_3)
lmdx2_4 = lm(data201_4$y...1.4....~.,data = data201_4)

summary(lmdx2_2)
summary(lmdx2_3)
summary(lmdx2_4)


#yz
data30_2 = zhihou2(data3)
data30_3 = zhihou3(data3)
data30_4 = zhihou4(data3)

data301_2 = data30_2[(1:400),]
data302_2 = data30_2[(401:450),]

data301_3 = data30_3[(1:400),]
data302_3 = data30_3[(401:450),]

data301_4 = data30_4[(1:400),]
data302_4 = data30_4[(401:450),]


lmyz2_2 = lm(data301_2$y...1.2....~.,data = data301_2)
lmyz2_3 = lm(data301_3$y...1.3....~.,data = data301_3)
lmyz2_4 = lm(data301_4$y...1.4....~.,data = data301_4)

summary(lmyz2_2)
summary(lmyz2_3)
summary(lmyz2_4)


#pg
data40_2 = zhihou2(data4)
data40_3 = zhihou3(data4)
data40_4 = zhihou4(data4)

data401_2 = data40_2[(1:400),]
data402_2 = data40_2[(401:450),]

data401_3 = data40_3[(1:400),]
data402_3 = data40_3[(401:450),]

data401_4 = data40_4[(1:400),]
data402_4 = data40_4[(401:450),]


lmpg2_2 = lm(data401_2$y...1.2....~.,data = data401_2)
lmpg2_3 = lm(data401_3$y...1.3....~.,data = data401_3)
lmpg2_4 = lm(data401_4$y...1.4....~.,data = data401_4)

summary(lmpg2_2)
summary(lmpg2_3)
summary(lmpg2_4)





#hr
data50_2 = zhihou2(data5)
data50_3 = zhihou3(data5)
data50_4 = zhihou4(data5)

data501_2 = data50_2[(1:400),]
data502_2 = data50_2[(401:450),]

data501_3 = data50_3[(1:400),]
data502_3 = data50_3[(401:450),]

data501_4 = data50_4[(1:400),]
data502_4 = data50_4[(401:450),]


lmhr2_2 = lm(data501_2$y...1.2....~.,data = data501_2)
lmhr2_3 = lm(data501_3$y...1.3....~.,data = data501_3)
lmhr2_4 = lm(data501_4$y...1.4....~.,data = data501_4)

summary(lmhr2_2)
summary(lmhr2_3)
summary(lmhr2_4)




#my
data60_2 = zhihou2(data6)
data60_3 = zhihou3(data6)
data60_4 = zhihou4(data6)

data601_2 = data60_2[(1:400),]
data602_2 = data60_2[(401:450),]

data601_3 = data60_3[(1:400),]
data602_3 = data60_3[(401:450),]

data601_4 = data60_4[(1:400),]
data602_4 = data60_4[(401:450),]


lmmy2_2 = lm(data601_2$y...1.2....~.,data = data601_2)
lmmy2_3 = lm(data601_3$y...1.3....~.,data = data601_3)
lmmy2_4 = lm(data601_4$y...1.4....~.,data = data601_4)

summary(lmmy2_2)
summary(lmmy2_3)
summary(lmmy2_4)



#yq
data70_2 = zhihou2(data7)
data70_3 = zhihou3(data7)
data70_4 = zhihou4(data7)

data701_2 = data70_2[(1:400),]
data702_2 = data70_2[(401:450),]

data701_3 = data70_3[(1:400),]
data702_3 = data70_3[(401:450),]

data701_4 = data70_4[(1:400),]
data702_4 = data70_4[(401:450),]


lmyq2_2 = lm(data701_2$y...1.2....~.,data = data701_2)
lmyq2_3 = lm(data701_3$y...1.3....~.,data = data701_3)
lmyq2_4 = lm(data701_4$y...1.4....~.,data = data701_4)

summary(lmyq2_2)
summary(lmyq2_3)
summary(lmyq2_4)




#bdl
data80_2 = zhihou2(data8)
data80_3 = zhihou3(data8)
data80_4 = zhihou4(data8)

data801_2 = data80_2[(1:200),]
data802_2 = data80_2[(201:250),]

data801_3 = data80_3[(1:200),]
data802_3 = data80_3[(201:250),]

data801_4 = data80_4[(1:200),]
data802_4 = data80_4[(201:250),]


lmbdl2_2 = lm(data801_2$y...1.2....~.,data = data801_2)
lmbdl2_3 = lm(data801_3$y...1.3....~.,data = data801_3)
lmbdl2_4 = lm(data801_4$y...1.4....~.,data = data801_4)

summary(lmbdl2_2)
summary(lmbdl2_3)
summary(lmbdl2_4)



#dgc
data90_2 = zhihou2(data9)
data90_3 = zhihou3(data9)
data90_4 = zhihou4(data9)

data901_2 = data90_2[(1:200),]
data902_2 = data90_2[(201:250),]

data901_3 = data90_3[(1:200),]
data902_3 = data90_3[(201:250),]

data901_4 = data90_4[(1:200),]
data902_4 = data90_4[(201:250),]


lmdgc2_2 = lm(data901_2$y...1.2....~.,data = data901_2)
lmdgc2_3 = lm(data901_3$y...1.3....~.,data = data901_3)
lmdgc2_4 = lm(data901_4$y...1.4....~.,data = data901_4)

summary(lmdgc2_2)
summary(lmdgc2_3)
summary(lmdgc2_4)


#yld
data100_2 = zhihou2(data10)
data100_3 = zhihou3(data10)
data100_4 = zhihou4(data10)

data1001_2 = data100_2[(1:400),]
data1002_2 = data100_2[(401:450),]

data1001_3 = data100_3[(1:400),]
data1002_3 = data100_3[(401:450),]

data1001_4 = data100_4[(1:400),]
data1002_4 = data100_4[(401:450),]


lmyld2_2 = lm(data1001_2$y...1.2....~.,data = data1001_2)
lmyld2_3 = lm(data1001_3$y...1.3....~.,data = data1001_3)
lmyld2_4 = lm(data1001_4$y...1.4....~.,data = data1001_4)

summary(lmyld2_2)
summary(lmyld2_3)
summary(lmyld2_4)



#yf
data110_2 = zhihou2(data11)
data110_3 = zhihou3(data11)
data110_4 = zhihou4(data11)

data1101_2 = data110_2[(1:200),]
data1102_2 = data110_2[(201:250),]

data1101_3 = data110_3[(1:200),]
data1102_3 = data110_3[(201:250),]

data1101_4 = data110_4[(1:200),]
data1102_4 = data110_4[(201:250),]


lmyf2_2 = lm(data1101_2$y...1.2....~.,data = data1101_2)
lmyf2_3 = lm(data1101_3$y...1.3....~.,data = data1101_3)
lmyf2_4 = lm(data1101_4$y...1.4....~.,data = data1101_4)

summary(lmyf2_2)
summary(lmyf2_3)
summary(lmyf2_4)



#llh
data120_2 = zhihou2(data12)
data120_3 = zhihou3(data12)
data120_4 = zhihou4(data12)

data1201_2 = data120_2[(1:200),]
data1202_2 = data120_2[(201:250),]

data1201_3 = data120_3[(1:200),]
data1202_3 = data120_3[(201:250),]

data1201_4 = data120_4[(1:200),]
data1202_4 = data120_4[(201:250),]


lmllh2_2 = lm(data1201_2$y...1.2....~.,data = data1201_2)
lmllh2_3 = lm(data1201_3$y...1.3....~.,data = data1201_3)
lmllh2_4 = lm(data1201_4$y...1.4....~.,data = data1201_4)

summary(lmllh2_2)
summary(lmllh2_3)
summary(lmllh2_4)




zhihou5 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:5),])
  yt1 = data.frame(y[-((n-4):n),])
  x = data[-((n-4):n),-1]
  newdata = cbind(yt,yt1,x)
}

data10_5 = zhihou5(data1)

data101_5 = data10_5[(1:400),]
data102_5 = data10_5[(401:450),]


lmbb1_5 = lm(data101_5$y...1.5....~.,data = data101_5)

summary(lmbb1_5)


zhihou6 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:6),])
  yt1 = data.frame(y[-((n-5):n),])
  x = data[-((n-5):n),-1]
  newdata = cbind(yt,yt1,x)
}


data10_6 = zhihou6(data1)

data101_6 = data10_6[(1:400),]
data102_6 = data10_6[(401:450),]


lmbb1_6 = lm(data101_6$y...1.6....~.,data = data101_6)

summary(lmbb1_6)


zhihou7 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:7),])
  yt1 = data.frame(y[-((n-6):n),])
  x = data[-((n-6):n),-1]
  newdata = cbind(yt,yt1,x)
}


data10_7 = zhihou7(data1)

data101_7 = data10_7[(1:400),]
data102_7 = data10_7[(401:450),]


lmbb1_7 = lm(data101_7$y...1.7....~.,data = data101_7)

summary(lmbb1_7)



zhihou8 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:8),])
  yt1 = data.frame(y[-((n-7):n),])
  x = data[-((n-7):n),-1]
  newdata = cbind(yt,yt1,x)
}


data10_8 = zhihou8(data1)

data101_8 = data10_8[(1:400),]
data102_8 = data10_8[(401:450),]


lmbb1_8 = lm(data101_8$y...1.8....~.,data = data101_8)

summary(lmbb1_8)



zhihou9 <- function(data){
  np = dim(data)
  n = np[1]
  p = np[2]
  y = data.frame(data[,1])
  yt = data.frame(y[-(1:9),])
  yt1 = data.frame(y[-((n-8):n),])
  x = data[-((n-8):n),-1]
  newdata = cbind(yt,yt1,x)
}


data10_9 = zhihou9(data1)

data101_9 = data10_9[(1:400),]
data102_9 = data10_9[(401:450),]


lmbb1_9 = lm(data101_9$y...1.9....~.,data = data101_9)

summary(lmbb1_9)

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

