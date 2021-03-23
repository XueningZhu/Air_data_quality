#对应论文中的图13，14，15，16，17，18的程序
library(maptools)
library(maps)
library(mapdata)
china <- readShapePoly("bou2_4p.shp")
cities <- readShapePoints('res2_4m.shp')
capital <- readShapePoints('res1_4m.shp')
x <- china
getColor <- function(mapdata,provname,provcol,othercol){
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(mapdata$NAME,f,provname);
  color=c(othercol,provcol)[colIndex+1];
  return(color);
}
#画出北京市地图
capital=c("北京市");
f <- function(x,y) ifelse(x %in% y,which(y==x),0);
id <- which(sapply(iconv(china$NAME,"gbk","utf-8"),f,capital)==1)
plot(
  x@polygons[[id]]@Polygons[[1]]@coords,
  type = "l",
  xlab = "longtitude",
  ylab = "latitude"
)
#读取已有的空气监测站点的经纬度，并用不同的颜色以区分站点所在的大致方位
station_exist=read.table(file="stations.txt");
station_exist_north=station_exist[(station_exist[,1]>40.08),];
points(station_exist_north[,2],station_exist_north[,1],pch=20);
station_exist_south=station_exist[(station_exist[,1]<39.85),];
points(station_exist_south[,2],station_exist_south[,1],pch=20,col="red");
station_exist_center=station_exist[((station_exist[,1]>39.85)
                                    &(station_exist[,1]<40.08)),];
points(station_exist_center[,2],station_exist_center[,1],pch=20,col="blue");
grid()
#设定北部，中部和南部增加站点的数量
add_num_north=3;
add_num_center=4;
add_num_south=6;
dist<-function(a,b)
{
  if(a[1]==b[1])
    return(0.00001);
  
  distance=(a[1]-b[1])^2+(a[2]-b[2])^2;
  distance=distance^(1/2);
  return(distance);
}
# the P-norm distance of the design
dist_design<-function(m)
{
  p=2;
  original_dist=0;
  r=nrow(m);
  for(i in 1:(r-1))
  {
    for(j in (i+1):r)
    {
      original_dist=original_dist+1/((dist(m[i,],m[j,]))^(p));
    }
  }
  original_dist=original_dist^(1/p);
  return(original_dist);
}
dist_additional_design<-function(m,d,addm)
{
  p=2;
  original_dist=d;
  for(i in 1:nrow(addm))
  {
    for(j in 1:nrow(m))
    {
      original_dist=original_dist+1/((dist(addm[i,],m[j,]))^(p));
    }
  }
  for(i in 1:(nrow(addm)-1))
  {
    for(j in (i+1):nrow(addm))
    {
      original_dist=original_dist+1/((dist(addm[i,],addm[j,]))^(p));
    }
  }
  original_dist=original_dist^(1/p);
  return(original_dist);
}
#人为划定要加入点的区域范围，即只在这三块区域增加点
longitude_min_north<-116.2
latitude_min_north<-40.05
longitude_max_north<-116.6
latitude_max_north<-40.3
#这些经纬度值是自己设定的设计区域，可根据要求实时修改
longitude_min_south=min(station_exist_south[,2]);
latitude_min_south<-39.625
longitude_max_south=max(station_exist_south[,2]);
latitude_max_south=max(station_exist_south[,1]);
longitude_min_center=min(station_exist_center[,2]);
latitude_min_center=min(station_exist_center[,1]);
longitude_max_center=116.72;
latitude_max_center=max(station_exist_center[,1]);
latitude_max_south=max(station_exist_south[,1]);
latitude_min_center=min(station_exist_center[,1]);
latitude_max_center=max(station_exist_center[,1]);
latitude_min_north=min(station_exist_north[,1]);
# the grids of potential design points
# this step provides discrete choices for the design points
N_north_latitude=10;N_north_longitude=20;
N_center_latitude=7;N_center_longitude=17;
N_south_latitude=10;N_south_longitude=20;
grid_north_latitude=latitude_min_north+(latitude_max_north-latitude_min_north)/
  N_north_latitude*(0:N_north_latitude);
grid_north_longitude=longitude_min_north+(longitude_max_north-longitude_min_north)/
  N_north_longitude*(0:N_north_longitude);
grid_center_latitude=latitude_min_center+(latitude_max_center-latitude_min_center)/
  N_center_latitude*(0:N_center_latitude);
grid_center_longitude=longitude_min_center+(longitude_max_center-longitude_min_center)/
  N_center_longitude*(0:N_center_longitude);
grid_south_latitude=latitude_min_south+(latitude_max_south-latitude_min_south)/
  N_south_latitude*(0:N_south_latitude);
grid_south_longitude=longitude_min_south+(longitude_max_south-longitude_min_south)/
  N_south_longitude*(0:N_south_longitude);
grid_north_la=rep(grid_north_latitude[1],N_north_longitude+1);

for(i in 1:N_north_latitude)
{
  grid_north_la=append(grid_north_la,rep(grid_north_latitude[i+1],N_north_longitude+1))
}
grid_north_lo=rep(grid_north_longitude,N_north_latitude+1);
#points(grid_north_lo,grid_north_la,pch=3);
grid_center_la=rep(grid_center_latitude[1],N_center_longitude+1);
for(i in 1:N_center_latitude)
{
  grid_center_la=append(grid_center_la,rep(grid_center_latitude[i+1],N_center_longitude+1))
}
grid_center_lo=rep(grid_center_longitude,N_center_latitude+1);
#points(grid_center_lo,grid_center_la,pch=3);
grid_south_la=rep(grid_south_latitude[1],N_south_longitude+1);
for(i in 1:N_south_latitude)
{
  grid_south_la=append(grid_south_la,rep(grid_south_latitude[i+1],N_south_longitude+1))
}
grid_south_lo=rep(grid_south_longitude,N_south_latitude+1);
#points(grid_south_lo,grid_south_la,pch=3);
potential_north=matrix(append(grid_north_lo,grid_north_la),c(length(grid_north_lo),4));
#得到所有北部的可能点，同理下面得到中部和南部的所有可能点
potential_center=matrix(append(grid_center_lo,grid_center_la),c(length(grid_center_lo),2));
potential_south=matrix(append(grid_south_lo,grid_south_la),c(length(grid_south_lo),2));
row_label_north=sample(1:length(grid_north_lo),add_num_north);
#此步骤为随机步，相当于初始点，之后我们会进行迭代更新
add_design_north=potential_north[row_label_north,];
#得到选出的这几个可能的要增加的点的经纬度坐标
row_label_center=sample(1:length(grid_center_lo),add_num_center);
add_design_center=potential_center[row_label_center,];
row_label_south=sample(1:length(grid_south_lo),add_num_south);
add_design_south=potential_south[row_label_south,];
#最关键的程序
myfunction <- function(station_exist1,station,add_design,add_num,grid_lo, potential)
{
  SE=station_exist1;
  SE[,1]=station_exist1[,2];
  SE[,2]=station_exist1[,1];
  preset=5;
  station=rbind(SE,add_design);
  ad=add_design;
  DD=dist_design(SE);
  distance=dist_design(station);
  for(h in 1:preset)
  {
    for(i in 1:add_num)
    {
      for(j in 1:length(grid_lo))
      {
        add_design[i,]=potential[j,];
        d=dist_additional_design(SE,DD,add_design)
        if(distance>d)
        {
          ad=add_design;
          distance=d;
        }
      }
      add_design=ad;
    }
  }
  return(add_design)
}
ad_north <- myfunction(station_exist_north,station_north,add_design_north,add_num_north,grid_north_lo,
                       potential_north)
points(ad_north[,1],ad_north[,2],pch=3,col="black")
ad_south<-myfunction(station_exist_south,station_south,add_design_south,add_num_south,grid_south_lo,
                     potential_south)
points(ad_south[,1],ad_south[,2],pch=3,col="red")
ad_center<-myfunction(station_exist_center,station_center,add_design_center,add_num_center,grid_center_lo,
                      potential_center)
points(ad_center[,1],ad_center[,2],pch=3,col="blue")
#给出新增站点的经纬度
write.csv(ad_north,"新加入北部点的经纬度.csv", row.names=F)
write.csv(ad_center,"新加入中部点的经纬度.csv", row.names=F)
write.csv(ad_south,"新加入南部点的经纬度.csv", row.names=F)
all=rbind(ad_north,ad_center,ad_south)
write.csv(all,"新加入所有点的经纬度.csv", row.names=F)

