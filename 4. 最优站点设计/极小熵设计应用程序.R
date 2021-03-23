#对应正文中的图5，6，7的程序
xmin=0;ymin=0;
xmax=1;ymax=1;
a=25;
plot(c(xmin,xmax),c(xmin,ymax),col="white",
     xlab = "x",ylab = "y");
#定义距离函数如下：
dist<-function(a,b)
{
  if(a[1]==b[1])
    return(0.00001);
  distance=(a[1]-b[1])^2+(a[2]-b[2])^2;
  distance=distance^(1/2);
  return(distance);
}
dist_design<-function(m)
{
  k=8;
  original_dist=0;
  r=nrow(m);
  for(i in 1:(r-1))
  {
    for(j in (i+1):r)
    {
      original_dist=original_dist+1/((dist(m[i,],m[j,]))^(k));
    }
  }
  original_dist=original_dist^(1/k);
  return(original_dist);
}
dist_additional_design<-function(m,d,addm)
{
  k=8;
  original_dist=d;
  for(i in 1:nrow(addm))
  {
    
    original_dist=original_dist+1/((dist(addm[i,],m))^(k));
  }
  
  for(i in 1:(nrow(addm)-1))
  {
    for(j in (i+1):nrow(addm))
    {
      original_dist=original_dist+1/((dist(addm[i,],addm[j,]))^(k));
    }
  }
  original_dist=original_dist^(1/k);
  return(original_dist);
}

xnumber=20;ynumber=20;
x_star=xmin+(xmax-xmin)/
  xnumber*(0:xnumber);
y_star=ymin+(ymax-ymin)/
  ynumber*(0:ynumber);
x_grid=rep(x_star[1],ynumber+1);
for(i in 1:xnumber)
{
  x_grid=append(x_grid,rep(x_star[i+1],ynumber+1))
}
x_grid
y_grid=rep(y_star,xnumber+1);

abline(v=seq(0,1, 0.05), lty=2, col="blue")  #参考线（垂直线）
abline(h=seq(0,1,0.05),lty=2, col="blue")
#points(x_grid,y_grid,pch=3);
potential_point=matrix(append(y_grid,x_grid),c(length(y_grid),2))
x1 <- matrix(c(0.5,0.5),nrow=1)
points(x1,pch=5,col="red")

row_label=sample(1:length(y_grid),a);
row_label
#此步骤为随机步，相当于初始点，之后我们会进行迭代更新
add_design_point=potential_point[row_label,];
add_design_point
preset=500;
point=rbind(x1,add_design_point);
point
adnp=add_design_point;
#DDDN=dist_design(SNE);
dististance=dist_design(point);
dististance
for(h in 1:preset)
{
  for(i in 1:a)
  {
    for(j in 1:length(y_grid))
    {
      add_design_point[i,]=potential_point[j,];
      d=dist_design(rbind(x1,add_design_point))
      if(dististance>d)
      {
        adnp=add_design_point;
        #station_north=rbind(SNE,add_design_north);
        dististance=d;
      }
    }
    add_design_point=adnp;
  }
}
points(adnp[,1],adnp[,2],pch=5,col=2)