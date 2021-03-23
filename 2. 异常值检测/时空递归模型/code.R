# ---------------------------------------------------- #
# 0. Load Packages
# ---------------------------------------------------- #
library(dplyr)
library(reshape2)
library(car)
library(tidyr)
library(MASS)
library(plyr)
library(ggplot2)
library(ggmap)
library(Hmisc)
library(leaflet)

# ---------------------------------------------------- #
# 1. Data Preparation
# ---------------------------------------------------- #

## load data
load("data_select.Rdata")
load("dist_matrix.Rdata")

## Set random seed
set.seed(1234)

# all pm2.5 concentrations
Ymat <- data_select %>%
  dplyr::select(avg_pm2_5, station_code, date) %>%
  as.data.frame() %>%
  spread(key = station_code, value = avg_pm2_5) %>%
  data.matrix() %>%
  t() %>%
  .[-1,]

# scaled distance matrix
W <- as.matrix(1/dist_matrix) / apply(as.matrix(1/dist_matrix),1,sum)

## reorginaize data for least square
N <- nrow(Ymat)       # number of stations
Time <- ncol(Ymat)    # number of monthes
WYmat <- W %*% Ymat   # weighted spatial matrix

# explanatory variables
X <- cbind(1,                                         # intercept
           as.vector(WYmat[, -c(1:11,Time)]),         # seasonal term
           as.vector(Ymat[, -c((Time-11):Time)])) %>% # spatial lag
  as.matrix()

# response variable
Y <- as.vector(Ymat[,-c(1:12)])

# ---------------------------------------------------- #
# 2. Descriptive Data Analysis
# ---------------------------------------------------- #

# hist(Y)

# Fig 1
pollutants_2 <- c("PM2.5", "PM10", "SO2", "NO2", "O3", "CO","AVG")
data_select %>%
  # groupby year and region
  group_by(year, region_py) %>%
  # calculate mean pm2.5 for each group
  summarise(PM2.5 = mean(avg_pm2_5,na.rm = T)) %>%
  mutate(region_en = recode(as.character(region_py),
                            "huabei" = "North",
                            "huazhong" = "Central",
                            "huanan" = "South",
                            "huadong" = "East",
                            "xibei" = "Northwest",
                            "xinan" = "Southwest",
                            "dongbei" = "Northeast")) %>%
  # calculate mean pm2.5 for all data
  bind_rows(data_select %>%
              group_by(year) %>%
              summarise(PM2.5 = mean(avg_pm2_5,na.rm = T)) %>%
              mutate(region_en = "All")) %>%
  mutate(region = factor(region_en,
                         levels = c('Central', 'North', 'East', 'South',
                                    'Northeast', 'Northwest',
                                    'Southwest', 'All'))) %>%
  # plot
  ggplot(aes(x = region, y = factor(year))) +
  geom_tile(aes(fill = PM2.5)) +
  # label every tile
  geom_text(aes(label = sprintf("%.2f", PM2.5)),
            vjust = 1, col="black") +
  # xticks & yticks
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  # legend & theme
  scale_fill_gradient2(low = "darkgreen",
                       mid = "yellow", high ="red3",
                       midpoint = 50) +
  # labs & theme
  labs(x='Region', y='Year',
       fill = expression(atop("Avg PM2.5","("~mu~"g/"~m^3~")"))) +
  theme(axis.text = element_text(face = "bold",size=10),
        axis.title = element_text(face = "bold",size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_rect(fill='transparent',
                                  color='transparent'),
        axis.line = element_line(color = "black")) +
  # save plot
  ggsave(filename = 'region_year.png', width = 8, height = 5)

# Fig 2
data_select %>%
  # select target stations and columns needed
  dplyr::filter(station_code %in% c("G533301", "G130502", "G110006", "G440104")) %>%
  dplyr::select(date, city_py, avg_pm2_5) %>%
  # plot
  ggplot(aes(x=as.Date(date), y=avg_pm2_5, colour=city_py)) + geom_line(size=1)+
    scale_x_date(date_labels="%Y-%m")+
  # labs & theme
    labs(x="",y="PM2.5 concentration", colour="Cities")+
    theme(axis.text = element_text(size=12), 
          axis.title = element_text(size=15),
          panel.background = element_rect(fill = "transparent"),
          panel.border=element_rect(fill='transparent', 
                                    color='transparent'),
          axis.line = element_line(color = "black"),
          strip.text = element_text(size=10),
          strip.background =element_rect(fill="white"),
          plot.title = element_text(hjust = 0.5,size = 15,face = "bold"),
          legend.key  = element_blank())

# Fig 3
# time-series plot acf
yn_series <- Ymat[rownames(Ymat)=="G533301", ]
hb_series <- Ymat[rownames(Ymat)=="G130502", ]
bj_series <- Ymat[rownames(Ymat)=="G110006", ]
gd_series <- Ymat[rownames(Ymat)=="G440104", ]

par(mfrow = c(1,1))
acf(ts(diff(yn_series,lag=12)),lag.max = 40,main = "Nujiang")
acf(ts(diff(hb_series,lag=12)),lag.max = 40,main = "Xingtai")
acf(ts(diff(bj_series,lag=12)),lag.max = 40,main = "Beijing")
acf(ts(diff(gd_series,lag=12)),lag.max = 40,main = "Guangzhou")

# Fig 4
# order stations based on PM2.5 level, select representative stations
low <- PM2.5[order(PM2.5$PM2.5),]
high <- PM2.5[order(-PM2.5$PM2.5),]

# data of the station(a) with high PM2.5 level and its neighbor(b)
a <- data_select[data_select$station_code == "G130502",c(33,45)]
b <- data_select[data_select$station_code == "G130501",c(33,45)]

b %>%
  # mutate two datasets
  mutate(avg_pm2_5.z = c(a$avg_pm2_5[-1],8888)) %>%
  # match level of station a at time t and level of station b at time t-1
  filter(avg_pm2_5.z!=8888) %>%
  # plot
  ggplot(aes(x = avg_pm2_5.z,y = avg_pm2_5))+
  geom_point(stat = "identity", size = 2)+
  # labs & theme
  labs(x="levels of target station at t",y="levels of neighbors at t-1",title = "Xingtai")+
  theme(axis.text = element_text(face = "bold",size=10), 
        axis.title = element_text(face = "bold",size=15),
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_rect(fill='transparent', 
                                  color='transparent'),
        axis.line = element_line(color = "black"),
        strip.text = element_text(face = "bold",size=10),
        strip.background =element_rect(fill="white"),
        plot.title = element_text(hjust = 0.5,size = 15,face = "bold"))


# data of the station(a) with low PM2.5 level and its neighbor(b)
a <- data_select[data_select$station_code == "G533301",c(33,45)]
b <- data_select[data_select$station_code == "G530302",c(33,45)]

b %>%
  mutate(avg_pm2_5.z = c(a$avg_pm2_5[-1],8888)) %>%
  filter(avg_pm2_5.z!=8888) %>%
  ggplot(aes(x = avg_pm2_5.z,y = avg_pm2_5))+
  geom_point(stat = "identity", size = 2)+
  labs(x="levels of target station at t",y="levels of neighbors at t-1",title = "Nujiang")+
  theme(axis.text = element_text(face = "bold",size=10), 
        axis.title = element_text(face = "bold",size=15),
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_rect(fill='transparent', 
                                  color='transparent'),
        axis.line = element_line(color = "black"),
        strip.text = element_text(face = "bold",size=10),
        strip.background =element_rect(fill="white"),
        plot.title = element_text(hjust = 0.5,size = 15,face = "bold"))

# data of the station(a) in Beijing and its neighbor(b)
a <- data_select[data_select$station_name_py == "aotizhongxin",c(33,45)]
b <- data_select[data_select$station_name_py == "nongzhanguan",c(33,45)]

b %>%
  mutate(avg_pm2_5.z = c(a$avg_pm2_5[-1],8888)) %>%
  filter(avg_pm2_5.z!=8888) %>%
  ggplot(aes(x = avg_pm2_5.z,y = avg_pm2_5))+
  geom_point(stat = "identity", size = 2)+
  labs(x="levels of target station at t",y="levels of neighbors at t-1",title = "Beijing")+
  theme(axis.text = element_text(face = "bold",size=10), 
        axis.title = element_text(face = "bold",size=15),
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_rect(fill='transparent', 
                                  color='transparent'),
        axis.line = element_line(color = "black"),
        strip.text = element_text(face = "bold",size=10),
        strip.background =element_rect(fill="white"),
        plot.title = element_text(hjust = 0.5,size = 15,face = "bold"))

# data of the station(a) in Guangzhou and its neighbor(b)
a <- data_select[data_select$station_name_py == "shiwuzhong",c(33,45)]
b <- data_select[data_select$station_name_py == "shibashiliuzhong",c(33,45)]

b %>%
  mutate(avg_pm2_5.z = c(a$avg_pm2_5[-1],8888)) %>%
  filter(avg_pm2_5.z!=8888) %>%
  ggplot(aes(x = avg_pm2_5.z,y = avg_pm2_5))+
  geom_point(stat = "identity", size = 2)+
  labs(x="levels of target station at t",y="levels of neighbors at t-1",title = "Guangzhou")+
  theme(axis.text = element_text(face = "bold",size=10), 
        axis.title = element_text(face = "bold",size=15),
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_rect(fill='transparent', 
                                  color='transparent'),
        axis.line = element_line(color = "black"),
        strip.text = element_text(face = "bold",size=10),
        strip.background =element_rect(fill="white"),
        plot.title = element_text(hjust = 0.5,size = 15,face = "bold"))

# ---------------------------------------------------- #
# 3. Data Modeling
# ---------------------------------------------------- #

## 3.1 Overall Model
Data <- as.data.frame(cbind(Y, X[,-1]))
lm.fit_1 <- lm(Y~., data = Data)
# summary(lm.fit_1)
# vif(lm.fit_1)
# plot(lm.fit_1,which=1) # residual plot
# qqnorm(lm.fit_1$residuals)
# qqline(lm.fit_1$residuals)

## 3.2 Model by region
## 3.2.1 Estimation
code_region <- data_select %>%
  dplyr::select('station_code','region_py') %>%
  unique()

region <- as.data.frame(unique(code_region$region_py))

Ymat_region <- cbind(as.data.frame(Ymat), code_region$region)
WYmat_region <- cbind(as.data.frame(WYmat), code_region$region)

coeff <- data.frame()
for (i in 1:nrow(region)){
  Ymat_spec <- Ymat_region %>%
    filter(code_region$region == region[i,1]) %>%
    dplyr::select(-c('code_region$region')) %>%
    as.matrix()
  WYmat_spec <- WYmat_region %>%
    filter(code_region$region == region[i,1]) %>%
    dplyr::select(-c('code_region$region')) %>%
    as.matrix()

  X_reg <- cbind(as.vector(WYmat_spec[,-c(1:11, ncol(WYmat_spec))]),
                 as.vector(Ymat_spec[,-((ncol(Ymat_spec)-11):ncol(Ymat_spec))]))
  Data_reg <- as.data.frame(cbind(as.vector(Ymat_spec[,-(1:12)]), X_reg))

  lm.fit <- lm(Data_reg$V1~., data = Data_reg)
  coeff <- rbind(coeff, lm.fit$coefficients)
}
colnames(coeff) <- c("Intercept", "Spatial", "Serial")

## 3.2.2 average PM2.5 for each region
data_select %>%
  dplyr::select(avg_pm2_5,region,month) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(avg = mean(avg_pm2_5, na.rm=T))

# ---------------------------------------------------- #
# 4. EM Algorithm
# ---------------------------------------------------- #

# function to calculate OLS
betaOLS <- function(Yvec, X){
  # Calculate OLS for time-space recursive model

  # Args:
  # Yvec - response
  # X - explanatory variables

  # calculate inverse of cross product {t(X)X}^{-1}
  invXX <- solve(crossprod(X))

  # OLS estimation for theta
  thetaEst <- invXX %*% colSums(X * Yvec)

  # estimate sigma square
  sigmaHat2 <- mean((Yvec - X %*% thetaEst)^2)

  # covariance for hat theta
  covHat <- invXX * sigmaHat2

  # return the result, a list of estimates for theta, sigma and cov
  return(list(theta = thetaEst, covHat = covHat, sigmaHat = sqrt(sigmaHat2)))
}

# function for EM algorithm
EM.TS <- function(Y, X, K, n=N, t=Time){
  # EM Algorithm for time-space recursive model

  # Args:
  # Y - response
  # X - explanatory variables
  # K - number of groups (K > 1)
  # N - number of stations
  # Time - number of monthes

  ### set initial values for the algorithm
  # weights of each normal
  alpha <- rep(1/K, K)
  # generate the initial estimator by OLS
  nar_para <- betaOLS(Y, X)
  # generate the initial values by small perturbations of OLS estimator
  theta <- t(sapply(nar_para$theta,
                    function(x) runif(K, min = x - 0.05, max = x + 0.05)))
  # for simplicity set sigma the same for initial values
  sigma <- rep(nar_para$sigmaHat, K)
  # initialize posterior probs
  ezK <- matrix(0, ncol = K, nrow = n)
  # convergence difference
  delta <- 1

  ### start the EM algorithm
  while(delta > 10^-5){
    ### E-step
    # scaled residuals
    eps_hat <- (Y - X %*% theta) %*% diag(x = 1/sigma)
    # update posterior probs (log scaled)
    for (k in 1:K)
    {
      eps_mat <- matrix(eps_hat[,k], ncol = t - 12)
      ezK[,k] <- -(t - 1) * log(sigma[k]) - rowSums(eps_mat^2/2) + log(alpha[k])
    }

    # centered by mean and exp-transform back to obtain the probability
    ezK <- ezK - rowMeans(ezK)
    ezK <- exp(ezK) / rowSums(exp(ezK))
    ezK[is.na(ezK)] <- 1

    ### M-step

    # parameters from t-1
    theta0 <- theta; sigma0 <- sigma; alpha0 <- alpha

    # update rule
    for (k in 1:K)
    {
      # calculate new weighted X
      zz <- rep(ezK[,k], t - 12)
      X_new <- zz * X

      # update theta and sigma
      theta[,k] <- ginv(crossprod(X_new, X)) %*% crossprod(X_new, Y)
      sigma[k] <- sqrt(sum(zz*(Y - X %*% theta[,k])^2) / sum(zz))

      # if the sigma is too small, change it to a small number
      if (sigma[k]==0 | is.na(sigma[k])) {
        sigma[k] = 10^-4
      }
    }

    # update alpha and delta
    alpha <- colSums(ezK) / n
    delta <- max(abs(c(theta - theta0, sigma - sigma0, alpha - alpha0)))
  }
  return(list(theta = theta, alpha = alpha, sigma = sigma, ezK = ezK))
}

# reture em result
EM.result <- EM.TS(Y, X, K=3)
group_em <- apply(EM.result$ezK, 1, which.max)
code_region$group <- group_em

# group 1 - high pollution group
group_3 <- code_region %>% filter(group == 3)

tmp <- data_select %>%
  filter(station_code %in% as.character(group_3$station_code)) %>%
  dplyr::select(avg_pm2_5, station_code, date, city_en, province_en,
                station_name_py, region_py, city_py) %>%
  group_by(station_code, station_name_py) %>%
  dplyr::summarise(region_py = first(region_py),
                   city_en = first(city_en),
                   province_en = first(province_en),
                   avg_pm2_5 = round(mean(avg_pm2_5),2))
write.table(tmp, file = "group3.txt", sep = '&')

# calculate mean of PM2.5 for each group
data_select %>%
  dplyr::select(avg_pm2_5, station_code) %>%
  dplyr::group_by(station_code) %>%
  dplyr::summarise(avg = mean(avg_pm2_5,na.rm=T)) %>%
  cbind(group_em) %>%
  dplyr::group_by(group_em) %>%
  dplyr::summarise(avg = mean(avg,na.rm=T))

# plot em map
group_em <- ifelse(group_em == 3, 1, ifelse(group_em == 1, 2, 3))
code_city <- data_select %>%
  dplyr::select(station_code, city, lat, lon) %>%
  unique() %>%
  mutate(EM = factor(group_em))

pal <- colorFactor(c("red", "orange","green"),
                   domain = 1:3)
leaflet(code_city) %>% addTiles() %>%
  addCircleMarkers(lng=~lon,lat=~lat,color = ~pal(group_em)) %>%
  addLegend("bottomright",pal=pal,values = ~group_em,title = "Group",opacity=1)

# ---------------------------------------------------- #
# 5. Outlier Detection
# ---------------------------------------------------- #

# posterior theta for each n
theta_hat <- do.call(rbind, replicate(Time - 12,
                                      EM.result$ezK %*% t(EM.result$theta),
                                      simplify=FALSE))
# em residuals
EM.eps <- Y - rowSums(X * theta_hat)
# sigma hat
sigmaHat <- sqrt(sum(EM.eps^2) / (N*Time-3))

t_it <- c()
tmp <- solve(t(X) %*% X)
for(i in 1:(N*(Time-12))){
  X_it <- matrix(X[i,], 1, 3)
  scale_it <- sqrt(1 + X_it %*% tmp %*% t(X_it))
  t_it[i] <- EM.eps[i] / (sigmaHat * scale_it)
}
hist(t_it)
sum(abs(t_it)>2.5)

# Time outlier visualization
data_select %>%
  dplyr::filter(city_py %in% c("shijiazhuang", "hengshui", "changzhi"),
                year %in% 2016:2017) %>%
  group_by(date, city_py) %>%
  dplyr::summarise(mean_pm2_5 = mean(avg_pm2_5, na.rm = T)) %>%
  mutate(City = capitalize(as.character(city_py)),
         Date = as.Date(date)) %>%
  ggplot() +
  geom_line(aes(x = Date, y = mean_pm2_5, colour=City), size = 1) +
  scale_x_date(date_labels = "%Y-%m") +
  labs(x = "", y = "PM2.5 Concentration") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_rect(fill='transparent',
                                  color='transparent'),
        axis.line = element_line(color = "black"),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 15,face = "bold")) +
  ggsave(filename = "PIC/graph2/Shijiazhuang2.png", height = 8, width = 8)

# Spatial Outlier visualization
temp <- data_select %>%
  filter(date == '2016-12-01', province_py=='hebei') %>%
  dplyr::select(avg_pm2_5, station_name_py, lon, lat)

ggmap(get_googlemap(c(mean(temp$lon), mean(temp$lat)),
                    zoom = 7,size=c(640,640)))+
  geom_point(aes(x=lon, y=lat, color=avg_pm2_5),
             data = temp, alpha = .6, size = 3) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  # legend & theme
  scale_color_gradient2(low = "darkgreen",
                        mid = "yellow", high ="red3",
                        midpoint = 200) +
  # labs & theme
  labs(color = expression(atop("Avg PM2.5","("~mu~"g/"~m^3~")"))) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border=element_rect(fill='transparent',
                                  color='transparent')) +
  ggsave(filename = "Shijiazhuang.png", width = 9, height = 8)

# outlier tables
outlier_data <- data.frame(outlier = (abs(t_it)>2.5)*1) %>%
  cbind(station_code = rep(rownames(Ymat),ncol(Ymat)-12)) %>%
  cbind(date = rep(data_select$date[13:48], each = nrow(Ymat))) %>%
  left_join(data_select %>%
              dplyr::select(station_code, date, month, year, city_py, region_py),
            by = c('station_code', 'date')) %>%
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  6:8  ~ "Summer",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Winter"))

outlier_data %>%
  group_by(city_py) %>%
  dplyr::summarise(count = sum(outlier)) %>%
  dplyr::arrange(desc(count)) %>%
  head(10)

outlier_data %>%
  group_by(season) %>%
  dplyr::summarise(count = sum(outlier))

outlier_data %>%
  group_by(year) %>%
  dplyr::summarise(count = sum(outlier))

# ---------------------------------------------------- #
# END
