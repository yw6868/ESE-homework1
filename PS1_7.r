
##7.1 [5 points] Load the csv, XLS, or XLSX file, and clean possible data points with missing values or bad quality.
## 读取中国的数据
## 全国县级城市201512实时天气数据(预处理后).csv
## 该数据集包括全国县级城市在2015年12月每个监测点的实时监测数据

## 读取数据
weather <- read.csv("上海天气数据.csv",header = T)
chinaweather <- data.frame(weather)
## 查看我们的数据
summary(chinaweather)

weather_rm_missing <- weather[which(!is.na(weather$temperature)),] #rm na
weather_rm_badPoint <- weather[which(weather_rm_missing<=-30),] #rm low quality

##7.2 [5 points] Plot the time series of a certain variable.

plot(weather$temperature, lwd=0.5,
     xlab = "month 12 of 2015",ylab="Temperature",type="l")

##7.3 [5 points] Conduct at least 5 simple statistical 
##checks with the variable, and report your findings.
min(weather$temperature,na.rm = T) 
max(weather$temperature,na.rm = T) 
table(weather$temperature)
mean(weather$temperature,na.rm = T) 
median(weather$temperature,na.rm = T)

