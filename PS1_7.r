
##7.1 [5 points] Load the csv, XLS, or XLSX file, and clean possible data points with missing values or bad quality.
## ��ȡ�й�������
## ȫ���ؼ�����201512ʵʱ��������(Ԥ������).csv
## �����ݼ�����ȫ���ؼ�������2015��12��ÿ�������ʵʱ�������

## ��ȡ����
weather <- read.csv("�Ϻ���������.csv",header = T)
chinaweather <- data.frame(weather)
## �鿴���ǵ�����
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
