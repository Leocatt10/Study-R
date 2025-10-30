#Cau 1
#a.Nhap du lieu 35 ho dan vao R
x_value <- c(1700, 1800, 1900, 2000, 2100, 2200, 2300)
freq <- c(3, 4, 5, 7, 8, 5, 3)
x <- rep(x_value,freq)
x
#b.Tinh gia tri trung binh, do lech tieu chuan
n <- length(x)
x_mean <- sum(x_value*freq)/n; x_mean
x_sd <- sqrt(sum(freq*(x_value-x_mean)^2)/(n-1)); x_sd
#c.Kiem tra y kien
u0 <- 1900
T <- (x_mean - u0)*sqrt(length(x))/x_sd
if(T > 1.757){
  print("Y kien so dien trung binh = 1900 la sai")
} else {
  print("Y kien so dien trung binh = 1900 la dung")
}
#d.Viet ham dau vao
kiemdinh_u <- function(x, u0){
  n <- length(x)
  x_bar <- mean(x)
  s <- sd(x)
  T <- (x_bar - u0)*sqrt(n)/s
  if(T > 1.757){
    cat("Y kien so dien trung binh = ",u0," la sai")
  } else{
    cat("Y kien so dien trung binh = ",u0," la dung")
  }
}
kiemdinh_u(x, 1900)
#e.Sinh vector
set.seed(123)
x_new <- rnorm(150,mean = x_mean,sd = x_sd)
mean(x_new)
sd(x_new)
#f.Kiem tra ham
kiemdinh_u(x_new,1900)

#Câu 2:
#a.
chieu_dai <- c(178, 179, 180, 181, 182)
so_ong <- c(12, 18, 35, 20, 15)
dulieu <- rep(chieu_dai, times = so_ong)
dulieu
#b.
n <- length(dulieu)
x2 <- sum(chieu_dai*so_ong)/n; x2
s2 <- sqrt(sum(so_ong*(chieu_dai-x2)^2)/(n-1)); s2
#c.
kiem_tra <- function(x, u0) {
  n_ham <- length(x)
  x_ham <- mean(x)
  s_ham <- sd(x)
  lower_lim <- x_ham - 1.96*s_ham/sqrt(n_ham)
  upper_lim <- x_ham + 1.96*s_ham/sqrt(n_ham)
  if(u0 > lower_lim & u0 < upper_lim) {
    print("Có thể đại diện cho chiều dài của các ống tuýp")
  } else {
    print("Không thể đại diện cho chiều dài của các ống tuýp")
  }
}
#d.
kiem_tra(dulieu, 185)

#e.
vecto2 <- rnorm(n = 1500, mean = x2, sd = s2)
vecto2
#f.
u0_2 <- median(vecto2)
kiem_tra(vecto2, u0_2)

#Câu 3:
#a
thoigian = c(40, 42, 44, 46, 48, 50)
sonhanvien = c(25, 50, 60, 110, 90, 15)
data = rep(thoigian, sonhanvien)
data

#b
n <- length(data)
x_mean <- sum(thoigian*sonhanvien)/n; x_mean
x_sd <- sqrt(sum(sonhanvien*(thoigian-x_mean)^2)/(n-1)); x_sd

#c
kiem_tra = function(x, p0) {
  n = length(x)                     
  m = sum(data > 46)                  
  f = m / n                           
  
  lower = f - 1.96 * sqrt(f * (1 - f) / n)
  upper = f + 1.96 * sqrt(f * (1 - f) / n)
  
  # Kiểm tra xem p0 có nằm trong khoảng hay không
  if(p0 >= lower & p0 <= upper){
    print("p0 có thể đại diện cho tỉ lệ nhân viên đi làm > 46 phút")
  } else {
    "p0 KHÔNG thể đại diện cho tỉ lệ nhân viên đi làm > 46 phút"
  }
}
#d
kiem_tra(data, 0.28)
kiem_tra(data, 0.33)
#Bài 4
## (a)
red <- read.csv("C:\\Users\\DOTHITHANHTAM\\Desktop\\winequality.red.5.csv", header = TRUE)
dim(red) # trả về số hàng và số cột

## (b)
for (i in 1:nrow(red)) {
  if (red$total.sulfur.dioxide[i] == 29 || red$total.sulfur.dioxide[i] == 102) 
    red$total.sulfur.dioxide[i] = 92
}
red$total.sulfur.dioxide
## (c)
for (i in 1:nrow(red)) {
  if (red$citric.acid[i] <= 0.02) {
    red$citric.acid[i] = red$citric.acid[i] + 0.01
  }
}
## (d)
classify_quality <- function(data, const = 8.3) {
  k = (data$alcohol * data$quality) / data$pH
  
  result = ifelse(k > const, "Tốt", "Xấu")
  
  return(result)
}
classify_quality(red)

red$quality_label <- classify_quality(red)
red$quality_label

## (e)
alcohol_good <- subset(red, quality_label == "Tốt")
alcohol_bad <- subset(red, quality_label == "Xấu")

mean_pH_good = mean(alcohol_good$pH) 
mean_density_good = mean(alcohol_good$density) 

mean_pH_bad = mean(alcohol_bad$pH) 
mean_density_bad = mean(alcohol_bad$density) 

sd_pH_good = sd(alcohol_good$pH)
sd_density_good = sd(alcohol_good$density)

sd_pH_bad = sd(alcohol_bad$pH)
sd_density_bad = sd(alcohol_bad$density)

print(mean_pH_good)
print(mean_pH_bad)

print(mean_density_good)
print(mean_density_bad)

print(sd_pH_good)
print(sd_pH_bad)

print(sd_density_good)
print(sd_density_bad)

#Câu 5

#a
setwd("C:\\Users\\DOTHITHANHTAM\\Desktop")
data <- read.csv("Sampledatasafety.csv", header=TRUE)
data = na.omit(data)
data
#b Trích ra một bộ dữ liệu con về nguyên nhân chấn thương do bỏng (Burn) và
#tính tổng chi phí điều trị (Incident Cost).
burn_data = subset(data,Incident.Type == "Burn")
burn_data

burn_data$Incident.Cost = as.numeric(gsub("[$,]","",burn_data$Incident.Cost))
burn_data$Incident.Cost

tongchiphi_dieutri_burn = sum(burn_data$Incident.Cost)
tongchiphi_dieutri_burn

#c
data1 = subset(data,Year == "2020")
data2 = subset(data,Year == "2021")
data1
data2

day_lost1 <- sum(data1$Days.Lost)
day_lost2 <- sum(data2$Days.Lost)
if(day_lost1 > day_lost2){
  print("Tổng số ngày công mất đi do tai nạn của 2020 lớn hơn 2021")
} else {
  print("Tổng số ngày công mất đi do tai nạn của 2020 nhỏ hơn hoặc bằng 2021")
}
#d
soloaichanthuong_2020 = unique(data1$Incident.Type); soloaichanthuong_2020
nguyennhanchanthuongchuyeu_2020 = c()
for (i in soloaichanthuong_2020) {
  solan =0
  for( j in 1:nrow(data1)){
    if(data1$Incident.Type[j] == i ){
      solan = solan+1
    }
  }
  nguyennhanchanthuongchuyeu_2020[i] = solan
}
nguyennhanchanthuongchuyeu_2020

names(nguyennhanchanthuongchuyeu_2020)[which.max(nguyennhanchanthuongchuyeu_2020)]

soloaichanthuong_2021 = unique(data2$Incident.Type)
nguyennhanchanthuongchuyeu_2021 = c()
for (i in soloaichanthuong_2021) {
  solan =0
  for( j in 1:nrow(data2)){
    if(data2$Incident.Type[j] == i ){
      solan = solan+1
    }
  }
  nguyennhanchanthuongchuyeu_2021[i] = solan
}
nguyennhanchanthuongchuyeu_2021
names(nguyennhanchanthuongchuyeu_2021)[which.max(nguyennhanchanthuongchuyeu_2021)]
#f
tong_chiphi = function(data){
  data$Incident.Cost = as.numeric(gsub("[$,]","",data$Incident.Cost))
  
  kq = numeric(length(unique(data$Incident.Type)))
  names(kq) = unique(data$Incident.Type)
  
  for (i in unique(data$Incident.Type)) {
    kq[i] = sum(data$Incident.Cost[data$Incident.Type==i])
  }
  
  return(kq)
}
chiphi_theonguyennhan = tong_chiphi(data)
chiphi_theonguyennhan

#
tong_chiphi2 = function(data){
  data$Incident.Cost = as.numeric(gsub("[$,]","",data$Incident.Cost))
  
  kq = numeric(length(unique(data$Department)))
  names(kq) = unique(data$Department)
  
  for (i in unique(data$Department)) {
    kq[i] = sum(data$Incident.Cost[data$Department==i])
  }
  
  return(kq)
}

chiphi_theoboptheobophan= tong_chiphi2(data)
chiphi_theoboptheobophan

max_ten = names(chiphi_theoboptheobophan)[which.max(chiphi_theoboptheobophan)]
if (max_ten == "Shipping"){
  print("Shipping co tong chi phi dieu tri cao nhat")
} else{
  cat ("Bo phan co chi phi cao nhat la ", max_ten,"\n")
}


