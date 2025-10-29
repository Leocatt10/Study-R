#Bài 1 
#a.

tong <- function(data1, data2){
  if(leng(data1) != length(data2 || ! is.numeric(data1)) || ! is.numeric(data2)){
    return (FALSE)
  }else{
    return (sum(data1 * data2))
  }
}


tieu_thu <- c(1700,1800,1900,2000,2100,2200,2300)
so_ho <- c(3,4,5,7,8,5,3)

m <- data.frame(tieu_thu, so_ho)
m

n = sum(so_ho)
n

#b.

gttb = total(tieu_thu,so_ho) / n
gttb

phan_tu = tieu_thu - gttb

do_lech = sqrt((total(so_ho , phan_tu^2)/ (n - 1)))
do_lech

#c.

T = (gttb - 190) * sqrt(n) / do_lech
T
ifelse(T > 1.757, print("kết quả trên là sai"), print("kết quả trên là đúng"))

#d.

ham <- function(x, Mo){
  n = length(x)
  gttb = mean(x)
  do_lech = sd(x)
  T = ((gttb - Mo) * sqrt(n)) / do_lech
  ifelse(T > 1.757,print("kết quả trên là sai"),print("kết quả trên là đúng"))
}

#e.

vecto_moi = rnorm(150, mean= gttb, sd = do_lech)
vecto_moi

#f.

test(vecto_moi,190)

#Bài2
#a.

Chieu_dai = c(178, 179, 180, 181, 182)
So_ong = c (12, 18, 35, 20, 15)
n = sum(So_ong)
dulieu = t(data.frame(Chieu_dai, So_ong))
dulieu

#b.

gttb = total(Chieu_dai, So_ong) / n
gttb

do_lech = sqrt(total(Chieu_dai - gttb)^2 , So_ong) /(n - 1)
do_lech

#c.

ham <- function(x, Mo){
  n = length(x)
  gttb = mean(x)
  do_lech = sd(x)
  if (Mo <= ( gttb - 1.96*(do_lech / sqrt(n))) || Mo >= (gttb + 1.96*(do_lech / sqrt(n)))){
    print("Giả thuyết sai")
  } else{
    print("Giả thuyết đúng")
  }
}

#d.

test(n, gttb, do_lech, 185)

#e.

vecto_y <- rnorm(1500, mean = gttb , sd = do_lech)
vecto_y

#f.

test(1500 , gttb , do_lech , median(y) )

#Bài 3
#a.

#Bài4
#a.

red = read.csv("C:\Users\Admin\Desktop\R\data\winequality.red.5.csv")
colnames(red)
red

#b.

red$total.sulfur.dioxide[red$total.sulfur.dioxide== 29 & red$total.sulfur.dioxide == 102] = 92 

#c.

cong_them = function(x){
  for (i in 1:length(x)){
    if(x[i] <= 0.02){
      x[i] = 0.01 +x[i]
    }
  }
}
cong_them(red$citric.acid)

#d.

phan_loai = function(alcohol,quality,pH){
  phan_loai_Tot_xau = c()
  for (i in 1:length(pH)){
    k = alcohol[i] * quality[i] / pH[i]
    if(k >8.3){
      phan_loai_Tot_xau = c(phan_loai_Tot_xau,"Tốt")
    }
    else{
      phan_loai_Tot_xau = c(phan_loai_Tot_xau,"Xấu")
    }
    
  }
  phan_loai_Tot_xau
}
phan_loai = phan_loai(red$alcohol,red$quality,red$pH)
phan_loai
red$phan_loai = phan_loai
red
#e.

mean_tot_pH = mean(red$pH[red$phan_loai == "Tốt"])
mean_xau_pH = mean(red$pH[red$phan_loai == "Xấu"])
dlc_tot_pH = sd 
for (i in unique(red$phan_loai)){
  mean = mean(red$pH[red$phan_loai == i])
  sd = sd(red$pH[red$phan_loai == i])
  mean_density = mean(red$density[red$phan_loai == i])
  sd_density = sd(red$density[red$phan_loai == i])
  print(i)
  print(mean)
  print(sd)
  print(mean_density)
  print(sd_density)
}



#Bài 5
#a. 

data_csv <- read.csv("C:\Users\Admin\Downloads\Sampledatasafety.csv")
data <- data_csv
data <- na.omit(data)
colSums(is.na(data))

#b.

data$Incident.Type = as.character(data$Incident.Type)
burn = subset(data,Incident.Type == "Burn")
sum_cost = sum(burn$Incident.Cost)

#c.

data$Date = as.Date(data$Date, format = "%d-%b-%y")

data1 <- subset(data, format(Date, "%Y") == "2020")
data2 <- subset(data, format(Date, "%Y") == "2021")
  if(sum(data1$Days.Lost) > sum(data2$Days.Lost)){
    print("số ngày công mất đi năm 20 nhiều hơn năm 21")
  }else{
    print("số ngày công mất đi năm 21 nhiều hơn năm 20")
  }

#d







