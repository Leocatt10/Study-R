#Bài1
#a


data_csv <- read.csv("C:\Users\Admin\Downloads\dulieu2.csv")

air= data_csv
air
air1 = na.omit(air)
colSums(is.na(air))

#b

air1$Segment = as.character(air1$Segment)
data1 = subset(air1, Segment == "Government")
data2 = subset(air1, Segment == "Midmarket")

#c

max =0
name =""
for (i in unique(air1$Product)){
  sum_product = sum (air1$Gross.Sales[air1$Product == i])
  print(i)
  print(sum_product)
  if (max < sum_product){
    max = sum_product
    name = i
  }
}
max
name

#d

colnames(vtt_mexico)
air1$Product = as.character(air1$Product)
air1$Country = as.character(air1$Country)
vtt_mexico = subset(air1, Product == "VTT" & Country == "Mexico")
colnames(vtt_mexico)
vtt_date = unique(vtt_mexico[c("Month.Number","Year")])
print(vtt_date)
#e

product_counts <- as.data.frame(table(air1$Product)) # table() trả về 1 bảng có tần suất 
colnames(product_counts) <- c("Product", "Transaction_Count")
max = 0
for (i in 1:length(product_counts$Product)){
  if(product_counts$Transaction_Count[i] > max){
    max = product_counts$Transaction_Count[i]
  }
}
for (i in 1:length(product_counts$Product)){
  if(product_counts$Transaction_Count[i] == max){
    print(product_counts$Product[i]);
  }
}

#f

sum1 =sum(data1$Gross.Sales)
sum2 = sum(data2$Gross.Sales)
ifelse (sum1 >= sum2,print(" goverment> mid"),print(" mid > gogo"))

#Bài2
#a

data_csv1 = read.csv("C:\Users\Admin\Downloads\WHO1.csv")
who = data_csv1

#b 

colSums(is.na(who))
who1 = na.omit(who)

#c

vitri1 <- which(is.na(who$LiteracyRate..Tylebietdocviet.))
for(i in vitri1){
  who$LiteracyRate..Tylebietdocviet.[i] <- mean(who$LiteracyRate..Tylebietdocviet., na.rm = TRUE)
}

vitri2 <- which(is.na(who$GNI..Thunhapquocdan.))
for(i in vitri2){
  who$GNI..Thunhapquocdan.[i] <- mean(who$GNI..Thunhapquocdan., na.rm = TRUE)
}
colSums(is.na(who)) 

#d

mean1 = mean(who$GNI..Thunhapquocdan., na.rm = TRUE)
mean2 = mean(who1$GNI..Thunhapquocdan., na.rm = TRUE)
if (mean1 > mean2){
  cat("who > who1")
}else {
  cat("who < who1")
}

#e

data_Bahrain = subset(who,Country =="Bahrain")
data_Mexico = subset(who,Country =="Mexico")
if (data_Bahrain$LifeExpectancy..Tuoitho. > data_Mexico$LifeExpectancy..Tuoitho. ){
  print("tuoi tho braha > mexico")
}else {
  print("tuoi tho mexico > braha")
}

#f

Canada = subset(who,Country =="Canada")
print(Canada$FertilityRate..Tylesinhsan.)

#g

max1 =0;
for (i in 1: length(who$Country)){
  if (who$Population..Danso.[i] > max){
    max = who$Population..Danso.[i]
  }
}
for (i in 1: length(who$Country)){
  if (who$Population..Danso.[i] == max){
    print("Nuoc co dan so nhieu nhat laf" )
    print(who$Country[i])
  }
}

#bai 3
#a 

Id = 1:300
gioi_tinh = sample (c("Nam","Nu"), 300,replace = TRUE)
tuoi = sample (18:24,300, replace = TRUE)
chieu_cao = sample (150 : 200,300, replace = TRUE)
can_nang = sample(40:100, 300, replace = TRUE)
diem_thuong_xuyen = sample (1:10,300, replace = TRUE)
diem_giua_ki = sample (1:10,300, replace = TRUE)
diem_cuoi_ki = sample (1:10,300, replace = TRUE)
data_sv = data.frame(Id,gioi_tinh,tuoi, chieu_cao ,can_nang,diem_thuong_xuyen,diem_giua_ki,diem_cuoi_ki)
data_sv

#b

data_sv[data_sv$diem_cuoi_ki >= 9, ]

#c

data_sv$diem_thuong_xuyen[data_sv$Id==5] = 10

#d

Tong = diem_giua_ki*0.2 + diem_giua_ki * 0.2 + diem_cuoi_ki * 0.6
Tong
data_sv$tong_diem = Tong
data_sv 

#e

xep_loai = function(diem){
  if (diem >= 9.0) {
    xep_loai <- "A+"
  } else if (diem >= 8.0) {
    xep_loai <- "A"
  } else if (diem >= 7.0) {
    xep_loai <- "B"
  } else {
    xep_loai <- "C"
  }
  print(xep_loai)
}
xep_loai(9)
Diem_chu = c()
for (i in 1:300){
  diem = data_sv$tong_diem[i]
  if (diem >= 9.0) {
    Diem_chu = c(Diem_chu,"A+")
  } else if (diem >= 8.0) {
    Diem_chu = c(Diem_chu,"A")
  } else if (diem >= 7.0) {
    Diem_chu = c(Diem_chu,"B")
  } else {
    Diem_chu = c(Diem_chu,"C")
  }
}

data_sv$Diem_chu = Diem_chu
data_sv$Diem_chu[data_sv$Id==10]

#f

data_sv$Diem_chu[data_sv$diem_cuoi_ki >= 9.0]

#g

count = nrow(data_sv[data_sv$diem_giua_ki == 0 & data_sv$tong_diem >= 5.0])
count

#h

ti_le_duoc_B = nrow(data_sv[data_sv$Diem_chu == "B", ] ) / 300 *100
ti_le_duoc_B

#i

for (i in 1 : 300){
  
  data_sv$diem_giua_ki[i] = data_sv$diem_giua_ki[i] + 2
  if(data_sv$diem_giua_ki[i] > 10){
    data_sv$diem_giua_ki[i] = 10
  }
  data_sv$tong_diem[i] = data_sv$diem_thuong_xuyen[i] * 0.2 + data_sv$diem_giua_ki[i]*0.2+ data_sv$diem_cuoi_ki[i] *0.6
  diem = data_sv$tong_diem[i]
  if (diem >= 9.0) {
    data_sv$Diem_chu[i] = "A+"
  } else if (diem >= 8.0) {
    data_sv$Diem_chu[i] = "A"
  } else if (diem >= 7.0) {
    data_sv$Diem_chu[i] = "B"
  } else {
    data_sv$Diem_chu[i] = "C"
  }
  
}

ti_le_duoc_B1 = nrow(data_sv[data_sv$Diem_chu == "B", ]) / 300 *100
ti_le_duoc_B1

#Bai 4
#a

n = 100
crim = rnorm(100, mean = 3.613, sd =8.6 )
crim[crim < 0] =0
indus = rnorm(100, mean= 11.14, sd = 6.86)
indus[indus < 0]=0
rm = runif(100,min = 2, max = 6)
tax = rnorm(100 , mean = 408 , sd = 167 )
tax[tax< 0] =0
nox =c()
for (i in 1:100){
  noxx = rnorm(1, mean = 0.55,sd = 0.116)
  if (noxx < 0.38 || noxx > 0.87){
    noxx = rnorm(1,mean =0.55, sd = 0.116)
  }
  nox[i] = noxx
  
}
chas = rbinom(100, size = 1,prob = 0.2)
age = c()
for (i in 1:100){
  agee = rnorm(1,mean = 68 ,sd = 28)
  if ( agee < 0|| agee > 100){
    agee = rnorm(1,mean = 68, sd = 28)
  }
  age[i] = agee
  
}
age
medv = rnorm(100, mean = 22.53, sd =9.2)
medv[medv < 0]=0
hanoi = data.frame(crim,indus,rm,tax,nox,chas,age,medv)

#b

hanoi$medv1 = -0.138108 * hanoi$crim - 0.068990 * hanoi$indus - 4.716034 * hanoi$nox + 7.677527 * hanoi$rm - 0.007923 * hanoi$tax - 18.599942
hanoi$medv1
do_lech_chuan = hanoi$medv1 - hanoi$medv
do_lech_chuan
sd1 = sd(do_lech_chuan)
sd1

#c

gan_song= mean(hanoi$medv[hanoi$chas == 1])
khong_gan_song = mean(hanoi$medv[hanoi$chas == 0])
ifelse (gan_song > khong_gan_song,print("gttb nha gan song lon hon"),print("gttb nha ko gan song lon hon"))

#bai5
#a

cov = read.csv("C:\Users\Admin\Downloads\COVID19.csv")
co19 =cov
colnames(co19)
tuoi_tb = function(x){
  me = mean(co19$Age[co19$Sex == x])
  print(me)
}
tuoi_tb("Male")
tuoi_tb("Female")

#b

ti_le_khoi_benh = function(x){
  ti_le =nrow(co19[co19$Status == "Recovered" & co19$Location == x,  ]) / nrow(co19[co19$Location== x, ] )
  print(ti_le)
}
ifelse(ti_le_khoi_benh("Ha Noi") > ti_le_khoi_benh("Ho Chi Minh"),print("Hanoi dc chua khoi nhieu hon"),print("'Hcm dc chua khoi nhieu hon"))

#c

nrow(co19[co19$Nationality != "Vietnamese" & co19$Status == "Recovered", ])

#d

ti_le_nhom = function(x,y){
  ti_le_benh = nrow(co19[co19$Age >= x & co19$Age < y, ])
  so_khoi_benh = nrow(co19[co19$Age >= x & co19$Age < y & co19$Status == "Recovered",  ])
  dang_dieu_tri = nrow(co19[co19$Age >= x & co19$Age < y & co19$Status == "Being treated",  ])
  da_mat = nrow(co19[co19$Age >= x & co19$Age < y & co19$Status == "Death",  ])
  ti_le_khoi_benh1 = so_khoi_benh / ti_le_benh
  ti_le_dieu_tri = dang_dieu_tri/ ti_le_benh
  ti_le_mat = da_mat / ti_le_benh
  return(ti_le_khoi_benh1)
  
}
tl_0_18 = ti_le_nhom(0, 18)
tl_18_40 = ti_le_nhom(18, 40)
tl_40_60 = ti_le_nhom(40, 60)
tl_60_100 = ti_le_nhom(60, 100)

max1 = max(tl_0_18, tl_18_40, tl_40_60, tl_60_100)
if (max1 == tl_0_18) {
  print("0 den 18")
} else if (max1 == tl_18_40) {
  print("18 den 40")
} else if (max1 == tl_40_60) {
  print("41 den 60")
} else if (max1 == tl_60_100) {
  print(" tren 60")
}

#bai6
#a

data = read.csv("‪C:\Users\Admin\Downloads\Product.csv")
colSums(is.na(data))
data = na.omit(data)
colnames(data)
data

#b

nrow(data[data$Product== "Chocolate Chip"], )

#c

data16 = subset(data, Category == "Bars")
data26= subset(data,Category == "Crackers")
gttb_bars = mean(data16$TotalPrice)
gttb_Cra = mean(data26$TotalPrice)
if (gttb_bars > gttb_Cra){
  print("Bars lon hon Cra")
}else if(gttb_bars == gttb_Cra){
  print("Bars bang Cra")
}else{
  print("bars nho hon Cra")
}

#d

tong_san_pham = function(x){
  sum(data$Quantity[data$Product == x])
}
tong_san_pham("Carrot")

#e

thanh_pho = function(x){
  max = 0
  name =""
  for (j in unique(data$City)){
    sum_1 = sum(data$Quantity[data$City == j])
    if(max < sum_1){
      name = j
      max = sum_1
    }
    
  }
  return(name)
}
thanh_pho("Cookies")

#f

name1 =""
for (i in 1:length(data$Region)){
  if(data$TotalPrice[i] != data$Quantity[i] *data$UnitPrice[i]){
    data$TotalPrice[i] == data$Quantity[i] *data$UnitPrice[i]
    name1 = data$Product[i]
  }
}
name1


