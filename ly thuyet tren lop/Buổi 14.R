#BÀI TẬP VỀ NHÀ CHƯƠNG 2
#Bài 1
#1.1
library("Stat2Data")

data("BirdNest")

nest <- na.omit(BirdNest)

View(nest)

#1.2 Liệt kê từng loại tổ

loaito <- unique(nest$Nesttype)

loaito

#Tạo bảng tần số

Table <- table(nest$Nesttype)

Table

#1.3. Hãy vẽ biểu đồ quạt thể hiện tỉ lệ các loài chim theo từng loại tổ

pie(Table,
    main = "Biểu đồ quạt tỉ lệ các loài chim theo từng loại tổ")

#1.4. Tạo bảng tần số về số lượng tổ chim theo các vị trí tổ (Location).

Table1 <- table(nest$Location)

Table1

#Hãy vẽ biểu đồ cột thể hiện số lượng tổ chim theo các vị trí tổ

barplot(Table1,
        xlab = "Vị trí tổ",
        ylab = "Số lượng",
        main = "số lượng tổ chim theo các vị trí tổ")

#1.5. Vẽ biểu đồ hộp của số lượng trứng trong tổ chim giữa các loại tổ
#(Nesttype). Tô các màu khác nhau cho mỗi nhóm.

boxplot(nest$No.eggs ~ nest$Nesttype,
        main = "biểu đồ hộp của số lượng trứng theo loại tổ",
        xlab = "loại tổ",
        ylab = "số lượng trứng",
        col = rainbow(length(loaito)))

#1.6. vẽ tổ chức đồ thể hiện sự phân bố số lượng trứng trong ổ

hist(nest$No.eggs,
     col = c("green"),
     freq = FALSE,
     main = "PHÂN BỐ SỐ LƯỢNG TRỨNG TRONG Ổ",
     xlab = "số lượng trứng",
     ylab = "Mật độ")


#2.
#Bảng tần số số lượng chim đực, chim cái
table(bird$KnownSex)
#Biểu đồ quạt tỉ lệ chim đực, chim cái
pie(table(bird$KnownSex),
    col = c("red", "green"),
    main = "Tỉ lệ giữa chim đực và chim cái")

#3.
#Tạo cột mới
bird$SortMass <- cut(bird$Mass,
                     breaks = c(-Inf, 60, 70, 80, Inf),
                     labels = c("Dưới 60g", "60-70g", "70-80g", "Trên 80g"))
table(bird$SortMass)
#Biểu đồ cột số các loài chim theo klg trên
barplot(table(bird$SortMass),
        col = c("red", "blue", "green", "yellow"),
        xlab = "Khoảng khối lượng",
        ylab = "Số loài chim",
        main = "Số các loài chim theo khối lượng")

#4.
#Tổ chức đồ phân bố của số lượng chim trong mỗi khoảng độ dài mỏ
hist(bird$BillLength, col = "green", freq = FALSE,
     main = "Phân bố của số lượng chim trong mỗi khoảng độ dài mỏ",
     xlab = "Độ dài mỏ",
     ylab = "Mật độ")
lines(density(bird$BillLength), col = "red")

#5.
#Biểu đồ tán xạ về tương quan giữa chiều rộng mỏ và chiều dài mỏ
plot(bird$BillWidth, bird$BillLength,
     col = ifelse(bird$KnownSex == "F", "red", "blue"),
     xlab = "Chiều rộng mỏ",
     ylab = "Chiều dài mỏ",
     main = "Tương quan giữa chiều rộng mỏ và chiều dài mỏ")

# Bài 3
#1.
data("ChickWeight")
chicken = ChickWeight
chicken
colSums(is.na(chicken)) #kiểm tra dữ liệu thiếu
#2.
diet2 = subset(chicken,Diet == 2)
diet2

time <- unique(diet2$Time)
mean_by_time = c()
for(i in time){
  mean_by_time <- c(mean_by_time, mean(diet2[diet2$Time == i,]$weight))
}
mean_by_time
names(mean_by_time) <- time
mean_by_time

barplot(mean_by_time,
        xlab = "Thời gian",
        ylab = "Cân nặng trung bình",
        main = "Trọng lượng trung bình của các con gà theo từng thời điểm")
#3.
ga <- subset(chicken, Chick == 20 | Chick == 22 | Chick == 23)
ga

plot(weight ~ Time, data = subset(ga, Chick == 20),
     type = "l", col = "blue", lwd = 2,
     main = "Sự thay đổi trọng lượng của những con gà ID 20,22,23 theo thời gian",
     xlab = "Thời gian", ylab = "Cân nặng ")

lines(weight ~ Time, data = subset(ga, Chick == 22),
      col = "red", lwd = 2)

lines(weight ~ Time, data = subset(ga, Chick == 23),
      col = "green", lwd = 2)

legend("topleft",
       legend = c("Chick 20", "Chick 22", "Chick 23"),
       col = c("blue", "red", "green"),
       lwd = 2)
#4.
diet3 = subset(chicken,Diet == 3)
plot(weight ~ Time,
     data = diet3,
     pch = 16,
     main = "Quan hệ giữa thời gian và khối lượng của gà trong chế độ ăn 3",
     xlab = "Thời gian(Ngày)",
     ylab = "Cân nặng")
#5.
day0 = subset(chicken,Time == 0)
hist(day0$weight,
     freq = FALSE,          
     col = "lightblue",
     main = "Phân bố của trọng lượng con gà khi mới sinh",
     xlab = "Cân nặng",
     ylab = "Mật độ")

lines(density(day0$weight), lwd = 2)

#Câu 4.
#a.
flower <- iris
sum(is.na(flower))
#b.
Q <- quantile(flower$Petal.Width, probs=c(.25,.75))
Q
IQR <- Q[2] - Q[1]; IQR

lower <- Q[1] - 1.5*IQR; lower
upper <- Q[2] + 1.5*IQR; upper

flower <- subset(flower, Petal.Width >= lower & Petal.Width <= upper)
#c.
species <- unique(flower$Species)
species

mean_len = c()
for(i in species){
  mean_len <- c(mean_len, mean(flower[flower$Species == i,]$Sepal.Length) )
}
mean_len
names(mean_len) <- species
mean_len

barplot(mean_len,
        xlab = "Loài hoa",
        ylab = "Chiều dài",
        main="Sepal.Length theo loài",
        col=rainbow(3))
#d.
setosa <- subset(flower, Species == "setosa")

hist(setosa$Sepal.Length, freq=FALSE,
     main="Phân bố Sepal.Length – Setosa")

lines(density(setosa$Sepal.Length), lwd=2)
#e.
colors <- c("red","blue","green")[flower$Species]

plot(flower$Sepal.Length, flower$Petal.Length,
     col = colors, pch=19,
     xlab = "Sepal Length", ylab = "Petal Length",
     main="Scatter by Species")

legend("topleft", legend=levels(flower$Species),
       col=c("red","blue","green"), pch=19)


# câu 5

# 1. Nhập dữ liệu & bảng tần số Card + biểu đồ tròn
install.packages("Stat2Data")
library(Stat2Data)
data("Clothing")

clothing_data <- Clothing

table(clothing_data$Card)

pie(table(clothing_data$Card),
    labels = c("Không có thẻ", "Có thẻ"),
    main = "Tỷ lệ khách hàng có và không có thẻ tín dụng")

# 2. Loại bỏ outlier của Amount, Dollar12, Dollar24 (IQR)
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(x >= lower & x <= upper)
}

clean_data <- clothing_data[
  remove_outliers(clothing_data$Amount) &
    remove_outliers(clothing_data$Dollar12) &
    remove_outliers(clothing_data$Dollar24),
]
clean_data

# 3. Scatter plot Dollar24 vs Freq24
plot(clean_data$Freq24, clean_data$Dollar24,
     main = "Mối quan hệ Dollar24 và Freq24",
     xlab = "Số lần mua sắm 24 tháng",
     ylab = "Tổng chi tiêu 24 tháng",
     pch = 19, col = "blue")

# 4. Biểu đồ cột ghép Freq12 và Freq24 của 5 khách đầu
freq_matrix <- rbind(clean_data$Freq12[1:5],
                     clean_data$Freq24[1:5])

colnames(freq_matrix) <- paste("KH", 1:5)
rownames(freq_matrix) <- c("Freq12", "Freq24")
freq_matrix

barplot(freq_matrix,
        beside = TRUE,
        main = "Freq12 và Freq24 của 5 khách hàng đầu",
        col = c("skyblue", "orange"),
        legend = TRUE)

# 5. Histogram Dollar12 + đường mật độ
hist(clean_data$Dollar12,
     main = "Phân bố Dollar12 (đã loại outlier)",
     xlab = "Dollar12",
     freq = FALSE,
     col = "lightgray")

lines(density(clean_data$Dollar12),
      col = "red", lwd = 2)

# 6. Biểu đồ quạt theo mức độ chi tiêu Dollar12 của khách không có thẻ

no_card <- clean_data[clean_data$Card == 0, ]

muc_chi_tieu <- cut(no_card$Dollar12,
                    breaks = c(-Inf, 150, 300, 450, Inf),
                    labels = c("Dưới 150", "150–300", "300–450", "Trên 450"))

muc_table <- table(muc_chi_tieu)
muc_table

pie(muc_table,
    main = "Mức chi tiêu Dollar12 của khách không có thẻ",
    col = c("pink", "lightblue", "lightgreen", "orange"))


#Cau6

#1
library(Stat2Data)
data("Diamonds")

diamonds_data <- na.omit(Diamonds)
#2
remove_outliers  = function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  x >= lower & x <= upper
}

depth_ok <- remove_outliers(diamonds_data$Depth)
priceperct_ok <- remove_outliers(diamonds_data$PricePerCt)
total_ok <- remove_outliers(diamonds_data$TotalPrice)

diamonds_clean <- diamonds_data[depth_ok & priceperct_ok & total_ok, ]

#3
plot(diamonds_clean$Carat, diamonds_clean$TotalPrice,
     xlab = "Trọng lượng",
     ylab = "Giá trị",
     main = "Mối quan hệ giữa trọng lượng và tổng giá trị của kim cương")
#4

color = table(diamonds_data$Color)
color
pie(table(diamonds_data$Color),
    main = "Tỉ lệ kim cương theo màu sắc")
#5
colorI_data <- diamonds_data[diamonds_data$Color == "I", ]

hist(colorI_data$PricePerCt,
     col = "green",
     freq = FALSE)

lines(density(colorI_data$PricePerCt), col = "red", lwd = 2)

#6

mucdo = unique(diamonds_data$Clarity)
mucdo

giatritrungbinh <- numeric(length(mucdo))
names(giatritrungbinh) <- mucdo    
for(i in mucdo){
  giatritrungbinh[i] = mean(diamonds_data$TotalPrice[diamonds_data$Clarity == i])
}
giatritrungbinh

barplot(giatritrungbinh,
        xlab = "Mức độ trong suốt",
        ylab = "Giá trị",
        main = "Giá trị trung bình theo từng mức độ trong suốt")


#Câu 7:
#1,
data("mtcars")

LuongXe = table(mtcars$cyl)
LuongXe 
barplot(LuongXe, col = "yellow",
        xlab = "Số xi lanh",
        ylab = "Số lượng xe", 
        main = "Số lượng xe theo số xi lanh")

#2,
gears = unique(mtcars$gear)
gears

gearTrungbinh = numeric(length(gears))        

for(i in seq_along(gears)) {
  gearTrungbinh[i] = mean(mtcars$mpg[mtcars$gear == gears[i]])
}
gearTrungbinh
names(gearTrungbinh) <- gears
gearTrungbinh

barplot(gearTrungbinh,
        xlab = "Số lượng bánh răng",
        ylab =" Mức tiêu thụ nhiên liệu",
        main = "Mức tiêu thụ nhiên liệu TB theo từng loại bánh răng")

#3,
hist(mtcars$hp,col="lightblue",freq = F,
     xlab = "Công suất",
     ylab = "Mật độ",
     main = "Phân bố của công suất")
lines(density(mtcars$hp),col = "red")

#4,
par(mfrow = c(1,2))
cols = c("4" = "blue", "6" = "green", "8" = "red")
plot(mtcars$hp, mtcars$mpg,
     col = cols[as.character(mtcars$cyl)],
     xlab = "Công suất",
     ylab = "Mức tiêu thụ nhiên liệu", 
     main = "Mức tiêu thụ theo công suất")
plot(mtcars$hp, mtcars$wt,
     col = cols[as.character(mtcars$cyl)],
     xlab = "Công suất",
     ylab = "Trọng lượng", 
     main = "Trọng lượng theo công suất")


#5,
par(mfrow = c(1,1))
boxplot(mpg ~ cyl,data = mtcars,
        col = c("lightblue", "lightgreen", "orange"),
        xlab = "Số xi lanh",
        ylab = "Mức tiêu thụ nhiên liệu",
        main = "Biểu đồ hộp")

# --- CÂU 8: airquality ---

# 1. Nhập dữ liệu và thay thế giá trị trống bằng giá trị trung bình
data(airquality)
air <- airquality
mean_ozone <- mean(air$Ozone, na.rm = TRUE)
mean_solar <- mean(air$Solar.R, na.rm = TRUE)
air$Ozone[is.na(air$Ozone)] <- mean_ozone
air$Solar.R[is.na(air$Solar.R)] <- mean_solar
air$Month <- factor(air$Month) # Chuyển Month thành factor

cat("\n>>> CÂU 8.1: Số lượng NA sau khi thay thế:", sum(is.na(air)), "\n")

# 2. Loại bỏ giá trị bất thường (outliers) của Ozone, Solar.R, Wind, Temp
remove_outliers_c8 <- function(data, col_name) {
  Q1 <- quantile(data[[col_name]], 0.25)
  Q3 <- quantile(data[[col_name]], 0.75)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  data <- subset(data, data[[col_name]] >= lower_bound & data[[col_name]] <= upper_bound)
  return(data)
}
air_cleaned <- air
air_cleaned <- remove_outliers_c8(air_cleaned, "Ozone")
air_cleaned <- remove_outliers_c8(air_cleaned, "Solar.R")
air_cleaned <- remove_outliers_c8(air_cleaned, "Wind")
air_cleaned <- remove_outliers_c8(air_cleaned, "Temp")


# 3. Tính trung bình của Temp và Solar.R theo Month và vẽ biểu đồ cột kép
month <- unique(air_cleaned$Month); month

temp_avg <- numeric(length(month))
solar_avg <- numeric(length(month))
for(i in seq_along(month)){
  temp_avg[i] <- mean(air_cleaned[air_cleaned$Month==month[i],]$Temp)
  solar_avg[i] <- mean(air_cleaned[air_cleaned$Month==month[i],]$Solar.R)
}
avg_by_month <- cbind(temp_avg, solar_avg)
row.names(avg_by_month ) <- month
avg_by_month 

cat(">>> CÂU 8.3: Vẽ Biểu Đồ Cột Kép Temp và Solar.R Trung Bình Theo Tháng\n")
barplot(t(avg_by_month) ,
        beside = TRUE,
        main = "Trung Bình Nhiệt Độ và Năng Lượng Mặt Trời Theo Tháng",
        xlab = "Tháng",
        ylab = "Giá Trị Trung Bình",
        col = c("salmon", "orange"),
        legend.text = c("Temp", "Solar.R"),
        args.legend = list(x = "topleft")
)

# 4. Vẽ biểu đồ hộp (boxplot) để so sánh nồng độ ozone (Ozone) giữa các tháng (Month)
cat(">>> CÂU 8.4: Biểu Đồ Hộp So Sánh Nồng Độ Ozone Giữa Các Tháng\n")
boxplot(Ozone ~ Month, data = air_cleaned,
        main = "Phân Bố Nồng Độ Ozone Theo Tháng",
        xlab = "Tháng",
        ylab = "Nồng Độ Ozone (ppb)",
        col = terrain.colors(5)
)

# 5. Vẽ biểu đồ đường để so sánh dự biến thiên nhiệt độ (Temp) theo ngày (Day) của tháng 5 và 6
air_may_jun <- subset(air_cleaned, Month %in% c(5, 6))
air_may <- subset(air_may_jun, Month == 5)
air_jun <- subset(air_may_jun, Month == 6)
cat(">>> CÂU 8.5: Biểu Đồ Đường So Sánh Biến Thiên Nhiệt Độ Tháng 5 và 6 Theo Ngày\n")
plot(air_may$Day, air_may$Temp,
     type = "l",
     main = "Biến Thiên Nhiệt Độ (Temp) Theo Ngày (Tháng 5 và 6)",
     xlab = "Ngày",
     ylab = "Nhiệt Độ (°F)",
     ylim = range(air_may_jun$Temp),
     col = "blue",
     lwd = 2
)
lines(air_jun$Day, air_jun$Temp, col = "red", lwd = 2)
legend("topleft", 
       legend = c("Tháng 5", "Tháng 6"), 
       col = c("blue", "red"), 
       lwd = 2)

# 6. Vẽ biểu đồ histogram của Temp và thêm đường mật độ
cat(">>> CÂU 8.6: Tổ Chức Đồ Phân Bố Nhiệt Độ (Temp) và Đường Mật Độ\n")
hist(air_cleaned$Temp,
     main = "Phân Bố Nhiệt Độ (°F)",
     xlab = "Nhiệt Độ (Temp, °F)",
     ylab = "Mật Độ",
     freq = FALSE, 
     col = "darkgoldenrod1",
     border = "black",
     breaks = 10
)
lines(density(air_cleaned$Temp), col = "red", lwd = 2)

# 7. Vẽ biểu đồ tán xạ giữa Wind và Temp, phân nhóm theo Month
cat(">>> CÂU 8.7: Biểu Đồ Tán Xạ giữa Tốc Độ Gió và Nhiệt Độ theo Tháng\n")
plot(air_cleaned$Wind, air_cleaned$Temp,
     main = "Tương Quan giữa Tốc Độ Gió và Nhiệt Độ (Theo Tháng)",
     xlab = "Tốc Độ Gió (Wind, mph)",
     ylab = "Nhiệt Độ (Temp, °F)",
     col = factor(air_cleaned$Month),
     pch = 19
)
legend("topright", 
       legend = levels(factor(air_cleaned$Month)), 
       col = 1:length(levels(factor(air_cleaned$Month))), 
       pch = 19, 
       title = "Tháng")
