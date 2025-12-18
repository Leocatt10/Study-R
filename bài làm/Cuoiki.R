# nen em tự tạo 1 dataframe giống như đề bài  


# Số lượng bệnh nhân
n_patients <- 1338

height <- sample(140:190,size = n_patients,replace = TRUE)

weight <- sample(40:100,size = n_patients,replace = TRUE)

bmi <- round(weight / (height^2), 1)


age <- sample(18:64, size = n_patients, replace = TRUE)
sex <- sample(c("female", "male"), size = n_patients, replace = TRUE)
children <- sample(0:5, size = n_patients, replace = TRUE)
smoker <- sample(c("yes", "no"), size = n_patients, replace = TRUE, prob = c(0.2, 0.8))
region <- sample(c("northeast", "southeast", "southwest", "northwest"), 
                 size = n_patients, replace = TRUE)

# Cập nhật charges: đưa BMI vào công thức tính chi phí
# Chi phí cơ bản + ảnh hưởng của tuổi, BMI, hút thuốc, con cái, v.v.
base_charges <- 8000 + (age * 120) + (children * 400) + (bmi * 400)

smoker_factor <- ifelse(smoker == "yes", 25000, 0)

random_noise <- rnorm(n = n_patients, mean = 0, sd = 4000)
charges <- base_charges + smoker_factor + random_noise

charges <- round(pmax(0, charges), 2)


# --- 4. Tạo data.frame mới ---
insurance_data_new <- data.frame(
  age = age,
  sex = sex,
  bmi = bmi,       # Cột mới quan trọng
  children = children,
  smoker = smoker,
  region = region,
  charges = charges
)

insurance_data_new


#1 
data1 = read.csv("D:\ptdl\insurance.csv")
data = insurance_data_new
# biến định lượng là : age,bmi,children,charges
# biến định tính là : sex,smoker,region
#2
sum(is.na(data$charges))
trung_vi = median(data$charges,na.rm= TRUE)
data$charges[is.na(data$charges)]= trung_vi
#3
mean_N = mean(data$age[data$region = "northeast"])
mean_S = mean(data$age[data$region = "southeast"])
if(mean_N > mean_S){
  cat("gttb tuổi ở đông bắc cao hơn ở đông nam ")
}else{
  cat("gttb tuổi ở đông nam cao hơn ở đông bắc ")
}
#4
max = 0
# tìm giạ trị trung bình lớn nhất theo tưng khu vực 
for( i in unique(data$region)){
  mean = mean(data$charges[data$region== i])
  if(mean> max){
    max =mean
  }
}
# tìm tên khu vực có giá trị trung bình lớn nhất
for (i in unique(data$region)){
  mean = mean(data$charges[data$region== i])
  if(mean == max){
    print(i)
  }
}
#5 

#6
#7 
data$Phan_loai= c()
data$Phan_loai[data$bmi < 18.5] = "Gầy"
data$Phan_loai[data$bmi < 24.9 & data$bmi >= 18.5] = "Bình thường"
data$Phan_loai[data$bmi < 29.9 & data$bmi >= 25] = "Thừa cân"
data$Phan_loai[data$bmi >= 30] = "Béo phì"
data$Phan_loai
table(data$Phan_loai)
#8
remove_outliers  = function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_value <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  return( x >= lower & x <= upper)
}
data$charges = remove_outliers(data$charges)
#9
tab1 = table(data$sex,data$region)
barplot(tab1,
        col = ifelse(data$sex == "male" , "blue", "pink"),
        beside = TRUE)
legend("topleft",
       legend = c("male","female"),
       fill = c("blue","pink"))
#10
hist(data$charges,
     col="gray",
     freq= FALSE)
lines(density(data$charges),col = "red")
