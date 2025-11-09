# Bai tap 1
#1 Hãy đọc dữ liệu vào trong R và đặt tên là data.
#Cho biết số hàng và số cột của bộ dữ liệu này.
setwd("C:\\Users\\DOTHITHANHTAM\\Desktop")
data <- read.csv("housing.csv", header=TRUE)
data
cat("so hang trong data la",nrow(data),"\n")
cat("so cot trong data la",ncol(data),"\n")
dim(data)
#2 Cột Dependents chứa các điểm dữ liệu có giá trị “3+”.
#Hãy sửa các giá trị này thành “3”.
data$Dependents[data$Dependents == "3+"] = "3"
data$Dependents
data$Dependents <- as.numeric(data$Dependents)
#3 Hãy cho biết cột nào trong data chứa giá trị NA.
#Đối với vector có kiểu dữ liệu ký tự,
#thay thế các giá trị NA bởi một giá trị bất kỳ khác NA trong vector đó.
#Đối với vector có kiểu dữ liệu số,
#thay thế các giá trị NA bởi giá trị trung bình của vector đó.
colSums(is.na(data)) 
#In ra tên các cột chứa dữ liệu thiếu
ten = names(data)[colSums(is.na(data))> 0]
ten
#Thay thế dữ liệu thiếu
for(i in ten){
  if(is.numeric(data[[i]]) == TRUE){
    mean_value = mean(data[[i]],na.rm=TRUE)
    data[[i]][is.na(data[[i]])] = mean_value
  }else{
    value2 = c()
    for(j in nrow(data)){
      if(is.na(data[[i]][j]) == F){
        value2 = data[[i]][j]
        break
      }
    }
    data[[i]][is.na(data[[i]])] = value2
  }
}
colSums(is.na(data)) 
#4 Thêm cột Total_Income là tổng thu nhập bằng tổng của 2 cột ApplicantIncome
#và CoapplicantIncome sau đó xóa đi 2 cột này.
data$Total_Income = data$ApplicantIncome + data$CoapplicantIncome
data <- subset(data, select = -c(ApplicantIncome,CoapplicantIncome) )
data

#5 So sánh số đơn đăng ký vay vốn được chấp thuận của khách hàng nam (Male) và
#khách hàng nữ (Female).
gender =c("Male","Female")
gender
sosanh  = c()
for(i in gender){
  sodon =0
  for(j in 1:nrow(data)){
    if (data$Gender[j] == i){
      sodon = sodon + 1
    }
  }
  sosanh[i] = sodon
}
sosanh
names(sosanh)[which.max(sosanh)]
#6
nam = data$Total_Income[data$Gender == "Male"]; nam
nu = data$Total_Income[data$Gender == "Female"]; nu

Xnam = mean(nam)
Sx = sd(nam)
Nx = sum(data$Gender == "Male")

Ynu = mean(nu)
Sy = sd(nu)
Ny = sum(data$Gender == "Female")

T = (Xnam - Ynu)/sqrt(((Sx^2/Nx)+(Sy^2/Ny))); T
if (T>1.96){
  print("thu nhap trung binh cua nam lon hon nu")
} else{
  print("thu nhap trung binh cua nu lon hon nam")
}
#7
data$Credit_History
data$Credit_History[data$Credit_History!=1] = 0
data$Credit_History[data$Credit_History == 1] = 1
data$Credit_History

#Bước 1: thay thế bằng log 
data$Total_Income = log(data$Total_Income)
data$LoanAmount = log(data$LoanAmount)
#Bước 2: Tính h
h = -1.5453+3.2736*data$Credit_History+
          0.1086*data$Total_Income-0.2744*data$LoanAmount
h
#Bước 3: tính phi(h)
oh = 1/(1+exp(-h))
oh
#Bước 4: So sánh phi(h) với 0.5
data$Predict_Loan_status[oh>=0.5] = "Y" 
data$Predict_Loan_status[oh<0.5] = "N"
data$Predict_Loan_status

#8

TP = sum(data$Loan_Status =="Y" & data$Predict_Loan_status == "Y"); TP
TN = sum(data$Loan_Status =="N" & data$Predict_Loan_status == "N"); TN
N = nrow(data); N

accuracy = (TP+TN)/N
accuracy

# Bài 2
# 1. Hãy đọc dữ liệu vào trong R và đặt tên là data sau đó loại bỏ các hàng chứa giá trị
#NA. Cho biết sau khi loại bỏ các giá trị NA thì bộ dữ liệu có bao nhiêu hàng và bao
#nhiêu cột.
setwd("C:\\Users\\DOTHITHANHTAM\\Desktop")
data = read.csv("healthcare.csv", header = TRUE)
data = na.omit(data)
dim(data)
#2. Trích ra 2 bộ dữ liệu có giá trị ở biến Gender lần lượt là Male, Female và đặt tên
#tương ứng là data.male, data.female.
data.male = subset(data, data$Gender == "Male"); data.male
data.female = subset(data, data$Gender == "Female"); data.female
#3. Làm tròn cột viện phí (Billing.Amount) đến một chữ số thập phân sau dấu phẩy
#và so sánh viện phí trung bình của nhóm bệnh nhân nam (Male) và nhóm bệnh
#nhân nữ (Female).
data$Billing.Amount = round(data$Billing.Amount, 1)

bill.male = mean(data.male$Billing.Amount); bill.male
bill.female = mean(data.female$Billing.Amount);bill.female
if(bill.male > bill.female){
  print("viện phí trung bình của nhóm bệnh nhân nam (Male) lớn hơn nhóm bệnh nhân nữ(Female)")} else {
    if(bill.male < bill.female) {
      print("viện phí trung bình của nhóm bệnh nhân nam (Male) bé hơn nhóm bệnh nhân nữ (Female)")} else {
        print("viện phí trung bình của nhóm bệnh nhân nam (Male) bằng nhóm bệnh nhân nữ (Female)")
      }
  }
#4. Cho biết số bệnh nhân ở từng nhóm máu.
for(i in unique(data$Blood.Type)){
  soluong = sum(data$Blood.Type == i)
  print(i)
  print(soluong)
}

#5. Thêm cột Classification.by.Age phân nhóm các bệnh nhân theo tuổi. Các bệnh
#nhân từ 0 đến 14 tuổi được phân vào nhóm Pediatric (Trẻ em), các bệnh nhân từ
#15 đến 47 tuổi được phân vào nhóm Young (Người trẻ), các bệnh nhân từ 48 đến
#63 tuổi được phân vào nhóm Middle age (Trung niên), các bệnh nhân lớn hơn 64
#tuổi được phân vào nhóm Elderly (Người già).
data$Classification.by.Age
data$Classification.by.Age[data$Age <= 14] = "Trẻ em"
data$Classification.by.Age[data$Age >= 15 & data$Age <= 47] = "Người trẻ"
data$Classification.by.Age[data$Age >= 48 & data$Age <= 63] = "Trung niên"
data$Classification.by.Age[data$Age >= 64] = "Người già"
#Cách khác
data$Classification.by.Age_2 <- cut(data$Age,
                                    breaks = c(-Inf, 14,47,63,Inf),
                                    labels = c("Trẻ em", "Người trẻ",
                                               "Trung niên","Người giá"))
data$Classification.by.Age_2 
#6. Viết một hàm nhận vào một tình trạng bệnh lý (Medical.Condition) và trả về độ
#tuổi trung bình của các bệnh nhân có bệnh lý đó. Áp dụng hàm vừa viết cho biết độ
#tuổi trung bình của các bệnh nhân mắc bệnh béo phì (Obesity), ung thư (Cancer)
#và tăng huyết áp (Hypertension).
kiem_tra = function(data, x){
  tuoi_tb = mean(data$Age[data$Medical.Condition == x])
  return(tuoi_tb)
}
kiem_tra(data, "Cancer")
kiem_tra(data, "Obesity")
kiem_tra(data,"Hypertension")
#7
test = function(A,p_0){
  n = nrow(data)
  k = sum(data$Test.Results == A)
  f = k/n
  T_0 = (f - p_0)*sqrt(n)/sqrt(p_0 * (1 - p_0))
  if (T_0 > 1.645){
    print("tỉ lệ bệnh nhân có kết quả xét nghiệm là A lớn hơn p0")} else {
      print("chưa đủ cơ sở")
    }
}

#8. Áp dụng hàm đã viết ở câu k kiểm định xem tỉ lệ bệnh nhân có kết quả xét nghiệm
#là bình thường (Normal) có lớn hơn 50% hay không.
test("Normal", 0.5)


