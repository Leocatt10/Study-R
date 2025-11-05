#Bài 1
#1. Hãy đọc dữ liệu vào trong R và đặt tên là data. Cho biết số hàng và số cột của bộ
#dữ liệu này.

data <- read.csv("C:/Users/Admin/Desktop/R/data/housing.csv")
data

dim(data)

#2. Cột Dependents chứa các điểm dữ liệu có giá trị “3+”. Hãy sửa các giá trị này thành
#“3”.

data$Dependents <- ifelse(data$Dependents == "3+", "3", data$Dependents)

#3.Hãy cho biết cột nào trong data chứa giá trị NA. Đối với vector có kiểu dữ liệu ký
#tự, thay thế các giá trị NA bởi một giá trị bất kỳ khác NA trong vector đó. Đối với
#vector có kiểu dữ liệu số, thay thế các giá trị NA bởi giá trị trung bình của vector
#đó.


data_empty = colSums(is.na(data))
data_empty = names(data_empty[data_empty > 0])

for(name in data_empty){
  col = data[[name]]
  if(is.numeric(col)){
    
    data[[name]][is.na(data[[name]])] = mean(data[[name]], na.rm = TRUE)
  }else{
    data[[name]][is.na(data[[name]])] = "missing"
  }
}


#4.Thêm cột Total_Income là tổng thu nhập bằng tổng của 2 cột ApplicantIncome
#và CoapplicantIncome sau đó xóa đi 2 cột này.

data$Total_Income <- data$ApplicantIncome + data$CoapplicantIncome
data$ApplicantIncome <- NULL
data$CoapplicantIncome <- NULL

#5.So sánh số đơn đăng ký vay vốn được chấp thuận của khách hàng nam (Male) và
#khách hàng nữ (Female).


male <- sum(data$Gender == "Male" & data$Loan_Status == "Y")
female <- sum(data$Gender == "Female" & data$Loan_Status == "Y")

if (male > female) {
  print("Nam nhiều hơn")
} else if (female > male) {
  print("Nữ nhiều hơn")
} else {
  print("Bằng nhau")
}
 
#6.
X = "Male"
Y = "Female"

length_X = sum(data$Gender == X)
length_Y = sum(data$Gender == Y)
income_mean_test = ((mean(data$Total_Income[data$Gender == X]) 
                     - mean(data$Total_Income[data$Gender == Y]))
                    / sqrt(sd(data$Total_Income[data$Gender == X])^2 / length_X
                           + sd(data$Total_Income[data$Gender == Y])^2 / length_Y))
#7,

data$Total_Income = log(data$Total_Income, base = exp(1))
data$LoanAmount = log(data$LoanAmount, base = exp(1))

data$Credit_History = ifelse(data$Credit_History > 0, 1, 0)

h = -1.5453 + 3.2736 * data$Credit_History + 0.1086 * data$Total_Income - 0.2744 * data$LoanAmount
results_predic = 1 / (1 +  exp(-h))
data$Predict_Loan_status = ifelse(results_predic > 0.5, "Y", "N")

#8, kiểm tra tính chính xác của mô hình
table(data$Predict_Loan_status)
accuracy = ((sum(data$Predict_Loan_status == "Y" & data$Loan_Status == "Y") 
             + sum(data$Predict_Loan_status == "N" & data$Loan_Status == "N"))
            / length(data$Predict_Loan_status))



