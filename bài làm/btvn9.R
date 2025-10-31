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

colSums(is.na(data))


colSums(is.na(data))

#4.Thêm cột Total_Income là tổng thu nhập bằng tổng của 2 cột ApplicantIncome
#và CoapplicantIncome sau đó xóa đi 2 cột này.

data$Total_Income <- data$ApplicantIncome + data$CoapplicantIncome
data$ApplicantIncome <- NULL
data$CoapplicantIncome <- NULL

#5.So sánh số đơn đăng ký vay vốn được chấp thuận của khách hàng nam (Male) và
#khách hàng nữ (Female).


male <- sum(data$Gender == "Male" & data$Loan_Status == "Approved")
female <- sum(data$Gender == "Female" & data$Loan_Status == "Approved")

if (male > female) {
  print("Nam nhiều hơn")
} else if (female > male) {
  print("Nữ nhiều hơn")
} else {
  print("Bằng nhau")
}
 
#6.



