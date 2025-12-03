#1. Bảng tần số
x <- sample(seq(1,5,0.5), 100, replace = TRUE)
x
#Để tạo bảng tần số, sử dụng lệnh table()
Table <- table(x)
Table
Table <- as.data.frame(Table)
Table
#Tính tần suất, tần suất tích lũy
f <- Table$Freq/length(x) #Tần suất
f
cumsum(f) #Tần suất tích lũy
#Ghép tần suất vừa tính được vào bảng
Table <- cbind(Table, f, cumsum(f))
Table

#2. Bảng tần số ghép lớp
y <- sample(seq(1:100), 500, replace = TRUE)
y
#Tạo các lớp ghép
y <- cut(y, breaks = c(-Inf, 20, 40, 60, 80, Inf))
y
#Tạo bảng tần số
Table_y <- table(y)
Table_y
Table_y <- as.data.frame(Table_y)
#Tính tần suất, tần suất tích lũy tương tự phần trên
f_y <- Table_y$Freq/length(y)
f_y
cumsum(f_y)
#Ghép vào bảng tần số 
Table_y <- cbind(Table_y, f_y, cumsum(f_y))
Table_y


#3. Bảng tần số đồng thời
install.packages("dplyr")
library(dplyr)
data("storms")
unique(storms$status) #có 9 loại bão
unique(storms$category) #có 5 cấp độ bão
#Tạo bảng tần số đồng thời
table(storms$status, storms$category)


#4 Các tham số thống kê
x <- sample(seq(1,5,0.5), 100, replace = TRUE)
x
#Trung bình mẫu
mean(x)
#Trung vị mẫu
median(x)
#mod
Table <- table(x)
Table <- as.data.frame(Table)
Table
which.max(Table$Freq) #Tìm vị trí có tần số lớn nhất
Table[which.max(Table$Freq), 1] #giá trị mod
#Phương sai
var(x)
#độ lệch tiêu chuẩn
sd(x)
#Khoảng tứ phân vị
Q1 <- quantile(x, 0.25); Q1 #Phân vị mức 25%
Q3 <- quantile(x, 0.75); Q3 #Phân vị mức 75%
IQR <- Q3 - Q1; IQR #Khoảng tứ phân vị


