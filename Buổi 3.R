#CÁC PHÉP TOÁN CƠ BẢN TRONG R
#Phép cộng
3+2
1234+567
#Phép trừ
3-2
321-12
#Phép nhân
3*2
123*456
#Phép chia
100/3 #phép chia thông thường
100%/%3 #phép chia có dư
100%%3 #số dư
#lũy thừa
3^2
4^2
#căn bậc hai
sqrt(9)
#hàm e mũ
exp(0)
exp(1)
#Hàm logarit cơ số e (hàm ln)
log(1)
log(10)
#Hàm logarit cơ số 10
log10(1000)
#Hàm logarit cơ số a nào đó
log(25, base = 5) #logarit cơ số 5 của 25

#VECTO
#Nhập vecto
#Để nhập dữ liệu bằngtay ta sử dụng hàm c()
c(1,2,3,4,5)
#đặt tên: có 2 cách
x <- c(1,2,3,4,5)
x = c
x
#Các phép toán trên vecto
x <- c(1,2,3,4,5)
y <- c(6,7,8,9,10)
x
y
#Phép cộng
x+2 #cộng tất cả các phần tử trong vecto thêm 2 đơn vị
x+y #cộng hai vecto với nhau
#Phép trừ: tương tự như phép cộng
#Phép nhân
x*2 #nhân tất cả các phần tử của vecto với 2
x
y
x*y #Nhân từng phần tử tương ứng với nhau
x%*%y #phép nhân vecto thông thường (tích vô hướng)
#phép chia
x
x/10 #chia từng phần tử của vecto cho 10
#độ dài của vecto: sử dụng hàm length()
length(x)
#tìm GTLN, GTNN trong vecto
max(x)
min(x)
#giá trị trung bình: sử dụng hàm mean()
mean(x)
#phương sai: sử dụng hàm var()
var(x)
#độ lệch tiêu chuẩn: sử dụng hàm sd()
sd(x)
#tính tổng: sử dụng hàm sum()
x
sum(x)
sum(x^2) #tổng của bình phương từng phần tử
#so sánh
x
y
x<y #so sánh từng phần tử tương ứng, kết quả trả về TRUE/FALSE
x==y #so sánh bằng
x!=y #so sánh khác
#sắp xếp: sử dụng hàm sort()
x
sort(x) #sắp xếp từ nhỏ đến lớn
sort(x, decreasing = FALSE) #sắp xếp từ nhỏ đến lớn
sort(x, decreasing = TRUE) #sắp xếp từ lớn đến bé
#tạo vector bằng hàm seq()
seq(4, 6, 0.5) #tạo vecto bắt đầu 4, kết thúc 6, khoảng cách các phần tử là 0.5
seq(4, 6, length=6) #tạo vecto có độ dài bằng 6, bắt đầu 4, kết thúc 6
#lặp vector bằng hàm rep()
rep(c(1,2,3,4), 3) #Lặp 3 lần vector 1 2 3 4 









