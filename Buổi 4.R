#Vecto
#Buổi trước: Các phép toán trên vecto, so sánh, sắp xếp, tạo
#Thay thế giá trị nào đó trong vecto
x <- c(1,2,3,4,5,6,7,9)
x
#Thay vị trí thứ 3 của vecto x bằng số 10
x[3] <- 10
x
#Thay số 10 bằng số 3
x[x==10] <- 3
x

#Ma trận
#Nhập vào ma trận: sử dụng lệnh matrix()
y <- c(1,2,3,4,5,6,7,8,9)
#Điền các giá trị của vecto y theo hàng
A <- matrix(y, nrow = 3, ncol = 3, byrow = TRUE)
A
#Điền các giá trị của vecto y theo cột
B <- matrix(y, nrow = 3, ncol = 3, byrow = FALSE)
B
matrix(y, nrow = 3, ncol = 3)
#Nhận xét: Ma trận B là chuyển vị của ma trận A và ngược lại
t(A) #hàm t() dùng để tìm ma trận chuyển vị

#Lấy ra một phần tử của ma trận
#A[số hàng, số cột]
A[1,3] #Lấy ra phần tử hàng 1 cột 3 của ma trận A
A[2,2] #Lấy ra phần tử hàng 2 cột 2 của ma trận A
A[2,] #Lấy tất cả phần tử hàng 2 của ma trận A
A[,1] #Lấy tất cả phần tử cột 1 của ma trận A
A[-1,] #Bỏ đi hàng 1 của ma trận A
A[,-3] #Bỏ đi cột 3 cua ma trận A

#Tạo ma trận đơn vị
I <- matrix(0, nrow = 3, ncol = 3)
I
diag(I) #Lấy các phần tử trên đường chéo của ma trận
diag(I) <- 1
I

#Các phép tính trên ma trận
A
B
#Phép nhân 
A*B #Chỉ là nhân từng phần tử tương ứng với nhau
C <- A%*%B #Đây mới là phép nhân 2 ma trận A và B
C
#Tính định thức
det(A)
det(C)
#Tìm ma trận nghịch đảo
x <- c(1,2,3,4,5,6,7,8,10)
D <- matrix(x,3,3)
D
solve(D) #Tính ma trận nghịch đảo của ma trận D

#Tính toán trong xác suất
#Các phép đếm
#Hoán vị: P_n = n!
factorial(3)
factorial(4)
#Tổ hợp: 
choose(3,1) 
choose(5,3)
#Chỉnh hợp: n!/(n-k)!

#Biến ngẫu nhiên: Hàm mật độ, hàm phân bố
#Sinh ra 100 giá trị ngẫu nhiên tuân theo phân phối chuẩn
#với kì vọng bằng 0, phương sai bằng 1
x <- rnorm(100,mean = 0, sd = 1)
x
mean(x)
sd(x)
#Hàm mật độ tại giá trị x1
dnorm(x[1], mean = 0, sd = 1)
#Hàm phân phối tại giá trị x1
pnorm(x[1], mean = 0, sd = 1)

#Bài tập Tuần 4
#Câu 1:
# a
sqrt(144)/(2^5) + exp(pi)
# b
(log(1000) + sqrt(37))/exp(2)
# c
sqrt(2^(log10(35))- log(5)) 
# d
(40/100)*(exp(2)^3 + pi)
# e
log2(128) + pi^2 - log(1/6)
# f
log((log(1000))^2) + 1/sqrt(2)

#Câu 2
#a
x <- 1:100
sum(x)
#b
sum(x^2)
#c
sum(x^3-4*x^2)
#d
i <- 1:9
sum(1/(i*(i+1)))

#Câu 3:
GK <- c(7,8,9,4,8,7,5)
GK
#a
GK[3]
#b 
GK[GK==4] <-8
GK
#c 
GK <- GK + 1.5
GK
GK[GK>10] <- 10
GK
#d 
TX <- c(1,2,3,2,9,10,10)
CK <- c(2,3,4,2,10,10,9)
TBC <- TX*0.2+GK*0.2+CK*0.6
TBC
#e 
diemchu  <- ifelse(TBC>=8,"A","B")
diemchu
#hoặc cách khác
diemchu2 <- TBC; diemchu2
diemchu2[diemchu2>=8] <- "A"
diemchu2[diemchu2<8] <- "B"
diemchu2
