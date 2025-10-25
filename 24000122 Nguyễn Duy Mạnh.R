### bài 4

#a
pr <- c(9000, 9500, 9400, 9400, 10000, 9500, 10300, 10200)
pr

#b
a = mean(pr)
a
Edmund = 9500
a > Edmund

#c
sum(pr>Edmund)



### bài 5

#a
work <- c(17,16,20,24,22,15,21,15,17,22)
min(work)
mean(work)
max(work)

#b
work[work==24] <- 18
work

#c
a = sum(work > 20)
a
b = sum(work <= 17)
tyle = b/length(work)*100
tyle


#bai6
#a
bill <- c(46,33,39,37,46,30,48,32,49,35,30,48)
sum(bill)

#b
mean(bill)   #gia tien dien trung binh 12 thang
max(bill)    #thang co tien dien nhieu nhat
min(bill)    #thang co tien dien it nhat

#c
bill[3] <- 40
bill

#d
a = sum(bill > 40)
tyle = a / length(bill) * 100
tyle




#Bài 7
#Câu a
teddy_sales = c(15, 20, 18, 25, 30, 35, 42)
sum(teddy_sales)

#Câu b
mean(teddy_sales)
min(teddy_sales)
max(teddy_sales)

#Câu c
teddy_sales[teddy_sales == 42] = 32
teddy_sales

#Câu d
countMoreThan30 = 0
for (i in 1:length(teddy_sales))
{
  if (teddy_sales[i] >= 30)
  {
    countMoreThan30 = countMoreThan30 + 1
  }
}
sprintf("%.f%%", (countMoreThan30/length(teddy_sales)) * 100)

countLessThan20 = 0
for (i in 1:length(teddy_sales))
{
  if (teddy_sales[i] <= 20)
  {
    countLessThan20 = countLessThan20 + 1
  }
}
sprintf("%.f%%", (countLessThan20/length(teddy_sales)) * 100)


#Bài 8
#Câu a
x = seq(5, 11, 0.3)
x

#Câu b
x = rep(c(-1, 3, 5, 7, 9), 2)
x
sort(x)

#Câu c
x = seq(6, 12, 1)
y = rep(x[4], 2)
fin = c(x, y)
fin

#Câu d
x = seq(102, length(fin), length = 9)
x

#Bài 9
#Câu a
x = seq(3, 6, length = 5)
x
y = rep(c(2, -5.1, -33), 2)
y
x = c(x, y)
x

#Câu b
a = x[-c(1, length(x))]
a

#Câu c
x = sort(x)
x

#Câu d
z = c(rep(a[3], 3), rep(a[6], 4), a[length(a)])
z


#bai11
mat2 <- matrix(c(seq(from=1, to=10, by=2), 5:1, rep(x=2017, times=5)), ncol=3)
mat2
#a
colnames(mat2) = c("day","month","year")
#b
rownames(mat2) = c("A","B","C","D","E")
mat2
#c
for(i in 1:length(rownames(mat2))){
  if(mat2[i,"month"] >=3){
    print(row.names(mat2)[i])
  }
}
#d
for(i in 1:length(rownames(mat2))){
  for (j in 1:length(colnames(mat2))){
    if(mat2[i,j] == 2017){
      mat2[i,j] = 2018
    }
  }
}
mat2
#e
for(i in 1:length(rownames(mat2))){
  mat2[i,2]= mat2[i,2]*7
}
mat2
