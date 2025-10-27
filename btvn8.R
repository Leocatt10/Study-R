#Bài 1 
#a.

tieu_thu <- seq(1700,2300,by = 100)
so_ho <- c(3,4,5,7,8,5,3)

m <- data.frame(tieu_thu, so_ho)

#b.

#Bài 5
#a. 

data_csv <- read.csv("C:\Users\Admin\Downloads\Sampledatasafety.csv")
data <- data_csv
data <- na.omit(data)
colSums(is.na(data))

#b.

data$Incident.Type = as.character(data$Incident.Type)
burn = subset(data,Incident.Type == "Burn")
sum_cost = sum(burn$Incident.Cost)

#c.






