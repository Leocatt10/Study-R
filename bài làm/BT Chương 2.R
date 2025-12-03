
library(Stat2Data)


# Câu 1 -------------------------------------------------------------------
  ## 1 ----
    data("BirdNest")
    nest = BirdNest
    str(nest)
    View(nest)
    nest = na.omit(nest)

  ## 2 ----
    Nesttype = unique(nest$Nesttype)
    Nesttype
    
    table1 = table(nest$Nesttype)
    table1
    
  ## 3 ----
    pie(table,
        main = "Tỷ lệ các loài chim theo loại tổ",
        col = rainbow(length(table)))
    
    legend("topleft",
        legend = names(table),
        fill = rainbow(length(table)))
    
  ## 4 ----
    Location = unique(nest$Location)
    
    table2 = table(nest$Location)
    table2
    
    barplot(table2,
          col = rainbow(length(table2)),
          ylab = "Số loài",
          xlab = "Vị trí tổ",
          main = "Số lượng tổ chim theo vị trí")
    
  ## 5 ----
    ### C1 ----
    boxplot(No.eggs ~ Nesttype,
            data = nest,
            main = "Biểu đồ hộp số lượng trứng theo loại tổ",
            xlab = "Loại tổ",
            ylab = "Số lượng trứng",
            col = rainbow(length(unique(nest$Nesttype))))
    
    ### C2 ----
    list = split(nest$No.eggs, nest$Nesttype)
    
    boxplot(list,
            main = "Biểu đồ hộp số lượng trứng theo tổ",
            xlab = "Loại tổ",
            ylab = "Số trứng",
            col = rainbow(length(list)))
    
  ## 6 ----
    hist(nest$No.eggs,
         main = "Số lượng trứng trong ổ",
         xlab = "Số trứng",
         col = "green",
         freq = F)    
    
    lines(density(nest$No.eggs), col = "red")
    

# Câu 2 -------------------------------------------------------------------
  ## 1 ----
    data("BlueJays")
    bird = BlueJays
    bird = na.omit(bird)
    View(bird)
    
  ## 2 ----
    table = table(bird$KnownSex)
    table
    
    cols = c("pink", "cyan")
    pie(table,
        col = cols,
        ylab = "Số lượng cá thể",
        xlab = "Giới tính",
        main = "Số lượng cá thể theo giới tính")
    
    legend("topleft",
        legend = names(table),
        fill = cols)
    
  ## 3 ----
    ### C1 ----
    bird$MassGroup <- ifelse(bird$Mass < 60, "<60 g",
                      ifelse(bird$Mass < 70, "60–70 g",
                      ifelse(bird$Mass < 80, "70–80 g", ">80 g")))
    
    
    ### C2 ----
    bird$MassGroup <- cut(
      bird$Mass,
      breaks = c(-Inf, 60, 70, 80, Inf),
      labels = c("<60 g", "60–70 g", "70–80 g", ">80 g")
    )
    
    barplot(table(bird$MassGroup),
            main = "Số lượng chim theo nhóm khối lượng",
            xlab = "Nhóm khối lượng",
            ylab = "Số lượng",
            col = "skyblue")
    
  ## 4 ----
    hist(bird$BillLength,
         col = "green",
         freq = F)
    
    lines(density(bird$BillLength), col = "red")
    
  ## 5 ---
    pairs(bird[, 4:5], col = bird$KnownSex)
    

# Câu 3 -------------------------------------------------------------------
  ## 1 ----
    data("ChickWeight")
    chicken = ChickWeight
    is.na(chicken)
    chicken = na.omit(chicken)
    View(chicken)
    
  ## 2 ----
    specificTime = unique(chicken$Time)
    avgWeight = numeric(length(specificTime))
    
    for (i in 1 : length(specificTime))
    {
        avgWeight[i] = mean(chicken$weight[chicken$Diet == 2 & chicken$Time == specificTime[i]])     
    }
      
    avgWeight

    barplot(avgWeight)
    
  ## 3 ----
    chicken[chicken$Chick == 20, ]
    chicken[chicken$Chick == 22, ]
    chicken[chicken$Chick == 23, ]    
    
  ## 4 ----
    chickDiet3 = subset(chicken, Diet == 3)
    chickDiet3
    
    pairs(chicken[, 1:2], col = chicken$Chick)
    
  ## 5 ----
    chickenBorn = subset(chicken, Time == 0)
    hist(chickenBorn$weight,
         cols = "green",
         freq = F)
    
    lines(density(chickenBorn$weight), col = "red")
    

# Câu 4 -------------------------------------------------------------------
  ## 1 ----
    data("iris")
    flower = iris
    is.na(flower)
    flower = na.omit(flower)
    View(flower)
    
  ## 2 ----
    Q1 = quantile(flower$Petal.Width, 0.25)
    Q3 = quantile(flower$Petal.Width, 0.25)
    IQR = Q3 - Q1
    
    lower = Q1 - 1.5*IQR
    upper = Q3 + 1.5*IQR
    
    flower2 = subset(flower, Petal.Width >= lower & Petal.Width <= upper)
    flower2
    
  ## 3 ----
    Flowers = unique(flower$Species)
    avgHeight = numeric(length(Flowers))
    
    for (i in 1 : length(Flowers))
    {
      avgHeight[i] = mean(flower$Sepal.Length[flower$Species == Flowers[i]])
    }
    
    barplot(avgHeight)
    
  ## 4 ----
    setosa = subset(flower, Species == "setosa")
    
    hist(setosa$Sepal.Length,
         freq = F)
    
    lines(density(setosa$Sepal.Length), col = "red")
    
  ## 5 ----
    pairs(flower[, c(1, 3)],
          col = flower$Species,
          pch = 19)


# Câu 5 -------------------------------------------------------------------
  ## 1 ----
    library(Stat2Data)
    data("Clothing")
    clothing_data = Clothing
    View(clothing_data)
    
    table = table(clothing_data$Card)
    table
    
  ## 2 ----
    Q1Am = quantile(clothing_data$Amount, 0.25)
    Q3Am = quantile(clothing_data$Amount, 0.75)
    IQRAm = Q3Am - Q1Am
    
    lowAm = Q1Am - 1.5*IQRAm
    upAm = Q3Am + 1.5*IQRAm
    
    
    Q1D12 = quantile(clothing_data$Dollar12, 0.25)
    Q3D12 = quantile(clothing_data$Dollar12, 0.75)
    IQRD12 = Q3D12 - Q1D12
    
    lowD12 = Q1D12 - 1.5*IQRD12
    upD12 = Q3D12 + 1.5*IQRD12
    
    
    Q1D24 = quantile(clothing_data$Dollar24, 0.25)
    Q3D24 = quantile(clothing_data$Dollar24, 0.75)
    IQRD24 = Q3D24 - Q1D24
    
    lowD24 = Q1D24 - 1.5*IQRD24
    upD24 = Q3D24 + 1.5*IQRD24
    
    clothing_data2 = subset(clothing_data,
                            (Amount >= lowAm & Amount <= upAm) &
                            (Dollar12 >= lowD12 & Dollar12 <= upD12) &
                            (Dollar24 >= lowD24 & Dollar24 <= upD24))
    
    clothing_data2
    
  ## 3 ----
    plot(clothing_data2$Dollar24 ~ clothing_data2$Freq24,
          col = "green",
          pch = 19)
    
  ## 4 ----
    dataCustomers = clothing_data2[1:5, c(4, 6)]
    matr = as.matrix(dataCustomers)
    matr
    
    barplot(matr,
            beside = T)
    
  ## 5 ----
    hist(clothing_data2$Dollar12,
         freq = F)
    
    lines(density(clothing_data2$Dollar12), col = "red")
    
  ## 6 ---- 
    noCardCus = subset(clothing_data2, Card == 0)
    
    noCardCus$SpendGroup = cut(noCardCus$Dollar12,
                               breaks = c(-Inf, 150, 300, 450, Inf),
                               labels = c("<150", "150-300", "300-450", ">450"))
    
    pie(table(noCardCus$SpendGroup),
        col = rainbow(length(table(noCardCus$SpendGroup))))
    
    legend("topleft",
        legend = names(table(noCardCus$SpendGroup)),
        fill = rainbow(length(table(noCardCus$SpendGroup))))
    

# Câu 6 -------------------------------------------------------------------
  ## 1 ----
    data("Diamonds")
    diamonds_data = Diamonds
    diamonds_data = na.omit(diamonds_data)
    View(diamonds_data)
    
  ## 2 ----
    removeAbnormalities = function(x)
    {
      Q1 = quantile(x, 0.25)
      Q3 = quantile(x, 0.75)
      IQR = Q3 - Q1
      low = Q1 - 1.5 * IQR
      up = Q3 + 1.5 * IQR
      
      return(x >= low & x <= up)
    }  
    
    diamonds_data2 = subset(diamonds_data,
                            removeAbnormalities(Depth) &
                            removeAbnormalities(PricePerCt) &
                            removeAbnormalities(TotalPrice))
    diamonds_data2
    
  ## 3 ----
    plot(diamonds_data2$Carat, diamonds_data2$TotalPrice,
         xlab = "Carat",
         ylab = "Total Price",
         main = "Scatter plot: Carat vs Total Price",
         pch = 19, col = "blue")
  
  ## 4 ----    
    table = table(diamonds_data2$Color)
    
    pie(table,
        col = rainbow(length(table)),
        labels = paste0(names(table), "(", round(100*table/sum(table), 2), "%", ")"),
        cex = 0.6)
    
    legend("topright",
          legend = names(table),
          fill = rainbow(length(table)))
    
  ## 5 ----
    diamonds_I = subset(diamonds_data2, diamonds_data2$Color == "I")
    
    hist(diamonds_I$TotalPrice,
         freq = F)
    
    lines(density(diamonds_I$TotalPrice), col = "red")
    
  ## 6 ----
    clarities = unique(diamonds_data2$Clarity)
    meanCla = numeric(length(clarities))
    
    for (i in 1 : length(clarities))
    {
      meanCla[i] = mean(diamonds_data2$TotalPrice[diamonds_data2$Clarity == clarities[i]])
    }
    
    barplot(meanCla)
    

# Câu 7 -------------------------------------------------------------------
  ## 1 ----
    data("mtcars")
    mtcars = mtcars
    View(mtcars)
    
    table = table(mtcars$cyl)
    
    barplot(table,
            col = rainbow(length(table)),
            xlab = "Số lượng xy lanh",
            ylab = "Số xe")
    
  ## 2 ---- 
    gears = unique(mtcars$gear)
    avgMpg = numeric(length(gears))
    
    for (i in 1 : length(gears))
    {
      avgMpg[i] = mean(mtcars$mpg[mtcars$gear == gears[i]])
    }
    
    barplot(avgMpg)
    
  ## 3 ----
    hist(mtcars$hp,
         freq = F)
    
    lines(density(mtcars$hp), col = "red")
    
  ## 4 ----
    cols = c("red", "blue", "green")
    names(cols) = sort(unique(mtcars$cyl))
    
    # hp vs mpg
    plot(mtcars$hp, mtcars$mpg,
         pch = 19,
         col = cols[as.character(mtcars$cyl)],
         xlab = "hp",
         ylab = "mpg",
         main = "hp vs mpg")
    
    legend("topright",
           legend = names(cols),
           col = cols, pch = 19,
           title = "cyl")
    
    # hp vs wt
    plot(mtcars$hp, mtcars$wt,
         pch = 19,
         col = cols[as.character(mtcars$cyl)],
         xlab = "hp",
         ylab = "wt",
         main = "hp vs wt")
    
    legend("topright",
           legend = names(cols),
           col = cols, pch = 19,
           title = "cyl")
    
  ## 5 ----
    boxplot(mtcars$mpg ~ mtcars$cyl,
            col = rainbow(3))
    
# Câu 8 -------------------------------------------------------------------
  ## 1 ----    
    data("airquality")
    air = airquality
    View(air)
    
    meanOz = mean(air$Ozone, na.rm = T)
    meanOz
    meanSR = mean(air$Solar.R, na.rm = T)
    meanSR
    
    air$Ozone[is.na(air$Ozone)] = meanOz
    air$Solar.R[is.na(air$Solar.R)] = meanSR
    air
  
  ## 2 ----    
    removeAbnormalities = function(x)
    {
      Q1 = quantile(x, 0.25)
      Q3 = quantile(x, 0.75)
      IQR = Q3 - Q1
      
      low = Q1 - 1.5 * IQR
      up = Q3 + 1.5 * IQR
      
      return(x >= low & x <= up)
    }
    
    air2 = subset(air,
                  removeAbnormalities(air$Ozone) &
                  removeAbnormalities(air$Solar.R) &
                  removeAbnormalities(air$Wind) &
                  removeAbnormalities(air$Temp))
    
    air2
    
  ## 3 ----
    
    months = unique(air2$Month)
    meanTemp = numeric(length(months))
    meanSR = numeric(length(months))
    
    for(i in 1 : length(months))
    {
      meanTemp[i] = mean(air2$Temp[air2$Month == months[i]])
      meanSR[i] = mean(air2$Solar.R[air2$Month == months[i]])
    }
    meanTemp
    meanSR
    
    mat = rbind(meanTemp, meanSR)
    colnames(mat) = months
    rownames(mat) = c("Temp", "Solar.R")
    mat
    
    barplot(mat,
            beside = T,
            col = rainbow(2),
            xlab = "Tháng",
            ylab = "Giá trị trung bình")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
