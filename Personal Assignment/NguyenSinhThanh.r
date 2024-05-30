#----------------------------------------------------------------------------------------------Bài 1--------------------------------------------------------------
wd <- getwd()
if (!is.null(wd)) setwd(wd)

raw = read.csv("gia_nha.csv")
new_DF = raw[,c("price","sqft_living15","floors","condition","sqft_above","sqft_living")]
new_DF = na.omit(new_DF)

new_DF[,c("price","sqft_living15","sqft_above","sqft_living")] <- log(new_DF[,c("price","sqft_living15","sqft_above","sqft_living")])


#thống kê mô tả cho biến liên tục
c_mean <- c(mean(new_DF[,c("price")]),mean(new_DF[,c("sqft_living15")]),mean(new_DF[,c("sqft_above")]),mean(new_DF[,c("sqft_living")]))
c_median <- c(median(new_DF[,c("price")]),median(new_DF[,c("sqft_living15")]),median(new_DF[,c("sqft_above")]),median(new_DF[,c("sqft_living")]))
c_sd <- c(sd(new_DF[,c("price")]),sd(new_DF[,c("sqft_living15")]),sd(new_DF[,c("sqft_above")]),sd(new_DF[,c("sqft_living")]))
c_min <- c(min(new_DF[,c("price")]),min(new_DF[,c("sqft_living15")]),min(new_DF[,c("sqft_above")]),min(new_DF[,c("sqft_living")]))
c_max <- c(max(new_DF[,c("price")]),max(new_DF[,c("sqft_living15")]),max(new_DF[,c("sqft_above")]),max(new_DF[,c("sqft_living")]))

c_table <- data.frame(rbind(c_mean,c_median,c_sd,c_min,c_max))

colnames(c_table) <- c("price","sqft_living15","sqft_above","sqft_living")
rownames(c_table) <- c("Mean","Median","Standard Deviation","Min","Max")
head(c_table,10)

#thống kê số lượng cho từng phân loại
table_floors <- data.frame(table(new_DF$floors)) #data.frame để đưa vector về list, cột dạng bảng mới tính toán
table_condition <- data.frame(table(new_DF$condition))

colnames(table_floors) <- c("floors","freq")
colnames(table_condition) <- c("condition","freq")



#Vẽ đồ thị
hist   (new_DF$price,main = "Distribution of Price", xlab = "Price", ylab = "Frequency")

boxplot(new_DF$price~new_DF$floors, main = "Distribution of Price for each group of Floors", xlab = "Number of floors", ylab = "Price",)
boxplot(new_DF$price~new_DF$condition, main = "Distribution of Price for each group of Condition", xlab = "Number of floors", ylab = "Price",)

#biểu dồ price lần lượt theo các biến liên tục...
pairs(price~sqft_living15, data = new_DF)
pairs(price~sqft_above, data = new_DF)
pairs(price~sqft_living, data = new_DF)


#http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/
#https://statsandr.com/blog/multiple-linear-regression-made-simple/
#https://towardsdatascience.com/understanding-linear-regression-output-in-r-7a9cbda948b3
model = lm(price ~ sqft_living15 + floors + condition + sqft_above + sqft_living, data = new_DF)
summary(model)
anova(model)

model2 = lm(price ~ sqft_living15 + floors  + sqft_above + sqft_living, data = new_DF)
#F của bảng anova cho ra giá trị Fisher xem mô hình đó CÓ PHÙ HỢP hay không, còn lm cho từng hệ số

#so sánh 2 mô hình, Pvalue thấp thì nhận
anova(model, model2)

#vẽ đồ thị và nhận xét
plot(model)


x1 = data.frame(sqft_living15 =     c_table["Mean", "sqft_living15"], 
                       sqft_above = c_table["Mean", "sqft_above"],
                       sqft_living =c_table["Mean", "sqft_living"],
                       floors = 2,
                       condition = 3)
x2 = data.frame(sqft_living15 = c_table["Max", "sqft_living15"], 
                sqft_above =    c_table["Max", "sqft_above"],
                sqft_living =   c_table["Max", "sqft_living"],
                floors = 2,
                condition = 3)



predict(model, newdata= x1)
predict(model, newdata= x2)



#-----------------------------------------------------------------------------------------------Bài 5---------------------------------------------------------------------------------
library(rlist)
library(dplyr)
library(car)

bigTable <- read.csv("chicken_feed.csv", header=TRUE)
bigTable$feed <- as.factor(bigTable$feed)
bigTable <- na.omit(bigTable)
foods = unique(bigTable $feed)

#tạo những bảng rỗng để sử dụng
set = list()
c_max=c_sd = c_range = c_mean = c_max = c_mean = c()
c_summary=data.frame(range(6)) #rỗng

for (i in c( 1:length(foods)))
{
  data = subset(bigTable, feed == foods[i]) $weight #tách được dữ liệu
  set <- list.append(set, data)
  print(foods[i])
  c_max    = c(c_max,    max(data))
  c_median = c(c_median, median(data)) 
  c_mean   = c(c_mean,   mean (data))
  c_sd     = c(c_sd,     sd(data))
  c_summary = data.frame(c_summary, data.frame (as.array(summary(data)))[2] ) #giá trị lớn nhất, nhỏ nhất, tứ phân vị, độ lệch chuẩn, outlier
}
names(set) <- foods
c_summary <- c_summary[ , -1] #để xóa cột đầu tiên
summary_table = rbind(c_summary, c_sd)
rownames(summary_table) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "Standard deviation")
colnames(summary_table) <- foods
summary_table #bảng cuối cùng ta có để mo tả về dữ liệu này


boxplot(set, names = foods,par(cex.axis=0.7),
        main = "Trọng lượng của gà sau một thời gian ăn", ylab ="Weight" )


#boxplot(weight~feed, data=bigTable, names = foods,par(cex.axis=0.7),
#        main = "Trọng lượng của gà sau một thời gian ăn1", ylab ="Weight" )


#kiểm định phương sai và trung bình mẫu
sharipowilk = c(1,2,3,4)
for (i in c( 1:length(foods)))
{
  sharipowilk <- rbind (sharipowilk, shapiro.test(set[[i]]))
  qqnorm(set[[i]], ylab = paste(foods[i], "quantiles", sep = ' ') )
  qqline(set[[i]])
}
sharipowilk = sharipowilk[-1, ]
rownames(sharipowilk) = foods
sharipowilk = sharipowilk[ , c(1,2,3)]

leveneTest(weight~feed,bigTable) #-> phương sai bằng nhau không thể bác bỏ


anova(lm(weight~feed, data = bigTable))
#residual: trong từng mẫu
#feed, giữa các mẫu
#-> bác bỏ trung bình bằng nhau
#https://stats.stackexchange.com/questions/253588/interpreting-tukeyhsd-output-in-r
model = aov(weight~feed, data=bigTable)
TukeyHSD(model)


#-----------------------------------------------------------------------------------------------Phần riêng-------------------------------------------------------------------------
library("tidyverse")
setwd((getwd()))

#---------READ DATA-------------
raw <- read.csv("Computers.csv")
attach(raw)

table <- data.frame(price, speed, hd, ram, screen, cd, trend)
detach(raw)
attach(table)

table[table[,] == ""] = NA

analyze <- function(x){
  parameter = c("Min:", "Qua1:", "Median:", "Qua3:", "Max:", "Min:", "Sd:", "Var:")
  value = c(min(x), summary(x)[2], median(x), summary(x)[5], max(x), min(x), sd(x), var(x))
  print(data.frame(parameter, value))
}
#apply(is.na(table), 2, which)
table <- na.omit(table)

#------------DATA VISUALIZATION---

#Thống kê mô tả các đại lượng đặc trưng
speed <- as.factor(speed)
ram <- as.factor(ram)
screen <- as.factor(screen)
cd <- as.factor(cd)



#Thống kê các biến định tính
SUMMARY <- function(x)
{
  df <- data.frame(table(x))
  colnames(df) <- c("Type", "Quantity")
  df$Percent <- df$Quantity / sum(df$Quantity) * 100
  print(df)
}

SUMMARY(speed)
SUMMARY(ram)
SUMMARY(cd)
SUMMARY(screen)
#Vẽ biểu đồ cho các biến định lượng

analyze(price)
analyze(hd)
analyze(trend)

par(mfrow = c(1,3))
hist(price, breaks = 10,
     xlab = "Price($)", main = "Histogram of Price",
     )

hist(hd, breaks = 10,
     xlab = "Hard Drive (GB)", main = "Histogram of Hard Drive",
     )
hist(trend, breaks = 10,
     xlab = "Trend", main = "Histogram of Trend of Computers",
     )

  #Kiểm định sự ảnh hưởng của các biến định tính lên price

par(mfrow = c(1,2), pch = 20)
boxplot(price ~ ram, 
        xlab = "RAM (GB)",
        ylab = "Price ($)",
        main = "Botplot of Price vs RAM",
        col = "goldenrod3")
boxplot(price ~ screen, 
        xlab = "Screen (inch)",
        ylab = "Price ($)",
        main = "Botplot of Price vs Screen",
        col = "goldenrod3")
boxplot(price ~ speed, 
        xlab = "Speed (Hz)",
        ylab = "Price ($)",
        main = "Botplot of Price vs Speed",
        col = "goldenrod3")
boxplot(price ~ cd, 
        xlab = "cd",
        ylab = "Price ($)",
        main = "Botplot of Price vs cd",
        col = "goldenrod3")
anova(lm(price ~ ram))
anova(lm(price ~ screen))
anova(lm(price ~ speed))
anova(lm(price ~ cd))

par(mfrow = c(1,2))
plot(price ~ hd, pch = 20, main = "Price vs Hard Drive", col = "blue",
     xlab = "Hard Drive (GB)", ylab = "Price ($)")
plot(price ~ as.numeric(ram), pch = 20, main = "Price vs RAM", col = "blue",
     xlab = "RAM (GB)", ylab = "Price ($)")

cd <- as.character(cd)
cd <- replace(cd, cd=="yes", 1)
cd <- replace(cd, cd=="no", 0)
speed=as.numeric(speed)
hd = as.numeric(hd)
trend = as.numeric(trend)
screen = as.numeric(screen)
ram = as.numeric(ram)
 
model = lm(formula = price ~ cd + speed + ram + hd + trend + screen)
summary(model)

par(mfrow = c(1,2))
plot(model)  

