setwd("D:/1-2/")
covid=read.csv("owid-covid-data.csv",header=TRUE) #set data
covid = covid[(covid['location']=="Brazil") | (covid['location']=="Chile") | (covid['location']=="Venezuela"),]
covid$new_cases = abs(covid$new_cases)
covid$new_deaths = abs(covid$new_deaths)

par(mfrow=c(1,3)) # 1x3 plots in one plot-drawing
name = covid[, c(3),drop=FALSE]
name = unique(na.omit(name))
cases = data.frame(Countries=character(),
                    Min = integer() ,
                    Q1=double(), 
                    Q2=double(), 
                    Q3=double(), 
                     Max=integer(), 
                    Avg=double(), 
                    Std=double(),
                    Outlier=integer())
deaths = data.frame(Countries=character(),
                   Min = integer() ,
                   Q1=double(), 
                   Q2=double(), 
                   Q3=double(), 
                   Max=integer(), 
                   Avg=double(), 
                   Std=double(),
                   Outlier=integer())
for (i in 1:3) {
  #lay thong so tung quoc gia
  uni=covid[covid$location==name[i,1],c(4,5,6)] 
  cases[i,1]=name[i,1]
  deaths[i,1]=name[i,1]
  cat("# Thong so nuoc",name[i,1],":\n")
  #Gia tri lon nhat, nho nhat cua cases va deaths
  cases[i,2]=min(uni$new_cases,na.rm = TRUE)
  cases[i,6]=max(uni$new_cases,na.rm = TRUE)
  cat(" 1/ So ca nhiem lon nhat theo ngay la :", cases[i,6], "\n")
  cat(" 2/ So ca nhiem nho nhat theo ngay la :", cases[i,2], "\n")
  deaths[i,2]=min(uni$new_deaths,na.rm = TRUE)
  deaths[i,6]=max(uni$new_deaths,na.rm = TRUE)
  cat(" 3/ So ca tu vong lon nhat theo ngay la :", deaths[i,6], "\n")
  cat(" 4/ So ca tu vong nho nhat theo ngay la :", deaths[i,2], "\n")
  #Tu phan vi thu nhat, thu hai va thu ba
  cases[i,3]=quantile(uni$new_cases,0.25,na.rm = TRUE)
  deaths[i,3]=quantile(uni$new_deaths,0.25,na.rm = TRUE)
  cases[i,4]=quantile(uni$new_cases,0.5,na.rm = TRUE)
  deaths[i,4]=quantile(uni$new_deaths,0.5,na.rm = TRUE)
  cases[i,5]=quantile(uni$new_cases,0.75,na.rm = TRUE)
  deaths[i,5]=quantile(uni$new_deaths,0.75,na.rm = TRUE)
  cat (" 5/ Tu phan vi thu nhat theo so ca nhiem :",cases[i,3],"\n")
  cat (" 6/ Tu phan vi thu hai theo so ca nhiem :",cases[i,4],"\n")
  cat (" 7/ Tu phan vi thu ba theo so ca nhiem :",cases[i,5],"\n")
  cat (" 8/ Tu phan vi thu nhat theo so ca tu vong :",deaths[i,3],"\n")
  cat (" 9/ Tu phan vi thu hai theo so ca tu vong :",deaths[i,4],"\n")
  cat (" 10/ Tu phan vi thu ba theo so ca tu vong :",deaths[i,5],"\n")
  #Gia tri trung binh
  cases[i,7]= mean(uni$new_cases,na.rm = TRUE)
  deaths[i,7]= mean(uni$new_deaths,na.rm = TRUE)
  cat (" 11/ Gia tri trung binh theo so ca nhiem :", cases[i,7],"\n")
  cat (" 12/ Gia tri trung binh theo so ca tu vong :", deaths[i,7],"\n")
  #Gia tri do lech chuan
  cases[i,8]= sd(uni$new_cases,na.rm = TRUE)
  deaths[i,8]= sd(uni$new_deaths,na.rm = TRUE)
  cat (" 13/ Gia tri do lech chuan theo so ca nhiem :", cases[i,8],"\n")
  cat (" 14/ Gia tri do lech chuan theo so ca tu vong :", deaths[i,8],"\n")
  #So gia tri ngoai lai
  cases[i,9] = length(which(uni$new_cases < cases[i,3] - 1.5*(cases[i,5] - cases[i,3])| uni$new_cases > cases[i,5] + 1.5*(cases[i,5] - cases[i,3])))
  deaths[i,9] = length(which(uni$new_deaths < deaths[i,3] - 1.5*(deaths[i,5] - deaths[i,3])| uni$new_deaths > deaths[i,5] + 1.5*(deaths[i,5] - deaths[i,3])))
  cat (" 15/ So gia tri ngoai lai Outliers theo so ca nhiem :",cases[i,9],"\n")
  cat (" 16/ So gia tri ngoai lai Outliers theo so ca tu vong :",deaths[i,9],"\n \n")
  #Bieu do Boxplot ve so ca nhiem
  boxplot(uni$new_cases, main=name[i, 1])
}

  

  