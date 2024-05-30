
read_data<- function(current){
  setwd(current)
  data<-read.csv(file = "owid-covid-data.csv")
  data$date<- strptime(data$date, format="%m/%d/%Y" )
  return(data)
}
loc_data<-function(data){
  
  data<-data[!(data$iso_code=='' | data$continent==''),]
  return(data)
}
loc_khong_du_lieu<-function(data){
  data<-data[!(is.na(data$new_cases) & is.na(data$new_deaths)),]
  return(data)
}
data=read_data('C:\\Users\\giand\\Desktop\\stu\\ki 212\\ctrr\\btl')

#######################################
#cau 1
  #data$date = strptime(data$date, format="%m/%d/%Y" )
  #all_year<-format(data$date, format = "%Y")
  #all_year<-unique(all_year)
  
cau_1 <- function(data){
  all_year<-format(data$date, format = "%Y")
  all_year<-unique(all_year)
  cat('1) tap mau nay thu duoc du lieu tu cac nam: ',all_year)
}
cau_1(data)
#######################################
#cau 2
cau_2<- function(data){
  data<-loc_data(data)
  iso_location<<-subset(data, select=c("iso_code", "location"))
  iso_location<<-unique(iso_location)
  rownames(iso_location) <<- 1:nrow(iso_location)
  cat('2) so luong dat nuoc va dinh danh cua moi dat nuoc:\n')
  iso_location<-rbind(iso_location, data.frame(iso_code='Count',location=nrow(iso_location)))
  ans_2<-rbind(head(iso_location,10),tail(iso_location,1))
  print(ans_2,row.names=FALSE)
}
cau_2(data)
#######################################
#cau 3
cau_3<- function(data){
  data<-loc_data(data)
  ans_3<-unique(data$continent)
  ans_3<-ans_3[!ans_3== ""] 
  ans_3<-as.data.frame(ans_3)
  colnames(ans_3)[1]<-paste("Continent :",nrow(ans_3))
  print(ans_3,row.names = FALSE)
}
cau_3(data)
#######################################
#cau 4
cau_4<- function(data){
  data<-loc_data(data)
  cont_ob<<-table(data$continent)
  cont_ob <<- as.data.frame(cont_ob)
  colnames(cont_ob)[1] <<- "Continent"
  colnames(cont_ob)[2] <<- "Observations"
  cont_ob <- rbind(cont_ob, data.frame(Continent='tong', Observations = sum(cont_ob$Observations)))
  print(cont_ob,row.names = FALSE)
}
cau_4(data)
#######################################
#cau 5
cau_5<- function(data){
  data<-loc_data(data)
  iso_ob<<-table(data$iso_code)
  iso_ob<<-as.data.frame(iso_ob)
  colnames(iso_ob)[1] <<- "iso_code"
  colnames(iso_ob)[2] <<- "Observations"
  iso_ob<-rbind(iso_ob,data.frame(iso_code="tong",Observations=sum(iso_ob$Observations)))
  print(tail(iso_ob,11),row.names=FALSE)
}
cau_5(data)
#######################################
#cau 6
cau_6<-function(cont_ob){
  data<-loc_data(data)
  min_cont_ob=min(cont_ob$Observations)
  min_cont<<-as.character(cont_ob$Continent[cont_ob['Observations']==min_cont_ob])
  cat('chau luc co du lieu thu thap nho nhat la ')
  cat(min_cont)
  cat('\ndu lieu nho nhat la',min_cont_ob)
}
cau_6(cont_ob)
#######################################
#cau 7
cau_7<- function(cont_ob){
  max_cont_ob=max(cont_ob$Observations)
  #max_cont=subset(cont_ob,Observations==max_cont_ob,select='Continent')
  max_cont<<-as.character(cont_ob$Continent[cont_ob['Observations']==max_cont_ob])
  cat('chau luc co du lieu thu thap lon nhat la ')
  cat(max_cont)
  cat('\ndu lieu lon nhat la',max_cont_ob)
}
cau_7(cont_ob)
#######################################
#cau 8
cau_8<- function(iso_ob,iso_location){
  min_iso_ob=min(iso_ob$Observations)
  min_iso=as.character(iso_ob$iso_code[iso_ob['Observations']==min_iso_ob])
  min_location<-as.character(iso_location$location[iso_location$iso_code %in% min_iso])
  cat('nuoc co du lieu thu thap nho nhat la',min_location)
  cat('\ndu lieu nho nhat la',min_iso_ob)
}
cau_8(iso_ob,iso_location)
#######################################
#cau 9
cau_9<- function(iso_ob,iso_location){
  max_iso_ob=max(iso_ob$Observations)
  max_iso=as.character(iso_ob$iso_code[iso_ob['Observations']==max_iso_ob])
  max_location<-as.character(iso_location$location[iso_location$iso_code %in% max_iso])
  cat('nuoc co du lieu thu thap lon nhat la',max_location)
  cat('\ndu lieu lon nhat la',max_iso_ob)
}
cau_9(iso_ob,iso_location)
#######################################
#cau 10
cau_10<- function(data){
  date_ob<<-table(as.POSIXct(data$date))
  date_ob<<-as.data.frame(date_ob)
  colnames(date_ob)[1]<<-"date"
  colnames(date_ob)[2]<<-"observastions"
  min_date_ob=min(date_ob$observastions)
  min_date=as.character(date_ob$date[date_ob$observastions %in% min_date_ob])
  cat('cau 10) cac date co du lieu thu thap nho nhat la',min_date)
  cat('\ndu lieu nho nhat la ',min_date_ob)
}
cau_10(data)
#######################################
#cau 11
cau_11<-function(date_ob){
  max_date_ob=max(date_ob$observastions)
  max_date=as.character(date_ob$date[date_ob$observastions %in% max_date_ob])
  cat('cau 11) cac date co du lieu thu thap lon nhat la',max_date)
  cat('\ndu lieu lon nhat la ',max_date_ob)
}
cau_11(date_ob)
#######################################
#cau 12
cau_12<-function(data){
  data<-loc_data(data)
  #stall.packages("dplyr")
  library(dplyr)
  cont_date_ob<-count(data, continent, date)
  print(cont_date_ob)
}
cau_12(data)

