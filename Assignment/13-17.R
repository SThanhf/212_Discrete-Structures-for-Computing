#cac thong so va bang du lieu lay tu file i.1-12
#cau 13
as.data.frame(cont_date_ob,row.names = NULL,optional=FALSE)
cat("So luong du lieu lon nhat thu duoc theo date va chau luc la :", max (cont_date_ob[,3],na.rm=TRUE),"\n")
#cau 14
cat("So luong du lieu nho nhat thu duoc theo date va chau luc la :", min (cont_date_ob[,3],na.rm=TRUE),"\n")
#cau 15
k=readline("Nhap date k (luu y dung dinh dang) : ")
t=readline("Nhap chau luc t : ")
cau_15<-filter(cont_date_ob,date==k& continent==t)
#cau 16
coun_data= count(owid_covid_data,iso_code)
no_rep= count (coun_data,n)
colnames(no_rep)= c("n","CR")
cau_16= merge(coun_data,no_rep,by="n")
cau_16=filter (cau_16,CR!=1)
cau_16= cau_16[-3]
#cau 17
cau_17=iso_location
cau_17$iso_length=nchar(cau_17[,1])
cau_17 = filter (cau_17,iso_length!=3 & iso_code!="Count")