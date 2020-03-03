library(dplyr)
library(tidyverse)
library(data.table)
library(readxl)
library(xlsx)
library(lubridate)

## wfood 로드

getwd()
wfood = data.frame()
for(i in 1:11) {
  a = read_excel("./ankus-lite-wfood_110718/wfood_salsdb.xls", i)
  a = as.data.frame(a)
  wfood = rbind(wfood, a)
  print(i)
}
b = read_excel("./ankus-lite-wfood_110718/wfood_salsdb(1).xls", 1)
b = as.data.frame(b)
wfood = rbind(wfood, b)

for(i in 1:3) {
  a = read_excel("./ankus-lite-wfood_110718/wfood_salsdb(2).xls", i)
  a = as.data.frame(a)
  wfood = rbind(wfood, a)
  print(i)
}




## weather 데이터 로드

a = fread("./ankus-lite-wfood_130632/weather_info1.csv")
b = fread("./ankus-lite-wfood_130632/weather_info2.csv")
wtr = rbind(a,b)

wtr_fct = read_excel("./ankus-lite-wfood_113211/weather_forecast.xls", 1)






## 상품 상세 데이터 로드
gds_dtl = read_excel("./ankus-lite-wfood_113211/shopng_goods_dtls_new.xls", 1)
gds_dtl =as.data.frame(gds_dtl)

gds_rvw = data.frame()
for(i in 1:5){
  a = read_excel("./ankus-lite-wfood_113211/shopng_goods_review.xls", i)
  a = as.data.frame(a)
  gds_rvw = rbind(gds_rvw, a)
  print(i)
}




## 법정 공휴일 데이터 로드

pub_hol = read_excel("./ankus-lite-wfood_113211/pubholiday.xls", 1)




wfood = as.data.table(wfood)
gds_dtl = as.data.table(gds_dtl)
gds_rvw = as.data.table(gds_rvw)
wtr = as.data.table(wtr)
wtr_fct = as.data.table(wtr_fct)
pub_hol = as.data.table(pub_hol)

wfood




wfood$invoicedate = as.Date(wfood$invoicedate, '%Y%m%d') 
wfood = wfood %>% arrange(invoicedate) %>% as.data.table()

pub_hol$locdate = as.Date(pub_hol$locdate, '%Y%m%d')
pub_hol = pub_hol %>% arrange(locdate) %>% as.data.table()




wtr$mtime = substr(wtr$mtime, 1, 11)

wtr$mtime = str_replace(wtr$mtime, "Jan", "01")
wtr$mtime = str_replace(wtr$mtime, "Feb", "02")
wtr$mtime = str_replace(wtr$mtime, "Mar", "03")
wtr$mtime = str_replace(wtr$mtime, "Apr", "04")
wtr$mtime = str_replace(wtr$mtime, "May", "05")
wtr$mtime = str_replace(wtr$mtime, "Jun", "06")
wtr$mtime = str_replace(wtr$mtime, "Jul", "07")
wtr$mtime = str_replace(wtr$mtime, "Aug", "08")
wtr$mtime = str_replace(wtr$mtime, "Sep", "09")
wtr$mtime = str_replace(wtr$mtime, "Oct", "10")
wtr$mtime = str_replace(wtr$mtime, "Nov", "11")
wtr$mtime = str_replace(wtr$mtime, "Dec", "12")


wtr$mtime = as.Date(wtr$mtime, '%Y-%m-%d')

wtr = wtr %>% arrange(mtime) %>% as.data.table()

wtr$rainfall = ifelse(wtr$rainfall == "NULL", 0, wtr$rainfall)
wtr$rainfall = as.double(wtr$rainfall)
wtr$stat = ifelse(wtr$stat == "", "미측정", wtr$stat)

wtr %>% str


colSums(is.na(wtr))

wtr_pp = wtr %>% group_by(mtime, area) %>% summarise(curtemp_mean = mean(curtemp),
                                            bodytemp_mean = mean(bodytemp),
                                            humidity_mean = mean(humidity),
                                            rainfall_mean = mean(rainfall))

wtr_pp = wtr_pp %>% as.data.table()
wtr_pp






