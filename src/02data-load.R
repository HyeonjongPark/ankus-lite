


wfood = fread("./preprocessing_data/wfood.csv", encoding = "UTF-8")
weather = fread("./preprocessing_data/weather.csv", encoding = "UTF-8")
goods = fread("./preprocessing_data/goods.csv", encoding = "UTF-8")

wfood %>% str()
weather %>% str()
goods %>% str()

wfood$invoicedate = as.Date(wfood$invoicedate, '%Y-%m-%d')
weather$mtime = as.Date(weather$mtime, '%Y-%m-%d')
goods$cret_dt.x = as.Date(goods$cret_dt.x, '%Y-%m-%d')

wfood %>% dim()
weather %>% dim()
goods %>% dim()


# 기상데이터 2018 01 ~ 05월까지 결측데이터가 존재. => 201805~ 201905 로 한정시간 변경

wfood = wfood %>% filter(invoicedate >= "2018-05-29" & invoicedate <= "2019-05-09")
wfood[wfood$invoicedate == "2019-01-01",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-02-04",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-02-05",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-02-06",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-03-01",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-05-05",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-05-12",]$isholiday = "Y"

as.data.table(wfood)

weather = weather %>% filter(mtime >= "2018-05-29" & mtime <= "2019-05-09")
goods = goods %>% filter(cret_dt.x >= "2018-05-29" & cret_dt.x <= "2019-05-09")

colSums(is.na(wfood));wfood = wfood[is.na(wfood$qty) == FALSE,]
colSums(is.na(weather))
colSums(is.na(goods));goods$mng_no.y = NULL




