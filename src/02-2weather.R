

## weather 데이터 로드

## 실제 관측 데이터
a = fread("./ankus-lite-wfood_130632/weather_info1.csv")
b = fread("./ankus-lite-wfood_130632/weather_info2.csv")
wtr = rbind(a,b)


## 예측 데이터
wtr_fct = read_excel("./ankus-lite-wfood_113211/weather_forecast.xls", 1)




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
wtr
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








wtr_fct$ftime = substr(wtr_fct$ftime, 1, 11)
wtr_fct

wtr_fct$ftime = as.Date(wtr_fct$ftime, '%Y-%m-%d')
wtr_fct = wtr_fct %>% arrange(ftime) %>% as.data.table()
wtr_fct$stat %>% unique

wtr_fct_pp = wtr_fct %>% group_by(ftime, area) %>% summarise(mintemp_mean = mean(mintemp),
                                                     maxtemp_mean = mean(maxtemp))
wtr_fct_pp = as.data.table(wtr_fct_pp)
wtr_fct_pp
names(wtr_fct_pp)[1] = "mtime"



weather = full_join(wtr_pp, wtr_fct_pp)
colSums(is.na(weather))


fwrite(weather, "./preprocessing_data/weather.csv")


