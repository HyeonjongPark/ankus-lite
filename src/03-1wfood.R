
## wfood eda

wfood %>% dim()

wfood$season = NA 
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("06","07","08")]  = "summer"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("09","10")]  = "fall"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("11","12","01","02")]  = "winter"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("03","04","05")]  = "spring"

wfood %>% head
wfood$item %>% table
wfood$companyname %>% table()

#wfood %>% mutate(season = ifelse(substr(invoicedate, 6, 7) %in% c("06","07","08") , "summer",
#                 ifelse(substr(invoicedate, 6, 7) %in% c("09","10"), "fall",
#                        ifelse(substr(invoicedate, 6, 7) %in% c("11","12","01","02"), "winter",
#                               ifelse(substr(invoicedate, 6, 7) %in% c("03","04","05"), "spring")))))


wfood %>% head

# 업종별 빈도
wfood$custclass %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "custclass", y = "빈도") + ggtitle("업종별 빈도") +
  theme(title = element_text(size = 15))


# 계절별 판매량
wfood %>% group_by(season) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = season, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "계절", y = "평균판매량") + ggtitle("계절별 판매량") +
  theme(title = element_text(size = 15))


# 아이템별 판매량
wfood %>% group_by(item) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = item, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "아이템", y = "평균판매량") + ggtitle("아이템별 판매량") +
  theme(title = element_text(size = 15))


# 회사별 판매량
wfood %>% group_by(companyname) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = companyname, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "회사명", y = "평균판매량") + ggtitle("회사별 판매량") +
  theme(title = element_text(size = 15))


# 공휴일 유무에 따른 판매량
wfood %>% 
  group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
  theme(title = element_text(size = 15))



wfood %>% head
dim(wfood)
wfood$custname %>% unique()















## 편의점으로 한정하고 생각해보기.

conven = wfood %>% filter(custclass == "편의점")
conven$custname %>% table() %>% length()



# 계절별 판매량
conven %>% group_by(season) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = season, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "계절", y = "평균판매량") + ggtitle("계절별 판매량") +
  theme(title = element_text(size = 15))

# 아이템별 판매량
conven %>% group_by(item) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = item, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "아이템", y = "평균판매량") + ggtitle("아이템별 판매량") +
  theme(title = element_text(size = 15))
table(conven$item)

# 회사별 판매량
conven %>% group_by(companyname) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = companyname, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "회사명", y = "평균판매량") + ggtitle("회사별 판매량") +
  theme(title = element_text(size = 15))


# 공휴일 유무에 따른 판매량
conven %>% 
  group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
  theme(title = element_text(size = 15))



conven %>% head
table(conven$itemname) %>% sort()

conven %>% filter(itemname == "칼몬드 100G") %>% tail(30)

conven %>% filter(itemname == "칼몬드 100G" & custname == "코리아세븐 대구물류센터") %>% 
  ggplot(aes(x =invoicedate , y = qty)) + geom_line(color = "orange") + labs(x = "date", y = "판매량") +
  ggtitle("코리아세븐 대구물류센터 - 칼몬드 100G")





conven_sp = conven %>% filter(itemname == "칼몬드 100G" & custname == "코리아세븐 대구물류센터") 

conven_sp
names(weather_daegu)[1] = "invoicedate"
weather_daegu

conven_sp = left_join(conven_sp, weather_daegu, by = "invoicedate")
conven_sp

attach(conven_sp)
lm(qty ~ isholiday + season + curtemp_mean + bodytemp_mean + humidity_mean + rainfall_mean +
     mintemp_mean + maxtemp_mean) %>% summary()
detach(conven_sp)


conven_sp %>% head




conven$itemname %>% unique()
