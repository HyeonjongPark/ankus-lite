

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
wfood %>% filter(invoicedate <= "2018-12-30") %>% 
  group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
  theme(title = element_text(size = 15))












## weather eda
weather = mutate(weather, month = substr(mtime, 6, 7))
colSums(is.na(weather))

weather %>% head

weather %>% 
  ggplot(aes(x = mtime, y = curtemp_mean)) +
  geom_line()

weather %>% filter(area == "강릉") %>% 
  ggplot(aes(x = mtime, y = mintemp_mean)) +
  geom_line()

weather %>% filter(area == "서울") %>% 
  ggplot(aes(x = mtime, y = maxtemp_mean)) +
  geom_line()







## goods eda

goods %>% head
goods$item %>% table()


goods2 = goods[goods$item != "",]

goods2 %>% group_by(item) %>% summarise(rec_mean = mean(as.integer(recommand))) %>% 
  ggplot(aes(x = reorder(item, rec_mean), y = rec_mean)) + geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + 
  labs(x = "아이템명", y = "평균추천수") + ggtitle("아이템별 평균 추천수") +
  theme(title = element_text(size = 15))




