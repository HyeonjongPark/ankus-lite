
## wfood eda

wfood %>% dim()
wfood %>% as.data.table()

wfood %>% head

# 업종별 빈도
wfood$custclass %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "custclass", y = "빈도") + ggtitle("업종별 빈도") +
  theme(title = element_text(size = 15))

ggsave("./visualization/업종별빈도.jpg")

# 계절별 판매량
wfood %>% group_by(season) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = season, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "계절", y = "평균판매량") + ggtitle("계절별 판매량") +
  theme(title = element_text(size = 15))
ggsave("./visualization/계절별판매량.jpg")

# 아이템별 판매량
wfood %>% group_by(item) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = item, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "아이템", y = "평균판매량") + ggtitle("아이템별 판매량") +
  theme(title = element_text(size = 15))
ggsave("./visualization/아이템별 판매량.jpg")


# 회사별 판매량
wfood %>% group_by(companyname) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = companyname, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "회사명", y = "평균판매량") + ggtitle("회사별 판매량") +
  theme(title = element_text(size = 15))
ggsave("./visualization/회사별판매량.jpg")


# 공휴일 유무에 따른 판매량
wfood %>% 
  group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
  theme(title = element_text(size = 15))
ggsave("./visualization/공휴일 유무에 따른 판매량.jpg")


wfood %>% head
dim(wfood)
wfood$custname %>% unique()














