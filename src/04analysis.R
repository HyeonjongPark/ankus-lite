

## 편의점으로 한정하고 생각해보기.

conven = wfood %>% filter(custclass == "편의점")
conven$custname %>% table() %>% length()
conven %>% as.data.table()

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
ggsave("./visualization/대구물류센터_칼몬드100G판매량추이.jpg")


conven_sp = conven %>% filter(itemname == "칼몬드 100G" & custname == "코리아세븐 대구물류센터") 

conven_sp

weather_daegu = weather %>% filter(area == "대구")
names(weather_daegu)[1] = "invoicedate"

weather_daegu$area = NULL

weather_daegu

conven_sp = left_join(conven_sp, weather_daegu, by = "invoicedate")
conven_sp %>% head

conven_sp %>% arrange(desc(qty))
conven_sp$qty %>% unique() %>% sort()

conven_sp = conven_sp %>% mutate(qty_class = ifelse(qty == 24, 1, 
                            ifelse(qty == 48, 2,
                                   ifelse(qty == 72, 3,
                                          ifelse(qty == 96, 4, 5)))))


conven_sp

conven_sp %>% group_by(qty_class) %>% summarise(mean_cur = mean(curtemp_mean))



attach(conven_sp)
lm(qty ~ isholiday + season + curtemp_mean + bodytemp_mean + humidity_mean + 
     rainfall_mean +mintemp_mean + maxtemp_mean) %>% summary()
detach(conven_sp)











kalmond


kalmond$mng_no.x = NULL
kalmond$goods_no = NULL
#kalmond$goods_review = NULL
kalmond$goods_nm

kalmond$goods_nm = gsub("\\(무료배송)", "", kalmond$goods_nm)

kalmond$goods_nm = gsub("\\[", "", kalmond$goods_nm)
kalmond$goods_nm = gsub("\\]", "", kalmond$goods_nm)

kalmond$goods_nm

kalmond$goods_count = substr(gsub("\\s", replacement = "", kalmond$goods_nm), 12,12)
kalmond$goods_count = ifelse(kalmond$goods_count == "", 1, kalmond$goods_count)

kalmond %>% str()
kalmond$goods_prc = as.numeric(kalmond$goods_prc)
kalmond$goods_count = as.numeric(kalmond$goods_count)

kalmond = kalmond %>%  transform(count_per_price = goods_prc / goods_count)

names(kalmond)[1] = "invoicedate"

kalmond_df = kalmond %>% group_by(invoicedate) %>% summarise(mean_count_per_price = mean(count_per_price),
                                                             mean_recommand = mean(as.integer(recommand)))

kalmond_df %>% ggplot(aes(x = invoicedate, y = mean_count_per_price)) + 
  geom_line(color = "red") + ggtitle("날짜별 칼몬드 100G 가격 변동") +
  labs(x = "날짜", y = "개당 가격") + 
  theme(title = element_text(size = 15))
ggsave("./visualization/날짜별 칼몬드 100G 가격 변동.jpg")

kalmond_df %>% ggplot(aes(x = invoicedate, y = mean_recommand)) + 
  geom_line(color = "blue") + ggtitle("날짜별 칼몬드 100G 추천수 변동") +
  labs(x = "날짜", y = "평균 추천수") + 
  theme(title = element_text(size = 15))
ggsave("./visualization/날짜별 칼몬드 100G 추천수 변동.jpg")




g1 = kalmond_df %>% ggplot(aes(x = invoicedate, y = scale(mean_count_per_price))) + 
  geom_line(color = "red") + ggtitle("날짜별 추천수와 가격변동") +
  labs(x = "날짜", y = "정규화된값") + 
  theme(title = element_text(size = 15))

g1 + geom_line(aes(x = invoicedate, y = scale(mean_recommand)), color = "blue")
ggsave("./visualization/날짜별 추천수와 가격변동-정규화.jpg")

M = cor(kalmond_df$mean_recommand , kalmond_df$mean_count_per_price)   # 제품가격과 만족도는 음의 상관관계가 있음
M




## 워드 클라우드


#install.packages("wordcloud")
#install.packages("NIADic")
#install.packages("qgraph")
library(KoNLP)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(NIADic)
library(tm)
library(qgraph)


kalmond_word = kalmond %>% select(cret_dt.x, goods_review)

kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "\\[옵션", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "\\]", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "색상/종류", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "1. 옵션:\\[0001", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "\\[00001", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "\\: 단일속성", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "선택:", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "[ㄱ-ㅎ]+", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "칼몬드100g x 3캔", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "1.옵션:", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "\\[0001칼몬드 100g x 3캔 ", "") 
kalmond_word$goods_review = str_replace_all(kalmond_word$goods_review, "[0-9]+", "") 


pal2 = brewer.pal(8, "Dark2")
pal = brewer.pal(12, "Set3")

pal = pal[-c(1:2)]
pal


useNIADic()
noun = sapply(kalmond_word$goods_review, extractNoun, USE.NAMES = F) %>% unlist()
noun2 = Filter(function(x){nchar(x) >= 2}, noun)

wordFreq = table(noun2)
noundata = sort(wordFreq, decreasing = TRUE, 200)
print(noundata)

png("wordcloud.png", width = 400, height = 300)
wordcloud(names(noundata), freq = noundata, min.freq = 20, random.order = T,
          rot.per = 0, col = pal)

ggsave("./visualization/칼몬드리뷰_워드클라우드.jpg")
