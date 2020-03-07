
library(tidyverse)
library(data.table)
library(readxl)
library(lubridate)




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


gds_dtl %>% as.data.table()
gds_dtl$item %>% unique
gds_dtl %>% as.data.table()
gds_dtl$orgnp

gds_rvw$recommand %>% unique()
gds_rvw %>% as.data.table()


gds = left_join(gds_rvw, gds_dtl, by = c("goods_no", "goods_no"))
gds = as.data.table(gds)
gds %>% head

gds$item %>% table()



gds$goods_url =NULL
gds$writer = NULL
gds$review_no = NULL
gds$purch_de = NULL
#gds$last_updt_dt.x %>% unique()
gds$last_updt_dt.x = NULL
#gds$last_updt_dt.y %>% unique()
gds$last_updt_dt.y = NULL
#gds$delivery %>% table()
gds$goods_option = NULL
gds$goods_sttus =NULL
gds$orgnp = NULL
gds$brand = NULL
#gds$prdnm_modlnm %>% unique()
gds$prdnm_modlnm = NULL
#gds$cttpc %>% unique()
gds$cttpc =NULL
gds$cret_dt.y = NULL
gds$shopng_knd = NULL
gds$seler_nm = NULL

colSums(is.na(gds))

gds$invoicedate = substr(gds$cret_dt.x, 1, 10)
gds$invoicedate = as.Date(gds$invoicedate, "%Y-%m-%d")
gds = gds %>% arrange(invoicedate)
gds$yearmonth = substr(gds$invoicedate, 1, 7)
gds$month = substr(gds$invoicedate, 6, 7)

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
gds$weekday = factor(weekdays((gds$invoicedate)), levels=day_levels, ordered=TRUE)

gds$recommand %>% table()
colSums(is.na(gds))

gds = gds[is.na(gds$recommand) == FALSE,]
gds$recommand[gds$recommand == "추천안함"] = 1
gds$recommand[gds$recommand == "보통"] = 3
gds$recommand[gds$recommand == "추천"] = 4
gds$recommand[gds$recommand == "적극추천"] = 5

gds$recommand = as.integer(gds$recommand)

gds %>% str()


## 감성분석

positive <- readLines("./original-data/positive.txt", encoding = "UTF-8")
negative <- readLines("./original-data/negative.txt", encoding = "UTF-8")



library(plyr)
sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


result=sentimental(gds$goods_review, positive, negative)

result$remark[result$score >=1] = "긍정"
result$remark[result$score ==0] = "중립"
result$remark[result$score < 0] = "부정"

table(result$remark)

gds$remark = result$remark
gds$score = result$score





## 20203037


a = gds %>% group_by(yearmonth) %>%
  summarise(mean_score = mean(score), mean_recommand = mean(recommand)) %>%
  as.data.frame()
a$mean_score = scale(a$mean_score)
a$mean_recommand = scale(a$mean_recommand)

cor(a[,-1])
p = a %>% 
  ggplot(aes(x = yearmonth, y = mean_score, group = 1)) + geom_line(color = "blue")
p + geom_line(aes(x = yearmonth, y = mean_recommand), color = "red")


gds_nut = gds %>% filter(item == "너트류")

gds_nut %>% as.data.table()
gds_nut %>% arrange(cret_dt.x) %>% as.data.table()


a = gds_nut %>% group_by(yearmonth) %>%
  summarise(mean_score = mean(score), mean_recommand = mean(recommand)) %>%
  as.data.frame()
a$mean_score = scale(a$mean_score)
a$mean_recommand = scale(a$mean_recommand)

cor(a[,-1])
p = a %>% 
  ggplot(aes(x = yearmonth, y = mean_score, group = 1)) + geom_line(color = "blue")
p + geom_line(aes(x = yearmonth, y = mean_recommand), color = "red")




gds$cret_dt.x = substr(gds$cret_dt.x, 1, 11)
gds$cret_dt.x = as.Date(gds$cret_dt.x, '%Y-%m-%d')
gds

gds = gds %>% arrange(cret_dt.x) %>% as.data.table()
colnames(gds)

getwd()
fwrite(gds, "./preprocessing_data/goods.csv")

