

## goods eda

goods %>% head
goods$item %>% table()


goods2 = goods[goods$item != "",]

goods2 %>% group_by(item) %>% summarise(rec_mean = mean(as.integer(recommand))) %>% 
  ggplot(aes(x = reorder(item, rec_mean), y = rec_mean)) + geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + 
  labs(x = "아이템명", y = "평균추천수") + ggtitle("아이템별 평균 추천수") +
  theme(title = element_text(size = 15))


goods2$item %>% table() %>% sort()

goods2

goods2 %>% head



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


result=sentimental(goods$goods_review, positive, negative)

result$color[result$score >=1] = "blue"  # 긍정
result$color[result$score ==0] = "green" # 중립
result$color[result$score < 0] = "red"   # 부정


result$remark[result$score >=1] = "긍정"
result$remark[result$score ==0] = "중립"
result$remark[result$score < 0] = "부정"

sentiment_result= table(result$remark)



## 감성분석 결과를 pie 차트로
ggplot(as.data.frame(sentiment_result), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") + 
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5)) + 
  theme_void() + ggtitle("감성분석 결과") +
  theme(plot.title = element_text(size = 20))




## goods에 score 와 긍부정을 파생변수로
goods %>% dim
result$score %>% length


goods$senti_score = result$score
goods$posneg = result$remark

goods %>% str()
goods$recommand %>% table()

goods$recommand[goods$recommand == "추천안함"] = 0
goods$recommand[goods$recommand == "보통"] = 3
goods$recommand[goods$recommand == "추천"] = 4
goods$recommand[goods$recommand == "적극추천"] = 5

goods$delivery %>% table()
goods$delivery[goods$delivery == ""] = "배송보통"
#goods = goods[goods$delivery != "",]

goods$delivery_score[goods$delivery == "배송느림"] = -1
goods$delivery_score[goods$delivery == "배송보통"] = 0
goods$delivery_score[goods$delivery == "배송빠름"] = 1

goods %>% head

goods$recommand = as.integer(goods$recommand)
library(corrplot)
cor(goods[,c("recommand", "senti_score", "delivery_score")]) %>% corrplot

# 긍부정 
goods_lm = lm(goods$recommand ~ goods$senti_score + goods$delivery_score)
goods_lm %>% summary()

goods %>% head
goods %>% str()
goods$senti_score %>% table()

goods$recommand = as.character(goods$recommand)

goods %>% select(recommand) %>% table()
goods %>% select(senti_score) %>% table()

r0 = goods %>% filter(recommand == 0) %>% summarise(senti_mean = mean(senti_score), delivery_score_mean = mean(delivery_score)) 
r1 = goods %>% filter(recommand == 1) %>% summarise(senti_mean = mean(senti_score), delivery_score_mean = mean(delivery_score)) 
r2 = goods %>% filter(recommand == 2) %>% summarise(senti_mean = mean(senti_score), delivery_score_mean = mean(delivery_score)) 
r3 = goods %>% filter(recommand == 3) %>% summarise(senti_mean = mean(senti_score), delivery_score_mean = mean(delivery_score)) 
r4 = goods %>% filter(recommand == 4) %>% summarise(senti_mean = mean(senti_score), delivery_score_mean = mean(delivery_score)) 
r5 = goods %>% filter(recommand == 5) %>% summarise(senti_mean = mean(senti_score), delivery_score_mean = mean(delivery_score)) 


rec_mean = rbind(r0,r1,r2,r3,r4,r5)
rec_mean$recommand = c(0,1,2,3,4,5)
a = rec_mean$senti_mean
rec_mean$senti_mean = NULL
rec_mean$sep = "delivery_score"
names(rec_mean)[1] = "mean"

a = as.data.frame(a)
a$recommand = c(0,1,2,3,4,5)
names(a)[1] = "mean"
a
a$sep = "senti_score"

rec_mean = rbind(rec_mean, a)
ggplot(rec_mean, aes(x = recommand, y = mean, fill = sep)) + 
  geom_bar(stat = "identity", position = "dodge")


goods %>% head

goods[grep("칼몬드 100g", goods$goods_nm),]
