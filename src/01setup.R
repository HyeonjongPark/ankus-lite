#install.packages("RJDBC")
#install.packages("RMySQL")
options(java.parameters = "-Xmx14g") # 메모리를 14G 까지 쓸 수 있도록 설정

library(DBI)
library(rJava)
library(RJDBC)
library(RMySQL)
library(dplyr)


# db 접속 정보
con <- DBI::dbConnect(
  RMySQL::MySQL(),
  user = "dba",
  password = "djslzja",
  dbname = "ankus-lite-wfood",
  # host = "localhost",
  host = "onycom.iptime.org",
  port = 40000
)

# 컬럼 목록
dbListTables(con)


# 한글 인코딩 문제 해결 및 데이터 프레임 생성
dbSendQuery(con, "SET NAMES utf8;") 
dbSendQuery(con, "SET CHARACTER SET utf8;") 
dbSendQuery(con, "SET character_set_connection=utf8;")
get <-dbGetQuery(con, "select * from MALL_DNW_DATA")
Encoding(get[,3]) <- 'UTF-8'
Encoding(get[,4]) <- 'UTF-8'
get %>% head()

nrow(get)
head(get)
tail(get)
str(get)

data = get

data$RDATE = as.Date(data$RDATE)

data_interpark = data %>% filter(STORE == "인터파크")

nrow(data_interpark)
head(data_interpark,100)
tail(data_interpark)
str(data_interpark)

data_interpark = data_interpark %>% arrange(RDATE,PNAME)
length(unique(data_interpark$PNAME))
head(data_interpark)
tail(data_interpark)




mirror_case = data_interpark %>% filter(PNAME == "1988y 갤럭시노트4 진주귀걸이를 한 소녀 미러 케이스")
mirror_case_price = mirror_case %>% select(SPRICE)
mirror_case_price_ts = ts(mirror_case_price)
plot.ts(mirror_case_price_ts)






get2 = dbGetQuery(con, "select * from BLOG_DATA limit 10")


colnames(get2) %>% length()


Encoding(get2) = "UTF-8"
str(get2)
class(get2)

library(readr)
encoding(get2$RNO)
guess_encoding(get2) 

get2

guess_encoding(get2)
!guess_encoding(get2[,1])[1,1] == "UTF-8"

for(i in 1:length(get2)) {
  Encoding(get2[,i]) = "UTF-8"
}


Encoding(get2[,2]) = "UTF-8"
Encoding(get2[,3]) = "UTF-8"
Encoding(get2[,4]) = "UTF-8"
Encoding(get2[,6]) = "UTF-8"
get2 %>% head



