


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


gds_dtl$item %>% unique
gds_dtl

gds_rvw$recommand %>% unique()
gds_rvw


gds = left_join(gds_dtl, gds_rvw, by = c("goods_no", "goods_no"))
gds
