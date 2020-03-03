


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
gds_dtl %>% as.data.table()

gds_rvw$recommand %>% unique()
gds_rvw %>% as.data.table()


gds = left_join(gds_rvw, gds_dtl, by = c("goods_no", "goods_no"))
gds = as.data.table(gds)
gds$goods_url =NULL
gds$writer = NULL
gds$review_no = NULL
gds$purch_de = NULL
gds$last_updt_dt.x %>% unique()
gds$last_updt_dt.x = NULL
gds$last_updt_dt.y %>% unique()
gds$last_updt_dt.y = NULL
gds$delivery %>% table()
gds$goods_option = NULL
gds$goods_sttus =NULL
gds$orgnp = NULL
gds$brand = NULL
gds$prdnm_modlnm %>% unique()
gds$prdnm_modlnm = NULL
gds$cttpc %>% unique()
gds$cttpc =NULL
gds$cret_dt.y = NULL
gds$shopng_knd = NULL
gds$seler_nm = NULL

colSums(is.na(gds))
gds



gds$cret_dt.x = substr(gds$cret_dt.x, 1, 11)
gds$cret_dt.x = as.Date(gds$cret_dt.x, '%Y-%m-%d')
gds

gds = gds %>% arrange(cret_dt.x) %>% as.data.table()
gds

getwd()
fwrite(gds, "./preprocessing_data/goods.csv")

