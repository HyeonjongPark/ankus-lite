
## wfood 로드

getwd()
wfood = data.frame()
for(i in 1:11) {
  a = read_excel("./ankus-lite-wfood_110718/wfood_salsdb.xls", i)
  a = as.data.frame(a)
  wfood = rbind(wfood, a)
  print(i)
}
b = read_excel("./ankus-lite-wfood_110718/wfood_salsdb(1).xls", 1)
b = as.data.frame(b)
wfood = rbind(wfood, b)

for(i in 1:3) {
  a = read_excel("./ankus-lite-wfood_110718/wfood_salsdb(2).xls", i)
  a = as.data.frame(a)
  wfood = rbind(wfood, a)
  print(i)
}


wfood = as.data.table(wfood)
wfood

wfood$invoicedate = as.Date(wfood$invoicedate, '%Y%m%d') 
wfood = wfood %>% arrange(invoicedate) %>% as.data.table()

wfood
wfood$itemname %>% unique

colSums(is.na(wfood))


wfood


pub_hol = read_excel("./ankus-lite-wfood_113211/pubholiday.xls", 1)
pub_hol = as.data.table(pub_hol)

pub_hol$locdate = as.Date(pub_hol$locdate, "%Y%m%d")
names(pub_hol)[1] = "invoicedate"
pub_hol

wfood = full_join(wfood, pub_hol)

wfood$isholiday[is.na(wfood$isholiday)] = "N"

wfood
head(wfood)
table(wfood$isholiday)

wfood %>% as.data.table()

wfood = wfood %>% arrange(invoicedate) %>% as.data.table()

#fwrite(wfood, "./preprocessing_data/wfood.csv")
