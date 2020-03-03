
wfood
gds_dtl
gds_rvw
wtr
wtr_fct
pub_hol




wfood %>% head

summary(wfood)
colSums(is.na(wfood))
str(wfood)

wfood$invoicedate %>% max
wfood$invoicedate %>% min



wfood$custclass %>% unique

ggplot(wfood, aes(x = custclass, y = qty)) + 
  geom_bar(stat = "identity", fill = "lightblue", colour = "black")
