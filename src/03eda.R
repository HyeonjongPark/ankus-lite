

## wfood eda

wfood %>% dim()

wfood$season = NA 
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("06","07","08")]  = "summer"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("09","10")]  = "fall"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("11","12","01","02")]  = "winter"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("03","04","05")]  = "spring"

wfood

#wfood %>% mutate(season = ifelse(substr(invoicedate, 6, 7) %in% c("06","07","08") , "summer",
#                 ifelse(substr(invoicedate, 6, 7) %in% c("09","10"), "fall",
#                        ifelse(substr(invoicedate, 6, 7) %in% c("11","12","01","02"), "winter",
#                               ifelse(substr(invoicedate, 6, 7) %in% c("03","04","05"), "spring")))))




wfood %>% group_by(season) %>% summarise(mean_qty = mean(qty))







## weather eda
weather = mutate(weather, month = substr(mtime, 6, 7))
colSums(is.na(weather))



weather %>% 
  ggplot(aes(x = mtime, y = curtemp_mean)) +
  geom_line()

weather %>% filter(area == "강릉") %>% 
  ggplot(aes(x = mtime, y = mintemp_mean)) +
  geom_line()

weather %>% filter(area == "서울") %>% 
  ggplot(aes(x = mtime, y = maxtemp_mean)) +
  geom_line()

