

## weather eda
weather = mutate(weather, month = substr(mtime, 6, 7))
colSums(is.na(weather))

weather %>% head

weather %>% 
  ggplot(aes(x = mtime, y = curtemp_mean)) +
  geom_line()

weather %>% filter(area == "강릉") %>% 
  ggplot(aes(x = mtime, y = mintemp_mean)) +
  geom_line()

weather %>% filter(area == "서울") %>% 
  ggplot(aes(x = mtime, y = maxtemp_mean)) +
  geom_line()




