

## weather eda
weather = mutate(weather, month = substr(mtime, 6, 7))
colSums(is.na(weather))

weather %>% as.data.table()
weather$area %>% unique()

weather %>% filter(area == "서울") %>% 
  ggplot(aes(x = mtime, y = curtemp_mean)) +
  geom_line()
ggsave("./visualization/서울실제온도.jpg")


weather %>% filter(area == "서울") %>% 
  ggplot(aes(x = mtime, y = mintemp_mean)) +
  geom_line()
ggsave("./visualization/서울예측온도min.jpg")

weather %>% filter(area == "서울") %>% 
  ggplot(aes(x = mtime, y = maxtemp_mean)) +
  geom_line()
ggsave("./visualization/서울실제온도max.jpg")



weather_daegu = weather %>% filter(area == "대구")
weather_daegu$area = NULL
