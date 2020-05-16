Autko_temp %>% group_by(cylinders) %>% summarise(srednia = mean(displacement)) %>%  ggplot(aes(x = cylinders, y = as.factor(srednia))) + geom_point()
