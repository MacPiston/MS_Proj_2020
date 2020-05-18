library(ggplot2)
library(visreg)
library(knitr)
library(corrplot)
library(ggcorrplot)

ggplot(data= Autko,aes(weight,mpg)) + geom_point()+ geom_smooth(method=lm) 

ggplot(Autko,aes(displacement,mpg)) +geom_point()+geom_smooth(method=lm) 

ggplot(Autko,aes(weight, displacement)) + geom_point() +geom_smooth(method = lm)


newdata <- cor(Autko[ , c('mpg','weight', 'displacement', 'horsepower', 'acceleration')], use='complete')
corrplot(newdata, method = "number")

# Plot
ggcorrplot(newdata, hc.order = TRUE, 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Korelacja", 
           ggtheme=theme_bw)


sample = sample.split(Autko, SplitRatio = .8)
train = subset(Autko, sample == TRUE)
test = subset(Autko, sample == FALSE)


model <- lm(mpg~weight+horsepower+origin+model_year+displacement+acceleration,data = Autko)
summary(model)
plot(model)
predictions <- predict(model, newdata = test)
sqrt(mean((predictions - test$mpg)^2))

