library(tidyverse)
library(Hmisc)

crime <- read_csv("https://bit.ly/2Z5zQlY")
head(crime)

crime <- separate(crime, 'City, State', into=c("City", "State"))
head(crime)

colnames(crime)[2] <- "House_price"
colnames(crime)[6] <- "Violent_Crimes"
head(crime)

crime %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) + 
  geom_point() + 
  geom_smooth(method = "lm")

rcorr(crime$Population, crime$Violent_Crimes)

crime_filtered <- filter(crime, Population < 2000000)

crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) + 
  geom_point() + 
  geom_smooth(method = "lm")

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) + 
  geom_point() + 
  geom_text(nudge_y = 500, check_overlap = TRUE) + 
  geom_smooth(method = "lm") + 
  xlim(0,1800000)

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)

model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered)
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)
anova(model1, model2)

summary(model2)

#Population size and robberies 
crime %>%
  ggplot(aes(x = Population, y = Robberies)) + 
  geom_point() + 
  geom_smooth(method = "lm")

rcorr(crime$Population, crime$Robberies)
crime_filtered <- filter(crime, Population < 2000000)
crime_filtered %>%
  ggplot(aes(x = Population, y = Robberies)) + 
  geom_point() + 
  geom_smooth(method = "lm")
rcorr(crime_filtered$Population, crime_filtered$Robberies)
crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>%
  ggplot(aes(x = Population, y = Robberies, label = City)) + 
  geom_point() + 
  geom_text(nudge_y = 500, check_overlap = TRUE) + 
  geom_smooth(method = "lm") + 
  xlim(0,1800000)
model1 <- lm(Robberies ~ 1, data = crime_filtered)
model2 <- lm(Robberies ~ Population, data = crime_filtered)

anova(model1, model2)
summary(model2)

crime %>%
  ggplot(aes(x = House_price, y = Violent_Crimes)) + 
  geom_point() + 
  geom_smooth(method = "lm")
rcorr(crime$House_price, crime$Violent_Crimes)
crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>%
  ggplot(aes(x = House_price, y = Violent_Crimes, label = City)) + 
  geom_point() + 
  geom_text( check_overlap = TRUE) + 
  geom_smooth(method = "lm")
rcorr(crime_filtered$House_price, crime_filtered$Violent_Crimes)

#Are house prices predicted by population size in 2015?
crime %>%
  ggplot(aes(x = Population, y = House_price)) + 
  geom_point() + 
  geom_smooth(method = "lm")
rcorr(crime$Population, Crime$House_prices)
rcorr(crime$Population, crime$House_price)
