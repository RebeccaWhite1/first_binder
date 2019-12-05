set.seed(1234)

a <- seq(1:20)
b <- a + runif(20, 0, .25)

my_data <- as.data.frame(c(a, b))

my_model <- lm(a ~ b, data  = my_data)

summary(my_model)
