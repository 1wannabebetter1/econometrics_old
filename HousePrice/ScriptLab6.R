# x - sqft_living
#Linear
linear_model<- lm(price~sqft_living, data = Price)
plot(Price$sqft_living, Price$price, xlab = "Площа будинку", ylab = "Ціна будинку")
curve(-43580.74 + 280.6236 * x, add = TRUE, col = 2)
summary(linear_model)

# y = b0*(b1^x)
# log y = log b0 + x* log b1

# y = b0*(e^(b1*X))
# log y = log b0 + b1*x
Price$priceX10k <- Price$price/10000


# y = e^(b0+b1*x)
exp_model <- lm(I(log(Price$price))~sqft_living, data = Price)
summary(exp_model)
# y = b0*x^(b1)
# log y = log b0 + b1* log x
loglog_model <- lm(I(log(price))~I(log(sqft_living)), data = Price)
summary(loglog_model)
# y = b0+ b1*(1/x)
reverse_model <- lm(price~I(1/sqft_living), data = Price)
summary(reverse_model)
#y=b0+b1*x+b2*(x^2)
quadratic_model <- lm(price~sqft_living+I(sqft_living^2), data = Price)
summary(quadratic_model)
library(AES)
library(MASS)
coefint(quadratic_model)

new_data<-data.frame(sqft_living=c(1000, 1001))
Y_hat <- predict(loglog_model, newdata = new_data)
diff(Y_hat)



new_data2<-data.frame(sqft_living=c(10000, 10001))
Y_hat <- predict(loglog_model, newdata = new_data2)
diff(Y_hat)

