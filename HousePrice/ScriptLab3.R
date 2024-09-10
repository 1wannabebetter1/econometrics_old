meanX <- mean(Price$sqft_living)
meanY <- mean(Price$price)

meanX
meanY

# ------- За допомогою формул
n<-nrow(Price)
n
# Варіація Х
varX <- 0
for(i in 1:n){
  varX = varX + (Price[i,6] - meanX)^2
}
varX
varX = varX / n
varX

# Варіація У
varY <- 0
for(i in 1:n){
  varY = varY + (Price[i,3] - meanY)^2
}
varY
varY = varY / n
varY
# Коваріація
covXY <- 0
for(i in 1:n){
  covXY = covXY + (Price[i,6] - meanX)*(Price[i,3] - meanY)
}
covXY = covXY / n
covXY
# Параметр b
b <- covXY / varX
b
# Параметр a
a <- meanY - b*meanX
a
plot(Price$sqft_living, Price$price, xlab = "Площа будинку", ylab = "Ціна будинку")
curve(-43580.74 + 280.6236 * x, add = TRUE, col = 2)

lm(Price$price ~ Price$sqft_living)

#Coefficients:
#  (Intercept)  Price$sqft_living  
#-43580.7              280.6  
plot(Price$sqft_living, Price$price, xlab = "Площа будинку", ylab = "Ціна будинку")
curve(-43580.7  + 280.6  * x, add = TRUE, col = 3)
