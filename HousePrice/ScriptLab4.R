meanX <- mean(Price$sqft_living)
meanY <- mean(Price$price)
# Визначити середнє x
meanX
# Визначити середнє y
meanY


sd_X <- sd(Price$sqft_living)  
# Визначити стандартне відхилення x
sd_X

sd_Y <- sd(Price$price) 
# Визначити стандартне відхилення y
sd_Y

quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9) 
quant_X <- quantile(Price$sqft_living, quantiles) 
quant_Y <- quantile(Price$price, quantiles) 

# зібрати все в data.frame 
DistributionSummary <- data.frame(Average = c(meanX, meanY),  
                                  StandardDeviation = c(sd_X, sd_Y),  
                                  quantile = rbind(quant_X, quant_Y)) 

# вивести резюме на консоль 
DistributionSummary 

cor(Price$sqft_living, Price$price) 


linear_model <- lm(price ~ sqft_living, data = Price)
mod_summary <- summary(linear_model)

mod_summary

SSR <- sum(mod_summary$residuals^2)
TSS <- sum((Price$price - meanY)^2)
R2 <- 1 - SSR/TSS
# вивести значення на консоль
R2


# обчислити SER вручну
n <- nrow(Price)
SER <- sqrt(SSR / (n-2))

# вивести значення на консоль
SER
#(A)Визначте стандартну похибку для оцінених коефіцієнтів
#Std. Error (Intercept) 4402.690
#Std. Error sqft_living  1.936

#(B) Визначте для вашого набору чому дорівнює 𝑡кр


qt(p=.05, df=21611, lower.tail=TRUE)

#(C) Визначте t-статистику для оцінених коефіцієнтів 𝛽0 та 𝛽1 та виконайте аналіз чи  приймається 𝐻0 чи приймається альтернативна 𝐻1


#(D) Перевірте виконання гіпотези 𝐻0: 𝛽0 = 0 або підтвердження виконання альтернативної;


#(E) Перевірте виконання гіпотези 𝐻0: 𝛽1 = 0 або підтвердження виконання альтернативної;

#(F) Визначте ступені вільності для вашого набору даних;
#21611 degrees of freedom
linear_model$df.residual
#(G) Визначте Pr(>|t|) для оцінених коефіцієнтів
#(Intercept)   <2e-16 ***
#sqft_living   <2e-16 ***
2*pt(144.920, df = 21611) # Pr(>|t|) для beta1
2 * pt(-9.899, df = 21611) # Pr(>|t|) для beta0

pt(144.920, df = 21611,lower.tail = TRUE)
#(H) Обчисліть 95% довірчий інтервал для коефіцієнтів
confint(linear_model)
lm_summ <- summary(linear_model)

c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2],
  "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])



##############

# Створіть фіктивну змінну, як визначено 
Price$D <- Price$sqft_living < 2079.9


plot(price ~ sqft_living, 
     data = Price,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

# Побудуйте дані
plot(Price$D, Price$price,            # надати дані для побудови графіка
     pch = 20,                                # використовувати заповнені кружечки як символи сюжету
     cex = 0.5,                               # встановити розмір символів графіка на 0,5
     col = "Steelblue",                       # встановити колір символів на "Steelblue"
     xlab = expression(D[i]),                 # Встановіть назву та назви осі
     ylab = "Price",
     main = "Dummy Regression")

# оцінити фіктивну регресійну модель
dummy_model <- lm(price ~ D, data = Price)
summary(dummy_model)

# додайте до сюжету прогнози для певної групи
points(x = Price$D, 
       y = predict(dummy_model), 
       col = "red", 
       pch = 20)
# довірчі інтервали для коефіцієнтів у моделі фіктивної регресії
confint(dummy_model)

#Завдання 4: Дослідження на гомоскедастичність чи гетероскедастичність.
#(A) Скажіть, дивлячись на діаграму розсіювання, ваше початкове розсіювання гомоскедастичне чи гетероскедастичне;



#(B) Обчислити стандартні помилки коефіцієнтів, як для гетероскедастичного розсіювання
#за допомогою vcovHC() та sqrt(diag()), та порівняти з аналогічним обчисленням за 
#допомогою coeftest( *, vcov. = vcov);