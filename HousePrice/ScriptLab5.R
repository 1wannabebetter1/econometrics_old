############################################################
#lab5
#Завдання 1: Перевірка виконання припущень для МНК.
#Для факторів 𝑥1, 𝑥2, 𝑥3, 𝑥4, 𝑥5 та 𝑦 побудувати в 𝑅 модель 𝑚𝑖: lm(𝑦~𝑥𝑖) та виконати наступні завдання (A)-(L)

y <- Price$price
x1 <- Price$sqft_living
x2 <- Price$sqft_lot
x3 <- Price$sqft_living15
x4 <- Price$bathroomsFixed
x5 <- Price$bedroomsFixed




library(MASS)


options(scipen = 100)



#(A) Побудувати діаграму розсіювання plot(*$𝑥𝑖, *$y) та накласти регресійну лінію
#(B) Перевірити значення *$r.squared та зробити висновки;
#(C) Перевірити sum(*$residuals^2) та зробити висновки
#(D) Обчислити var(*$𝑥𝑖);
#(E) Обчислити var(*$y);
#(F) Побудувати hist(*$𝑥𝑖)
#(G) Побудувати hist(*$y)
#(H) Побудувати plot(*$residuals) зробити припущення чи відповідає 𝑁(0; 1);
#(I) Перевірити mean(*$residuals);
#(J) Обчислити var(*$residuals);
#(K) Побудувати hist(*$residuals) та перевірити чи відповідає 𝑁(0; 1);



########################################################
m1 <- lm(Price$price ~ Price$sqft_living, data = Price)

plot(Price$sqft_living, Price$price, xlab="sqft_living", ylab = "price")
abline(coef = m1$coefficients, col=2)

summ1 <- summary(m1)
summary(m1)

summ1$r.squared #!
sum(summ1$residuals^2)
var(Price$sqft_living)
var(Price$price)
plot(summ1$residuals)
mean(summ1$residuals)
var(summ1$residuals)
hist(summ1$residuals)

###########################################################
m2 <- lm(Price$price ~ Price$sqft_lot, data = Price)

plot(Price$sqft_lot, Price$price, xlab="sqft_lot", ylab = "price")
abline(coef = m2$coefficients, col=2)

summ2 <- summary(m2)
summary(m2)

summ2$r.squared #!
sum(summ2$residuals^2)
var(Price$sqft_lot)
var(Price$price)
plot(summ2$residuals)
mean(summ2$residuals)
var(summ2$residuals)
hist(summ2$residuals)
###########################################################
m3 <- lm(Price$price ~ Price$sqft_living15, data = Price)

plot(Price$sqft_living15, Price$price, xlab="sqft_living15", ylab = "price")
abline(coef = m3$coefficients, col=2)

summ3 <- summary(m3)
summary(m3)

summ3$r.squared #!
sum(summ3$residuals^2)
var(Price$sqft_living15)
var(Price$price)
plot(summ3$residuals)
mean(summ3$residuals)
var(summ3$residuals)
hist(summ3$residuals)

###########################################################
m4 <- lm(Price$price ~ Price$bathroomsFixed, data = Price)

plot(Price$bathroomsFixed, Price$price, xlab="bathroomsFixed", ylab = "price")
abline(coef = m4$coefficients, col=2)

summ4 <- summary(m4)
summary(m4)

summ4$r.squared #!
sum(summ4$residuals^2)
var(Price$bathroomsFixed)
var(Price$price)
plot(summ4$residuals)
mean(summ4$residuals)
var(summ4$residuals)
hist(summ4$residuals)

###########################################################
m5 <- lm(Price$price ~ Price$bedroomsFixed, data = Price)

plot(Price$bedroomsFixed, Price$price, xlab="bedroomsFixed", ylab = "price")
abline(coef = m5$coefficients, col=2)

summ5 <- summary(m5)
summary(m5)

summ5$r.squared #!
sum(summ5$residuals^2)
var(Price$bedroomsFixed)
var(Price$price)
plot(summ5$residuals)
mean(summ5$residuals)
var(summ5$residuals)
hist(summ5$residuals)



#Завдання 2: Аналіз множинної регресії.
#(A) Побудувати лінійну модель (m1) за не менше ніж 5-ма параметрами;

m1<- lm(price ~ sqft_living + sqft_lot + sqft_living15+bathroomsFixed+bedroomsFixed, data = Price)
#(B) Визначити з summary() чому дорівнює 𝑅𝑆𝐸 та порахувати вручну, а також перевірити чи вони співпадають.
mod_summary <- summary(m1)
mod_summary
SSR <- sum(mod_summary$residuals^2)
SSR
# обчислити SER вручну
n <- nrow(Price)
SER <- sqrt(SSR / (n-2))
SER
#(C) Створити модель (m2) в якої на 1-н параметр менше
m2<- lm(price ~ sqft_living + sqft_lot + bathroomsFixed+bedroomsFixed, data = Price)
# (D) Порівняти моделі (m1) та (m2) за допомогою функцій summary() та car::compareCoefs(m1,m2) на предмет: 
#𝑅^2, 𝑅𝑆𝐸, 𝑆𝐸(𝛽𝑖). Зробити висновок, яка модель краща.
summary(m1)
summary(m2)
car::compareCoefs(m1,m2)
#(E) Визначити 𝑡кр для моделі (m1);
tKr <-1.96
#(F) Визначити ступені вільності для (m1);
#21607 degrees of freedom
#(G) Перевірити t-статистику для кожного з 5-ти коефіцієнтів моделі (m1);

summary(m1)
# sqft_living

#a. Сформулювати гіпотези 𝐻0 та 𝐻1;

# якщо t -tkr>val>tkr - H 0
# інакше H1

#b. Вказати значення t-статистики (t-value) для відповідного коефіцієнта;
#72.093

#c. Значення p-значення (Pr(>|t|)) для відповідного коефіцієнта;
#sqft_living    < 0.0000000000000002 ***
#d. Вказати яка гіпотеза виконується;
# H1
#e. Зробити графічне представлення;
t <- seq(-80, 80, 0.01)

plot(x = t,
     y = dnorm(t, 0, 1),
     type = "l",
     col = "steelblue",
     lwd = 2,
     yaxs = "i",
       axes = F,
     ylab = "",
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "= 72.093"),
     cex.lab = 0.7,
     cex.main = 1)
tactOne <- 72.093
axis(1, at = c(0, -1.96, 1.96, -tactOne, tactOne), cex.axis = 0.7)
# Затінити критичні області за допомогою polygon():
# критична область в лівому хвості
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0),
        col = 'orange')
# критична область в правому хвості
polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0),
        col = 'orange')
# Додайте стрілки та тексти, що вказують на критичні області та p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
arrows(tactOne, 0.16, tactOne, 0, length = 0.1)
arrows(-tactOne, 0.16, - tactOne, 0, length = 0.1)
text(-3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(tactOne, 0.18,
     labels = expression(paste("-|",t[act],"|")),
     cex = 0.7)
text(- tactOne, 0.18,
     labels = expression(paste("|",t[act],"|")),
     cex = 0.7)
# Додайте галочки, що вказують критичні значення на рівні 0,05, t^act і -t^act
rug(c(-1.96, 1.96), ticksize = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")

# sqft_lot           

#a. Сформулювати гіпотези 𝐻0 та 𝐻1;

# якщо t -tkr>val>tkr - H 0
# інакше H1

#b. Вказати значення t-статистики (t-value) для відповідного коефіцієнта;
#-9.074

#c. Значення p-значення (Pr(>|t|)) для відповідного коефіцієнта;
#sqft_lot       < 0.0000000000000002 ***
#d. Вказати яка гіпотеза виконується;
# H1
#e. Зробити графічне представлення;
t <- seq(-10, 10, 0.01)

plot(x = t,
     y = dnorm(t, 0, 1),
     type = "l",
     col = "steelblue",
     lwd = 2,
     yaxs = "i",
     axes = F,
     ylab = "",
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "= -9.074"),
     cex.lab = 0.7,
     cex.main = 1)
tact <- -9.074
axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)
# Затінити критичні області за допомогою polygon():
# критична область в лівому хвості
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0),
        col = 'orange')
# критична область в правому хвості
polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0),
        col = 'orange')
# Додайте стрілки та тексти, що вказують на критичні області та p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
arrows(tact, 0.16, tact, 0, length = 0.1)
arrows(- tact, 0.16, - tact, 0, length = 0.1)
text(-3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(tact, 0.18,
     labels = expression(paste("-|",t[act],"|")),
     cex = 0.7)
text(-tact, 0.18,
     labels = expression(paste("|",t[act],"|")),
     cex = 0.7)
# Додайте галочки, що вказують критичні значення на рівні 0,05, t^act і -t^act
rug(c(-1.96, 1.96), ticksize = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")

# sqft_living15             

#a. Сформулювати гіпотези 𝐻0 та 𝐻1;

# якщо t -tkr>val>tkr - H 0
# інакше H1

#b. Вказати значення t-статистики (t-value) для відповідного коефіцієнта;
#15.613

#c. Значення p-значення (Pr(>|t|)) для відповідного коефіцієнта;
#sqft_living15  < 0.0000000000000002 ***
#d. Вказати яка гіпотеза виконується;
# H1
#e. Зробити графічне представлення;
t <- seq(-20, 20, 0.01)

plot(x = t,
     y = dnorm(t, 0, 1),
     type = "l",
     col = "steelblue",
     lwd = 2,
     yaxs = "i",
     axes = F,
     ylab = "",
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "= 15.613"),
     cex.lab = 0.7,
     cex.main = 1)
tact <- 15.613
axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)
# Затінити критичні області за допомогою polygon():
# критична область в лівому хвості
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0),
        col = 'orange')
# критична область в правому хвості
polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0),
        col = 'orange')
# Додайте стрілки та тексти, що вказують на критичні області та p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
arrows(-tact, 0.16, -tact, 0, length = 0.1)
arrows(tact, 0.16, tact, 0, length = 0.1)
text(-3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(-tact, 0.18,
     labels = expression(paste("-|",t[act],"|")),
     cex = 0.7)
text(tact, 0.18,
     labels = expression(paste("|",t[act],"|")),
     cex = 0.7)
# Додайте галочки, що вказують критичні значення на рівні 0,05, t^act і -t^act
rug(c(-1.96, 1.96), ticksize = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")

# bathroomsFixed                           

#a. Сформулювати гіпотези 𝐻0 та 𝐻1;

# якщо t -tkr>val>tkr - H 0
# інакше H1

#b. Вказати значення t-статистики (t-value) для відповідного коефіцієнта;
# 1.542
#c. Значення p-значення (Pr(>|t|)) для відповідного коефіцієнта;
#bathroomsFixed              0.12304  
#d. Вказати яка гіпотеза виконується;
# H1
#e. Зробити графічне представлення;
t <- seq(-6, 6, 0.01)

plot(x = t,
     y = dnorm(t, 0, 1),
     type = "l",
     col = "steelblue",
     lwd = 2,
     yaxs = "i",
     axes = F,
     ylab = "",
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=  1.542"),
     cex.lab = 0.7,
     cex.main = 1)
tact <-  1.542
axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)
# Затінити критичні області за допомогою polygon():
# критична область в лівому хвості
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0),
        col = 'orange')
# критична область в правому хвості
polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0),
        col = 'orange')
# Додайте стрілки та тексти, що вказують на критичні області та p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
arrows(-tact, 0.16, -tact, 0, length = 0.1)
arrows(tact, 0.16, tact, 0, length = 0.1)
text(-3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(-tact, 0.18,
     labels = expression(paste("-|",t[act],"|")),
     cex = 0.7)
text(tact, 0.18,
     labels = expression(paste("|",t[act],"|")),
     cex = 0.7)
# Додайте галочки, що вказують критичні значення на рівні 0,05, t^act і -t^act
rug(c(-1.96, 1.96), ticksize = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")

# bedroomsFixed                             

#a. Сформулювати гіпотези 𝐻0 та 𝐻1;

# якщо t -tkr>val>tkr - H 0
# інакше H1

#b. Вказати значення t-статистики (t-value) для відповідного коефіцієнта;
#-24.379

#c. Значення p-значення (Pr(>|t|)) для відповідного коефіцієнта;
#bedroomsFixed  < 0.0000000000000002 ***
#d. Вказати яка гіпотеза виконується;
# H1
#e. Зробити графічне представлення;
t <- seq(-30, 30, 0.01)

plot(x = t,
     y = dnorm(t, 0, 1),
     type = "l",
     col = "steelblue",
     lwd = 2,
     yaxs = "i",
     axes = F,
     ylab = "",
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "= -24.379"),
     cex.lab = 0.7,
     cex.main = 1)
tact <- -24.379
axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)
# Затінити критичні області за допомогою polygon():
# критична область в лівому хвості
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0),
        col = 'orange')
# критична область в правому хвості
polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0),
        col = 'orange')
# Додайте стрілки та тексти, що вказують на критичні області та p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)
arrows(tact, 0.16, tact, 0, length = 0.1)
arrows(-tact, 0.16, -tact, 0, length = 0.1)
text(-3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22,
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(tact, 0.18,
     labels = expression(paste("-|",t[act],"|")),
     cex = 0.7)
text(-tact, 0.18,
     labels = expression(paste("|",t[act],"|")),
     cex = 0.7)
# Додайте галочки, що вказують критичні значення на рівні 0,05, t^act і -t^act
rug(c(-1.96, 1.96), ticksize = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")

#(H) Вказати довірчі інтервали для коефіцієнтів з рівнем надійності 95%, 90% та 99%;
confint(m1, level =0.95)
confint(m1, level =0.9)
confint(m1, level =0.99)
#(I) Виконати масштабування (центрування) моделі (m2) та перевірити чи співпадають коефіцієнти 𝛽1,𝛽2, 𝛽3, 𝛽4;




#Завдання 3: F-статистика.
#(A) Обчислити SST, SSR, SSE;
#(B) Перевірка моделі за F-критерієм;
#a. Сформулювати гіпотези 𝐻0та 𝐻1;
#b. Вказати значення F-статистики;
#c. Значення p-значення;
#d. Вказати яка гіпотеза виконується;
#(C) Обчислити коефiцiєнт 𝑅Adj2 (adjusted) для моделей від 2 до 5 змінних та зробити висновок;