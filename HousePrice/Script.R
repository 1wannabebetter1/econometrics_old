summary(Price)
str(Price)
Price$bedrooms
Room_Labels <- c('0-3', '4-6', '7-10', '11-15', '16-20', '20-30', '30-40' )
# Призначення мітки
Price$NumOfBedrooms <- cut(Price$bedrooms, c(-1, 3, 6,10,  15, 20, 30, 40), include.highest=TRUE, labels= Room_Labels)
# Графік
hist(Price$bedrooms, breaks = 30)

plot(Price$bedrooms, log="y", type='h', lwd=10, lend=2)

#install.packages("ggplot2")                   # Install & load ggplot2 package
library("ggplot2")
ggplot(Price, aes(x = Age_group)) + geom_histogram() + scale_y_continuous(trans='sqrt')

ggplot(Price, aes(x = Price$price)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма цін")

ggplot(Price, aes(Price$NumOfBedrooms)) + geom_bar() + scale_y_continuous(trans='sqrt')
hist(Price$bathrooms, breaks = 30)

ggplot(Price, aes(x = Price$bathroom)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Кількість ванних кімнат")

ggplot(Price, aes(x = Price$sqft_living)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма площ будівель")

ggplot(Price, aes(x = Price$sqft_lot)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма площ земляних ділянок")

ggplot(Price, aes(x = Price$floors)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма кількості поверхів")


ggplot(Price, aes(x = Price$waterfront)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма наявності виду на водойму")

ggplot(Price, aes(x = Price$view)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма оцінки вигляду")

ggplot(Price, aes(x = Price$condition)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма оцінки стану")

ggplot(Price, aes(x = Price$grade)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма загальної оцінки")

ggplot(Price, aes(x = Price$sqft_above)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма площі будинку над рівнем землі")

ggplot(Price, aes(x = Price$sqft_basement)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма площі будинку під рівнем землі")

ggplot(Price, aes(x = Price$yr_built)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма року побудови будинку")

ggplot(Price, aes(x = Price$yr_renovated)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма року масштабного ремонту будинку(0-ремонту не було)")

ggplot(Price, aes(x = Price$zipcode)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма поштового індексу")

ggplot(Price, aes(x = Price$lat)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма широти")

ggplot(Price, aes(x = Price$long)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма довготи")

ggplot(Price, aes(x = Price$sqft_living15)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма площі будинків 15 найбл. сусідів")

ggplot(Price, aes(x = Price$sqft_lot15)) + geom_histogram() + scale_y_continuous(trans='sqrt')+ggtitle("Гістограма площі ділянок 15 найбл. сусідів")
