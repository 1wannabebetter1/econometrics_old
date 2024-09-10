Price$waterfront <-as.factor(Price$waterfront)
Price$view<-as.factor(Price$view)
Price$condition <-as.factor(Price$condition)
Price$grade <-as.factor(Price$grade)

Price["bedrooms"][Price["bedrooms"] == 0] <- NA
Price["bathrooms"][Price["bathrooms"] == 0] <- NA
summary(Price)

Price$bedroomsFixed <- Price$bedrooms
summary(Price$bedroomsFixed)
library(rpart)
bedroomsfit <- rpart(bedroomsFixed ~ sqft_living + sqft_above+floors+price,
                data=Price[!is.na(Price$bedroomsFixed),],
                method="anova")
Price$bedroomsFixed[is.na(Price$bedroomsFixed)] <- predict(bedroomsfit, Price[is.na(Price$bedroomsFixed),])
Price$bedroomsFixed <- as.integer(Price$bedroomsFixed)
summary(Price$bedroomsFixed)

Price$bathroomsFixed <- Price$bathrooms
summary(Price$bathrooms)
library(rpart)
bathroomsfit <- rpart(bathroomsFixed ~ sqft_living + sqft_above+floors+price,
                     data=Price[!is.na(Price$bathroomsFixed),],
                     method="anova")
Price$bathroomsFixed[is.na(Price$bathroomsFixed)] <- predict(bathroomsfit, Price[is.na(Price$bathroomsFixed),])

summary(Price$bathroomsFixed)


Price$bathroomsFixed <- Price$bathroomsFixed *4
Price$bathroomsFixed <- as.integer(Price$bathroomsFixed)
Price$bathroomsFixed <- as.numeric(Price$bathroomsFixed)
Price$bathroomsFixed <- Price$bathroomsFixed /4

Price$LastMajorUpdate <- pmax(Price$yr_built, Price$yr_renovated)

Price$dateFixed <- substr(Price$date, 1, 8)
Price$dateFixed <- as.numeric(Price$dateFixed)
Price$dateFixed <- as.integer(Price$dateFixed)

Price_range <- c('Low', 'Mid', 'High')
Price$PriceRange <- cut(Price$price, c(0, 300000, 900000, 10000000), include.highest=TRUE, labels= Price_range)





g <- ggplot(Price, aes(dateFixed ))
g + geom_bar(aes(fill=PriceRange), width = 1) +
   scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для floors ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "floors ", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = condition, fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "*Назва Колонки*", y = "У частці PriceRange")
g2




sqft_living_range <- c('0-1000', '1001-2500', '2500-4000', '4001+')
Price$SqftLivingRange <- cut(Price$sqft_living, c(0, 1000, 2500, 4000, 14000), include.highest=TRUE, labels= sqft_living_range)


g <- ggplot(Price, aes(SqftLivingRange))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для SqftLivingRange  ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "SqftLivingRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = SqftLivingRange, fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "SqftLivingRange", y = "У частці PriceRange")
g2


sqft_lot_range <- c('0-5000', '5001-8000', '8000-12000', '12001+')
Price$SqftLotRange <- cut(Price$sqft_lot, c(0, 5000, 8000, 12000, 2000000), include.highest=TRUE, labels= sqft_living_range)


g <- ggplot(Price, aes(SqftLotRange))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для SqftLotRange  ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "SqftLotRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = SqftLotRange, fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "SqftLotRange", y = "У частці PriceRange")
g2


sqft_above_range <- c('0-1000', '1001-1600', '1601-2400', '2400+')
Price$SqftAboveRange <- cut(Price$sqft_above, c(0, 1000, 1600, 2400, 10000), include.highest=TRUE, labels= sqft_living_range)


g <- ggplot(Price, aes(SqftAboveRange))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для SqftAboveRange ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "SqftAboveRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = SqftAboveRange, fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "SqftAboveRange", y = "У частці PriceRange")
g2



sqft_basement_range <- c('0', '1-400', '400-1000', '1000+')
Price$SqftBasementRange <- cut(Price$sqft_basement, c(-1, 1, 400, 1000, 10000), include.highest=TRUE, labels= sqft_basement_range)


g <- ggplot(Price, aes(lat))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для SqftBasementRange ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "SqftBasementRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = lat, fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "SqftBasementRange", y = "У частці PriceRange")
g2

lat_range <- c('0-47.2', '47.21-47.4', '47.41-47.6', '47.61-47.8')
Price$LatRange <- cut(Price$lat, c(0, 47.2, 47.4, 47.6, 47.8), include.highest=TRUE, labels= lat_range)


g <- ggplot(Price, aes(LatRange))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для LatRange ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "LatRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = LatRange, fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "LatRange", y = "У частці PriceRange")
g2



long_range <- c('(-123)-(-122.3))', '(-122.29)-(-122.1)', '(-122.09)-(-121.85)', '(-121.84)-(-121)')
Price$LongRange <- cut(Price$long, c(-123, -122.3, -122.1, -121.85, -121), include.highest=TRUE, labels= long_range)


g <- ggplot(Price, aes(LongRange))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для LongRange ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "LongRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = LongRange, fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "LongRange", y = "У частці PriceRange")
g2




zip_range <- c('98000-98050)', '98051-98100', '98101-98150', '98151-98200')
Price$ZipCodeRange <- cut(Price$zipcode, c(97999, 98050, 98100, 98150, 98200), include.highest=TRUE, labels= zip_range)


g <- ggplot(Price, aes(ZipCodeRange))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для ZipCodeRange ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "ZipCodeRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = ZipCodeRange , fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "ZipCodeRange", y = "У частці PriceRange")
g2

zip_range <- c('98000-98050)', '98051-98100', '98101-98150', '98151-98200')
Price$ZipCodeRange <- cut(Price$zipcode, c(97999, 98050, 98100, 98150, 98200), include.highest=TRUE, labels= zip_range)


g <- ggplot(Price, aes(ZipCodeRange))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для ZipCodeRange ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "ZipCodeRange", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = sqft_living15 , fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "ZipCodeRange", y = "У частці PriceRange")
g2



sqft_living15_range <- c('0-2000', '2001-3000', '3001-4000', '4000+')
Price$sqft_living15Range <- cut(Price$sqft_living15, c(0, 2000, 3000, 4000, 10000), include.highest=TRUE, labels= sqft_living15_range)


g <- ggplot(Price, aes(sqft_living15Range))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для sqft_living15Range ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "sqft_living15Range", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = sqft_living15Range , fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "sqft_living15Range", y = "У частці PriceRange")
g2




sqft_lot_range <- c('0-4500', '4501-7000', '7001-11000', '11001+')
Price$sqft_lot15Range <- cut(Price$sqft_lot15, c(0, 4500, 7000, 11000, 871201), include.highest=TRUE, labels= sqft_lot_range)


g <- ggplot(Price, aes(sqft_lot15Range))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для sqft_lot15Range ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "sqft_lot15Range", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = sqft_lot15Range , fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "sqft_lot15Range", y = "У частці PriceRange")
g2




summary(Price)

Price$yr_renovatedF <- Price$yr_renovated
Price["yr_renovatedF"][Price["yr_renovatedF"] == 0] <- NA

g <- ggplot(Price, aes(yr_renovatedF))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для yr_renovated ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "yr_renovated", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = dateFixed , fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "timeFixed", y = "У частці PriceRange")
g2





time_range <- c('20140501-20140703', '20140704-20140902', '20140903-20141204', '20141205-20150527')
Price$sDateRange <- cut(Price$dateFixed, c(20140501, 20140703, 20140902, 20141204, 20150528), include.highest=TRUE, labels= time_range)


g <- ggplot(Price, aes(condition  ))
g + geom_bar(aes(fill=PriceRange), width = 1) +
  scale_y_continuous(trans='sqrt') +
  theme(axis.text.x = element_text(angle=65, vjust=2)) + 
  labs(title="Гістограма для condition   ", 
       subtitle="з розподілом цінових діапазонів")+
  labs(x = "condition  ", y = "Кількість житла")




library("ggplot2")
g2<-ggplot(as.data.frame(Price)) + 
  aes(x = condition   , fill = PriceRange, weight = ..count..) + 
  geom_bar(position = "fill") + 
  labs(x = "condition  ", y = "У частці PriceRange")
g2
