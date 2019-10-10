library(ggplot2)
airbnb<-read.csv("C:/Users/yadav/Desktop/MVA proj/airbnb/airbnb_1/Airbnb Host Data For Newyork City.csv")
View(airbnb)
head(airbnb)
airbnb<-as.data.frame(airbnb)
str(airbnb)
summary(airbnb)

#replacing missing values with zero
airbnb$reviews_per_month<- replace(airbnb$reviews_per_month, airbnb$reviews_per_month == "", NA)
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)] <- 0

#dropping columns 1,4,13
airbnb<- airbnb[, -c(1,4,13)]

#getting unique value of cities
airbnb_neighbourhood_group <-unique(airbnb$neighbourhood_group)
airbnb_neighbourhood_group<-as.vector(airbnb_neighbourhood_group)
airbnb_neighbourhood_group

#getting unique value of room type
airbnb_room_type <-unique(airbnb$room_type)
airbnb_room_type<-as.vector(airbnb_room_type)
airbnb_room_type

#getting unique value of cities neighbourhood
airbnb_neighbourhood <-unique(airbnb$neighbourhood)
airbnb_neighbourhood<-as.vector(airbnb_neighbourhood)
airbnb_neighbourhood

#finding top-hosts-the most listings on Airbnb platform and taking advantage of this service
airbnb_host_id<-as.factor(airbnb$host_id)
airbnb_host_id
airbnb_host_id<-as.vector(airbnb$host_id)
p <- as.data.frame(table(airbnb_host_id))
top_hosts <- p[order(-p$Freq),]
top_hosts_head<-head(top_hosts)
top_hosts_head<-as.vector(top_hosts_head)

#plotting bar chart
barchart<-ggplot(data = top_hosts_head,aes(x=airbnb_host_id,y=Freq))+
  geom_bar(stat ="Identity")
barchart

#Brooklyn_price
sub_1<- subset(airbnb, neighbourhood_group =="Brooklyn")
sub_1
Brooklyn=sub_1[['price']]
Brooklyn

#Manhattan_price
sub_2<- subset(airbnb, neighbourhood_group =="Manhattan")
sub_2
price_sub2=sub_2[['price']]
price_sub2

#Queens_price
sub_3<-subset(airbnb, neighbourhood_group =="Queens")
sub_3
price_sub3=sub_3[['price']]
price_sub3

#Staten Island_price
sub_4<-subset(airbnb, neighbourhood_group =="Staten Island")
sub_4
price_sub4=sub_4[['price']]
price_sub4

#Bronx_price
sub_5<-subset(airbnb, neighbourhood_group =="Bronx")
sub_5
price_sub5=sub_5[['price']]
price_sub5

#price_distribution
price_list_by_n <- cbind(quantile(Brooklyn),quantile(price_sub2),quantile(price_sub3),quantile(price_sub4),quantile(price_sub5))
price_list_by_n

#finding top neighbourhood
p_1 <- as.data.frame(table(airbnb$neighbourhood))
top_neighbourhood<- p_1[order(-p_1$Freq),]
top_neighbourhood<-head(top_neighbourhood)
top_neighbourhood

barchart<-ggplot(data = top_neighbourhood,aes(x=Var1,y=Freq))+
  geom_bar(stat ="Identity")
barchart

#finding top room type
p_2 <- as.data.frame(table(airbnb$room_type))
top_room_type <- p_2[order(-p_2$Freq),]
top_room_type<-head(top_room_type)
top_room_type<-as.vector(top_room_type)
top_room_type

barchart<-ggplot(data = top_room_type,aes(x=Var1,y=Freq))+
  geom_bar(stat ="Identity")
barchart

#exploring room type in neighbourhood
manhattan_room_type <- table(sub_2$room_type)
manhattan_room_type

brooklyn_room_type <- table(sub_1$room_type)
brooklyn_room_type
queens_room_type <- table(sub_3$room_type)
queens_room_type

staten_island_room_type <- table(sub_4$room_type)
staten_island_room_type

bronx_room_type <- table(sub_5$room_type)
bronx_room_type

airbnb_roomtype_group <- rbind(manhattan_room_type, bronx_room_type,queens_room_type,staten_island_room_type,brooklyn_room_type)
airbnb_roomtype_group

