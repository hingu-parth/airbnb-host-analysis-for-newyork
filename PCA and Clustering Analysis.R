#Airbnb Datset EDA
#Author: Parth Hingu

##Importing libraries
library(data.table)
library(ggplot2)  # tidyverse data visualization package
library(stringr)

library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(shiny)   # for web applications

library(car)
#Importing csv file from my local computer
airbnbOriginalDF =read.csv("C:/Users/yadav/Desktop/MVA proj/airbnb/airbnb_1/Airbnb Host Data For Newyork City.csv")
#Converting data frame to data table
setDT(airbnbOriginalDF)


######## DATA CLEANING #########

#Checking null/missing value in dataset
table(is.na(airbnbOriginalDF))

#Checking null values in review per month column
table(is.na(airbnbOriginalDF$reviews_per_month))


#Removing values which are null and storing in new table.
airbnbNoNADT = airbnbOriginalDF[airbnbOriginalDF$reviews_per_month != 'NA']

# Rechecking, and can see no null values present now.
table(is.na(airbnbNoNADT))
table(is.na(airbnbNoNADT$reviews_per_month)) #airbnbNoNADT is datatable with not any null values

#Converting datatype of last review date to DAte Format.
airbnbNoNADT[,last_review:=as.Date(last_review, '%m/%d/%Y')]
str(airbnbNoNADT)


#Lets try to further analyze our data by analysing data types.

#CONVERTING CATEGORICAL VALUES TO FACTORS

unique(airbnbNoNADT$neighbourhood_group)
#As the neighbourhood_group column has 5 categorical values, we can factor it, and convert our string data type.
airbnbNoNADT[,neighbourhood_group:= factor(neighbourhood_group)]


unique(airbnbNoNADT$neighbourhood)
#For neighbourhood, we get 217 unique values. Here to reduce storage we can covert all similar type to lower case and also trim white spaces, so that each anme is unique.

#Converting all same type name to lower cases
airbnbNoNADT[,neighbourhood:=tolower(neighbourhood)]

#Removing all white spaces
airbnbNoNADT[,neighbourhood:=trimws(neighbourhood)]

#For room type, we get 3 unique categorical values. we can factor it, and convert our string datatype.
unique(airbnbNoNADT$room_type)          
airbnbNoNADT[,room_type:= factor(room_type)]

######  Exploratory Data Analysis #######

#We found few ouliers, therefore that data and we have dropped below..
airbnbCleaned = airbnbNoNADT[price<2500 & number_of_reviews<400 & reviews_per_month<10]
#airbnbCleaned is our Final cleaned data

#Below we have stored the data for each boroughs in different table which will help to analyze each borough individually as well if required
#Manhattan area dataset
airbnbManhattan = airbnbCleaned[neighbourhood_group=='Manhattan']
nrow(airbnbManhattan)

#Queens area dataset
airbnbQueens = airbnbCleaned[neighbourhood_group=='Queens']
nrow(airbnbQueens)

#Brooklyn area dataset
airbnbBrooklyn = airbnbCleaned[neighbourhood_group=='Brooklyn']
nrow(airbnbBrooklyn)

#Bronx area dataset
airbnbBronx = airbnbCleaned[neighbourhood_group=='Bronx']
nrow(airbnbBronx)

#Staten Island area dataset
airbnbStatenIsland = airbnbCleaned[neighbourhood_group=='Staten Island']
nrow(airbnbStatenIsland)

#Creating corelation matrix for each  boroughs
diagnolcol = c("price","minimum_nights","reviews/month", "numberOfReviews", "availabilityFor365")


#MANHATTAN
pairs(data.table(
  airbnbManhattan$price,
  airbnbManhattan$minimum_nights,
  airbnbManhattan$reviews_per_month,
  airbnbManhattan$number_of_reviews,
  airbnbManhattan$availability_365), labels = diagnolcol)

#BROOKLYN
pairs(data.table(
  airbnbBrooklyn$price,
  airbnbBrooklyn$minimum_nights,
  airbnbBrooklyn$reviews_per_month,
  airbnbBrooklyn$number_of_reviews,
  airbnbBrooklyn$availability_365), labels = diagnolcol)

#QUEENS
pairs(data.table(
  airbnbQueens$price,
  airbnbQueens$minimum_nights,
  airbnbQueens$reviews_per_month,
  airbnbQueens$number_of_reviews,
  airbnbQueens$availability_365), labels = diagnolcol)

#Staten Island
pairs(data.table(
  airbnbStatenIsland$price,
  airbnbStatenIsland$minimum_nights,
  airbnbStatenIsland$reviews_per_month,
  airbnbStatenIsland$number_of_reviews,
  airbnbStatenIsland$availability_365), labels = diagnolcol)

#BRONX
pairs(data.table(
  airbnbBronx$price,
  airbnbBronx$minimum_nights,
  airbnbBronx$reviews_per_month,
  airbnbBronx$number_of_reviews,
  airbnbBronx$availability_365), labels = diagnolcol)
pairs(data.table(airbnbBronx$price,
                 airbnbBronx$minimum_nights,
                 airbnbBronx$reviews_per_month,
                 airbnbBronx$number_of_reviews,
                 airbnbBronx$availability_365), labels = diagnolcol)

####### PCA ########

######PCA for Manhattan######

library(dplyr)
library(data.table)

#Taking the numeric columns that will contribute for variance in data
airbnbManhattanPCA = data.frame(
  airbnbManhattan$id,
  airbnbManhattan$host_id,
  airbnbManhattan$room_type,
  airbnbManhattan$price,
  airbnbManhattan$minimum_nights,
  airbnbManhattan$number_of_reviews,
  airbnbManhattan$reviews_per_month,
  airbnbManhattan$availability_365)

setDT(airbnbManhattanPCA)

##Setting column names for our new dataframe
names(airbnbManhattanPCA) <- c(
  'id',
  'host_id',
  'room_type',
  'price',
  'minimum_nights',
  'number_of_reviews',
  'reviews_per_month',
  'availability_365')

head(airbnbManhattanPCA, 5)

##Here we have used prcomp function to get Principal components of data
airbnbPC <- prcomp(airbnbManhattanPCA[,-1:-3], scale=TRUE)
airbnbPC

##prcomp() gives three values x, sdev, rotation
names(airbnbPC)

## x contains principal components for drawing a graph.
##since there are 5 samples(COLUMNS), there are 5 PC

#To get a sense how meaningful this is, let's see how much variation in the original data PC1 or together with PC2 accounts for
#To do this we require the square of sdev, to see how much variance in the original data each PC accounts for
##The goal is to draw a graph that shows how the samples are related(not related)to each other

##Creating eigen values for airbnb (sqaure of sdev)  ----> representing by pca_var
(pca_var <- airbnbPC$sdev^2)
names(pca_var)
names(pca_var) <- paste("PC",1:5,sep="")
names(pca_var)
pca_var

##Taking sum of all eigen values  
sum_var <- sum(pca_var)
sum_var

##Calculating percentage of variance to better visualize each PC proportion in data
pcavarpercent <- (pca_var/sum_var)*100
##Visulaization using Bar chart
barplot(pcavarpercent, main="Scree Plot", xlab="Principal Component", ylab = "Percent Varaiation")

##Visualization using scree plot
plot(pcavarpercent, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

##From the plot it can be deciphered that all PC components have good amount of information with them, approx 80% of variance is presented with PC1,PC2, PC3, and thus we cannot choose only two PC for dimensionality reduction
##since it will lead to information loss.

######PCA for Queens######
#Taking the numeric columns that will contribute for variance in data
airbnbQueensPCA = data.frame(
  airbnbQueens$id,
  airbnbQueens$host_id,
  airbnbQueens$room_type,
  airbnbQueens$price,
  airbnbQueens$minimum_nights,
  airbnbQueens$number_of_reviews,
  airbnbQueens$reviews_per_month,
  airbnbQueens$availability_365)

setDT(airbnbQueensPCA)

##Setting column names for our new dataframe
names(airbnbQueensPCA) = c(
  'id',
  'host_id',
  'room_type',
  'price',
  'minimum_nights',
  'number_of_reviews',
  'reviews_per_month',
  'availability_365')

head(airbnbQueensPCA, 5)

##Here we have used prcomp function to get Principal components of data
airbnbQueensPC <- prcomp(airbnbQueensPCA[,-1:-3], scale=TRUE)
airbnbQueensPC

##prcomp() gives three values x, sdev, rotation
names(airbnbQueensPC)

## x contains principal components for drawing a graph.
##since there are 5 samples(COLUMNS), there are 5 PC

#To get a sense how meaningful this is, let's see how much variation in the original data PC1 or together with PC2 accounts for
#To do this we require the square of sdev, to see how much variance in the original data each PC accounts for
##The goal is to draw a graph that shows how the samples are related(not related)to each other

##Creating eigen values for airbnb (sqaure of sdev)  ----> representing by pca_var
(Queens_pca_var <- airbnbQueensPC$sdev^2)
names(Queens_pca_var)
names(Queens_pca_var) <- paste("PC",1:5,sep="")
names(Queens_pca_var)
Queens_pca_var

##Taking sum of all eigen values  
sum_var <- sum(Queens_pca_var)
sum_var

##Calculating percentage of variance to better visualize each PC proportion in data
pcavarpercent <- (Queens_pca_var/sum_var)*100
##Visulaization using Bar chart
barplot(pcavarpercent, main="Scree Plot", xlab="Principal Component", ylab = "Percent Varaiation")

##Visualization using scree plot
plot(pcavarpercent, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

##From the plot it can be deciphered that all PC components have good amount of information with them, approx 80% of variance is presented with PC1,PC2, PC3, and thus we cannot choose only two PC for dimensionality reduction
##since it will lead to information loss.




######PCA for Brooklyn######

#Taking the numeric columns that will contribute for variance in data
airbnbBrooklynPCA = data.frame(
  airbnbBrooklyn$id,
  airbnbBrooklyn$host_id,
  airbnbBrooklyn$room_type,
  airbnbBrooklyn$price,
  airbnbBrooklyn$minimum_nights,
  airbnbBrooklyn$number_of_reviews,
  airbnbBrooklyn$reviews_per_month,
  airbnbBrooklyn$availability_365)

setDT(airbnbBrooklynPCA)

##Setting column names for our new dataframe
names(airbnbBrooklynPCA) = c(
  'id',
  'host_id',
  'room_type',
  'price',
  'minimum_nights',
  'number_of_reviews',
  'reviews_per_month',
  'availability_365')

head(airbnbBrooklynPCA, 5)

##Here we have used prcomp function to get Principal components of data
airbnbBrooklynPCA <- prcomp(airbnbBrooklynPCA[,-1:-3], scale=TRUE)
airbnbBrooklynPCA

##prcomp() gives three values x, sdev, rotation
names(airbnbBrooklynPCA)

## x contains principal components for drawing a graph.
##since there are 5 samples(COLUMNS), there are 5 PC

#To get a sense how meaningful this is, let's see how much variation in the original data PC1 or together with PC2 accounts for
#To do this we require the square of sdev, to see how much variance in the original data each PC accounts for
##The goal is to draw a graph that shows how the samples are related(not related)to each other

##Creating eigen values for airbnb (sqaure of sdev)  ----> representing by pca_var
(Brooklyn_pca_var <- airbnbBrooklynPCA$sdev^2)
names(Brooklyn_pca_var)
names(Brooklyn_pca_var) <- paste("PC",1:5,sep="")
names(Brooklyn_pca_var)
Brooklyn_pca_var

##Taking sum of all eigen values  
sum_var <- sum(Brooklyn_pca_var)
sum_var

##Calculating percentage of variance to better visualize each PC proportion in data
pcavarpercent <- (Brooklyn_pca_var/sum_var)*100
##Visulaization using Bar chart
barplot(pcavarpercent, main="Scree Plot", xlab="Principal Component", ylab = "Percent Varaiation")

##Visualization using scree plot
plot(pcavarpercent, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

##From the plot it can be deciphered that all PC components have good amount of information with them, approx 80% of variance is presented with PC1,PC2, PC3, and thus we cannot choose only two PC for dimensionality reduction
##since it will lead to information loss.


######PCA for Bronx######
#Taking the numeric columns that will contribute for variance in data
airbnbBronxPCA = data.frame(
  airbnbBronx$id,
  airbnbBronx$host_id,
  airbnbBronx$room_type,
  airbnbBronx$price,
  airbnbBronx$minimum_nights,
  airbnbBronx$number_of_reviews,
  airbnbBronx$reviews_per_month,
  airbnbBronx$availability_365)

setDT(airbnbBronxPCA)

##Setting column names for our new dataframe
names(airbnbBronxPCA) = c(
  'id',
  'host_id',
  'room_type',
  'price',
  'minimum_nights',
  'number_of_reviews',
  'reviews_per_month',
  'availability_365')

head(airbnbBronxPCA, 5)

##Here we have used prcomp function to get Principal components of data
airbnbBronxPCA <- prcomp(airbnbBronxPCA[,-1:-3], scale=TRUE)
airbnbBronxPCA

##prcomp() gives three values x, sdev, rotation
names(airbnbBronxPCA)

## x contains principal components for drawing a graph.
##since there are 5 samples(COLUMNS), there are 5 PC

#To get a sense how meaningful this is, let's see how much variation in the original data PC1 or together with PC2 accounts for
#To do this we require the square of sdev, to see how much variance in the original data each PC accounts for
##The goal is to draw a graph that shows how the samples are related(not related)to each other

##Creating eigen values for airbnb (sqaure of sdev)  ----> representing by pca_var
(Bronx_pca_var <- airbnbBronxPCA$sdev^2)
names(Bronx_pca_var)
names(Bronx_pca_var) <- paste("PC",1:5,sep="")
names(Bronx_pca_var)
Bronx_pca_var

##Taking sum of all eigen values  
sum_var <- sum(Bronx_pca_var)
sum_var

##Calculating percentage of variance to better visualize each PC proportion in data
pcavarpercent <- (Bronx_pca_var/sum_var)*100
##Visulaization using Bar chart
barplot(pcavarpercent, main="Scree Plot", xlab="Principal Component", ylab = "Percent Varaiation")

##Visualization using scree plot
plot(pcavarpercent, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

##From the plot it can be deciphered that all PC components have good amount of information with them, approx 80% of variance is presented with PC1,PC2, PC3, and thus we cannot choose only two PC for dimensionality reduction
##since it will lead to information loss.



######PCA for Staten Island######

#Taking the numeric columns that will contribute for variance in data
airbnbStatenIslandPCA = data.frame(
  airbnbStatenIsland$id,
  airbnbStatenIsland$host_id,
  airbnbStatenIsland$room_type,
  airbnbStatenIsland$price,
  airbnbStatenIsland$minimum_nights,
  airbnbStatenIsland$number_of_reviews,
  airbnbStatenIsland$reviews_per_month,
  airbnbStatenIsland$availability_365)

setDT(airbnbStatenIslandPCA)

##Setting column names for our new dataframe
names(airbnbStatenIslandPCA) = c(
  'id',
  'host_id',
  'room_type',
  'price',
  'minimum_nights',
  'number_of_reviews',
  'reviews_per_month',
  'availability_365')

head(airbnbStatenIslandPCA, 5)

##Here we have used prcomp function to get Principal components of data
airbnbStatenIslandPCA <- prcomp(airbnbStatenIslandPCA[,-1:-3], scale=TRUE)
airbnbStatenIslandPCA

##prcomp() gives three values x, sdev, rotation
names(airbnbStatenIslandPCA)

## x contains principal components for drawing a graph.
##since there are 5 samples(COLUMNS), there are 5 PC

#To get a sense how meaningful this is, let's see how much variation in the original data PC1 or together with PC2 accounts for
#To do this we require the square of sdev, to see how much variance in the original data each PC accounts for
##The goal is to draw a graph that shows how the samples are related(not related)to each other

#Creating eigen values for airbnb (sqaure of sdev)  ----> representing by pca_var
(StatenIsland_pca_var <- airbnbStatenIslandPCA$sdev^2)
names(StatenIsland_pca_var)
names(StatenIsland_pca_var) <- paste("PC",1:5,sep="")
names(StatenIsland_pca_var)
StatenIsland_pca_var

#Taking sum of all eigen values  
sum_var <- sum(StatenIsland_pca_var)
sum_var

#Calculating percentage of variance to better visualize each PC proportion in data
pcavarpercent <- (StatenIsland_pca_var/sum_var)*100
#Visulaization using Bar chart
barplot(pcavarpercent, main="Scree Plot", xlab="Principal Component", ylab = "Percent Varaiation")

#Visualization using scree plot
plot(pcavarpercent, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#From the plot it can be deciphered that all PC components have good amount of information with them, approx 80% of variance is presented with PC1,PC2, PC3, and thus we cannot choose only two PC for dimensionality reduction
#since it will lead to information loss.


########## K-means Clustering #########
#install.packages("cluster")
library(cluster)
airbnbManhattanClust = data.frame(
  airbnbManhattan$price,
  airbnbManhattan$number_of_reviews,
  airbnbManhattan$reviews_per_month)

#Making property id as Rownames, so cluster will be formed iwth these points.
rownames(airbnbManhattanClust) <- airbnbManhattan$id

#Scaling done to make the data on one scale.
scaleManhattan <- scale(airbnbManhattanClust[,1:ncol(airbnbManhattanClust)])
head(scaleManhattan)

# We will find K-means by taking k=2, 3, 4, 5, 6...
# Centers (k's) are numbers thus, 10 random sets are chosen

#For 2 clusters, k-means = 2
(kmeans2.Manhattan <- kmeans(scaleManhattan,2,nstart = 10))
# Computing the percentage of variation accounted for two clusters
perc_var_kmeans2 <- round(100*(1 - kmeans2.Manhattan$betweenss/kmeans2.Manhattan$totss),1)
names(perc_var_kmeans2) <- "Perc. 2 clus"
perc_var_kmeans2

#For 3 clusters, k-means = 3
(kmeans3.Manhattan <- kmeans(scaleManhattan,3,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc_var_kmeans3 <- round(100*(1 - kmeans3.Manhattan$betweenss/kmeans3.Manhattan$totss),1)
names(perc_var_kmeans3) <- "Perc. 3 clus"
perc_var_kmeans3

#For 4 clusters, k-means = 4
(kmeans4.Manhattan <- kmeans(scaleManhattan,4,nstart = 10))
# Computing the percentage of variation accounted for. Two clusters
perc_var_kmeans4 <- round(100*(1 - kmeans4.Manhattan$betweenss/kmeans4.Manhattan$totss),1)
names(perc_var_kmeans4) <- "Perc. 4 clus"
perc_var_kmeans4

#From above, after computing percentage of variation for each k means, we found that k means 3 could be good to preseent our data
# Saving above 3 k-means (1,2,3)  in a list

#Filtering properties which are in 1 cluster of k mean 3
clus1 <- matrix(names(kmeans3.Manhattan$cluster[kmeans3.Manhattan$cluster == 1]),
                ncol=1, nrow=length(kmeans3.Manhattan$cluster[kmeans3.Manhattan$cluster == 1]))

colnames(clus1) <- "Cluster 1"

#Filtering properties which are in 2 cluster of k mean 3
clus2 <- matrix(names(kmeans3.Manhattan$cluster[kmeans3.Manhattan$cluster == 2]),
                ncol=1, nrow=length(kmeans3.Manhattan$cluster[kmeans3.Manhattan$cluster == 2]))
colnames(clus2) <- "Cluster 2"

#Filtering properties which are in 3 cluster of k mean 3
clus3 <- matrix(names(kmeans3.Manhattan$cluster[kmeans3.Manhattan$cluster == 3]),
                ncol=1, nrow=length(kmeans3.Manhattan$cluster[kmeans3.Manhattan$cluster == 3]))
colnames(clus3) <- "Cluster 3"

list(clus1,clus2,clus3)
head(clus1,5)
head(clus2,5)
head(clus3,5)


#Now we will plot these clusters
library(fpc)
plotcluster(airbnbManhattanClust,kmeans3.Manhattan$cluster)

#We can make three subsets for three clusters by row filtering
airbnbManhattanCluster1 <- subset(airbnbManhattan, airbnbManhattan$id %in% clus1)
airbnbManhattanCluster2 <- subset(airbnbManhattan, airbnbManhattan$id %in% clus2)
airbnbManhattanCluster3 <- subset(airbnbManhattan, airbnbManhattan$id %in% clus3)

#Tried checking if properties in particular clusters are located in some specific area in Manhattan
length(unique(airbnbManhattanCluster1$neighbourhood))
length(unique(airbnbManhattanCluster2$neighbourhood))
length(unique(airbnbManhattanCluster3$neighbourhood))

#We did not get any idea, as all clusters have almost all locations.

#This is to check the mean of 3 clusters
kmeans3.Manhattan$centers

#We will see average price, average number of reviews , average reviews per month for houses in each cluster to get a better idea of most recommendable properties.
mean(airbnbManhattanCluster1$price)
mean(airbnbManhattanCluster1$number_of_reviews)
mean(airbnbManhattanCluster1$reviews_per_month)

mean(airbnbManhattanCluster2$price)
mean(airbnbManhattanCluster2$number_of_reviews)
mean(airbnbManhattanCluster2$reviews_per_month)

mean(airbnbManhattanCluster3$price)
mean(airbnbManhattanCluster3$number_of_reviews)
mean(airbnbManhattanCluster3$reviews_per_month)

#From above means , we find that properties in cluter 2 have average price of 150 and avargae no of reviews as 16.
#However for clust 1, avg price is 150 and avg no.of reviews is 11.
# for 3, avg price is 692 and avg no of reviews is 16

#Thus the most recommended properties for people to stay in Manhattan lies in Cluster 2


setDT(airbnbManhattanCluster2)

#Here we are trying to see the top apartment type available in cluster 2.


nrow(airbnbManhattanCluster2[airbnbManhattanCluster2$room_type == 'Entire home/apt'])
nrow(airbnbManhattanCluster2[airbnbManhattanCluster2$room_type == 'Private room'])
nrow(airbnbManhattanCluster2[airbnbManhattanCluster2$room_type == 'Shared room'])



ggplot(airbnbManhattanCluster2, aes(x=airbnbManhattanCluster2$room_type)) +geom_bar(fill ='purple') +theme_minimal()

# From this we see, that Entire home/apt and private room are the most available ones.


#Below we have shown the araes in Manhattan which have these properties in Cluster 2.
#There is no specific location in Manhattan have this spread out.
ggplot(airbnbManhattanCluster2, aes(x=airbnbManhattanCluster2$longitude,y=airbnbManhattanCluster2$latitude)) + geom_point(size=0.1, color = 'dark blue')


