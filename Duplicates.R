library(tidyverse)
my_data<-as_tibble(iris)
my_data
#Find and drop dulipcated elements
 x<-c(1,1,4,5,4,6)
duplicated(x) 
#Extract the duplicated elements
x[duplicated(x)]

#Drop the duplicated elements
x[!duplicated(x)]


#Remove duplicates  based on Sepal.width column
my_data[!duplicated(my_data$Petal.Width),]


#Extract unique elements 
unique(x)

#Applying unique to remove duplicated values
unique(my_data)




#Using the distict function
my_data%>%distinct()

#Remove duplicated based on sepal.length
my_data%>%distinct(Sepal.Length,.keep_all=TRUE)

#Remove duplicated based on sepal.length And petal.width
my_data%>%distinct(Sepal.Length,Petal.Length,.keep_all=TRUE)



#Delete or drop rows with condtion
# create dataframe
df1 = data.frame(Name = c('George','Andrea', 'Micheal','Maggie','Ravi','Xien','Jalpa'), 
                 Grade_score=c(4,6,2,9,5,7,8),
                 Mathematics1_score=c(45,78,44,89,66,49,72),
                 Science_score=c(56,52,45,88,33,90,47))
df1

#Method 1
df2<-df1[!(df1$Name=="George" | df1$Name=="Andrea"),]
df2

#Dropping using subset function
df3<-subset(df1, Name!="George" & Name!="Andrea")
df3

### Drop rows using slice() function in R

library(dplyr)
df4 <- df1 %>% slice(-c(2, 4, 6))
df4

#Dummy coding
#Uising the ifelse
data$Disc_A<-ifesle(data$displine=="A",1,0)
data$Disc_B<-ifesle(data$displine=="B",1,0)

library(tidyverse)
my_data<-as_tibble(iris)
my_data
#Find and drop dulipcated elements
 x<-c(1,1,4,5,4,6)
duplicated(x) 
#Extract the duplicated elements
x[duplicated(x)]

#Drop the duplicated elements
x[!duplicated(x)]


#Remove duplicates  based on Sepal.width column
my_data[!duplicated(my_data$Petal.Width),]


#Extract unique elements 
unique(x)

#Applying unique to remove duplicated values
unique(my_data)




#Using the distict function
my_data%>%distinct()

#Remove duplicated based on sepal.length
my_data%>%distinct(Sepal.Length,.keep_all=TRUE)

#Remove duplicated based on sepal.length And petal.width
my_data%>%distinct(Sepal.Length,Petal.Length,.keep_all=TRUE)



#Delete or drop rows with condtion
# create dataframe
df1 = data.frame(Name = c('George','Andrea', 'Micheal','Maggie','Ravi','Xien','Jalpa'), 
                 Grade_score=c(4,6,2,9,5,7,8),
                 Mathematics1_score=c(45,78,44,89,66,49,72),
                 Science_score=c(56,52,45,88,33,90,47))
df1

#Method 1
df2<-df1[!(df1$Name=="George" | df1$Name=="Andrea"),]
df2

#Dropping using subset function
df3<-subset(df1, Name!="George" & Name!="Andrea")
df3

### Drop rows using slice() function in R

library(dplyr)
df4 <- df1 %>% slice(-c(2, 4, 6))
df4

#Dummy coding
#Uising the ifelse
data$Disc_A<-ifesle(data$displine=="A",1,0)
data$Disc_B<-ifesle(data$displine=="B",1,0)

