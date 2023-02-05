heart<-read.csv("C:/Users/adm/Desktop/heart.csv")
library(tidyverse)
head(heart)
tail(heart)
glimpse(heart)
colnames(heart)

summary(heart)


data2<-heart%>%
  mutate(sex=if_else(sex==1,"MALE","FEMALE"),
         fbs =if_else(fbs ==1 ,">120","<=120"),
         exang=if_else(exang ==1 ,"YES","NO"),
         cp = if_else(cp==1,"ATYPICAL ANGINA",
               if_else(cp==2,"NON_ANGINAL PAIN","ASYMPOTOMATIC")),
        restecg=if_else(restecg==0,"NORMAL",
                if_else(restecg==1,"ABNORMALITY","PROBABLE OR DEFINITE")),
        slope = as.factor(slope),
        ca = as.factor(ca),
        thal= as.factor(thal),
        target = if_else(target==1,"YES","NO")
        )%>%
  mutate_if(is.character,as.factor)%>%
  dplyr::select(target,sex,fbs,exang,cp,restecg,slope,ca,thal,everything())

#DATA VISUALIZATION
#Barplot for the target variable( heart disease)
library(ggplot2)
ggplot(data2,aes(x=data2$target,fill=data2$target))+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("Count")+
  ggtitle("Presence and  Absence of Heart Disease")+
  scale_fill_discrete(name="Heart Disease",labels=c("Absence","Precence"))


prop.table(table(data2$target))

#Count of the frequency of the values of age

data2%>%group_by(?..age)%>%
  count()%>%
  filter(n>10)%>%
  ggplot()+
  geom_col(aes(?..age,n),fill="green")+
  ggtitle("Age analysis")+
  xlab("Age")+
  ylab("Age count")


 #comparing bloodpressure across the chest pain
 data2%>%ggplot(aes(x=sex,y=trestbps))+
    geom_boxplot(fill="Purple")+
   xlab("Sex")+
   ylab("cp")+
   facet_grid(~cp)
 
 
heart%>%ggplot(aes(x=sex,y=trestbps))+
   geom_boxplot(fill="Purple")+
   xlab("Sex")+
   ylab("cp")+
   facet_grid(~cp)

data2%>%ggplot(aes(x=sex,y=chol))+
  geom_boxplot(fill="darkred")+
  xlab("Sex")+
  ylab("Chol")+
  facet_grid(~cp) 


# % correalation
library(corrplot)
cor_heart<-cor(data2[,10:14]) 
cor_heart
corrplot(cor_heart,method = "square",type="upper")
