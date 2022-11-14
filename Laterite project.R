#Loading libraries
library(tidyverse)

laterite<-read.csv("C:/Users/adm/Documents/Datasets/laterite_mobilemoney_data.csv")
head(laterite)

laterite<-laterite%>%distinct(hhid,.keep_all=TRUE)
laterite[duplicated(laterite$hhid)]
#Results show there are no duplicated values 

laterite$finacially_excluded<-ifelse(laterite$account_type=="None",1,0)
laterite$digitally_finacially_included<-ifelse(laterite$account_type=="None",0,1)



ggplot(laterite,aes(x=factor(district),y=prop.table(stat(count)),fill=factor(finacially_excluded),
                    label=scales::percent(prop.table(stat(count)))))+
  geom_bar(position = "dodge")+
  geom_text(stat = "count",position = position_dodge(.9),vjust=.9,size=3)+
  scale_y_continuous(labels=scales::percent)+
  labs(x="District",y="financially_included",title ="Finacial exclusion per district ",
       fill="district")+
  scale_fill_viridis_d()

#3 .How the mobile money market is distributed among the 3 Companies
#Pie chart to show distribution among the 3 Companies
laterite$mm_account_telco<-as.factor(laterite$mm_account_telco)
my_table<-table(laterite$mm_account_telco)
my_table
lbls<-c(" ","A","A & B","A,B & C","A & C","B","B & C","C")
lbls<-paste(lbls, "\n", my_table)

pie(my_table,labels = lbls,
    main="Pie Chart of  Companies \n (With sample size)",
    col = rainbow(length(lbls)))
#From the pie chart its evident  a majority of the people didn't use either of the companies i.e 380.
#Company A and B combined had the majority use.

#Bar graph for a better visualization
ggplot(laterite,aes(x=mm_account_telco,y=prop.table(stat(count)),
                    fill=mm_account_telco,label=scales::percent(prop.table(stat(count)))))+
  scale_y_continuous(labels=scales::percent)+
  geom_bar()+
  geom_text(stat = "count",vjust=-1,size=2.7)+
  scale_fill_viridis_d()+
  labs(title = "Distribution of mobile market \n among the three companies",y="Percentage")+
  theme(axis.text.x = element_text(face = "bold",angle = 90,size = 7))

#The individual companies with most users was company A followed by B then C had the least.
#A majority  of the people used both A and B i.e 27.47%

#4.
laterite2<-laterite
laterite2$failed_urban<-ifelse(laterite2$urban=="Urban" &laterite2$v240=="yes",1,0)
laterite2$rular<-ifelse()




#5.varaibles that are strong evidence that one will cancel their mobile money account
library(caret)
library(randomForest)
laterite_dummy<-laterite
dmy<-dummyVars("~.",data = laterite_dummy,fullRank = T)
laterite_dummy<-data.frame(predict(dmy,newdata=laterite_dummy))

model_rf<-randomForest(mm_account_cancelledyes~.,data=laterite_dummy,importance=TRUE)
rf_importance <- varImp(model_rf) 
rf_importance

#Plotting variable importances
ggplot(data = rf_importance, mapping = aes(x = rf_importance[,1])) + 
  geom_boxplot() + 
  labs(title = "Variable importance: ") +
  theme_light() 
