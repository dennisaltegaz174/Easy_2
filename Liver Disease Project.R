# Reading the datafile
liver_data<-read.csv("C:/Users/adm/Documents/Datasets/Indian Liver Patient Dataset (ILPD).csv")

# loading all the required libraries
library(psych)
library(ggplot2)
library(caret)
library(VIM)
library(mice)
library(kernlab)
library(randomForest)
library(caretEnsemble)
library(shiny)
library(shinydashboard)

#Data Exploration
str(liver_data)
#Using the  str() analyze the basic structure of the data.The data has 11 varaibles and 583 observations
#Response variable is is_patient which is an integer value with values 1 and 2.2indicates patient with liver disease while
# 1  represents patients without liver  disease.Gender is  a  chr with two levels male and  females, while other variables are int and nun datatypes

#Convert gender to factor
liver_data$gender<-as.factor(liver_data$gender)

d<-describeBy(liver_data)
d
# from the skew values its evident that there is skewness present in the data.The features like Aspartate_Aminotransferase and Alamine_Aminotransferase,
#THe mean trimmed value  which indicates the mean of a dataset by trimming outliers,is widely diffrent from the actual mean indicating presence of outliers

#Exploratory Data plots
temp<-liver_data
temp$is_patient<-as.factor(liver_data$is_patient)
temp$gender<-as.numeric(temp$gender)

#Plotting pie chart of attribute is_patient
mytable<-table(temp$is_patient)
mytable
lbs<-c("Patients without liver Disease","Patient withn liver Disease")
lbs<-paste(lbs, "\n", mytable)
pie(mytable,labels = lbs,
    main="Pie Chart of  is_patient \n (With sample size)",
    col = rainbow(length(lbs)))
#From the pie chart i inferred that the number of patients with liver disease (1) is 416  which is more than 
# the number of patients record without liver disease (2)

#Plotting Histogram of Attribute Gender according to Response attribute is_patient
ggplot(data = temp,aes(x=gender))+
  geom_histogram(barwidth=0.2,color="black",aes(fill=is_patient),position = "dodge")+
  xlab("Gender")+
  ylab("Is_patient")+
  ggtitle("Histogram of Gender")
# From the Gender histogram i inferred that the number of male  records is more than the number of female Records and
# that the number of males with liver disease (1) i.e >100 is also greater than Female

#Further exploring the skewness of the data by plotting a histogram
par(mfrow=c(3,3))
col_hist<-c("darkred","khaki","steelblue","orange","yellow","green","purple","pink")
hist(liver_data$age,col = "#999999",border = "black",las=1,xlab = "Age",
     main = "Histogram of Age")
#Plotting for all features
for (i in 3:10) {
  hist(liver_data[,i],cex.axis=.5,col = col_hist[i-2],las=1,xlab = names(liver_data)[i],
       main = paste("Histogram of",names(liver_data)[i]))
  
}
# I realized that high right skewness is present in features Alkaline phosphates,total bilirubin,
#alamine aminostransfer,Direct bilirubin and aspartate aminotranfe

#Visualizing the data using ggplot
library(reshape2)
ggplot(data = melt(liver_data),mapping = aes(x=value,fill=variable))+
  geom_histogram(bins = 30)+
  facet_wrap(~variable,scales = "free_x")
#To handle the left skewness in the data i analyzed log transformation of the data and plotted the histograms
ggplot(data = melt(liver_data),mapping = aes(x=log(value),fill=variable))+
  geom_histogram(bins = 30,fill="steelblue")+
  facet_wrap(~variable,scales = "free_x")
#There was some slight improvement in the data.Log transformation wount be of much use here


#Density plot for each attribute by class value shows the distribution of the data for each attribute
#with respect to feature is_patient
x<-temp[,1:5]
y<-temp[,11]
scales<-list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y,plot = "density",scales=scales)

xx<-temp[,5:10]
y<-temp[,11]
scales<-list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=xx,y=y,plot = "density",scales=scales)
#pink Density graph indicates a  density plot wit features with liver disease and blue indicates features without liver disease


##Analyzing the distribution of age  w.r.t  response variable using box plot and mapping a jitter over it
ggplot(temp,aes(x=factor(1),y=age))+
  geom_boxplot(width=0.4,fill="white")+
  geom_jitter(aes(color=is_patient,shape=is_patient),
              width = 0.1,size=1)+
  scale_color_manual(values=c("darkred","darkgreen"))+
  labs(x=NULL)

#Detecting outliers
#Using Box plots to analyze outliers
par(mfrow=c(2,2))
ggplot(data = melt(liver_data),aes(x=value,fill=variable))+
  geom_boxplot(outlier.colour = "darkgreen",outlier.shape = 5)+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap(~variable,scales = "free_x")+
  labs(title = "Boxplots")
# I analyzed that most of the data in "alkphos"  "sgpt" are outliers

#Using cook's distance which is a multivariate approach to depict outliers.
# showing the values which are 3 cooks  distance away from the mean as outliers

mod<-glm(is_patient~.,data = liver_data)
cooksd<-cooks.distance(mod)

plot(cooksd,pch="*",cex=2,main = "Influencial obs by cooks distance")
abline(h=3*mean(cooksd,na.rm = T),col="red")
text(x=1:length(cooksd)+1,y=cooksd,labels = ifelse(cooksd>3*mean(cooksd,na.rm=T),names(cooksd),""),
col="red")


#Correlation and colinearity analysis
#using the scatter plot matrix
pairs.panels(liver_data,pch = 10)

#A long the diagonal the output shows the distribution of each variable i.e histogram
#The negative correlation between features indicates that as one goes up the other goes down.
# as in the case of total_bilirubin and albumin.This is logical as we know from data defination that incase of liver disease total bilirubin
#increases and albumin a protein made by liver decreases

# we notice that there is  a strong colinearity between features Total_albumi  and  Direct A lbumin
#,alamine_aminotransfarese aspartate_aminotransferase,total_proteins and albumin.
# Due to strong colinearity between features  total_bilirubin and direct_bilirubin,it may lead to overfitting of the model
# hence we eliminate one of them during model building

#Plotting for a more clearer view
pairs.panels(liver_data[c(3,4,6,7,8,9)])

#The bottom part shows scatter plots of different features

#/plotting a scatter plot of total bilirubin vs direct_bilirubin
ggplot(data = temp,aes(x=tot_bilirubin,y=direct_bilirubin))+
  geom_point(aes(color=is_patient,shape=is_patient))+
  labs(x="Total_bilirubin",y="Direct_Bilirubin",title = "Total_bilirubin-Direct_Bilirubin")


#Analyzing the missing values ins the dataset
table(is.na(liver_data))

#Number of missing values for each feature.
sapply(liver_data,function(x)sum(is.na(x)))

#Visualizing the missing values using the vim package
 missing_plot<-aggr(liver_data,col=c("navyblue","yellow"),numbers=T,sortvars=TRUE,
                    labels=names(liver_data),cex.axis=.5,gap=3,ylab=c("Missing data","Pattern"))
# I have visualized  a pattern of missing values  and noticed 99.31% of data is complete
 
 #DATA PREPATION 
 #data cleaning and shaping
 #imputing missing values
 #removing ouutliers
 #Normalizing the data
 
 #Feature engineering : Dummy codes
liver_data_dummy <- liver_data
# converting gender which is factor into dummy codes
dmy<-dummyVars("~.",data = liver_data_dummy,fullRank = T)
liver_data_dummy<-data.frame(predict(dmy,newdata=liver_data_dummy))
str(liver_data_dummy)

#converting is_patient to zero and 1
library(dplyr)
liver_data_dummy1<-liver_data_dummy%>%mutate(is_patient=if_else(is_patient== 1,1,0))
str(liver_data_dummy1)

#Imputing the missing values
# Using the predictive modeling for imputation from the mice package.This method uses regressio to impute missing values based on other values
#as  the missing  values are numeric il use the predictive mean matching pmm
#n=4 indicates that it will use 4  imputed datasets
#maxit =20 ,specifying the number of iterations
inputed_liver_dt_dummy<-mice(liver_data_dummy1,maxit = 20,method="pmm",seed=500)
summary(inputed_liver_dt_dummy)

# since 4 imputed  datasets are created, am using any of the  4 ie 2
liver_data_inputed_dummy<-complete(inputed_liver_dt_dummy,2)
#checking if there missig values
table(is.na(liver_data_inputed_dummy))
#since all values are false there are no missing values

#Handling outliers ( using the cooks distance)
# storing rows with outliers
influencial<-as.numeric(names(cooksd)[(cooksd>3*mean(cooksd,na.rm = T))])
influencial
#Dropping all the outliers
liver_data_dummy_imputed_noout<-liver_data_inputed_dummy[-influencial]

#Normalization or standardization of  feature values
#Creatingg the Normalize function
normalize<-function(x)
{
  return((x-mean(x))/sd(x)) #z scorenormalization
}

#Applying the normalization function to the data set apart from Gender and is_patient.
liver_data_dummy_imputed_noout_norm<-as.data.frame(lapply(liver_data_dummy_imputed_noout[,c(1,3:10)],normalize))

is_patient<-liver_data_dummy_imputed_noout$is_patient

gender<-liver_data_dummy_imputed_noout$gender.Male
#Adding gender and is_patient to normalized data
liver_data_dummy_imputed_noout_norm$gender<-gender
liver_data_dummy_imputed_noout_norm$is_patient<-is_patient

summary(liver_data_dummy_imputed_noout_norm)
#storing as clean data
liver_clean_data<-liver_data_dummy_imputed_noout_norm
str(liver_clean_data)


#CONSTRUCTING A MODEL
#Model construction and evaluation
#Creating training and validation data

set.seed(80)
liver_clean_data_lm<-liver_clean_data
# converting is_patient to a factor variable
liver_clean_data$is_patient<-as.factor(liver_clean_data$is_patient)

# Stratified portioning the data
index<-createDataPartition(liver_clean_data$is_patient,p=0.8,list = FALSE)

#splitting clean data with all features into training and testing data
train_data<-liver_clean_data[index,]
test_data<-liver_clean_data[-index,]

#Splitting the data with numeric response variable for logistic regression
test_data_lm<-liver_clean_data[index,]
train_data_lm<-liver_clean_data[-index,]


#Support vector Machine
set.seed(200)
model_svm<-ksvm(is_patient~.,data=train_data,kernel="tanhdot")

#Predicting values for test data
pred_svm<-predict(model_svm,test_data)

#confusion matrix for svm
confusionMatrix(pred_svm,test_data$is_patient)

# svm  has an accuracy of 61.21% .Number of false negatives is 22 is a good result

#Logistic Regression
model_glm<-glm(formula=is_patient~.,family=binomial,data=liver_clean_data_lm)
summary(model_glm)
#Considering features whose p-value is greater than 0.5 and dropping the rest
model_glm2<-glm(formula = is_patient~.,family = binomial,data = 
                  subset(liver_clean_data_lm,select = c(-gender,-tot_bilirubin,-direct_bilirubin,-ag_ratio)))
summary(model_glm2)
#Prediction
pred_lm<-predict(model_glm2,test_data_lm[,c(1,4,5,7,8,9)])
# Coverting probabilities to 0 and 1
pred_lm<-ifelse(pred_lm>=0.5,1,0)
# confusion matrix
confusionMatrix(as.factor(pred_lm),as.factor(test_data_lm$is_patient))
#An accuracy of 70.88 ,82 false negatives


#K-NearestNeighbour
ctrl<-trainControl(method = "repeatedcv",repeats = 3)
model_knn<-train(is_patient~.,data = train_data,method="knn",trControl=ctrl,preProcess=c("center","scale"),tuneLength=20)

#Predicting values for the test data
pred_knn<-predict(model_knn,newdata=test_data)
#confusionMatrix() knn
confusionMatrix(pred_knn,test_data$is_patient)
# KNN has an accuracy 0f 65.52 and fewer false negatives 8

#Random Forest
set.seed(100)
model_rf<-randomForest(is_patient~.,data=train_data,importance=TRUE)
#Prediction
pred_rf<-predict(model_rf,test_data)
#Confusin Matrix
confusionMatrix(pred_rf,test_data$is_patient)
#Random forest has a predicted accuracy of 65.52



#Comparison of the models
#storing the accuracy of the model
SVM<-confusionMatrix(pred_svm,test_data$is_patient)$overall['Accuracy']
GLM<-confusionMatrix(as.factor(pred_lm),as.factor(test_data_lm$is_patient))$overall['Accuracy']
KNN<-confusionMatrix(pred_knn,test_data$is_patient)$overall['Accuracy']
RF<-confusionMatrix(pred_rf,test_data$is_patient)$overall['Accuracy']

#Creating a dataframe of the model and model names
accuracy<-data.frame(model=c("SVM","GLM","KNN","RF"))
Accuracy<-c(SVM,GLM,KNN,RF)

#Plotting the model 
ggplot(accuracy,aes(x=model,y=Accuracy,fill=model))+
  geom_bar(stat = "identity")+
  theme_bw()+
  ggtitle("Comparision of MOdel Accuracy")
#Logistic regression has the highest accuracy

#Model ensemble
#Its combining models together and compare the models based on accuracy and kappa values
levels(liver_clean_data$is_patient)<-make.names(levels(liver_clean_data$is_patient))

#Creating train Control
control<-trainControl(method = "repeatedcv",number = 10,repeats = 3,savePredictions = T,classProbs = TRUE)

#Creating list of all algorithm to be ensembled
algorithmlist<-c("svmRadial","glm","knn","rf")

set.seed(200)

#Using caretlist to build the ensemble models listed in  algorithims list
models<-caretList(is_patient~.,data=liver_clean_data,trControl = control,methodList = algorithmlist)

output<-resamples(models)
summary(output)
dotPlot(output)

#Model improvement k-fold cross validation

set.seed(200)
#define train control for kfold cv
train_control_svm<-trainControl(method = "repeatedcv",number = 10)
#Fit svm
cross_model_svm<-train(is_patient~.,data = train_data,trControl=train_control_svm,
                       method="svmRadial")
#summarize the results
print(cross_model_svm)
#prediction
pred_svm_cross<-predict(cross_model_svm,test_data)
#confusion matrix
confusionMatrix(pred_svm_cross,test_data$is_patient)
