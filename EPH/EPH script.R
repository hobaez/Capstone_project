if(!require(tidyverse)) install.packages("tidyverse", repos="http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos="http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos="http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos="http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos="http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos="http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos="http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos="http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos="http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos="http://cran.us.r-project.org")

#Loading the datasets=======================================
eph_train <- read_sav("REG02_EPHC_ANUAL_2017.SAV")

eph_test <- read_sav("REG02_EPHC_ANUAL_2018.SAV")

#Exploring the train dataset=============================================
class(eph_train)

class(eph_test)

glimpse(eph_train)
dim(eph_train)


#Income distribution by department
eph_train%>%
  filter(as.numeric(E01A)<999999999)%>%
  filter(E01A!=0)%>%
  group_by(DPTO)%>%
  ggplot(aes(x=as_factor(DPTO), y=log10(as.numeric(E01A))))+
  geom_boxplot()+
  coord_flip()+
  ylab("Log income")+
  xlab("Department")+
  ggtitle("Ingresos por departamento")+
  theme_economist()

#Income distribution by department
eph_train%>%
  filter(as.numeric(E01A)<999999999)%>%
  filter(E01A!=0)%>%
  group_by(DPTO)%>%
  ggplot(aes(x=as_factor(AREA), y=log10(as.numeric(E01A))))+
  geom_boxplot()+
  ylab("Log income")+
  xlab("Area")+
  ggtitle("Ingresos por departamento")+
  theme_economist()

#Income by ID type
eph_train%>%
  filter(E01A<999999999)%>%
  filter(E01A!=0)%>%
  group_by(P04A)%>%
  ggplot(aes(y=log10(as.numeric(E01A)), x=(as_factor(P04A))))+
  geom_boxplot()+
  ylab("Income")+
  xlab("ID type")+
  theme_economist()

#Income by contract type (temporal-permanent, with-without invoice)
eph_train%>%
  filter(E01A<999999999)%>%
  filter(E01A!=0)%>%
  filter(B26%in%c("1",'2','3','4'))%>%
  ggplot(aes(y=log10(as.numeric(E01A)), x=(as.factor(B26))))+
  geom_boxplot(aes(fill=as_factor(B26)))+
  ylab("Log Income")+
  xlab("Contract type")+
  labs(fill="Contract type")+
  theme_economist()

#Income by retirement fund
eph_train%>%
  filter(E01A<999999999)%>%
  filter(E01A!=0)%>%
  group_by(B11)%>%
  ggplot(aes(y=log10(as.numeric(E01A)), x=as.factor(B11)))+
  geom_boxplot(aes(fill=as_factor(B11)))+
  ylab("Log Income")+
  xlab("Retirement Fund")+
  labs(fill="Retirement Fund")+
  theme_economist()

#Years of study by decil of income
eph_train%>%
  filter(añoest%in%c('0',"1","2","3","4",'5','6','7','8','9','10','11','12','13','14','15','16','17','18'))%>%
  filter(decili%in%c('0',"1","2","3","4",'5','6','7','8','9','10'))%>%
  ggplot(aes(y=as.numeric(añoest),x=as.factor(decili)))+
  geom_violin()+
  ylab("Years of study")+
  xlab("Decil")+
  theme_economist()

#Years of study by quintile
eph_train%>%
  filter(añoest%in%c('0',"1","2","3","4",'5','6','7','8','9','10','11','12','13','14','15','16','17','18'))%>%
  filter(quintili%in%c('0',"1","2","3","4",'5'))%>%
  ggplot(aes(y=as.numeric(añoest),x=as.factor(quintili)))+
  geom_violin()+
  ylab("Years of study")+
  xlab("Quintil")+
  theme_economist()

#Language spoken by area
eph_train%>%
  filter(ED01%in%c("1","2","3","4"))%>%
  ggplot(aes(x=as_factor(ED01)))+
  geom_bar(aes(fill=as_factor(AREA)))+
  ylab("Count")+
  xlab("Language spoken")+
  labs(fill="Area")+
  theme_economist()

#Years of study by language spoken
eph_train%>%
  filter(ED01%in%c("1","2","3","4"))%>%
  filter(añoest%in%c('0',"1","2","3","4",'5','6','7','8','9','10',
                     '11','12','13','14','15','16','17','18'))%>%
  group_by(ED01)%>%
  ggplot(aes(x=as_factor(ED01), y=as.numeric(añoest)))+
  geom_violin()+
  ylab("Study years")+
  xlab("Language spoken")+
  theme_economist()

#Literacy by language spoken
eph_train%>%
  filter(ED01%in%c("1","2","3","4"))%>%
  filter(ED02%in%c("1","6"))%>%
  ggplot(aes(x=as_factor(ED01)))+
  geom_bar(aes(fill=as_factor(ED02)))+
  ylab("Count")+
  xlab("Language spoken")+
  labs(fill="Literate")+
  theme_economist()
  
#Language spoken by department
eph_train%>%
  filter(ED01%in%c("1","2","3","4"))%>%
  ggplot(aes(x=as_factor(DPTO)))+
  geom_bar(aes(fill=as_factor(ED01)))+
  coord_flip()+
  ylab("Count")+
  xlab("Department")+
  labs(fill="Language spoken")+
  theme_economist()

#Years of study by department
eph_train%>%
  filter(añoest%in%c('0',"1","2","3","4",'5','6','7','8','9','10','11','12','13','14','15','16','17','18'))%>%
  ggplot(aes(y=as.numeric(añoest),x=as_factor(DPTO)))+
  geom_violin()+
  coord_flip()+
  ylab("Years of study")+
  xlab("Department")+
  theme_economist()  

#Highest degree by department
eph_train%>%
  filter(ED06C%in%c("1","2","3","4","14"))%>%
  group_by(ED06C)%>%
  ggplot(aes(x=as_factor(DPTO)))+
  geom_bar(aes(fill=as_factor(ED06C)))+
  coord_flip()+
  ylab("Count")+
  xlab("Department")+
  labs(fill="Highest degree - Simplified")+
  theme_economist()

#Dropout by reason and sex
eph_train%>%
  filter(ED10%in%c("1","2","3","4","5","6","7","8","9","10","11","12",'13',"14","15","16","17","18"))%>%
  group_by(ED10)%>%
  ggplot(aes(x=as.factor(ED10)))+
  geom_bar(aes(fill=as_factor(P06)))+
  scale_y_log10()+
  ylab("Count")+
  xlab("Department")+
  labs(fill="Drop-out reason")+
  theme_economist()

#Summarize dropout reasons by sex
eph_train%>%
  filter(ED10%in%c("1","2","3","4","5","6","7","8","9","10","11","12",'13',"14","15","16","17","18"))%>%
  group_by(ED10)%>%
  summarize(male_prop=mean(P06==1), female_prop= 1-male_prop)

#Study years by sex
eph_train%>%
  filter(ED10%in%c("1","2","3","4","5","6","7","8","9","10","11","12",'13',"14","15","16","17","18"))%>%
  ggplot(aes(x=as_factor(P06), y=as.numeric(ED10)))+
  geom_violin()+
  xlab("Sex")+
  ylab("Study years")+
  theme_economist()

#Scholar kit by department
eph_train%>%
  filter(ED11B9%in%c("1","6"))%>%
  group_by(ED11B9)%>%
  ggplot(aes(x=as_factor(DPTO)))+
  geom_bar(aes(fill=as_factor(ED11B9)))+
  ylab("Count")+
  xlab("Department")+
  labs(fill="Scholar kit")+
  theme_economist()

#Scholar lunch by department
eph_train%>%
  filter(ED11F1%in%c("1","6"))%>%
  group_by(ED11F1)%>%
  ggplot(aes(x=as_factor(DPTO)))+
  geom_bar(aes(fill=as_factor(ED11F1)))+
  ylab("Count")+
  xlab("Department")+
  labs(fill="Scholar lunch")+
  theme_economist()

#Dropout by department and sex
eph_train%>%
  filter(ED10%in%c("1","2","3","4","5","6","7","8","9","10","11","12",'13',"14","15","16","17","18"))%>%
  group_by(ED10)%>%
  ggplot(aes(x=as.factor(DPTO)))+
  geom_bar(aes(fill=as_factor(P06)))+
  ylab("Count")+
  xlab("Department")+
  labs(fill="Drop-out sex")+
  theme_economist()

#Years of study by health insurance
eph_train%>%
  filter(añoest%in%c('0',"1","2","3","4",'5','6','7','8','9','10','11','12','13','14','15','16','17','18'))%>%
  filter(S01A%in%c("1","2","3","4",'5','6','7'))%>%
  ggplot(aes(y=as.numeric(añoest),x=as_factor(S01A)))+
  geom_violin()+
  coord_flip()+
  ylab("Years of study")+
  xlab("Health insurance")+
  theme_economist()

#Summarize departments population by area
eph_train%>%
  group_by(as_factor(DPTO))%>%
  summarize(urban_prop=mean(AREA==1), rural_prop= 1-urban_prop)

#Departments by area
eph_train%>%
  ggplot(aes(x=as_factor(DPTO)))+
  geom_bar(aes(fill=as_factor(AREA)))+
  ylab("Count")+
  xlab("Department")+
  labs(fill="Area")+
  theme_economist()#

#Years of study by area
eph_train%>%
  filter(añoest%in%c('0',"1","2","3","4",'5','6','7','8','9','10','11','12','13','14','15','16','17','18'))%>%
  ggplot(aes(y=as.numeric(añoest),x=as_factor(AREA)))+
  geom_violin()+
  coord_flip()+
  ylab("Years of study")+
  xlab("Area")+
  theme_economist()


#Model training============================================
#Creating a `desertor` variable
eph_train <- eph_train %>% filter(!is.na(ED0504))%>%filter(!is.na(decili))%>%
  filter(S01A%in%c("1","2","3","4",'5','6','7'))%>%
  mutate(desertor = as.factor(ifelse(ED0504 < 503 &ED0504 != 409 & !is.na(ED10), "desertor", "no_desertor")))

#Transforming labelled variables into regular factor variables
eph_train <- eph_train %>% mutate_at(
  vars('P06', 'DPTO', 'AREA', 'ED01', 'S01A'),
  funs(as_factor(.))
)

#Proportion of desertors
mean(eph_train$desertor=="desertor")

#Splitting into train and test sets
test_index <- createDataPartition(eph_train$desertor, times=1, p=0.2, list = F)
train_set <- eph_train[-test_index,]
test_set <- eph_train[test_index,]

#Creating a `desertor` variable for the validation set
eph_test <- eph_test %>% filter(!is.na(ED0504))%>% filter(!is.na(decili))%>%
  filter(S01A%in%c("1","2","3","4",'5','6','7'))%>%
  mutate(desertor = as.factor(ifelse(as.numeric(ED0504) < 503 &ED0504 != 409 & !is.na(ED10), "desertor", "no_desertor")))

#Transforming labelled variables into regular factor variables
eph_test <- eph_test %>% mutate_at(
  vars('P06', 'DPTO', 'AREA', 'ED01', 'S01A'),
  funs(as_factor(.))
)


#Creating the metric function
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}

#Creatting the weights for the imbalanced model
model_weights <- ifelse(train_set$desertor == "desertor",
                        (1/table(train_set$desertor)[1]) * 0.5,
                        (1/table(train_set$desertor)[2]) * 0.5)
sum(model_weights)#The sum MUST equals 1

#Bayesian general linear model===============
glmbayes<-train(desertor ~P02+P06+DPTO+AREA+ED01+decili+S01A, 
                data = train_set,
                method="bayesglm",
                weights = model_weights,
                metric="F1",
                trControl = trainControl(summaryFunction = f1,
                                         method = "cv",
                                         number = 3,
                                         classProbs = TRUE))
glmbayes$results

#Prediction for glmbayes 
pred_glm<-predict(glmbayes, test_set)

#Confusion matrix for prediction vs observer
confusionMatrix(pred_glm, test_set$desertor, mode="prec_recall")

#F1 score
F_meas(pred_glm, test_set$desertor)

#Results dataframe
results<- data.frame(Method="Bayesian GLM",
                     F1_Score=F_meas(pred_glm, test_set$desertor),
                     Sensitivity=sensitivity(pred_glm, test_set$desertor))
results

#naive_bayes method==================
naive_bayes<-train(desertor ~ P02+P06+DPTO+AREA+ED01+decili+S01A, 
                   data = train_set,
                   method="naive_bayes",
                   weights = model_weights,
                   metric="F1",
                   trControl = trainControl(summaryFunction =f1,
                                            method = "cv",
                                            number = 3,
                                            classProbs = TRUE))
naive_bayes$results

#Predicting with `naive bayes` method
pred_naive_bayes<-predict(naive_bayes, test_set)

#Confusion matrix prediction vs observation
confusionMatrix(pred_naive_bayes, test_set$desertor, mode="prec_recall")

#F1 score
F_meas(pred_naive_bayes, test_set$desertor)

#Results dataframe
results<- bind_rows(results,data.frame(Method="Naive Bayes",
                                       F1_Score=F_meas(pred_naive_bayes, test_set$desertor),
                                       Sensitivity=sensitivity(pred_naive_bayes, test_set$desertor)))
results

#Random forest with `rpart` method=======
cart<-train(desertor ~P02+P06+DPTO+AREA+ED01+decili+S01A, 
           data = train_set,
           method="rpart",
           weights = model_weights,
           metric="F1",
           trControl = trainControl(summaryFunction = f1,
                                    method = "cv",
                                    number = 3,
                                    classProbs = TRUE),
           tuneGrid= data.frame(cp=seq(0.0,0.1, length = 25)))
cart$results

#Prediction with the `rpart` randomforest method
pred_cart<- predict(cart, test_set)

#Confusion martix prediction vs observation
confusionMatrix(pred_cart, test_set$desertor, mode="prec_recall")

#F1 score
F_meas(pred_cart, test_set$desertor)

#Results dataframe
results<- bind_rows(results,data.frame(Method="Rpart",
                                       F1_Score=F_meas(pred_cart, test_set$desertor),
                                       Sensitivity=sensitivity(pred_cart, test_set$desertor)))
results

#Final Validation
#Creatting the weights for the imbalanced final model
model_weights <- ifelse(eph_train$desertor == "desertor",
                        (1/table(eph_train$desertor)[1]) * 0.5,
                        (1/table(eph_train$desertor)[2]) * 0.5)
sum(model_weights)#The sum MUST equals 1

validation<-train(desertor ~P02+P06+DPTO+AREA+ED01+decili+S01A, 
            data = eph_train,
            method="rpart",
            weights = model_weights,
            metric="F1",
            trControl = trainControl(summaryFunction = f1,
                                     method = "cv",
                                     number = 3,
                                     classProbs = TRUE),
            tuneGrid= data.frame(cp=seq(0.0,0.1, length = 25)))
validation$results

#Prediction with the `rpart` randomforest method
pred_valid<- predict(validation, eph_test)

#Confusion martix prediction vs observation
confusionMatrix(pred_valid, eph_test$desertor, mode="prec_recall")

#F1 score
F_meas(pred_valid, eph_test$desertor)

#Results dataframe of final validation
results<- bind_rows(results,data.frame(Method="Final validation - rpart",
                                       F1_Score=F_meas(pred_valid, eph_test$desertor),
                                       Sensitivity=sensitivity(pred_valid, eph_test$desertor)))
results

