

train <- read.csv(file="D:/R Analytics/Analytics Vidhya/Loan Prediction/train_u6lujuX_CVtuZ9i (1).csv",na.strings=c(""," ","NA"))
str(train)
head(train,15)

test <- read.csv(file="D:/R Analytics/Analytics Vidhya/Loan Prediction/test_Y3wMUE5_7gLdaTN.csv",na.strings=c(""," ","NA"))

submission_file <- read.csv(file="D:/R Analytics/Analytics Vidhya/Loan Prediction/sample_submission_S7jWYrJ (1).csv",na.strings=c(""," ","NA"))

dim(train)
dim(test)

test$Loan_Status <- "0"

combi <- rbind(train, test)


str(combi)

mystats <- function(x){
  if(class(x)=="numeric"){
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  s <- sd(a)
  min <- min(a)
  max <- max(a)
  q1 <- quantile(a,0.01)
  q95 <- quantile(a,0.95)
  q99 <- quantile(a,0.99)
  out1 <- m+3*s
  iqr <- IQR(a)
  Q3 <- quantile(a,0.75)
  Q1 <- quantile(a,0.25)
  out2 <- Q3+1.5*iqr
  out3 <- Q1-1.5*iqr
  return(c(nmiss=nmiss, mean=m,sdv=s,min=min,max=max,q1=q1,q95=q95,q99=q99,out1=out1,out2=out2,out3=out3))
  }
  else{
    var_type <- class(x)
    nmiss <- sum(is.na(x))
    return(c(var_type=var_type,nmiss=nmiss))
  }
  
}

num_var <- sapply(combi,is.numeric)
other_var <- !sapply(combi,is.numeric)

diag_stats1 <- t(data.frame(apply(combi[num_var],2,mystats)))
diag_stats2 <- t(data.frame(apply(combi[other_var],2,mystats)))


write.csv(diag_stats1, file="D:/R Analytics/Analytics Vidhya/Loan Prediction/diag_stats1.csv")
write.csv(diag_stats2, file= "D:/R Analytics/Analytics Vidhya/Loan Prediction/diag_stats2.csv")

combi <- subset(combi, select = -c(Loan_ID))

combi$ApplicantIncome[combi$ApplicantIncome>25671] <- 25671
combi$CoapplicantIncome[combi$CoapplicantIncome>11034] <- 11034
combi$LoanAmount[combi$LoanAmount>484] <- 484


library(ggplot2)
library(cowplot)

p1 <- ggplot(combi)+geom_histogram(aes(Loan_Amount_Term), fill = "blue", binwidth = 20, breaks=seq(0,500,by=150))

p2 <- ggplot(combi)+geom_bar(aes(Gender), fill="coral1")
p3 <- ggplot(combi)+geom_bar(aes(Self_Employed), fill="coral1")
p4 <- ggplot(combi)+geom_bar(aes(Married), fill="coral1")

plot_grid(p1,p2,p3,p4, ncol=4)

mean(combi$ApplicantIncome[which(combi$Gender=="Male")])
mean(combi$ApplicantIncome[which(combi$Gender=="Female")])

missing_index <- which(is.na(combi$Gender))
for(i in missing_index){
  combi$Gender[i] <- ifelse(combi$ApplicantIncome[i]>=mean(combi$ApplicantIncome[which(combi$Gender=="Male")]),"Male","Female")
}

combi$Married[is.na(combi$Married) == TRUE ] <- "Yes"
combi$Married <- as.factor(combi$Married)


missing_index1 <- which(is.na(combi$Dependents))
for(i in missing_index1){
  combi$Dependents[i] <- ifelse(combi$Married[i]=="yes",1,0)
}

combi$Self_Employed[is.na(combi$Self_Employed)==TRUE] <- "No"
combi$Self_Employed <- as.factor(combi$Self_Employed)
str(combi)

for(i in which(is.na(combi$LoanAmount))){
  combi$LoanAmount[i] <- mean(combi$LoanAmount[combi$Property_Area==combi$Property_Area[[i]]],na.rm=T)
}

sum(is.na(combi$LoanAmount))


combi$Property_Area_new <- ifelse(combi$Property_Area=="Rural",1,
                                  
                                  ifelse(combi$Property_Area=="Semiurban",2,3))

combi$Property_Area <- NULL

combi$Loan_Status_new <- ifelse(combi$Loan_Status=="Y",1,0)

combi$Loan_Status <- NULL



str(combi)

combi$Loan_Amount_Term[is.na(combi$Loan_Amount_Term)==TRUE] <-  which.max(prop.table(table(combi$Loan_Amount_Term)))

combi$Credit_History[is.na(combi$Credit_History)==TRUE] <-  which.max(prop.table(table(combi$Credit_History)))

combi$Dependents_new <- ifelse(combi$Dependents=="0",0,
                               ifelse(combi$Dependents=="1",1,
                                      ifelse(combi$Dependents=="2",2,3)))
combi$Dependents <- NULL

####################### New Variable #############################################

combi$total_income <- combi$ApplicantIncome+combi$CoapplicantIncome
combi$EMI <- combi$LoanAmount/combi$Loan_Amount_Term
combi$loan_to_income <- ifelse(combi$total_income=="0",0,combi$LoanAmount/combi$total_income)
combi$Balance_Income <- (combi$total_income-(combi$LoanAmount))
combi$coapp_to_AppIncome <- combi$CoapplicantIncome/combi$total_income

combi$ApplicantIncome <- NULL
combi$ApplicantIncome <- NULL

library(fastDummies)
library(dplyr)

combi <- dummy_cols(combi)

combined <- dplyr::select(combi,-c("Gender","Married","Education","Self_Employed"))

library(stringr)

names(combined)<-str_replace_all(names(combined), c(" " = "_"))



training <- combined[1:nrow(train),]
testing <- combined[nrow(train)+1:nrow(test),]

testing$Loan_Status_new <- NULL

training$Loan_Status_new <- as.factor(training$Loan_Status_new)

library(caret)
library(e1071)

set.seed(1234)
custom <- trainControl(method="cv",
                       number=10)

grid <- expand.grid(maxdepth=8)

fit2 <- train(Loan_Status_new~.,metric="Accuracy",method="rf",trControl=custom,maximise=F,data=training)



t1 <- cbind(training, prediction = predict(fit2,training))
table(t1$Loan_Status_new,t1$prediction)


submission_file$Loan_Status = predict(fit2, testing)
submission_file$Loan_Status <- ifelse(submission_file$Loan_Status==1,"Y","N")
submission_file <- subset(submission_file, select = c(Loan_ID,Loan_Status))


write.csv(submission_file,file="D:/R Analytics/Analytics Vidhya/Loan Prediction/sample_submission_S7jWYrJ (1).csv")

#### As this is for practice , not using other algo to improve the accuracy ###############



# colnames(training)
# 
# 
# 
# library(xgboost)
# library(Matrix)
# 
# training$Loan_Status_new <- as.integer(training$Loan_Status_new)-1
# 
# 
# xg_matrix_train <- sparse.model.matrix(Loan_Status_new~	EMI+	Gender_Female+	Married_Yes+	Self_Employed_No+	LoanAmount+	Loan_Amount_Term+	loan_to_income+	Gender_Male+	
#                                          Education_Graduate+	Self_Employed_Yes+	
#                                          Credit_History+	Dependents_new+	Balance_Income+	Married_No+	Education_Not_Graduate+	
#                                          Property_Area_new+	total_income-1, data = training)
# 
# xg_matrix_test <- sparse.model.matrix(~EMI+	Gender_Female+	Married_Yes+	Self_Employed_No+	LoanAmount+	Loan_Amount_Term+	loan_to_income+	Gender_Male+	
#                                         Education_Graduate+	Self_Employed_Yes+	
#                                         Credit_History+	Dependents_new+	Balance_Income+	Married_No+	Education_Not_Graduate+	
#                                         Property_Area_new+	total_income-1, data= testing)
# 
# xgb_training <- xgb.DMatrix(as.matrix(xg_matrix_train),label=training$Loan_Status_new)
# 
# xgb_testing <- xgb.DMatrix(as.matrix(xg_matrix_test))

col_maxdep <- c()
col_sample <- c()
col_colsam <- c()
col_gamma <- c()
xgb_index <- c()
xgb_error <- c()

for(i in c(6,7,8,9,10)){
  for(j in c(0.7,0.75,0.8,0.85)){
    for(k in c(0.5,0.6,0.7,0.8)){
      for(l in seq(1,5,1)){
      
      param1 <- list(objective="binary:logistic", eta=0.001,gamma=l,eval_metric="error",max_depth=i,subsample=j,colsample_bytree=k,seed=123)
      
      fit <- xgb.cv(params=param1,xgb_training,nrounds=1000,nfold=5,early_stopping_rounds = 30,print_every_n = 30,maximise=F)
    
      col_maxdep <- c(col_maxdep,i)
      col_sample <- c(col_sample,j)
      col_colsam <- c(col_colsam,k)
      col_gamma <- c(col_gamma,l)
      fit_index <- fit$best_iteration
      min_error <- fit$evaluation_log[fit_index]$test_error_mean
      xgb_index <- c(xgb_index,fit_index)
      xgb_error <- c(xgb_error,min_error)
      }
  }
}
}

result <- data.frame(col_maxdep=col_maxdep,col_sample=col_sample,
                     col_colsam=col_colsam,col_gamma=col_gamma,xgb_index=xgb_index,xgb_error=xgb_error)


a <- min(result$xgb_error)
b <- result[which(result$xgb_error==min(result$xgb_error)),]

param <- list(objective="binary:logistic", eta=0.001,gamma=3,eval_metric="error",max_depth=6,subsample=0.75,colsample_bytree=0.7,seed=123)


fit3 <- xgb.train(data= xgb_training, params = param, nrounds=67)

xgb_t1 <- cbind(testing, predict(fit3, xgb_testing)) ####these are probablities, need to have cut off#########

submission_file$Loan_Status = predict(fit3, xgb_testing)

submission_file$Loan_Status <- ifelse(submission_file$Loan_Status > 0.50,"Y","N")

write.csv(submission_file,file="D:/R Analytics/Analytics Vidhya/Loan Prediction/sample_submission_S7jWYrJ (1).csv")

############# Final ##############################################
