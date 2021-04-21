# Justin Corlett, Brian Beers, Zane Petras, Alex Russett


# Loading the necessary packages

library(xgboost)
library(data.table)
library(caret)


##########################################Reading the Data########################################


train<-fread("application_train.csv",select=c('SK_ID_CURR','TARGET','DAYS_BIRTH','DAYS_EMPLOYED'
                                                       ,'AMT_CREDIT','AMT_GOODS_PRICE'
                                                       ,'EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3'
                                                       ,'NAME_INCOME_TYPE','NAME_EDUCATION_TYPE'
                                                       ,'OCCUPATION_TYPE','REGION_RATING_CLIENT'
                                                       ,'REGION_RATING_CLIENT_W_CITY','ORGANIZATION_TYPE'
))
test<-fread("application_test.csv",select=c('SK_ID_CURR','DAYS_BIRTH','DAYS_EMPLOYED'
                                                     ,'AMT_CREDIT','AMT_GOODS_PRICE'
                                                     ,'EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3'
                                                     ,'NAME_INCOME_TYPE','NAME_EDUCATION_TYPE'
                                                     ,'OCCUPATION_TYPE','REGION_RATING_CLIENT'
                                                     ,'REGION_RATING_CLIENT_W_CITY','ORGANIZATION_TYPE'
))




########################################### Pre-processing ##########################################


# Creating the index
index<-1:nrow(train)
train_test<-rbind(train,test,fill=T)

# Encoding the categorical variables into numerical data

for (f in names(train_test)) {
  if (class(train_test[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- sort(unique(train_test[[f]]))
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}


train<-train_test[index,]
test<-train_test[-index,]

# Establishing the features
feats<-setdiff(names(train),c('TARGET','SK_ID_CURR'))




##################################### Modeling Process #######################################
xgb_params = list(
  eta = 0.1,
  objective = 'binary:logistic', #Binary because categorical
  eval_metric='auc', #1 vs rest. this is our ogoal
  colsample_bytree=0.7,
  subsample=0.7,
  min_child_weight=10
)

feats<-setdiff(names(train),'TARGET')
dtrainmat = xgb.DMatrix(as.matrix(train[,feats,with=FALSE]), label=train$TARGET)
dtestmat = xgb.DMatrix(as.matrix(test[,feats,with=FALSE]))

###################################### Fitting our Model ###############################
xgbmod<-xgb.train(xgb_params,dtrainmat,nrounds=125,verbose=2)
pred<-predict(xgbmod,dtestmat)

# Converting our numeric data to binary. We change this from number around to test our
# Results

prediction <- as.numeric(pred > 0.05)
print(head(prediction))

sub<-data.frame(SK_ID_CURR = test$SK_ID_CURR,TARGET=pred)
write.csv(sub,'baseline1.csv',row.names = F)

# We then compute our predictions to the above csv file and compare them in
# The CSV file with our 'Target.' This is how we get our accuracy rate