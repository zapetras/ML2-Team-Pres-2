#### Objective is to create a robust variable using EXT_SOURCE variables and not use them in the main model ####

library(car)
library(pROC)
gc()

application_test = read.csv("../input/application_test.csv")
application_train = read.csv("../input/application_train.csv")

train = application_train[,c("SK_ID_CURR" , 
                             "EXT_SOURCE_1" , 
                             "EXT_SOURCE_2" , 
                             "EXT_SOURCE_3" ,
                             "TARGET")]

test = application_test[,c("SK_ID_CURR" , 
                           "EXT_SOURCE_1" , 
                           "EXT_SOURCE_2" , 
                           "EXT_SOURCE_3" )]

rm(application_train , application_test)

train$EXT_SOURCE_1[is.na(train$EXT_SOURCE_1)] = 999999
train$EXT_SOURCE_2[is.na(train$EXT_SOURCE_2)] = 999999
train$EXT_SOURCE_3[is.na(train$EXT_SOURCE_3)] = 999999

test$EXT_SOURCE_1[is.na(test$EXT_SOURCE_1)] = 999999
test$EXT_SOURCE_2[is.na(test$EXT_SOURCE_2)] = 999999
test$EXT_SOURCE_3[is.na(test$EXT_SOURCE_3)] = 999999

train$EXT_SOURCE_1_woe = ifelse(train$EXT_SOURCE_1<=0.1580071 & train$EXT_SOURCE_1 > 0,-1.07758802402084,
                                ifelse(train$EXT_SOURCE_1<=0.2124817,-0.66828550498736,
                                       ifelse(train$EXT_SOURCE_1<=0.2564344,-0.483504231342617,
                                              ifelse(train$EXT_SOURCE_1<=0.296219,-0.311609369202929,
                                                     ifelse(train$EXT_SOURCE_1<=0.3339857,-0.176570210366635,
                                                            ifelse(train$EXT_SOURCE_1<=0.3692444,-0.0844532588065556,
                                                                   ifelse(train$EXT_SOURCE_1<=0.403899,-0.16960227962122,
                                                                          ifelse(train$EXT_SOURCE_1<=0.4379984,0.105630431078415,
                                                                                 ifelse(train$EXT_SOURCE_1<=0.4721585,0.180501912299328,
                                                                                        ifelse(train$EXT_SOURCE_1<=0.5059604,0.221363064985234,
                                                                                               ifelse(train$EXT_SOURCE_1<=0.5400087,0.283545558353166,
                                                                                                      ifelse(train$EXT_SOURCE_1<=0.5731644,0.349489943719169,
                                                                                                             ifelse(train$EXT_SOURCE_1<=0.6065448,0.494201502627933,
                                                                                                                    ifelse(train$EXT_SOURCE_1<=0.6403405,0.469754393653925,
                                                                                                                           ifelse(train$EXT_SOURCE_1<=0.6750196,0.604563728472206,
                                                                                                                                  ifelse(train$EXT_SOURCE_1<=0.7100435,0.696956055212795,
                                                                                                                                         ifelse(train$EXT_SOURCE_1<=0.7461033,0.852735033969215,
                                                                                                                                                ifelse(train$EXT_SOURCE_1<=0.7866066,0.909496351933442,
                                                                                                                                                       ifelse(train$EXT_SOURCE_1<=0.8321812,1.07049454489204,
                                                                                                                                                              ifelse(train$EXT_SOURCE_1<=0.9626928,1.35429379123806,-0.0587218857200679))))))))))))))))))))

train$EXT_SOURCE_2_woe = ifelse(train$EXT_SOURCE_2<=0.1332941,-1.17279543733881,
                                ifelse(train$EXT_SOURCE_2<=0.2156633,-0.665826305525141,
                                       ifelse(train$EXT_SOURCE_2<=0.278936,-0.491709130884039,
                                              ifelse(train$EXT_SOURCE_2<=0.3397628,-0.403097577616425,
                                                     ifelse(train$EXT_SOURCE_2<=0.3924053,-0.300118417816109,
                                                            ifelse(train$EXT_SOURCE_2<=0.4404184,-0.154391458094307,
                                                                   ifelse(train$EXT_SOURCE_2<=0.479005,-0.0886198741157158,
                                                                          ifelse(train$EXT_SOURCE_2<=0.5121415,0.0130533359169237,
                                                                                 ifelse(train$EXT_SOURCE_2<=0.5409717,0.0719304594441333,
                                                                                        ifelse(train$EXT_SOURCE_2<=0.5659238,0.107051086194533,
                                                                                               ifelse(train$EXT_SOURCE_2<=0.5881965,0.210280041371224,
                                                                                                      ifelse(train$EXT_SOURCE_2<=0.6082811,0.209578478421491,
                                                                                                             ifelse(train$EXT_SOURCE_2<=0.6275815,0.31828713762916,
                                                                                                                    ifelse(train$EXT_SOURCE_2<=0.6458287,0.373852175896365,
                                                                                                                           ifelse(train$EXT_SOURCE_2<=0.6635878,0.49217370382358,
                                                                                                                                  ifelse(train$EXT_SOURCE_2<=0.6820309,0.581122231383335,
                                                                                                                                         ifelse(train$EXT_SOURCE_2<=0.7006819,0.627512300596299,
                                                                                                                                                ifelse(train$EXT_SOURCE_2<=0.7219853,0.767171855221493,
                                                                                                                                                       ifelse(train$EXT_SOURCE_2<=0.7476599,0.978864222046116, 1.05506941911431)))))))))))))))))))

train$EXT_SOURCE_3_woe = ifelse(train$EXT_SOURCE_3<=0.1538026,-1.24370806927982,
                                ifelse(train$EXT_SOURCE_3<=0.2250869,-0.828721251543278,
                                       ifelse(train$EXT_SOURCE_3<=0.2807896,-0.602694260908495,
                                              ifelse(train$EXT_SOURCE_3<=0.3264752,-0.435672319830297,
                                                     ifelse(train$EXT_SOURCE_3<=0.3656165,-0.21997028116992,
                                                            ifelse(train$EXT_SOURCE_3<=0.4014074,-0.171216408850566,
                                                                   ifelse(train$EXT_SOURCE_3<=0.4365065,-0.0386691218565073,
                                                                          ifelse(train$EXT_SOURCE_3<=0.4704561,0.0430242241649722,
                                                                                 ifelse(train$EXT_SOURCE_3<=0.5010752,0.171867861216299,
                                                                                        ifelse(train$EXT_SOURCE_3<=0.5280928,0.345475066632138,
                                                                                               ifelse(train$EXT_SOURCE_3<=0.5549468,0.353337829419081,
                                                                                                      ifelse(train$EXT_SOURCE_3<=0.58499,0.497361749500644,
                                                                                                             ifelse(train$EXT_SOURCE_3<=0.6092757,0.483537029238217,
                                                                                                                    ifelse(train$EXT_SOURCE_3<=0.6347055,0.590251685766761,
                                                                                                                           ifelse(train$EXT_SOURCE_3<=0.6577838,0.692241544525337,
                                                                                                                                  ifelse(train$EXT_SOURCE_3<=0.6848277,0.676915044017857,
                                                                                                                                         ifelse(train$EXT_SOURCE_3<=0.7121552,0.813301602716983,
                                                                                                                                                ifelse(train$EXT_SOURCE_3<=0.7407991,0.872147061454559,
                                                                                                                                                       ifelse(train$EXT_SOURCE_3<=0.7738957,0.906718796839085,
                                                                                                                                                              ifelse(train$EXT_SOURCE_3<=0.8256357,1.01775527548481,
                                                                                                                                                                     ifelse(train$EXT_SOURCE_3<=0.8960095,1.01295655237986,-0.156352568479553 )))))))))))))))))))))
                                                                                                                                                                            
                                                                                                                                                              
                                                                                                                                                                     
test$EXT_SOURCE_1_woe = ifelse(test$EXT_SOURCE_1<=0.1580071 & test$EXT_SOURCE_1 > 0,-1.07758802402084,
                                ifelse(test$EXT_SOURCE_1<=0.2124817,-0.66828550498736,
                                       ifelse(test$EXT_SOURCE_1<=0.2564344,-0.483504231342617,
                                              ifelse(test$EXT_SOURCE_1<=0.296219,-0.311609369202929,
                                                     ifelse(test$EXT_SOURCE_1<=0.3339857,-0.176570210366635,
                                                            ifelse(test$EXT_SOURCE_1<=0.3692444,-0.0844532588065556,
                                                                   ifelse(test$EXT_SOURCE_1<=0.403899,-0.16960227962122,
                                                                          ifelse(test$EXT_SOURCE_1<=0.4379984,0.105630431078415,
                                                                                 ifelse(test$EXT_SOURCE_1<=0.4721585,0.180501912299328,
                                                                                        ifelse(test$EXT_SOURCE_1<=0.5059604,0.221363064985234,
                                                                                               ifelse(test$EXT_SOURCE_1<=0.5400087,0.283545558353166,
                                                                                                      ifelse(test$EXT_SOURCE_1<=0.5731644,0.349489943719169,
                                                                                                             ifelse(test$EXT_SOURCE_1<=0.6065448,0.494201502627933,
                                                                                                                    ifelse(test$EXT_SOURCE_1<=0.6403405,0.469754393653925,
                                                                                                                           ifelse(test$EXT_SOURCE_1<=0.6750196,0.604563728472206,
                                                                                                                                  ifelse(test$EXT_SOURCE_1<=0.7100435,0.696956055212795,
                                                                                                                                         ifelse(test$EXT_SOURCE_1<=0.7461033,0.852735033969215,
                                                                                                                                                ifelse(test$EXT_SOURCE_1<=0.7866066,0.909496351933442,
                                                                                                                                                       ifelse(test$EXT_SOURCE_1<=0.8321812,1.07049454489204,
                                                                                                                                                              ifelse(test$EXT_SOURCE_1<=0.9626928,1.35429379123806,-0.0587218857200679))))))))))))))))))))

test$EXT_SOURCE_2_woe = ifelse(test$EXT_SOURCE_2<=0.1332941,-1.17279543733881,
                                ifelse(test$EXT_SOURCE_2<=0.2156633,-0.665826305525141,
                                       ifelse(test$EXT_SOURCE_2<=0.278936,-0.491709130884039,
                                              ifelse(test$EXT_SOURCE_2<=0.3397628,-0.403097577616425,
                                                     ifelse(test$EXT_SOURCE_2<=0.3924053,-0.300118417816109,
                                                            ifelse(test$EXT_SOURCE_2<=0.4404184,-0.154391458094307,
                                                                   ifelse(test$EXT_SOURCE_2<=0.479005,-0.0886198741157158,
                                                                          ifelse(test$EXT_SOURCE_2<=0.5121415,0.0130533359169237,
                                                                                 ifelse(test$EXT_SOURCE_2<=0.5409717,0.0719304594441333,
                                                                                        ifelse(test$EXT_SOURCE_2<=0.5659238,0.107051086194533,
                                                                                               ifelse(test$EXT_SOURCE_2<=0.5881965,0.210280041371224,
                                                                                                      ifelse(test$EXT_SOURCE_2<=0.6082811,0.209578478421491,
                                                                                                             ifelse(test$EXT_SOURCE_2<=0.6275815,0.31828713762916,
                                                                                                                    ifelse(test$EXT_SOURCE_2<=0.6458287,0.373852175896365,
                                                                                                                           ifelse(test$EXT_SOURCE_2<=0.6635878,0.49217370382358,
                                                                                                                                  ifelse(test$EXT_SOURCE_2<=0.6820309,0.581122231383335,
                                                                                                                                         ifelse(test$EXT_SOURCE_2<=0.7006819,0.627512300596299,
                                                                                                                                                ifelse(test$EXT_SOURCE_2<=0.7219853,0.767171855221493,
                                                                                                                                                       ifelse(test$EXT_SOURCE_2<=0.7476599,0.978864222046116, 1.05506941911431)))))))))))))))))))

test$EXT_SOURCE_3_woe = ifelse(test$EXT_SOURCE_3<=0.1538026,-1.24370806927982,
                                ifelse(test$EXT_SOURCE_3<=0.2250869,-0.828721251543278,
                                       ifelse(test$EXT_SOURCE_3<=0.2807896,-0.602694260908495,
                                              ifelse(test$EXT_SOURCE_3<=0.3264752,-0.435672319830297,
                                                     ifelse(test$EXT_SOURCE_3<=0.3656165,-0.21997028116992,
                                                            ifelse(test$EXT_SOURCE_3<=0.4014074,-0.171216408850566,
                                                                   ifelse(test$EXT_SOURCE_3<=0.4365065,-0.0386691218565073,
                                                                          ifelse(test$EXT_SOURCE_3<=0.4704561,0.0430242241649722,
                                                                                 ifelse(test$EXT_SOURCE_3<=0.5010752,0.171867861216299,
                                                                                        ifelse(test$EXT_SOURCE_3<=0.5280928,0.345475066632138,
                                                                                               ifelse(test$EXT_SOURCE_3<=0.5549468,0.353337829419081,
                                                                                                      ifelse(test$EXT_SOURCE_3<=0.58499,0.497361749500644,
                                                                                                             ifelse(test$EXT_SOURCE_3<=0.6092757,0.483537029238217,
                                                                                                                    ifelse(test$EXT_SOURCE_3<=0.6347055,0.590251685766761,
                                                                                                                           ifelse(test$EXT_SOURCE_3<=0.6577838,0.692241544525337,
                                                                                                                                  ifelse(test$EXT_SOURCE_3<=0.6848277,0.676915044017857,
                                                                                                                                         ifelse(test$EXT_SOURCE_3<=0.7121552,0.813301602716983,
                                                                                                                                                ifelse(test$EXT_SOURCE_3<=0.7407991,0.872147061454559,
                                                                                                                                                       ifelse(test$EXT_SOURCE_3<=0.7738957,0.906718796839085,
                                                                                                                                                              ifelse(test$EXT_SOURCE_3<=0.8256357,1.01775527548481,
                                                                                                                                                                     ifelse(test$EXT_SOURCE_3<=0.8960095,1.01295655237986,-0.156352568479553 )))))))))))))))))))))

model_lr = glm(TARGET~ 
                 EXT_SOURCE_1_woe +
                 EXT_SOURCE_2_woe +
                 EXT_SOURCE_3_woe,
               family = binomial(link = "logit") , data = train)

summary(model_lr)
vif(model_lr)
new_var_train = predict(model_lr , train , type = "response")
auc(train$TARGET , new_var_train)
                 
new_var_test = predict(model_lr , test , type = "response")
train_new_var = data.frame(train , new_var_train)
test_new_var = data.frame(test , new_var_test)
test_new_var = test_new_var[,c("SK_ID_CURR" , "new_var_test")]
train_new_var = train_new_var[,c("SK_ID_CURR" , "new_var_train")]
colnames(test_new_var)[2] = "TARGET"

rm(train , test )

write.csv(test_new_var , file = "submission_1.csv" , row.names = FALSE)
write.csv(train_new_var , file = "train_new_var.csv" , row.names = FALSE)

