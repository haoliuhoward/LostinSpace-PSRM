
# To get the results from the China data, you only need to run this code


##### Set up #####
rm(list=ls())

# Program the frequently used paths
base.path<- "         "  #set the root directory of the project as the base.path  (default: "base.path<-getwd()" )
r.path<-paste0(base.path, "/Rcodes")
china.path<-paste0(base.path, "/china")

# Load packages/ helper functions (manually defined)
setwd(r.path) 
source("setup.R", echo=T, print.eval = T)
source("functions.R", echo=T, print.eval = T)

# Load dictionaries pre-defined for the dataset
setwd(china.path)
source("dictionary_china.R", echo=T, print.eval = T)
## --> "china_province" & "names" have been created.
## --> four types of dictionaries have been created as well.

#Load data
china_data<-read.csv("china_cleantext.csv") #text file pre-treated. (output from text_treatment_china.R)
#make sure that the texts, province_human, province_ICEWS are all character strings
# this data file contains a pretreated text variable named as 'cleantext'
china_data$cleantext<-as.character(china_data$cleantext)
# correct location coded by human coder
china_data$province_human<-as.character(china_data$province_human)
# locations coded by ICEWS
china_data$province_ICEWS<-as.character(china_data$province_ICEWS)



######## Step 1:  Feature Selection ########


k<-9 # three 3-fold cross validation; 
rf<-svm<-nnet<-logit<-rep(NA, k)
variable_selection<-predicted<-s.id<-probabilities<-list(NULL)
set.seed(2015)
sampled1<-sample(1:3, nrow(china_data), replace=TRUE)
set.seed(2016)
sampled2<-sample(1:3, nrow(china_data), replace=TRUE)
set.seed(2017)
sampled3<-sample(1:3, nrow(china_data), replace=TRUE)


for(i in 1:k){
  ## Split the data into 3 sets: corpus set, train set, test set
  set.seed(2014+i)
  
  if(i==1|i==2|i==3){
    sampled<-sampled1
    t<-i
  }
  if(i==4|i==5|i==6){
    sampled<-sampled2
    t<- i-3
  }
  if(i==7|i==8|i==9){
    sampled<-sampled3
    t<- i-6
  }
  
  #set1 - train set ; set2 - test set
  set2<-china_data[sampled==t,]
  set1<-china_data[sampled!=t,]
  
  
  #### Step 1-a: build corpus -- set1 (training set) ####
  data<-set1 
  # creating the "incorrect" Ngram corpus 
  Ngrams_incorrect<-incNgrams(texts=as.character(data$cleantext),     #corpora built using only the Ngrams from the training set
                                namespace=names,
                                solution=as.character(data$province_human), 
                                min=2, max=7)
  # creating the "correct" Ngram corpus  
  Ngrams_correct<-corNgrams(texts=as.character(data$cleantext),     #corpora built using only the Ngrams from the training set
                              namespace=names,
                              solution=as.character(data$province_human),
                              min=2, max=7)
  
  
  #### Step 1-b: feature selection -- set1 (still the same training set) ####
  data<-set1
  
  # build the Dependent variable based on human coding (1: correct event location, 0: otherwise)
  binary_data<-buildY(texts=data$cleantext,  
                      namespace=names,
                      solution=as.character(data$province_human), 
                      text_no=data$story_id)
  
  # VAR 1 - Ngram) Ngram variable type 1 - rates within articles
  ngrams_article<-buildNgram1(texts=data$cleantext, 
                              namespace=names, 
                              Ngrams_correct=Ngrams_correct, 
                              Ngrams_incorrect=Ngrams_incorrect, 
                              min=2, max=7, 
                              within="article") 
  # VAR 1 - Ngram) Ngram variable type 1 - rates within data
  ngrams_data<-buildNgram1(texts=data$cleantext, 
                           namespace=names, 
                           Ngrams_correct=Ngrams_correct, 
                           Ngrams_incorrect=Ngrams_incorrect, 
                           min=2, max=7,
                           within="data") 
  # VAR 1 - Ngram) Ngram variable type 2- rates within articles
  ngrams_matched_article<-buildNgram2(texts=data$cleantext, 
                                      namespace=names, 
                                      Ngrams_correct=Ngrams_correct, 
                                      Ngrams_incorrect=Ngrams_incorrect, 
                                      min=2, max=7, 
                                      within="article", 
                                      nontopic.word="nontopic", topic.word="AVERB")
  # VAR 1 - Ngram) Ngram variable type 2-  rates within data
  ngrams_matched_data<-buildNgram2(texts=data$cleantext, 
                                   namespace=names, 
                                   Ngrams_correct=Ngrams_correct, 
                                   Ngrams_incorrect=Ngrams_incorrect, 
                                   min=2, max=7,
                                   within="data",
                                   nontopic.word="nontopic", topic.word="AVERB")
  
  
  # VAR 2 - Frequency) how many times does this loc word appear?) 
  # both within article (compared to other location words in the article?) 
  # and within data (compared to other location words in the entire data set?) generated together
  frequency<-buildFrequency(texts=data$cleantext, namespace=names, text.id=data$story_id)
  
  # VAR 3 - Context ) how many relevant words (eg. action verbs & key nouns) does the text contain? - output: both across (how many action verbs?) and within article ratios
  materiality<-buildContext(texts=data$cleantext, solutions=data$province_human, namespace=names, verbs="AVERB")
  # VAR 3 - Context ) how many IRrelevant words (eg. irrelvant verbs & nouns) does the text contain? - output: both across (how many action verbs?) and within article ratios
  imateriality<-buildContext(texts=data$cleantext, solutions=data$province_human, namespace=names, verbs="NONTOPIC")
  # compute Ngram frequencies and build data frame for covariates (training set )
 
  
#combine all the produced variables (Y, X: Ngram frequencies, frequencies, & context)
  data_train<-as.data.frame(cbind(binary_data,
                                  ngrams_article, 
                                  ngrams_data,
                                  ngrams_matched_article,
                                  ngrams_matched_data,
                                  "frequency_data"=as.numeric(as.character(frequency$frequency_data)),
                                  "frequency_article"=as.numeric(as.character(frequency$frequency_article)),
                                  "materiality_data"=as.numeric(as.character(materiality$words_matched_data)),
                                  "materiality_article"=as.numeric(as.character(materiality$words_matched_article)),
                                  "imateriality_data"=as.numeric(as.character(imateriality$words_matched_data)),
                                  "imateriality_article"=as.numeric(as.character(imateriality$words_matched_article))
  ) )
  
  
  
  
  #### Step 1-c: feature selection -- set2 test set ####
  data<-set2
  # build the Dependent variable based on human coding (1: correct event location, 0: otherwise)
  binary_data<-buildY(texts=data$cleantext,  
                      namespace=names,
                      solution=as.character(data$province_human), 
                      text_no=data$story_id)
  
  # VAR 1 - Ngram) Ngram variable type 1 - rates within articles
  ngrams_article<-buildNgram1(texts=data$cleantext, 
                              namespace=names, 
                              Ngrams_correct=Ngrams_correct, 
                              Ngrams_incorrect=Ngrams_incorrect, 
                              min=2, max=7, 
                              within="article") 
  # VAR 1 - Ngram) Ngram variable type 1 - rates within data
  ngrams_data<-buildNgram1(texts=data$cleantext, 
                           namespace=names, 
                           Ngrams_correct=Ngrams_correct, 
                           Ngrams_incorrect=Ngrams_incorrect, 
                           min=2, max=7,
                           within="data") 
  # VAR 1 - Ngram) Ngram variable type 2- rates within articles
  ngrams_matched_article<-buildNgram2(texts=data$cleantext, 
                                      namespace=names, 
                                      Ngrams_correct=Ngrams_correct, 
                                      Ngrams_incorrect=Ngrams_incorrect, 
                                      min=2, max=7, 
                                      within="article", 
                                      nontopic.word="nontopic", topic.word="AVERB")
  # VAR 1 - Ngram) Ngram variable type 2-  rates within data
  ngrams_matched_data<-buildNgram2(texts=data$cleantext, 
                                   namespace=names, 
                                   Ngrams_correct=Ngrams_correct, 
                                   Ngrams_incorrect=Ngrams_incorrect, 
                                   min=2, max=7,
                                   within="data",
                                   nontopic.word="nontopic", topic.word="AVERB")
  
  
  # VAR 2 - Frequency) how many times does this loc word appear?) 
  # both within article (compared to other location words in the article?) 
  # and within data (compared to other location words in the entire data set?) generated together
  frequency<-buildFrequency(texts=data$cleantext, namespace=names, text.id=data$story_id)
  
  # VAR 3 - Context ) how many relevant words (eg. action verbs & key nouns) does the text contain? - output: both across (how many action verbs?) and within article ratios
  materiality<-buildContext(texts=data$cleantext, solutions=data$province_human, namespace=names, verbs="AVERB")
  # VAR 3 - Context ) how many IRrelevant words (eg. irrelvant verbs & nouns) does the text contain? - output: both across (how many action verbs?) and within article ratios
  imateriality<-buildContext(texts=data$cleantext, solutions=data$province_human, namespace=names, verbs="NONTOPIC")
  # compute Ngram frequencies and build data frame for covariates (training set )
  
  
  #combine all the produced variables (Y, X: Ngram frequencies, frequencies, & context)
  data_test<-as.data.frame(cbind(binary_data,
                                  ngrams_article, 
                                  ngrams_data,
                                  ngrams_matched_article,
                                  ngrams_matched_data,
                                  "frequency_data"=as.numeric(as.character(frequency$frequency_data)),
                                  "frequency_article"=as.numeric(as.character(frequency$frequency_article)),
                                  "materiality_data"=as.numeric(as.character(materiality$words_matched_data)),
                                  "materiality_article"=as.numeric(as.character(materiality$words_matched_article)),
                                  "imateriality_data"=as.numeric(as.character(imateriality$words_matched_data)),
                                  "imateriality_article"=as.numeric(as.character(imateriality$words_matched_article))
  ) )
  
  
  ######## Step 2: Classification Algorithms ########
  # clean up the 'Doparallel' environment for caret required for variable selection
  cl <- makeCluster(2)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  set.seed(2014+i) # set the same seed again for our classifiers that have stochastic components
  
  
  #### Step 2-a: Random Forest ####
  #trainining set
  data<-data_train[complete.cases(data_train),] #the data created might contain NAs but the classifiers won't take NA values
  
  # Variable selection: run the RFE algorithm for variable combinations
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  results <- rfe(data[,4:57], data$Y,
                 rfeControl=control, verbose = FALSE) 
  variable_selection[[i]]<-results
  plot(results, type=c("g", "o"))
  no2keep<-results$results$Variables[which(results$results$Accuracy==max(results$results$Accuracy))]
  temp<- as.data.frame(results$fit$importance)
  trial2<-row.names(temp)[order(temp, decreasing=TRUE)][1:no2keep]
  
  # classifier
  data2<-as.data.frame(cbind("Y"=data$Y, data[, names(data) %in% trial2]) )
  form<-as.formula(paste("Y~", paste(names(data2)[c(2:ncol(data2))], collapse="+") ) )
  randomf<-randomForest(form,data=data2,ntree=1000)
  varImpPlot(randomf, sort=TRUE)
  
  #test set - validation
  pred<-data_test[complete.cases(data_test),c(4:57)]
  pred2<-pred[, names(pred) %in% trial2]
  pred.rf<-predict(randomf, pred2 , type="response") 
  predicted.data.rf<-predict(randomf, pred2, type="prob")
  probabilities[[i]]<-predicted.data.rf[,which(colnames(predicted.data.rf)=="1")] # probabilities that the observation has Y = 1 (true event location) 
  rf[i]<- mean(pred.rf==data_test$Y[complete.cases(data_test)]) # if you want to see the score, check out the value in this line
  predicted[[i]]<-pred.rf
  
  
  # #### Step 2-b: SVM ####
  #tune parameters
  tune.out=tune.svm(form, data=data2, kernel="radial", decision.values = TRUE, probability=TRUE)
  
  # test set - validation
  pred.svm = predict(tune.out$best.model, pred2, decision.values = TRUE, probability = TRUE)
  predicted.data.svm<-attr(pred.svm, "probabilities") 
  probabilities[[i+k]]<-predicted.data.svm[,which(colnames(predicted.data.svm)=="1")] 
  svm[i]<-mean(pred.svm == data_test$Y[complete.cases(data_test)]) # if you want to see the score, check out the value in this line
  predicted[[i+k]]<-pred.svm
  
  
  #### Step 2-c:  Neural Net ####
  #tune parameters
  tune.nn <- train(form, data = data2, method = "nnet", verbose=FALSE) 
  size<-tune.nn$finalModel$n[2]
  decay<-tune.nn$finalModel$decay
  nn <- nnet(form, data=data2, size=size, decay=decay)
  # test set - validation
  nn.probs <- predict(nn, pred2)
  summary(nn.probs)
  pred.nnet = rep(NA, length(data_test$Y[complete.cases(data_test)]))
  pred.nnet[nn.probs  >= 0.5] <- 1
  pred.nnet[nn.probs  < 0.5] <- 0
  # table(nn.pred, data_test$Y[complete.cases(data_test)])
  nnet[i]<-mean(pred.nnet == data_test$Y[complete.cases(data_test)] ) # if you want to see the score, check out the value in this line
  predicted[[i+(2*k)]]<- pred.nnet
  probabilities[[i+(2*k)]]<-nn.probs
  
  s.id[[i]]<-paste0(data_test$text_id[complete.cases(data_test)], data_test$location_names[complete.cases(data_test)])
  
#### logit: base line #### -- how much of the accuracy rates is about the classifiers?
logit.model<-glm(form, data=data2, family = binomial(link="logit") )
logit.prediction<-predict(logit.model, pred2, type="response")
logit.prediction[logit.prediction>=0.5] <-1
logit.prediction[logit.prediction<0.5]<-0
logit[i]<-mean(logit.prediction == data_test$Y[complete.cases(data_test)] ) 

}


summary(rf)
summary(svm)
summary(nnet)
summary(logit)

####### Want to examine the Ngram lists?
# View(Ngrams_correct[[1]])
# View(Ngrams_incorrect[[1]])
# View(Ngrams_correct[[2]])
# View(Ngrams_incorrect[[2]])





######### SAVING RESULTS ############


#just to make sure that files are saved in the correct directory!
results_china<-as.data.frame(cbind(rf,svm,nnet, logit))
china.results.path<-paste0(china.path, "/results")
setwd(china.results.path)
write.csv(results_china, "results_china.csv")

# save the predicted values for graphics
for ( i in 1:k) {
  dat<-paste0("seed_",(2014+i) )
  fram<-as.data.frame(cbind( as.character(s.id[[i]])  ,
                             as.numeric(as.character(predicted[[i]])), 
                             as.numeric(as.character(predicted[[i+k]])), 
                             as.numeric(as.character(predicted[[i+(2*k)]])) ) )
  names(fram)<-c("story_id", paste0("rf_",(2014+i)), paste0("svm_",(2014+i)), paste0("nnet_",(2014+i))) 
  assign(dat, fram)
  write.csv(fram, paste0("predicted_",dat,".csv"))
}

rm(fram)
# save the predicted PROBABILITIES 
for ( i in 1:k) {
  dat<-paste0("seed_",(2014+i) )
  fram<-as.data.frame(cbind( as.character(s.id[[i]]),
                             as.numeric(as.character(probabilities[[i]])), 
                             as.numeric(as.character(probabilities[[i+k]])), 
                             as.numeric(as.character(probabilities[[i+(2*k)]])) ) )
  names(fram)<-c("story_id", paste0("rf_",(2014+i)), paste0("svm_",(2014+i)), paste0("nnet_",(2014+i))) 
  assign(dat, fram)
  write.csv(fram, paste0("probabilities_",dat,".csv"))
}


# human coded provinces (we will compare the machined coded provinces to this)
china_all<-buildY(texts=china_data$cleantext, namespace=names,
                  solution=china_data$province_human, text_no=china_data$story_id)
china_all$id<-paste0(china_all$text_id, china_all$location_names)


#read in data!
for( i in 1:k){
  fram<-read.csv(paste0("probabilities_seed_",2014+i,".csv"))
  dat<-paste0("data_",2014+i)
  assign(dat, fram)
}
# create a data set named as temp that contain every predicted value
temp<-Reduce(function(...) merge(..., all=TRUE), list(data_2015, data_2016, data_2017, data_2018, data_2019, 
                                                      data_2020, data_2021, data_2022, data_2023
))


#options: if you want to plot each model differently
rf_temp<-temp[,c(1,2,3,6,9,12,15,18,21,24,27)]
svm_temp<-temp[,c(1,2,4,7,10,13,16,19,22,25,28)]
nnet_temp<-temp[,c(1,2,5,8,11,14,17,20,23,26,29)]


classifiers<-c("svm", "rf", "nnet")
for( cl in classifiers) {
  name<-paste0(cl,"_temp")
  temp<-get(name) # opposite of assign(name, object)
  
  
  # extract and combine values to plot 
  prob<-id<-NULL #id= story_id + location name
  id.list<-unique(as.character(temp$story_id))
  for( i in 1: length(id.list) ) { #each location word was in each classifier three times
    prob<-c(prob, my.sum( temp[which(temp$story_id==id.list[i]),(3:ncol(temp))]) /3 ) # combine! and get the average
    id<-c(id, as.character(id.list[i]) )
  }
  
  machine.predicted<-rep(0, length(prob))
  machine.predicted[prob>=0.5]<-1
  
  assign(paste0("machine.predicted.",cl), machine.predicted)
  assign(paste0("machine.probabilities.",cl), prob)
  
}

#original value coded by human
y<-rep(NA, length(id))
for( i in 1:length(id)){
  y[i]<-as.numeric(as.character(china_all$Y[which(id[i]==china_all$id)] ) )
}

china_results.all<-as.data.frame(cbind(id, "original"=y,
                                       machine.predicted.rf, machine.predicted.svm, machine.predicted.nnet, 
                                       machine.probabilities.rf, machine.probabilities.svm, machine.probabilities.nnet))
write.csv(china_results.all, "china_results.all.csv")






####### End of 2_algorithm_china.R ########
