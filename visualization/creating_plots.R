
##### Graphics ########
base.path<- "    "  #set the root directory of the project as the base.path (  default: "base.path=getwd()"   )

### for ROC curves
require(AUC)

### for bubble maps
require(stringr)
require(RgoogleMaps)
# our built in AtoBinC function
setwd(paste0(base.path, "/Rcodes")) 
source("setup.R", echo=T, print.eval = T)
source("functions.R", echo=T, print.eval = T)


# Load results files
setwd(paste0(base.path, "/china/results"))
china_results.all<-read.csv("china_results.all.csv")
setwd(paste0(base.path, "/drc/results"))
drc_results.all<-read.csv("drc_results.all.csv")
setwd(paste0(base.path, "/syria/results"))
syria_results.all<-read.csv("syria_results.all.csv")


####### ROC Curve ########
par(mfrow=c(1,3))
par(mgp=c(2,0,0))
par(mar=c(3.5,3.5,3.5,3))

# China
plot(roc(china_results.all$machine.probabilities.nnet, as.factor(china_results.all$original)), lty=1, main="China", family="Garamond",
     cex.axis=1, cex.lab=1.5, cex.main=2) 
ob.svm<-roc(as.numeric(as.character(china_results.all$machine.probabilities.svm)), as.factor(china_results.all$original))
lines(ob.svm$fpr, ob.svm$tpr, lty=2, col='red')
ob.rf<-roc(as.numeric(as.character(china_results.all$machine.probabilities.rf)), as.factor(china_results.all$original) )
lines(ob.rf$fpr, ob.rf$tpr, lty=3, col='blue')
legend(0.6, 0.3, legend=c("NNet", "SVM", "R.For"), lty=1:3, col=c('black','red','blue'))


# DRC
par(mfrow=c(1,3))
par(mgp=c(2,0,0))
par(mar=c(3.5,3.5,3.5,3))
plot(roc(drc_results.all$machine.probabilities.nnet, as.factor(drc_results.all$original) ), lty=1, main="Congo (DRC)", family="Garamond",
     cex.axis=1, cex.lab=1.5, cex.main=2) 
ob.svm<-roc(drc_results.all$machine.probabilities.svm, as.factor(drc_results.all$original) )
lines(ob.svm$fpr, ob.svm$tpr, lty=2, col='red')
ob.rf<-roc(as.numeric(as.character(drc_results.all$machine.probabilities.rf)), as.factor(drc_results.all$original))
lines(ob.rf$fpr, ob.rf$tpr, lty=3, col='blue')
legend(0.6, 0.3, legend=c("NNet", "SVM", "R.For"), lty=1:3, col=c('black','red','blue'))


#Syria
plot(roc(syria_results.all$machine.probabilities.nnet, as.factor(syria_results.all$original) ), lty=1, main="Syria", family="Garamond",
     cex.axis=1, cex.lab=1.5, cex.main=2) 
ob.svm<-roc(as.numeric(as.character(syria_results.all$machine.probabilities.svm)), as.factor(syria_results.all$original) )
lines(ob.svm$fpr, ob.svm$tpr, lty=2, col='red')
ob.rf<-roc(as.numeric(as.character(syria_results.all$machine.probabilities.rf)), as.factor(syria_results.all$original) )
lines(ob.rf$fpr, ob.rf$tpr, lty=3, col='blue')
legend(0.6, 0.3, legend=c("NNet", "SVM", "R.For"), lty=1:3, col=c('black','red','blue'))





######## Bubble plots ###########
# Comparing the machine predicted Positives to the True positives
# The bubble plots were created in javascript using the plotly library, but the codes below generates the raw data for the bubble plots


### China

#Ground truth
originallycorrect.china<-china_results.all[which(china_results.all$original==1),]
table(str_replace_all(originallycorrect.china$id, "[0-9]", "")  ) # ground truth
# machine coded locations
china_results.all$machine.predicted.svm<-as.numeric(as.character(china_results.all$machine.predicted.svm))
predicted2bcorrect.china<-china_results.all[which(china_results.all$machine.predicted.svm>0.5),]
table(str_replace_all(predicted2bcorrect.china$id, "[0-9]", "") ) # the ones our algorithm predicted
# ICEWS
setwd(paste0(base.path,"/china"))
china_data<-read.csv('china_cleantext.csv')
table(str_trim( c(unlist(str_split(china_data$province_ICEWS, ",") ) ), side="both")) #ICEWS
# get coordinates
ss<-as.data.frame(as.character(names( table(str_replace_all(originallycorrect$id, "[0-9]", "")  ) )))
apply(ss, 1, getGeoCode) #part of RgoogleMaps package



### DRC
# prob.all<-as.data.frame(cbind( machine.probabilities.rf, machine.probabilities.svm, machine.probabilities.nnet))
# machine.predicted<-apply(prob.all, 1, sum)

#ground truth
originallycorrect.drc<-drc_results.all[which(drc_results.all$original==1),]
table(str_replace_all(originallycorrect.drc$id, "[0-9]", "")  )
# machine coded locations 
drc_results.all$machine.predicted.svm<-as.numeric(as.character(drc_results.all$machine.predicted.svm))
predicted2bcorrect.drc<-drc_results.all[which(drc_results.all$machine.predicted.svm>0.5),]
table(str_replace_all(predicted2bcorrect.drc$id, "[0-9]", "") )
# ICEWS
setwd(paste0(base.path,"/drc"))
drc_data<-read.csv('drc_cleantext.csv')
drc_province<-read.csv("drc_province.csv")
# table(str_trim( c(unlist(str_split(drc_data$province_ICEWS, ",") ) ), side="both"))
need2bcleaned<-c(unlist(str_split(drc_data$province_ICEWS, ",") ) )
need2bcleaned<-AtoBinC(drc_province$city, drc_province$province, need2bcleaned) 
need2bcleaned<-str_replace_all(need2bcleaned, "province", "")
need2bcleaned<-str_replace_all(need2bcleaned, "du ", "")
need2bcleaned[is.na(need2bcleaned)]<-"No_location"
table(str_trim( c(unlist(str_split(need2bcleaned, " ") ) ), side="both"))
# get coordinates
ss<-as.data.frame(as.character(names( table(str_replace_all(originallycorrect$id, "[0-9]", "")  ) )))
apply(ss, 1, getGeoCode) #require RgoogleMaps package





### Syria
# prob.all<-as.data.frame(cbind( machine.probabilities.rf, machine.probabilities.svm, machine.probabilities.nnet))
# machine.predicted<-apply(prob.all, 1, sum)

#ground truth
originallycorrect<-syria_results.all[which(syria_results.all$original==1),]
ss<-str_split(originallycorrect$id, "_")
syrian.original<-NULL
for(i in 1:length(ss)){
  syrian.original<-c(syrian.original, str_replace_all(ss[[i]][length(ss[[i]])], "[0-9]", "") )
}
unique(syrian.original) 
table(syrian.original)

# machine coded locations
syria_results.all$machine.predicted.svm<-as.numeric(as.character(syria_results.all$machine.predicted.svm))
predicted2bcorrect<-syria_results.all[which(syria_results.all$machine.predicted.svm>0.5),]
ss<-str_split(predicted2bcorrect$id, "_")
syrian.predicted<-NULL
for(i in 1:length(ss)){
  syrian.predicted<-c(syrian.predicted, str_replace_all(ss[[i]][length(ss[[i]])], "[0-9]", "") )
}
unique(syrian.predicted)
table(syrian.predicted)


#OEDA
table(str_trim( c(unlist(str_split(syria_data$province_oeda, ",") ) ), side="both"))
unique(str_trim( c(unlist(str_split(syria_data$province_oeda, ",") ) ), side="both"))


# Get coordinates
apply(as.data.frame(unique(syrian.original)), 1, getGeoCode) #RgoogleMaps
apply(as.data.frame(unique(str_trim( c(unlist(str_split(syria_data$province_oeda, ",") ) ), side="both"))), 1, getGeoCode) #RgoogleMaps


