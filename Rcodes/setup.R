

##### Packages #####
# data management
# library(magrittr) # for speedy processing of data management

# text data management
# library(stringr) #string matching
# library(plyr) # ddply function in the parse_sentences function
# library(tidyr) #spread function in the parse_sentences function
# library(ngram) #concatenate function
datamanagement<-c("stringr","plyr","tidyr","ngram","magrittr")

# text analysis
# library(tm) #for VCorpus - (Volatile Corpora), stem words, frequencies etc.
# library(RWeka) #for tokenizing based on the ngram model
# library("openNLPmodels.en") # NLP dictionary for English
# library("NLP")
# library("openNLP") 

textanalysis=c("tm","RWeka")
               
# for classification algorithms
classification=c("randomForest","e1071","neuralnet") #random forest, svm, & neuralnetworks

# packages for plotting
plotting=c('RgoogleMaps',"AUC")



##### Helper functions #####

## package control
package = function(names)
{
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")
    
    library(name, character.only=TRUE)
  }
}


#for Synonyms functions
# package('wordnet')
# # initDict()
# setDict("/WordNet-3.0/dict")

package(datamanagement)

package(textanalysis)

package(classification)

# package(plotting) 

package('doParallel') # some of our key functions use parallel cores

package('caret') # for tuning

