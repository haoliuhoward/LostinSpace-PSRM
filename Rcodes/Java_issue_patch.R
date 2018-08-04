

# If you have problems with the 'RWeka' package, which is a JAVA based program, you can use these functions that do not rely on JAVA.
# run this code after running setup and functions so the corNgrams and incNgram functions can be overwritten.
# Ideally, you want to fix the JAVA issue on your computer sooner or later. 


corNgrams<-function(texts, namespace, solution, min, max){#build lists with ngrams of inccorrect location words and store as a list
  if(missing(min)){
    min=2
  }
  if(missing(max)){
    max=7
  }
  
  
  #parallelization for faster running time
  require(doParallel)#parallelization
  cluster=makeCluster(6)#parallelization
  registerDoParallel(cluster)#parallelization
  
  Ngrams_correct <<- foreach(m=min:max) %dopar%{#parallelization
    # call necessary functions in the new cluster  
    require(stringr) #str_detect
    require(RWeka) #NGramTokenizer
    
    
    Ngrams_correct=list(NULL)    
    n=length(texts)  
    phrases=NULL
    # for(m in min:max){
    
    
    for(i in 1:n){
      
      # search 
      TF<-str_detect(texts[i], namespace)
      if(length(which(TF==TRUE))>0){
        
        #define tokenizer manually
        Tokenizer <- function(x, m) {
          n<-m
          trim <- function(x) gsub("(^\\s+|\\s+$)", "", x)
          terms <- strsplit(trim(x), split = "\\s+")[[1]]
          ngrams <- vector()
          if (length(terms) >= n) {
            for (i in n:length(terms)) {
              ngram <- paste(terms[(i-n+1):i], collapse = " ")
              ngrams <- c(ngrams,ngram)
            }
          }
          ngrams
        }
        # end of tokenizer
        
        
        
        locations<-namespace[which(TF==TRUE)]
        test<-Tokenizer(texts[i], m=m)#manually defined function (better option: RWeka's tokenizer) 
        hu<-tolower(solution[i]); if(is.na(hu)){hu<-"EWW"}
        
        correct=NULL  #important: go through the each loc in the location list. (doesn't matter whether the length(hu)>1)
        for(c in 1:length(locations)){
          if(str_detect(hu, str_trim(locations[c], "both")     )){
            correct=c(correct,str_trim(locations[c], "both"))
          }
          
        }
        
        
        j=length(correct)
        
        if(j>0){
          for(a in 1:j){
            temp<-test[which(!is.na(str_match(test, correct[a])))]
            temp<-str_replace_all(temp, correct[a], "locz")
            phrases=c(phrases,temp)
          } 
        } else{
          phrases=c(phrases,NA)
        }
      }else{
        phrases=c(phrases,NA)
      }
      
    }  # end of function
    
    ## end of for loop
    matrix<-as.data.frame(table(phrases))
    Ngrams_correct[m]<-list(matrix[order(matrix$Freq, decreasing=TRUE),])  #create the number of Ngrams = m
    return(Ngrams_correct[m])
    
  } ## end of the foreach
  
  
  save(Ngrams_correct, file=".RData", envir=.GlobalEnv)
  stopCluster(cluster)
  
}





incNgrams<-function(texts, namespace, solution, min, max){ ##build lists with ngrams of inccorrect location words and store as a list
  if(missing(min)){
    min=2
  }
  if(missing(max)){
    max=7
  }
  
  
  
  #parallelization for faster running time
  require(doParallel)#parallelization
  cluster=makeCluster(6)#parallelization
  registerDoParallel(cluster)#parallelization
  
  Ngrams_incorrect <<- foreach(m=min:max) %dopar%{#parallelization
    # call necessary functions in the new cluster  
    library(stringr) #str_detect
    library(RWeka) #NGramTokenizer
    
    
    Ngrams_incorrect=list(NULL)
    n=length(texts)  
    
    phrases=NULL
    
    for(i in 1:n){
      #     newvar<-str_split(newvar[i], "\\.")[[1]]
      
      # search 
      TF<-str_detect(texts[i], namespace)
      
      
      
      if(length(which(TF==TRUE))>0){
        locations<-namespace[which(TF==TRUE)]
        
        # define the tokenizer function manually
        Tokenizer <- function(x, m) {
          n<-m
          trim <- function(x) gsub("(^\\s+|\\s+$)", "", x)
          terms <- strsplit(trim(x), split = "\\s+")[[1]]
          ngrams <- vector()
          if (length(terms) >= n) {
            for (i in n:length(terms)) {
              ngram <- paste(terms[(i-n+1):i], collapse = " ")
              ngrams <- c(ngrams,ngram)
            }
          }
          ngrams
        }
        # end of the tokenizer function
        
        
        
        ## for loop begins
        test<-Tokenizer(texts[i],m=m)  #manually defined function (better option: RWeka's tokenizer)  
        hu<-tolower(solution[i]); if(is.na(hu)){hu<-"EWW"}  #what the human code says
        wrong=NULL
        for(c in 1:length(locations)){
          if(str_detect(hu, str_trim(locations[c], "both") ) ){
            #if the location word was found in the human coded loc. list, nothing happens
          } else{
            wrong=c(wrong, str_trim(locations[c], "both") )
            #if the location word was NOT found in the human coded loc. list, consider it as an incorrect one.
          }
          
        }
        
        j=length(wrong)
        
        if(j>0){
          for(a in 1:j){
            temp<-test[which(!is.na(str_match(test, wrong[a])))]
            temp<-str_replace_all(temp, wrong[a], "locz")
            phrases=c(phrases,temp)
          } 
        } else{
          phrases=c(phrases,NA)
        }
      }else{
        phrases=c(phrases,NA)
      }
      
      
    }  
    
    matrix<-as.data.frame(table(phrases))
    Ngrams_incorrect[m]<-list(matrix[order(matrix$Freq, decreasing=TRUE),])  #create the number of Ngrams = m
    return(Ngrams_incorrect[m])
    
  } ## end of the foreach-loop
  
  #   return(results)
  save(Ngrams_incorrect, file=".RData", envir=.GlobalEnv)
  stopCluster(cluster)
  
}
