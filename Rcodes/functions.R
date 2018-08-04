
##### functions for pre-treatment #####


## this function computes the maximum value discarding NAs.
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)


## this function removes special characters defined 
cleaning<-function(text, pattern.list, option="none", ...){
  if(option=="whitespace"){
    sub<-" "
  } else {
    sub<-""
  }
  for(i in 1:length(pattern.list)){
    text=str_replace_all(text, pattern.list[i], sub)
  }
  return(text)
}


# this function cleans up the data
text_treatment<-function(newvar, stopwords, ...){
  l<-stopwords
  nv=rep(NA, length(newvar))
  for(i in 1:length(newvar)){
    text<-newvar[i]
    vc<-VCorpus(VectorSource(text))  
    vc<-tm_map(vc, removeWords, l) 
    vc<-tm_map(vc, stemDocument)
    #   vc<-tm_map(vc, content_transformer(tolower)) #better to do it outside (eg. Beijing)
    coerced<-as.String(vc[[1]]$content)
    nv[i]<-as.character(coerced)
  }
  return(nv)
}


#this function generalizes web addresses. relies on 'str_replace_all' from "stringr" 
generalize_webaddress<-function(newvar){
  nv=rep(NA, length(newvar))
  web<-c("http\\S+\\s*|http(.*?)/", "www.(.*?).|www.(.*?)/")
  for(i in 1:length(newvar)){
    coerced<-newvar[i]
    for(m in 1:length(web)){
      webZ<-paste0("\\b",web[m],"\\b")
      coerced<-str_replace_all(coerced, webZ, "WWW")    
    }
    nv[i]<-as.character(coerced)
  }
  return(nv)
}


# this function generalizes (converts) the words defined to the user defined word
generalize<-function(newvar, list, code="GENERAL", capture.nested=FALSE, ...){
  # rule: whenever the word in the "list" is captured, change it to the "code" word
  code<-code
  nv<-rep(NA, length(newvar))
  sources<-list
  
  if(capture.nested==TRUE){
    bb<-"." #captures nested words 
    cc<-"." 
    code<-paste0(" ",code," ")
  } else{
    bb<-"\\b" 
    cc<-"\\b"  #captures words as a single word only
  }
  
  for(i in 1:length(newvar)){
    coerced<-newvar[i]
    
    for(m in 1:length(sources)){
      capture<-paste0(bb,sources[m],cc)
      
      if(capture.nested==TRUE){
        coerced<-str_replace_all(coerced, capture, code)#replace phrases containing the set word  
        #           sub(capture, code, coerced, fixed=TRUE )
      } else { 
        coerced <-str_replace_all(coerced, capture, code) #replace phrases that exactly match the set word
      } 
      
    }
    
    nv[i]<-as.character(coerced)
  }
  
  return(nv)
}


#this function recognizes A (as a whole word) in text C and converts to B
AtoBinC<-function(A, B, C){ 

  A<-tolower(as.character(A))
  B<-tolower(as.character(B))
  C<-tolower(as.character(C))

    for ( i in 1:length(A)){
    pattern<-paste0("\\b",tolower(A[i]),"\\b")
    to<-paste0("\\",B[i], "\\")
    C<- str_replace_all(C, pattern, to)
  }

  return(as.character(C))
}


# this function replaces all numerals to N
generalize_numbers<-function(newvar){
  nv=rep(NA, length(newvar))
  numbers<-"([0-9]+)"
  for(i in 1:length(newvar)){
    nv[i]<-str_replace_all(newvar[i],numbers,"N")
  }
  return(nv)
}


## this function expands texts and grabs sentences with location words (sentences without loc. words discarded)
sentencify<-function(texts, solutions, namespace, text_id=c(1:length(texts))){#(the texts must include periods that mark the end of each sentence)
  id<-NULL
  sentences<-NULL
  sol<-NULL
  for( i in 1:length(texts)){
    text<-as.character(unlist(texts[i]))
    temp<-str_split(text, '\\.')[[1]]
    solution<-as.character(unlist(solutions[i]))
    
    TF<-str_detect(text, namespace)
    
    if(length(which(TF==TRUE))>0){
      locations<-namespace[which(TF==TRUE)]
      
      for(c in 1:length(locations)){
        
        
        l<-as.data.frame(sapply(temp, function(row) str_detect(row, locations[c]) ) )
        #     tf<-as.vector(apply(l, 2, any))
        #     temp<-as.data.frame(temp[tf])
        temp2<-as.character(unlist(temp[which(l==TRUE)]))
        
        
        id<-c(id, rep(text_id[i], length(temp2)) )
        sol<-c(sol, rep(solution, length(temp2)))
        sentences<-c(sentences, temp2)
      } # end of for loop - for the number of correct locations in each sentence
    } else {} # end of the if statement; else - no action
    
    
  } # end of for-loop: i
  
  return(as.data.frame(cbind(id, sentences, "correct_location"=sol) ) )
}



#### main functions for feature selection #####


## create the Ngram corpus
incNgrams<-function(texts, namespace, solution, min, max){ #build lists with ngrams of inccorrect location words and store as a list
  
  if(missing(min)){
    min=2  }
  if(missing(max)){
    max=7  }
  
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

      # search 
      TF<-str_detect(texts[i], namespace)
      
      if(length(which(TF==TRUE))>0){
        locations<-namespace[which(TF==TRUE)]
      
        ## for loop begins
        hu<-tolower(solution[i]); if(is.na(hu)){hu<-"EWW"}  #what the human code says
        wrong=NULL
        for(c in 1:length(locations)){
          
          prep <-texts[i] # i-th text at hand
          
          #more text generalization: change all other location names to "OTHERLOCZ"
          namespace.2<-namespace[-which(namespace==locations[c])]
          for(t in namespace.2){
            prep<-str_replace_all(prep, t, "OTHERLOCZ")
          }
          
          #split text into sentences
          prep <-str_split(prep, "\\.")[[1]]
          
          # generate N gram list
          test<-NGramTokenizer(prep, Weka_control(min=m,max=m))  #requires RWeka   
          
          if(str_detect(hu, str_trim(locations[c], "both") ) ){
            #if the location word was found in the human coded loc. list, nothing happens
          } else{
            wrong=c(wrong, str_trim(locations[c], "both") )
            #if the location word was NOT found in the human coded loc. list, classify it as an incorrect one.
          }
          
        }
        
        j=length(wrong)
        
        if(j>0){
          for(a in 1:j){
            # Now capture those containing the wrong location words
            temp<-test[which(!is.na(str_match(test, wrong[a])))]
            
            # more generalization: distinguish sub-location names (converted) from the province names (written as is)
            temp<-str_replace_all(temp, "sub_(.*?)\\b", "SUBLOC")
            
            #final generalization: convert the current location name to a code: "LOCZ"
            temp<-str_replace_all(temp, wrong[a], "LOCZ")
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
    Ngrams_incorrect[m]<-list(matrix[order(matrix$Freq, decreasing=TRUE),])  #create the number of lists = m
    return(Ngrams_incorrect[m])
    
  } ## end of the foreach-loop
  
   stopCluster(cluster)
  return(Ngrams_incorrect)
}

corNgrams<-function(texts, namespace, solution, min, max){#build lists with ngrams of inccorrect location words and store as a list
  if(missing(min)){
    min=2  }
  if(missing(max)){
    max=7  }
  
  #parallelization for faster running time
  require(doParallel)#parallelization
  cluster=makeCluster(6)#parallelization
  registerDoParallel(cluster)#parallelization
  
  Ngrams_correct <<- foreach(m=min:max) %dopar%{#parallelization
    # call necessary functions in the new cluster  
    library(stringr) #str_detect
    library(RWeka) #NGramTokenizer
    
    Ngrams_correct=list(NULL)    
    n=length(texts)  
    phrases=NULL

    for(i in 1:n){
      
      # search 
      TF<-str_detect(texts[i], namespace)
      if(length(which(TF==TRUE))>0){
        
        locations<-namespace[which(TF==TRUE)]
        hu<-tolower(solution[i]); if(is.na(hu)){hu<-"EWW"}
        correct=NULL  #important: go through the each loc in the location list. (doesn't matter whether the length(hu)>1)
        for(c in 1:length(locations)){
          
          #generalize location names other than the one at hand
          prep <-texts[i]
          
          #more text generalization: change all other location names to "OTHERLOCZ"
          namespace.2<-namespace[-which(namespace==locations[c])]
          for(t in namespace.2){
            prep<-str_replace_all(prep, t, "OTHERLOCZ")
          }
          
          #split text into sentences
          prep <-str_split(prep, "\\.")[[1]]
          
          # generate N gram list
          test<-NGramTokenizer(prep, Weka_control(min=m,max=m))  #requires RWeka   
          
          if(str_detect(hu, str_trim(locations[c], "both")     )){
            correct=c(correct,str_trim(locations[c], "both"))
          }
          
        }
        
        j=length(correct)
        
        if(j>0){
          for(a in 1:j){
            
            # Now capture those containing the wrong location words
            temp<-test[which(!is.na(str_match(test, correct[a])))]
            
            # more generalization: distinguish sub-location names (converted) from the province names (written as is)
            temp<-str_replace_all(temp, "sub_(.*?)\\b", "SUBLOC")
            
            #final generalization: convert the current location name to a code: "LOCZ"
            temp<-str_replace_all(temp, correct[a], "LOCZ")
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
    Ngrams_correct[m]<-list(matrix[order(matrix$Freq, decreasing=TRUE),])  #create the number of lists = m
    return(Ngrams_correct[m])
    
  } ## end of the foreach
  
  stopCluster(cluster)
  return(Ngrams_correct)
}

    

## this function builds the data frame "binary_data" 
buildY<-function(texts, namespace, solution, text_no){# (unit of observation=each location word) where binary takes 1 if the location word is correct.
  
  n=length(texts)  
  l=t=y=no_location=NULL
  for(i in 1:n){
    
    # search 
    TF<-str_detect(texts[i], namespace)
    
    if(length(which(TF==TRUE))>0){
      locations<-namespace[which(TF==TRUE)]
      hu<-tolower(solution[i]) 
      if(length(hu)==0 | hu=="NA" |is.na(hu) ){
        hu<-"EWW"}  #what the human code says
      wrong=NULL
      short=rep(NA, length(locations))
      
      for(c in 1:length(locations)){ #determine whether the human coded location word matches the detected loc. word
        if(str_detect(hu, str_trim(locations[c], "both") ) ){
          short[c]<-1    # matched!
        } else{
          wrong=c(wrong, str_trim(locations[c], "both") )
          short[c]<-0   # does not match.
        } #one short vector per article 
        
      }
      
    } else{ no_location<-c(no_location, i); locations<-NA; short<-NA  }
    
    if(missing(text_no)) {
      index<-i  
    } else {index<- text_no[i]}
    l=c(l, locations)   #list of locations
    t=c(t, rep(index, length(locations)))  #text ID
    y=c(y, short)    #Y 
    
  }
  
  binary_data<-as.data.frame(cbind("text_id"=t, "location_names"=l, "Y"=y))
  print(no_location)
  print('Text numbers of those without the location names. If no numbers printed, or these texts 1) do not include any location words and 2) are coded as having no event locations, ignore this message.')
  return(binary_data)
}


## calculate the frequencies of N-grams in corpus
buildNgram1<-function(texts, namespace, Ngrams_correct, Ngrams_incorrect, min, max, within){
  
  #parallelization for faster running time
  require(doParallel)#parallelization
  cluster=makeCluster(6)#parallelization
  registerDoParallel(cluster)#parallelization
  covariates<-NULL
  
  data_frame_results<<-foreach(m=min:max) %dopar%{#parallelization    # for each parameter in n-grams
    # call necessary functions in the new cluster  
    library(stringr) #str_detect
    library(RWeka) #NGramTokenizer
    # helper function
    my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA) 
    
    n=length(texts)
    k=m-(min-1)
    covar=NULL #one covar per each m pair
    for(i in 1:n){  # for each text
      
      TF<-str_detect(texts[i], namespace)
      locations<-namespace[which(TF==TRUE)]
      
      var=matrix(NA, nrow=length(locations), ncol=2)  #one var line per loc word = one var matrix per article
      # the matrix represents the values of correct & incorrect N-gram
      
      #if the i-th text contains no location words, then the value becomes (NA, NA)
      if(length(which(TF==TRUE))==0){var<-matrix(c(NA, NA), nrow=1)}else{
        
        
        for(j in 1:length(locations)){ # for each location word
          
          
          prep <-texts[i]
          #more text generalization: change all other location names to "OTHERLOCZ"
          namespace.2<-namespace[-which(namespace==locations[j])]
          for(t in namespace.2){
            prep<-str_replace_all(prep, t, "OTHERLOCZ")
          }
          
          #split text into sentences
          prep <-str_split(prep, "\\.")[[1]]
          
          # generate N gram list 
          test<-NGramTokenizer(prep, Weka_control(min=m,max=m))  #RWeka   
          
          # Now capture those containing the the j-th location word in the locations vector
          temp<-test[which(!is.na(str_match(test, locations[j])))]
          
          # more generalization: distinguish sub-location names (converted) from the province names (written as is)
          temp<-str_replace_all(temp, "sub_(.*?)\\b", "SUBLOC")
          
          #final generalization: convert the current location name to a code: "LOCZ"
          temp<-str_replace_all(temp, locations[j], "LOCZ")
          
          
          #now calculate the frequency rates!
          t<-length(temp)
          if(t>0){
            inc<-cor<-NULL
            for(s in 1:t){ # for each collocation pattern
              # match in the incorect corpus
              no<-which(temp[s]==Ngrams_incorrect[[k]][[1]]$phrases)
              if(length(no)==0){
                inc<-sum(inc, 1)#/sum(Ngrams_incorrect[[k]][[1]]$Freq))
              } else{
                inc<-sum(inc, (Ngrams_incorrect[[k]][[1]]$Freq[no]+1 ))#/sum(Ngrams_incorrect[[k]][[1]]$Freq) )
                #       prob_inc= -prob_inc
              }
              
              #match in the correct corpus
              no<-which(temp[s]==Ngrams_correct[[k]][[1]]$phrases)
              if(length(no)==0){
                cor<-sum(cor, 1)#1/sum(Ngrams_correct[[k]][[1]]$Freq))
              } else{
                cor<-sum(cor, (Ngrams_correct[[k]][[1]]$Freq[no]+1) )#/sum(Ngrams_correct[[k]][[1]]$Freq) )
              }
              
            }  # repeat for the times of collocation list (=length of temp) for each word
            var[j,]=cbind(inc, cor)   # one line per location word, one var matrix per article (text)
            
          } else {
            var[j,]=c(0,0)  #there's no collocation patterns for the parameter N. (eg. two gram sentence when N=3)
          } 
          
          }
        
      } #end of else condition. (the location word coded by human exists in the text)
      
      if(within=="article") { 
        # within the "article "variable   
        var[,1]<-var[,1]/my.max(var[,1]) # or sum
        var[,2]<-var[,2]/my.max(var[,2]) # or sum 
      } #one covar per each m pair (eg. ngrams of 2 for correct & incorrect)
      covar<-rbind(covar, var)
      covar[which(covar=="NaN")]<-0
    }
    # find frequencies in Ngrams _correct & Ngrams _incorrect
    if(within=="data") { 
      covar<-apply(covar, 2, function(x) x/my.max(x) ) # within the "data" ratio variable 
      covar[which(covar=="NaN")]<-0
       } 
    covariates<- as.data.frame(cbind(covariates, covar))

  }
  
  stopCluster(cluster)
  name=NULL
  if(within=="data"){ 
    for(i in min:max){
      name<-c(name, c(paste0('incorrect', i, '_data'), paste0('correct', i, '_data')))
    }
  } else {
    for(i in min:max){
      name<-c(name, c(paste0('incorrect', i, '_article'), paste0('correct', i, '_article')))
    }
  }
  data_frame_results<-do.call(cbind.data.frame, data_frame_results)
  names(data_frame_results)<-name
  print(paste0('Variables ', name, ' produced.'))
  return(data_frame_results)
}


## Matched N-gram: how many of the collocation patterns for each loc. word are matched to the top N-gram patterns?
buildNgram2<-function(texts, namespace, Ngrams_correct, Ngrams_incorrect, min, max, within,
                      nontopic.word, topic.word){
  
  #parallelization for faster running time
  require(doParallel)#parallelization
  cluster=makeCluster(6)#parallelization
  registerDoParallel(cluster)#parallelization
  covariates<-NULL
  
  data_frame_results<<-foreach(m=min:max) %dopar%{#parallelization    # for each parameter in n-grams
    # call necessary functions in the new cluster  
    library(stringr) #str_detect
    library(RWeka) #NGramTokenizer
    # helper function
    my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA) 
    
    n=length(texts)
    k=m-(min-1)
    covar=NULL #one covar per each m pair
    
    rate<-0.5
    obj.freq.inc<-Ngrams_incorrect[[k]][[1]]$Freq 
    obj.freq.cor<-Ngrams_correct[[k]][[1]]$Freq
    threshold.inc<- ceiling(sum(obj.freq.inc)*rate) 
    threshold.cor<- ceiling(sum(obj.freq.cor)*rate)
    inc.top.ngram <- Ngrams_incorrect[[k]][[1]]$phrases[ 1:which(cumsum(Ngrams_incorrect[[k]][[1]]$Freq) > threshold.inc)[1] ] 
    cor.top.ngram<- Ngrams_correct[[k]][[1]]$phrases[ 1:which(cumsum(Ngrams_correct[[k]][[1]]$Freq) > threshold.cor)[1] ]
    
    # kick out ngrams that shouldn't be here. eg) nontopic words should not be in top correct ngrams
    if( any(str_detect(tolower(cor.top.ngram), paste0(nontopic.word)) ) ) {
      cor.top.ngram<-cor.top.ngram[-which(str_detect(tolower(cor.top.ngram), paste0(nontopic.word) )  ) ] }
    
    if( any(str_detect(tolower(inc.top.ngram), paste0(topic.word)) ) ) {
      inc.top.ngram<-inc.top.ngram[-which(str_detect(tolower(inc.top.ngram), paste0(topic.word) )  ) ] }
    # make sure that top ngrams for correct and incorrect are mutually exclusive
    if(any(cor.top.ngram %in% inc.top.ngram) ){
      x<-cbind(obj.freq.cor[ which( cor.top.ngram %in% inc.top.ngram) ]/sum(obj.freq.cor) ,
               obj.freq.inc[ which( inc.top.ngram %in% cor.top.ngram) ]/sum(obj.freq.inc) )
      xx<-cbind(as.character( cor.top.ngram[ which( cor.top.ngram %in% inc.top.ngram) ] ) ,
                as.character( inc.top.ngram[ which( inc.top.ngram %in% cor.top.ngram) ])  )
      dt<- x==apply(x, 1, max)
      cor.top.ngram<-as.character(cor.top.ngram[which(cor.top.ngram %in% xx[which(dt[,1]==FALSE),1]==FALSE)])
      inc.top.ngram<-as.character(inc.top.ngram[which(inc.top.ngram %in% xx[which(dt[,2]==FALSE),2] == FALSE)])
    } 
    
    for(i in 1:n){  # for each text
      
      TF<-str_detect(texts[i], namespace)
      locations<-namespace[which(TF==TRUE)]
      
      var=matrix(NA, nrow=length(locations), ncol=2)  #one var line per loc word = one var matrix per article
      # the matrix represents the values of correct & incorrect N-gram
      
      #if the i-th text contains no location words, then the value becomes (NA, NA)
      if(length(which(TF==TRUE))==0){var<-matrix(c(NA, NA), nrow=1)}else{
        
        for(j in 1:length(locations)){ # for each location word
          
          prep <-texts[i]
          #more text generalization: change all other location names to "OTHERLOCZ"
          namespace.2<-namespace[-which(namespace==locations[j])]
          for(t in namespace.2){
            prep<-str_replace_all(prep, t, "OTHERLOCZ")
          }
          
          #split text into sentences
          prep <-str_split(prep, "\\.")[[1]]
          
          # generate N gram list 
          test<-NGramTokenizer(prep, Weka_control(min=m,max=m))  #RWeka   
          
          # Now capture those containing the the j-th location word in the locations vector
          temp<-test[which(!is.na(str_match(test, locations[j])))]
          
          # more generalization: distinguish sub-location names (converted) from the province names (written as is)
          temp<-str_replace_all(temp, "sub_(.*?)\\b", "SUBLOC")
          
          #final generalization: convert the current location name to a code: "LOCZ"
          temp<-str_replace_all(temp, locations[j], "LOCZ")
          
          # Now calculate how many neibhoring phrases are matched to the top ngram lists
          var[j,1]<- length( which( temp %in% inc.top.ngram ==TRUE ) )
          var[j,2]<- length( which( temp %in% cor.top.ngram ==TRUE ) )
          
        }
        
      } #end of else condition. (the location word coded by human exists in the text)
      
      if(within=="article") { 
        # within the "article "variable   
        var[,1]<-var[,1]/my.max(var[,1]) # or sum
        var[,2]<-var[,2]/my.max(var[,2]) # or sum 
      } #one covar per each m pair (eg. ngrams of 2 for correct & incorrect)
      covar<-rbind(covar, var)
      covar[which(covar=="NaN")]<-0
    }
    # find frequencies in Ngrams _correct & Ngrams _incorrect
    if(within=="data") { 
      covar<-apply(covar, 2, function(x) x/my.max(x) ) # within the "data" ratio variable 
      covar[which(covar=="NaN")]<-0
    } 
    covariates<- as.data.frame(cbind(covariates, covar))
  }
  
  stopCluster(cluster)
  name=NULL
  if(within=="data"){ 
    for(i in min:max){
      name<-c(name, c(paste0('incor.matched', i, '_data'), paste0('cor.matched', i, '_data')))
    }
  } else {
    for(i in min:max){
      name<-c(name, c(paste0('incor.matched', i, '_article'), paste0('cor.matched', i, '_article')))
    }
  }
  data_frame_results<-do.call(cbind.data.frame, data_frame_results)
  names(data_frame_results)<-name
  print(paste0('Variables ', name, ' produced.'))
  return(data_frame_results)
}


## Frequency: how many times does this location word appear in the article? - both within article and data level
buildFrequency<-function(texts, namespace, text.id){ 
  if(missing(text.id)){ # if text.id is not assigned, assign text ID numbers from 1: nrow(texts)
    no.var<-1:length(texts)
  } else {no.var<-text.id}
  output<-locs<-text_no<-within.o<-NULL
  for(i in 1:length(texts)){
    TF<-str_detect(texts[i], namespace)
    locations<-namespace[which(TF==TRUE)]      
    if(length(which(TF==TRUE))>0){ #Test whether the i-th text contains location words (that are in the dictionary)
      for(c in 1:length(locations)) {# for each location word
        output<-c(output, str_count(texts[i], locations[c]) )
        locs<-c(locs, locations[c])
        text_no<-c(text_no, no.var[i]) 
      }
      # now compute the within article occurence ratio
      within.o<-c(within.o, output[( length(output)-length(locations)+1 ):length(output) ] /
                    my.max( output[ (length(output)-length(locations)+1 ):length(output)] ) )
    } else { #if the i-th text contains no location words (that are in the dictionary)
      output<- c(output, 0) 
      locs<-c(locs, NA)
      text_no<-c(text_no, no.var[i])
      within.o<-c(within.o, NA)
    }
  }
  return(as.data.frame( cbind(text_no, 
                              "location_names"=locs, 
                              "frequency_data"=output/my.max(output),
                              "frequency_article"=within.o) ) )
}


## Context : Is the topic discussed in the sentence something related to the scope of interest?
buildContext<-function(texts, solutions, verbs, namespace){ # text has to be the raw article with 1) "periods" after each sentence and 2) location names cleaned
  require(ngram) #for 'concatenate' function
  require (stringr) #for 'str_  ' functions
  bi<-within.tr<-loc<-province_human<-text_no<-NULL
  names<-namespace
  
  for( i in 1:length(texts)) {
    solution<-as.character(unlist(solutions[i]))
    text<-as.character(unlist(texts[i]))
    TF<-str_detect(text, namespace)
    
    if(length(which(TF==TRUE))>0){
      locations<-namespace[which(TF==TRUE)] #the entire location words captured in the i-th text
      for(c in 1:length(locations)){
        # split the text by period(.)s so each search space is a sentence.
        temp<-str_split(text, '\\.')[[1]]
        #    get sentences with the c-th location word 
        l<-as.data.frame(sapply(temp, function(row) str_detect(row, locations[c]) ) )
        #keep & combine the sentences with the c-th location word and store it as a temporary object (temp2). 
        temp2<-as.character(ngram::concatenate(unlist(temp[which(l==TRUE)])))
        det<-any(str_detect(temp2, verbs))
        # "det" tests whether "temp2" contains the pre-defined words. 
        if(det) {
          # tr: how many pre-defined verbs can you match?
          tr<-as.numeric(str_count(temp2, verbs) ) 
          #store the 'tr' values.
          bi<-c(bi,  tr)
          #                 / sum(length(verbs))) # if you want the across-articles proportion.
          # store the location names as well to be able to match with other data frames          
          loc<-c(loc, locations[c])
          # store the text numbers so you can match the generated variables with other data frames.          
          text_no<-c(text_no, i) 
          #save province_human. same logic as loc and text_no.
          province_human<-c(province_human, solution)
        } else { # temp2 DOES NOT contain any of the pre-defined words.
          bi<-c(bi, 0)
          loc<-c(loc, locations[c])
          text_no<-c(text_no, i)
          province_human<-c(province_human, solution)
        }
      }
      #get the within-article ratios and store them.
      within.tr<-c(within.tr, bi[( length(bi)-length(locations)+1 ):length(bi) ] /
                     sum( bi[ (length(bi)-length(locations)+1 ):length(bi)] ) )
      
    } else { # if there's no location words in the text.
      bi<-c(bi, NA)
      loc<-c(loc, NA)
      text_no<-c(text_no, i)
      province_human<-c(province_human, solution)
      within.tr<-c(within.tr, NA)
    }
    
  } 
  if(any(which(within.tr=="NaN"))) { #convert NaN cases generated due to the division by 0s.
    within.tr[which(within.tr=="NaN")]<-0
  }
  my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
  return(as.data.frame(cbind("text_no"=text_no, 
                             "words_matched_data"=bi/my.max(bi), 
                             "words_matched_article"=within.tr,
                             "location_names"=loc,
                             province_human)) ) 
}



# This is for saving results 
my.sum<-function(input){
  sum(input, na.rm=TRUE)
}
