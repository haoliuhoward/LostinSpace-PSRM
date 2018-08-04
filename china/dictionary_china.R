## China Dictionaries ##

##### Building Dictionaries #######


#Caveats: if the province names end with ing, ed, or any forms that could be considered ending in English, change them back after data processing
#if the province names contain white spaces, make them as one word by removing the space both in the raw text and the province list. 

## define special characters to remove
special.characters<-c("\n",
                      "\\?", 
                      "\\)", 
                      ";", ":",
                      "!","$", 
                      "]", 
                      "\\[", 
                      "\\(" , 
                      "\"",
                      "@",
                      "\\$",
                      "\\&",
                      "\\*",
                      "\\,",
                      "=",
                      "#",
                      "\\'",
                      "-",
                      "_",
                      "\\'s",
                      "`")


#### 1) LOCATION DICTIONARY ####

#load data
current.path<-getwd()
setwd(china.path)

# list of province names to search for: has to be one word to avoid confusion. (=no whitespace!)
china_province<-read.csv("china_province.csv")
china_province$province<-as.character(iconv(china_province$province, "ASCII", "utf-8", sub="")) #to ASCII 
china_province$city<-as.character(iconv(china_province$sublevel, "ASCII", "utf-8", sub=""))
summary(china_province)
names<-c(as.character(unique(tolower(china_province$province))))
names<-names[order(names)]
names=names[-c(21)] #no hongkong or macau
names<-str_replace_all(names," ","") #no space in province names!
names # double check!
#also, the naming scheme has to be consistent throughout the articles. 
#if some province names contain white spaces, include one more step in the data-processing stage.



#### 2) ACTOR DICTIONARY ####
# raw_china<-read.csv('     ')  # this file contains FACTIVA proprietary contents and hence can't be released.
# actors<-unique(as.character(raw_china$source_actor))
# actors<-as.character(iconv(actors, "ASCII", "utf-8", sub=" "))
# actors<-str_replace_all(tolower(actors), "\\(.*?\\)", "")
# actors<-unique(actors)
# actors<-str_trim(actors, side="both")

# We imported this list from ICEWS, but this can be done manually through parsing sentences
actors<-c("citizen", 
          "activist"                                           
          , "falun gong"                                         
          ,"employee"                                           
          , "protester"                                          
          ,"scholar"                                            
          ,"tibet autonomous region"                            
          , "men"                                                
          , "representatives"                                    
          , "foreign affairs"                                    
          , "rights activist"                                    
          , "farm worker"                                        
          ,"li tieying"                                         
          , "labor activist"                                     
          , "immigrants"                                         
          ,"student"                                            
          , "lawmaker"                                           
          ,"civil servant"                                      
          ,"people first party"                                 
          , "joseph zen"                                         
          ,"company - owner or operator"                        
          , "villager"                                           
          ,"new party"                                          
          ,"buddhist"                                           
          ,"wang dan"                                           
          , "government official"                                
          , "women"                                              
          , "professor"                                          
          , "james soong"                                        
          ,"human rights group"                                 
          , "su tseng chang"                                     
          ,"nobutaka machimura"                                 
          , "business"                                           
          , "non partisan solidarity union"                      
          ,"democratic progressive party"                       
          , "congress"                                           
          , "donald tsang"                                       
          , "central committee of the communist party of china"  
          , "military police"                                    
          ,"human rights activist"                              
          , "chen shui bian"                                     
          , "yu shyi kun"                                        
          , "children"                                           
          ,"opposition force"                                   
          ,"guangxi zhuang"                                     
          ,"civic group"                                        
          , "education"                                          
          ,"monastery"                                          
          , "paramilitary police"                                
          ,"buddhist monk"                                      
          ,"party member"                                       
          ,"wen jiabao"                                         
          ,"joseph wu"                                          
          , "liu qi"                                             
          ,"expatriate"                                         
          , "indigenous people"                                  
          ,"lai shin-yuan"                                      
          , "religion"                                           
          ,"separatist"                                         
          , "association for relations across the taiwan straits"
          , "medical personnel"                                  
          , "nur bekri"                                          
          , "rebiya kadeer"                                      
          , "uighur"                                             
          , "nurse"                                              
          , "muslim"                                             
          ,"ethnic group"                                       
          , "xinjiang uyghur"                                    
          ,"yang jiechi"                                        
          , "bank"                                               
          , "governor"                                           
          ,"lobsang sangay"                                     
          , "ruling party"                                       
          , "veterans"                                           
          , "democratic party"                                   
          , "state media"                                        
          , "labor party"                                        
          , "scientist"                                          
          , "fishermen"                                          
          ,"hu jia"                                             
          , "striking worker"                                    
          ,"student dissident"                                  
          , "leung chun-ying"                                    
          , "opposition activist"                                
          , "tenzin gyatso"                                      
          , "political parties"                                  
          ,"kuomintang"                                         
          , "socialist party"                                    
          , "protesting worker"                                  
          , "tung chee-hwa"                                      
          , "rioter"                                             
          , "mob"                                                
          ,"detainee"                                           
          , "opposition supporter"                               
          , "labor union"                                        
          , "industry"                                           
          , "city mayor"                                         
          , "criminal"                                           
          ,"attacker"                                           
          ,"al qaeda in the arabian peninsula"                  
          , "organized crime"                                    
          , "tenants"                                            
          ,"armed band"                                         
          , "militant")


# target<-unique(as.character(raw_china$target_actor))
# target<-as.character(iconv(target, "ASCII", "utf-8", sub=" "))
# target<-str_replace_all(tolower(target), "\\(.*?\\)", "")
# target<-unique(target)
# target<-str_trim(target, side="both")


target<- c("police"                                             
           , "china"                                              
           , "government"                                         
           , "falun gong"                                         
           ,"government official"                                
           , "other authorities / officials"                      
           ,"jiang zemin"                                        
           , "hong kong"                                          
           ,"military personnel - special"                       
           , "tung chee-hwa"                                      
           ,"military"                                           
           ,"congress"                                           
           , "foreign affairs"                                    
           , "city mayor"                                         
           , "business"                                           
           ,"legislature"                                        
           , "communist party"                                    
           , "chinese communist party"                            
           , "party member"                                       
           , "li peng"                                            
           , "taiwan"                                             
           , "japan"                                              
           , "hu jintao"                                          
           , "student"                                            
           , "tibet autonomous region"                            
           , "royal administration"                               
           ,"bureaucrat"                                         
           , "macao"                                              
           ,"chen shui bian"                                     
           , "annette lu"                                         
           , "thief"                                              
           ,"zhao ziyang"                                        
           , "wen jiabao"                                         
           , "priest"                                             
           , "democratic progressive party"                       
           , "lien chan"                                          
           , "healthcare facility"                                
           , "luo gan"                                            
           , "education"                                          
           , "zhang dejiang"                                      
           , "employee"                                           
           , "company - owner or operator"                        
           , "kuomintang"                                         
           ,"lawyer/attorney"                                    
           ,"education ministry"                                 
           ,"cppcc national committee"                           
           ,"xinhua"                                             
           ,"pervez musharraf"                                   
           ,"civil servant"                                      
           ,"paramilitary police"                                
           ,"tenzin gyatso"                                      
           ,"member of the judiciary"                            
           ,"rioter"                                             
           ,"separatist"                                         
           ,"ma ying jeou"                                       
           ,"central military commission"                        
           ,"head of government"                                 
           ,"wang lequan"                                        
           ,"muslim"                                             
           , "state owned business"                               
           , "xinjiang uyghur"                                    
           ,"media personnel"                                    
           , "gordon brown"                                       
           , "donald tsang"                                       
           , "liu xiaobo"                                         
           , "newspaper"                                          
           , "government religious"                               
           , "defense attorney"                                   
           , "hosni mubarak"                                      
           , "inner mongolia"                                     
           , "10 downing street"                                  
           , "ministry"                                           
           , "provincial officials"                               
           , "bank"                                               
           , "main opposition"                                    
           , "media"                                              
           , "syria"                                              
           , "military police"                                    
           , "news editor"                                        
           , "defense / security ministry"                        
           , "norodom sihanouk"                                   
           , "sinopec"                                            
           , "intellectual"                                       
           ,"liu qi"                                             
           , "leung chun-ying"                                    
           , "criminal"                                           
           , "immigrants"                                         
           , "myanmar"                                            
           , "medical personnel"                                  
           , "china national petroleum corporation"               
           , "li keqiang"                                         
           , "lawmaker"                                           
           , "thaksin shinawatra"                                 
           , "naval base"                                         
           , "goodluck jonathan"                                  
           , "people's democratic party"                          
           , "nambaryn enkhbayar"                                 
           , "professor"                                          
           , "nawaz sharif"                                       
           , "wu rong-i"                                          
           ,"industry"                                           
           , "general contractor"                                 
           , "afghanistan"                                        
           , "egypt"                                              
           , "admiral"                                            
           , "election commission"                                
           , "israeli defense forces"                             
           ,"association for relations across the taiwan straits"
           , "yemeni armed forces"                                
           , "farm worker"                                        
           , "yingluck shinawatra"                                
           , "democratic party"                                   
           , "domestic affairs"                                   
           , "india" )

actor<-unique(c(actors, target))

actor<-c(actor, "laywer","attorney")
actor2<-c("personnel", "ccp", "ministry", "government","party","administration","officials","company","worker","officer", "group")

actor<-apply(as.data.frame(actor), 1, 
        function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                     stemDocument)[[1]]$content)) )
actor2<-apply(as.data.frame(actor2), 1, 
                function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                             stemDocument)[[1]]$content)) )

# rm(raw_china)



###### 3) RELEVANT AND IRRELEVANT WORDS DICTIONARIES  ######
# if stemming, the different forms of irregular verbs (eg. feel, felt) should be added separately.
# if lemmatizing, adding the base form should suffice. 

actions = c("gather", "gathered", "gathering",
            "protest", "protested", "protesting",
            "demonstrate", "demonstrated", "demonstrating",
            "rally", "rallied", "rallying",
            "march", "marched", "marching",
            "clash","clashed", "clashing",
            "block", "blocked", "blocking",
            "stage",
            "staged", "staging",
            "riot", "rioted", "rioting",
            "strike", "strikes","striking",
            "escalate","escalated", "escalating",
            "surround", "surrounded", "surrounding",
            "block","blocked","blocking",
            "damage","damaged","damaging",
            "kill","killed","killing",
            "occupy","occupied","occupying", 
            "sack", "sacked","sacking",
            "sit",  "sitting",
            "besiege", "besieged",
            "storm","stormed","storming",
            "wound", "wounded",
            "stab","stabbed",
            "beat", "beaten", "beating",
            "throw", "throwing",
            "demobilize","demobilized",
            "petition", "petitioned","petitioning",
            "injure","injured",
            "die" ,"died",
            "dead", "death",
            "destroy","destroyed","destroying",
            "overturn", "overturned", "overturning",
            "chant", "chanted", "chanting",
            "condemn","condemned","condemning",
            "shout", "shouted","shouting",
            "turned",  "injuring", "obstruct", "vandalized", "fled", "seized",  "smashed",  "resumed", "recur")

action_stemmed<-apply(as.data.frame(actions), 1, 
                      function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                                   stemDocument)[[1]]$content)) )
action_stemmed<-unique(action_stemmed)


non.actions<-c('say','said','report', 'confirm','interview','announce','accuse','arrest','prison',
               'state','tell','told','cover','describe','confirm','declare','contain',"sourz")
non.action_stemmed<-apply(as.data.frame(non.actions), 1, 
                          function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                                       stemDocument)[[1]]$content)) )



#### 4) GENERIC WORDS DICTIONARIES ####

#days
dayz<-c("mon|monday","tue| tuesday","wed|wednesdays", "thr|thursday","fri|friday","sat|saturday", "sun|sunday")

days<-apply(as.data.frame(days), 1, 
            function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                         stemDocument)[[1]]$content)) )


#numerals
numeral<-c('one','two','three','four','five','six','seven','eight','nine','ten', 'hundred', 'hundreds','thousand','thousands',
           'few','many','several','million','millions', 'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 
           'eighth','ninth','tenth')

numeral<-apply(as.data.frame(numeral), 1, 
               function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                            stemDocument)[[1]]$content)) )

#directional words
directz<-c("eastern", "western", "northern", "southern",
           "east","west","north","south", # be careful with this one when one of these words are used for province names
           "southwest", "southwestern", "southeast", "southeastern",
           "central", 
           "northwest", "northwestern", "northeast", "northeastern")

directz<-apply(as.data.frame(directz), 1, 
               function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                            stemDocument)[[1]]$content)) )


#months
months<-c("january|jan","february|feb","march|mar","april|apr",
          "may"," june|jun", "july|jul","august|aug","september|sep|sept",
          "october|oct","november|nov","december|dec", "month|months")

months<-apply(as.data.frame(months), 1, 
              function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                           stemDocument)[[1]]$content)) )




# news agencies
sources=c('AP','CNN','AFP', 'Xinhua News Agency', 'Reuters', 'Yonhap', #some common abbreviations
          'BBC', 'China Daily', 'Al Jazeera', 'Korea Times', 'Kyodo',
          'Independent Online', 'South China Morning Post', 'FARS News Agency',
          'Washington Post', 'IANS India', 'FARS News', 'Philippines News',
          'Agence France-Presse', 'New York Times', 'EFE News')
sources2<-c('Xinhua',"News","report","radio","press")

sources<-apply(as.data.frame(sources), 1, 
                                        function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                                                    stemDocument)[[1]]$content)) )

sources2<-apply(as.data.frame(sources2), 1, 
               function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                            stemDocument)[[1]]$content)) )

#administrative divisions
admin<-c("city", "cities", "village", "villages", "province", "provinces", "provincial", "region","prefecture",
         "prefectures","town","township","towns","county","counties", "district","districts","municipal","municipality",
         "municipalities", "office")
admin<-apply(as.data.frame(admin), 1,
                    function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)),
                                                                stemDocument)[[1]]$content)) )


# setwd(current.path)
# rm(current.path)





