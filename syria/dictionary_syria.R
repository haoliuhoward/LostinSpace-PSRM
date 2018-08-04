## Syria Dictionaries ##

##### Building Dictionaries #######


#Caveats: if the province names end with ing, ed, or any forms that could be considered ending in English, change them back after data processing
#if the province names contain white spaces, make them as one word by removing the space both in the raw text and the province list. 

## define special characters to remove
special.characters<-c(
  "\n",
  "\\?",
  "\\)",
  ";", ":",
  "!","$",
  "\\]",
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
  #"-", be careful in Arabic texts with "el-"s & "al-"s
  "_ ",
  "__",  # csv file miss many of these 
  "\\'s",
  "\\`",
  "\\\\"
)


#### 1) LOCATION DICTIONARY ####
#load data
# current.path<-getwd()
# setwd(syria.path)
syria_province<-read.csv("syria_province.csv")
names=c(str_trim(as.character(tolower(syria_province$governorate))))
names<-unique(names[order(names)])
names



#### 2) ACTOR DICTIONARY ####

syria.sentences<-sentencify(data$cleantext, solutions=data$province_human, namespace=names, text_id=data$story_id)

# actor dictionary (these are not very helpful)
# raw_syria<-read.csv("       ") # this file contains FACTIVA proprietary contents and hence can't be released.
# actors<-unique(as.character(raw_syria$ActorEntity1))
# actors<-as.character(iconv(actors, "ASCII", "utf-8", sub=" "))
# actors<-str_replace_all(tolower(actors), "\\(.*?\\)", "")
# actors<-unique(actors)
# actors<-str_trim(actors, side="both")
# actors = str_replace(actors, "gov", "government") %>% str_replace(. , "mil", "military") %>% str_replace(. ,"reb", "rebel") %>% str_replace(. ,"opp", "opposition") %>%
#   str_replace(. ,"cvl", "civilians") %>% .[!is.na(.)]
# 
# target<-unique(as.character(raw_syria$ActorEntity2))
# target<-as.character(iconv(target, "ASCII", "utf-8", sub=" "))
# target<-str_replace_all(tolower(target), "\\(.*?\\)", "")
# target<-unique(target)
# target<-str_trim(target, side="both")
# target = str_replace(target, "gov", "government") %>% str_replace(. , "mil", "military") %>% str_replace(. ,"reb", "rebel") %>%
#   str_replace(. ,"opp", "opposition") %>% str_replace(. ,"med", "mediator") %>% str_replace(. ,"edu", "eduation") %>%
#   str_replace(. ,"cop", "cooperation") %>%
#   str_replace(. ,"cvl", "civilian") %>% .[!is.na(.)]
# 
# actor<-unique(c(actors, target))
# actor<-c(actor, "laywer","attorney")
# We imported this list from OEDA, but this can be done manually through parsing sentences
actor<-c("government" , "military forces", "military"  ,  "rebel" ,      "opposition" , "civilians" ,  "mediator"   ,"cooperation",
         "eduation"  ,  "civilian" ,   "laywer"  ,    "attorney",   "hezbollah", "syrian air force", "air force",
         "syrian force", "syrian army", "army", "spokesman", "spokesperson", "representative", "islamic state", "ISIS",
         "un envoy", "observatori network of activis", "jihadist" , "bashar assad", "akcakal turkey", "al-qaida")



actor2<-c("rebels", "oppositions", "governments", "civilians", "fighters", "isis","ISIL", "al qaeda",
          "personnel", "ministry", "government","party","administration","officials","company","worker","officer", "group",
          "people", "citizens", "syria", "syrian" ,  "washington", "russia", "russian", "moskva", "un", "activists",
          "iran", "iranian", "iraq", "iraqi", "us", "pentagon", "president", "prime minister", "extremist", "militants", "militia", "lebanese", "lebanon",
          "israel", "turkey", "kurdistan", "moscow", "beirut", "europe", "brussels","geneva", "egyptian", "egypt")




actor<-apply(as.data.frame(actor), 1, 
             function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                          stemDocument)[[1]]$content)) )
actor2<-apply(as.data.frame(actor2), 1, 
              function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                           stemDocument)[[1]]$content)) )



###### 3) RELEVANT AND IRRELEVANT WORDS DICTIONARIES  ######
# if stemming, the different forms of irregular verbs (eg. feel, felt) should be added separately.
# if lemmatizing, adding the base form should suffice. 
###### dictionaries for the verb similarity variable ######
# this should be cameo19
# cameo19: action verb, nouns (basically go thru 432 words)
# cameo19_clean = c("kill",
#                   "attack",
#                     "damage","damaged","damaging",
#             "kill","killed","killing",
#                   "hit", "fire",
#                   "hurt", "fighter",
#                   "assault", "control" ,
#                   "gun","aid", "stage", "fighterjet","airstrik", "aircraft","fight", "gunfir" ,"ceasefir" , "arm", "underfir", "raid", "bomber","casualti" ,"massacr", "shot" ,"peac" ,  "broke"  , "polic","combat","hold" ,"purg","action", "bombard","seig","pursuit","rocketstrik", "blast" ,"airraid"  ,"injuri"  ,"skirmish" ,"clash","firefight","machine-gunfir","wound","ground-fir","flare" ,"artillari","dynamit" ,"gunfight","fir" , "blew","duel" ,"rock"      ,"brokeout","lock","anti-aircraft", "fighter-bomb" , "injur","tear", "rifl","strike","storm" ,"streetbattl","intensifi","gundown" ,"gunned", "heavyfight", "patrol", "explos", "shootout","violenc","ambush","gunbattl","blow","flush","groundfens","counter-attack" ,"resist"  , "collaps","held","unprovok", "reoccupi","helicoptergunship" ,"counter-offens"
# ,"anti-insurg","recaptur" ,"entri" ,"repel","drop", "driven","takenov" , "struck" , "fallen" , "protest","riot","shut" ,"slaughter","crush","seiz","flushout","grip" ,"gunshot" )

actions = c("bomb", "bombed","bombing","bombers","bomber","bombard","bombarded", "kill",
            "shell", "shelling", "shelled",
            "attack", "attacks", "attacked", "attacking",
            "hit", "hitting",
            "assault", "assaulted", "assaulting",
            "advance", "advancing",
            "capture", "captured", "capturing",
            "recapture", "recaptured","recapturing",
            "fallen", "crush",
            "fight", "fought", "fighting",
            "besiege", "besieged",
            "contest","contested",
            "control", "controlled",
            "enter", "entered",
            "take", "retake",
            "clash","clashed", "clashing",
            "block", "blocked", "blocking",
            "blockaded", "blockade",
            "battle", "battles", "battling",
            "destroy", "destroyed", "destroying",
            "encircle","encircled",
            "seize", "seized",
            "siege", "sieged",
            "besiege", "besieged",
            "shot", "shoot",
            "airstrike", "airstrikes","air strike",
            "strike", "stroke", "strikes","striking",
            "storm","storms","storm","stormed","storming",
            "riot", "rioted", "rioting",
            "escalate","escalated", "escalating",
            "surround", "surrounded", "surrounding",
            "block","blocked","blocking",
            "repel", "repelled",
            "occupy","occupied","occupying",
            "escape","escaping", "escaped",
            "displaced",
            "contested", "contesting",
            "wound", "wounded",
            "stab","stabbed",
            "beat", "beaten", "beating",
            "injure","injured",
            "die" ,"died",
            "dead", "death",
            "destroy","destroyed","destroying",
            "fled", "flee",
            "smashed",  "resumed", "recur"
)

action_stemmed<-apply(as.data.frame(actions), 1,
                      function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)),
                                                                   stemDocument)[[1]]$content)) )

non.actions<-c('say','said','report','reported',
               'tell','told','tells',
               'interview','announce','accuse',
               'cover','describe','confirm','declare','contain',"sourz")
non.action_stemmed<-apply(as.data.frame(non.actions), 1,
                          function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)),
                                                                       stemDocument)[[1]]$content)) )





#### 4) GENERIC WORDS DICTIONARIES ####

#days
dayz<-c("mon|monday","tue|tuesday","wed|wednesday", "thr|thursday","fri|friday","sat|saturday", "sun|sunday")

dayz<-apply(as.data.frame(dayz), 1, 
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
          "may","june|jun", "july|jul","august|aug","september|sep|sept",
          "october|oct","november|nov","december|dec", "month|months", "july", "january")

months<-apply(as.data.frame(months), 1, 
              function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                           stemDocument)[[1]]$content)) )




# news agencies
# raw_syria$publisher<-as.character(iconv(raw_syria$publisher, "ASCII", "utf-8", sub=" "))
# sources1=unique(as.character(raw_syria$publisher))
sources1<-c("almonitor"                                                  
, "pakistan_dailymessanger"                                    
, "southaf_capetownt"                                          
, "wn_world"                                                   
, "thailand_bankokpost;wn_world"                               
, "voa_all"                                                    
, "dw"                                                         
,"rfe"                                                        
, "malstar_world"                                              
,"guardian_americas"                                          
, "wn_politics"                                                
, "bbc"                                                        
,"csm_world"                                                  
, "menafn_syria"                                               
, "pakistan_frontierpost"                                      
,"tolo"                                                       
, "int_the_news_world;asianage_int;asianage_int;shanghai_world"
, "int_the_news_world;daily_star_middle_east"                  
, "cyprus_mail"                                                
, "daily_star_middle_east"                                     
, "jpost_me"                                                   
, "wn_europe"                                                  
, "china_scmp_world"                                           
, "wn_mideast"                                                 
, "menafn_syria;daily_star_middle_east;reuters"                
, "malta_today"                                                
, "menafn_syria;reuters;daily_star_middle_east"                
, "japan_times"                                                
, "thailand_bankokpost"                                        
, "mb"                                                         
, "wn_americas"                                                
, "google"                                                     
,"wn_asia"                                                    
, "pakistan_worldtribune;thailand_bankokpost"                  
,"nigeria_leadership"                                         
,"int_the_news_latest"                                        
, "pakistan_worldtribune"                                      
, "int_the_news_world"                                         
, "chinapost_international"                                    
, "yahoo_india"                                                
,"euronews"                                                   
, "dw;int_the_news_latest"                                     
, "nyt"                                                        
, "phil_manilastandard"                                        
, "hurriyet"                                                   
, "xinhua"                                                     
, "nytmiddleeast;google;nyt"                                   
, "voa_euro"                                                   
, "asianage_int"                                               
, "reuters"                                                    
, "nigeria_businessday"                                        
, "hindu_int"                                                  
, "alakhbar"                                                   
, "ap"                                                         
, "nyt;wn_mideast;dw"                                          
,"guardian_africa"                                            
, "nytmiddleeast"                                              
, "nzherald_world"                                             
, "phil_inquirer" )   
sources1<-str_replace_all(sources1, "_", " ")
sources1<-c(unlist(str_split(sources1, ";")))
sources2<-c('AP','CNN','AFP', 'Xinhua News Agency', 'Reuters', 'Yonhap', #some common abbreviations
            'BBC', 'egypt Daily', 'Al Jazeera', 'Korea Times', 'Kyodo',
            'Independent Online', 'South egypt Morning Post', 'FARS News Agency',
            'Washington Post', 'IANS India', 'FARS News', 'Philippines News',
            'Agence France-Presse', 'New York Times', 'EFE News'
)
sources3<-c('Xinhua',"News","report","radio","press","ap")

sources<-apply(as.data.frame(sources), 1, 
               function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                            stemDocument)[[1]]$content)) )

sources2<-apply(as.data.frame(sources2), 1, 
                function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                             stemDocument)[[1]]$content)) )

sources3<-apply(as.data.frame(sources3), 1, 
               function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                            stemDocument)[[1]]$content)) )


#delete raw_egypt to save the memory
#rm(raw_egypt)
#setwd(current.path)
#rm(current.path)

#administrative divisions

# admin<-c("area", "areas",
#          "city", "cities",
#          "village", "villages",
#          "province", "provinces", "provincial",
#          "region","prefecture", "prefectures",
#          "town","township","towns",
#          "county","counties",
#          "district","districts", # use it to differential human name and prov name
#          "municipal","municipality",
#          "municipalities", "office",
#          "governorate", "muhafazat",
#          "suburb"
# )

# ## in the towns of Zirba "and" Khan Touman
admin <-c("town",
          "village",
          "city",
          "province",
          "governorate",
          "region",
          "district",
          "municipalities",
          "suburb",
          "neighborhood",
          "area",
          "province of"
)

## in the towns of Talet al-Iss, Zitan, Zirba "and" Khan Touman
adminz <-c("towns",
           "villages",
           "cities",
           "provinces",
           "governorates",
           "regions",
           "districts", # use these to differentiate human names from prov name
           "municipalities",
           "suburbs",
           "neighborhoods",
           "areas"
)

admin<-apply(as.data.frame(admin), 1,
             function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)),
                                                          stemDocument)[[1]]$content)) )


adminz<-apply(as.data.frame(adminz), 1,
             function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)),
                                                          stemDocument)[[1]]$content)) )


