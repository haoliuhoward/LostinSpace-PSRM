## DRC dictionaries ###

##### Building Dictionaries #######


#Caveats: if the province names end with ing, ed, or any forms that could be considered ending in English, change them back after data processing
#if the province names contain white spaces, make them as one word by removing the space both in the raw text and the province list. 

## define special characters to remove
special.characters<-c("\n",
                      "\\?", 
                      ")", 
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
setwd(drc.path)


# list of province names to search for: has to be one word to avoid confusion. (=no whitespace!)
drc_province<-read.csv("drc_province.csv") # this file in the drc directory
drc_province<-drc_province[!duplicated(drc_province$city),]
drc_province<-drc_province[-which(drc_province$note=="delete"),]
drc_province$city<-as.character(drc_province$city)
drc_province$province<-as.character(drc_province$province)
names<-unique(drc_province$province)
names #double check!


#### 2) ACTOR DICTIONARY ####
# drc<-read.csv('       ') # raw data with publishers etc. ; is in the drc directory
# actor dictionary
# actors<-unique(as.character(drc$source_actor))
# actors<-as.character(iconv(actors, "ASCII", "utf-8", sub=" "))
# actors<-str_replace_all(tolower(actors), "\\(.*?\\)", "")
# actors<-unique(actors)
# actors<-str_trim(actors, side="both")
# 
# target<-unique(as.character(drc$target_actor))
# target<-as.character(iconv(target, "ASCII", "utf-8", sub=" "))
# target<-str_replace_all(tolower(target), "\\(.*?\\)", "")
# target<-unique(target)
# target<-str_trim(target, side="both")
# 
# actor<-unique(c(actors, target))

actor<-c("police"                                          
, "military personnel - special"                    
,"citizen"                                         
,"armed forces of the democratic republic of congo"
,"government"                                      
,"militia"                                         
, "congolese rally for democracy"                   
, "armed forces of the republic of the congo"       
,"national congress for the defence of the people" 
, "rebel group"                                     
, "rwandan defence forces"                          
, "nigerian army"                                   
, "armed gang"                                      
, "combatant"                                       
, "children"                                        
, "lord's resistance army"                          
, "attacker"                                        
, "healthcare facility"                             
, "peacekeeping troop"                              
, "armed opposition"                                
, "military"                                        
, "men"                                             
, "armed rebel"                                     
, "democratic republic of congo"                    
, "commando"                                        
, "military personnel"                              
, "democratic forces for the liberation of rwanda"  
, "front for patriotic resistance in ituri"         
, "thief"                                           
, "armed band"                                      
, "dissident"                                       
, "population"                                      
, "uganda people's defence force"                   
, "south african national defence force"            
, "air force"                                       
, "terrorist"                                       
, "independent electoral commission"                
, "sex trade"                                       
, "criminal"                                        
, "secret agent"                                    
, "refugee"                                         
, "militant"                                        
, "rcd-national"                                    
, "congolese national police"                       
, "egyptian armed forces"                           
, "allied democratic forces"                        
, "attack helicopter"                               
, "bosco ntaganda"                                  
, "armed services deserter"                         
, "nationalist and integrationist front"            
, "laurent nkunda"                                  
, "movement for the liberation of the congo"        
, "civil servant"                                   
, "transport"                                       
, "separatist"                                      
, "prelate"                                         
, "employee"                                        
, "laurent kabila"                                  
, "media personnel"                                 
, "insurgent"                                       
, "army chief of staff"                             
, "women"                                           
, "opposition supporter"                            
,"protester"                                       
,"ma_ ma_"                                         
, "congo"                                           
, "fishermen"                                       
, "company owner| operator"                     
, "uganda people's congress"                        
, "activist"                                        
, "egypt"                                           
, "election commission"                             
, "uganda"                                          
, "human rights activist"                           
, "peter karim"                                     
, "religion"                                        
, "rwanda"                                          
,"joseph kabila"                                   
,"education"                                       
, "boko haram"                                      
,"people's armed forces of congo"                  
, "union for democracy and social progress"         
, "student"                                         
, "south africa"                                    
, "forces for renewal"                              
,"fran ois joseph nzanga mobutu ngbangawe"         
,"military intelligence"                           
, "joseph-d_sir_ mobutu"                            
, "lawmaker")


actor<-c(actor, "laywer","attorney", "maimai", "militia", "nkunda", "fardc", "rcd", "un", "munoc", "cndp", "fdlr", "drc", "u.n.", 
         "fpjc", "united nations", "rcd"," upc", "m23")

actor<-apply(as.data.frame(actor), 1, 
             function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                          stemDocument)[[1]]$content)) )

actor2<-c("personnel","ministry", "government","party","administration","officials","company","worker","officer", "group")

actor2<-apply(as.data.frame(actor2), 1, 
             function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                          stemDocument)[[1]]$content)) )




###### 3) RELEVANT AND IRRELEVANT WORDS DICTIONARIES  ######
# if stemming, the different forms of irregular verbs (eg. feel, felt) should be added separately.
# if lemmatizing, adding the base form should suffice. 


###### dictionaries for the verb similarity variable ######
actions<-c('kill', 'fight', 'fought', 'attack', 'take', 'took','clash','occupy','seize','battle','shoot', 'shot',
           'wound', 'hit', 'murder',
           'barrage',  'bombard', 'slay', 'strike', 'capture', 'conquer' ,'bomb', 'target' ,'blood', 'intensify',
           'injur' ,'offend', 'hurt', "fire",
           'displace',
           'prosecute',
           'defeat' ,'obliterate' ,'assail', 'assault' ,
           'war' ,
           'force', 'control' ,'aggress' ,'burn' ,'terminate',
           'fortify', 'raid',
           'slaughter' ,'battle' ,'combat', 'purge', 'coerce' ,'storm' ,'smash' ,'skirmish' ,'collide' ,'agitate',
           'struggle' , 'outrage',
           'loot', 'pillage',
           'plunder' ,'oppress', 'squash' ,'squelch', 'suppress' ,'demolish' ,'shut' ,'repress', 'oppress' ,
           'beat', 'crush', 'die', 'death', 'toll', 'casualties',
           'plunge')

# action_stemmed<-apply(as.data.frame(actions), 1, 
#                       function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
#                                                                    stemDocument)[[1]]$content)) )

#without relying on the tm package (the above function produces the following list)
action_stemmed<-c( "kill"  ,    "fight"  ,   "fought"  ,  "attack"   , "take"   ,   "took"  , 
                   "clash" ,    "occupi"  ,  "seiz"    ,  "battl"   ,  "shoot"   , 
                   "shot"   ,   "wound"  ,   "hit"    ,   "murder"  ,  "barrag"  ,  "bombard" ,
                   "slay"  ,    "strike"   , "captur"  ,  "conquer"  , "bomb"     ,
                   "target"  ,  "blood"  ,   "intensifi", "injur"  ,   "offend"  ,  "hurt"    ,
                   "fire"    ,  "displac" ,  "prosecut",  "defeat"   , "obliter"  ,
                   "assail" ,   "assault" ,  "war"  ,     "forc"   ,   "control"   ,"aggress"   ,
                   "burn"    ,  "termin"  ,  "fortifi" ,  "raid"  ,    "slaughter",
                   "battl"   ,  "combat"   , "purg"    ,  "coerc"   ,  "storm"    , "smash"   ,
                   "skirmish" , "collid"  ,  "agit"  ,    "struggl"  , "outrag" ,  
                   "loot"    ,  "pillag"  ,  "plunder"  , "oppress" ,  "squash"   , "squelch"  ,
                   "suppress" , "demolish" , "shut"   ,   "repress"  , "oppress"  ,
                   "beat"   ,   "crush"  ,   "die"   ,    "death"  ,  "toll"  ,    "casualti",  "plung" )


non.actions<-c('say','said','report', 'confirm','interview','announce','accuse','arrest',
               'rape',
               'state','tell','told','base','cover','describe','confirm','declare','contain',"united","sourz")
# non.action_stemmed<-apply(as.data.frame(non.actions), 1, 
#                           function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
#                                                                        stemDocument)[[1]]$content)) )

#without relying on the tm package (the above function produces the following list)
non.action_stemmed<-c( "say"   ,    "said"  ,    "report" ,   "confirm" ,  "interview" ,"announc"   ,"accus"  ,
                       "arrest" ,   "rape" ,     "state"  ,   "tell"    ,
                       "told"    ,  "base"   ,   "cover"   ,  "describ" ,  "confirm" ,  "declar"  ,
                       "contain" ,  "unit"    ,  "sourz"   )





#### 4) GENERIC WORDS DICTIONARIES ####

#days
dayz<-c(
  #   "mon","tue","wed","thr","fri","sat","sun",
  "mon|monday","tue| tuesday","wed|wednesdays",
  "thr|thursday","fri|friday","sat|saturday",
  "sun|sunday")
dayz<-apply(as.data.frame(dayz), 1, 
             function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                          stemDocument)[[1]]$content)) )


#numerals
numeral<-c('one','two','three','four','five','six','seven','eight','nine','ten', 'hundred', 'hundreds','thousand','thousands',
           'few','many','several','million','millions', 'first', 'second', 'third', 'fourth', 'fifth', 
           'sixth', 'seventh', 'eighth','ninth','tenth')

numeral<-apply(as.data.frame(numeral), 1, 
             function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                          stemDocument)[[1]]$content)) )

#directional words
directz<-c("eastern", "western", "northern", "southern",
           #            "east","west","north","south", # be careful with this one when one of these words are used for province names
           "southwest",
           "southwestern",
           "southeast",
           "southeastern",
           "central",
           "northwest",
           "northwestern",
           "northeast",
           "northeastern")
directz<-apply(as.data.frame(directz), 1, 
               function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                            stemDocument)[[1]]$content)) )


#months
months<-c("january|jan","february|feb","march|mar","april|apr",
          "may"," june|jun", "july|jul","august|aug","september|sep|sept",
          "october|oct","november|nov","december|dec", "month|months"
          #             ,
          #             "","february", "march", "april","june",
          #             "july","august","september","october","november","december"
)
months<-apply(as.data.frame(months), 1, 
               function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
                                                            stemDocument)[[1]]$content)) )



# news agencies
# sources<-unique(as.character(drc$publisher))
# sources=c(sources, 'AP','CNN','AFP', 'Xinhua News Agency', 'Reuters', 'Yonhap', #some common abbreviations
#           'BBC', 'China Daily', 'Al Jazeera', 'Korea Times', 'Kyodo',
#           'Independent Online', 'South China Morning Post', 'FARS News Agency',
#           'Washington Post', 'IANS India', 'FARS News', 'Philippines News',
#           'Agence France-Presse', 'New York Times', 'EFE News', 'Xinhua')
# sources2<-c("news","report","radio","press","source")
# sources<-as.data.frame(as.character(sources[-duplicated(sources)]) )
# names(sources)<-"v"
# sources<-as.data.frame(tolower(sources[-is.na(sources$v),]) )
# names(sources)<-"v"
# sources<-apply(as.data.frame(sources), 1, 
#                                        function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
#                                                                                    stemDocument)[[1]]$content)) )



#without relying on the tm package (the above function produces the following list)

sources<-c( "reuter ltd."                                           
, "afp (world service)"                                   
, "all africa global media"                               
,"drcongo un-sponsor radio okapi"                        
, "bbc monitor"                                           
, "british broadcast corpor"                              
, "the new vision (internet version-www)"                 
,"radio canal revel"                                     
, "drc -- host product"                                   
, "afrol news"                                            
, "agenc franc press"                                     
, "the british broadcast corpor"                          
, "un news centr"                                         
, "the cairo post"                                        
, "integr region inform network"                          
, "news24"                                                
, "radio maria"                                           
, "bbc world servic"                                      
,"radio maendeleo"                                       
, "liquidafrica hold limit"                               
, "al jazeera english tv"                                 
, "al jazeera intern"                                     
, "misna"                                                 
, "press association, inc."                               
, "xinhua news agenc"                                     
, "dow jone & company, inc."                              
, "l'avenir onlin"                                        
, "human right watch"                                     
, "reuter limit"                                          
, "radio franc international"                             
, "trend agenc"                                           
, "bonesha fm"                                            
, "inquir interactive, inc."                              
, "la prosperit onlin"                                    
, "osc translat on sub-saharan africa"                    
, "radio bukavu"                                          
, "itron limit"                                           
, "rtnc radio"                                            
, "guardian newspap limit"                                
, "misna news"                                            
, "le soir (internet version-www)"                        
, "radio-televis national congolais tv"                   
, "la tempet des tropiqu"                                 
, "congoindepend"                                         
, "afp (north american service)"                          
, "radio candip"                                          
, "oana (organis of asia-pacif news agencies)"            
,"nationwid news pti ltd."                               
, "unit nation"                                           
, "goma rtnc radio"                                       
, "the monitor (internet version-www)"                    
, "acp (internet version-e-mail)"                         
, "un news center"                                        
, "le potentiel onlin"                                    
, "mega fm"                                               
, "acp"                                                   
, "radio okapi onlin"                                     
, "digitalcongo www-text"                                 
,"the news onlin"                                        
, "pana"                                                  
, "thomson reuter (markets) llc"                          
, "safm radio"                                            
, "le palmar"                                             
,"goma radio okapi"                                      
,"deutsch well"                                          
, "un integr region inform network (internet version-www)"
, "the south african press associ"                        
, "agenc de press africain"                               
, "radio okapi fm"                                        
, "drc -- osc summari"                                    
, "washington post"                                       
, "radio okapi"                                           
, "xinhua"                                                
, "raga fm"                                               
, "the new vision"                                        
, "misna www-text"                                        
, "telegraph.co.uk"                                       
, "radio libert"                                          
, "institut for secur studi"                              
, "sbs (special broadcast service)"                       
, "dow jone & compani inc."                               
, "bennett, coleman & co., ltd."                          
, "ap"                                                    
, "cnn"                                                   
, "afp"                                                   
, "xinhua news agenc"                                     
, "reuter"                                                
, "yonhap"                                                
, "bbc"                                                   
, "china daili"                                           
, "al jazeera"                                            
, "korea time"                                            
, "kyodo"                                                 
, "independ onlin"                                        
, "south china morn post"                                 
, "far news agenc"                                        
, "washington post"                                       
, "ian india"                                             
, "far news"                                              
, "philippin news"                                        
, "agenc france-press"                                    
, "new york time"                                         
, "efe news"                                              
, "xinhua")   




#administrative divisions
# admin<-c("area", 
#          "city", "cities",
#          "village", 
#          "province", "provincial",
#          "region",
#          "prefecture",
#          "town","township",
#          "county","counties",
#          "district",
#          "municipal","municipality",
#          "office",
#          "governorate", "muhafazat",
#          "territory", 
#          "suburb"
# )
# admin<-apply(as.data.frame(admin), 1, 
#                     function(word) as.character(as.String(tm_map(VCorpus(VectorSource(word)), 
#                                                                 stemDocument)[[1]]$content)) )

#without relying on the tm package (the above function produces the following list)
admin<-c("area"   ,   "citi"    ,  "citi"  ,    "villag" ,   "provinc"  , "provinci" ,
 "region"  ,  "prefectur" ,"town"    ,  "township" , "counti"  ,  "counti"   ,
"district"  ,"municip" ,  "municip" ,  "offic"  ,   "governor" , "muhafazat",
"territori", "suburb"  ) 

#back to the current working directory
setwd(current.path)
rm(current.path)






