

##########################################################################
######################### DO NOT RUN THIS CODE ##########################
#The original news articles are factiva proprietary and cannot be released.
#### THIS CODE IS JUST TO SHOW WHAT WE DID IN THE PREPROCESSING STEP #####
# INSTEAD, START WITH "drc_cleantext.csv" FILE IN "2_algorithm_drc.R" #
##########################################################################


############ 0) SET UP ###############
base.path<- " "
r.path<-paste0(base.path, "/Rcodes")
drc.path<-paste0(base.path, "/drc")
# load packages/ helper functions (manually defined)
setwd(r.path) 
source("setup.R", echo=T, print.eval = T)
source("functions.R", echo=T, print.eval = T)


######### 1) Building Dictionaries ########
# load dictionaries pre-defined for the dataset
setwd(drc.path)
source("dictionary_drc.R", echo=T, print.eval = T)
## --> "drc_province" & "names" have been created.
## --> dictionaries have been created as well.




######## 2) Pre-treatment (text cleaning) ########
# load the data: reports on "fight" in drc -- this data file NOT released (factiva property)
drc_data<-read.csv(" ") # unit = article 
# convert all characters to utf-8 as foreign characters are often coded in ASCII, which R does not like..
texts<-as.character(iconv(drc_data$text, "ASCII", "utf-8", sub=" "))
# to lower characters (R filters differentiates upper and lower cases..)
texts2<-tolower(texts)
# remove posessives
texts<-str_replace_all(texts, "\'s", " ")
texts<-str_replace_all(texts, "s\'", " ")
# Remove bylines
texts<-str_replace_all(texts2, "\n\n", "--- ")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\) ---", "")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)---", "")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\) -","")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)-","")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)--","")
texts<-str_replace_all(texts, "---", ".")
texts2 = texts %>% str_replace(. ,"reuters kinshasa:","")
texts2 = texts2 %>% str_replace(. ,"kinshasa local press summary -june 17, 2013","") #text_no 17
texts2 = texts2 %>% str_replace(. ,"--bunia [northeastern drcongo], ","") %>% str_replace(.,"22 may:","") %>% str_replace(.,"--text of report by un regional information network irin on 22 may","") #text_no 29
texts2 = texts2 %>% str_replace(. ,"kinshasa, 1 february:","")  %>%str_replace(.,"excerpt from report by un regional information network irin on 1 february","")
texts2 = texts2 %>% str_replace(. ,"kinshasa:","") #112,136
texts2 = texts2 %>% str_replace(. ,"bunia, 13/11 ","") %>% str_replace(. ,"(acp).","") #165
texts2 = texts2 %>% str_replace(. ,"kinshasa, congo (ap)--","") #168
texts2 = texts2 %>% str_replace(. ,"text of report by rwandan news agency rna","") %>% str_replace(. ,"kigali, ","") %>% str_replace(. ,"29 may (ari)","")#204
texts2 = texts2 %>% str_replace(. ,"kigali, 29 may (ari)","")
texts2 = texts2 %>% str_replace(. ,"kigali, 30 june:","") %>% str_replace(. ,"text of report in english by rwandan news agency rna","")
texts2 = texts2 %>% str_replace(. ,"kinshasa, 2 january:","") %>% str_replace(. ,"excerpt from report by un regional information network irin on 2 january","") # 225
texts2 = texts2 %>% str_replace(. ,"\\(reuters\\)","")%>% str_replace(.,"\nkinshasa, sept 6","") %>%str_replace(.,"\\(adds details\\)","")


# convert sentence breakers to "." (periods)
texts<-str_replace_all(texts, "\n", ". ")
texts<-str_replace_all(texts, " -- ", ". ")
texts<-str_replace_all(texts, "<", ". ")
texts<-str_replace_all(texts, ">", ". ")
texts<-str_replace_all(texts, "\\[", ". ")
texts<-str_replace_all(texts, "\\]", ". ")
texts<-str_replace_all(texts, " - ", ". ")
texts<-str_replace_all(texts, ";", ". ")
texts<-str_replace_all(texts, ":", ". ")
texts<-str_replace_all(texts, "Mr\\. ", "Mr")
texts<-str_replace_all(texts, "u\\.n\\.", "un")
texts<-str_replace_all(texts, "u\\.s\\.", "us")
texts<-str_replace_all(texts, "\\..", "\\.")


######## 3) Homogenization ########
# homogenize the province names (city names -> province names; consistent spelling, etc.)
newvar<-AtoBinC(drc_province$city, drc_province$province_levels, texts)

# remove stop words in english -> stem words (Porter stemmer)
#modify stopwords("english") provided in the tm package
l=stopwords("english")
l[c(120, 121, 122, 123, 135, 136, 139, 140)] #don't remove prepositions used with location words from the texts. 
# "of" "at"   "by"  "for" "to"   "from" "in"   "out" 
l=l[-c(120, 121, 122, 123, 135, 136, 139, 140)]  
newvar<-text_treatment(newvar, stopwords=l)

# 5) changed province names ending with ing back to original names (eg. stemmer removes "ing"s)
text_treatment(names, stopwords=l)
names #difference? kasaioccident -> kasaioccidental, kasai

newvar<-str_replace_all(newvar,"kasaioccident","kasaioccidental")
newvar<-str_replace_all(newvar,"kasaiorient","kasaioriental")
newvar<-str_replace_all(newvar," oriental","orientale")



######## 4) Generalization ########
# generalize some phrases/words (As much as possible!) the code should be a "non-existing" "UNCOMMON" word
# generalize all numbers -> N
newvar<-as.character(iconv(newvar, "ASCII", "utf-8", sub=" "))
newvar<-tolower(newvar)
newvar<-AtoBinC(drc_province$city, drc_province$province, newvar)

# removing special characters
newvar<-cleaning(newvar, pattern.list=special.characters, option="whitespace") # special characters defined in setup.R
#! be careful not to remove any special characters that are in province/city names here

# generalize some phrases/words (As much as possible!) the code should be a "non-existing" "UNCOMMON" word
# generalize all numbers -> N
newvar<-generalize_numbers(newvar)
newvar<-generalize(newvar, list=tolower(numeral), code=" N ", capture.nested=FALSE)
newvar<-str_replace_all(newvar, "NN", "N")
newvar<-str_replace_all(newvar, "N N", "N")
newvar<-str_replace_all(newvar, "N  N", "N")
newvar<-str_replace_all(newvar, "N   N", "N")
newvar<-str_replace_all(newvar, "N    N", "N")
# and all month names -> MONZ
newvar<-generalize(newvar, list=months, code=" MONZ ", capture.nested=FALSE)

# web addresses -> WWW
newvar<-generalize_webaddress(newvar)

# news sources -> SOURZ
newvar<-generalize(newvar, list=tolower(sources$v), code=" SOURZ ", capture.nested=FALSE)
newvar<-generalize(newvar, list=tolower(sources2), code=" SOURZ ", capture.nested=FALSE)

#actors -> ACTAR
newvar<-generalize(newvar, list=tolower(actor), code=" ACTAR ", capture.nested=FALSE)
newvar<-generalize(newvar, list=tolower(actor2), code=" ACTAR ", capture.nested=FALSE)
newvar<-str_replace_all(newvar, "ACTARACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR ACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR  ACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR   ACTAR", "ACTAR")

# words referrnig to administrative divisions -> ADMINN
newvar<-generalize(newvar, list=tolower(admin), code=" ADMINN ", capture.nested=FALSE)

#days to DAYZ
newvar<-generalize(newvar, list=tolower(dayz), code=" DAYZ ", capture.nested=FALSE)

#direction to DIRECZ
newvar<-generalize(newvar, list=tolower(directz), code=" DIRECTZ ", capture.nested=FALSE)


# extra step: removing more special characters. (doing it all once at the beginning created problems)
newvar<-str_replace_all(newvar, "\\--", " . ")
newvar<-str_replace_all(newvar, "\\-", "")
newvar<-str_replace_all(newvar, "/", " ")
newvar<-str_replace_all(newvar, "\\,", "")
newvar<-str_trim(newvar, side="both")


if(any(str_detect(newvar, "  ") )) {
  newvar<-str_replace_all(newvar, "  ", " ")
} 


newvar2 = str_replace_all(newvar, "sub ", "sub_")# because "_" is cleaned away, we're adding it back
newvar2 = str_replace_all(newvar2, "suborientale", "sub_orientale")
newvar2 = str_replace_all(newvar2, " s ", " ") ## many "monthes" converted to "MONZ s" as two words
newvar2 = generalize(newvar2, list=action_stemmed, code="AVERB", capture.nested=FALSE) # !!convert verbs
newvar2 = generalize(newvar2, list=non.action_stemmed, code="NONTOPIC", capture.nested=FALSE) # !!convert verbs


# store the pre-processed text as "cleantext"
drc_data$cleantext<-newvar2 



##### homogenizing the province names coded by human just in case 
drc_data$province_human<-as.character(unlist(drc_data$province_human))

##### homogenizing the province names coded by ICEWS
drc_data$province_ICEWS<-AtoBinC(tolower(drc_province$city), 
                                 tolower(drc_province$province), 
                                 tolower(drc_data$province))
drc_data$province_ICEWS<-str_replace_all(drc_data$province_ICEWS, "province", "")
drc_data$province_ICEWS<-str_trim(drc_data$province_ICEWS, side="both")
drc_data$province_ICEWS<-as.character(unlist(drc_data$province_ICEWS))


data<-as.data.frame(cbind("story_id"=drc_data$story_id,
                          "province_human"=drc_data$province_human,
                          "province_ICEWS"=drc_data$province_ICEWS,
                          "cleantext"=drc_data$cleantext))
data$cleantext<-as.character(unlist(data$cleantext))
data$province_ICEWS<-as.character(unlist(data$province_ICEWS))
data$province_human<-as.character(unlist(data$province_human))


#save the data 
write.csv(data, "drc_cleantext.csv")


##### End of preprocessing the DRC data #### 
