##########################################################################
######################### DO NOT RUN THIS CODE ##########################
#The original news articles are factiva proprietary and cannot be released.
#### THIS CODE IS JUST TO SHOW WHAT WE DID IN THE PREPROCESSING STEP #####
# INSTEAD, START WITH "syria_cleantext.csv" FILE IN "2_algorithm_syria.R" #
##########################################################################



############ 0) SET UP ###############
base.path<- " "
r.path<-paste0(base.path, "/Rcodes")
syria.path<-paste0(base.path, "/syria")
# load packages/ helper functions (manually defined)
setwd(r.path)
source("setup.R", echo=T, print.eval = T)
source("functions.R", echo=T, print.eval = T)


######### 1) Building Dictionaries ########
# load dictionaries pre-defined for the dataset
setwd(syria.path)
source("dictionary_syria.R", echo=T, print.eval = T)
## --> "syria_province" & "names" have been created.
## --> dictionaries have been created as well.



######## 2) Pre-treatment (text cleaning) ########

syria_data<-read.csv("syria_data_raw.csv")
texts<-as.character(iconv(syria_data$text, "ASCII", "utf-8", sub=" "))


texts<-tolower(texts)
# remove posessives
texts<-str_replace_all(texts, "\'s", " ")
texts<-str_replace_all(texts, "s\'", " ")
# remove bylines
texts<-str_replace_all(texts, "\n\n", "--- ")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\) ---", "")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)---", "")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\) -", "")
texts = texts %>% str_replace(. , "^.*?\\(.*?\\)-", "")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)--", "")
texts<-str_replace(texts, "Damascus, July 9", " ") # 21
texts<-str_replace(texts, "Damascus, June 11", " ") #7


# convert sentence breakers to "." (periods)
texts<-str_replace_all(texts, "---", ".")
texts<-str_replace_all(texts, "\n", ". ")
texts<-str_replace_all(texts, " -- ", ". ")
texts<-str_replace_all(texts, "<", ". ")
texts<-str_replace_all(texts, ">", ". ")
texts<-str_replace_all(texts, "\\[", ". ")
texts<-str_replace_all(texts, "\\]", ". ")
texts<-str_replace_all(texts, " - ", ". ")
texts<-str_replace_all(texts, ";", ". ")
texts<-str_replace_all(texts, ":", ". ")
texts<-str_replace_all(texts, "\\(", ". ")
texts<-str_replace_all(texts, "\\)", ". ")
texts<-str_replace_all(texts, "Mr\\. ", "Mr")
texts<-str_replace_all(texts, "u\\.n.", "un")
texts<-str_replace_all(texts, "u\\.s.", "us")
texts<-str_replace_all(texts, "\\.\\.", "\\.")
texts<-str_replace_all(texts, "\\. \\.", "\\. ")
texts<-cleaning(texts, pattern.list=special.characters)



######## 3) Homogenization ########
# homogenize the province names (city names -> province names; consistent spelling, etc.)
texts = AtoBinC(syria_province$sublevel, syria_province$domain_replace, texts)

# remove stop words in english & stem words (Porter stemmer)
#modify stopwords("english") provided in the tm package
l=stopwords("english") ## shouldn't toss and
l[c(120, 121, 122, 123, 135, 136, 139, 140, 141, 112)] #don't remove prepositions used with location words from the texts.
# "of" "at"   "by"  "for" "to"   "from" "in"   "out" "on" "and"
# ex. around 30 miles southeast of Aleppo city
l=l[-c(120, 121, 122, 123, 135, 136, 139, 140, 141, 112)]
newvar<-text_treatment(texts, stopwords=l)

# changed province names ending with ing back to original names (eg. stemmer removes "ing"s)
#any(text_treatment(names, stopwords=l) ==names) #difference? (IF FALSE) change it back!
###### -> None of the Syrian province names have been affected by the stemmer. 



######## 4) Generalization ########
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
newvar<-generalize(newvar, list=tolower(sources1), code=" SOURZ ", capture.nested=FALSE)
newvar<-generalize(newvar, list=tolower(sources2), code=" SOURZ ", capture.nested=FALSE)
newvar<-generalize(newvar, list=tolower(sources3), code=" SOURZ ", capture.nested=FALSE)
#actors -> ACTAR

newvar<-generalize(newvar, list=tolower(actor), code=" ACTAR ", capture.nested=FALSE)
newvar<-generalize(newvar, list=tolower(actor2), code=" ACTAR ", capture.nested=FALSE)
newvar<-str_replace_all(newvar, "ACTARACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR ACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR  ACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR   ACTAR", "ACTAR")

# words referrnig to administrative divisions -> ADMINN
newvar<-generalize(newvar, list=tolower(admin), code=" ADMINN ", capture.nested=FALSE)
newvar <- generalize(newvar, list=tolower(adminz), code="ADMINNZZ", capture.nested=FALSE) 

#days to DAYZ
newvar<-generalize(newvar, list=tolower(dayz), code=" DAYZ ", capture.nested=FALSE)

#direction to DIRECZ
newvar<-generalize(newvar, list=tolower(directz), code=" DIRECTZ ", capture.nested=FALSE)


# relevant and irrelevant words
newvar = generalize(newvar, list=action_stemmed, code="AVERB", capture.nested=FALSE) # !!convert verbs
newvar= generalize(newvar, list=non.action_stemmed, code="NONTOPIC", capture.nested=FALSE) # !!convert verbs

newvar<-str_replace_all(newvar, "-", "")


# store the pre-processed text as "cleantext"
syria_data$cleantext<-newvar

##### homogenizing the province names coded by human just in case
syria_data$province_human<-  str_replace_all(as.character(syria_data$province_original), "\\(.*?\\)", "")
syria_data$province_human<-AtoBinC(tolower(syria_province$sublevel),
                                   tolower(syria_province$domain_replace),
                                   tolower(syria_data$province_human))
syria_data$province_human<-str_replace_all(syria_data$province_human, "and ", "," )

##### homogenizing the province names coded by oeda

syria_data$province_oeda = syria_data$province_oeda %>% as.character()
syria_data$province_oeda = syria_data$province_oeda %>% str_replace_all(. , " Governorate", "")
syria_data$province_oeda = syria_data$province_oeda %>% str_replace_all(. , "Mu���_ǩ�_ǁfaz���_at R�_�_f Dimashq", "")
syria_data$province_oeda = syria_data$province_oeda %>% str_replace_all(. , "Mu’\u008dÎÙÎ©’ÜÎ\u0081faz’‘Î_at `Amm’ÜÎ\u0081n", "")
syria_data$province_oeda[nchar(syria_data$province_oeda) ==0 ] <- "no_loc"
syria_data$province_oeda<-as.character(iconv(syria_data$province_oeda, "ASCII", "utf-8", sub=" "))
syria_data$province_oeda<-str_replace_all(tolower(syria_data$province_oeda), "governorate", "")
syria_data$province_oeda<-AtoBinC(tolower(syria_province$sublevel),
                                  tolower(syria_province$domain_replace),
                                   tolower(syria_data$province_oeda))
syria_data$province_oeda<-str_trim(syria_data$province_oeda, side="both")



##### save the data frame #####

data<-as.data.frame(cbind("story_id"=as.character(syria_data$oeda),
                          "id"=as.character(syria_data$id),
                          "province_human"=syria_data$province_human,
                          "province_oeda"=syria_data$province_oeda,
                          "cleantext"=syria_data$cleantext))
data$cleantext<-as.character(unlist(data$cleantext))
data$province_oeda<-as.character(unlist(data$province_oeda))
data$province_human<-as.character(unlist(data$province_human))
data$province_oeda[is.na(data$province_oeda)]<-"no_loc"


# save the data
write.csv(data, "syria_cleantext.csv")
# rm(syria_data)



##### End of preprocessing the syria data #### 

