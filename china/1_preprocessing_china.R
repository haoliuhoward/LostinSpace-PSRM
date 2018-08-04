
##########################################################################
######################### DO NOT RUN THIS CODE ##########################
#The original news articles are factiva proprietary and cannot be released.
#### THIS CODE IS JUST TO SHOW WHAT WE DID IN THE PREPROCESSING STEP #####
# INSTEAD, START WITH "china_cleantext.csv" FILE IN "2_algorithm_china.R" #
##########################################################################


############ 0) SET UP ###############
base.path<- " "
r.path<-paste0(base.path, "/Rcodes")
china.path<-paste0(base.path, "/china")
# load packages/ helper functions (manually defined)
setwd(r.path)
source("setup.R", echo=T, print.eval = T)
source("functions.R", echo=T, print.eval = T)



######### 1) Building Dictionaries ########
# load dictionaries pre-defined for the dataset
setwd(china.path)
source("dictionary_china.R", echo=T, print.eval = T)
## --> "china_province" & "names" have been created.
## --> dictionaries have been created as well.




######## 2) Pre-treatment (text cleaning) ########
# load the data: reports on "protest" in China -- this data file NOT released (factiva property)
china_data<-read.csv("     ") # unit = article 
# convert all characters to utf-8 as foreign characters are often coded in ASCII, which R does not like..
texts<-as.character(iconv(china_data$text, "ASCII", "utf-8", sub=" "))
# to lower characters (R filters differentiates upper and lower cases..)
texts<-tolower(texts)
# remove posessives
texts<-str_replace_all(texts, "\'s", " ")
texts<-str_replace_all(texts, "s\'", " ")
# Remove bylines
texts<-str_replace_all(texts, "\n\n", "--- ")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\) ---", "")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)---", "")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\) -","")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)-","")
texts = texts %>% str_replace(. ,"^.*?\\(.*?\\)--","")
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
texts<-str_replace_all(texts, ")  ", ". ")
# Remove the non-sentence breaking periods. (text dependent)
texts<-str_replace_all(texts, "Mr\\. ", "Mr")
texts<-str_replace_all(texts, "u\\.n.", "un")
texts<-str_replace_all(texts, "u\\.s.", "us")
texts<-str_replace_all(texts, "\\..", "\\.")
# removing special characters
texts<-cleaning(texts, pattern.list=special.characters, option="whitespace") # special characters defined in the dictionary
#! be careful not to remove any special characters that are in province/city names here





######## 3) Homogenization ########
# homogenize the province names (city names -> province names; consistent spelling, etc.)
newvar<-AtoBinC(china_province$city, china_province$province, texts)

# remove stop words in english  & stem words (Porter stemmer)
package("openNLPmodels.en","NLP","openNLP")  # They require manual installation.
#modify stopwords("english") provided in the tm package
l=stopwords("english")
l[c(120, 121, 122, 123, 135, 136, 139, 140)] #don't remove prepositions used with location words from the texts.
# "of" "at"   "by"  "for" "to"   "from" "in"   "out"
l=l[-c(120, 121, 122, 123, 135, 136, 139, 140)]
newvar<-text_treatment(newvar, stopwords=l) 

# change province names ending with "ing" back to original names (stemmer removes "ing"s)
newvar<-str_replace_all(newvar,"liaon","liaoning")
newvar<-str_replace_all(newvar,"chongq","chongqing")
newvar<-str_replace_all(newvar,"beij","beijing")
newvar<-str_replace_all(newvar,"beijinging","beijing")
newvar<-str_replace_all(newvar,"hong kong","hongkong")
newvar<-str_replace_all(newvar,"inner mongolia","innermongolia")
newvar<-str_replace_all(newvar," mongolia","innermongolia")


######## 4) Generalization ########
# generalize some phrases/words (As much as possible!) the code should be a "non-existing" "UNCOMMON" word
# generalize all numbers -> N
newvar<-generalize_numbers(newvar)
newvar<-generalize(newvar, list=numeral, code="N", capture.nested=FALSE)
newvar<-str_replace_all(newvar, "NN", "N")
newvar<-str_replace_all(newvar, "N N", "N")
newvar<-str_replace_all(newvar, "N  N", "N")
newvar<-str_replace_all(newvar, "N   N", "N")
newvar<-str_replace_all(newvar, "N    N", "N")
# and all month names -> MONZ
newvar<-generalize_months(newvar)
# web addresses -> WWW
newvar<-generalize_webaddress(newvar)
# news sources -> SOURZ
newvar<-generalize(newvar, list=tolower(sources), code="SOURZ", capture.nested=FALSE)
newvar<-generalize(newvar, list=tolower(sources2), code="SOURZ", capture.nested=FALSE)
#actors -> ACTAR
newvar<-generalize(newvar, list=tolower(actor), code="ACTAR", capture.nested=TRUE)
newvar<-generalize(newvar, list=tolower(actor2), code="ACTAR", capture.nested=TRUE)
newvar<-str_replace_all(newvar, "ACTARACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR ACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR  ACTAR", "ACTAR")
newvar<-str_replace_all(newvar, "ACTAR   ACTAR", "ACTAR")
# words referrnig to administrative divisions -> ADMINN
newvar<-generalize(newvar, list=tolower(admin), code="ADMINN", capture.nested=TRUE)
#days to DAYZ
newvar<-generalize(newvar, list=tolower(dayz), code="DAYZ ", capture.nested=FALSE)
#direction to DIRECZ
newvar<-generalize(newvar, list=tolower(directz), code="DIRECTZ ", capture.nested=TRUE)

# relevant and irrelevant words
newvar = generalize(newvar, list=action_stemmed, code="AVERB", capture.nested=FALSE) # !!convert verbs
newvar= generalize(newvar, list=non.action_stemmed, code="NONTOPIC", capture.nested=FALSE) # !!convert verbs


# store the pre-processed text as "cleantext"
china_data$cleantext<-newvar

##### homogenizing the province names coded by human just in case 
china_data$province_human<-AtoBinC(tolower(china_province$city),
                                   tolower(china_province$province),
                                   tolower(china_data$province_human))

##### homogenizing the province names coded by ICEWS
china_data$province_ICEWS<-AtoBinC(tolower(china_province$city),
                                   tolower(china_province$province),
                                   tolower(china_data$province_ICEWS))
china_data$province_ICEWS<-str_trim(china_data$province_ICEWS, side="both")



data<-as.data.frame(cbind("story_id"=china_data$story_id,
                          "province_human"=china_data$province_human,
                          "province_ICEWS"=china_data$province_ICEWS,
                          "cleantext"=china_data$cleantext))
data$cleantext<-as.character(unlist(data$cleantext))
data$province_ICEWS<-as.character(unlist(data$province_ICEWS))
data$province_human<-as.character(unlist(data$province_human))



# save the data
write.csv(data, "china_cleantext.csv")
rm(china_data)



##### End of preprocessing the China data #### 