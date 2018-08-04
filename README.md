---
output:
  html_document: default
  pdf_document: default
---
# Lost in Space: Geolocation in Event Data
### Sophie J. Lee, Howard Liu and Michael D. Ward
For the detailed description of our alogorithm, please see the preprint of our paper here: https://arxiv.org/abs/1611.04837

========================================

# Table of Contents

1. [Introduction](#Introduction)

2. [Replication](#Replication)

3. [Application](#Application)





========================================



# Introduction <a name="Introduction"></a>

#### 1. Abstract

The aim of LostInSpace program is to extract the "correct" place of event from text data through the pipeline that consists of text preprocessing, feature selection and classification. We begin by homogenizing and generalizing the raw texts in terms of locations, actors, relevant words, and irrelevant words. In the feature selection stage, we extract contextual information from texts, namely, the N-gram patterns for location words, the frequency of mention, and the context of the sentences containing location words. In the classification stage, we use three classifiers to estimate the model parameters in the training set and then to predict whether a location word in the test set news articles is the place of the event. The validation results show that our algorithm improves the accuracy rate of the current geolocation methods of dictionary approach by as much as 25%. The program works best with the structured data such as news articles. 

*Flow Chart for the LostInSpace Pipeline
http://people.duke.edu/~jl324/flowchart.pdf 


#### 2. Folders in this repository

'china': This folder contains the China data (ICEWS) as well as codes that we used to produce the results in the paper.

'drc': This folder contains the D. R. Congo (ICEWS) data as well as codes that we used to produce the results in the paper.

'syria': This folder contains the Syria data (OEDA) as well as codes that we used to produce the results in the paper.

'Rcodes': This folder contains the helper functions we built to extract the correct event locations from structured text data.

'visualization': This folder contains the figures we included in the paper.


========================================

# Replication <a name="Replication"></a>


To replicate the numbers in our paper, please click a country name folder and run the algorithm codes named as "2_algorithm_country name.R". 
For the figures in the paper, go to "visualization" folder and run "creating_plots.R". 


========================================

# Application <a name="Application"></a>

To use LostInSpace, please download the dependencies listed in setup.R and the codes in functions.R. Simply by running setup.R first and then functions.R would suffice. Then you can perform the same task as in algorithms files under country name folders. Descriptions of the functions we built are as follows. A more detailed version is in in the paper. 

### Functions for pre-treatment 

##### cleaning
This function removes special characters pre-defined. 

##### text_treatment
This function removes stop words (a list containing words that do not carry meaning) defined in texts and stems words (Porter Stemmer). The function relies on 'tm' package.  

##### generalize
this function generalizes (converts) the words defined to the user defined word

##### AtoBinC
This function recognizes A (words in vector A) in C (text vector) and converts to B (words in vector B). The length of A and B has to be the same. For instance, if A and B were to be vectors of length 2 as in the following table, "AtoBinC(A, B, texts)" would convert all "bj"s and "peking" in the texts to "beijing". 

| A  | B   |
|---|---|
| bj  | beijing  |
| peking  |  beijing |


### Functions for feature selection 

##### incNgrams 
This function creates the Ngram corpus of incorrect location words. Human coder has to pre-determine the correct locations from each text.

##### corNgrams
This function creates the Ngram corpus of correct location words.

##### buildY
This function builds the data frame containing Y (1=correct event location, 0=incorrect event location) 

##### buildNgram1
Ngram frequency: This function calculates the rates of the given Ngram in the frequent N-grams lists. The function takes options of "within=article" and "within=data".

##### buildNgram2
Matched N-gram: how many of the collocation patterns for each loc. word are matched to the top N-gram patterns? 
This function calculates the matched rates of the given Ngram in the most frequent N-grams lists. The function takes options of "within=article" and "within=data".


##### buildFrequency
Frequency: how many times does this location word appear in the article? The function takes options of "within=article" and "within=data".

##### buildContext
Context : Is the topic discussed in the sentence something related to the scope of interest? The function takes options of "within=article" and "within=data". This function requires pre-treatment of texts using pre-defined dictionaries of relevant and irrelevant words. 



### Working examples 

Please click the folders with a country name and follow the steps in:

1.dictionary_"country name".R

and

2.algorithm_"country name".R





















