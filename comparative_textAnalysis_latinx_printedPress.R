# Script Name:          comparative_textAnalysis_latinx_printedPress.R
# Created On:           July_30_2023
# Author:               Dr. Martin Calvino
# Purpose:              analyze the contextual meaning of the latinx term depending on the newspaper
# Version:              v1.07.30.2023

# Corpus:               text extracted from 6 newspapers containing the term latinx
# The New York Times:   103 news articles up to April-22-2022
# Houston Chronicle:    101 news articles up to May-04-2022
# Los Angeles Times:    101 news articles up to May-06-2022
# Miami Herald:         78 news articles up to May-13-2022
# The Washington Post:  20 news articles up to May-17-2022
# Chicago Tribune:      31 news articles up to May-20-2022


# load libraries
library(tidyverse)
library(tidytext)
library(patchwork)
library(mallet)
library(wordcloud)


### RELATIVE FREQUENCY COUNT FOR LATIN AMERICAN COUNTRIES DEPENDING ON THE NEWSPAPER

# Visualize mentions of Latin American countries per newspaper

# create a function that tokenizes text
tokenizeText <- function(textFile) {
  textFile <- file.choose()
  textFile.scanned <- scan(textFile, what = "character", sep = "\n")
  textFile.scanned <- paste(textFile.scanned, collapse = " ")
  textFile.scanned <- tolower(textFile.scanned)
  textFile.scanned.l <- strsplit(textFile.scanned, "[^A-Za-z0-9'_]")
  textFile.scanned.v <- unlist(textFile.scanned.l)
  notBlanks <- which(textFile.scanned.v != "")
  textFile.scanned.v.withoutBlanks <- textFile.scanned.v[notBlanks]
  return(textFile.scanned.v.withoutBlanks)
}

# tokenise text from news articles for each newspaper
# I don't know the reason but my tokenizer does not take contractions on the text files I created
# "museum's" is processed as "museum" + "s"

# Washington Post
wp <- tokenizeText(wpText)
wp
# New York Times
nyt <- tokenizeText(nytText)
nyt
# LA Times
lat <- tokenizeText(latText)
lat
# Miami Herald
mihe <- tokenizeText(miheText)
mihe
# Chicago Tribune
chit <- tokenizeText(chitText)
chit
# Houston Chronicle
huc <- tokenizeText(hucText)
huc

# create vectors containing tokens associated with countries in Latin America
Argentina <- c("argentina", "argentineans", "argentines", "argentine", "argentinean")
Bolivia <- c("bolivia", "bolivians", "bolivian")
Chile <- c("chile", "chileans", "chilean")
Colombia <- c("colombia", "colombians", "colombian")
Costa_Rica <- c("costa_rica", "costa_ricans", "costa_rican")
Cuba <- c("cuba", "cubans", "cuban")
Dominican_Republic <- c("dominican", "dominican_republic", "dominicans")
Ecuador <- c("ecuador", "ecuadorians", "ecuadoran")
El_Salvador <- c("el_salvador", "salvadorans", "el_salvadorans", "el_salvadoran", "salvadoran")
Guatemala <- c("guatemala", "guatemalans", "guatemalan")
Honduras <- c("honduras", "hondurans", "honduran")
Mexico <- c("mexico", "mexicans", "chicano", "chicana", "chicanos", "chicanas", "chicanxs", "mexican")
Nicaragua <- c("nicaragua", "nicaraguans", "nicaraguan")
Panama <- c("panama", "panamanians", "panamenian")
Paraguay <- c("paraguay", "paraguayans", "paraguayan")
Peru <- c("peru", "peruvians")
Puerto_Rico <- c("puerto_rico", "puerto_rican", "puerto_ricans", "boricua")
Uruguay <- c("uruguay", "uruguayans", "uruguayan")
Venezuela <- c("venezuela", "venezuelans", "venezuelan")

# calculate relative frequency of mentions for each country

# Washington Post newspaper
argentina.wp <- 100*(length(which(wp == Argentina)) / length(wp))
bolivia.wp <- 100*(length(which(wp == Bolivia)) / length(wp))
chile.wp <- 100*(length(which(wp == Chile)) / length(wp))
colombia.wp <- 100*(length(which(wp == Colombia)) / length(wp))
costarica.wp <- 100*(length(which(wp == Costa_Rica)) / length(wp))
cuba.wp <- 100*(length(which(wp == Cuba)) / length(wp))
dominican.wp <- 100*(length(which(wp == Dominican_Republic)) / length(wp))
ecuador.wp <- 100*(length(which(wp == Ecuador)) / length(wp))
salvador.wp <- 100*(length(which(wp == El_Salvador)) / length(wp))
guatemala.wp <- 100*(length(which(wp == Guatemala)) / length(wp))
honduras.wp <- 100*(length(which(wp == Honduras)) / length(wp))
mexico.wp <- 100*(length(which(wp == Mexico)) / length(wp))
nicaragua.wp <- 100*(length(which(wp == Nicaragua)) / length(wp))
panama.wp <- 100*(length(which(wp == Panama)) / length(wp))
paraguay.wp <- 100*(length(which(wp == Paraguay)) / length(wp))
peru.wp <- 100*(length(which(wp == Peru)) / length(wp))
puertorico.wp <- 100*(length(which(wp == Puerto_Rico)) / length(wp))
uruguay.wp <- 100*(length(which(wp == Uruguay)) / length(wp))
venezuela.wp <- 100*(length(which(wp == Venezuela)) / length(wp))

# create a function to calculate relative frequency of mentions (what I did on the above code)
# country relative frequency as 'corf'
corf <- function(Country, newspaper) {
  Country <-  100*(length(which(newspaper == Country)) / length(newspaper))
  return(Country)
}

# relative frequency of mentions for each country
# The New York Times
argentina.nyt <- corf(Argentina, nyt)
bolivia.nyt <- corf(Bolivia, nyt)
chile.nyt <- corf(Chile, nyt)
colombia.nyt <- corf(Colombia, nyt)
costarica.nyt <- corf(Costa_Rica, nyt)
cuba.nyt <- corf(Cuba, nyt)
dominican.nyt <- corf(Dominican_Republic, nyt)
ecuador.nyt <- corf(Ecuador, nyt)
salvador.nyt <- corf(El_Salvador, nyt)
guatemala.nyt <- corf(Guatemala, nyt)
honduras.nyt <- corf(Honduras, nyt)
mexico.nyt <- corf(Mexico, nyt)
nicaragua.nyt <- corf(Nicaragua, nyt)
panama.nyt <- corf(Panama, nyt)
paraguay.nyt <- corf(Paraguay, nyt)
peru.nyt <- corf(Peru, nyt)
puertorico.nyt <- corf(Puerto_Rico, nyt)
uruguay.nyt <- corf(Uruguay, nyt)
venezuela.nyt <- corf(Venezuela, nyt)

# LA Times
argentina.lat <- corf(Argentina, lat)
bolivia.lat <- corf(Bolivia, lat)
chile.lat <- corf(Chile, lat)
colombia.lat <- corf(Colombia, lat)
costarica.lat <- corf(Costa_Rica, lat)
cuba.lat <- corf(Cuba, lat)
dominican.lat <- corf(Dominican_Republic, lat)
ecuador.lat <- corf(Ecuador, lat)
salvador.lat <- corf(El_Salvador, lat)
guatemala.lat <- corf(Guatemala, lat)
honduras.lat <- corf(Honduras, lat)
mexico.lat <- corf(Mexico, lat)
nicaragua.lat <- corf(Nicaragua, lat)
panama.lat <- corf(Panama, lat)
paraguay.lat <- corf(Paraguay, lat)
peru.lat <- corf(Peru, lat)
puertorico.lat <- corf(Puerto_Rico, lat)
uruguay.lat <- corf(Uruguay, lat)
venezuela.lat <- corf(Venezuela, lat)

# Miami Herald
argentina.mihe <- corf(Argentina, mihe)
bolivia.mihe <- corf(Bolivia, mihe)
chile.mihe <- corf(Chile, mihe)
colombia.mihe <- corf(Colombia, mihe)
costarica.mihe <- corf(Costa_Rica, mihe)
cuba.mihe <- corf(Cuba, mihe)
dominican.mihe <- corf(Dominican_Republic, mihe)
ecuador.mihe <- corf(Ecuador, mihe)
salvador.mihe <- corf(El_Salvador, mihe)
guatemala.mihe <- corf(Guatemala, mihe)
honduras.mihe <- corf(Honduras, mihe)
mexico.mihe <- corf(Mexico, mihe)
nicaragua.mihe <- corf(Nicaragua, mihe)
panama.mihe <- corf(Panama, mihe)
paraguay.mihe <- corf(Paraguay, mihe)
peru.mihe <- corf(Peru, mihe)
puertorico.mihe <- corf(Puerto_Rico, mihe)
uruguay.mihe <- corf(Uruguay, mihe)
venezuela.mihe <- corf(Venezuela, mihe)

# Chicago Tribune
argentina.chit <- corf(Argentina, chit)
bolivia.chit <- corf(Bolivia, chit)
chile.chit <- corf(Chile, chit)
colombia.chit <- corf(Colombia, chit)
costarica.chit <- corf(Costa_Rica, chit)
cuba.chit <- corf(Cuba, chit)
dominican.chit <- corf(Dominican_Republic, chit)
ecuador.chit <- corf(Ecuador, chit)
salvador.chit <- corf(El_Salvador, chit)
guatemala.chit <- corf(Guatemala, chit)
honduras.chit <- corf(Honduras, chit)
mexico.chit <- corf(Mexico, chit)
nicaragua.chit <- corf(Nicaragua, chit)
panama.chit <- corf(Panama, chit)
paraguay.chit <- corf(Paraguay, chit)
peru.chit <- corf(Peru, chit)
puertorico.chit <- corf(Puerto_Rico, chit)
uruguay.chit <- corf(Uruguay, chit)
venezuela.chit <- corf(Venezuela, chit)

# Huston Chronicle
argentina.huc <- corf(Argentina, huc)
bolivia.huc <- corf(Bolivia, huc)
chile.huc <- corf(Chile, huc)
colombia.huc <- corf(Colombia, huc)
costarica.huc <- corf(Costa_Rica, huc)
cuba.huc <- corf(Cuba, huc)
dominican.huc <- corf(Dominican_Republic, huc)
ecuador.huc <- corf(Ecuador, huc)
salvador.huc <- corf(El_Salvador, huc)
guatemala.huc <- corf(Guatemala, huc)
honduras.huc <- corf(Honduras, huc)
mexico.huc <- corf(Mexico, huc)
nicaragua.huc <- corf(Nicaragua, huc)
panama.huc <- corf(Panama, huc)
paraguay.huc <- corf(Paraguay, huc)
peru.huc <- corf(Peru, huc)
puertorico.huc <- corf(Puerto_Rico, huc)
uruguay.huc <- corf(Uruguay, huc)
venezuela.huc <- corf(Venezuela, huc)

# start constructing data frame
LatinAmericanCountries <- c("Argentina", "Bolivia", "Chile", "Colombia", "Costa_Rica", "Cuba",
                            "Dominican_Republic", "Ecuador", "El_Salvador", "Guatemala", "Honduras",
                            "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto_Rico", 
                            "Uruguay", "Venezuela")

Washington_Post <- c(argentina.wp, bolivia.wp, chile.wp, colombia.wp, costarica.wp, cuba.wp,
                     dominican.wp, ecuador.wp, salvador.wp, guatemala.wp, honduras.wp, mexico.wp,
                     nicaragua.wp, panama.wp, paraguay.wp, peru.wp, puertorico.wp, uruguay.wp, venezuela.wp)

New_York_Times <- c(argentina.nyt, bolivia.nyt, chile.nyt, colombia.nyt, costarica.nyt, cuba.nyt, dominican.nyt,
                    ecuador.nyt, salvador.nyt, guatemala.nyt, honduras.nyt, mexico.nyt, nicaragua.nyt,
                    panama.nyt, paraguay.nyt, peru.nyt, puertorico.nyt, uruguay.nyt, venezuela.nyt)

Los_Angeles_Times <- c(argentina.lat, bolivia.lat, chile.lat, colombia.lat, costarica.lat, cuba.lat,
                       dominican.lat, ecuador.lat, salvador.lat, guatemala.lat, honduras.lat,
                       mexico.lat, nicaragua.lat, panama.lat, paraguay.lat, peru.lat, puertorico.lat,
                       uruguay.lat, venezuela.lat)

Miami_Herald <- c(argentina.mihe, bolivia.mihe, chile.mihe, colombia.mihe, costarica.mihe, cuba.mihe,
                  dominican.mihe, ecuador.mihe, salvador.mihe, guatemala.mihe, honduras.mihe, mexico.mihe,
                  nicaragua.mihe, panama.mihe, paraguay.mihe, peru.mihe, puertorico.mihe, uruguay.mihe, venezuela.mihe)

Chicago_Tribune <- c(argentina.chit, bolivia.chit, chile.chit, colombia.chit, costarica.chit, cuba.chit,
                     dominican.chit, ecuador.chit, salvador.chit, guatemala.chit, honduras.chit, mexico.chit,
                     nicaragua.chit, panama.chit, paraguay.chit, peru.chit, puertorico.chit, uruguay.chit,
                     venezuela.chit)

Houston_Chronicle <- c(argentina.huc, bolivia.huc, chile.huc, colombia.huc, costarica.huc, cuba.huc,
                       dominican.huc, ecuador.huc, salvador.huc, guatemala.huc, honduras.huc, mexico.huc,
                       nicaragua.huc, panama.huc, paraguay.huc, peru.huc, puertorico.huc, uruguay.huc,
                       venezuela.huc)

# data frame Countries relative frequency per newspaper as 'corelfren'
corelfren <- data.frame(LatinAmericanCountries, Washington_Post, New_York_Times, Los_Angeles_Times,
                        Miami_Herald, Chicago_Tribune, Houston_Chronicle)
View(corelfren)

# Visuallize results

# Determine the overall limits for x 
x_limits <- range(c(0, 0.07))

# plot relative frequencies for the Washington Post
wp <- ggplot(corelfren, aes(x = Washington_Post, y = LatinAmericanCountries)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01)) +
  theme_minimal()

wp <- wp + coord_cartesian(xlim = x_limits)

# plot relative frequencies for the New York Times
nyt <- ggplot(corelfren, aes(x = New_York_Times, y = LatinAmericanCountries)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01)) +
  theme_minimal()

nyt <- nyt + coord_cartesian(xlim = x_limits)

# plot relative frequencies for the Los Angeles Times
lat <- ggplot(corelfren, aes(x = Los_Angeles_Times, y = LatinAmericanCountries)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01)) +
  theme_minimal()

lat <- lat + coord_cartesian(xlim = x_limits)

# plot relative frequencies for the Miami Herald
mihe <- ggplot(corelfren, aes(x = Miami_Herald, y = LatinAmericanCountries)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01)) +
  theme_minimal()

mihe <- mihe + coord_cartesian(xlim = x_limits)

# plot relative frequencies for Chicago Tribune
chica <- ggplot(corelfren, aes(x = Chicago_Tribune, y = LatinAmericanCountries)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01)) +
  theme_minimal()

chica <- chica + coord_cartesian(xlim = x_limits)

# plot relative frequencies for Houston Chronicle
hucro <- ggplot(corelfren, aes(x = Houston_Chronicle, y = LatinAmericanCountries)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.07, 0.01)) +
  theme_minimal()

hucro <- hucro + coord_cartesian(xlim = x_limits)

# plot results
wp + nyt + lat + mihe + chica + hucro

# save plot as .svg and modify it in Adobe Illustrator for better visualization
ggsave("newspapers_relFreq.svg", width = 4000, height = 3000, units = "px")


################################################################################

### COMPARATIVE TOPIC MODELING

# place .R file in same folder as .txt files
input_dir <- "/Users/martincalvinotorterolo/Desktop/latinX/data"
files_v <- dir(input_dir, "\\.txt$")
files_v

# create empty data frame
documents_df <- NULL
# set chunk size > I will extract/learn the topics from each text segment of 1,000 tokens
chunk_size <- 1000


# loop over each text file
for (i in seq_along(files_v)) {
  text_v <- scan(files_v[i], what = "character", sep = "\n")
  text_v <- paste(text_v, collapse = " ")
  text_words_v <- tolower(text_v)
  text_words_v <- strsplit(text_words_v, "[^A-Za-z0-9_']")
  text_words_v <- unlist(text_words_v)
  text_words_v <- text_words_v[which(text_words_v != "")]
  x <- seq_along(text_words_v)
  chunks_l <- split(text_words_v, ceiling(x / chunk_size))
  if (length(chunks_l[[length(chunks_l)]]) <= chunk_size / 2) {
    chunks_l[[length(chunks_l) - 1]] <- c(chunks_l[[length(chunks_l) - 1]], chunks_l[[length(chunks_l)]])
    chunks_l[[length(chunks_l)]] <- NULL
  }
  chunk_strings_l <- lapply(chunks_l, paste, collapse = " ")
  chunks_df <- do.call(rbind, chunk_strings_l)
  textname_v <- gsub("\\..*", "", files_v[i])
  chunk_ids_v <- 1:nrow(chunks_df)
  chunk_names_v <- paste(textname_v, chunk_ids_v, sep = "_")
  file_df <- data.frame(id = chunk_names_v, text = chunks_df, stringsAsFactors = FALSE)
  documents_df <- rbind(documents_df, file_df)
}

# inspect data frame
head(documents_df)
View(documents_df)
# "id" is the number for each chunk per newspaper text, and "text" contains the 1,000 tokens text segment
colnames(documents_df)

# instantiate a mallet object
mallet.instances <- mallet.import(documents_df$id, documents_df$text,
                                  "/Users/martincalvinotorterolo/Desktop/latinX/data/stopwords.csv",
                                  FALSE, token.regexp = "[\\p{L}']+")

# create a topic model trainer object
# set the number of topics the model will contain to 6
topic_model <- MalletLDA(num.topics = 6)
# load text data for modeling
topic_model$loadDocuments(mallet.instances)

# access the list of the vocabulary after stop words were removed
vocabulary <- topic_model$getVocabulary()
vocabulary[1:200]

# access info about the frequency of words in the corpus
word_freqs <- mallet.word.freqs(topic_model)
head(word_freqs, n = 50)

### train the topic model with 1,000 iterations
topic_model$train(1000)

# unpack the model

# extract words per topic
# create a matrix of 6 rows (one for each topic extracted) and each column a unique word type in the corpus
topic_words_m <- mallet.topic.words(topic_model, smoothed = TRUE, normalized = TRUE)
topic_words_m[1:6, 1:10]
dim(topic_words_m) # a matrix of 6 rows x 21,511 columns

colnames(topic_words_m) <- vocabulary
topic_words_m[1:6, 1:10]

rowSums(topic_words_m) # because normalized = TRUE was used as argument to mallet.topic.words() function
# the values in each topic row were converted to percentages that sum to 1

# compare the relative weight of specific word types as a percentage of each topic
keywords <- c("pandemic", "covid", "virus", "health")
topic_words_m[, keywords]

# now examine the 100 most heavily weighted words in each topic
topic1_top_words <- mallet.top.words(topic_model, topic_words_m[1, ], 200)
topic1_top_words # about the pandemic and health
topic2_top_words <- mallet.top.words(topic_model, topic_words_m[2, ], 200)
topic2_top_words # about political representation
topic3_top_words <- mallet.top.words(topic_model, topic_words_m[3, ], 200)
topic3_top_words # about student life, university activities by latinos
topic4_top_words <- mallet.top.words(topic_model, topic_words_m[4, ], 200)
topic4_top_words # social justice and latinos
topic5_top_words <- mallet.top.words(topic_model, topic_words_m[5, ], 200)
topic5_top_words # theater. music
topic6_top_words <- mallet.top.words(topic_model, topic_words_m[6, ], 200)
topic6_top_words # art, books and films

# topic visualization using word clouds

# Topic1
set.seed(1234)
wordcloud(words = topic1_top_words$term, freq = topic1_top_words$weight, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5, 0))
# Topic 2
set.seed(1234)
wordcloud(words = topic2_top_words$term, freq = topic2_top_words$weight, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.4, 0.1))

# Topic 3
set.seed(1234)
wordcloud(words = topic3_top_words$term, freq = topic3_top_words$weight, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5, 0.1))

# Topic 4
set.seed(1234)
wordcloud(words = topic4_top_words$term, freq = topic4_top_words$weight, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.6, 0.4))

# Topic 5
set.seed(1234)
wordcloud(words = topic5_top_words$term, freq = topic5_top_words$weight, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.7, 0.35))

# Topic 6
set.seed(1234)
wordcloud(words = topic6_top_words$term, freq = topic6_top_words$weight, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, scale=c(3.5, 0.35))


# inspect the probability of each topic appearing on each document (composed of news from each newspaper)
# assess the proportion of a document that is about each topic
doc_topic_m <- mallet.doc.topics(topic_model, smoothed = TRUE, normalized = TRUE)
doc_topic_m # each column is a topic and each row is a text chunk 1,000 tokens long

# look at the document from each newspaper as a whole
# calculate mean topical values

file_ids_v <- documents_df[, 1]
head(file_ids_v)

# modify documents_df as to be able to use group_by() function later on
file_id_l <- strsplit(file_ids_v, "_")
file_chunk_id_l <- lapply(file_id_l, rbind)
file_chunk_id_m <- do.call(rbind, file_chunk_id_l)
head(file_chunk_id_m)

# convert matrix to data frame
doc_topics_df <- as.data.frame(doc_topic_m)

doc_topics_df <- data.frame(file = file_chunk_id_m[,1],
                            doc_topics_df,
                            stringsAsFactors = FALSE)

# calculate the proportion of topics per newspaper document
doc_topic_means_df <- group_by(doc_topics_df, file) %>%
  summarize_all(mean)

View(doc_topic_means_df)

# visualize results

# set the same scale for the x-axis
x_limits <- range(c(0, 0.60))

# topic 1
t1 <- ggplot(data = doc_topic_means_df, mapping = aes(x = V1, y = file)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.60, 0.10)) +
  coord_flip() +
  theme_minimal()

t1 <- t1 + coord_cartesian(xlim = x_limits)
t1

# topic 2
t2 <- ggplot(data = doc_topic_means_df, mapping = aes(x = V2, y = file)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.60, 0.10)) +
  coord_flip() +
  theme_minimal()

t2 <- t2 + coord_cartesian(xlim = x_limits)
t2

# topic 3
t3 <- ggplot(data = doc_topic_means_df, mapping = aes(x = V3, y = file)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.60, 0.10)) +
  coord_flip() +
  theme_minimal()

t3 <- t3 + coord_cartesian(xlim = x_limits)
t3

# topic 4
t4 <- ggplot(data = doc_topic_means_df, mapping = aes(x = V4, y = file)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.60, 0.10)) +
  coord_flip() +
  theme_minimal()

t4 <- t4 + coord_cartesian(xlim = x_limits)
t4

# topic 5
t5 <- ggplot(data = doc_topic_means_df, mapping = aes(x = V5, y = file)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.60, 0.10)) +
  coord_flip() +
  theme_minimal()

t5 <- t5 + coord_cartesian(xlim = x_limits)
t5

# topic 6
t6 <- ggplot(data = doc_topic_means_df, mapping = aes(x = V6, y = file)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 0.60, 0.10)) +
  coord_flip() +
  theme_minimal()

t6 <- t6 + coord_cartesian(xlim = x_limits)
t6

# create composite plot
t1 + t2 + t3 + t4 + t5 + t6

# save plot, export it as .svg and modify it accordingly in Adobe Illustrator
ggsave("topics_per_newspaper.svg", width = 4000, height = 3000, units = "px")


################################################################################

### KEYWORD-IN-CONTEXT

# show path to newspaper documents
input_dir <- "/Users/martincalvinotorterolo/Desktop/latinX/data"
files_v <- dir(input_dir, "\\.txt$", full.names = TRUE)
files_v

# create function to show files' paths
show_files <- function(directory_path, pattern = "\\.txt$") {
  # function to print a vector of file paths and their index numbers in user-friendly format
  file_name_v <- dir(directory_path, pattern, full.names = TRUE)
  for(i in seq_along(file_name_v)) {
    cat(i, file_name_v[i], "\n", sep = " ")
  }
}
# show files's paths
show_files(input_dir)

# create function to tokenize text in newspaper document
make_token_v <- function(file_path, pattern = "\\W") {
  # function returns an ordered vector of tokens from the file referenced in the file_path argument
  text_v <- scan(file_path, what = "character", sep = "\n")
  text_v <- paste(text_v, collapse = " ")
  text_lower_v <- tolower(text_v)
  text_words_v <- strsplit(text_lower_v, pattern)
  text_words_v <- unlist(text_words_v)
  text_words_v <- text_words_v[which(text_words_v != "")]
  return(text_words_v)
}


# NEW YORK TIMES
# tokenize news articles from THe New York Times document
latinx_nyt <- make_token_v("/Users/martincalvinotorterolo/Desktop/latinX/data/NYTimes.txt")
# inspect tokens
latinx_nyt
# save positions corresponding to each mention of latinx
positions_latinx_nyt <- which(latinx_nyt == "latinx")
# inspect positions
positions_latinx_nyt
length(positions_latinx_nyt) # 292 mentions of the latinx term
# inspect the latinx's mention number 432 and its word context as an example
latinx_nyt[432] 
latinx_nyt[427:437]

# extract the word context for each mention of latinx in NYTimes
for (i in 1:length(positions_latinx_nyt)) {
  cat(latinx_nyt[(positions_latinx_nyt[i]-5):(positions_latinx_nyt[i]-1)], "[", latinx_nyt[positions_latinx_nyt[i]], "]",  
      latinx_nyt[(positions_latinx_nyt[i]+1):(positions_latinx_nyt[i]+5)], "\n", sep = " ")
}

# save the console output of the above code into a text file
# open connection to a text file
sink(file = "NYTimes_kwic_latinx.txt")
# execute code to extract keyword in context
for (i in 1:length(positions_latinx_nyt)) {
  cat(latinx_nyt[(positions_latinx_nyt[i]-5):(positions_latinx_nyt[i]-1)], "[", latinx_nyt[positions_latinx_nyt[i]], "]",  
      latinx_nyt[(positions_latinx_nyt[i]+1):(positions_latinx_nyt[i]+5)], "\n", sep = " ")
}
# close connection to text file
sink()


# LOS ANGELES TIMES
# tokenize news articles from Los Angeles Times document
latinx_lat <- make_token_v("/Users/martincalvinotorterolo/Desktop/latinX/data/LATimes.txt")
# inspect tokens
latinx_lat
# save positions corresponding to each mention of latinx
positions_latinx_lat <- which(latinx_lat == "latinx")
# inspect positions
positions_latinx_lat
length(positions_latinx_lat) # 527 mentions of the latinx term
# inspect the latinx's mention number 729 and its word context as an example
latinx_lat[729] 
latinx_lat[724:734]

# extract the word context for each mention of latinx in Los Angeles Times
for (i in 1:length(positions_latinx_lat)) {
  cat(latinx_lat[(positions_latinx_lat[i]-5):(positions_latinx_lat[i]-1)], "[", latinx_lat[positions_latinx_lat[i]], "]",  
      latinx_lat[(positions_latinx_lat[i]+1):(positions_latinx_lat[i]+5)], "\n", sep = " ")
}

# save the console output of the above code into a text file
# open connection to a text file
sink(file = "LATimes_kwic_latinx.txt")
# execute code to extract keyword in context
for (i in 1:length(positions_latinx_lat)) {
  cat(latinx_lat[(positions_latinx_lat[i]-5):(positions_latinx_lat[i]-1)], "[", latinx_lat[positions_latinx_lat[i]], "]",  
      latinx_lat[(positions_latinx_lat[i]+1):(positions_latinx_lat[i]+5)], "\n", sep = " ")
}
# close connection to text file
sink()


# HOUSTON CHRONICLE
# tokenize news articles from Houston Chronicle document
latinx_hc <- make_token_v("/Users/martincalvinotorterolo/Desktop/latinX/data/HoustonChronicle.txt")
# inspect tokens
latinx_hc
# save positions corresponding to each mention of latinx
positions_latinx_hc <- which(latinx_hc == "latinx")
# inspect positions
positions_latinx_hc
length(positions_latinx_hc) # 207 mentions of the latinx term
# inspect the latinx's mention number 9497 and its word context as an example
latinx_hc[9497] 
latinx_hc[9492:9502]

# extract the word context for each mention of latinx in Houston Chronicle
for (i in 1:length(positions_latinx_hc)) {
  cat(latinx_hc[(positions_latinx_hc[i]-5):(positions_latinx_hc[i]-1)], "[", latinx_hc[positions_latinx_hc[i]], "]",  
      latinx_hc[(positions_latinx_hc[i]+1):(positions_latinx_hc[i]+5)], "\n", sep = " ")
}

# save the console output of the above code into a text file
# open connection to a text file
sink(file = "HC_kwic_latinx.txt")
# execute code to extract keyword in context
for (i in 1:length(positions_latinx_hc)) {
  cat(latinx_hc[(positions_latinx_hc[i]-5):(positions_latinx_hc[i]-1)], "[", latinx_hc[positions_latinx_hc[i]], "]",  
      latinx_hc[(positions_latinx_hc[i]+1):(positions_latinx_hc[i]+5)], "\n", sep = " ")
}
# close connection to text file
sink()


# THE WASHINGTON POST
# tokenize news articles from The Washington Post document
latinx_wp <- make_token_v("/Users/martincalvinotorterolo/Desktop/latinX/data/WashingtonPost.txt")
# inspect tokens
latinx_wp
# save positions corresponding to each mention of latinx
positions_latinx_wp <- which(latinx_wp == "latinx")
# inspect positions
positions_latinx_wp
length(positions_latinx_wp) # 186 mentions of the latinx term
# inspect the latinx's mention number 160 and its word context as an example
latinx_wp[160] 
latinx_wp[155:165]

# extract the word context for each mention of latinx in The Washington Post
for (i in 1:length(positions_latinx_wp)) {
  cat(latinx_wp[(positions_latinx_wp[i]-5):(positions_latinx_wp[i]-1)], "[", latinx_wp[positions_latinx_wp[i]], "]",  
      latinx_wp[(positions_latinx_wp[i]+1):(positions_latinx_wp[i]+5)], "\n", sep = " ")
}

# save the console output of the above code into a text file
# open connection to a text file
sink(file = "WP_kwic_latinx.txt")
# execute code to extract keyword in context
for (i in 1:length(positions_latinx_wp)) {
  cat(latinx_wp[(positions_latinx_wp[i]-5):(positions_latinx_wp[i]-1)], "[", latinx_wp[positions_latinx_wp[i]], "]",  
      latinx_wp[(positions_latinx_wp[i]+1):(positions_latinx_wp[i]+5)], "\n", sep = " ")
}
# close connection to text file
sink()


# MIAMI HERALD
# tokenize news articles from Miami Herald document
latinx_mihe <- make_token_v("/Users/martincalvinotorterolo/Desktop/latinX/data/MiamiHerald.txt")
# inspect tokens
latinx_mihe
# save positions corresponding to each mention of latinx
positions_latinx_mihe <- which(latinx_mihe == "latinx")
# inspect positions
positions_latinx_mihe
length(positions_latinx_mihe) # 133 mentions of the latinx term
# inspect the latinx's mention number 381 and its word context as an example
latinx_mihe[381] 
latinx_mihe[376:386]

# extract the word context for each mention of latinx in Miami Herald
for (i in 1:length(positions_latinx_mihe)) {
  cat(latinx_mihe[(positions_latinx_mihe[i]-5):(positions_latinx_mihe[i]-1)], "[", latinx_mihe[positions_latinx_mihe[i]], "]",  
      latinx_mihe[(positions_latinx_mihe[i]+1):(positions_latinx_mihe[i]+5)], "\n", sep = " ")
}

# save the console output of the above code into a text file
# open connection to a text file
sink(file = "MIHE_kwic_latinx.txt")
# execute code to extract keyword in context
for (i in 1:length(positions_latinx_mihe)) {
  cat(latinx_mihe[(positions_latinx_mihe[i]-5):(positions_latinx_mihe[i]-1)], "[", latinx_mihe[positions_latinx_mihe[i]], "]",  
      latinx_mihe[(positions_latinx_mihe[i]+1):(positions_latinx_mihe[i]+5)], "\n", sep = " ")
}
# close connection to text file
sink()


# CHICAGO TRIBUNE
# tokenize news articles from Chicago Tribune document
latinx_chi <- make_token_v("/Users/martincalvinotorterolo/Desktop/latinX/data/ChicagoTribune.txt")
# inspect tokens
latinx_chi
# save positions corresponding to each mention of latinx
positions_latinx_chi <- which(latinx_chi == "latinx")
# inspect positions
positions_latinx_chi
length(positions_latinx_chi) # 126 mentions of the latinx term
# inspect the latinx's mention at position 637 and its word context as an example
latinx_chi[637] 
latinx_chi[632:642]

# extract the word context for each mention of latinx in Miami Herald
for (i in 1:length(positions_latinx_chi)) {
  cat(latinx_chi[(positions_latinx_chi[i]-5):(positions_latinx_chi[i]-1)], "[", latinx_chi[positions_latinx_chi[i]], "]",  
      latinx_chi[(positions_latinx_chi[i]+1):(positions_latinx_chi[i]+5)], "\n", sep = " ")
}

# save the console output of the above code into a text file
# open connection to a text file
sink(file = "CHI_kwic_latinx.txt")
# execute code to extract keyword in context
for (i in 1:length(positions_latinx_chi)) {
  cat(latinx_chi[(positions_latinx_chi[i]-5):(positions_latinx_chi[i]-1)], "[", latinx_chi[positions_latinx_chi[i]], "]",  
      latinx_chi[(positions_latinx_chi[i]+1):(positions_latinx_chi[i]+5)], "\n", sep = " ")
}
# close connection to text file
sink()


################################################################################

### MOST FREQUENT WORDS IN-CONTEXT-WITH LATINX

# tokenize keyword-in-context text files created earlier for each newspaper
# use tokenizer function created earlier
tokenizeText <- function(textFile) {
  textFile <- file.choose()
  textFile.scanned <- scan(textFile, what = "character", sep = "\n")
  textFile.scanned <- paste(textFile.scanned, collapse = " ")
  textFile.scanned <- tolower(textFile.scanned)
  textFile.scanned.l <- strsplit(textFile.scanned, "[^A-Za-z0-9_']")
  textFile.scanned.v <- unlist(textFile.scanned.l)
  notBlanks <- which(textFile.scanned.v != "")
  textFile.scanned.v.withoutBlanks <- textFile.scanned.v[notBlanks]
  return(textFile.scanned.v.withoutBlanks)
}

### Los Angeles Times
lat <- tokenizeText(textFile)
lat
length(lat) # 5,803 tokens

# create table with token counts
lat_t <- table(lat)
View(lat_t)
# create sorted table
sor_lat_t <- sort(lat_t, decreasing = TRUE)
View(sor_lat_t)

# calculate relative frequency for each token 
rf_lat <- 100*(sor_lat_t / length(lat))
View(rf_lat)
# convert relative frequency table to data frame
df_rf_lat <- as.data.frame(rf_lat)

# remove stop words from df_rf_lat
stop_words <- read.csv(file.choose()) # access stopwords.csv file
colnames(stop_words)
View(stop_words)
# create vector of stop words and add new ones
stop_words_vector <- c(stop_words$words, "latinx", "s", "t", "among", "like", "term", "non", "using", "l", "many")

# filter out stop words
df_rf_lat <- filter(df_rf_lat, !(lat %in% stop_words_vector))
View(df_rf_lat)

# plot results for top 25 most frequent words in context associated with latinx
lat_plot <- ggplot(data = df_rf_lat[1:25, ], aes(x = lat, y = Freq)) +
  geom_col() +
  coord_flip() +
  theme_bw()
# inspect plot
lat_plot


### The New York Times
nyt <- tokenizeText(textFile)
nyt
length(nyt) # 3,217 tokens

# create table with token counts
nyt_t <- table(nyt)
View(nyt_t)
# create sorted table
sor_nyt_t <- sort(nyt_t, decreasing = TRUE)
View(sor_nyt_t)

# calculate relative frequency for each token 
rf_nyt <- 100*(sor_nyt_t / length(nyt))
View(rf_nyt)
# convert relative frequency table to data frame
df_rf_nyt <- as.data.frame(rf_nyt)

# create vector of stop words and add new ones
stop_words_vector <- c(stop_words$words, "latinx", "s", "t", "among", "like", "term", "non", "using", "l", "many")

# filter out stop words
df_rf_nyt <- filter(df_rf_nyt, !(nyt %in% stop_words_vector))
View(df_rf_nyt)

# plot results for top 25 most frequent words in context associated with latinx
nyt_plot <- ggplot(data = df_rf_nyt[1:25, ], aes(x = nyt, y = Freq)) +
  geom_col() +
  coord_flip() +
  theme_bw()
# inspect plot
nyt_plot


### Houston Chronicle
huc <- tokenizeText(textFile)
huc
length(huc) # 2,279 tokens

# create table with token counts
huc_t <- table(huc)
View(huc_t)
# create sorted table
sor_huc_t <- sort(huc_t, decreasing = TRUE)
View(sor_huc_t)

# calculate relative frequency for each token 
rf_huc <- 100*(sor_huc_t / length(huc))
View(rf_huc)
# convert relative frequency table to data frame
df_rf_huc <- as.data.frame(rf_huc)

# create vector of stop words and add new ones
stop_words_vector <- c(stop_words$words, "latinx", "s", "t", "among", "like", "term", "non", "using", "l", "many",
                       "percent", "times")

# filter out stop words
df_rf_huc <- filter(df_rf_huc, !(huc %in% stop_words_vector))
View(df_rf_huc)

# plot results for top 25 most frequent words in context associated with latinx
huc_plot <- ggplot(data = df_rf_huc[1:25, ], aes(x = huc, y = Freq)) +
  geom_col() +
  coord_flip() +
  theme_bw()
# inspect plot
huc_plot


### The Washington Post
wap <- tokenizeText(textFile)
wap
length(wap) # 2,046 tokens

# create table with token counts
wap_t <- table(wap)
View(wap_t)
# create sorted table
sor_wap_t <- sort(wap_t, decreasing = TRUE)
View(sor_wap_t)

# calculate relative frequency for each token 
rf_wap <- 100*(sor_wap_t / length(wap))
View(rf_wap)
# convert relative frequency table to data frame
df_rf_wap <- as.data.frame(rf_wap)

# create vector of stop words and add new ones
stop_words_vector <- c(stop_words$words, "latinx", "s", "t", "among", "like", "term", "non", "using", "l", "many",
                       "percent", "times", "used", "heard", "despite", "often")

# filter out stop words
df_rf_wap <- filter(df_rf_wap, !(wap %in% stop_words_vector))
View(df_rf_wap)

# plot results for top 25 most frequent words in context associated with latinx
wap_plot <- ggplot(data = df_rf_wap[1:25, ], aes(x = wap, y = Freq)) +
  geom_col() +
  coord_flip() +
  theme_bw()
# inspect plot
wap_plot


### Miami Herald
mih <- tokenizeText(textFile)
mih
length(mih) # 1,463 tokens

# create table with token counts
mih_t <- table(mih)
View(mih_t)
# create sorted table
sor_mih_t <- sort(mih_t, decreasing = TRUE)
View(sor_mih_t)

# calculate relative frequency for each token 
rf_mih <- 100*(sor_mih_t / length(mih))
View(rf_mih)
# convert relative frequency table to data frame
df_rf_mih <- as.data.frame(rf_mih)

# create vector of stop words and add new ones
stop_words_vector <- c(stop_words$words, "latinx", "s", "t", "among", "like", "term", "non", "using", "l", "many",
                       "percent", "times", "used", "heard", "despite", "often", "called", "4")

# filter out stop words
df_rf_mih <- filter(df_rf_mih, !(mih %in% stop_words_vector))
View(df_rf_mih)

# plot results for top 25 most frequent words in context associated with latinx
mih_plot <- ggplot(data = df_rf_mih[1:25, ], aes(x = mih, y = Freq)) +
  geom_col() +
  coord_flip() +
  theme_bw()
# inspect plot
mih_plot


### Chicago Tribune
chit <- tokenizeText(textFile)
chit
length(chit) # 1,387 tokens

# create table with token counts
chit_t <- table(chit)
View(chit_t)
# create sorted table
sor_chit_t <- sort(chit_t, decreasing = TRUE)
View(sor_chit_t)

# calculate relative frequency for each token 
rf_chit <- 100*(sor_chit_t / length(chit))
View(rf_chit)
# convert relative frequency table to data frame
df_rf_chit <- as.data.frame(rf_chit)

# create vector of stop words and add new ones
stop_words_vector <- c(stop_words$words, "latinx", "s", "t", "among", "like", "term", "non", "using", "l", "many",
                       "percent", "times", "used", "heard", "despite", "often", "called", "4", "re")

# filter out stop words
df_rf_chit <- filter(df_rf_chit, !(chit %in% stop_words_vector))
View(df_rf_chit)

# plot results for top 25 most frequent words in context associated with latinx
chit_plot <- ggplot(data = df_rf_chit[1:25, ], aes(x = chit, y = Freq)) +
  geom_col() +
  coord_flip() +
  theme_bw()
# inspect plot
chit_plot



### create composite plot
lat_plot + nyt_plot + huc_plot + wap_plot + mih_plot + chit_plot

# save plot as .svg and modify it in Adobe Illustrator for better visualization
ggsave("mostFrequent_words_in_context.svg", width = 4000, height = 3000, units = "px")



