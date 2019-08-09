# Load the necessary libraries for Text Mining
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(readr)
library(lubridate)


# Loading data
vivienda <- read.delim("Descripciones_Vivienda_2017_2019_vTOAD2.txt", sep = "|", stringsAsFactors = F)

glimpse(vivienda)

table(vivienda$CAUSA_SINIESTRO_TX)

# Transform dates from fct to dmy
vivienda$OCURRENCIA_FC <- dmy(vivienda$OCURRENCIA_FC)

# Select random sample to extract topics (too little RAM)
vivi_short <- vivienda %>%
                  sample_n(18500, replace = F)


# Taking descriptions to Corpus
vivi_corpus <- Corpus(VectorSource(vivi_short$SINIESTRO_DESCRIPCION), readerControl=list(readPlain, language="spanish", load=TRUE))



#########################
# Let's clean the data!
#########################
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  #corpus <- tm_map(corpus, PlainTextDocument)
  return(corpus)
}

clean_vivi <- clean_corpus(vivi_corpus)

# Now that the data is cleaned, we can create a Document Term object
vivi_dtm <- DocumentTermMatrix(clean_vivi)
findFreqTerms(vivi_dtm, lowfreq=500)

# Removing words common but not important
clean_vivi <- tm_map(clean_vivi, 
                     removeWords, 
                     c("dos", "informa", "sabe", "ayer", "indica", "mañana", 
                       "clienta", "cliente", "dia", "día", "debido"))


# Moving data to dtm format (needed for LDA usage)
vivi_dtm <- DocumentTermMatrix(clean_vivi)

# Asigning number of claim to the DTM for tracking purposes
vivi_dtm$dimnames$Docs <- vivi_short$SINIESTRO_ORIGEN_NU


# Check if there are documents with no info after cleaning
rowTotals <- apply(vivi_dtm , 1, sum) # Find the sum of words in each Document
sum(rowTotals)
dtm.new   <- vivi_dtm[rowTotals > 0, ] 


##########################
# Finding Topics
library(topicmodels)

# Searching for 4 topics
#lda <- LDA(dtm.new, k=10, control = list(seed = 31416))
lda <- LDA(vivi_dtm, k=10, control = list(seed = 31416))
term <- terms(lda, 10) #first 5 terms of every topic
term #checking


######################
# Words per topic
vivi_topics <- tidy(lda, matrix = "beta")

vivi_top_terms <- vivi_topics %>%
  group_by(topic) %>%
  top_n(12, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

vivi_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#####################
# Topic per Document
vivi_documents <- tidy(lda, matrix = "gamma")

vivi_top_doc <- vivi_documents %>%
                  group_by(document) %>%
                  summarise(gamma = max(gamma))

vivi_top_doc2 <- inner_join(vivi_top_doc, vivi_documents, by=c("document", "gamma"))
vivi_top_doc2$document_v2 <- as.numeric(vivi_top_doc2$document)
                                      
vivi_top_doc2 <- vivi_top_doc2 %>% arrange(document_v2)
                  

vivi_top_doc3 <- cbind(vivi_top_doc2, vivi_short$CAUSA_SINIESTRO_TX)
names(vivi_top_doc3) <- c("doc_v1","gamma","topic","doc_v2","causa")


# Comparing topics vs causes
prop.table(table(vivi_top_doc3$topic, vivi_top_doc3$causa),1)
prop.table(table(vivi_top_doc3$topic, vivi_top_doc3$causa),2)

# Counting topics by document and groupin by cause of claim
topic_doc <- vivi_documents %>% 
              filter(gamma > 0.15) %>%
              group_by(document) %>%
              summarise(qty_topics = n())

topic_doc$document_v2 <- as.numeric(topic_doc$document)

topic_doc <- topic_doc %>% arrange(document_v2)
names(topic_doc) <- c("doc_v1","qty_topics","doc_v2")

vivi_top_doc4 <- inner_join(vivi_top_doc3, topic_doc, by="doc_v2")

                            
vivi_top_doc4 %>%
  ggplot(aes(x=qty_topics, fill=causa)) + 
  geom_histogram(bins=10) +
  facet_wrap(~ causa, scales = "free")


vivi_top_doc4 %>%
  group_by(causa) %>%
  summarise(mean = mean(qty_topics),
            median = median(qty_topics))

###########################################################
# Plotting assigment by topic vs declared cause of claim

# By causa
vivi_top_doc3 %>%
  count(causa, topic) %>%
  group_by(causa) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(as.factor(topic), causa, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignación por Tópicos",
       y = "Asignación por Canal",
       fill = "% asignaciones")


# By topic
vivi_top_doc3 %>%
  count(causa, topic) %>%
  group_by(topic) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(as.factor(topic), causa, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignación por Tópicos",
       y = "Asignación por Canal",
       fill = "% asignaciones")






##################################
# Only Fast Track analysis

# Loading the data
fs <- read.delim("Descripciones_Vivienda_2017_2019_vTOAD3_FS.txt", sep = "|", stringsAsFactors = F)
# Just the 3 principal categories
fs_short <- fs %>% filter(CAUSA_SINIESTRO_TX %in% c("DAÑOS / ROTURAS","EVENTOS CLIMATICOS","PERDIDA DE FRIO"))
# Creating Corpus
fs_corpus <- Corpus(VectorSource(fs_short$SINIESTRO_DESCRIPCION), readerControl=list(readPlain, language="spanish", load=TRUE))
# Cleaning data
clean_fs <- clean_corpus(fs_corpus)
# Taking not needed words
clean_fs <- tm_map(clean_fs, 
                     removeWords, 
                     c("dos", "informa", "sabe", "ayer", "indica", "mañana", 
                       "clienta", "cliente", "dia", "día", "debido", "mismo", "mas"))
# Creating dtm
#fs_dtm <- DocumentTermMatrix(clean_fs, control=list(weighting=weightTfIdf))
fs_dtm <- DocumentTermMatrix(clean_fs)

# Running LDA
fs_lda <- LDA(fs_dtm, k=6, control = list(seed = 27172))


######################
# Words per topic
fs_topics <- tidy(fs_lda, matrix = "beta")

fs_top_terms <- fs_topics %>%
  group_by(topic) %>%
  top_n(12, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

fs_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#####################
# Topic per Document
fs_documents <- tidy(fs_lda, matrix = "gamma")

fs_top_doc <- fs_documents %>%
  group_by(document) %>%
  summarise(gamma = max(gamma))

fs_top_doc2 <- inner_join(fs_top_doc, fs_documents, by=c("document", "gamma"))
fs_top_doc2$document_v2 <- as.numeric(fs_top_doc2$document)

fs_top_doc2 <- fs_top_doc2 %>% arrange(document_v2)


fs_top_doc3 <- cbind(fs_top_doc2, fs_short$CAUSA_SINIESTRO_TX)
names(fs_top_doc3) <- c("doc_v1","gamma","topic","doc_v2","causa")

###########################################################
# Plotting assigment by topic vs declared cause of claim

# By causa
fs_top_doc3 %>%
  count(causa, topic) %>%
  group_by(causa) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(as.factor(topic), causa, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignación por Tópicos",
       y = "Asignación por Canal",
       fill = "% asignaciones")


# By topic
fs_top_doc3 %>%
  count(causa, topic) %>%
  group_by(topic) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(as.factor(topic), causa, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Asignación por Tópicos",
       y = "Asignación por Canal",
       fill = "% asignaciones")





################################################
# Analizo largo de descripciones por circuito

largos <- read.delim("LENGTH_DESCRIPCIONES.txt", sep="|")
largo2 <- read.delim("Length_Desc_Ind.txt", sep="|")
dejo_fun <- read.delim("Dejo_funcionar.txt", sep="|")

glimpse(largo2)

largo2 %>%
    filter(CIRCUITO_LIQUIDACION_TX %in% c("Administrativo","Fast Track", "Liquidador"), LARGO_DESCRIPCION < 700) %>%
    ggplot(aes(x=LARGO_DESCRIPCION, color=CIRCUITO_LIQUIDACION_TX)) +
    geom_density()

dejo_fun %>%
  filter(CIRCUITO_LIQUIDACION_TX %in% c("Administrativo","Fast Track", "Liquidador"), LARGO_DESCRIPCION < 700) %>%
  ggplot(aes(x=LARGO_DESCRIPCION, color=CIRCUITO_LIQUIDACION_TX)) +
  geom_density()
