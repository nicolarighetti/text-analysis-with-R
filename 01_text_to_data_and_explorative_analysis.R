library(quanteda)
library(udpipe)
library(tidyverse)
library(quanteda.textstats)
library(quanteda.textplots)

# load data 
dat <- read.csv("data/data_inaugural.csv")

# check the data
names(dat)
str(dat)

# text to lower
dat$clean_text <- tolower(dat$texts)

 # remove apostrophes
dat$clean_text <- gsub("'", " ", dat$clean_text)
dat$clean_text <- gsub("‘", " ", dat$clean_text)


# lemmatization: load the udpipe model
ud_english <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# annotate the text
annotated_text <- udpipe_annotate(ud_english, x = dat$clean_text, 
                                  tagger = "default", 
                                  parser = "none")

annotated_text <- as.data.frame(annotated_text)

# create the corpus
dat_corpus <- quanteda::corpus(dat,
                               text_field = "clean_text")

# check docvars names
names(docvars(dat_corpus))
summary(dat_corpus)

# tokenize
dat_tokens <- dat_corpus %>%
  # remove punctuation
  tokens(remove_punct = TRUE) %>%
  # remove stopwords
  tokens_remove(stopwords("en")) %>%
  # select words with 2 or more characters
  tokens_select(min_nchar = 2) %>%
  # lemmatization
  tokens_replace(pattern = annotated_text$token, 
                 replacement = annotated_text$lemma,
                 valuetype = "fixed")

# create the dfm 
dat_dfm <- dat_tokens %>%
  dfm() %>%
  dfm_trim(min_docfreq = 0.5, max_docfreq = 0.99, 
           docfreq_type = "prop") %>%
  # this is to remove "empty" documents
  dfm_subset(ntoken(.) > 0)

# top features
topfeatures(dat_dfm, n = 20)

# top five features in each president sub-corpus
topfeatures(dat_dfm, 
            groups = President, 
            n = 10)

# KWIC
kwic(dat_tokens, pattern = "nation", window = 5)
kwic(tokens(dat_corpus), pattern = "nation", window = 3)

# install quanteda.textplots and create a wordcloud
quanteda.textplots::textplot_wordcloud(dat_dfm)

# create a comparison world cloud
dem_rep <- dfm_subset(
  dat_dfm, Party %in% c("Democratic", "Republican")) %>%
  dfm_group(groups = Party)

# worcloud by party
textplot_wordcloud(dem_rep, 
                   comparison = TRUE, 
                   max_words = 100,
                   color = c("blue", "red"))


# install and load quanteda.textstats and identify keywords of Trump's speeches 
dat_dfm_presindent <- dfm_group(dat_dfm, groups = President)
keyness_trump <- textstat_keyness(dat_dfm_presindent, 
                                  target = "Trump")

# check the words
head(keyness_trump, n = 20)

# plot keyness
textplot_keyness(keyness_trump)

# compare keyness Trump vs Obama
trump_obama <- dfm_subset(dat_dfm_presindent, 
                          subset = President %in% c("Trump", "Obama"))

keyness_trump_obama <- textstat_keyness(trump_obama, 
                                        target = "Obama")

textplot_keyness(keyness_trump_obama)


# use KWIC to check common words used by Trump
# first subset the corpus
trump_corpus <- corpus_subset(dat_corpus, President == "Trump")
# tokenize
trump_tokens <- tokens(trump_corpus)
# kwic
kwic(trump_tokens, pattern = "america", 
     window = 5) 

# collocations 
textstat_collocations(dat_tokens, 
                      size = 2, min_count = 3) 

inaug_tokens_comp <- tokens_compound(dat_tokens, 
                                     pattern = list(c("american", "people"),
                                                    c("years", "ago")))

kwic(inaug_tokens_comp, 
     pattern = c("years_*", "american_*"), 
     window = 4) 

###################
# Dictionaries ####
###################
LSD2015 <- data_dictionary_LSD2015

inaug_lsd <- dfm_lookup(dat_dfm, 
                        dictionary = LSD2015)

head(inaug_lsd)

df_lsd <- inaug_lsd %>%
  # group data by variable
  dfm_group(groups = President) %>%
  # convert to data frame to perform data manipulation easily
  convert(to = "data.frame")

# create a summary of the party-grouped corpus to get type and tokens statistics
corpus_group_presindent <- corpus_group(dat_corpus, groups = President)
inaug_corpus_presindent_summ <- summary(corpus_group_presindent)

df_lsd <- df_lsd %>%
  # merge the sentiment df and the types metrics by year
  left_join(inaug_corpus_presindent_summ[, c("Tokens", "Text")], 
            by = c("doc_id" = "Text")) %>%
  # proportions out of total types
  mutate(neg_prop = negative/Tokens,
         pos_prop = positive/Tokens) %>%
  # difference in polarity and total emotions
  mutate(diff = pos_prop - neg_prop,
         emot = pos_prop + neg_prop)

head(df_lsd)

df_lsd <- df_lsd %>%
  pivot_longer(-doc_id)

head(df_lsd, n=20)

df_lsd %>%
  filter(name %in% c("neg_prop", "pos_prop")) %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(doc_id, -value),
               y = value, group = name, fill = name), 
           # "dodge" to place columns side-by-side
           position = "stack") +
  scale_fill_manual(values = c(neg_prop = "skyblue",
                               pos_prop = "tomato"),
                    labels = c("Negative", "Positive"),
                    name = "Emotions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("") +
  ylab("Proportions") +
  ggtitle("Simple sentiment analysis with Quanteda and LSD2015",
          subtitle = "Proportion of emotional words out of total tokens")

# To create an user-defined dictionary

my_dictionary <-
  dictionary(
    list(
      america = c("america*", "usa"),
      great = c("great", "reject", "notincorpus"),
      taxing = "taxing",
      taxation = "taxation",
      taxregex = "tax*",
      country = "america"
    )
  )

dfm_lookup(dat_dfm, 
           dictionary = my_dictionary)


#####################
# topic modeling ####
#####################

install.packages("topicmodels") 
install.packages("topicdoc") # library for topic models validation
install.packages("ldatuning") # find the number of topics
install.packages("tidytext") # library to reshape data frames in tidy format

library(topicmodels)
library(topicdoc)
library(ldatuning)
library(tidytext)

# convert the dfm to a topicmodels object
dat_dtm <- convert(dat_dfm, to = "topicmodels")

# fit the topic model
topicModel <- LDA(dat_dtm, 
                  k = 5, 
                  method = "Gibbs")

# get top 10 terms by topic
terms(topicModel, 10)

# create a tidy data frame
tidy_model <- tidy(topicModel)
head(tidy_model, n=20)

# The beta matrix includes the information about 
# the distribution of terms by topics.
top_terms <- tidy_model %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# plot top terms
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# the gamma matrix includes information about the distribution 
# of topics in each document
tidy_model_gamma <- tidy(topicModel, matrix = "gamma")
tidy_model_gamma

# plot document distribution by 
ggplot(tidy_model_gamma) +
  geom_col(aes(x = topic, y = gamma)) +
  facet_wrap(~ document, nrow = 3)

# assign top topics to documents
docvars(dat_corpus, "pred_topic") <- topics(topicModel)
names(docvars(dat_corpus))
# convert to data frame
dat_topic_df <- convert(dat_corpus, to = "data.frame") 
# add the whole gamma matrix to the data frame
dat_topic_df <- cbind(dat_topic_df, topicModel@gamma)


#######################
# how many topics? ####
######################
topic_number <- FindTopicsNumber(dat_dtm, 
                       topics = seq(5, 100, by = 5),
                       metrics = c("Griffiths2004", 
                                   "CaoJuan2009", 
                                   "Arun2010", 
                                   "Deveaud2014"))

FindTopicsNumber_plot(topic_number)

ggplot(topic_number,
       aes(x = topics, y = Griffiths2004)) +
  geom_point() +
  geom_line() + 
  ggtitle("Griffiths2004")


# Based on this indication, we can perhaps fit a model with about 15 topics
topicModel30 <- LDA(dat_dtm, 
                    k = 30, 
                    method="Gibbs")

topicmodels::terms(topicModel30, 5)

tidy_model_gamma_30 <- tidy(topicModel30, matrix = "gamma")

ggplot(tidy_model_gamma_30) +
  geom_col(aes(x = topic, y = gamma)) +
  facet_wrap(~ document, nrow = 3)

# The package topicdoc provides diagnostic measures for topic models. 
# A particularly useful and commonly-used metrics are semantic coherence and exclusivity. 
# A good topic model should have coherent topics (i.e., about a single theme and not a mixture of 
# different themes), which also are well distinguishable from each other, without overlaps (exclusivity).

topicModel_diag5 <- topic_diagnostics(topicModel, dat_dtm)
topicModel_diag30 <- topic_diagnostics(topicModel30, dat_dtm)

topicModel_diag5
topicModel_diag30

topicModel_diag30 %>%
  mutate(topic = as_factor(topic_num)) %>%
  ggplot() +
  geom_point(aes(x = topic_coherence, y = topic_exclusivity, color = topic),
             size = 3) +
  ylab(label = "Semantic Coherence") +
  xlab("Exclusivity") +
  ggtitle("A topic model with 30 topics")

topicModel_diag5 %>%
  mutate(topic = as_factor(topic_num)) %>%
  ggplot() +
  geom_point(aes(x = topic_coherence, y = topic_exclusivity, color = topic),
             size = 3) +
  ylab(label = "Semantic Coherence") +
  xlab("Exclusivity") +
  ggtitle("A topic model with 5 topics")
