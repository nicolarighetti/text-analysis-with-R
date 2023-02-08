library(quanteda)
library(udpipe)
library(tidyverse)

# load data 
dat <- read.csv("data/data_inaugural.csv")

# check the data
names(dat)
str(dat)

# some text cleaning
dat <- dat %>%
  # text to lower
  mutate(clean_text = tolower(texts)) %>%
  # remove apostrophes
  mutate(clean_text = gsub("'", " ", clean_text))

# lemmatization: load the udpipe model
ud_english <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

# annotate the text
annotated_text <- udpipe_annotate(ud_english, x = dat$clean_text, 
                                  tagger = "default", 
                                  parser = "none")

# create the corpus
dat_corpus <- quanteda::corpus(dat,
                               text_field = "clean_text")

# check docvars names
names(docvars(dat_corpus))

# tokenize
dat_tokens <- dat_corpus %>%
  # remove punctuation
  tokens(remove_punct = TRUE) %>%
  # remove stopwords
  tokens_remove(stopwords("en")) %>%
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
topfeatures(dat_dfm, n = 10)

# top five features in each president sub-corpus
topfeatures(dat_dfm, 
            groups = President, 
            n = 5)

# KWIC
kwic(dat_tokens, pattern = "people", window = 5)

# install quanteda.textplots and create a wordcloud
install.packages("quanteda.textplots")
library(quanteda.textplots)
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
install.packages("quanteda.textstats")
library(quanteda.textstats)
dat_dfm_presindent <- dfm_group(dat_dfm, groups = President)
keyness_trump <- textstat_keyness(dat_dfm_presindent, 
                                  target = "Trump")

# check the words
head(keyness_trump)

# plot keyness
textplot_keyness(keyness_trump)

# use KWIC to check common words used by Trump
# first subset the corpus
trump_corpus <- corpus_subset(inaug_corpus, president == "Trump")
# tokenize
trump_tokens <- tokens(trump_corpus)
# kwic
kwic(trump_tokens, pattern = "trillion", 
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


# Dictionaries ####
LSD2015 <- data_dictionary_LSD2015

inaug_lsd <- dfm_lookup(dat_dfm, 
                        dictionary = LSD2015)

df_lsd <- 
  # output from tokens_lookup
  inaug_lsd %>%
  # to dfm
  dfm() %>% 
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

head(df_lsd)

df_lsd %>%
  filter(name %in% c("neg_prop", "pos_prop")) %>%
  ggplot() +
  geom_col(aes(x = doc_id,
               y = value, group = name, fill = name), 
           # "dodge" to place columns side-by-side
           position = "dodge") +
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
      christmas = c("Christmas", "Santa", "holiday"),
      opposition = c("Opposition", "reject", "notincorpus"),
      taxing = "taxing",
      taxation = "taxation",
      taxregex = "tax*",
      country = "america"
    )
  )

dfm_lookup(dat_dfm, 
           dictionary = my_dictionary)
