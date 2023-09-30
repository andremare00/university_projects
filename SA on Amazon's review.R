## ---------------------------------------
### Objective of the analysis: 
### scraping reviews of an Amazon's product 
### (Apple iPhone 11, 64GB, Black (Renewed))
### and compute the sentiment of those reviews
## ---------------------------------------

library(tidyverse);library(tidytext);library(rvest);library(robotstxt)
library(xml2);library(RSelenium);library(jsonlite);library(ggplot2)
library(igraph);library(ggraph);library(scales);library(wordcloud2)
library(wordcloud);library(udpipe);library(textplot);library(corrplot)
library(quanteda);library(quanteda.textmodels);library(caret); library(topicmodels)
library(cld2);library(SnowballC);library(reshape2);library(robotstxt); library(tm)

## Customized ggtheme
theme_custom = function() {
  theme(panel.border = element_rect(colour = "#80C080", fill = NA,
                                    linetype = 2), panel.background = element_rect("white"),
        panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line("#80C080", linetype = 3),
        panel.grid.minor.y = element_line("#339933", linetype = 3),
        axis.title = element_text(face = "italic", family = "Times New Roman"),
        axis.text = element_text(face = "italic", family = "Times New Roman"))
}

## ----------------------------------------
### 1 -- PRODUCT INFORMATION SCRAPING -----
## ----------------------------------------

### 1.1 -- Product details scraping -- ###
url = "https://www.amazon.co.uk/APPLE-iPhone-Black-pixels-Refurbished-Black/dp/B082P6M3HC/ref=zg-bs_amazon-renewed_sccl_2/262-0609069-6966829?pd_rd_w=TJ3Bu&content-id=amzn1.sym.3f9daf0e-df48-4e4c-9b03-d5dd85519204&pf_rd_p=3f9daf0e-df48-4e4c-9b03-d5dd85519204&pf_rd_r=9H4T6SMXBMJF0XRENVZP&pd_rd_wg=IqjSu&pd_rd_r=d69f20bc-8691-4c70-a483-aeac40eecc14&pd_rd_i=B082P6M3HC&th=1"
html = read_html(url)

prod_det = html %>%
  html_element("[id='detailBullets_feature_div']") %>%
  html_text2() 

prod_det%>%
  cat()

prod_det_split= prod_det %>%
  str_split("\\n") %>%
  unlist()

prod_det_split

save(prod_det_split, file = "prod_det_split.rda")


### 1.2 -- Product ratings number scraping --- ###
prod_nrat= html %>%
  html_element("[id='acrCustomerReviewText']") %>%
  html_text2() 

prod_nrat
save(prod_nrat, file = "prod_nrat.rda")


### 1.3 -- Fastest delivery scraping --- ###

prod_fastdel= html %>%
  html_element("[id='mir-layout-DELIVERY_BLOCK']") %>%
  html_text2() 
prod_fastdel

save(prod_fastdel, file = "prod_fastdel.rda")

prod_fastdel = prod_fastdel %>% str_split("delivery ") %>% as_vector() %>% str_split(". Details") %>% as_vector() 
prod_fastdel[2] 


## -------------------------------------------------------------

## ----------------------------
### 2 -- REVIEWS SCRAPING -----
## ----------------------------

amazon_reviews <- function(id, page) { 
  
  url <- paste0("https://www.amazon.co.uk/product-reviews/", 
                id, "/?pageNumber=", page)
  html <- read_html(url) 
  
  
  title = html %>%
    html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text2()
  
  title = title %>%
    c(html %>%
        html_elements("[class='a-size-base review-title a-color-base review-title-content a-text-bold']") %>%
        html_text2())

  text = html %>%
    html_elements("[class='a-size-base review-text review-text-content']") %>% 
    html_text2() 
  
  star = html %>%
    html_elements("[data-hook='review-star-rating']") %>%
    html_text2()
  
  star = star %>%
    c(html %>%
        html_elements("[data-hook='cmps-review-star-rating']") %>%
        html_text2())

  tibble(title, text, star, page = page) %>%
    return()
}


### Scrape reviwes of the phone
id = "B082P6M3HC"
page = 1:61

data = map_df(page, ~amazon_reviews(id = id , page = .))
data

data$doc_id = 1:nrow(data)

save(data, file = "data.rda")

## -------------------------------------------------------------

## --------------------
### 3 -- ANALYSIS -----
## --------------------

### 3.1 -- Data Cleaning --- ###

## 3.1.1 -- Language detection: we want to find the reviews that are not in English 
library(cld2)

data$title_lang = detect_language(data$title)
data$text_lang = detect_language(data$text)

table(Text = data$text_lang, Title = data$title_lang, useNA = "always")

# Drop the comments apart those from en, de, es, fr, it
transl_data= data%>%
  filter(text_lang=="en"|
           text_lang=="de"|
           text_lang=="es"|
           text_lang=="fr"|
           text_lang=="it")
table(Text = transl_data$text_lang, Title = transl_data$title_lang, useNA = "always")
table(transl_data$text_lang)

# Translation (!!! it needs the authorization key in order to work)
library(deeplr)
trad=translate2(transl_data$text, target_lang = "EN", source_lang = NULL)
transl_data$trad=trad

transl_data= transl_data %>%
  rename(or_text=text) %>%
  rename(text=trad)

table(detect_language(transl_data$or_text))
transl_data$text[is.na(detect_language(transl_data$text))] #ok they are all in english!


## 3.1.2 -- Score (= number of stars)
# Now it is a character, we need to transform it into a number
transl_data = transl_data %>%
  mutate(score = as.numeric(substring(star, 1, 1)))

summary(transl_data$score)

transl_data %>%
  count(score) %>%
  mutate(p = round(n/sum(n), 2))
# the majority of comments have 5 stars


# To apply sentiment analysis we need to pass from a 5 point scale, to a positive/negative scale
# If stars>=4, positive; otherwise, negative

transl_data = transl_data %>%
  mutate(star_sent = ifelse(score >= 4, "positive", "negative"))

transl_data %>%
  count(star_sent) %>%
  mutate(p = n/sum(n))


### 3.2 -- Data Visualization --- ###

## 3.2.1 -- Plot 1
# Compare the length of positive and negative reviews

transl_data$nchar = str_length(transl_data$text)

ggplot(transl_data, aes(x = factor(score), y = nchar, fill = star_sent)) +
  geom_boxplot() + 
  theme_custom() + 
  scale_fill_manual(values = c("#339933", "#99cc99"))+
  labs(title = "Review length by stars", subtitle = "Apple iPhone 11, 64GB, Black (Renewed)",
       x = "Stars", y = "Number of characters") +
  guides(fill=guide_legend(title="Sentiment"))+
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))
  

## 3.2.2 -- Plot 2
#distribution of languages

transl_data %>%
  count(text_lang) %>%
  mutate(text_lang=reorder(text_lang, desc(n))) %>%
  ggplot(aes(text_lang, n))+
  geom_col(fill="#339933")+
  labs(title = "Reviews language frequency", subtitle = "Apple iPhone 11, 64GB, Black (Renewed)",
       x = "Languages", 
       y = "Number of reviews") + 
  theme_custom()+
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))+
  scale_x_discrete(labels=c("en" = "EN", "it" = "IT", "fr"="FR", "de"="DE", "es"="ES"))


### 3.3 -- Sentiment Analysis --- ###
## 3.3.1 -- Tidy approach 
afinn = get_sentiments("afinn") # get the dictionary

# Compute polarity scores
sa_afinn = transl_data %>% 
  unnest_tokens(word, text) %>% # tokenization
  anti_join(stop_words) %>% # remove stop words
  inner_join(afinn, by=c("word")) %>% # join with the dictionary
  group_by(doc_id) %>%
  summarise(polarity_tidy=sum(value))

transl_data= transl_data %>%
  left_join(sa_afinn)


## 3.3.2 -- udpipe approach
afinn2 = get_sentiments("afinn") %>% # get again the dictionary
  rename(term = word, polarity = value) # rename columns for udpipe()

# Tokenization with udpipe()
sa_udpipe = udpipe(transl_data, "english-gum") #udpipe format

# Convert all tokens to lower characters 
sa_udpipe$token = str_to_lower(sa_udpipe$token) 

# Compute the polarity score
scores_token = txt_sentiment(sa_udpipe,
                             term = "token",
                             polarity_terms = afinn2,
                             polarity_negators = qdapDictionaries::negation.words,
                             polarity_amplifiers = qdapDictionaries::amplification.words,
                             polarity_deamplifiers = qdapDictionaries::deamplification.words,
                             amplifier_weight = 0.8,
                             n_before = 3,
                             n_after = 0)

# Create a dataframe with doc_id and polarity scores
df = data.frame(doc_id = scores_token$overall$doc_id,
                polarity_udpipe = scores_token$overall$sentiment_polarity) %>% 
  mutate(doc_id = as.integer(doc_id))

# Attach the column of polarities to the original dataset
transl_data=transl_data%>% 
  left_join(df, by = c("doc_id" = "doc_id"))


### 3.4 -- Visualization --- ### 

# Since in the afinn approach we drop the stopwords and in the udpipe we do not,
# the number of comments that are assigned a polarity score is different in the 
# two approaches. To compare them, we decide to filter out all the comments that 
# have missing values in the afinn polarity score.

## 3.4.2 -- Summary statistics of scores
# Summary statistics of afinn scores
transl_data %>%
  filter(!is.na(polarity_tidy))%>%
  summarise(mean_afinn=mean(polarity_tidy), sd_afinn=sd(polarity_tidy), 
            median_afinn=median(polarity_tidy),
            min_afinn=min(polarity_tidy), max_afinn=max(polarity_tidy))

# Summary statistics of udpipe scores
transl_data %>%
  filter(!is.na(polarity_tidy))%>%
  summarise(mean_udpipe=mean(polarity_udpipe), sd_udpipe=sd(polarity_udpipe), 
            median_udpipe=median(polarity_udpipe),
            min_udpipe=min(polarity_udpipe), max_udpipe=max(polarity_udpipe))


## 3.4.2 -- Histograms
# Afinn scores histogram
h1=transl_data %>%
  filter(!is.na(polarity_tidy))%>%
  ggplot(aes(polarity_tidy))+
  geom_histogram(fill="#339933")+
  labs(title = "Afinn polarity scores distribution", subtitle = "Apple iPhone 11, 64GB, Black (Renewed)",
       x = "Scores", 
       y = "Number of comments") + 
  theme_custom()+
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))

# Udpipe scores histogram
h2=transl_data %>%
  filter(!is.na(polarity_tidy))%>% # because otherwise here the NA are treated like 0
  ggplot(aes(polarity_udpipe))+
  geom_histogram(fill="#339933")+
  labs(title = "Udpipe polarity scores distribution", subtitle = "Apple iPhone 11, 64GB, Black (Renewed)",
       x = "Scores", 
       y = "Number of comments") + 
  theme_custom()+
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))

## 3.4.3 -- Boxplots
# Afinn scores boxplot
b1=transl_data %>%
  filter(!is.na(polarity_tidy))%>%
  ggplot(aes(polarity_tidy))+
  geom_boxplot(fill="#339933")+
  labs(title = " ",subtitle = " ",
       x = "Scores") + 
  theme_custom()+
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))

# Udpipe scores boxplot
b2=transl_data %>%
  filter(!is.na(polarity_tidy))%>% # because otherwise here the NA are treated like 0
  ggplot(aes(polarity_udpipe))+
  geom_boxplot(fill="#339933")+
  labs(title = " ",subtitle = " ", 
       x = "Scores") +
  theme_custom()+
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))

# Group together the 4 plots
# install.packages("cowplot")
library("cowplot")
plot_grid(h1, b1, h2, b2,
          ncol = 2, nrow = 2)

## 3.4.4 -- Confusion matrix afinn-udpipe
comparison=transl_data %>% # I have to create a new object because of the filter
  filter(!is.na(polarity_tidy))%>%
  mutate(afinn.cat=ifelse(polarity_tidy>0, "positive", 
                          ifelse(polarity_tidy==0, "neutral", "negative")),
         udpipe.cat=ifelse(polarity_udpipe>0, "positive", 
                           ifelse(polarity_udpipe==0, "neutral", "negative")))

table(AFINN=comparison$afinn.cat, UDPIPE=comparison$udpipe.cat)
cor(comparison$polarity_tidy, comparison$polarity_udpipe)

## 3.4.5 -- Word contribution to sentiment using udpipe
transl_data %>%
  filter(!is.na(polarity_tidy)) %>%
  unnest_tokens(word, text) %>% # tokenization
  anti_join(stop_words) %>% # remove stop words
  inner_join(afinn, by=c("word")) %>%
  mutate(value.cat=ifelse(value>=0, "positive", "negative")) %>%
  count(word, value, value.cat, sort=T) %>% 
  mutate(weight = n*value) %>%
  arrange(desc(weight)) %>%
  group_by(value.cat) %>%
  slice_max(abs(weight), n=10) %>%
  mutate(word=reorder(word, abs(weight))) %>%
  ggplot(aes(word, weight, fill=value.cat))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(~value.cat, scale="free") +
  theme_custom() + 
  scale_fill_manual(values = c("#339933", "#99cc99"))+
  labs(title = "Word contribution to polarity", subtitle = "Apple iPhone 11, 64GB, Black (Renewed)",
       x = NULL, y = "Weight") +
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))


## 3.4.6 -- Confusion matrix udpipe-start
transl_data= transl_data %>%
  mutate(udpipe.cat=ifelse(polarity_udpipe>0, "positive", 
                           ifelse(polarity_udpipe==0, "neutral", "negative")))

table(UDPIPE=transl_data$udpipe.cat, STARS=transl_data$star_sent)

transl_data %>%
  ggplot(aes(star_sent, fill=udpipe.cat))+
  geom_bar(position="fill") +
  theme_custom() + 
  scale_fill_manual(values = c("#339933", "#99cc99", "#cce6cc"))+
  labs(title = "Stars classification vs Udpipe classification", subtitle = "Apple iPhone 11, 64GB, Black (Renewed)",
       x = "Stars class", y = "Percentage") +
  theme(plot.title = element_text(color = "black", 
                                  size = 12,
                                  face = "bold",
                                  family="Calibri"), 
        plot.subtitle = element_text(color = "black", size=9, face="italic"))+
  guides(fill=guide_legend(title="Udpipe class"))



### 3.5 -- Topic Modelling --- ###
## 3.5.1 -- LDA computation
# Create the dtm object
dtm = transl_data %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(doc_id, word) %>% 
  cast_dtm(doc_id, word, n)

inspect(dtm)

# Divide the sample in train and test
set.seed(1)
train <- sample(rownames(dtm), nrow(dtm) * 0.75)
dtm_train <- dtm[rownames(dtm) %in% train, ]
dtm_test <- dtm[!rownames(dtm) %in% train, ]

# Run LDA for different values of k
# (tried using perplexity score, but seeing the words for every topic we concluded 
# that it does not make any sense)
topic = data.frame(k = seq(2,30,1), perplexity = NA)

for (i in 1:nrow(topic)) {
  print(topic$k[i])
  m = LDA(dtm_train, method = "Gibbs", k = topic$k[i], control = list(alpha = 0.01, seed = 1))
  topic$perplexity[i] = perplexity(m, dtm_test)
}

topic[which.min(topic$perplexity), ] # perplexity minimiZed at k=25

# Plot perplexities
ggplot(topic, aes(x = k, y = perplexity)) + geom_line(col = "#339933") +
  theme_custom() + ggtitle("Perplexity score for different number of topics")

# We can choose k=13 according to the elbow rule
transl_data.lda = LDA(dtm, method = "Gibbs", k=13, control = list(alpha = 0.01, seed = 1))

###
# However, recent studies have shown that predictive likelihood (or equivalently, 
# perplexity) and human judgment are often not correlated, and even sometimes slightly anti-correlated.
# Optimizing for perplexity may not yield human interpretable topics

transl_data.lda = LDA(dtm, method = "Gibbs", k=3, control = list(alpha = 0.01, seed = 1))
terms = terms(transl_data.lda, 50) #1: return// 2: delivery and product opening// 3: product itself

# Let's see which words are different in topic 2/3
(df = data.frame(topic2 = terms[,2],topic3 = terms[,3]) %>% 
  filter(!topic2 %in% topic3)) #we decided based on considering different words to choose k=3

## Computing beta's
(tidy_beta = tidy(transl_data.lda, matrix = "beta"))  #per word per topic assignment

top_terms = tidy_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = F) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) + geom_col(show.legend = F, fill = "#339933") +
  facet_wrap(~topic, scales = "free") + scale_y_reordered() +
  ggtitle("Top 10 representative words for each topic (k=3)") +
  theme_custom()

## Computing gamma's
(tidy_gamma = tidy(transl_data.lda, matrix = "gamma") %>% 
  group_by(document) %>% 
  mutate(assignment = topic[which.max(gamma)]) %>% 
  pivot_wider(names_from = topic, values_from = gamma) %>% 
  relocate(document, `1`, `2`, `3`, assignment))

# Number of documents per topic
table(tidy_gamma$assignment)


