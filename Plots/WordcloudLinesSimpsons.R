library(tidyverse)
library(wordcloud)
library(tm)

dialogues <- read_csv("simpsons_script_lines.csv")







##Homer
sl <- split(dialogues, dialogues$raw_character_text)
Homer <- sl$`Homer Simpson`

words <- Homer$normalized_text %>%
  VectorSource%>%
  Corpus%>% 
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)%>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords ("english")) %>%
  TermDocumentMatrix%>%
  as.matrix %>%
  rowSums%>%
  sort(decreasing = TRUE)

df <- data.frame(word = names(words), freq=words)


write_csv(df, "words_hommer_df.csv")


##Marge

sl <- split(dialogues, dialogues$raw_character_text)
Marge <- sl$`Marge Simpson`

words <- Marge$normalized_text %>%
  VectorSource%>%
  Corpus%>% 
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)%>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords ("english")) %>%
  TermDocumentMatrix%>%
  as.matrix %>%
  rowSums%>%
  sort(decreasing = TRUE)

dfm <- data.frame(word = names(words), freq=words)

write_csv(dfm, "words_marge_dfm.csv")

##Bart
sl <- split(dialogues, dialogues$raw_character_text)
Bart <- sl$`Bart Simpson`

words <- Bart$normalized_text %>%
  VectorSource%>%
  Corpus%>% 
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)%>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords ("english")) %>%
  TermDocumentMatrix%>%
  as.matrix %>%
  rowSums%>%
  sort(decreasing = TRUE)

dfb <- data.frame(word = names(words), freq=words)

write_csv(dfb, "words_bart_dfb.csv")

## Lisa
sl <- split(dialogues, dialogues$raw_character_text)
Lisa <- sl$`Lisa Simpson`

words <- Lisa$normalized_text %>%
  VectorSource%>%
  Corpus%>% 
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(stripWhitespace)%>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords ("english")) %>%
  TermDocumentMatrix%>%
  as.matrix %>%
  rowSums%>%
  sort(decreasing = TRUE)

dfl <- data.frame(word = names(words), freq=words)

write_csv(dfl, "words_lisa_df.csv")
