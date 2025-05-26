


#' ---
#' title: "Projekt zaliczeniowy: recenzja paryskiej restauracji"
#' author: "Małgorzata Kowalczyk, Weronika Rudowska, Maja Tęczyńska"
#' date: ""
#' output:
#'    html_document:
#'      df_print: paged
#'      theme: cerulean
#'      highlight: default
#'      toc: yes
#'      toc_depth: 3
#'      toc_float:
#'         collapsed: false
#'         smooth_scroll: true
#'      code_fold: show
#' ---


knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)





#' # Wymagane pakiety
# Wymagane pakiety ----
library(tm)
library(tidytext)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(SnowballC)
library(SentimentAnalysis)
library(ggthemes)
library(tidyverse)

#' # I. ANALIZA SENTYMENTU
# I. ANALIZA SENTYMENTU

#' # 0. Funkcja do przetwarzania tekstu z apostrofami, stemmingiem i stemCompletion
# 0. Funkcja do przetwarzania tekstu z apostrofami, stemmingiem i stemCompletion ----
process_text <- function(file_path) {
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  text <- gsub("[\u2019\u2018\u0060\u00B4]", "'", text)
  text <- removeNumbers(text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  words <- words[!str_detect(words, "'")]
  words <- str_replace_all(words, "[[:punct:]]", "")
  words <- words[words != ""]
  words <- str_trim(words)
  
  tidy_stopwords <- tolower(stop_words$word)
  tidy_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tidy_stopwords)
  tm_stopwords <- tolower(stopwords("en"))
  tm_stopwords <- gsub("[\u2019\u2018\u0060\u00B4]", "'", tm_stopwords)
  
  words <- words[!(words %in% tidy_stopwords)]
  words <- words[!(words %in% tm_stopwords)]
  
  # Stemming + stem completion
  stemmed_doc <- stemDocument(words)
  completed_doc <- stemCompletion(stemmed_doc, dictionary=words, type="prevalent")
  completed_doc <- completed_doc[completed_doc != ""]
  
  return(completed_doc)
}



#' # 0. Funkcja do obliczania częstości występowania słów
# 0. Funkcja do obliczania częstości występowania słów ----
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}



#' # 0. Funkcja do tworzenia chmury słów 
# 0. Funkcja do tworzenia chmury słów ----
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 16,
            colors = brewer.pal(8, color_palette))
  title(title)
}




#' # ANALIZA TEXT MINING 
# ANALIZA TEXT MINING ----



# Wczytanie i przetworzenie tekstu ----

file_path <- "reviews.txt"
words <- process_text(file_path)

# Dodatkowe niestandardowe stopwords
custom_stopwords <- c("$")
words <- words[!words %in% custom_stopwords]

# Częstość słów
freq_df <- word_frequency(words)

# Chmura słów
plot_wordcloud(freq_df, "Chmura słów", "Dark2")

# Wyświetl top 10
print(head(freq_df, 10))




#' # Analiza sentymentu słowniki CSV
# Analiza sentymentu słowniki CSV ----



#' # Wczytaj słowniki z plików csv
# Wczytaj słowniki z plików csv ----
afinn <- read.csv("afinn.csv", stringsAsFactors = FALSE)
bing <- read.csv("bing.csv", stringsAsFactors = FALSE)
loughran <- read.csv("loughran.csv", stringsAsFactors = FALSE)
nrc <- read.csv("nrc.csv", stringsAsFactors = FALSE)


tidy_tokeny <- as_tibble(freq_df)



#' # Analiza sentymentu przy użyciu słownika Loughran
# Analiza sentymentu przy użyciu słownika Loughran ----

# Użycie inner_join()
tidy_tokeny %>%
  inner_join(loughran, relationship = "many-to-many")
# Liczba słów drastycznie się zmniejszyła,
# ponieważ inner_join zachował tylko te słowa,
# które występowały w słowniku


# Zliczanie sentymentu
sentiment_review <- tidy_tokeny %>%
  inner_join(loughran, relationship = "many-to-many")

sentiment_review %>%
  count(sentiment)


# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()

# Filtrowanie analizy sentymentu
# i pozostawienie tylko słów
# o sentymencie pozytywnym lub negatywnym

sentiment_review2 <- sentiment_review %>%
  filter(sentiment %in% c("positive", "negative"))


word_counts <- sentiment_review2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )

# Wizualizacja sentymentu
ggplot(word_counts[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Loughran)") +
  scale_fill_manual(values = c("firebrick", "darkolivegreen4"))



#' # Analiza sentymentu przy użyciu słownika NRC
# Analiza sentymentu przy użyciu słownika NRC ----


# Zliczanie sentymentu
sentiment_review_nrc <- tidy_tokeny %>%
  inner_join(nrc, relationship = "many-to-many")

sentiment_review_nrc %>%
  count(sentiment)

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review_nrc %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()

# Filtrowanie analizy sentymentu
# i pozostawienie tylko słów
# o sentymencie pozytywnym lub negatywnym

sentiment_review_nrc2 <- sentiment_review_nrc %>%
  filter(sentiment %in% c("positive", "negative"))


word_counts_nrc2 <- sentiment_review_nrc2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )

# Wizualizacja sentymentu
ggplot(word_counts_nrc2[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (NRC)")



#' # Analiza sentymentu przy użyciu słownika Bing
# Analiza sentymentu przy użyciu słownika Bing ----


# Zliczanie sentymentu
sentiment_review_bing <- tidy_tokeny %>%
  inner_join(bing)

sentiment_review_bing %>%
  count(sentiment)

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review_bing %>%
  group_by(sentiment) %>%
  arrange(desc(freq)) %>%
  ungroup()

# Filtrowanie analizy sentymentu
# i pozostawienie tylko słów
# o sentymencie pozytywnym lub negatywnym

sentiment_review_bing2 <- sentiment_review_bing %>%
  filter(sentiment %in% c("positive", "negative"))


word_counts_bing2 <- sentiment_review_bing2 %>%
  group_by(sentiment) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )

# Wizualizacja sentymentu
ggplot(word_counts_bing2[1:30,], aes(x=word2, y=freq, fill=sentiment)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (Bing)") +
  scale_fill_manual(values = c("dodgerblue4", "goldenrod1"))



#' # Analiza sentymentu przy użyciu słownika Afinn
# Analiza sentymentu przy użyciu słownika Afinn ----


# Zliczanie sentymentu
sentiment_review_afinn <- tidy_tokeny %>%
  inner_join(afinn)

sentiment_review_afinn %>%
  count(value)

# Zliczanie, które słowa są najczęstsze
# dla danego sentymentu
sentiment_review_afinn %>%
  group_by(value) %>%
  arrange(desc(freq)) %>%
  ungroup()

# Silnie pozytywne lub silnie negatywne słowa:
# filtrowanie analizy sentymentu
# i pozostawienie tylko słów o wartości w zakresie od -5 do 5

sentiment_review_afinn3 <- sentiment_review_afinn %>%
  filter(value %in% c("3", "-3" , "4", "-4", "5", "-5"))


word_counts_afinn3 <- sentiment_review_afinn3 %>%
  group_by(value) %>%
  top_n(20, freq) %>%
  ungroup() %>%
  arrange(desc(freq), word) %>%
  mutate(
    word2 = factor(word, levels = rev(unique(word)))
  )

# Wizualizacja sentymentu
ggplot(word_counts_afinn3[1:30,], aes(x=word2, y=freq, fill=value)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~value, scales="free") +
  coord_flip() +
  labs(x = "Słowa", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Liczba słów wg sentymentu (AFINN)")





#' # Analiza sentymentu w czasie o ustalonej długości linii
# Analiza sentymentu w czasie o ustalonej długości linii ----



# Połączenie wszystkich słów words w jeden ciąg znaków
full_text <- paste(words, collapse = " ")


# Funkcja do dzielenia tekstu na segmenty o określonej długości
split_text_into_chunks <- function(text, chunk_size) {
  start_positions <- seq(1, nchar(text), by = chunk_size)
  chunks <- substring(text, start_positions, start_positions + chunk_size - 1)
  return(chunks)
}


# Podzielenie tekstu na segmenty
#
# ustaw min_lentgh jako jednolitą długość jednego segmentu
set_length <- 50
text_chunks <- split_text_into_chunks(full_text, set_length)


# Wyświetlenie wynikowych segmentów
# print(text_chunks)



#' # Analiza sentymentu przy użyciu pakietu SentimentAnalysis
# Analiza sentymentu przy użyciu pakietu SentimentAnalysis ----
sentiment <- analyzeSentiment(text_chunks)



#' # Słownik GI (General Inquirer)
### Słownik GI (General Inquirer) ----
#
# Słownik ogólnego zastosowania
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z psychologicznym słownikiem harwardzkim Harvard IV-4
# DictionaryGI


# Wczytaj słownik GI
# data(DictionaryGI)
# summary(DictionaryGI)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem GI
sentimentGI <- convertToDirection(sentiment$SentimentGI)


# Wykres skumulowanego sentymentu kierunkowego
#plot(sentimentGI)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_GI <- data.frame(index = seq_along(sentimentGI), value = sentimentGI, Dictionary = "GI")

# Usunięcie wierszy, które zawierają NA
df_GI <- na.omit(df_GI)

ggplot(df_GI, aes(x = value)) +
  geom_bar(fill = "green", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (GI)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




#' # Słownik HE (Henry’s Financial dictionary)
### Słownik HE (Henry’s Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych
# zgodnych z finansowym słownikiem "Henry 2008"
# pierwszy, jaki powstał w wyniku analizy komunikatów prasowych 
# dotyczących zysków w branży telekomunikacyjnej i usług IT
# DictionaryHE


# Wczytaj słownik HE
# data(DictionaryHE)
# summary(DictionaryHE)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem HE
sentimentHE <- convertToDirection(sentiment$SentimentHE)


# Wykres skumulowanego sentymentu kierunkowego
# plot(sentimentHE)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_HE <- data.frame(index = seq_along(sentimentHE), value = sentimentHE, Dictionary = "HE")

# Usunięcie wierszy, które zawierają NA
df_HE <- na.omit(df_HE)

ggplot(df_HE, aes(x = value)) +
  geom_bar(fill = "blue", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (HE)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




#' # Słownik LM (Loughran-McDonald Financial dictionary)
### Słownik LM (Loughran-McDonald Financial dictionary) ----
#
# zawiera listę słów pozytywnych i negatywnych oraz związanych z niepewnością
# zgodnych z finansowym słownikiem Loughran-McDonald
# DictionaryLM


# Wczytaj słownik LM
# data(DictionaryLM)
# summary(DictionaryLM)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem LM
sentimentLM <- convertToDirection(sentiment$SentimentLM)


# Wykres skumulowanego sentymentu kierunkowego
# plot(sentimentLM)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_LM <- data.frame(index = seq_along(sentimentLM), value = sentimentLM, Dictionary = "LM")

# Usunięcie wierszy, które zawierają NA
df_LM <- na.omit(df_LM)

ggplot(df_LM, aes(x = value)) +
  geom_bar(fill = "orange", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (LM)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()




#' # Słownik QDAP (Quantitative Discourse Analysis Package)
### Słownik QDAP (Quantitative Discourse Analysis Package) ----
#
# zawiera listę słów pozytywnych i negatywnych
# do analizy dyskursu


# Wczytaj słownik QDAP
qdap <- loadDictionaryQDAP()
# summary(qdap)


# Konwersja ciągłych wartości sentymentu 
# na odpowiadające im wartości kierunkowe 
# zgodnie ze słownikiem QDAP
sentimentQDAP <- convertToDirection(sentiment$SentimentQDAP)


# Wykres skumulowanego sentymentu kierunkowego
# plot(sentimentQDAP)


# Ten sam wykres w ggplot2:
# Konwersja do ramki danych (ggplot wizualizuje ramki danych)
df_QDAP <- data.frame(index = seq_along(sentimentQDAP), value = sentimentQDAP, Dictionary = "QDAP")

# Usunięcie wierszy, które zawierają NA
df_QDAP <- na.omit(df_QDAP)

ggplot(df_QDAP, aes(x = value)) +
  geom_bar(fill = "red", alpha = 0.7) + 
  labs(title = "Skumulowany sentyment (QDAP)",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw()



#' # Porównanie sentymentu na podstawie różnych słowników
# Porównanie sentymentu na podstawie różnych słowników ----


# Wizualnie lepsze w ggplot2
# Połączenie poszczególnych ramek w jedną ramkę
df_all <- bind_rows(df_GI, df_HE, df_LM, df_QDAP)

# Tworzenie wykresu z podziałem na słowniki
ggplot(df_all, aes(x = value, fill = Dictionary)) +
  geom_bar(alpha = 0.7) + 
  labs(title = "Skumulowany sentyment według słowników",
       x = "Sentyment",
       y = "Liczba") +
  theme_bw() +
  facet_wrap(~Dictionary) +  # Podział na cztery osobne wykresy
  scale_fill_manual(values = c("GI" = "green", 
                               "HE" = "blue", 
                               "LM" = "orange",
                               "QDAP" = "red" ))





#' # Agregowanie sentymentu z różnych słowników w czasie
# Agregowanie sentymentu z różnych słowników w czasie ----


# Sprawdzenie ilości obserwacji
length(sentiment[,1])


# Utworzenie ramki danych
df_all <- data.frame(sentence=1:length(sentiment[,1]),
                     GI=sentiment$SentimentGI, 
                     HE=sentiment$SentimentHE, 
                     LM=sentiment$SentimentLM,
                     QDAP=sentiment$SentimentQDAP)



# USUNIĘCIE BRAKUJĄCYCH WARTOŚCI
# gdyż wartości NA (puste) uniemożliwiają generowanie wykresu w ggplot
#

# Usunięcie wartości NA
# Wybranie tylko niekompletnych przypadków:
puste <- df_all[!complete.cases(df_all), ]


# Usunięcie pustych obserwacji
# np. dla zmiennej QDAP (wszystkie mają NA)
df_all <- df_all[!is.na(df_all$QDAP), ]


# Sprawdzenie, czy wartości NA zostały usunięte
# wtedy puste2 ma 0 wierszy:
puste2 <- df_all[!complete.cases(df_all), ]
puste2




#' # Wykresy przedstawiające ewolucję sentymentu w czasie
# Wykresy przedstawiające ewolucję sentymentu w czasie ----



ggplot(df_all, aes(x=sentence, y=QDAP)) +
  geom_line(color="red", size=1) +
  geom_line(aes(x=sentence, y=GI), color="green", size=1) +
  geom_line(aes(x=sentence, y=HE), color="blue", size=1) +
  geom_line(aes(x=sentence, y=LM), color="orange", size=1) +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")



ggplot(df_all, aes(x=sentence, y=QDAP)) + 
  geom_smooth(color="red") +
  geom_smooth(aes(x=sentence, y=GI), color="green") +
  geom_smooth(aes(x=sentence, y=HE), color="blue") +
  geom_smooth(aes(x=sentence, y=LM), color="orange") +
  labs(x = "Oś czasu zdań", y = "Sentyment") +
  theme_gdocs() + 
  ggtitle("Zmiana sentymentu w czasie")

#' # II. TF-IDF
# II. TF-IDF----

#' # Dane tekstowe
# Dane tekstowe ----


# Utwórz korpus dokumentów tekstowych

# Gdy tekst znajduje się w jednym pliku csv:
data <- read.csv("reviews.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
corpus <- VCorpus(VectorSource(data$Review_Text))


# Korpus
# inspect(corpus)


# Korpus - zawartość przykładowego elementu
corpus[[1]]
corpus[[1]][[1]]
corpus[[1]][2]



#' # 1. Przetwarzanie i oczyszczanie tekstu
# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usunięcie zbędnych znaków ----

# Zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))


# Funkcja do zamiany znaków na spację
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Sprawdzenie
corpus[[1]][[1]]

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]]

# usunięcie ewt. zbędnych nazw własnych
corpus <- tm_map(corpus, removeWords, c("chez", "georges"))
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]]




#' # Tokenizacja
# Tokenizacja ----



#' # A. Macierz częstości TDM ----
# A. Macierz częstości TDM ----

tdm <- TermDocumentMatrix(corpus)
tdm_m <- as.matrix(tdm)



#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz same częstości słów w macierzach
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_df, 10))



#' # B. Macierz częstości TDM z TF-IDF ----
# B. Macierz częstości TDM z TF-IDF ----

tdm_tfidf <- TermDocumentMatrix(corpus,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

tdm_tfidf
# inspect(tdm_tfidf)


tdm_tfidf_m <- as.matrix(tdm_tfidf)


#' # 2. Zliczanie częstości słów
# 2. Zliczanie częstości słów ----
# (Word Frequency Count)


# Zlicz same częstości słów w macierzach
v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)
head(tdm_tfidf_df, 10)



#' # 3. Eksploracyjna analiza danych
# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura słów (globalna)
wordcloud(words = tdm_tfidf_df$word, freq = tdm_tfidf_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyświetl top 10
print(head(tdm_tfidf_df, 10))









