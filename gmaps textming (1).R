library(readr)

data = read_csv("google maps.csv")
data_teks = data$text
head(data_teks, n=10)
summary(data_teks) #2606 data
sum(duplicated(data_teks))


#menghapus data kosong
data_teks = na.omit(data_teks)
sum(is.na(data_teks))
summary(data_teks) #1014 data

library(tm)
data_corpus <- Corpus(VectorSource(data_teks))

#mengubah menjadi huruf kecil
data_corpus <- tm_map(data_corpus, content_transformer(tolower))
inspect(data_corpus[9]) #cek data

#menghilangkan tanda baca 
data_corpus <- tm_map(data_corpus, content_transformer(function(x) gsub("[[:punct:]]", " ", x)))
inspect(data_corpus[23:25]) #cek data

#menghilangkan \n 
data_corpus <- tm_map(data_corpus, content_transformer(function(x) gsub("\n", " ", x)))
inspect(data_corpus[22])

#menghapus angka
remove_numbers <- content_transformer(function(x) {
  gsub("[0-9]+", "", x)
})
data_corpus <- tm_map(data_corpus, remove_numbers)
inspect(data_corpus[24])

# menghilangkan stopwords
stopwords_file <- "id.stopwords.02.01.2016.txt"  # Path ke file stopwords Anda
custom_stopwords <- readLines(stopwords_file)
sw <- c("bandara", "for", "the", "and", "nya", "airport", "balikpapan",
        "sepinggan", "aji", "bandaranya")
all_stopwords <- unique(c(sw, custom_stopwords))
data_corpus <- tm_map(data_corpus, removeWords, all_stopwords)
inspect(data_corpus[1:5]) #cek data

#menghapus spasi berlebih
remove_double_space <- content_transformer(function(x) {
  gsub("\\s+", " ", x) # Ganti satu atau lebih spasi dengan satu spasi
})
data_corpus <- tm_map(data_corpus, remove_double_space)
inspect(data_corpus[121:150])

#ganti kata international ke internasional
data_corpus <- tm_map(data_corpus, content_transformer(function(x) 
  gsub("international", "internasional", x))
  )

#mengubah berbagai kata panas
replacement_words <- c("panas", "panaaaassssss", "panass", "puanaaasssss", "panaassss", 
                       "pnas", "panaaaasss", "puanass", "kepanasan", "keringetan", 
                       "hot", "sauna", "gerahh", "panasnya")
data_corpus <- tm_map(data_corpus, content_transformer(function(text) {
  gsub(paste(replacement_words, collapse = "|"), "panas", text)
}))

#tokenizing
tokenize_corpus <- content_transformer(function(text) {
  unlist(strsplit(text, "\\s+"))  # Tokenisasi berdasarkan spasi
})
data_corpus <- tm_map(data_corpus, tokenize_corpus)
inspect(data_corpus [2:10])

tdm <- TermDocumentMatrix(data_corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
summary(d)
top_words = head(d, 10)

#wordcloud
library(wordcloud2)
library(RColorBrewer)
wordcloud2(data = d, size = 0.5, color= RColorBrewer::brewer.pal(10, "Dark2"), 
           shape = "cloud", backgroundColor = "white")


# Misalnya, data yang ingin disimpan adalah data frame 'd'
write.csv(d, file = "data_output.csv", row.names = FALSE)


#sentiment analysis
library(dplyr)
library(tidytext)
library(lexicon)
library(ggplot2)
library(reshape2)
library(wordcloud)

positive_file <- "positive_words_output.txt"
positive_words <- readLines(positive_file)
negative_file <- "negative_words_output.txt"
negative_words <- readLines(negative_file)
neutral_file <- "neutral_words_output.txt"
neutral_words <- readLines(neutral_file)

# Gabungkan semua dalam satu dataframe
manual_lexicon <- data.frame(
  word = c(positive_words, negative_words, neutral_words),
  sentiment = c(
    rep("positive", length(positive_words)),
    rep("negative", length(negative_words)),
    rep("neutral", length(neutral_words))
  )
)

library(tm)  # Jika data_corpus berasal dari tm

# Konversi SimpleCorpus menjadi data frame
data_corpus_df <- data.frame(text = sapply(data_corpus, as.character), stringsAsFactors = FALSE)

sentiment_data <- data_corpus_df %>%
  inner_join(manual_lexicon, by = c("text" = "word"))

sentiment_summary <- sentiment_data %>%
  count(sentiment)

ggplot(sentiment_summary, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("positive" = "blue", "negative" = "red", "neutral" = "gray")) +
  labs(title = "Analisis Sentimen Bandara Sepinggan", x = "Sentimen", y = "Jumlah Kata") +
  theme_minimal(base_size = 14) +
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))

# Hitung jumlah total kata yang sudah diberi sentimen
total_sentiments <- sum(sentiment_summary$n)

# Hitung proporsi untuk setiap sentimen
sentiment_summary <- sentiment_summary %>%
  mutate(proportion = n / total_sentiments * 100)

# Tampilkan proporsi sentimen
sentiment_summary


# Hitung frekuensi kata berdasarkan sentimen
top_words <- sentiment_data %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment, text) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(sentiment, desc(freq))

# Ambil 10 kata teratas untuk setiap jenis sentimen
top_words_each_sentiment <- top_words %>%
  group_by(sentiment) %>%
  slice_max(order_by = freq, n = 10, with_ties = FALSE) # Hindari lebih dari 10 kata karena ties

# Visualisasi Top Kata (Bar Chart)
ggplot(top_words_each_sentiment, aes(x = reorder(text, freq), y = freq, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Kata Berdasarkan Sentimen", x = "Kata", y = "Frekuensi") +
  scale_fill_manual(values = c("positive" = "#8BC34A", "negative" = "#F06292")) +
  theme_minimal(base_size = 14)



# membuat wordcloud antara sentimen positif dan negatif
library(dplyr)
library(tidyr)
library(reshape2)
library(wordcloud)
library(tidytext)

# Filter hanya sentimen positif dan negatif dari manual lexicon
manual_lexicon_filtered <- manual_lexicon %>%
  filter(sentiment %in% c("positive", "negative"))

# Gabungkan data teks dengan lexicon manual yang difilter
tidy_sentiment_data <- data_corpus_df %>%
  unnest_tokens(word, text) %>%  # Tokenisasi teks menjadi kata-kata
  inner_join(manual_lexicon_filtered, by = "word") %>%  # Gabungkan dengan lexicon manual
  count(word, sentiment, sort = TRUE)  # Hitung frekuensi kata berdasarkan sentimen

# Ubah data ke dalam format matriks untuk comparison.cloud
sentiment_matrix <- tidy_sentiment_data %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)  # Konversi ke matriks

# Wordcloud dengan perbandingan sentimen
comparison.cloud(sentiment_matrix,
                 colors = c("#d14439", "#17577a"),  # Warna untuk negatif dan positif
                 max.words = 100,
                 scale = c(, 2),  # Skala ukuran kata
                 random.order = FALSE,  # Atur posisi agar lebih terstruktur
                 family = "sans",  # Pilih font yang lebih bold (gunakan font bold yang tersedia di sistem)
                 font = 2)  # Parameter tambahan untuk mempertebal teks

# Visualisasi dengan ggplot2
library(ggplot2)
library(forcats)

tidy_sentiment_data %>%
  mutate(word = forcats::fct_reorder(word, n)) %>%  # Urutkan kata berdasarkan frekuensi
  ggplot(aes(x = word, y = n, fill = sentiment)) +
  geom_col(position = "dodge", color = "black", size = 0.8) +  # Bar chart dengan outline hitam
  coord_flip() +  # Balik sumbu untuk menampilkan kata secara horizontal
  scale_fill_manual(values = c("positive" = "#17577a", "negative" = "#d14439")) +  # Warna sentimen
  labs(
    title = "Frekuensi Kata Berdasarkan Sentimen",
    x = "Kata",
    y = "Frekuensi",
    fill = "Sentimen"
  ) +
  theme_minimal(base_size = 14) +  # Tema minimalis dengan font lebih besar
  theme(
    text = element_text(face = "bold"),  # Bold pada seluruh teks
    axis.text.x = element_text(face = "bold"),  # Bold pada label sumbu x
    axis.text.y = element_text(face = "bold"),  # Bold pada label sumbu y
    plot.title = element_text(hjust = 0.5, size = 18)  # Bold dan tengah untuk judul
  )
