#install.packages("reshape2")
library(reshape2)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyr")
library(tidyr)


df <- read.csv("dataset.csv")
head(df)


df_general <- df[c("Walmart_General", "Ebay_General", "AliExpress_General", "Amazon_General")]
head(df_general)

molten_general <- reshape2::melt(df_general)

head(molten_general)



model_general <- aov(value ~ variable, data = molten_general)
summary(model_general)



TukeyHSD(model_general, conf.level=.95)



df_item <- df[c("Walmart_Item", "Ebay_Item", "AliExpress_Item", "Amazon_Item")]
head(df_item) 



molten_item <- reshape2::melt(df_item)



model_item <- aov(value ~ variable, data = molten_item)
summary(model_item)



TukeyHSD(model_item, conf.level=.95)



# Get word frequencies of Like_Keywords
like_keywords <- df$Like_Keyword

split_data <- do.call(rbind, strsplit(like_keywords, ","))

word_counts <- table(unlist(split_data))

word_df <- data.frame(word = names(word_counts), count = as.numeric(word_counts))


ggplot(word_df, aes(x = word, y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Word Frequencies")



dislike_keywords <- df$Dislike_Keyword

split_data <- do.call(rbind, strsplit(dislike_keywords, ","))

word_counts <- table(unlist(split_data))

word_df <- data.frame(word = names(word_counts), count = as.numeric(word_counts))

ggplot(word_df, aes(x = word, y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Word Frequencies")




df_general <- df[c("Walmart_General", "Ebay_General", "AliExpress_General", "Amazon_General")]



df_general %>%
  boxplot(figsize = c(20, 10),
           main = "Boxplot of General Search Likeability",
           xlab = "Store",
           ylab = "Price",
           names = c("Walmart", "Ebay", "AliExpress", "Amazon"),
           col = c("red", "blue", "green", "yellow"),
           grid = FALSE,
           fontsize = 15)



df_item <- df[c("Walmart_Item", "Ebay_Item", "AliExpress_Item", "Amazon_Item")]

df_item %>%
  boxplot(figsize = c(20, 10),
           main = "Boxplot of Item Search Likeability",
           xlab = "Store",
           ylab = "Price",
           names = c("Walmart", "Ebay", "AliExpress", "Amazon"),
           col = c("red", "blue", "green", "yellow"),
           grid = FALSE,
           fontsize = 15)


# The reasons why people liked Amazon and what keywords used, as wel as why people disliked AliExpress and what keywords used were conducted in Python, and is in this directory as a PDF and a script in python called analysis.py

