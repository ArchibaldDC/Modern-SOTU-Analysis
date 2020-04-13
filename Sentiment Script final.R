library(readr)
library(tidytext)
library(dplyr)
library(tm)
library(tidyr)
library(ggthemes)
library(ggplot2)
library(sentimentr)
library(stringr)
library(cowplot)


#Extract .txt files from working directory and put in a list
TEMP <- list.files(path = getwd(), pattern="*.txt")
newName <- sub("[A-Z][.] ", "", TEMP)

file.rename(TEMP, newName)

as.list(TEMP)

#create a dataframe for overview
df <- data.frame(str_split_fixed(unlist(TEMP)," ", n = Inf), stringsAsFactors = F)

names(df)[1] <- "SOTU"
names(df)[2] <- "Year"
names(df)[3] <- "Surname"
names(df)[4] <- "Name"
names(df)[5] <- "Party"

df <-unite(df, "Name", c("Surname", "Name"), remove = T)
df$Name <- gsub("_", " ", df$Name)
df$Party <- gsub(".txt", "", df$Party)


df$Text = TEMP


#extract text and sentiment from list of txt.files
listTXT <- list()

for (i in 1:nrow(df)) {
  x <- readLines(TEMP[i])
  x1 <- get_sentences(x)
  listTXT[[i]] <- x1
  gsub("\\[.*?\\]", "", listTXT[[i]])
  gsub("\\(.*?\\)", "", listTXT[[i]])
  x2 <- sentiment_by(listTXT[[i]])
  df$SENT[[i]] <- x2
}

#create title vector to put in plot loop
title <- paste("SOTU", df$Name, df$Year, ifelse(df$Party %in% "Democrat", "(D)", "(R)"))


#plot loop
sotu.plots <- vector("list")
for (i in 1:nrow(df)){
    sotu.plots[[i]] <- ggplot(df$SENT[[i]], aes(element_id, ave_sentiment)) + 
      geom_point(color = ifelse(df$Party[[i]] %in% "Democrat", "#0009FF", "#FF0000"), alpha = 0.5) + 
      geom_smooth(method = "auto", color = "black") +
      xlab("Speech Duration (Sentences)") +
      ylab("Sentiment - Polarity Score") + 
      theme_hc() +
      ggtitle(title[[i]])
}

#create plot grid for each president
Truman.grid <- plot_grid(plotlist = sotu.plots[c(1:7, 9)], ncol = 2)
Eisenhower.grid <- plot_grid(plotlist = sotu.plots[c(8, 10:18)], ncol = 2)
Kennedy.grid <- plot_grid(plotlist = sotu.plots[c(19:21)], ncol = 2)
Johnson.grid <- plot_grid(plotlist = sotu.plots[c(22:27)], ncol = 2)
Nixon.grid <- plot_grid(plotlist = sotu.plots[c(28:32)], ncol = 2)
Ford.grid <- plot_grid(plotlist = sotu.plots[c(33:35)], ncol = 2)
Carter.grid <- plot_grid(plotlist = sotu.plots[c(36:39)], ncol = 2)
Reagan.grid <- plot_grid(plotlist = sotu.plots[c(40:47)], ncol = 2)
H.W.Bush.grid <- plot_grid(plotlist = sotu.plots[c(48:51)], ncol = 2)
Clinton.grid <- plot_grid(plotlist = sotu.plots[c(52:59)], ncol = 2)
W.Bush.grid <- plot_grid(plotlist = sotu.plots[c(60:67)], ncol = 2)
Obama.grid <- plot_grid(plotlist = sotu.plots[c(68:75)], ncol = 2)
Trump.grid <- plot_grid(plotlist = sotu.plots[c(76:79)], ncol = 2)

#each plot grid is exported to working directory as a png file
png("Truman.png", width = 900, height = 1000)
plot(Truman.grid)
dev.off()

png("Eisenhower.png", width = 900, height = 1000)
plot(Eisenhower.grid)
dev.off()

png("Kennedy.png", width = 900, height = 1000)
plot(Kennedy.grid)
dev.off()

png("Johnson.png", width = 900, height = 1000)
plot(Johnson.grid)
dev.off()

png("Nixon.png", width = 900, height = 1000)
plot(Nixon.grid)
dev.off()

png("Ford.png", width = 900, height = 1000)
plot(Ford.grid)
dev.off()

png("Carter.png", width = 900, height = 1000)
plot(Carter.grid)
dev.off()

png("Reagan.png", width = 900, height = 1000)
plot(Reagan.grid)
dev.off()

png("H.W. Bush.png", width = 900, height = 1000)
plot(H.W.Bush.grid)
dev.off()

png("Clinton.png", width = 900, height = 1000)
plot(Clinton.grid)
dev.off()

png("W. Bush.png", width = 900, height = 1000)
plot(W.Bush.grid)
dev.off()

png("Obama.png", width = 900, height = 1000)
plot(Obama.grid)
dev.off()

png("Trump.png", width = 900, height = 1000)
plot(Trump.grid)
dev.off()



#If you want every individual plot separately 
Truman <- sotu.plots[c(1:7, 9)]
Eisenhower <- sotu.plots[c(8, 10:18)]
Kennedy <-  sotu.plots[c(19:21)]
Johnson <-  sotu.plots[c(22:27)]
Nixon <-  sotu.plots[c(28:32)]
Ford <-  sotu.plots[c(33:35)]
Carter <-  sotu.plots[c(36:39)]
Reagan <-  sotu.plots[c(40:47)]
H.W.Bush <-  sotu.plots[c(48:51)]
Clinton <-  sotu.plots[c(52:59)]
W.Bush <-  sotu.plots[c(60:67)]
Obama <-  sotu.plots[c(68:75)]
Trump <-  sotu.plots[c(76:79)]
