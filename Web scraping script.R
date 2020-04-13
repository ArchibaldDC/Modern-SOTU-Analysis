library(rvest)
library(dplyr)
library(tidyr)
library(qdap)
library(dplyr)



#load webpage
pres.library <- read_html(x = "https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/annual-messages-congress-the-state-the-union")

#get URL of links
links <- pres.library %>%
  html_nodes("span a , td~ td+ td a") %>%
  html_attr("href")

#get link textb
text <- pres.library %>%
  html_nodes("span a , td~ td+ td a") %>%
  html_text()

#combine into df 
sotu <- data.frame (text = text, links = links, stringsAsFactors = F)

#change text column to year
names(sotu)[1] <- "Year"

#remove redunant rows & row of 1973 sotu(nixon wrote 6 separate speeches with a different topic, will be added to 1973)
sotu <- sotu[-c(89, 87, 77, 52 ),]

#replace symbols
sotu$Year <- sub("???", "", sotu$Year)

#replace Nixon 1973 speeches
sotu$Year <- sub("State of the Union Message to the Congress: Overview and Goals", "1973", sotu$Year)
sotu$Year <- sub("State of the Union Message to the Congress on Natural Resources and the Environment", "1973", sotu$Year)
sotu$Year <- sub("State of the Union Message to the Congress on the Economy", "1973", sotu$Year)
sotu$Year <- sub("State of the Union Message to the Congress on Human Resources", "1973", sotu$Year)
sotu$Year <- sub("State of the Union Message to the Congress on Community Development", "1973", sotu$Year)
sotu$Year <- sub("State of the Union Message to the Congress on Law Enforcement and Drug Abuse Prevention", "1973", sotu$Year)

#order by year (no SOTU in 1933)
sotu <- arrange(sotu, desc(Year))

#add party and president name to the dataframe (watch out! because different parties are overlapping for the same year: 1953, 1961 & 1981) => add manually

#1 add order column
sotu$order <- c(1:246)

#2. Add name of presidents (probably a more productive way to do this, but haven't found it yet), William Harisson & James A. Garfield are ommited, no SOTU. Ad NA as starting value. 
sotu$President <- NA
sotu$President <- ifelse(sotu$order %in% c(246:239),
                         "George Washington", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(238:235),
                         "John Adams", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(234:227),
                         "Thomas Jefferson", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(226:219),
                         "James Madison", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(218:211),
                         "James Monroe", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(210:207),
                         "John Quincy Adams", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(206:199),
                         "Andrew Jackson", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(198:195),
                         "Martin Van Buren", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(194:191),
                         "John Tyler", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(190:187),
                         "James K. Polk", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(186),
                         "Zachary Taylor", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(185:183),
                         "Millard Fillmore", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(182:179),
                         "Franklin Pierce", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(178:175),
                         "James Buchanan", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(174:171),
                         "Abraham Lincoln", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(170:167),
                         "Andrew Johnson", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(166:159),
                         "Ulysses S. Grant", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(158:155),
                         "Rutherford B. Hayes", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(154:151),
                         "Chester A. Arthur", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(150:147),
                         "Grover Cleveland", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(146:143),
                         "Benjamin Harrison", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(142:139),
                         "Grover Cleveland", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(138:135),
                         "William McKinley", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(134:127),
                         "Theodore Roosevelt", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(126:123),
                         "William Howard Taft", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(122:115),
                         "Woodrow Wilson", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(114:113),
                         "Warren G. Harding", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(112:107),
                         "Calvin Coolidge", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(106:103),
                         "Herbert Hoover", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(102:90),
                         "Franklin D. Roosevelt", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(89:82),
                         "Harry S. Truman", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(81:72),
                         "Dwight D. Eisenhower", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(71:69),
                         "John F. Kennedy", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(68:63),
                         "Lyndon B. Johnson", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(62:51),
                         "Richard M. Nixon", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(50:48),
                         "Gerald R. Ford", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(47:41),
                         "Jimmy Carter", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(40:33),
                         "Ronald Reagan", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(32:29),
                         "George Bush", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(28:21),
                         "William J. Clinton", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(20:13),
                         "George W. Bush", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(12:5),
                         "Barack Obama", sotu$President)
sotu$President <- ifelse(sotu$order %in% c(4:1),
                         "Donald J. Trump", sotu$President)

#Add Political Party
sotu$Party <- NA

sotu$Party <- ifelse(sotu$President %in% c("George Washington"),
                     "Independent", sotu$Party)
sotu$Party <- ifelse(sotu$President %in% c("John Adams"),
                     "Federalist", sotu$Party)
sotu$Party <- ifelse(sotu$President %in% c("Thomas Jefferson", "James Madison", "James Monroe", "John Quincy Adams"),
                     "Democratic-Republican", sotu$Party)
sotu$Party <- ifelse(sotu$President %in% c("Andrew Jackson", "Martin Van Buren", "James K. Polk", "Franklin Pierce", "James Buchanan", "Grover Cleveland", 
                                           "Woodrow Wilson", "Franklin D. Roosevelt", "Harry S. Truman", "John F. Kennedy", "Lyndon B. Johnson", "Jimmy Carter", 
                                           "William J. Clinton", "Barack Obama"),
                     "Democrat", sotu$Party)
sotu$Party <- ifelse(sotu$President %in% c("John Tyler", "Zachary Taylor", "Millard Fillmore"),
                     "Whig", sotu$Party)
sotu$Party <- ifelse(sotu$President %in% c("Andrew Johnson"),
                     "Democrat (Union)", sotu$Party)
sotu$Party <- ifelse(sotu$President %in% c("Abraham Lincoln", "Ulysses S. Grant", "Rutherford B. Hayes", "Chester A. Arthur", "Benjamin Harrison", 
                                           "William McKinley", "Theodore Roosevelt", "William Howard Taft", "Warren G. Harding", "Calvin Coolidge", "Herbert Hoover", 
                                           "Dwight D. Eisenhower", "Richard M. Nixon", "Gerald R. Ford", "Ronald Reagan", "George Bush", "George W. Bush", 
                                           "Donald J. Trump"),
                     "Republican", sotu$Party)

#order column is redundant, so it can be removed
sotu$order <- NULL

#rearrange the columns
select(sotu, "Year", "President", "Party", "links")



#extract text and put in directory
for (i in seq(nrow(sotu))) {
  sotu.text <- read_html(sotu$links[i]) %>%
    html_nodes(".date-display-single , .field-docs-content p") %>%
    html_text()
  filename <- paste0("SOTU", " ", sotu$Year[i], " ", sotu$President[i], " ", sotu$Party[i], ".txt")
  cat(sotu.text, file=filename, sep = "\n")
}

#You can then separate them as you see fit, and copy/paste all of Nixon's messages into 1 .txt file.















