
library(tidyverse)
library(stringr)
library (readr)
library(tidytext)
library(janeaustenr)
library(glue)
library(textdata)


get_sentiments(lexicon = c("bing", "afinn", "loughran", "nrc"))



tidy_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup() %>%
    unnest_tokens(word, text)



pride_prejudice <- tidy_books %>%
    filter(book == "Pride & Prejudice")

pride_prejudice



afinn <- pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    group_by(index = linenumber %/% 80) %>%
    summarise(sentiment = sum(200)) %>%
    mutate(method = "AFINN")



bing_and_nrc <- bind_rows(
    pride_prejudice %>%
        inner_join(get_sentiments("bing")) %>%
        mutate(method = "Bing et al."),
    pride_prejudice %>%
        inner_join(get_sentiments("nrc") %>%
                       filter(sentiment %in% c("positive",
                                               "negative"))) %>%
        mutate(method = "NRC")) %>%
    count(method, index = linenumber %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

















austen_chapters <- austen_books() %>%
    group_by(book) %>%
    unnest_tokens(chapter, text, token = "regex",
                  pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
    ungroup()

austen_chapters %>%
    group_by(book) %>%
    summarise(chapters = n())




bingnegative <- get_sentiments("bing") %>%
    filter(sentiment == "negative")

wordcounts <- tidy_books %>%
    group_by(book, chapter) %>%
    summarize(words = n())

tidy_books %>%
    semi_join(bingnegative) %>%
    group_by(book, chapter) %>%
    summarize(negativewords = n()) %>%
    left_join(wordcounts, by = c("book", "chapter")) %>%
    mutate(ratio = negativewords/words) %>%
    filter(chapter != 0) %>%
    top_n(1) %>%
    ungroup()









# write a function that takes the name of a file and returns the # of postive
# sentiment words, negative sentiment words, the difference & the normalized difference
GetSentiment <- function(file){
    # get the file
    fileName <- glue("./presentation/email/", file, sep = "")
    # get rid of any sneaky trailing spaces
    fileName <- trimws(fileName)
    
    # read in the new file
    fileText <- glue(read_file(fileName))
    # remove any dollar signs (they're special characters in R)
    fileText <- gsub("\\$", "", fileText) 
    
    # tokenize
    tokens <- tibble(text = fileText) %>% unnest_tokens(word, text)
    
    # get the sentiment from the first text: 
    sentiment <- tokens %>%
        inner_join(get_sentiments("bing")) %>% # pull out only sentimen words
        count(sentiment) %>% # count the # of positive & negative words
        spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
        mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
        mutate(file = file) %>% # add the name of our file
        mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
        mutate(president = str_match(file, "(.*?)_")[2]) # add president
    
    # return our sentiment dataframe
    return(sentiment)
}



files <- list.files("./presentation/email")


GetSentiment(files[1])









GetSentiment_af <- function(file){
    # get the file
    fileName <- glue("./presentation/email/", file, sep = "")
    # get rid of any sneaky trailing spaces
    fileName <- trimws(fileName)
    
    # read in the new file
    fileText <- glue(read_file(fileName))
    # remove any dollar signs (they're special characters in R)
    fileText <- gsub("\\$", "", fileText) 
    
    # tokenize
    tokens <- tibble(text = fileText) %>% unnest_tokens(word, text)
    
    # get the sentiment from the first text: 
    afinn <- tokens %>%
        mutate(word_count = 1:n(),
               index = word_count %/% 500 + 1) %>% 
        inner_join(get_sentiments("afinn")) %>%
        group_by(index) %>%
        summarise(sentiment = sum(value)) %>%
        mutate(method = "AFINN")
    
    # return our sentiment dataframe
    return(afinn)
}



GetSentiment_af(files[1])






# read in the new file
fileText <- glue(read_file("/Users/dsimbandumwe/dev/cuny/data_607/DATA607/presentation/email/Foolproof.txt"))
# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 


str <- unlist(str_extract_all(fileText, "[:graph:]*@[:graph:]*"))
str

str <-  str_remove_all(str, "\\<|\\>|\\\"")
str


known_emails <- c("dsimbandumwe@gmail.com","ds@gmail.com","return@gmail.com")
str <-  setdiff(str, known_emails)
str




s2 <- unlist(str_extract_all("To: bob@gmail.com", "[t]*\\:\\s[:graph:]*@[:graph:]*"))
s2


s2 <- unlist(str_extract_all(fileText, "[t]*\\:\\s[:graph:]*@[:graph:]*"))
s2 <-  str_remove_all(s2, "\\s|\\:|\\<|\\>|\\\"")
s2

s3 = unlist(regmatches(fileText, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", fileText)))
s3


df <- tibble(str)
df



df <- data.frame( name = character(),
                  event_id = double(),
                  event = character(),
                  email_related = character()
)











email_path = "/Users/dsimbandumwe/dev/cuny/data_607/DATA607/presentation/email/"
    
GetFile <- function(file, path){
    # get the file
    fileName <- glue(path, file, sep = "")
    # get rid of any sneaky trailing spaces
    fileName <- trimws(fileName)
    
    # read in the new file
    fileText <- glue(read_file(fileName))
    # remove any dollar signs (they're special characters in R)
    fileText <- gsub("\\$", "", fileText) 
    return(fileText)
}


GetToEmail <- function(fileText) {
    s1 <- unlist(str_extract_all(fileText, "[t]*\\:\\s[:graph:]*@[:graph:]*"))
    s1 <-  str_remove_all(s1, "\\s|\\:|\\<|\\>|\\\"")
    return (s1)
}


GetRelatedEmails <- function(fileText, toEmail) {
    known_emails <- c("dsimbandumwe@gmail.com","ds@gmail.com","return@gmail.com")
    
    str = unlist(regmatches(fileText, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", fileText)))
    str <-  setdiff(str, known_emails)
    str <- setdiff(str, toEmail)
    return (tibble(str))
}



GetSentiment <- function(fileText, toEmail) {


    # tokenize
    tokens <- tibble(text = fileText) %>% unnest_tokens(word, text)
    
    # get the sentiment from the first text: 
    afinn <- tokens %>%
        mutate(word_count = 1:n(),
               index = word_count %/% 500 + 1) %>% 
        inner_join(get_sentiments("afinn")) %>%
        group_by(index) %>%
        summarise(sentiment = sum(value)) %>%
        mutate(method = "AFINN")
    
    # return our sentiment dataframe
    
    afinn %>% 
        mutate (
            email = toEmail
        )
    
    return(afinn)
    
}



files <- list.files("./presentation/email")
fileText <- GetFile(files[1],email_path)
toEmail <- GetToEmail(fileText)
sent_df <- GetSentiment(fileText, toEmail)
rel_df <- GetRelatedEmails(fileText, toEmail)




