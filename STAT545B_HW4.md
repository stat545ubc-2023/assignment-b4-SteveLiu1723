    # Import the book and break it into a list of words
    library(janeaustenr)
    book <- janeaustenr::prideprejudice
    first_split_book <- book |> strsplit(split=" ")
    second_split_book <- list()
    for(i in 1:length(first_split_book)){
      sub_book <- first_split_book[[i]] |> strsplit(split=" ")
      second_split_book <- append(second_split_book,sub_book)
    }
    book_words_unfiltered <- unlist(second_split_book)

    # Remove all non-letter symbols in the list 
    book_words_unfiltered <- gsub("[[:punct:]]", "", book_words_unfiltered)
    book_words_unfiltered <- gsub("[[:digit:]]", "", book_words_unfiltered)

    # filter the list of words using stopwords
    library(tidytext)
    stopwords <- tidytext::stop_words[,1] |> as.list()
    word_list <- stopwords$word
    book_words_unfiltered <- as.character(book_words_unfiltered)
    choose_index <- book_words_unfiltered %in% word_list
    book_words_filtered <- book_words_unfiltered[!choose_index]

    # Select 15 words with the highest frequency
    word_counts <- as.data.frame(table(unlist(strsplit(book_words_filtered, "\ ")))) 
    word_counts <- with(word_counts, word_counts[Var1 != "", ] )
    word_counts <- word_counts[order(word_counts$Freq, decreasing = TRUE),]
    words_for_plot <- head(word_counts,15)

    # Make the Barplot
    library(ggplot2)
    library(forcats)
    ggplot(data=words_for_plot,aes(x=fct_rev(fct_reorder(Var1, Freq)), y=Freq)) +
    geom_bar(stat="identity")+
    geom_text(aes(label = Freq), vjust = -0.2)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(x="Words that often occur", y = "Frequncy of the words")

![](STAT545B_HW4_files/figure-markdown_strict/unnamed-chunk-2-1.png)
    #' Steve-Style Pig Latin Generator
    #' 
    #' @description
    #' This is a function that convert a word into a Steve-Style Pig Latin word.
    #' The two components in this function are modified based on the default Pig
    #' Latin rearrangement rule and the default Pig Latin addition rule. The first
    #' component will move the last letter to the beginning of the word, and the 
    #' second component will add a "steve" to the end of word.
    #'
    #' @param
    #' The input of this function will be a single character or a list of character.
    #' Before the converting the input, whether the input belongs to the type of 
    #' character will be tested first.
    #' 
    #' @return
    #' It will return a single character or a list of character, so the same type of 
    #' the input. If a valid input is provided, then it will throw an error.
    #' 
    #' @example
    #' # valid input
    #' steve_PL_generator("word") -> "dworsteve"
    #' steve_PL_generator("steve") -> "estevsteve"
    #' # invalid input
    #' steve_PL_generator(123) -> Error: Please pass character(s)

    steve_PL_generator <- function(word){
      if (is.character(word)){
        str_c(str_sub(word,-1,-1), str_sub(word,1,-2), "steve")
      }
      else{
        stop("Please pass character(s)")
      }
    }

    # Three non-redundant tests to the function 
    test_that("steve_PL_generator", {
        expect_equal('dworsteve', steve_PL_generator("word"))
        expect_equal(c('cabsteve','dbcsteve'), steve_PL_generator(c("abc","bcd")))
        expect_error(steve_PL_generator(iris))
        expect_error(steve_PL_generator(123))
    })

    ## ── Error ('<text>:3:5'): steve_PL_generator ────────────────────────────────────
    ## Error in `str_c(str_sub(word, -1, -1), str_sub(word, 1, -2), "steve")`: could not find function "str_c"
    ## Backtrace:
    ##  1. testthat::expect_equal("dworsteve", steve_PL_generator("word"))
    ##  4. global steve_PL_generator("word")

    ## Error in `reporter$stop_if_needed()`:
    ## ! Test failed

\`
