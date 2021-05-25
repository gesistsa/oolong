## context("Support for BTM")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}

test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(abstracts_btm)
    expect_null(x$topic)
})

test_that("BTM word intrusion", {
    x <- oolong:::.generate_test_content(abstracts_btm)
    expect_false(oolong:::.check_test_content_complete(x))
    x$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    z <- create_oolong(abstracts_btm)
    expect_error(z$lock())
})

test_that("BTM topic intrusion", {
    library(quanteda)
    library(BTM)
    abstracts_corpus <- corpus(abstracts$text)
    tokens(abstracts_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>%  tokens_tolower() %>% tokens_remove(stopwords("en")) %>% tokens_wordstem() -> toks_q
    as.data.frame.tokens <- function(x) {
        data.frame(
            doc_id = rep(names(x), lengths(x)),
            tokens = unlist(x, use.names = FALSE)
        )
    }
    abstracts_df <- as.data.frame.tokens(toks_q)
    expect_error(create_oolong(abstracts_btm, abstracts_corpus))
    expect_error(create_oolong(abstracts_btm, abstracts$text, btm_dataframe = abstract_df))
    expect_error(create_oolong(abstracts_btm, abstracts_corpus, btm_dataframe = abstracts_df), NA)
})
