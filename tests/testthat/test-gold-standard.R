context("Gold Standard")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}

test_that("generate_gold_standard", {
    expect_error(oolong:::.generate_gold_standard(quanteda::corpus(newsgroup5$text)), NA)
    expect_error(oolong:::.generate_gold_standard(newsgroup5$text, frac = 2))
    expect_error(oolong:::.generate_gold_standard(newsgroup5$text, frac = -0.1))
    expect_error(oolong:::.generate_gold_standard(newsgroup5$text, frac = 0.5), NA)
})

test_that("integration into create_oolong", {
    expect_error(create_oolong(input_corpus = quanteda::corpus(newsgroup5$text)), NA)
    expect_error(create_oolong(input_corpus = newsgroup5$text, frac = 2))
    expect_error(create_oolong(input_corpus = newsgroup5$text, frac = -0.1))
    expect_error(create_oolong(input_corpus = newsgroup5$text, frac = 0.5), NA)
})

