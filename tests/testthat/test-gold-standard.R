##context("Gold Standard")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}

test_that("generate_gold_standard", {
    expect_error(oolong:::.generate_gold_standard(quanteda::corpus(abstracts$text)), NA)
    expect_error(oolong:::.generate_gold_standard(abstracts$text, frac = 2))
    expect_error(oolong:::.generate_gold_standard(abstracts$text, frac = -0.1))
    expect_error(oolong:::.generate_gold_standard(abstracts$text, frac = 0.5), NA)
})

test_that("integration into create_oolong", {
    expect_error(create_oolong(input_corpus = quanteda::corpus(abstracts$text)), NA)
    expect_error(create_oolong(input_corpus = abstracts$text, frac = 2))
    expect_error(create_oolong(input_corpus = abstracts$text, frac = -0.1))
    expect_error(create_oolong(input_corpus = abstracts$text, frac = 0.5), NA)
})

test_that("locking", {
    x <- create_oolong(input_corpus = quanteda::corpus(abstracts$text))
    x$lock(force = TRUE)
    expect_error(x$do_gold_standard_test())
})

test_that("type override input_model", {
    x <- create_oolong(abstracts_seededlda, abstracts$text, type = "gs")
    expect_true("oolong_test_gs" %in% class(x))
})
