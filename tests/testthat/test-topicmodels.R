## context("Support for topicmodels")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}

test_that("generate_test_content", {
    skip_on_cran()
    skip_if_not(exists("abstracts_topicmodels"))
    x <- oolong:::.generate_test_content(abstracts_topicmodels)
    expect_null(x$ti)
    x <- oolong:::.generate_test_content(abstracts_topicmodels, quanteda::corpus(abstracts$text))
    expect_false(is.null(x$ti))
})

test_that("check_complete", {
    skip_on_cran()
    skip_if_not(exists("abstracts_topicmodels"))
    x <- oolong:::.generate_test_content(abstracts_topicmodels)
    expect_false(oolong:::.check_test_content_complete(x))
    x$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_topicmodels, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$ti$answer <- 1
    expect_false(oolong:::.check_test_content_complete(y))
    y$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(y))
    z <- create_oolong(abstracts_topicmodels)
    expect_error(z$lock())
})

test_that("github issue #8 - word", {
    skip_if_not_installed("topicmodels")
    library(topicmodels)
    data("AssociatedPress")
    lda <- LDA(AssociatedPress[1:20,], k = 5)
    expect_error(create_oolong(lda), NA)
})

test_that("github issue #8 - topic", {
    skip_if_not_installed("topicmodels")
    library(topicmodels)
    library(quanteda)
    test_corpus <- tokens(corpus(abstracts[1:10,], text_field = "text"))
    test_dfm <- dfm(test_corpus)
    topicmodels_dfm <- convert(test_dfm, to = "topicmodels")
    lda <- LDA(topicmodels_dfm, k = 5)
    expect_error(create_oolong(lda, abstracts$text[1:10], exact_n = 5), NA)
})

test_that("generate_topic_content", {
    skip_if_not(exists("abstracts_topicmodels"))
    ## reference issue #8
    ## frac too small!
    expect_error(oolong:::.generate_test_content(abstracts_topicmodels, abstracts$text[1:10], frac = 0.1))
    expect_warning(oolong:::.generate_test_content(abstracts_topicmodels, abstracts$text[1:10], exact_n = 12))
})

test_that("ui", {
    skip_if_not(exists("abstracts_topicmodels"))
    expect_error(wi(abstracts_topicmodels), NA)
    expect_error(witi(abstracts_topicmodels, abstracts$text), NA)
    expect_error(ti(abstracts_topicmodels, abstracts$text), NA)
    expect_error(wsi(abstracts_topicmodels), NA)
})
