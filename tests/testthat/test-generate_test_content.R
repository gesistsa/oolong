## context("check helper functions")

test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(abstracts_seededlda)
    expect_null(x$ti)
    x <- oolong:::.generate_test_content(abstracts_seededlda, quanteda::corpus(abstracts$text))
    expect_false(is.null(x$ti))
})

test_that("check_complete", {
    x <- oolong:::.generate_test_content(abstracts_seededlda)
    expect_false(oolong:::.check_test_content_complete(x))
    x$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_seededlda, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$ti$answer <- 1
    expect_false(oolong:::.check_test_content_complete(y))
    y$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(y))
    z <- create_oolong(abstracts_seededlda)
    expect_error(z$lock())
})

test_that("K is too small", {
    expect_error(oolong:::.generate_test_content(abstracts_seededlda, abstracts$text, n_top_topics = 20))
})

test_that("n_top_terms is considered #29", {
    z <- oolong:::.generate_test_content(abstracts_seededlda, abstracts$text, n_top_terms = 10)
    expect_equal(length(z$wi$candidates[[1]]), 11)
})

test_that("generate_test_content type", {
    x <- oolong:::.generate_test_content(abstracts_seededlda, input_corpus = abstracts$text, type = 'wi')
    expect_null(x$ti)
    x <- oolong:::.generate_test_content(abstracts_seededlda, input_corpus = abstracts$text, type = 'ti')
    expect_null(x$wi)
})

test_that("generate_test_content wsi", {
    x <- oolong:::.generate_test_content(abstracts_seededlda, input_corpus = abstracts$text, n_topiclabel_words = 4, type = 'wsi')
    expect_null(x$ti)
    expect_null(x$wi)
    expect_false(is.null(x$wsi))
    expect_error(oolong:::.generate_test_content(abstracts_seededlda, n_correct_ws = 10, n_topiclabel_words = 8, type = 'wsi'))
    expect_error(oolong:::.generate_test_content(abstracts_seededlda, n_correct_ws = 10, n_topiclabel_words = 8, wsi_n_top_terms = 100,  type = 'wsi'), NA)
})
