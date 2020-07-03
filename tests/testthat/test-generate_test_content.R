context("check helper functions")

test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(abstracts_stm)
    expect_null(x$topic)
    x <- oolong:::.generate_test_content(abstracts_stm, quanteda::corpus(abstracts$text))
    expect_false(is.null(x$topic))
})

test_that("check_complete", {
    x <- oolong:::.generate_test_content(abstracts_stm)
    expect_false(oolong:::.check_test_content_complete(x))
    x$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_stm, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$topic$answer <- 1
    expect_false(oolong:::.check_test_content_complete(y))
    y$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(y))
    z <- create_oolong(abstracts_stm)
    expect_error(z$lock())
})

test_that("K is too small", {
    expect_error(oolong:::.generate_test_content(abstracts_stm, abstracts$text, n_top_topics = 20))
})

test_that("n_top_terms is considered #29", {
    z <- oolong:::.generate_test_content(abstracts_stm, abstracts$text, n_top_terms = 10)
    expect_equal(length(z$word$candidates[[1]]), 11)
})

