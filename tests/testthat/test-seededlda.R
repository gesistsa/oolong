## context("Support for seededlda")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}

## seeded lda

test_that("seeded lda: generate_test_content", {
    skip_on_cran()
    x <- oolong:::.generate_test_content(abstracts_seededlda)
    expect_null(x$topic)
    x <- oolong:::.generate_test_content(abstracts_seededlda, quanteda::corpus(abstracts$text))
    expect_false(is.null(x$topic))
})

test_that("seeded lda: check_complete", {
    skip_on_cran()
    x <- oolong:::.generate_test_content(abstracts_seededlda)
    expect_false(oolong:::.check_test_content_complete(x))
    x$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_seededlda, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$topic$answer <- 1
    expect_false(oolong:::.check_test_content_complete(y))
    y$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(y))
    z <- create_oolong(abstracts_seededlda)
    expect_error(z$lock())
})

test_that("unseeded lda: generate_test_content", {
    skip_on_cran()
    x <- oolong:::.generate_test_content(abstracts_unseededlda)
    expect_null(x$topic)
    x <- oolong:::.generate_test_content(abstracts_unseededlda, quanteda::corpus(abstracts$text))
    expect_false(is.null(x$topic))
})

test_that("unseeded lda: check_complete", {
    skip_on_cran()
    x <- oolong:::.generate_test_content(abstracts_unseededlda)
    expect_false(oolong:::.check_test_content_complete(x))
    x$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_unseededlda, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$topic$answer <- 1
    expect_false(oolong:::.check_test_content_complete(y))
    y$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(y))
    z <- create_oolong(abstracts_unseededlda)
    expect_error(z$lock())
})
