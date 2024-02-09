## context("Support for seededlda")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}

## seeded lda

test_that("seeded lda: generate_test_content", {
    x <- oolong:::.generate_test_content(abstracts_seededlda)
    expect_null(x$ti)
    x <- oolong:::.generate_test_content(abstracts_seededlda, quanteda::corpus(abstracts$text))
    expect_false(is.null(x$ti))
})

test_that("seeded lda: check_complete", {
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

test_that("unseeded lda: generate_test_content", {
    skip_on_cran()
    skip_if_not(exists("abstracts_unseededlda"))
    x <- oolong:::.generate_test_content(abstracts_unseededlda)
    expect_null(x$ti)
    x <- oolong:::.generate_test_content(abstracts_unseededlda, quanteda::corpus(abstracts$text))
    expect_false(is.null(x$ti))
})

test_that("unseeded lda: check_complete", {
    skip_on_cran()
    skip_if_not(exists("abstracts_unseededlda"))
    x <- oolong:::.generate_test_content(abstracts_unseededlda)
    expect_false(oolong:::.check_test_content_complete(x))
    x$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_unseededlda, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$ti$answer <- 1
    expect_false(oolong:::.check_test_content_complete(y))
    y$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(y))
    z <- create_oolong(abstracts_unseededlda)
    expect_error(z$lock())
})

## ui
test_that("UI", {
    skip_if_not(exists("abstracts_unseededlda"))
    expect_error(witi(abstracts_seededlda, abstracts$text), NA)
    expect_error(witi(abstracts_unseededlda, abstracts$text), NA)
    expect_error(ti(abstracts_seededlda, abstracts$text), NA)
    expect_error(ti(abstracts_unseededlda, abstracts$text), NA)
    expect_error(wi(abstracts_seededlda), NA)
    expect_error(wi(abstracts_unseededlda), NA)
    expect_error(wsi(abstracts_seededlda), NA)
    expect_error(wsi(abstracts_unseededlda), NA)
})
