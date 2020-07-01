context("Support for keyATM")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}

test_that("generate_test_content", {
    skip_on_cran()
    x <- oolong:::.generate_test_content(abstracts_keyatm)
    expect_null(x$topic)
})

test_that("check_complete", {
    skip_on_cran()
    x <- oolong:::.generate_test_content(abstracts_keyatm)
    expect_false(oolong:::.check_test_content_complete(x))
    x$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_keyatm, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    z <- create_oolong(abstracts_keyatm)
    expect_error(z$lock())
    z1 <- create_oolong(abstracts_keyatm, abstracts$text)
    expect_error(z1$lock())
})
