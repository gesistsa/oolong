## context("Support for keyATM")

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
    skip_if_not(exists("abstracts_keyatm"))
    x <- oolong:::.generate_test_content(abstracts_keyatm)
    expect_null(x$ti)
})

test_that("check_complete", {
    skip_on_cran()
    skip_if_not(exists("abstracts_keyatm"))
    x <- oolong:::.generate_test_content(abstracts_keyatm)
    expect_false(oolong:::.check_test_content_complete(x))
    x$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(abstracts_keyatm, abstracts$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$wi$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    z <- create_oolong(abstracts_keyatm)
    expect_error(z$lock())
    z1 <- create_oolong(abstracts_keyatm, abstracts$text)
    expect_error(z1$lock())
})
