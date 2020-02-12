context("Support for topicmodels")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}

test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(newsgroup_topicmodels)
    expect_null(x$topic)
    x <- oolong:::.generate_test_content(newsgroup_topicmodels, quanteda::corpus(newsgroup5$text))
    expect_false(is.null(x$topic))
})

test_that("check_complete", {
    x <- oolong:::.generate_test_content(newsgroup_topicmodels)
    expect_false(oolong:::.check_test_content_complete(x))
    x$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(x))
    y <- oolong:::.generate_test_content(newsgroup_topicmodels, newsgroup5$text)
    expect_false(oolong:::.check_test_content_complete(y))
    y$topic$answer <- 1
    expect_false(oolong:::.check_test_content_complete(y))
    y$word$answer <- 1
    expect_true(oolong:::.check_test_content_complete(y))
    z <- create_oolong(newsgroup_topicmodels)
    expect_error(z$lock())
})
