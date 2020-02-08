test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(newsgroup_stm)
    expect_null(x$topic)
    x <- oolong:::.generate_test_content(newsgroup_stm, quanteda::corpus(newsgroup5$text))
    expect_false(is.null(x$topic))
})
