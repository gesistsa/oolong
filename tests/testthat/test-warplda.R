context("check Warp LDA support")

test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(newsgroup_warplda)
    expect_null(x$topic)
    ### NO input_dfm
    expect_error(oolong:::.generate_test_content(newsgroup_warplda, quanteda::corpus(newsgroup5$text)))
    ### TODO This one generate warnings randomly
    x <- oolong:::.generate_test_content(newsgroup_warplda, quanteda::corpus(newsgroup5$text), input_dfm = newsgroup5_dfm)
    expect_true(!is.null(x$topic))
})

