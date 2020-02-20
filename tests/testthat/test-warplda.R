context("check Warp LDA support")
test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(abstracts_warplda)
    expect_null(x$topic)
    ### NO input_dfm
    expect_error(oolong:::.generate_test_content(abstracts_warplda, quanteda::corpus(abstracts$text)))
    x <- oolong:::.generate_test_content(abstracts_warplda, quanteda::corpus(abstracts$text), input_dfm = abstracts_dfm)
    expect_true(!is.null(x$topic))
})

