##context("check Warp LDA support")
test_that("generate_test_content", {
    skip_if_not(exists("abstracts_warplda"))
    x <- oolong:::.generate_test_content(abstracts_warplda)
    expect_null(x$ti)
    ### NO input_dfm
    expect_error(oolong:::.generate_test_content(abstracts_warplda, quanteda::corpus(abstracts$text)))
    x <- oolong:::.generate_test_content(abstracts_warplda, quanteda::corpus(abstracts$text), input_dfm = abstracts_dfm)
    expect_true(!is.null(x$ti))
})

