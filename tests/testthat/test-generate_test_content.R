test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(newsgroup_stm)
    expect_equal(x$topic, NULL)
})
