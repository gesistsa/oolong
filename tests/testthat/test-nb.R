set.seed(46709394)

test_that("generate_test_content", {
    x <- oolong:::.generate_test_content(newsgroup_nb)
    expect_null(x$topic)
})

test_that("basic", {
    expect_error(wi(newsgroup_nb), NA)
    expect_error(wsi(newsgroup_nb), NA)
    expect_error(ti(newsgroup_nb, abstracts$text))
    expect_error(witi(newsgroup_nb, abstracts$text))    
})
