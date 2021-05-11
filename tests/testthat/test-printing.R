## context("Prining objects")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}

test_that("gs_creation", {
    ## output1 <- capture_output({ x <- create_oolong(input_corpus = abstracts$text); x }, print = TRUE)
    ## expect_true(stringr::str_detect(output1, "gold standard generation"))
    expect_snapshot(create_oolong(input_corpus = abstracts$text))
})

test_that("gs_turngold", {
    expect_snapshot({ x <- create_oolong(input_corpus = abstracts$text); x$lock(force = TRUE); x$turn_gold() })
})


