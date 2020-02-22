context("summarize_oolong")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}

test_that("Correct UI", {
    obj1 <- create_oolong(abstracts_stm)
    obj2 <- create_oolong(input_corpus = trump2k, exact_n = 20)
    obj1 <- genius_word(obj1)
    obj1$lock()
    res1 <- summarize_oolong(obj1)
    expect_true(res1$type == "tm")
    obj2$.__enclos_env__$private$test_content$gold_standard$answer <- sample(1:5, size = 20, replace = TRUE)
    obj2$lock()
    res2 <- summarize_oolong(obj2, target_value = rnorm(n = 20))
    expect_true(res2$type == "gs")
})

test_that("check_calculation_word_intrusion_multiobject", {
    obj1 <- create_oolong(abstracts_stm)
    obj2 <- clone_oolong(obj1)
    obj3 <- clone_oolong(obj1)
    ## Mocking coding
    obj1 <- genius_word(obj1)
    obj1$lock()
    obj2 <- genius_word(obj2)
    obj2$.__enclos_env__$private$test_content$word$answer[1] <- "wronganswer"
    obj2$lock()
    obj3 <- genius_word(obj3)
    obj3$.__enclos_env__$private$test_content$word$answer[1:4] <- "wronganswer"
    obj3$lock()
    res <- summarize_oolong(obj1, obj2, obj3)
    expect_length(res$rater_precision, 3)
    expect_length(res$k_precision, 20)
    ### Single object
    res <- summarize_oolong(obj1)
    expect_length(res$rater_precision, 1)
})

test_that("check_calculation_word_intrusion_single_object", {
    obj1 <- create_oolong(abstracts_stm)
    obj1 <- genius_word(obj1)
    obj1$lock()
    expect_error(summarize_oolong(obj1), NA)
})

test_that("check_calculation_topic_intrusion_single_object", {
    obj1 <- create_oolong(abstracts_stm, abstracts$text)
    obj1 <- genius_word(obj1)
    obj1 <- genius_topic(obj1)
    obj1$lock()
    expect_error(summarize_oolong(obj1), NA)
})

test_that("check_calculation_topic_intrusion_multiobject", {
    obj1 <- create_oolong(abstracts_stm, abstracts$text, exact_n = 10)
    obj2 <- clone_oolong(obj1)
    obj1 <- genius_word(obj1)
    obj1 <- genius_topic(obj1)
    obj1$lock()
    obj2 <- genius_word(obj2)
    obj2 <- genius_topic(obj2)
    obj2$lock()
    res <- summarize_oolong(obj1, obj2)
    expect_length(res$tlo_p_value, 1)
    expect_length(res$tlo, 20)
    ## Might be better to move this to test-printing.
    ## Travis - Don't like this
    output1 <- capture_output({ res }, print = TRUE)
    expect_true(stringr::str_detect(output1, "Mean model precision"))
    expect_error(plot(res))
})

test_that("Forcibly locking", {
    ex1 <- create_oolong(abstracts_stm, abstracts$text, exact_n = 100)
    ex2 <- clone_oolong(ex1)
    ex1 <- genius_word(ex1)
    ex1 <- genius_topic(ex1)
    ex1$lock()
    ex2 <- genius_word(ex2)
    ex2 <- genius_topic(ex2)
    ex2$.__enclos_env__$private$test_content$topic$answer[1] <- NA
    ex2$lock(force = TRUE)
    expect_warning({ res <- summarize_oolong(ex1, ex2) })
    expect_warning({ res <- summarize_oolong(ex2) })
})

test_that("Monkeying problem #14", {
    obj1 <- create_oolong(abstracts_stm, abstracts$text)
    obj1 <- genius_topic(obj1)
    obj1 <- genius_word(obj1)
    previous_answer <- obj1$.__enclos_env__$private$test_content$topic$answer
    obj1$lock(force = TRUE)
    summarise_oolong(obj1)
    new_answer <- obj1$.__enclos_env__$private$test_content$topic$answer
    expect_true(all.equal(previous_answer, new_answer))
})
