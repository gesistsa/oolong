context("check_correctness")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}

test_that("defensive programming", {
    obj1 <- create_oolong(newsgroup_stm)
    obj2 <- clone_oolong(obj1)
    ### Not lock
    expect_error(summarize_oolong(obj1))
    obj1 <- genius_word(obj1)
    obj1$lock()
    ### Obj2 is not lock
    expect_error(summarize_oolong(obj1, obj2))
    ### Testing checking hash.
    set.seed(1212112)
    obj1 <- create_oolong(newsgroup_stm)
    obj2 <- clone_oolong(obj1)
    set.seed(12121999)
    obj3 <- create_oolong(newsgroup_stm)
    obj1 <- genius_word(obj1)
    obj2 <- genius_word(obj2)
    obj3 <- genius_word(obj3)
    obj1$lock()
    obj2$lock()
    obj3$lock()
    ## obj1/obj2 are different from obj3
    expect_error(summarise_oolong(obj1, obj2, obj3))
    ## obj2 is a clone of obj1
    expect_error(summarise_oolong(obj1, obj2), NA)
    ## Warning about premature locking
    obj1 <- create_oolong(newsgroup_stm)
    obj1$lock(force = TRUE)
    expect_warning(summarize_oolong(obj1))
})

test_that("check_calculation_word_intrusion", {
    obj1 <- create_oolong(newsgroup_stm)
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
    expect_length(res$k_precision, 10)
    ### Single object
    res <- summarize_oolong(obj1)
    expect_length(res$rater_precision, 1)
    res <- summarize_oolong(obj1, obj2, obj3)
    res2 <- summarise_oolong(obj1, obj2, obj3)
    expect_equal(res, res2)
})

