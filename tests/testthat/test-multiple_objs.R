context("check_correctness")
test_that("check_calculation", {
    obj1 <- create_oolong(newsgroup_stm)
    obj2 <- oolong_clone(obj1)
    obj3 <- oolong_clone(obj1)
    ## Mocking coding

    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    obj1$lock()
    obj2$.__enclos_env__$private$test_content$word$answer <- obj2$.__enclos_env__$private$test_content$word$intruder
    obj2$.__enclos_env__$private$test_content$word$answer[1] <- "wronganswer"
obj2$lock()
    obj3$.__enclos_env__$private$test_content$word$answer <- obj3$.__enclos_env__$private$test_content$word$intruder
    obj3$.__enclos_env__$private$test_content$word$answer[1:4] <- "wronganswer"
    obj3$lock()
    res <- summarize_oolong(obj1, obj2, obj3)
    expect_length(res$rater_precision, 3)
    expect_length(res$k_precision, 10)
})

