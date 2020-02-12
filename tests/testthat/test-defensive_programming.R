context("Defensive program")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$word$answer <- obj1$.__enclos_env__$private$test_content$word$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$topic$answer <- obj1$.__enclos_env__$private$test_content$topic$intruder
    return(obj1)
}


test_that("locking", {
    ## premature locking
    x <- create_oolong(newsgroup_stm)
    expect_error(x$lock())
    expect_error(x$lock(force = TRUE), NA)
    x <- create_oolong(newsgroup_stm, newsgroup5$text)
    expect_error(x$lock())
    ## error when only word intrusion test is done.
    x <- genius_word(x)
    expect_error(x$lock())
    x <- genius_topic(x)
    expect_error(x$lock(), NA)
})

test_that("cloning", {
    x <- create_oolong(newsgroup_stm)
    expect_error(clone_oolong(x), NA)
    x <- genius_word(x)
    ## Cannot clone a partially coded object.
    expect_error(clone_oolong(x))
    x <- create_oolong(newsgroup_stm)
    x$lock(force = TRUE)
    expect_error(clone_oolong(x))
    ## Cloned object is not sharing the same private space.
    x <- create_oolong(newsgroup_stm)
    y <- clone_oolong(x)
    x$lock(force = TRUE)
    expect_true(x$.__enclos_env__$private$finalized)
    expect_false(y$.__enclos_env__$private$finalized)
})
