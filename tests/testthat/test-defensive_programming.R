##context("Defensive program")

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}

test_that("precondiction", {
    expect_error(create_oolong())
    expect_error(create_oolong(input_model = NULL, input_corpus = NULL))
    ## Don't use positional if one wants to create gold standard.
    expect_error(create_oolong(abstracts$text))
})

test_that("locking", {
    ## premature locking
    x <- create_oolong(abstracts_seededlda)
    expect_error(x$lock())
    expect_error(x$lock(force = TRUE), NA)
    x <- create_oolong(abstracts_seededlda, abstracts$text)
    expect_error(x$lock())
    ## error when only word intrusion test is done.
    x <- genius_word(x)
    expect_error(x$lock())
    x <- genius_topic(x)
    expect_error(x$lock(), NA)
})

test_that("cloning", {
    x <- create_oolong(abstracts_seededlda)
    expect_error(clone_oolong(x), NA)
    x <- genius_word(x)
    ## Cannot clone a partially coded object.
    expect_error(clone_oolong(x))
    x <- create_oolong(abstracts_seededlda)
    x$lock(force = TRUE)
    expect_error(clone_oolong(x))
    ## Cloned object is not sharing the same private space.
    x <- create_oolong(abstracts_seededlda)
    y <- clone_oolong(x)
    x$lock(force = TRUE)
    expect_true(x$.__enclos_env__$private$finalized)
    expect_false(y$.__enclos_env__$private$finalized)
})

test_that("cloning all types", {
    ## pure wi
    x <- wi(abstracts_seededlda)
    expect_error(clone_oolong(x), NA)
    x$.__enclos_env__$private$test_content$wi$answer[1] <- "x"
    expect_error(clone_oolong(x))
    ## pure ti
    x <- ti(abstracts_seededlda, abstracts$text)
    expect_error(clone_oolong(x), NA)
    x$.__enclos_env__$private$test_content$ti$answer[1] <- "x"
    expect_error(clone_oolong(x))
    ## witi
    x <- witi(abstracts_seededlda, abstracts$text)
    expect_error(clone_oolong(x), NA)
    x$.__enclos_env__$private$test_content$ti$answer[1] <- "x"
    expect_error(clone_oolong(x))
    x <- witi(abstracts_seededlda, abstracts$text)
    expect_error(clone_oolong(x), NA)
    x$.__enclos_env__$private$test_content$wi$answer[1] <- "x"
    expect_error(clone_oolong(x))
    x <- witi(abstracts_seededlda, abstracts$text)
    expect_error(clone_oolong(x), NA)
    x$.__enclos_env__$private$test_content$wi$answer[1] <- "x"
    x$.__enclos_env__$private$test_content$ti$answer[1] <- "x"
    expect_error(clone_oolong(x))
    ## wsi
    x <- wsi(abstracts_seededlda)
    expect_error(clone_oolong(x), NA)
    x$.__enclos_env__$private$test_content$wsi$answer[1] <- "x"
    expect_error(clone_oolong(x))
    ## gs
    x <- gs(abstracts$text)
    expect_error(clone_oolong(x), NA)
    x$.__enclos_env__$private$test_content$gs$answer[1] <- 1
    expect_error(clone_oolong(x))
})

test_that("Can't launch $do_topic_intrusion_test() when no test content", {
    x <- create_oolong(abstracts_seededlda)
    expect_error(x$do_topic_intrusion_test())
})

test_that("Can't launch $do_word_set_intrusion_test() when no test content", {
    x <- create_oolong(abstracts_seededlda)
    expect_error(x$do_word_set_intrusion_test())
})

test_that("Can't launch $do_word_intrusion_test() when no test content", {
    x <- wsi(abstracts_seededlda)
    expect_error(x$do_word_intrusion_test())
})

test_that("hash function", {
    expect_true(is.null(.safe_hash(NULL)))
    expect_type(.safe_hash(abstracts_seededlda), "character")
})

test_that("hash_input_model tm", {
    ## TI
    x <- create_oolong(abstracts_seededlda)
    expect_false(is.null(x$.__enclos_env__$private$hash_input_model))
    expect_equal(x$.__enclos_env__$private$hash_input_model, .safe_hash(abstracts_seededlda))
    ## WITI
    x <- create_oolong(abstracts_seededlda, abstracts$text)
    expect_false(is.null(x$.__enclos_env__$private$hash_input_model))
    expect_false(is.null(x$.__enclos_env__$private$hash_input_corpus))
    expect_equal(x$.__enclos_env__$private$hash_input_model, .safe_hash(abstracts_seededlda))
    expect_equal(x$.__enclos_env__$private$hash_input_corpus, .safe_hash(abstracts$text))
})

test_that("hash_input_corpus gs", {
    x <- create_oolong(input_corpus = abstracts$text)
    expect_true(is.null(x$.__enclos_env__$private$hash_input_model))
    expect_false(is.null(x$.__enclos_env__$private$hash_input_corpus))
    expect_equal(x$.__enclos_env__$private$hash_input_corpus, .safe_hash(abstracts$text))
})

test_that("invalid type", {
    expect_error(create_oolong(abstracts_seededlda, type = "1111"))
    expect_error(create_oolong(abstracts_seededlda, type = NA))
})

test_that("userid",  {
    expect_error(wi(abstracts_seededlda, userid = c("a", "b")))
    expect_error(wi(abstracts_seededlda, userid = "a"), NA)
    expect_error(wsi(abtracts_stm, abstracts$text))
})
