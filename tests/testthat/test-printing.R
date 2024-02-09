## context("Prining objects")

set.seed(46709394)

genius_word <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wi$answer <- obj1$.__enclos_env__$private$test_content$wi$intruder
    return(obj1)
}

genius_topic <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$ti$answer <- obj1$.__enclos_env__$private$test_content$ti$intruder
    return(obj1)
}

genius_wsi <- function(obj1) {
    obj1$.__enclos_env__$private$test_content$wsi$answer <- obj1$.__enclos_env__$private$test_content$wsi$intruder
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

test_that("check_calculation_topic_intrusion_multiobject (Printing)", {
    obj1 <- create_oolong(abstracts_seededlda, abstracts$text, exact_n = 10)
    obj2 <- clone_oolong(obj1)
    obj1 <- genius_word(obj1)
    obj1 <- genius_topic(obj1)
    obj1$lock()
    obj2 <- genius_word(obj2)
    obj2 <- genius_topic(obj2)
    obj2$lock()
    res <- summarize_oolong(obj1, obj2)
    expect_snapshot(res)
})

test_that("ti only", {
    expect_snapshot(create_oolong(input_model = abstracts_seededlda, input_corpus = abstracts$text, type = "ti"))
})

test_that("wsi only", {
    expect_snapshot(create_oolong(input_model = abstracts_seededlda, input_corpus = abstracts$text, type = "wsi", wsi_n_top_terms = 100))
})

test_that("check_calculation_wsi_multiobject (printing)", {
    obj1 <- wsi(abstracts_seededlda)
    obj2 <- clone_oolong(obj1)
    obj3 <- clone_oolong(obj1)
    ## Mocking coding
    obj1 <- genius_wsi(obj1)
    obj1$lock()
    obj2 <- genius_wsi(obj2)
    obj2$.__enclos_env__$private$test_content$wsi$answer[1] <- "wronganswer"
    obj2$lock()
    obj3 <- genius_wsi(obj3)
    obj3$.__enclos_env__$private$test_content$wsi$answer[1:4] <- "wronganswer"
    obj3$lock()
    res <- summarize_oolong(obj1, obj2, obj3)
    expect_snapshot(res)
    ### Single object
    res <- summarize_oolong(obj1)
    expect_snapshot(res)
})

test_that("export printing", {
    skip_on_cran()
    obj1 <- wsi(abstracts_seededlda)
    newdir <- "~/oolong_testing"
    expect_snapshot(export_oolong(obj1, dir = newdir, verbose = TRUE, use_full_path = FALSE))
    expect_snapshot(export_oolong(obj1, dir = newdir, verbose = FALSE, use_full_path = FALSE))
    unlink(newdir, recursive = TRUE)
})

test_that("update", {
    y <- readRDS("../testdata/oolong_tm_prev4.RDS")
    expect_snapshot(update_oolong(y))
    y <- readRDS("../testdata/oolong_gs_prev4.RDS")
    expect_snapshot(update_oolong(y))
    y <- readRDS("../testdata/oolong_gs_prev4_locked.RDS")
    expect_snapshot(update_oolong(y))
    y <- readRDS("../testdata/massive_oolong_old.RDS")
    expect_snapshot_error(print(y))
    expect_snapshot(update_oolong(y))
})
