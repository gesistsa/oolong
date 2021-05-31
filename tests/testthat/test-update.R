## check
test_that("check_oolong", {
    skip_on_cran()
    expect_error(check_oolong(readRDS("../testdata/oolong_tm_prev4.RDS")))
    expect_error(check_oolong(readRDS("../testdata/oolong_gs_prev4.RDS")))
    expect_error(check_oolong(readRDS("../testdata/oolong_gs_prev4_locked.RDS")))
    expect_error(check_oolong(readRDS("../testdata/massive_oolong_old.RDS")))
    expect_error(check_oolong(readRDS("../testdata/massive_oolong.RDS")), NA)
    expect_error(check_oolong(wi(abstracts_keyatm)), NA)
    expect_error(check_oolong(witi(abstracts_keyatm, abstracts$text)), NA)
    expect_error(check_oolong(wsi(abstracts_keyatm)), NA)
    expect_error(check_oolong(gs(abstracts$text)), NA)
})

test_that("update_oolong; needless update", {
    skip_on_cran()
    expect_error(update_oolong(wi(abstracts_keyatm)))
    expect_error(update_oolong(witi(abstracts_keyatm, abstracts$text)))
    expect_error(update_oolong(wsi(abstracts_keyatm)))
    expect_error(update_oolong(gs(abstracts$text)))
    expect_error(update_oolong(readRDS("../testdata/massive_oolong.RDS")))
})


test_that("update_oolong; pre v 0.3.23", {
    skip_on_cran()
    expect_silent(update_oolong(readRDS("../testdata/massive_oolong_old.RDS"), verbose = FALSE))
    expect_silent(update_oolong(readRDS("../testdata/massive_oolong_old.RDS"), verbose = TRUE))
    x <- update_oolong(readRDS("../testdata/massive_oolong_old.RDS"), verbose = FALSE)
    y <- readRDS("../testdata/massive_oolong_old.RDS")
    expect_equal(names(x$.__enclos_env__$private$test_content), "wi")
    expect_false(x$.__enclos_env__$private$hash == y$.__enclos_env__$private$hash) ## rehash after renaming
    expect_true(x$.__enclos_env__$private$hash_input_model == y$.__enclos_env__$private$hash_input_model)
    expect_true(is.null(x$.__enclos_env__$private$hash_input_corpus) == is.null(y$.__enclos_env__$private$hash_input_corpus))
    expect_true(x$.__enclos_env__$private$finalized == y$.__enclos_env__$private$finalized)
    expect_false(is.null(x$.__enclos_env__$private$meta))
})

test_that("update_oolong; without meta", {
    skip_on_cran()
    ## tm
    y <- readRDS("../testdata/oolong_tm_prev4.RDS")
    expect_silent(update_oolong(y, verbose = FALSE))
##    expect_warning(update_oolong(y))
    x <- update_oolong(y, verbose = FALSE)
    expect_equal(names(x$.__enclos_env__$private$test_content), c("wi", "ti"))
    expect_false(x$.__enclos_env__$private$hash == y$.__enclos_env__$private$hash) ## rehash after renaming
    expect_true(x$.__enclos_env__$private$finalized == y$.__enclos_env__$private$finalized)
    expect_false(is.null(x$.__enclos_env__$private$meta))
    expect_true(is.na(x$userid))
    expect_equal(class(x), class(y))
    ## gs; not locked
    y <- readRDS("../testdata/oolong_gs_prev4.RDS")
    expect_silent(update_oolong(y, verbose = FALSE))
##    expect_warning(update_oolong(y))
    x <- update_oolong(y, verbose = FALSE)
    expect_equal(names(x$.__enclos_env__$private$test_content), c("gs"))
    expect_false(x$.__enclos_env__$private$hash == y$.__enclos_env__$private$hash) ## rehash after renaming
    expect_true(x$.__enclos_env__$private$finalized == y$.__enclos_env__$private$finalized)
    expect_false(is.null(x$.__enclos_env__$private$meta))
    expect_true(is.na(x$userid))
    expect_equal(class(x), class(y))
    ## gs; locked
    y <- readRDS("../testdata/oolong_gs_prev4_locked.RDS")
    expect_silent(update_oolong(y, verbose = FALSE))
##    expect_warning(update_oolong(y))
    x <- update_oolong(y, verbose = FALSE)
    expect_equal(names(x$.__enclos_env__$private$test_content), c("gs"))
    expect_false(x$.__enclos_env__$private$hash == y$.__enclos_env__$private$hash) ## rehash after renaming
    expect_true(x$.__enclos_env__$private$finalized == y$.__enclos_env__$private$finalized)
    expect_false(is.null(x$.__enclos_env__$private$meta))
    expect_true(is.na(x$userid))
    expect_equal(class(x), class(y))    
})
