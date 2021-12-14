require(shinytest)

SLEEPTIME <- 0.5

test_that("Click of death bug #51", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    test$click("confirm")
    Sys.sleep(SLEEPTIME)
    expect_error(test$getAllValues(), NA)
    first_ele <- test$findElement("div.radio")$getText()
    test$setValue("intruder", first_ele)
    test$click("confirm")
    Sys.sleep(SLEEPTIME)
    ## still moving after confirm
    expect_equal(test$getValue("current_topic"), "<strong>Topic  2 of 10 </strong>")
    test$finalize()
})

test_that("launchable", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$getAllValues(), NA)
    test$finalize()
    x <- wsi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$getAllValues(), NA)
    test$finalize()
    x <- gs(abstracts$text)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$getAllValues(), NA)
    test$finalize()
    x <- ti(abstracts_keyatm, abstracts$text)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$getAllValues(), NA)
    test$finalize()
})

test_that("Downloading", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    for (i in 1:10) {
        expect_equal("", test$getValue("download_button", "output"))
        expect_equal("", test$getValue("userid_entry", "output"))
        test$findElement("input[type='radio']")$click()
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    ## now the download button should be there
    expect_false("" == test$getValue("download_button", "output"))
    expect_false("" == test$getValue("userid_entry", "output"))
    test$setValue("userid", "shinytest")
    Sys.sleep(1)
    test$click("download_button")
    ## unfortunately we can't check this.
    test$finalize()
})

nextq <- function(test, k = 10) {
##    skip_on_cran()
    Sys.sleep(SLEEPTIME)
    test$click("nextq")
    Sys.sleep(SLEEPTIME)
    expect_equal(test$getValue("current_topic"), paste0("<strong>Topic  2 of ", k," </strong>"))
    for (i in 1:(k-1)) {
        first_ele <- test$findElement("div.radio")$getText()
        test$setValue("intruder", first_ele)
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    ## item 1 is not coded
    expect_equal(paste0("<strong>Topic  1 of ", k," </strong>"), test$getValue("current_topic"))
    test$click("nextq")
    Sys.sleep(SLEEPTIME)
    expect_equal(paste0("<strong>Topic  2 of ", k, "  [coded]</strong>"), test$getValue("current_topic"))
    test$click("ff")
    Sys.sleep(SLEEPTIME)
    ## It should go back to item 1
    expect_equal(paste0("<strong>Topic  1 of ", k, " </strong>"), test$getValue("current_topic"))
    first_ele <- test$findElement("div.radio")$getText()
    test$setValue("intruder", first_ele)
    test$click("confirm")
}

## "native" is based on abstracts_stm, thus k = 20

test_that("wi next q & ff (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    nextq(test)
    expect_error(test$getValue("done", "input"))
    test$finalize()
})

test_that("wi next q & ff (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- ShinyDriver$new("apps/wi")
    nextq(test, 20)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
    expect_error(test$getValue("done", "input"), NA)
    test$finalize()
})

test_that("wsi next q & ff (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- wsi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    nextq(test, 10)
    expect_error(test$getValue("done", "input"))
    test$finalize()
})

test_that("wsi next q & ff (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- ShinyDriver$new("apps/wsi")
    nextq(test, 20)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
    expect_error(test$getValue("done", "input"), NA)
    test$finalize()
})

test_ti <- function(test, k = 10) {
    Sys.sleep(SLEEPTIME)
    for (i in 1:k) {
        expect_equal(test$getValue("current_topic"), paste0("<strong>Case  ", i, " of ", k," </strong>"))
        expect_null(test$getValue("intruder"))
        test$findElement("input[type='radio']")$click()
        expect_false(is.null(test$getValue("intruder")))
        Sys.sleep(SLEEPTIME)
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    expect_equal(test$getValue("current_topic"), paste0("<strong>Case  1 of ",10,"  [coded]</strong>"))
}

test_that("ti (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- ti(abstracts_keyatm, abstracts$text, exact_n = 10)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    test_ti(test)
    expect_false("" == test$getValue("download_button", "output"))
    expect_false("" == test$getValue("userid_entry", "output"))
    expect_error(test$getValue("done", "input"))
    test$finalize()
})


# This ti native is only with k = 10
test_that("ti (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- ShinyDriver$new("apps/ti")
    test_ti(test, k = 10)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
    expect_error(test$getValue("done", "input"), NA)
    test$finalize()
})

test_gs <- function(test) {
    for (i in 1:5) {
        test$getValue("intruder")
        test$setValue("intruder", sample(1:5, 1), "input")
        Sys.sleep(SLEEPTIME)
        expect_equal(test$getValue("current_topic"), paste0("<strong>Case  ", i, " of 5 </strong>"))
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    expect_equal(test$getValue("current_topic"), "<strong>Case  1 of 5  [coded]</strong>")
}

test_that("gs (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- ShinyDriver$new("apps/gs")
    test_gs(test)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
    expect_error(test$getValue("done", "input"), NA)
    test$finalize()
})

test_that("gs (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- gs(abstracts$text, exact_n = 5)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    test_gs(test)
    expect_false("" == test$getValue("download_button", "output"))
    expect_false("" == test$getValue("userid_entry", "output"))
    expect_error(test$getValue("done", "input"))
    test$finalize()
})
