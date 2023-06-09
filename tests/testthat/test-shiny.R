## only do this on Linux
skip_on_os(c("windows", "mac", "solaris"))

require(shinytest2)

## To get this run:
## You'll need to install the Github version of chromate
## remotes::install_github("rstudio/chromate")
## If you want to use test$view()
## And Chrome or Chromium

SLEEPTIME <- 0.5

.get_html <- function(output, test) {
    as.character(test$get_value(output = output)$html)
}

test_that("Click of death bug #51", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    test$click("confirm")
    Sys.sleep(SLEEPTIME)
    expect_error(test$get_values(), NA)
    first_ele <- test$get_text("#intruder > div > div:nth-child(1) > label > span")
    test$set_inputs("intruder" = first_ele, timeout_ = SLEEPTIME * 30000)
    test$click("confirm")
    Sys.sleep(SLEEPTIME)
    ## still moving after confirm
    expect_equal(.get_html("current_topic", test), "<strong>Topic  2 of 10 </strong>")
    test$stop()
})

test_that("launchable", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$get_values(), NA)
    test$stop()
    x <- wsi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$get_values(), NA)
    test$stop()
    x <- gs(abstracts$text)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$get_values(), NA)
    test$stop()
    x <- ti(abstracts_keyatm, abstracts$text)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    Sys.sleep(SLEEPTIME)
    expect_error(test$get_values(), NA)
    test$stop()
})

test_that("Downloading", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    for (i in 1:10) {
        expect_null(test$get_value(output = "download_button"))
        expect_null(test$get_value(output = "userid_entry"))
        test$click(selector = "#intruder > div > div:nth-child(1) > label > input[type=radio]")
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    ## now the download button should be there
    expect_false(is.null(test$get_value(output = "download_button")))
    expect_false(is.null(test$get_value(output = "userid_entry")))
    test$set_inputs("userid" = "shinytest", timeout_ = SLEEPTIME * 30000)
    Sys.sleep(1)
    test$click(selector = "#download_button")
    ## unfortunately we can't check this.
    test$stop()
})

nextq <- function(test, k = 10) {
    Sys.sleep(SLEEPTIME)
    test$click("nextq")
    Sys.sleep(SLEEPTIME)
    expect_equal(.get_html("current_topic", test), paste0("<strong>Topic  2 of ", k," </strong>"))
    for (i in 1:(k-1)) {
        first_ele <- test$get_text("#intruder > div > div:nth-child(1) > label > span")
        test$set_inputs("intruder" = first_ele, timeout_ = SLEEPTIME * 30000)
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    ## item 1 is not coded
    expect_equal(.get_html("current_topic", test), paste0("<strong>Topic  1 of ", k," </strong>"))
    test$click("nextq")
    Sys.sleep(SLEEPTIME)
    expect_equal(.get_html("current_topic", test), paste0("<strong>Topic  2 of ", k, "  [coded]</strong>"))
    test$click("ff")
    Sys.sleep(SLEEPTIME)
    ## It should go back to item 1
    expect_equal(.get_html("current_topic", test), paste0("<strong>Topic  1 of ", k, " </strong>"))
    first_ele <- test$get_text("#intruder > div > div:nth-child(1) > label > span")
    test$set_inputs("intruder" = first_ele, timeout_ = SLEEPTIME * 30000)
    test$click("confirm")
}

test_that("wi next q & ff (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    nextq(test)
    test$stop()
})

test_that("wi next q & ff (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- AppDriver$new("apps/wi")
    nextq(test, 10)
    ## there should be no download button or userid_entry
    expect_true(is.null(test$get_value(output = "download_button")))
    expect_true(is.null(test$get_value(output = "userid_entry")))
    expect_false(is.null(test$get_value(input = "done")))
    test$stop()
})

test_that("wsi next q & ff (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- wsi(abstracts_keyatm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    nextq(test)
    test$stop()
})

test_that("wsi next q & ff (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- AppDriver$new("apps/wsi")
    nextq(test, 10)
    ## there should be no download button or userid_entry
    expect_true(is.null(test$get_value(output = "download_button")))
    expect_true(is.null(test$get_value(output = "userid_entry")))
    expect_false(is.null(test$get_value(input = "done")))
    test$stop()
})

test_ti <- function(test, k = 10) {
    Sys.sleep(SLEEPTIME)
    for (i in 1:k) {
        expect_equal(.get_html("current_topic", test), paste0("<strong>Case  ", i, " of ", k," </strong>"))
        expect_null(test$get_value(input = "intruder"))
        test$click(selector = "#intruder > div > div:nth-child(1) > label > input[type=radio]")
        ##test$findElement("input[type='radio']")$click()
        expect_false(is.null(test$get_value(input = "intruder")))
        Sys.sleep(SLEEPTIME)
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    expect_equal(.get_html("current_topic", test), paste0("<strong>Case  1 of ",k,"  [coded]</strong>"))
}

test_that("ti (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- ti(abstracts_keyatm, abstracts$text, exact_n = 10)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    test_ti(test)
    expect_false(is.null(test$get_value(output = "download_button")))
    expect_false(is.null(test$get_value(output = "userid_entry")))
    ##expect_error(test$getValue("done", "input"))
    test$stop()
})


# This ti native is only with k = 10
test_that("ti (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- AppDriver$new("apps/ti")
    test_ti(test, k = 10)
    ## there should be no download button or userid_entry
    expect_true(is.null(test$get_value(output = "download_button")))
    expect_true(is.null(test$get_value(output = "userid_entry")))
    expect_false(is.null(test$get_value(input = "done")))
    test$stop()
})

test_gs <- function(test) {
    for (i in 1:5) {
        ##test$getValue("intruder")
        test$set_inputs("intruder" = sample(1:5, 1), timeout_ = SLEEPTIME * 30000)
        Sys.sleep(SLEEPTIME)
        expect_equal(.get_html("current_topic", test), paste0("<strong>Case  ", i, " of 5 </strong>"))
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    expect_equal(.get_html("current_topic", test), "<strong>Case  1 of 5  [coded]</strong>")
}

test_that("gs (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- gs(abstracts$text, exact_n = 5)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- AppDriver$new(dir)
    test_gs(test)
    expect_false(is.null(test$get_value(output = "download_button")))
    expect_false(is.null(test$get_value(output = "userid_entry")))
    test$stop()
})


test_that("gs (native)", {
    skip_on_cran()
    skip_if_not(dir.exists("apps"))
    test <- AppDriver$new("apps/gs")
    test_gs(test)
    ## there should be no download button or userid_entry
    expect_true(is.null(test$get_value(output = "download_button")))
    expect_true(is.null(test$get_value(output = "userid_entry")))
    expect_false(is.null(test$get_value(input = "done")))
    test$stop()
})

