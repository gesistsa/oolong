require(shinytest)

SLEEPTIME <- 0.5

if (!dependenciesInstalled()) {
    installDependencies()
}

test_that("Click of death bug #51", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_stm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    test$click("confirm")
    expect_error(test$getAllValues(), NA)
    first_ele <- test$findElement("div.radio")$getText()
    test$setValue("intruder", first_ele)
    test$click("confirm")
    ## still moving after confirm
    expect_equal(test$getValue("current_topic"), "<strong>Topic  2 of 20 </strong>")
    test$finalize()
})

test_that("launchable", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_stm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    expect_error(test$getAllValues(), NA)
    test$finalize()
    x <- wsi(abstracts_stm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    expect_error(test$getAllValues(), NA)
    test$finalize()
    x <- gs(abstracts$text)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    expect_error(test$getAllValues(), NA)
    test$finalize()
    x <- ti(abstracts_stm, abstracts$text)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    expect_error(test$getAllValues(), NA)
    test$finalize()
})

test_that("Downloading", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_stm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    for (i in 1:20) {
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

nextq <- function(test) {
    Sys.sleep(SLEEPTIME)        
    test$click("nextq")
    Sys.sleep(SLEEPTIME)
    expect_equal(test$getValue("current_topic"), "<strong>Topic  2 of 20 </strong>")
    for (i in 1:19) {
        first_ele <- test$findElement("div.radio")$getText()
        test$setValue("intruder", first_ele)
        test$click("confirm")
        Sys.sleep(SLEEPTIME)        
    }
    ## item 1 is not coded
    expect_equal("<strong>Topic  1 of 20 </strong>", test$getValue("current_topic"))
    test$click("nextq")
    Sys.sleep(SLEEPTIME)
    expect_equal("<strong>Topic  2 of 20  [coded]</strong>", test$getValue("current_topic"))
    test$click("ff")
    Sys.sleep(SLEEPTIME)
    ## It should go back to item 1
    expect_equal("<strong>Topic  1 of 20 </strong>", test$getValue("current_topic"))
    first_ele <- test$findElement("div.radio")$getText()
    test$setValue("intruder", first_ele)
    test$click("confirm")
}

test_that("wi next q & ff (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- wi(abstracts_stm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    nextq(test)
    test$finalize()    
})

test_that("wi next q & ff (native)", {
    skip_on_cran()
    test <- ShinyDriver$new("apps/wi")
    nextq(test)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
    test$finalize()
})

test_that("wsi next q & ff (exported)", {
    skip_on_cran()
    dir <- tempdir()
    x <- wsi(abstracts_stm)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    nextq(test)
    test$finalize()    
})

test_that("wsi next q & ff (native)", {
    skip_on_cran()
    test <- ShinyDriver$new("apps/wsi")
    nextq(test)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
    test$finalize()
})

test_ti <- function(test) {
    Sys.sleep(SLEEPTIME)
    for (i in 1:10) {
        expect_equal(test$getValue("current_topic"), paste0("<strong>Case  ", i, " of 10 </strong>"))
        expect_null(test$getValue("intruder"))
        test$findElement("input[type='radio']")$click()
        expect_false(is.null(test$getValue("intruder")))
        Sys.sleep(SLEEPTIME)    
        test$click("confirm")
        Sys.sleep(SLEEPTIME)
    }
    expect_equal(test$getValue("current_topic"), "<strong>Case  1 of 10  [coded]</strong>")
}

test_that("ti (exported)", {
    dir <- tempdir()
    x <- ti(abstracts_stm, abstracts$text, exact_n = 10)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    test_ti(test)
    expect_false("" == test$getValue("download_button", "output"))
    expect_false("" == test$getValue("userid_entry", "output"))
    test$finalize()
})

test_that("ti (native)", {
    skip_on_cran()
    test <- ShinyDriver$new("apps/ti")
    test_ti(test)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
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
    test <- ShinyDriver$new("apps/gs")
    test_gs(test)
    ## there should be no download button or userid_entry
    expect_equal("", test$getValue("download_button", "output"))
    expect_equal("", test$getValue("userid_entry", "output"))
    test$finalize()
})

test_that("gs (exported)", {
    dir <- tempdir()
    x <- gs(abstracts$text, exact_n = 5)
    export_oolong(x, dir = dir, verbose = FALSE)
    test <- ShinyDriver$new(dir)
    test_gs(test)
    expect_false("" == test$getValue("download_button", "output"))
    expect_false("" == test$getValue("userid_entry", "output"))
    test$finalize()
})
