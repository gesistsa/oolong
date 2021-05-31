
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


test_that("preconditions witi", {
    expect_error(deploy_oolong(witi(abstracts_keyatm, abstracts$text)))
    expect_error(export_oolong(witi(abstracts_keyatm, abstracts$text)))
})

test_that("preconditions coded", {
    x <- wi(abstracts_keyatm)
    expect_error(deploy_oolong(x), NA)
    expect_error(export_oolong(x, verbose = FALSE), NA)
    x <- genius_word(x)
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x))
    x <- ti(abstracts_keyatm, abstracts$text)
    expect_error(deploy_oolong(x), NA)
    expect_error(export_oolong(x, verbose = FALSE), NA)
    x <- genius_topic(x)
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x, verbose = FALSE))
    x <- wsi(abstracts_keyatm)
    expect_error(deploy_oolong(x), NA)
    expect_error(export_oolong(x, verbose = FALSE), NA)
    x <- genius_wsi(x)
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x))
    x <- gs(abstracts$text)
    expect_error(deploy_oolong(x), NA)
    expect_error(export_oolong(x, verbose = FALSE), NA)
    x$.__enclos_env__$private$test_content$gs$answer <- 1
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x))
})

test_that("preconditions locked", {
    x <- wi(abstracts_keyatm)
    x$lock(force = TRUE)
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x))
    x <- ti(abstracts_keyatm, abstracts$text)
    x$lock(force = TRUE)
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x))
    x <- wsi(abstracts_keyatm)
    x$lock(force = TRUE)
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x))
    x <- gs(abstracts$text)
    x$lock(force = TRUE)
    expect_error(deploy_oolong(x))
    expect_error(export_oolong(x))
})

test_that("export_app dir", {
    ## new dir
    newdir <- paste0(tempdir(), "/test")
    expect_error(export_oolong(wsi(abstracts_keyatm), dir = newdir, verbose = FALSE), NA)
    ## existing dir
    olddir <- newdir
    expect_error(export_oolong(wsi(abstracts_keyatm), dir = olddir, verbose = FALSE), NA)
    unlink(olddir, recursive = TRUE)
})
