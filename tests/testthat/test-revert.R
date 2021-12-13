

test_that("case in vignette", {
    wsi_test <- readRDS(system.file("extdata", "wsi_test.RDS", package = "oolong"))
    expect_error(revert_oolong(wsi_test, system.file("extdata", "hadley.RDS", package = "oolong")), NA)
})


test_that("big_revert", {
    skip_if_not(dir.exists("../testdata/downloaded"))
    skip_if_not(file.exists("../testdata/massive_oolong.RDS"))
    massive_oolong <- readRDS("../testdata/massive_oolong.RDS")
    all_downloads_rds <- list.files("../testdata/downloaded", full.names = TRUE)
    expect_error(purrr::map(all_downloads_rds, ~revert_oolong(massive_oolong, .)), NA)
    all_res <- purrr::map(all_downloads_rds, ~revert_oolong(massive_oolong, .))
    expect_error(summarise_oolong(all_res[[1]], all_res[[2]], all_res[[3]], all_res[[4]], all_res[[5]], all_res[[6]]), NA)
})


test_that("verification unmatched test_content hash", {
    skip_if_not(file.exists("../testdata/massive_oolong.RDS"))
    massive_oolong <- readRDS("../testdata/massive_oolong.RDS")
    hadley <- system.file("extdata", "hadley.RDS", package = "oolong")
    ## no match
    expect_error(revert_oolong(massive_oolong, hadley))
})

test_that("cheating", {
    skip_if_not(dir.exists("../testdata/downloaded"))
    all_downloads_rds <- list.files("../testdata/downloaded", full.names = TRUE)
    cheater <- readRDS(all_downloads_rds[1])
    cheater$test_content$answer <- "wrong"
    saveRDS(cheater, "cheater.RDS")
    expect_error(revert_oolong(massive_oolong, cheater))
    unlink("cheater.RDS")
})

