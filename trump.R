devtools::load_all()

test_content <- .generate_gold_standard(trump2k, exact_n = 30)

res <- .do_oolong_test(test_content, ui = .UI_GOLD_STANDARD_TEST, .ren = .ren_gold_standard_test)

