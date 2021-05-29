library(oolong)

x <- gs(abstracts$text, exact_n = 5)
x$do_gold_standard_test()
