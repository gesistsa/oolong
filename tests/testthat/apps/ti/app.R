library(oolong)

x <- ti(abstracts_stm, abstracts$text, exact_n = 10)

x$do_topic_intrusion_test()
