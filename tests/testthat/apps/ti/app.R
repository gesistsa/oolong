library(oolong)

x <- ti(abstracts_keyatm, abstracts$text, exact_n = 10)

x$do_topic_intrusion_test()
