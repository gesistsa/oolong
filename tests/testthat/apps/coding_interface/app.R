library(oolong)

set.seed(46709394)
test_content <- oolong:::.generate_test_content(abstracts_stm)

oolong:::.gen_shinyapp(test_content$word, oolong:::.UI_WORD_INTRUSION_TEST, oolong:::.ren_word_intrusion_test)
