devtools::load_all()

oolong_test <- readRDS('trump_coded.RDS')

oolong_test$lock()

oolong_test$turn_gold()
