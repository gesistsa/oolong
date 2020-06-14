
# define a database user which has only write access but no readaccess
load("../data/abstracts_btm.Rda")
input_model<-abstracts_btm

o1<-create_oolong(input_model)

# modify this to be an app that takes the oolong object as input


o1$do_word_intrusion_test()

save(o1,file="oolong_object.Rdata")

o1$return_data()

#o1$do_word_intrusion_test()


test_content<-o1$get_test()$word

save(test_content,file="test.Rdata")