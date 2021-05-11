# gs_creation

    Code
      create_oolong(input_corpus = abstracts$text)
    Message <cliMessage>
      i An oolong test object (gold standard generation) with 25 cases, 0 coded.
      i Use the method $do_gold_standard_test() to generate gold standard.
      i Use the method $lock() to finalize this object and see the results.

# gs_turngold

    Code
      x <- create_oolong(input_corpus = abstracts$text)
      x$lock(force = TRUE)
      x$turn_gold()
    Output
      Corpus consisting of 25 documents and 1 docvar.
      text1 :
      "According to some researchers, particularly political econom..."
      
      text2 :
      "The third-person perception as it relates to the issue of te..."
      
      text3 :
      "This study examined the individual and combined effects of e..."
      
      text4 :
      "This article explores the communicative acts employed in the..."
      
      text5 :
      "A survey of 286 White and Black female college students exam..."
      
      text6 :
      "Using the risk perception attitude (RPA) framework and the 2..."
      
      [ reached max_ndoc ... 19 more documents ]
    Message <cliMessage>
      i Access the answer from the coding with quanteda::docvars(obj, 'answer')

