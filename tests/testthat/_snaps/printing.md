# gs_creation

    Code
      create_oolong(input_corpus = abstracts$text)
    Message <cliMessage>
      
      -- oolong (gold standard generation) -------------------------------------------
      i GS: n = 25, 0 coded.
      i Construct:  positive.
      
      -- Methods --
      
      * <$do_gold_standard_test()>: generate gold standard
      * <$lock()>: finalize this object and see the results

# gs_turngold

    Code
      x <- create_oolong(input_corpus = abstracts$text)
      x$lock(force = TRUE)
      x$turn_gold()
    Output
      Corpus consisting of 25 documents and 1 docvar.
      text1 :
      "This article explores the authors' experiences in creating a..."
      
      text2 :
      "Drawing on Social Representations Theory, this study investi..."
      
      text3 :
      "Until now, social marketing campaigns mainly targeted childr..."
      
      text4 :
      "Two studies investigated (a) how public figures' interaction..."
      
      text5 :
      "This study focuses on how Social Media Networks (SMN) have b..."
      
      text6 :
      "According to previous research conducted mainly in the Unite..."
      
      [ reached max_ndoc ... 19 more documents ]
    Message <cliMessage>
      i Access the answer from the coding with quanteda::docvars(obj, 'answer')

# check_calculation_topic_intrusion_multiobject (Printing)

    Code
      res
    Message <cliMessage>
      
      -- Summary (topic model): ------------------------------------------------------
      
      -- Word intrusion test --
      
      i Mean model precision: 1
      i Quantiles of model precision: 1, 1, 1, 1, 1
      i P-value of the model precision
      (H0: Model precision is not better than random guess): 0
      i Krippendorff's alpha: 1
      i K Precision:
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      
      -- Topic intrusion test --
      
      i Mean TLO: 0
      i Median TLO: 0
      i Quantiles of TLO: 0, 0, 0, 0, 0
      i P-Value of the median TLO 
      (H0: Median TLO is not better than random guess): 0

# ti only

    Code
      create_oolong(input_model = abstracts_stm, input_corpus = abstracts$text, type = "ti")
    Message <cliMessage>
      
      -- oolong (topic model) --------------------------------------------------------
      x WI v TI x WSI
      i TI: n = 25, 0 coded.
      
      -- Methods --
      
      * <$do_topic_intrusion_test()>: do topic intrusion test
      * <$lock()>: finalize and see the results

# wsi only

    Code
      create_oolong(input_model = abstracts_stm, input_corpus = abstracts$text, type = "wsi",
      wsi_n_top_terms = 100)
    Message <cliMessage>
      
      -- oolong (topic model) --------------------------------------------------------
      x WI x TI v WSI
      i WSI: n = 20, 0 coded.
      
      -- Methods --
      
      * <$do_word_set_intrusion_test()>: do word set intrusion test
      * <$lock()>: finalize and see the results

# check_calculation_wsi_multiobject (printing)

    Code
      res
    Message <cliMessage>
      
      -- Summary (topic model): ------------------------------------------------------
      
      -- Word set intrusion test --
      
      i Mean model precision: 0.916666666666667
      i K Precision:
      0.3, 0.7, 0.7, 0.7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
      i Krippendorff's alpha: 0.135

---

    Code
      res
    Message <cliMessage>
      
      -- Summary (topic model): ------------------------------------------------------
      
      -- Word set intrusion test --
      
      i Mean model precision: 1
      i K Precision:
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

# export printing

    Code
      export_oolong(obj1, dir = newdir, verbose = TRUE, use_full_path = FALSE)
    Message <cliMessage>
      i The Shiny has been written to the directory: ~/oolong_testing
      i You can test the app with: shiny::runApp("~/oolong_testing")

---

    Code
      export_oolong(obj1, dir = newdir, verbose = FALSE, use_full_path = FALSE)

