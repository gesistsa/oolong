---
title: 'oolong: An R package for validating  automated content analysis tools'
tags:
  - R
  - text analysis
  - topic model
  - sentiment analysis
  - validation
authors:
  - name: Chung-hong Chan
    orcid: 0000-0003-0872-7098
    affiliation: 1
  - name: Marius Sältzer
    orcid: 0000-0002-8604-4666
    affiliation: 1
affiliations:
 - name: Mannheimer Zentrum für Europäische Sozialforschung, Universität Mannheim
   index: 1
citation_author: Chan & Sältzer.
date: 15 October 2020
year: 2020
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Statement of need

Oolong is an R package providing functions for semantic validation of topic modeling and dictionary-based methods, two main tools for doing automated content analysis [@boumans2016taking;@gunther2016word]. 

While the validation of statistical properties of topics models is well established, the substantive meaning of categories uncovered is often less clear and their interpretation reliant on "intuition" or "eyeballing". As @chang2009reading [p. 1] put it: "qualitative evaluation of the latent space" or figuratively, reading tea leaves.

The story for dictionary-based methods is not better. Researchers usually assume these dictionaries have built-in validity and use them directly in their research. However, multiple validation studies [@boukes2020whatsthetone;@gonzalez2015signals;@ribeiro2016sentibench] demonstrate these dictionaries have very limited criterion validity.

Oolong provides a set of tools to objectively judge substantive interpretability to applied users in disciplines such as political science and communication science. It allows standardized content based testing of topic models as well as dictionary-based methods with clear numeric indicators of semantic validity. Oolong makes it easy to generate standard validation tests suggested by @chang2009reading and @song2020validations.

# Validation of automated content analysis

Validity is a requirement of content analysis [@krippendorff2018content; @neuendorf2016content]. Validation of automated methods has been called for by many scholars, e.g. @grimmer2013text; @ribeiro2016sentibench; @van2018communication. But how to validate these methods? The paper by @dimaggio2013exploiting conceptualizes validation of automated methods as three different operations and the three operations supplement each other. These three operations are: 1) *statistical* validation --to see if the model results agree with the assumptions of the model. Examples of statistical validation are calculation of pointwise mutual information, perplexity or semantic coherence of a topic model; 2) *semantic* validation --to see if the model results are semantically making sense. This procedure involves comparing model results with human judgment [@grimmer2011general]; 3) *predictive* validation --to see if the model results can predict external events [@quinn2010analyze]. For example, one can study whether external events can explain surges in attention to a topic extracted by a topic model.

This package focuses on semantic validation for three reasons: 
First, there is existing architecture for conducting statistical validation and predictive validation. Topic modeling packages such as `text2vec` [@selivanov2020tex2vec], `topicmodels` [@bettina2011topicmodels], and `textmineR` [@jones2019textminer] provide functions to calculate metrics such as perplexity and semantic coherence. Packages such as `stminsights` [@schwemmer2018stminsights] and `LDAvis` [@sievert2015ldavis] offer additional qualitative methods for predictive validation. As of writing, `tosca` [@koppers2020tosca] is the only package dealing with semantic validation. But the text-based interface might pose challenges to human annotators and it can only support topic models from the `lda` package [@change2015lda].

Second, results from statistical validation do not always agree with those from semantic validation. For example, a topic model with a lower perplexity does not have a better interpretability [@chang2009reading]. Of course, there are also metrics from statistical validation that are shown to be correlated with semantic validity, e.g. semantic coherence [@mimno2011optimizing]. But this correlation is also dependent on the text material. For example, @fan2019assessing show that semantic coherence is weakly correlated at best with human assessment, when the text material used for training a topic model has some frequent terms. But still, calculation of semantic coherence is recommended in the best practice paper by @maier2018applying. Nonetheless, conducting only statistical validation is not adequate because these three validation operations supplement each other.

Finally, predictive validation is dependent on research questions and thus it is difficult to be generalized as a reusable software framework. Additionally, the relationship between external (sociopolitical) events and the results from automated content analysis tools is usually what social scientists are eager to study, cf. using topic models for information retrieval [@yi2008evaluating]. We do not believe social scientists would ignore conducting any form of predictive validation.

Oolong focuses on semantic validation. The package provides the "human-in-the-loop" semantic validation procedures suggested by @chang2009reading and @song2020validations. The procedure proposed by @chang2009reading has been adopted in subsequent social science studies as the gold standard to validate topic models, e.g. @bohr2020reporting, @chuang2015topiccheck, and @miller2017australia. The procedure proposed by @song2020validations emphasizes both criterion validity and interrater reliability.

# Semantic validation of topic models

Topic models can be validated by word intrusion test and topic intrusion test [@chang2009reading]. In these tests, a human rater is asked to pick an odd word from a bunch of words (word intrusion test) or pick an odd topic from a bunch of topics for a document (topic intrusion test). Oolong provides an easy-to-use Shiny interface for these tests (Figure 1).

Currently, oolong supports a variety of topic models, e.g. structural topic models / correlated topic models from `stm` [@roberts2019stm], warp-LDA models from `text2vec` [@selivanov2020tex2vec], latent dirichlet allocation / correlated-topic models from `topicmodels` [@bettina2011topicmodels], biterm topic models from `BTM` [@wijffels2020btm] and keyword-assisted topic models from `keyATM` [@eshima2020keyatm].

For instance, `abstracts_stm` is a structural topic model trained with the text data from `abstracts$text` [@chan2020high].

\begin{figure}
\includegraphics[width=0.5\linewidth]{paper_files/fig1} \caption{A screenshot of word intrusion test}\label{fig:unnamed-chunk-1}
\end{figure}


```r
library(stm)
library(tibble)
library(dplyr)
library(quanteda)
library(oolong)
```


```r
abstracts_stm
```

```
## A topic model with 20 topics, 2500 documents and a 3998 word dictionary.
```

The function `create_oolong` creates a test object with both word intrusion test and topic intrusion test.


```r
oolong_test <- create_oolong(input_model = abstracts_stm,
                             input_corpus = abstracts$text)
oolong_test
```

```
## An oolong test object with k = 20, 0 coded.
## Use the method $do_word_intrusion_test() to do word intrusion test.
## With 25 cases of topic intrusion test. 0 coded.
## Use the method $do_topic_intrusion_test() to do topic intrusion test.
## Use the method $lock() to finalize this object and see the results.
```

The tests can be administered with methods `do_word_intrusion_test` and `do_topic_intrusion_test`.

```r
oolong_test$do_word_intrusion_test()
oolong_test$do_topic_intrusion_test()
```

After both tests has been done by a human rater, the test object must be locked and then accuracy metrics such as model precision (MP) and TLO (topic log odd) are displayed. 





```r
oolong_test$lock()
oolong_test
```

```
## An oolong test object with k = 20, 20 coded.
## 95%  precision
## With 25 cases of topic intrusion test. 25 coded.
## TLO: -0.135
```

The suggested workflow is to have at least two human raters to do the same set of tests. Test object can be cloned to allow multiple raters to do the test. More than one test object can be studied together using the function `summarize_oolong()`.


```r
oolong_test_rater1 <- create_oolong(abstracts_stm, abstracts$text)
oolong_test_rater2 <- clone_oolong(oolong_test_rater1)
```

```r
## Let rater 1 do the test.
oolong_test_rater1$do_word_intrusion_test()
oolong_test_rater1$do_topic_intrusion_test()
oolong_test_rater1$lock()

## Let rater 2 do the test.
oolong_test_rater2$do_word_intrusion_test()
oolong_test_rater2$do_topic_intrusion_test()
oolong_test_rater2$lock()
```



Get a summary of the two objects.




```r
summarize_oolong(oolong_test_rater1, oolong_test_rater2)
```

```
## Mean model precision: 0.3
## Quantiles of model precision: 0.25, 0.275, 0.3, 0.325, 0.35
## P-value of the model precision
##  (H0: Model precision is not better than random guess): 0.0494
## Krippendorff's alpha: 0.071
## K Precision:
## 0, 0, 0, 0, 0, 0.5, 1, 0, 0.5, 0, 0.5, 0, 0, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 1
## Mean TLO: -1.9
## Median TLO: -1.54
## Quantiles of TLO: -6.05, -3.56, -1.54, 0, 0
## P-Value of the median TLO 
## (H0: Median TLO is not better than random guess): 0.014
```

Two key indicators of semantic validity are mean model precision and median TLO. Please interpret the magnitude of the two values [see @chang2009reading] rather than the two statisical tests. The two statistical tests are testing whether the raters did better than random guess. Therefore, rejection of the null hypothesis is just the bare minimum of topic interpretability, *not* an indicator of adquate semantic validity of the topic model. Besides, please a very conservative significant level, e.g. alpha < 0.001.

# Semantic validation of dictionary-based methods

Dictionary-based methods such as AFINN [@nielsen2011new] can be validated by creating a gold standard dataset [@song2020validations]. Oolong provides a workflow for generating such gold standard dataset.

For example, you are interested in studying the sentiment of tweets from Donald Trump. `trump2k` is a random subset of 2,000 tweets from Donald Trump. And you would like to use AFINN to extract sentiment from these tweets. In this analysis, AFINN sentiment score is the *target value*.

A test object can be generated also with `create_oolong`. The argument `construct` should be an adjective, e.g. "positive" or "liberal".


```r
trump <- create_oolong(input_corpus = trump2k,
                       construct = "positive",
                       exact_n = 20)
trump
```

```
## An oolong test object (gold standard generation) with 20 cases, 0 coded.
## Use the method $do_gold_standard_test() to generate gold standard.
## Use the method $lock() to finalize this object and see the results.
```

Similarly, we suggest to have at least two human coders to do the same set of tests.


```r
trump2 <- clone_oolong(trump)
```

Instruct two coders to code the tweets and lock the objects.

```r
trump$do_gold_standard_test()
trump2$do_gold_standard_test()
trump$lock()
trump2$lock()
```



The method `turn_gold` converts a test object into a quanteda corpus [@benoit2018quanteda]. 


```r
gold_standard <- trump$turn_gold()
gold_standard
```

```
## Corpus consisting of 20 documents and 1 docvar.
## text1 :
## "Thank you Eau Claire, Wisconsin.  #VoteTrump on Tuesday, Apr..."
## 
## text2 :
## ""@bobby990r_1: @realDonaldTrump would lead polls the second ..."
## 
## text3 :
## ""@KdanielsK: @misstcassidy @AllAboutTheTea_ @realDonaldTrump..."
## 
## text4 :
## "Thank you for a great afternoon Birmingham, Alabama! #Trump2..."
## 
## text5 :
## ""@THETAINTEDT: @foxandfriends @realDonaldTrump Trump 2016 ht..."
## 
## text6 :
## "People believe CNN these days almost as little as they belie..."
## 
## [ reached max_ndoc ... 14 more documents ]
## Access the answer from the coding with quanteda::docvars(obj, 'answer')
```

This corpus can be used to calculate the target value, e.g. AFINN.


```r
dfm(gold_standard, remove_punct = TRUE) %>% dfm_lookup(afinn) %>%
    quanteda::convert(to = "data.frame") %>%
    mutate(matching_word_valence = (neg5 * -5) + (neg4 * -4) +
               (neg3 * -3) + (neg2 * -2) + (neg1 * -1) +
               (zero * 0) + (pos1 * 1) + (pos2 * 2) + (pos3 * 3) +
               (pos4 * 4) + (pos5 * 5),
           base = ntoken(gold_standard, remove_punct = TRUE),
           afinn_score = matching_word_valence / base) %>% 
		   pull(afinn_score) -> afinn_score
```

Summarize all oolong objects with the target value.


```r
res <- summarize_oolong(trump, trump2, target_value = afinn_score)
```

Printing the summary shows Krippendorff's Alpha, an indicator of interrater reliability. The validity metrics of a text analytic method can be tinted by poor interrater reliability of manual annotations [@song2020validations]. It is important to ensure high interrater reliability first.


```r
res
```

```
## Krippendorff's Alpha: 0.931
## Correlation: 0.744 (p = 2e-04)
## Effect of content length: -0.323 (p = 0.1643)
```

Additional diagnostic plots can also be displayed (Figure 2).


```r
plot(res)
```

![Diagnostic plots generated by oolong](paper_files/figure-latex/diagplot-1.pdf) 

The 4 subplots from left to right, top to bottom are: 

1. Correlation between human judgement and target value - A strong correlation between the two is an indicator of criterion validity of the target value.
2. Bland-Altman plot - If the dots are randomly scattering around the mean value (solid line), it is an indicator of good agreement between human judgement and the target value.
3. Correlation between target value and content length - If there is no strong correlation between the target value and content length, it is an indicator of robustness against the influence of content length [see @chan_4b].
4. Cook's distance of all data points - if there are only a few dots above the threshold (dotted line), it is an indicator of robustness against the influence of outliers.

# Acknowledgements

The development of oolong is partially supported by SAGE Concept Grant.

# References
