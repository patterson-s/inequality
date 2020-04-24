Library
=======

``` r
pacman::p_load(stm,
               dplyr,
               quanteda)
```

Ingest
======

Since the speecehs are saved as individual .txt files, I use the `quanteda` approach to import the text. Output: `corpus_1`, raw text files with unordered covariate metadata.

Citation: Mikhaylov, Slava; Baturo, Alexander; Dasandi, Niheer, 2017, "United Nations General Debate Corpus", <doi:10.7910/DVN/0TJX8Y>, Harvard Dataverse, V3

``` r
# Read using Quanteda ----
corpus_1 <- readtext::readtext("/Users/ScottPatterson/OneDrive - McGill University/patterson_pouliot/inequality/inequality/unga_speeches", docvarsfrom = c("filenames"), dvsep = "_")
```

Pre-processing
==============

This step removes stop words, numbers, punctuation, stems the words, converts everything to lowercase. Use the `textProcessor` function. Output: corpus\_2

``` r
# Specify metadata from corpus ----
corpus_1_meta <- data.frame(corpus_1[,c(1,3:5)])

# textProcessor ----
corpus_2 <- textProcessor(documents = corpus_1$text,metadata = corpus_1_meta)
```

    ## Building corpus... 
    ## Converting to Lower Case... 
    ## Removing punctuation... 
    ## Removing stopwords... 
    ## Removing numbers... 
    ## Stemming... 
    ## Creating Output...

Remove Infrequent Words
=======================

`plotRemoved` Removing infrequently used words. This function will allow us to see how many words would be removed at different threshholds.

``` r
# plotRemoved: test different thresholds for infrequent word removal ----

plotRemoved(corpus_2$documents, lower.thresh = seq(1,200, by = 25))
```

![](inequality_stm_files/figure-markdown_github/Plot%20Removed%20Word%20thresholds-1.png)

None of the documents are dropped at the 200 word threshhold. I can try to specify different models at different levels of sensitivity. The sharpest acceleration occurs between 0 - 25, gradually tapering off around 50 and eventually flattening out.

Removing too many words may exclude important but infrequently appearing key words. Removing too few words will XXXX

Start with a lower threshold, later specifications at higher thresholds.

`prepDocuments` lower.thresh = 15 Output: corpus\_3

``` r
#prepDocuments ----

corpus_3 <- prepDocuments(corpus_2$documents, corpus_2$vocab, corpus_2$meta, lower.thresh = 15)
```

    ## Removing 41189 of 50938 terms (101505 of 5169003 tokens) due to frequency 
    ## Your corpus now has 7507 documents, 9749 terms and 5067498 tokens.

COVARIATES
==========

P5 = USA, FRA, CHN, GBR, RUS -The authors of the dataset code Soviet Union as Russia

``` r
# P5 ----
corpus_4 <- mutate(corpus_1, P5 = ifelse(docvar1 == "FRA", 1,
                                         ifelse(docvar1 == "USA", 1,
                                                ifelse(docvar1 == "GBR",1,
                                                       ifelse(docvar1 == "CHN",1,
                                                              ifelse(docvar1 == "RUS",1,0))))))
```

Nuclear Weapon States: NWS P5 India (1974) Pakistan (1998) North Korea (2006) Israel (unknown; 1966)

``` r
#NWS
corpus_4$NWS <- NA
corpus_4 <- corpus_4 %>%
  mutate(
    NWS = case_when(
      P5 == 1 ~ 1,
      docvar1 == "IND" & docvar3 > 1973 ~ 1,
      docvar1 == "PAK" & docvar3 > 1997 ~ 1,
      docvar1 == "PRK" & docvar3 > 2005 ~ 1,
      docvar1 == "ISR" & docvar3 > 1965 ~ 1,
      is.na(NWS) ~ 0
    )
  )
```

ECOWAS: <https://en.wikipedia.org/wiki/Treaty_of_Lagos>

``` r
# ECOWAS
corpus_4$ECOWAS <- NA
corpus_4 <- corpus_4 %>%
  mutate(
    ECOWAS = case_when(
      docvar1 == "BEN" & docvar3 > 1974 ~ 1,
        docvar1 == "BFA" & docvar3 > 1974 ~ 1,
        docvar1 == "CPV" & docvar3 > 1974 ~ 1,
        docvar1 == "GMB" & docvar3 > 1974 ~ 1,
        docvar1 == "GHA" & docvar3 > 1974 ~ 1,
        docvar1 == "GNB" & docvar3 > 1974 ~ 1,
        docvar1 == "LBR" & docvar3 > 1974 ~ 1,
        docvar1 == "MLI" & docvar3 > 1974 ~ 1,
        docvar1 == "WAN" & docvar3 > 1974 ~ 1,
        docvar1 == "SEN" & docvar3 > 1974 ~ 1,
        docvar1 == "SLE" & docvar3 > 1974 ~ 1,
        docvar1 == "TGO" & docvar3 > 1974 ~ 1,
        docvar1 == "GIN" & docvar3 > 1974 & docvar3 < 2009 ~ 1,
        docvar1 == "NER" & docvar3 > 1974 & docvar3 < 2010 ~ 1,
        docvar1 == "CIV" & docvar3 > 1974 & docvar3 < 2011 ~ 1,
        is.na(ECOWAS) ~ 0
    )
  )
```

EU: Maastricht 1993

``` r
#  ----
corpus_4$EU <-NA
corpus_4 <- corpus_4 %>%
  mutate(
    EU = case_when(
      docvar1 == "BEL" & docvar3 > 1970 ~ 1,
      docvar1 == "ITA" & docvar3 > 1970 ~ 1,
      docvar1 == "LUX" & docvar3 > 1970 ~ 1,
      docvar1 == "FRA" & docvar3 > 1970 ~ 1,
      docvar1 == "NLD" & docvar3 > 1970 ~ 1,
      docvar1 == "DEU" & docvar3 > 1970 ~ 1,
      docvar1 == "DNK" & docvar3 > 1972 ~ 1,
      docvar1 == "IRL" & docvar3 > 1972 ~ 1,
      docvar1 == "GBR" & docvar3 > 1972 ~ 1,
      docvar1 == "GRC" & docvar3 > 1981 ~ 1,
      docvar1 == "PRT" & docvar3 > 1985 ~ 1,
      docvar1 == "ESP" & docvar3 > 1985 ~ 1,
      docvar1 == "AUT" & docvar3 > 1994 ~ 1,
      docvar1 == "FIN" & docvar3 > 1994 ~ 1,
      docvar1 == "SWE" & docvar3 > 1994 ~ 1,
      docvar1 == "CYP" & docvar3 > 2003 ~ 1,
      docvar1 == "CZE" & docvar3 > 2003 ~ 1,
      docvar1 == "EST" & docvar3 > 2003 ~ 1,
      docvar1 == "HUN" & docvar3 > 2003 ~ 1,
      docvar1 == "LVA" & docvar3 > 2003 ~ 1,
      docvar1 == "LTU" & docvar3 > 2003 ~ 1,
      docvar1 == "MLT" & docvar3 > 2003 ~ 1,
      docvar1 == "POL" & docvar3 > 2003 ~ 1,
      docvar1 == "SVK" & docvar3 > 2003 ~ 1,
      docvar1 == "SVN" & docvar3 > 2003 ~ 1,
      docvar1 == "BGR" & docvar3 > 2006 ~ 1,
      docvar1 == "ROU" & docvar3 > 2006 ~ 1,
      docvar1 == "HRV" & docvar3 > 2012 ~ 1,
      is.na(EU) ~ 0))

#corpus_4 <- corpus_4 %>%
#  mutate(EU = case_when(is.na(EU) ~ 0))
```

CTBT: <https://en.wikipedia.org/wiki/List_of_parties_to_the_Comprehensive_Nuclear-Test-Ban_Treaty>

Preliminary Models
==================

``` r
corpus_5 <- corpus_4

# corpus_5 meta ----
corpus_5_meta <- data.frame(corpus_5[,c(1,3:6)])

# corpus_5 textProcessor ----
corpus_5_pro <- textProcessor(documents = corpus_5$text,metadata = corpus_5_meta)
```

    ## Building corpus... 
    ## Converting to Lower Case... 
    ## Removing punctuation... 
    ## Removing stopwords... 
    ## Removing numbers... 
    ## Stemming... 
    ## Creating Output...

``` r
# corpus_5 @ 5tokens prepDocuments ----
corpus_5_prep <- prepDocuments(corpus_5_pro$documents, corpus_5_pro$vocab, corpus_5_pro$meta, lower.thresh = 5)
```

    ## Removing 36463 of 50938 terms (57434 of 5169003 tokens) due to frequency 
    ## Your corpus now has 7507 documents, 14475 terms and 5111569 tokens.

``` r
# Estimate STM ----

prev_1 <- stm(documents = corpus_5_prep$documents, 
              vocab = corpus_5_prep$vocab, 
              K = 18, 
              prevalence = ~P5, 
              data = corpus_5_prep$meta, 
              init.type = "Spectral")
```

    ## Beginning Spectral Initialization 
    ##   Calculating the gram matrix...
    ##   Using only 10000 most frequent terms during initialization...
    ##   Finding anchor words...
    ##      ..................
    ##   Recovering initialization...
    ##      ....................................................................................................
    ## Initialization complete.
    ## ....................................................................................................
    ## Completed E-Step (24 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 1 (approx. per word bound = -7.280) 
    ## ....................................................................................................
    ## Completed E-Step (20 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 2 (approx. per word bound = -7.210, relative change = 9.527e-03) 
    ## ....................................................................................................
    ## Completed E-Step (18 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 3 (approx. per word bound = -7.196, relative change = 1.968e-03) 
    ## ....................................................................................................
    ## Completed E-Step (21 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 4 (approx. per word bound = -7.190, relative change = 9.001e-04) 
    ## ....................................................................................................
    ## Completed E-Step (17 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 5 (approx. per word bound = -7.186, relative change = 5.180e-04) 
    ## Topic 1: state, peac, intern, unit, nation 
    ##  Topic 2: develop, nation, unit, countri, global 
    ##  Topic 3: peopl, countri, state, unit, nation 
    ##  Topic 4: nation, unit, will, world, state 
    ##  Topic 5: nation, unit, human, intern, right 
    ##  Topic 6: intern, secur, peac, countri, nation 
    ##  Topic 7: countri, nation, intern, peac, develop 
    ##  Topic 8: intern, nation, peac, israel, unit 
    ##  Topic 9: countri, develop, intern, nation, africa 
    ##  Topic 10: nation, unit, develop, world, intern 
    ##  Topic 11: intern, nation, unit, will, countri 
    ##  Topic 12: world, nation, peopl, will, must 
    ##  Topic 13: countri, intern, develop, nation, econom 
    ##  Topic 14: nation, countri, will, state, peopl 
    ##  Topic 15: nation, peac, unit, develop, intern 
    ##  Topic 16: intern, nation, unit, develop, secur 
    ##  Topic 17: nation, world, will, develop, countri 
    ##  Topic 18: will, world, nuclear, can, countri 
    ## ....................................................................................................
    ## Completed E-Step (18 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 6 (approx. per word bound = -7.184, relative change = 3.325e-04) 
    ## ....................................................................................................
    ## Completed E-Step (19 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 7 (approx. per word bound = -7.182, relative change = 2.339e-04) 
    ## ....................................................................................................
    ## Completed E-Step (18 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 8 (approx. per word bound = -7.181, relative change = 1.744e-04) 
    ## ....................................................................................................
    ## Completed E-Step (15 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 9 (approx. per word bound = -7.180, relative change = 1.360e-04) 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 10 (approx. per word bound = -7.179, relative change = 1.094e-04) 
    ## Topic 1: state, peac, intern, unit, nation 
    ##  Topic 2: develop, nation, countri, unit, global 
    ##  Topic 3: peopl, countri, unit, state, nation 
    ##  Topic 4: nation, unit, state, world, will 
    ##  Topic 5: nation, unit, human, right, intern 
    ##  Topic 6: intern, secur, peac, countri, state 
    ##  Topic 7: countri, intern, peac, develop, nation 
    ##  Topic 8: israel, peac, intern, nation, unit 
    ##  Topic 9: countri, develop, intern, africa, nation 
    ##  Topic 10: nation, unit, develop, world, intern 
    ##  Topic 11: intern, will, unit, nation, countri 
    ##  Topic 12: peopl, world, will, nation, must 
    ##  Topic 13: countri, intern, develop, econom, nation 
    ##  Topic 14: nation, countri, unit, state, will 
    ##  Topic 15: nation, peac, develop, unit, africa 
    ##  Topic 16: intern, nation, unit, develop, secur 
    ##  Topic 17: world, nation, develop, intern, will 
    ##  Topic 18: will, can, world, year, nuclear 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 11 (approx. per word bound = -7.178, relative change = 8.897e-05) 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 12 (approx. per word bound = -7.178, relative change = 7.475e-05) 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 13 (approx. per word bound = -7.177, relative change = 6.527e-05) 
    ## ....................................................................................................
    ## Completed E-Step (22 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 14 (approx. per word bound = -7.177, relative change = 5.979e-05) 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 15 (approx. per word bound = -7.176, relative change = 5.253e-05) 
    ## Topic 1: state, peac, intern, unit, peopl 
    ##  Topic 2: develop, nation, countri, unit, global 
    ##  Topic 3: peopl, countri, unit, state, struggl 
    ##  Topic 4: nation, unit, state, world, intern 
    ##  Topic 5: nation, unit, human, intern, secur 
    ##  Topic 6: intern, secur, peac, countri, state 
    ##  Topic 7: countri, intern, peac, develop, african 
    ##  Topic 8: israel, peac, nation, intern, arab 
    ##  Topic 9: countri, intern, develop, africa, south 
    ##  Topic 10: nation, unit, develop, peac, countri 
    ##  Topic 11: intern, will, unit, nation, peac 
    ##  Topic 12: peopl, world, will, nation, must 
    ##  Topic 13: countri, intern, develop, econom, peac 
    ##  Topic 14: nation, countri, unit, state, will 
    ##  Topic 15: nation, peac, develop, africa, countri 
    ##  Topic 16: intern, nation, unit, develop, secur 
    ##  Topic 17: world, develop, nation, intern, countri 
    ##  Topic 18: will, can, world, year, one 
    ## ....................................................................................................
    ## Completed E-Step (14 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 16 (approx. per word bound = -7.176, relative change = 4.463e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 17 (approx. per word bound = -7.176, relative change = 3.979e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 18 (approx. per word bound = -7.176, relative change = 3.619e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 19 (approx. per word bound = -7.175, relative change = 3.335e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 20 (approx. per word bound = -7.175, relative change = 3.053e-05) 
    ## Topic 1: state, peac, unit, intern, nuclear 
    ##  Topic 2: develop, nation, countri, unit, state 
    ##  Topic 3: peopl, countri, unit, struggl, state 
    ##  Topic 4: nation, unit, state, intern, world 
    ##  Topic 5: unit, nation, human, secur, intern 
    ##  Topic 6: intern, secur, peac, countri, state 
    ##  Topic 7: countri, peac, intern, african, develop 
    ##  Topic 8: israel, peac, nation, intern, state 
    ##  Topic 9: countri, intern, develop, africa, south 
    ##  Topic 10: nation, develop, unit, peac, countri 
    ##  Topic 11: will, intern, unit, peac, communiti 
    ##  Topic 12: peopl, world, will, nation, must 
    ##  Topic 13: countri, intern, develop, peac, govern 
    ##  Topic 14: nation, unit, countri, state, will 
    ##  Topic 15: nation, develop, peac, africa, countri 
    ##  Topic 16: intern, nation, unit, develop, countri 
    ##  Topic 17: world, develop, intern, nation, countri 
    ##  Topic 18: will, can, world, one, year 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 21 (approx. per word bound = -7.175, relative change = 2.771e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 22 (approx. per word bound = -7.175, relative change = 2.591e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 23 (approx. per word bound = -7.175, relative change = 2.391e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 24 (approx. per word bound = -7.174, relative change = 2.226e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 25 (approx. per word bound = -7.174, relative change = 2.044e-05) 
    ## Topic 1: state, peac, unit, intern, nuclear 
    ##  Topic 2: develop, nation, countri, state, global 
    ##  Topic 3: peopl, countri, unit, struggl, state 
    ##  Topic 4: nation, unit, state, intern, europ 
    ##  Topic 5: unit, nation, secur, intern, human 
    ##  Topic 6: intern, secur, peac, state, countri 
    ##  Topic 7: countri, peac, intern, african, develop 
    ##  Topic 8: israel, peac, nation, intern, state 
    ##  Topic 9: countri, intern, develop, africa, south 
    ##  Topic 10: nation, develop, unit, countri, peac 
    ##  Topic 11: will, intern, unit, peac, communiti 
    ##  Topic 12: peopl, world, will, nation, must 
    ##  Topic 13: countri, intern, peac, govern, develop 
    ##  Topic 14: nation, unit, countri, state, will 
    ##  Topic 15: nation, develop, peac, africa, countri 
    ##  Topic 16: intern, nation, develop, unit, countri 
    ##  Topic 17: world, develop, intern, nation, countri 
    ##  Topic 18: will, can, world, one, year 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 26 (approx. per word bound = -7.174, relative change = 1.918e-05) 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 27 (approx. per word bound = -7.174, relative change = 1.752e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 28 (approx. per word bound = -7.174, relative change = 1.697e-05) 
    ## ....................................................................................................
    ## Completed E-Step (12 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 29 (approx. per word bound = -7.174, relative change = 1.559e-05) 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 30 (approx. per word bound = -7.174, relative change = 1.419e-05) 
    ## Topic 1: state, peac, unit, intern, nuclear 
    ##  Topic 2: develop, nation, countri, state, global 
    ##  Topic 3: peopl, countri, unit, struggl, state 
    ##  Topic 4: nation, unit, state, intern, right 
    ##  Topic 5: unit, nation, secur, intern, human 
    ##  Topic 6: intern, secur, peac, state, countri 
    ##  Topic 7: countri, peac, intern, african, develop 
    ##  Topic 8: israel, peac, nation, state, intern 
    ##  Topic 9: countri, intern, africa, develop, south 
    ##  Topic 10: nation, develop, unit, countri, peac 
    ##  Topic 11: will, intern, unit, peac, communiti 
    ##  Topic 12: peopl, world, will, nation, human 
    ##  Topic 13: countri, intern, govern, peac, america 
    ##  Topic 14: nation, unit, will, state, countri 
    ##  Topic 15: nation, develop, peac, africa, countri 
    ##  Topic 16: intern, nation, develop, unit, countri 
    ##  Topic 17: world, develop, intern, nation, countri 
    ##  Topic 18: will, can, world, one, unit 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 31 (approx. per word bound = -7.174, relative change = 1.376e-05) 
    ## ....................................................................................................
    ## Completed E-Step (15 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 32 (approx. per word bound = -7.173, relative change = 1.268e-05) 
    ## ....................................................................................................
    ## Completed E-Step (15 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 33 (approx. per word bound = -7.173, relative change = 1.163e-05) 
    ## ....................................................................................................
    ## Completed E-Step (13 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 34 (approx. per word bound = -7.173, relative change = 1.124e-05) 
    ## ....................................................................................................
    ## Completed E-Step (14 seconds). 
    ## Completed M-Step. 
    ## Completing Iteration 35 (approx. per word bound = -7.173, relative change = 1.050e-05) 
    ## Topic 1: state, peac, unit, intern, nuclear 
    ##  Topic 2: develop, nation, countri, state, island 
    ##  Topic 3: peopl, countri, unit, struggl, state 
    ##  Topic 4: nation, unit, state, intern, right 
    ##  Topic 5: unit, nation, secur, intern, human 
    ##  Topic 6: intern, peac, secur, state, countri 
    ##  Topic 7: countri, peac, intern, african, develop 
    ##  Topic 8: israel, peac, nation, state, unit 
    ##  Topic 9: countri, intern, africa, develop, south 
    ##  Topic 10: nation, develop, unit, countri, peac 
    ##  Topic 11: will, unit, intern, peac, communiti 
    ##  Topic 12: peopl, world, will, nation, human 
    ##  Topic 13: countri, intern, govern, peac, state 
    ##  Topic 14: nation, unit, will, state, countri 
    ##  Topic 15: nation, develop, peac, africa, countri 
    ##  Topic 16: intern, nation, develop, unit, countri 
    ##  Topic 17: world, develop, intern, countri, nation 
    ##  Topic 18: will, world, can, one, countri 
    ## ....................................................................................................
    ## Completed E-Step (17 seconds). 
    ## Completed M-Step. 
    ## Model Converged

Preliminary Analysis
====================

``` r
# Base topic names ----
topicNames_18 <-c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", "Topic 11", "Topic 12", "Topic 13", "Topic 14", "Topic 15", "Topic 16", "Topic 17", "Topic 18")
```

Label Topics
============

-   Go back through and filter out country names

-   The words are displayed according to 4 metrics. Each represents a different mathematical weighting scheme for assigning words to topics

-   Highest Prob: word frequency
-   FREX: word frequency weigted w/ topic exclusivity
-   LIFT: another form of weighting; gives higher probability to words that don't appear in other topics
-   SCORE: similar to LIFT, but on log scale

``` r
labelTopics(prev_1, n = 20)
```

    ## Topic 1 Top Words:
    ##       Highest Prob: state, peac, unit, nuclear, intern, nation, peopl, countri, republ, weapon, secur, soviet, world, disarma, relat, develop, arm, general, forc, union 
    ##       FREX: soviet, german, socialist, nuclear, detent, prohibit, disarma, outer, race, space, weapon, armament, missil, mongolian, poland, germani, korean, ussr, warsaw, korea 
    ##       Lift: ssr, anti-hitl, ceausescu, erich, forward-bas, honeck, husak, ilyich, jaruzelski, juch, presidium, shcherbitski, songun, soviet-french, soviet-indian, todor, wojciech, zhivkov, binari, jong 
    ##       Score: soviet, co-oper, nuclear, ssr, mongolian, detent, socialist, romania, disarma, europ, german, byelorussian, ukrainian, poland, republ, favor, weapon, czechoslovak, ussr, state 
    ## Topic 2 Top Words:
    ##       Highest Prob: develop, nation, countri, state, island, global, unit, small, climat, chang, intern, will, govern, sustain, continu, support, peopl, econom, must, world 
    ##       FREX: barbado, tobago, fiji, sid, papua, trinidad, bahama, island, solomon, pacif, saint, caribbean, vulner, emiss, mauritius, caricom, dominica, small, hurrican, climat 
    ##       Lift: acidif, aosi, banana-produc, carbon-neg, fiji’, honiara, mangrov, marshalles, ncds, nouméa, oceanscap, oec, pan-caribbean, tobago’, tonga’, vanuatu’, waigani, sid, apia, barbados’ 
    ##       Score: barbado, sid, mdgs, island, tobago, bahama, fiji, trinidad, papua, pacif, caribbean, post-, develop, emiss, caricom, grenada, dominica, solomon, millennium, maldiv 
    ## Topic 3 Top Words:
    ##       Highest Prob: peopl, countri, unit, struggl, state, independ, nation, world, aggress, republ, support, forc, peac, will, govern, intern, imperialist, liber, viet, imperi 
    ##       FREX: imperi, imperialist, viet, nam, vietnames, revolutionari, lao, kampuchea, reactionari, victori, fascist, colonialist, cuban, khmer, struggl, chines, heroic, cuba, capitalist, neo-coloni 
    ##       Lift: hoxha, anti-imperi, battambang, enver, glorieus, imperialist-zionist, kerekou, social-imperi, social-imperialist, azanian, kompong, sari, lon, nol, guevara, fretilin, heng, samrin, independent, shabbi 
    ##       Score: imperialist, viet, kampuchea, imperi, nam, vietnames, lao, peopl, zionist, racist, albania, kampuchean, struggl, colonialist, reactionari, coloni, social-imperialist, albanian, cuba, aggress 
    ## Topic 4 Top Words:
    ##       Highest Prob: nation, unit, state, intern, will, right, new, europ, cooper, european, world, region, secur, countri, bosnia, peac, war, conflict, organ, polit 
    ##       FREX: bosnia, herzegovina, croatia, latvia, forty-eighth, yugoslavia, estonia, georgia, kosovo, serbia, lithuania, ethnic, boutros-ghali, russian, peace-keep, boutro, balkan, albania, baltic, former 
    ##       Lift: metohija, abkhaz, non-albanian, ossetian, post-communist, republika, riga, serbia’, srpska, tallinn, mladic, ratko, georgian, vojvodina, danubian, estonian, lithuanian, croatian, tskhinvali, sukhumi 
    ##       Score: bosnia, herzegovina, croatia, kosovo, albania, latvia, serbia, forty-eighth, peace-keep, albanian, csce, georgia, estonia, europ, georgian, serb, macedonia, andorra, ukrain, boutros-ghali 
    ## Topic 5 Top Words:
    ##       Highest Prob: unit, nation, secur, intern, human, must, right, council, organ, develop, state, will, member, need, work, reform, also, respons, commit, support 
    ##       FREX: peacekeep, court, reform, oper, prolifer, canada, statut, non-prolifer, council, finland, mandat, personnel, humanitarian, convent, ratifi, women, sweden, crimin, human, crime 
    ##       Lift: luxembourg’, ahern, berti, monaco’, rainier, todayâ, olara, people-smuggl, belgium’, subsidiar, otunnu, iceland’, secretary-generalâ, cost-cut, cross-sector, amsterdam, ireland’, “respons, austria’, protect” 
    ##       Score: peacekeep, millennium, unit, reform, nation, human, council, global, marino, must, peacebuild, landmin, anti-personnel, iceland, annan, secur, kofi, council’, right, monaco 
    ## Topic 6 Top Words:
    ##       Highest Prob: intern, peac, secur, state, countri, region, peopl, nation, develop, effort, unit, will, arab, achiev, also, council, iraq, call, resolut, palestinian 
    ##       FREX: yemen, kuwait, iraqi, morocco, libya, iraq, tunisia, emir, libyan, syrian, bahrain, sudan, arab, kingdom, gulf, saudi, arabia, qatar, oman, brother 
    ##       Lift: abdrabuh, abidin, al-jab, alija, arab-islam, emirates’, emirati, hamad, mansour, mousa, qaboo, tumb, tunb, al-saud, egypt’, gcc, hadi, shaikh, yemen’, hanish 
    ##       Score: arab, yemen, kuwait, emir, iraqi, morocco, libyan, iraq, sudan, syrian, tunisia, islam, oman, tunb, sultan, libya, bahrain, iraq’, yemeni, palestinian 
    ## Topic 7 Top Words:
    ##       Highest Prob: countri, peac, intern, african, develop, peopl, africa, will, organ, nation, republ, communiti, state, govern, particular, world, situat, presid, must, like 
    ##       FREX: chad, niger, mali, burundi, seneg, togo, gabon, rwanda, guinea, equatori, benin, comoro, congo, zair, cameroon, burkina, faso, guinea-bissau, african, sahara 
    ##       Lift: abdoulay, ange-félix, anjouan, bata, benin’, biya, bozizé, cameroon’, compaor, compaoré, debi, ecca, gabon’, idriss, itno, lissouba, mali’, marcoussi, mbasogo, minurcat 
    ##       Score: chad, burundi, niger, benin, mali, african, togo, guinea, rwanda, burkina, zair, faso, equatori, oau, comoro, comorian, gabon, congoles, seneg, congo 
    ## Topic 8 Top Words:
    ##       Highest Prob: israel, peac, nation, state, unit, arab, intern, palestinian, peopl, secur, resolut, right, lebanon, aggress, territori, world, isra, palestin, war, forc 
    ##       FREX: israel, lebanon, lebanes, iran, zionist, isra, iranian, jerusalem, palestin, jewish, islam, arab, occupi, occup, palestinian, egypt, jew, jordan, entiti, aggress 
    ##       Lift: aqsa, judaea, kafr, mosh, samaria, yasin, dayan, irgun, khomeini, judea, euphrat, zion, balfour, kippur, deir, yom, american-mad, rejectionist, baluchi, satan 
    ##       Score: zionist, arab, israel, islam, lebanon, lebanes, palestinian, iran, isra, iranian, afghanistan, occup, aggress, palestin, afghan, jerusalem, moslem, jewish, jew, iraqi 
    ## Topic 9 Top Words:
    ##       Highest Prob: countri, intern, africa, develop, south, econom, nation, peopl, peac, continu, unit, namibia, state, problem, independ, world, session, will, assembl, co-oper 
    ##       FREX: namibia, apartheid, namibian, pretoria, south, racist, zimbabw, black, swapo, front-lin, southern, africa, commod, indian, non-align, fortieth, white, regim, co-oper, program 
    ##       Lift: self-extinct, -payment, jayewarden, administrator-gener, tricamer, pac, sobhuza, fswapoj, koevoet, transkei, turnhall, swapoj, botha, bigpow, nimeiri, siaka, billion—, roll-back, pinié, xix 
    ##       Score: co-oper, namibia, racist, africa, kampuchea, namibian, apartheid, swapo, pretoria, south, zimbabw, african, oau, non-align, dialog, cyprus, develop, bantustan, nonalign, lesotho 
    ## Topic 10 Top Words:
    ##       Highest Prob: nation, develop, unit, countri, peac, world, will, intern, econom, region, new, year, effort, nuclear, hope, also, issu, continu, south, confer 
    ##       FREX: japan, india, asean, pakistan, nepal, thailand, bangladesh, myanmar, sri, indonesia, cambodia, philippin, lanka, malaysia, organis, australia, asian, south-east, kashmir, environ 
    ##       Lift: arf, unv, nepales, uncdf, yasushi, suharto, akashi, wangchuck, cambodia’, tae, nepal, premadasa, saarc, jigm, re-ord, asean’, singy, rajiv, ziaur, khaleda 
    ##       Score: nepal, thailand, asean, myanmar, japan, pakistan, develop, australia, sri, cambodia, lanka, bangladesh, nuclear, drug, bhutan, lao, india, kashmir, philippin, peace-keep 
    ## Topic 11 Top Words:
    ##       Highest Prob: will, unit, intern, peac, communiti, govern, negoti, effort, problem, right, nation, solut, secur, year, also, countri, continu, european, hope, concern 
    ##       FREX: cyprus, ireland, turkey, malta, turkish, spain, mediterranean, greec, cypriot, austria, itali, european, greek, negoti, northern, portug, europ, co-oper, belgium, solut 
    ##       Lift: austro-italian, ellemann-jensen, famagusta, turkish-cypriot, cyprus’, denktash, varosha, bizon, bi-zon, partitionist, tyrol, rauf, german-speak, denkta, greece’, tyrolean, spaniard, valletta, turkey’, papandreou 
    ##       Score: co-oper, cyprus, turkish, ireland, turkey, greec, cypriot, malta, greek, european, europ, austria, spain, mediterranean, irish, peace-keep, itali, belgium, dialog, negoti 
    ## Topic 12 Top Words:
    ##       Highest Prob: peopl, world, will, nation, human, must, peac, can, terror, live, right, today, one, year, unit, global, freedom, democraci, war, countri 
    ##       FREX: terrorist, extremist, humankind, god, children, dream, taliban, apv, school, mother, young, religion, girl, kill, corrupt, syria, timor-lest, woman, compass, toler 
    ##       Lift: tahrir, hindus, aylan, afghan-l, ltte, drone, saviour, benazir, centigrad, -sex, homosexu, ecclesiast, shahe, buddhism, al-assad’, shia, seventh-largest, monoth, islamist, psalm 
    ##       Score: taliban, terror, humankind, terrorist, world’, democraci, syria, muslim, peopl, timor-lest, ebola, girl, millennium, human, global, isil, children, iran’, syrian, obama 
    ## Topic 13 Top Words:
    ##       Highest Prob: countri, intern, govern, peac, state, america, will, nation, peopl, american, latin, develop, econom, social, polit, right, central, support, unit, region 
    ##       FREX: guatemala, paraguay, bolivia, panama, costa, venezuela, rica, peru, hondura, ecuador, latin, argentina, colombia, mexico, dominican, salvador, american, chile, america, nicaragua 
    ##       Lift: belaund, bilingu, borja, bustamant, chamorro, ecuador’, fujimori, guatemalan, honduras’, hyper-infl, instraw, paraguayan, peru’, peruvian, quechua, sánchez, amazonian, betancur, bolivia’, cerezo 
    ##       Score: bolivia, paraguay, panama, ecuador, hondura, costa, guatemala, rica, venezuela, american, peru, dominican, bolivian, panamanian, chile, argentina, latin, argentin, drug, guatemalan 
    ## Topic 14 Top Words:
    ##       Highest Prob: nation, unit, will, state, countri, organ, govern, assembl, world, peopl, general, power, intern, problem, session, one, great, deleg, right, charter 
    ##       FREX: connexion, portugues, sea-b, rhodesia, decolon, man, waldheim, portug, sea, honor, twenty-fifth, program, xxv, kurt, dialog, item, jurisdict, favor, floor, coastal 
    ##       Lift: ardor, brooksrandolph, gowon, trawler, lamizana, sangoul, prebisch, uthant, work-, vina, volta, anglo-rhodesian, aaddl, non-arma, hambro, sea-, sakiet, unctadj, addl, halfheart 
    ##       Score: co-oper, connexion, rhodesia, viet-nam, dialog, sea-b, favor, waldheim, portugues, coloni, honor, volta, xxv, kurt, sea, deleg, portug, endeavor, neighbor, man 
    ## Topic 15 Top Words:
    ##       Highest Prob: nation, develop, peac, africa, countri, african, govern, unit, intern, communiti, will, continu, conflict, peopl, secur, support, also, effort, econom, somalia 
    ##       FREX: sierra, liberia, leon, ethiopia, uganda, malawi, somalia, kenya, ghana, swaziland, somali, eritrea, africa’, liberian, sudan, gambia, nigeria, unita, ecowa, sadc 
    ##       Lift: ajj, eritrea’, ethiopia’, gambia’, indlovukazi, jammeh, kabbah, leonean, masir, mele, muluzi, museveni, nigeria’, npfl, tanzania’, tejan, unamsil, unme, unmil, yoweri 
    ##       Score: malawi, sierra, liberia, leon, somalia, ecowa, african, liberian, swaziland, uganda, somali, sudan, ethiopia, hivaid, africa, ghana, kenya, africa’, sadc, eritrea 
    ## Topic 16 Top Words:
    ##       Highest Prob: intern, nation, develop, unit, countri, secur, region, cooper, global, will, support, effort, state, import, econom, peac, communiti, terror, stabil, world 
    ##       FREX: azerbaijan, kazakhstan, tajikistan, turkmenistan, mongolia, peacebuild, kyrgyzstan, armenia, slovakia, millennium, uzbekistan, ki-moon, partnership, coordin, country’, moldova, counter-terror, peacekeep, cooper, people’ 
    ##       Lift: aral, ashgabat, baku-tbilisi-ceyhan, baku-tbilisi-kar, cica, dushanb, kyrgyzstan’, nazarbaev, niyazov, nursultan, turkmenistan, uzbekistan’, alyaksandr, armenia-azerbaijan, astana, bishkek, darya, emomali, heydar, karimov 
    ##       Score: azerbaijan, kazakhstan, tajikistan, millennium, turkmenistan, moldova, mdgs, mongolia, armenia, belarus, peacebuild, kyrgyzstan, uzbekistan, osc, ki-moon, slovakia, global, karabakh, ukrain, kyrgyz 
    ## Topic 17 Top Words:
    ##       Highest Prob: develop, world, intern, countri, nation, econom, new, will, must, problem, polit, right, order, can, relat, human, communiti, peac, social, unit 
    ##       FREX: interdepend, technolog, concept, industri, balanc, mankind, factor, structur, confront, dimens, trend, bloc, ideolog, machineri, evolut, common, analysi, awar, contemporari, raw 
    ##       Lift: self-determination—, unpromis, inter-connexion, post-industri, trite, stratif, laissez-fair, autarchi, raw-material-produc, order-, foretold, interchang, community-, subsystem, undreamed-, exclusiv, civilis, side-effect, treaty-mak, bi-polar 
    ##       Score: co-oper, develop, world, econom, intern, problem, must, relat, new, interdepend, mankind, detent, technolog, order, global, industri, social, world-wid, polit, system 
    ## Topic 18 Top Words:
    ##       Highest Prob: world, will, can, one, countri, unit, year, now, must, nation, mani, new, time, even, war, state, power, need, nuclear, much 
    ##       FREX: zealand, franc, perhap, told, get, britain, enough, wrong, littl, oil, suggest, seem, someth, hear, simpli, tell, imf, spend, argu, blame 
    ##       Lift: re-inscrib, diesel, maori, shorthand, robot, solubl, re-inscript, shopkeep, bach, anglo-french, pretti, néstor, keynesian, pundit, south—, beam, tether, fresh-wat, harmless, zimbabwe-rhodesia 
    ##       Score: zealand, can, nuclear, caledonia, soviet, now, one, even, let, franc, seem, much, war, know, oil, want, must, super-pow, pacif, power

Proportions
===========

``` r
# Topical Proportions ----

proportions_prelim <- plot(prev_1, type = "summary", custom.labels = "", topic.names = topicNames_18, par(col="grey40", lwd=5))
```

![](inequality_stm_files/figure-markdown_github/Topical%20Proportions-1.png)
