Overview:
=========

STM preprocessing for "unga\_2017\_corpus.csv"

To-do:
======

-   Country,Names as stopwords

Library
=======

``` r
pacman::p_load(stm,
               dplyr,
               quanteda,
               readr)
```

Load Data
=========

unga\_2017\_corpus.csv

``` r
unga_2017_corpus <- read_csv("unga_2017_corpus.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_double(),
    ##   doc_id = col_character(),
    ##   text = col_character(),
    ##   docvar1 = col_character(),
    ##   docvar2 = col_double(),
    ##   docvar3 = col_double()
    ## )

``` r
unga_2017_corpus$X1=NULL
```

Add Metadata Covariates
=======================

P5

``` r
# P5 ----
unga_2017_corpus$P5 <- NA

unga_2017_corpus <- unga_2017_corpus %>%
  mutate(
    P5 = case_when(
      docvar1 == "USA" ~ 1,
      docvar1 == "FRA" ~ 1,
      docvar1 == "RUS" ~ 1,
      docvar1 == "GBR" ~ 1,
      docvar1 == "CHN" ~ 1,
      is.na(P5) ~ 0
    )
  )

# NWS ----
#NWS
unga_2017_corpus$NWS <- NA
unga_2017_corpus <- unga_2017_corpus %>%
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

# ECOWAS ----
unga_2017_corpus$ECOWAS <- NA
unga_2017_corpus <- unga_2017_corpus %>%
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

# EU ----
unga_2017_corpus$EU <-NA
unga_2017_corpus <- unga_2017_corpus %>%
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
```

Pre-processing w/ textProcessor
===============================

``` r
# Specify metadata from corpus ----
unga_2017_corpus_meta <- data.frame(unga_2017_corpus[,c(1,3:(length(names(unga_2017_corpus))))])

# textProcessor ----
unga_2017_tp <- textProcessor(documents = unga_2017_corpus$text,metadata = unga_2017_corpus_meta)
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

plotRemoved(unga_2017_tp$documents, lower.thresh = seq(1,200, by = 25))
```

![](unga_2017v3_corpuspreparation_files/figure-markdown_github/Plot%20Removed%20Word%20thresholds-1.png)

None of the documents are dropped at the 200 word threshhold. I can try to specify different models at different levels of sensitivity. The sharpest acceleration occurs between 0 - 25, gradually tapering off around 50 and eventually flattening out.

Models at different threshholds: 0,10,20,30,40,50

`prepDocuments` lower.thresh = 0-50 by 10 Outputs: thresh0...thresh50

``` r
#prepDocuments ----

thresh0 <- prepDocuments(unga_2017_tp$documents, unga_2017_tp$vocab, unga_2017_tp$meta, lower.thresh = 0)

thresh10 <- prepDocuments(unga_2017_tp$documents, unga_2017_tp$vocab, unga_2017_tp$meta, lower.thresh = 10)
```

    ## Removing 39651 of 50938 terms (81824 of 5169003 tokens) due to frequency 
    ## Your corpus now has 7507 documents, 11287 terms and 5087179 tokens.

``` r
thresh20 <- prepDocuments(unga_2017_tp$documents, unga_2017_tp$vocab, unga_2017_tp$meta, lower.thresh = 20)
```

    ## Removing 42170 of 50938 terms (118966 of 5169003 tokens) due to frequency 
    ## Your corpus now has 7507 documents, 8768 terms and 5050037 tokens.

``` r
thresh30 <- prepDocuments(unga_2017_tp$documents, unga_2017_tp$vocab, unga_2017_tp$meta, lower.thresh = 30)
```

    ## Removing 43356 of 50938 terms (148599 of 5169003 tokens) due to frequency 
    ## Your corpus now has 7507 documents, 7582 terms and 5020404 tokens.

``` r
thresh40 <- prepDocuments(unga_2017_tp$documents, unga_2017_tp$vocab, unga_2017_tp$meta, lower.thresh = 40)
```

    ## Removing 44094 of 50938 terms (174511 of 5169003 tokens) due to frequency 
    ## Your corpus now has 7507 documents, 6844 terms and 4994492 tokens.

``` r
thresh50 <- prepDocuments(unga_2017_tp$documents, unga_2017_tp$vocab, unga_2017_tp$meta, lower.thresh = 50)
```

    ## Removing 44663 of 50938 terms (200230 of 5169003 tokens) due to frequency 
    ## Your corpus now has 7507 documents, 6275 terms and 4968773 tokens.
