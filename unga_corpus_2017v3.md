Overview
========

This file ingests the following data - Citation: Mikhaylov, Slava; Baturo, Alexander; Dasandi, Niheer, 2017, "United Nations General Debate Corpus", <doi:10.7910/DVN/0TJX8Y>, Harvard Dataverse, V3

Saves as a .csv file for quick input and greater portability.

The output is "unga\_2017\_corpus.csv"

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

``` r
# Read using Quanteda ----
unga_2017_corpus <- readtext::readtext("/Users/ScottPatterson/OneDrive - McGill University/patterson_pouliot/inequality/unga_speeches", docvarsfrom = c("filenames"), dvsep = "_")
```

Export corpus to .csv
=====================

I'm doing this because I want to save space so that I can fit everything into the Github repository

``` r
write.csv(unga_2017_corpus,"unga_2017_corpus.csv")
```
