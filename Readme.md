# Tier 1 Analysis

A repository containing the dockerized version of the Tier 1 Analysis.

This is a lightweight package for parsing small flow cytometry files containing
measurements of the HLA A2, A3 and B7 alleles. In each batch of files, autoHLA
finds an appropriate decision boundary between cells that are negative
and positive for each allele.
# TODO: REPLACE DESCRIPTION AND REWRITE

<img src="images/thresholds.png" alt="" width="800" height="auto">

The goal is to determine each subject's HLA
type: based on the distribution of the subject's cells, they could be
positive, negative or inconclusive for each allele.

<img src="images/heatmap.png" alt="" width="500" height="auto">

## Working locally

To run locally:

`docker-compose up --build`