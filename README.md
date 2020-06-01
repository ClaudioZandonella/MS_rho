# Designing Studies and Evaluating Research Results: Type M and Type S Errors for Pearson Correlation Coefficient

<span style="font-size:1.75em;">Giulia Bertoldo, Claudio Zandonella Callegher, and Gianmarco Altoè</span>

Department of Developmental Psychology and Socialisation, University of Padova, Padova, Italy

**Submitted to *Meta-psychology***

### Abstract

It is widely appreciated that many studies in psychological science suffer from low statistical power. One of the consequences of analyzing underpowered studies with thresholds of statistical significance, is a high risk of finding exaggerated effect size estimates, in the right or in the wrong direction. These inferential risks can be directly quantified in terms of Type M (magnitude) error and Type S (sign) error, which directly communicate the consequences of design choices on effect size estimation. Given a study design, Type M error is the factor by which a statistically significant effect is on average exaggerated. Type S error is the probability to find a statistically significant result in the opposite direction to the plausible one. Ideally, these errors should be considered during a *prospective design analysis* in the design phase of a study to determine the appropriate sample size. However, they can also be considered when evaluating studies’ results in a *retrospective design analysis*. In the present contribution we aim to facilitate the considerations of these errors in the research practice in psychology. For this reason we illustrate how to consider Type M and Type S errors in a design analysis using one of the most common effect size measures in psychology: Pearson correlation coefficient. We provide various examples and make the R functions freely available to enable researchers to perform design analysis for their research projects.


### Repository Structure

In this repository we collect all the material used in the article. To compile and execute the different scripts, the whole repository is required as there are some dependencies between files. The repository is organized as an R-project, thus we suggest to download the whole repository and open the project in a new R session.

In the folder [Documents/Paper_main](Documents/Paper_main/) you can find the scripts to compile the pdf version of the paper:

- [Paper_main.pdf](Documents/Paper_main.pdf) is the the pdf of the paper
- [Paper_main.Rnw](Documents/Paper_main.Rnw) is the R script to compile the pdf including R chunk code
- [Paper_main.tex](Documents/Paper_main.tex) is the LaTeX script to compile the pdf
- [Paper_main.bib](Documents/Paper_ma|in.bib) is the reference list
- [screens](Documents/Paper_main/screens/) is a folder with images included in the paper

In the folder [R](R/) you can find the R scripts with the functions code:

- [Design_analysis_r.R](R/Design_analysis_r.R) the R functions for prospective and retrospective design analysis are defined
- [Auxiliary_functions.R](R/Auxiliary_functions.R) other R functions are defined for the plots included in the paper

In the folder [Data](Data/) you can find the pre-compiled datasets used in the plots and tables to avoid long running times. To obtain these datasets you can run the chunks code where `eval=F`.

In the folder [renv](renv/) there are information about project’s R dependencies. To know more about `renv` see its homepage ([link](https://rstudio.github.io/renv/)).
