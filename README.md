---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rabinoffmmmr

<!-- badges: start -->
<!-- badges: end -->

The goal of rabinoffmmmr is to provide an interface for building and employing media mix models that more closely resembles building and employing standard statistical and machine learning models. Model parameters will be stored in an initial model spec object of class `mmmr`, which then can be used with `fit`, `predict`, and `coef`. Plotting and reporting functions will be added.

## Installation

You can install the development version of rabinoffmmmr like so:

``` r
## NOT RUN
#devtools::install_github("DerksenRabinoff/rabinoffmmmr")
```

## Example

Attaching rabinoffmmmr and loading data


```r
library(rabinoffmmmr)
data <- readr::read_csv("data/ad_spend_sample_data.csv")
```

The mmm needs to know the dependent variable, the date variable, and which channels need diminishing returns and adstocking calculated.


```r
## The data has the following columns
dependent <- "Sales"
date <- "Week Starting"

## Let's suppose the following are subject to diminishing returns effects
saturation_vars <- c("TV Spend", "Radio Spend", "Online Video Spend", "Social Media Spend")

## Let's suppose the following are subject to adstocking effects (retention in consumer memory)
adstocking_vars <- c("TV Spend", "Radio Spend", "Social Media Spend", "Search Ads Spend", "Direct Mail Spend")

## The predictors will include all variables other than the date and dependent variable
predictor_vars <- setdiff(names(data), c(dependent, date))

## Make sure the date variable is of type "Date" and that every row has a date
data[[date]] <- lubridate::as_date(data[[date]], format = "%B %d, %Y")
data %<>% dplyr::filter(!is.na(`Week Starting`))
head(data) %>% knitr::kable()
```



|Week Starting |     Sales| TV Spend| Radio Spend| Online Video Spend| Social Media Spend| Search Ads Spend| Direct Mail Spend| black friday sale| xmas| adverse event|
|:-------------|---------:|--------:|-----------:|------------------:|------------------:|----------------:|-----------------:|-----------------:|----:|-------------:|
|2019-01-28    | 152950.29|     5678|        2394|                 70|                276|              246|              1484|                 0|    0|             0|
|2019-02-04    |  76461.56|     2716|           0|                 94|                236|              376|               224|                 0|    0|             0|
|2019-02-11    | 183703.37|     5166|        2278|                179|                438|              439|               802|                 0|    0|             0|
|2019-02-18    | 110030.79|     4869|           0|                139|                276|              450|               572|                 0|    0|             0|
|2019-02-25    | 194915.23|     4097|        2471|                 82|                325|              308|              1852|                 0|    0|             0|
|2019-03-04    | 119382.20|        0|           0|                 66|                264|              242|               550|                 0|    0|             0|


 
Set up an mmmr model

```r
## Use the variables above to make an mmmr model. The remainder of the variables will use default values
model <- mmmr(predictors = predictor_vars, saturated = saturation_vars, adstocked = adstocking_vars, dep_col = dependent, date_col = date)
```
 
Train the model

```r
start_time <- Sys.time()
model_fit <- fit.mmmr(object = model, data = data, silent = TRUE, maxiter = 1)
print(glue::glue("Time elapsed: {Sys.time() - start_time}"))
```

Parameters and coefficients of the model:

```r
## "complete" means include coefs that are 0. "params" means include the alpha, gamma, and theta parameters of the fit.
model_results <- coef(model_fit, complete=TRUE, params = TRUE)
model_results %>% knitr::kable()
```



|predictors         |          coef|    alphas|    gammas|    thetas|
|:------------------|-------------:|---------:|---------:|---------:|
|(Intercept)        | -5.816438e+04|        NA|        NA|        NA|
|TV Spend           |  7.469329e+04| 0.8185835| 0.9023566| 0.0193691|
|Radio Spend        |  9.149510e+04| 1.9994234| 0.3002393| 0.2338067|
|Online Video Spend |  0.000000e+00| 2.3311007| 0.3492010|        NA|
|Social Media Spend |  5.260751e+04| 1.4089773| 0.6101861| 0.0035023|
|Search Ads Spend   |  1.369697e+01|        NA|        NA| 0.2898560|
|Direct Mail Spend  |  6.936803e+00|        NA|        NA| 0.4631322|
|black friday sale  |  2.607113e+04|        NA|        NA|        NA|
|xmas               | -5.423722e+04|        NA|        NA|        NA|
|adverse event      | -9.711974e+04|        NA|        NA|        NA|
|yearly             |  8.003503e-01|        NA|        NA|        NA|
|trend              |  1.001315e+00|        NA|        NA|        NA|
|holidays           |  9.075114e-01|        NA|        NA|        NA|


