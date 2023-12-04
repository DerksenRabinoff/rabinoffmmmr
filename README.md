


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
##library(rabinoffmmmr)
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
data[[date]] <- lubridate::as_date(data[[date]])
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
print(Sys.time() - start_time)
```

Parameters and coefficients of the model:

```r
## "complete" means include coefs that are 0. "params" means include the alpha, gamma, and theta parameters of the fit.
model_results <- coef(model_fit, complete=TRUE, params = TRUE)
model_results %>% knitr::kable()
```

Predictions from the model. If adstocking is high (high thetas) consider cutting the first few rows. Adstocking values depend on previous rows.

```r
## Load new data
new_data <- readr::read_csv("data/ad_spend_plan_for_predict.csv")
#> Rows: 24 Columns: 11
#> ── Column specification ───────────────────────────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (1): Week Starting
#> dbl (10): Traffic, TV Spend, Radio Spend, Online Video Spend, Social Media Spend, Search Ads Spend,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
new_data[[date]] <- lubridate::as_date(new_data[[date]], format = "%B %d, %Y")
new_data %<>% dplyr::filter(!is.na(`Week Starting`))

predictions <- predict.mmmr_fit(model_fit, new_data)

print(predictions)
```
