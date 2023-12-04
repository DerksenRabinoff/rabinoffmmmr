


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



| Traffic| (NET) Local TV Spend| Local TV Impressions| OTT Spend (NET)| OTT Impressions| (NET) Local Cable Spend| Local Cable IMPS| (NET) Local Radio Spend| Local Radio GRPs| Online Video Spend| Online Video Impressions| Online Audio Spend| Online Audio Impressions|Pandora Video Spend |Pandora Video Impressions | Online Display Spend| Online Display Impressions| Social Media Spend| Social Media Impressions| Search Spend| Search Impressions| Shopping (LIA) Spend| Shopping (LIA) Impressions| Print Insert Spend| Print Insert Inserts| Direct Mail Spend| Direct Mail Quantity|Week Starting | Bf| new_years| memorial_day| Pres| presidents_day_sh| xmas| adverse_event| Jul4| labor day|    ...38|
|-------:|--------------------:|--------------------:|---------------:|---------------:|-----------------------:|----------------:|-----------------------:|----------------:|------------------:|------------------------:|------------------:|------------------------:|:-------------------|:-------------------------|--------------------:|--------------------------:|------------------:|------------------------:|------------:|------------------:|--------------------:|--------------------------:|------------------:|--------------------:|-----------------:|--------------------:|:-------------|--:|---------:|------------:|----:|-----------------:|----:|-------------:|----:|---------:|--------:|
|   469.0|               4270.4|               102666|          354.43|           20030|                       0|                0|                      NA|               NA|                 69|                     6242|                  0|                        0|NA                  |NA                        |                    3|                       3986|             239.72|                    41308|          290|               6022|                   NA|                         NA|                  0|                    0|                75|                   97|2018-12-31    |  0|         1|            0|    0|                 0|    0|             0|    0|         0| 310.8496|
|   324.0|               3299.7|               131729|          386.46|           21840|                       0|                0|                      NA|               NA|                 69|                     7762|                  0|                        0|NA                  |NA                        |                    0|                          0|             239.72|                    39738|          287|               5401|                   NA|                         NA|                  0|                    0|              1792|                 3128|2019-01-07    |  0|         0|            0|    0|                 0|    0|             0|    0|         0|       NA|
|   321.5|               4418.3|               125033|          386.60|           21848|                       0|                0|                      NA|               NA|                 70|                     8007|                  0|                        0|NA                  |NA                        |                    0|                          0|             239.71|                    17678|          288|               4265|                   NA|                         NA|                  0|                    0|              2235|                 3797|2019-01-14    |  0|         0|            0|    0|                 0|    0|             0|    0|         0|       NA|
|   355.0|                   NA|                   NA|            0.00|               0|                       0|                0|                      NA|               NA|                 68|                     8038|                  0|                        0|NA                  |NA                        |                    0|                          0|             239.72|                    20800|          268|               4329|                   NA|                         NA|                  0|                    0|              3547|                 6731|2019-01-21    |  0|         0|            0|    0|                 0|    0|             0|    0|         0|       NA|
|   348.0|               4800.8|               123915|          386.53|           21844|                       0|                0|                 2022.25|              102|                 71|                     8817|                  0|                        0|NA                  |NA                        |                    0|                          0|             239.72|                    21274|          304|               4571|                   NA|                         NA|                  0|                    0|              1303|                 2332|2019-01-28    |  0|         0|            0|    0|                 0|    0|             0|    0|         0|       NA|
|   372.5|               3309.9|                85192|          354.47|           20032|                       0|                0|                      NA|               NA|                 77|                     8061|                  0|                        0|NA                  |NA                        |                    0|                          0|             239.72|                    41150|          323|               4493|                   NA|                         NA|               2017|                24665|               181|                  241|2019-02-04    |  0|         0|            0|    0|                 0|    0|             0|    0|         0|       NA|


 
Set up an mmmr model

```r
## Use the variables above to make an mmmr model. The remainder of the variables will use default values
model <- mmmr(predictors = predictor_vars, saturated = saturation_vars, adstocked = adstocking_vars, dep_col = dependent, date_col = date)
#> Error in check_presence(saturated, predictors, namecheck = FALSE): <chr> contains values not listed in <chr>
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
#> Error in coef(model_fit, complete = TRUE, params = TRUE): object 'model_fit' not found
model_results %>% knitr::kable()
#> Error in knitr::kable(.): object 'model_results' not found
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
#> Error in which(names(prphdat) == object$dep_col): object 'model_fit' not found

print(predictions)
#> Error in print(predictions): object 'predictions' not found
```
