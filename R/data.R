#' Sample historical spend data
#'
#' A contrived example of weekly ad spends. There is a column for each advertising channel, the sales of the store that week, and the date.
#'
#' @format ## `historical_ad_spends`
#' A data frame with 11 columns and 212 rows:
#' \describe{
#'    \item{Week Starting}{The date of the week (the monday)}
#'    \item{Sales}{The sales of that week}
#'    \item{TV Spend}{How much was spent on TV ads that ran that week}
#'    \item{Radio Spend}{How much was spent on radio ads that ran that week}
#'    \item{Online Video Spend}{How much was spent on online video ads that ran that week}
#'    \item{Social Media Spend}{How much was spent on social media ads that ran that week}
#'    \item{Search ads Spend}{How much was spent on search ads that ran that week}
#'    \item{Direct Mail Spend}{How much was spent on mailed flyers delivered that week}
#'    \item{black friday sale}{1 if that week includes Black Friday, 0 otherwise}
#'    \item{xmas}{1 if that week includes Christmas day, 0 otherwise}
#'    \item{adverse event}{1 if that week includes some known barrier to sales (construction, extreme weather etc.), 0 otherwise}
#' }
"historical_ad_spends"

#' Spend Plan
#'
#' A contrived example of weekly planned ad spends. There is a column for each advertising channel of the store that week, and the date.
#'
#' @format ## `spend_plan`
#' A data frame with 11 columns and 212 rows:
#' \describe{
#'    \item{Week Starting}{The date of the week (the monday)}
#'    \item{TV Spend}{The planned spend for TV ads that ran that week}
#'    \item{Radio Spend}{The planned spend for radio ads that ran that week}
#'    \item{Online Video Spend}{The planned spend for online video ads that ran that week}
#'    \item{Social Media Spend}{The planned spend for social media ads that ran that week}
#'    \item{Search ads Spend}{The planned spend for search ads that ran that week}
#'    \item{Direct Mail Spend}{The planned spend for mailed flyers delivered that week}
#'    \item{black friday sale}{1 if that week includes Black Friday, 0 otherwise}
#'    \item{xmas}{1 if that week includes Christmas day, 0 otherwise}
#'    \item{adverse event}{1 if that week includes some known barrier to sales (construction, extreme weather etc.), 0 otherwise}
#' }
"spend_plan"

#' Generated Holidays
#'
#' The holiday data set from the `prophet` package.
#'
#' @format ## `generated_holidays`
#' A data frame with 4 columns and 46192 rows:
#' \describe{
#'    \item{ds}{The date of the holiday}
#'    \item{holiday}{The name of the holiday}
#'    \item{country}{The country code of the country in which the holiday is observed}
#'    \item{year}{The year of the holiday}
#' }
#' @source <https://github.com/facebook/prophet/tree/main/R/data-raw>
"generated_holidays"

#' Model Fit
#'
#' An example fitted mmmr model
#'
#' @format ## `model_fit`
#' An mmmr_fit object which includes the following fields
#' \describe{
#'    \item{predictors}{A character vector of predictor column names}
#'    \item{saturated}{A character vector of saturated column names}
#'    \item{adstocked}{A character vector of adstocked column names}
#'    \item{dep_col}{The name of the dependant column}
#'    \item{date_col}{The name of the date column}
#'    \item{seed}{The seed for internal use}
#'    \item{country}{The country code for prophet use}
#'    \item{train}{The training data                                        }
#'    \item{de}{The de object from GA::de}
#'    \item{hyps}{A dataframe containing the coefficients and solution parameters}
#'    \item{glm}{A glmnet::cv.glmnet object used to get the coefficients}
#'    \item{mod_df}{The training data modified with saturated and adstocking effects}
#'    \item{proph}{The fitten prophet model}
#'    \item{fitness}{The fitness function used in GA::de}
#' }
"model_fit"

