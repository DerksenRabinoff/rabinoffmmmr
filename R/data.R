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
