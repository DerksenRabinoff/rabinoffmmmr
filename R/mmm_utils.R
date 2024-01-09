###########################################################
############ Functions to support MMM creation ############
###########################################################

#' Saturation Hill
#'
#' Applies diminishing returns transformation on media exposure vector
#'
#' @param input The input vector to be transformed
#' @param alpha The alpha parameter for transformation. This controls the shape of the curve.
#' @param gammatrans The gammatrans parameter for transformation. This controls the inflection point of the curve.
#' @param na.rm Remove NA values?
#'
#' @return The transformed vector
#'
#' @examples
#'
#' set.seed(1234)
#' input <- rnorm(20, 10, 5)
#'
#' input
#'
#'  [1]  3.964671 11.387146 15.422206 -1.728489 12.145623 12.530279  7.126300  7.266841  7.177740  5.549811  7.614037 [12]  5.008068  6.118731 10.322294 14.797470  9.448573  7.444952  5.444023  5.814142 22.079176
#'
#' saturation_hill_trans(input, 1, 7)
#'
#' [1]  0.3615860  0.6192993  0.6878095 -0.3278924  0.6343812  0.6415822  0.5044704  0.5093518  0.5062683  0.4422227 [11]  0.5210084  0.4170586  0.4664118  0.5958965  0.6788618  0.5744312  0.5154017  0.4374809  0.4537285  0.7592779
#' 
#' @export
saturation_hill_trans <- function(input, alpha, gammatrans, na.rm = TRUE){
    ret <- (input^alpha) / (input^alpha + gammatrans^alpha)
    if(na.rm){return(ret[!is.na(ret)])}
    return(ret)        
}

## inverse: input = ((sat*gammatrans^alpha)/(1-sat))^(1/alpha)

#' Saturation Hill Derivative
#'
#' Like saturation_hill_trans, but the derivative of the curve
#'
#' @param input The input vector to be transformed
#' @param alpha The alpha parameter for transformation. This controls the shape of the curve.
#' @param gammatrans The gammatrans parameter for transformation. This controls the inflection point of the curve.
#' @param na.rm Remove NA values?
#'
#' @return The transformed vector
#' 
#' @examples
#'
#' set.seed(1234)
#' input <- rnorm(20, 10, 5)
#'
#' input
#'
#'  [1]  3.964671 11.387146 15.422206 -1.728489 12.145623 12.530279  7.126300  7.266841  7.177740  5.549811  7.614037 [12]  5.008068  6.118731 10.322294 14.797470  9.448573  7.444952  5.444023  5.814142 22.079176
#'
#' saturation_hill_trans_deriv(input, 1, 7)
#'
#'  [1] 0.058224640 0.020704721 0.013923274 0.251899744 0.019096730 0.018351904 0.035078513 0.034390811 0.034824431 [10] 0.044445079 0.032776129 0.048545813 0.040673769 0.023328522 0.014732819 0.025872692 0.033547936 0.045203955 [19] 0.042630360 0.008278161
#'
#' 
#' @export
saturation_hill_trans_deriv <- function(input, alpha, gammatrans, na.rm = TRUE){
    num <- input^alpha
    numder <- alpha*(input^(alpha - 1))
    denum <- (input^alpha) + (gammatrans^alpha)
    denumder <- numder
    ret <- (numder*denum - num*denumder)/(denum^2)
    if(na.rm){return(ret[!is.na(ret)])}
    return(ret)
}

#' Geometric Adstock
#'
#' Applies geometric adstocking to vector
#'
#' @param input The input vector to be transformed
#' @param decay The decay rate parameter. Also called "theta" in other contexts.
#'
#' @return The transformed vector
#'
#' @examples
#' set.seed(1234)
#' input <- rnorm(20, 10, 5)
#'
#' input
#'
#'  [1]  3.964671 11.387146 15.422206 -1.728489 12.145623 12.530279  7.126300  7.266841  7.177740  5.549811  7.614037 [12]  5.008068  6.118731 10.322294 14.797470  9.448573  7.444952  5.444023  5.814142 22.079176
#'
#' adstock_geometric(input, .5)
#' [1]  3.964671 13.369482 22.106947  9.324985 16.808116 20.934337 17.593469 16.063575 15.209528 13.154575 14.191324 [12] 12.103730 12.170595 16.407592 23.001266 20.949206 17.919555 14.403801 13.016042 28.587197 
#' 
#' @export
adstock_geometric <- function(input, decay){
    output_vec <- numeric(length(input))
    output_vec[1] <- input[1]
    for(i in 2:length(input)){
        output_vec[i] <- (output_vec[i-1]*decay) + input[i]
    }
    return(output_vec)
}

#' Calculate Gamma Trans
#'
#' The gamma parameter controls the point of diminishing returns. For consistency, gamma is given as a number between 0 and 1. Gammatrans is gamma scaled up sensibly to the size of the input.
#'
#' @param input The vector on which to calculate gammatrans
#' @param gamma The gamma parameter
#' @param decay If non-null, then the input is first transformed with [adstock_geometric()]
#'
#' @return The gammatrans parameter (a single numeric value)
#' 
#' @export
gamma_to_gammatrans <- function(input, gamma, decay = NA){
    if(!is.na(decay)){
        return(as.numeric(range(adstock_geometric(input, decay)) %*% c(1 - gamma, gamma)))
    }
    return(as.numeric(c(range(input) %*% c(1 - gamma, gamma))))
}

check_presence <- function(toCheck, vec, namecheck = TRUE){
    if(!is.null(toCheck)){
        if(namecheck){
            if(!all(names(toCheck) %in% vec)){
                stop(rlang::englue("{{toCheck}} has names not listed in {{vec}}"))
            }
        } else{
            if(!all(toCheck %in% vec)){
                stop(rlang::englue("{{toCheck}} contains values not listed in {{vec}}"))
            }
        }
    }
}

#' Apply Media Transforms
#'
#' Given a dataframe, and a dataframe of alpha, gamma, and theta parameters, this function applies adstocking and diminishing returns effects to the original dataframe.
#'
#' @param data The dataframe to be computed on.
#' @param hyps A dataframe storing the hyperparameters. It needs four columns: A "predictors" column identifying the variables to be transformed, an "alphas" column with the alpha parameter for each predictor, a "gammas" columns with the gamma parameter for each predictor, and a "thetas" column for each theta paramter for each predictor. For non-saturated variables, leave alphas and gammas as "NA". For non-adstocked variables leave thetas as NA.
#' @param compute_gammatrans Defaults to TRUE. If TRUE, the gammaTrans value for each parameter will be computed for each saturated variable. If false, gammaTrans must be a column in the "hyps" dataframe.
#' @return The gammatrans parameter (a single numeric value)
#' 
#' @export
apply_media_transforms <- function(data, hyps, compute_gammatrans = TRUE){
    
    datmod <- data

    if(!all(is.na(hyps$gammas) == is.na(hyps$alphas))){
        stop("Cannot apply media transforms. Alphas and Gammas mismatch")
    }

    if(!compute_gammatrans && ! "gammaTrans" %in% names(hyps)){
        stop("gammaTrans is missing from 'hyps'. Please supply it or set 'compute_gammatrans' to TRUE.")
    }

    check_presence(hyps$predictors, names(data))

    saturated <- hyps$predictors[!is.na(hyps$gammas)]
    adstocked <- hyps$predictors[!is.na(hyps$thetas)]

    ## Apply adstocking
    datmod %<>% dplyr::mutate(
                           dplyr::across(
                                      .cols= adstocked,
                                      .fns= ~ adstock_geometric(.x, hyps$thetas[which(hyps$predictors == dplyr::cur_column())])))

    ## Calculate gammaTrans
    if(compute_gammatrans){
        hyps %<>% dplyr::mutate(
                           gammaTrans =
                               unlist(
                                   purrr::map(
                                              hyps$predictors,
                                              function(x){
                                                  if(x %in% saturated){
                                                      gamma_to_gammatrans(
                                                          datmod[[x]],
                                                          hyps$gammas[[which(hyps$predictors == x)]], NA)}
                                                  else{NA}})))
    }
    ## Apply saturation effects
    datmod %<>% dplyr::mutate(
                           dplyr::across(
                                      .cols= saturated,
                                      .fns= function(x){
                                          saturation_hill_trans(
                                              x,
                                              hyps$alphas[which(hyps$predictors == dplyr::cur_column())],
                                              hyps$gammaTrans[which(hyps$predictors == dplyr::cur_column())])}))
    return(datmod)
    }

frame_to_named_vec <- function(data){
    vec <- data[[2]]
    names(vec) <- data[[1]]
    vec
    
    }

#' Prophetize DF
#'
#' Take a dataframe with a date column. Use Meta's "prophet" to add a trend, seasonality, and holidays column to the dataframe.
#'
#' @param data The dataframe to be added to.
#' @param dep_col The name of the dependent variable.
#' @param date_col The name of the date variable.
#' @param predictors The names of the predictor variables. If NULL, then all variables will be used other than the dep_col.
#' @param country The country code to be used to derive the "holidays" column. Valid country codes include any in the "country" column of prophet::generated_holidays.
#' @param prph A prophet model object. If NULL, one will be created.
#' @param daily.seasonality If TRUE, daily seasonality will be computed. Currently non-functional.
#' @param weekly.seasonality If TRUE, weekly seasonality will be computed. Currently non-functional.
#' @param ... Other arguments passed to prophet::prophet().
#' @return A dataframe similar to `data` but supplemented with `trend`, `yearly`, and `holidays` variables.
#' 
#' @export
prophetize_df <- function(data, dep_col, date_col, predictors = NULL, country = "CA", prph = NULL, daily.seasonality = FALSE, weekly.seasonality = FALSE, ...){

    ## Prophet is picky about names. Storing original names and cleaning
    original_names <- names(data)
    original_predictors <- predictors

    names(data)[which(names(data) == dep_col)] <- "y"
    
    names(data)[which(names(data) == date_col)] <- "ds"

    data <- janitor::clean_names(data)

    toreplace <- as.list(rep(0, ncol(data) - 1))
    names(toreplace) <- setdiff(names(data), "ds")
    data %<>% tidyr::replace_na(toreplace)
    rm(toreplace)
    
    if(is.null(predictors)){
        predictors <- setdiff(names(data), c("y", "ds"))
    } else{
        predictors <- janitor::make_clean_names(predictors)
    }

    if(is.null(prph)){
        prph <- prophet::prophet(
                             daily.seasonality = daily.seasonality,
                             weekly.seasonality = weekly.seasonality, ...)       
        
        ## Predictors added as regressors in prophet
        for(predictor in predictors){
            invisible(prph <- prophet::add_regressor(prph, predictor))
        }
        if(!is.null(country)){
            prph$country_holidays <- country
        }
    }

    dat_fit <- dplyr::select(data, dplyr::any_of(c("ds", "y", predictors)))

    prph <- prophet::fit.prophet(prph, dat_fit)
    
    prphdat <- prph %>%
        stats::predict(dat_fit) %>%
        dplyr::select(dplyr::any_of(c("ds", "yearly", "trend", "holidays"))) %>%
        dplyr::mutate(ds = lubridate::as_date(.data$ds))

    names(prphdat)[1] <- date_col

    names(data) <- original_names

    returndat <- dplyr::left_join(data, prphdat, by = date_col)

    attr(returndat, "prph") <- prph

    return(returndat)
    
}
