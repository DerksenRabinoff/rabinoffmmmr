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
#'
#' @return The transformed vector
#' 
#' @export
saturation_hill_trans <- function(input, alpha, gammatrans){
    input^alpha / (input^alpha + gammatrans^alpha)
}

## inverse: input = ((sat*gammatrans^alpha)/(1-sat))^(1/alpha)

#' Saturation Hill Derivative
#'
#' Like saturation_hill_trans, but the derivative of the curve
#'
#' @param input The input vector to be transformed
#' @param alpha The alpha parameter for transformation. This controls the shape of the curve.
#' @param gammatrans The gammatrans parameter for transformation. This controls the inflection point of the curve.
#'
#' @return The transformed vector
#' 
#' @export
saturation_hill_trans_deriv <- function(input, alpha, gammatrans){
    num <- input^alpha
    numder <- alpha*(input^(alpha - 1))
    denum <- (input^alpha + gammatrans^alpha)
    denumder <- numder
    return((numder*denum - num*denumder)/(denum^2))
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
        dplyr::mutate(ds = lubridate::as_date(ds))

    names(prphdat)[1] <- date_col

    names(data) <- original_names

    returndat <- dplyr::left_join(data, prphdat, by = date_col)

    attr(returndat, "prph") <- prph

    return(returndat)
    
}
