###########################################################
############### Definition of S3 class mmmr ###############
###########################################################
 
#' MMM Fitness Function Preparation
#'
#' Used to generate the inputs for the GA::de function used to calculate saturation and adstock for media channels.
#'
#' @param data The training data for the model
#' @param dep_col The name of the dependent variable
#' @param date_col The name of the date variable
#' @param saturated The names of the columns which should undergo a diminishing returns transformation.
#' @param adstocked The names of the columns which should undergo an adstocking transformation.
#' @param alphas_low A named vector of values representing the minimum alpha parameter of some saturated columns.
#' @param alphas_high A named vector of values representing the maximum alpha parameter of some saturated columns.
#' @param gammas_low A named vector of values representing the minimum gamma parameter of some saturated columns.
#' @param gammas_high A named vector of values representing the maximum gamma parameter of some saturated columns.
#' @param thetas_low A named vector of values representing the minimum theta parameter of some adstocked columns.
#' @param thetas_high A named vector of values representing the maximum theta parameter of some saturated columns.
#' @param seed The seed for the random elemens of the function
#' @param country The country code for the "Prophet" component of the analysis
#'
#' @return A list. The first element is a list of length 2 containing the "lower" and "upper" arguments to the GA::de function. The second element is a function which takes a solution from the GA::de function and casts it back into named hyperparameter vectors. The third and final value is the fitness function.
#' @examples
mmm_fitness_gen <- function(data, dep_col, date_col, saturated, adstocked, alphas_low = NULL, alphas_high = NULL, gammas_low = NULL, gammas_high = NULL, thetas_low = NULL, thetas_high = NULL, seed = 1234, country = "CA"){

                                        #Function to expand the boundary variables (e.g. alphas_low) to include all relevant variables (not just the one the user specified)
    
    supplement_bound <- function(allchannels, tweaks, default_bound){
        returnvec <- numeric(length(allchannels))
        if(is.null(tweaks)){
            returnvec <- rep(default_bound, length(allchannels))
            names(returnvec) <- allchannels
            return(returnvec)
        } else {
            for(channel in allchannels){
                if(channel %in% names(tweaks)){
                    returnvec[channel] <- tweaks[channel]
                } else{
                    returnvec[channel] <- default_bound
                }
            }
            return(returnvec)
        }
    }
    
    alphas_low <- supplement_bound(saturated, alphas_low, .5)
    alphas_high <- supplement_bound(saturated, alphas_high, 3)
    gammas_high <- supplement_bound(saturated, gammas_high, 1)
    gammas_low <- supplement_bound(saturated, gammas_low, .3)
    thetas_high <- supplement_bound(adstocked, thetas_high, .9)
    thetas_low <- supplement_bound(adstocked, thetas_low, 0)    

    ## copy data for processing
    datmod <- data
    sat_and_adstocked <- intersect(saturated, adstocked)

    ## It's assumed all variables in the dataframe are predictors.
    ## TODO use a formula input instead of this assumption
    predictors <- setdiff(names(datmod), c(dep_col, date_col))

                                        #Replace NA with 0
    datmod %<>% dplyr::mutate(dplyr::across(.cols = dplyr::any_of(predictors),
                                            .fns = function(x){tidyr::replace_na(x, replace=0)}))

    ## Construct prophet preliminaries

    ## Prophet is picky about variable names. Cleaning predictor names and setting up prophet model

    original_predictors <- predictors
    predictors <- janitor::make_clean_names(predictors)

    ## Instantiated a prophet model.
    prph <- prophet::prophet(daily.seasonality = FALSE, weekly.seasonality = FALSE)

    ## Predictors added as regressors in prophet
    for(predictor in predictors){
        prph <- prophet::add_regressor(prph, predictor)
    }

    if(!is.null(country)){
        prph$country_holidays <- country
    }

    ## Restoring original predictor names
    predictors <- original_predictors
    ## Combining the parameter boundaries for use in the fitness function

    slower <- c(alphas_low, gammas_low, thetas_low)
    shigher <- c(alphas_high, gammas_high, thetas_high)
    serialized_bounds <- list(low_bounds = slower, high_bounds = shigher)

    ## A function to return. This takes the solution of the GA::de model and converts it into named vectors of the alpha, gamma, and theta solutions.
    
    unserialize_hyperparams <- function(params){
        alphas <- params[1:length(saturated)]
        gammas <- params[(length(saturated) + 1):(2*length(saturated))]
        thetas <- params[(2*length(saturated)+1):length(params)]
        names(alphas) <- saturated
        names(gammas) <- saturated
        names(thetas) <- adstocked
        newhyps <- data.frame(predictors = original_predictors)
        newhyps %<>% dplyr::mutate(
                         alphas = ifelse(predictors %in% saturated, alphas[predictors], NA),
                         gammas = ifelse(predictors %in% saturated, gammas[predictors], NA),
                         thetas = ifelse(predictors %in% adstocked, thetas[predictors], NA))
        return(newhyps)                         
    }

    ## The fitness function for use in GA::de
    mmm_fitness <- function(params, model = FALSE, seed=seed, silent=FALSE, ...){

        sat_names <- saturated
        ads_names <- adstocked
        num_sat <- length(sat_names)
        num_ad <- length(ads_names)

        ## Extract the parameters from "params"
        alphas <- params[1:num_sat]
        gammas <- params[(num_sat + 1):(2*num_sat)]
        thetas <- params[((2*num_sat) + 1):length(params)]

        names(thetas) <- ads_names
        names(alphas) <- sat_names
        names(gammas) <- sat_names
        ## Compute gammaTrans. This scales the gamma parameter with the media channel to a reasonable inflection point.
        gammaTrans <- unlist(purrr::map(sat_names,
                                        .f=function(x){
                                            gamma_to_gammatrans(
                                                datmod[[x]],
                                                gammas[x],
                                                decay = ifelse(x %in% ads_names, thetas[x], NA))
                                        }))
        names(gammaTrans) <- sat_names

        
        ## Apply adstocking transformation to adstocked channels
        datmod %<>% dplyr::mutate(
                               dplyr::across(
                                          .cols=dplyr::any_of(ads_names),
                                          .fns= ~ adstock_geometric(.x, thetas[dplyr::cur_column()])))

        ## Apply diminishing returns transformation to saturated channels
        datmod %<>% dplyr::mutate(
                               dplyr::across(
                                          .cols=dplyr::any_of(sat_names),
                                          .fns= function(x){
                                              saturation_hill_trans(
                                                  x,
                                                  alphas[dplyr::cur_column()],
                                                  gammaTrans[dplyr::cur_column()])}))
      
        ## Fitting prophet and extracting seasonality, trend, and holiday components
        datmod %<>% prophetize_df(dep_col, date_col, predictors = predictors, country = country, prph = prph)

        predictors <- c(predictors, "yearly", "trend", "holidays")
        ## Setting up ridge regression fit for the chosen alpha, gamma, and theta parameters
        omits <- date_col
        lambdas <- 10^seq(-3,-5, by = -.05)
        datmodcut <- dplyr::filter(datmod, !is.na(.data$trend))
        datmod_matrix <- as.matrix(dplyr::select(datmodcut, dplyr::any_of(setdiff(predictors, omits))))
        ## Lower bounds for the glmnet. We assume non-negative effect from media channels. Holidays, trend, and seasonality can have negative effect.
        lower <- ifelse(setdiff(predictors, omits) %in% c(sat_names, ads_names), 0, -Inf)
        dep <- datmodcut[[dep_col]]
        glm_cv <- glmnet::cv.glmnet(datmod_matrix, dep, alpha = 0, lower.limits = lower, keep = TRUE, ...)
        error <- min(glm_cv$cvm)
        if(!silent){print(glue::glue("Model error: {error}"))}
        
        ## When used with GA::de, only the error is returned.
        if(model){
            return(list(glm_cv, datmod))}
        else{return(-error)}
    }
    return(list(serialized_bounds, unserialize_hyperparams, mmm_fitness))
}

#' MMM
#'
#' Creates a media mix model object. Use `fit` to train the model.
#'
#' @param predictors A charactor vector containing the names of the predictor columns of the data.
#' @param saturated A character vector containing the names of predictor columns for which you'd like to calculated diminishing returns.
#' @param adstocked A character vector containing the names of predictor columns for which you'd like to calculated adstocking effects.
#' @param dep_col A string: the name of the dependent variable of the data.
#' @param date_col A string: the name of the date variable of the data.
#' @param alphas_low A named numeric vector indicating the minimum value for the alpha parameter for some media channels. If omitted, defaults are used.
#' @param alphas_high A named numeric vector indicating the maximum value for the alpha parameter for some media channels. If omitted, defaults are used.
#' @param gammas_low A named numeric vector indicating the minimum value for the gamma parameter for some media channels. If omitted, defaults are used.
#' @param gammas_high A named numeric vector indicating the maximum value for the gamma parameter for some media channels. If omitted, defaults are used.
#' @param thetas_low A named numeric vector indicating the minimum value for the theta parameter for some media channels. If omitted, defaults are used.
#' @param thetas_high A named numeric vector indicating the maximum value for the theta parameter for some media channels. If omitted, defaults are used.
#' @param seed A numeric value for the seed. Required for reproducibility.
#' @param country A string denoting a country code for use with prophet. This code allows the model to use holidays in its time-series component. Country codes are in the `country` column of `prophet::generated_holidays`.
#'
#' @return An S3 object of type mmmr
#' 
#' @export
mmmr <- function(predictors, saturated, adstocked, dep_col, date_col, alphas_low = NULL, alphas_high = NULL, gammas_low = NULL, gammas_high = NULL, thetas_low = NULL, thetas_high = NULL, seed=1234, country = "CA"){

    if(!is.na(country) && !is.null(country) && !(country %in% prophet::generated_holidays$country)){
        stop("Invalid country code. Use one of: {prophet::generated_holidays$country}")
    }
    
    check_presence(saturated, predictors, namecheck = FALSE)
    check_presence(adstocked, predictors, namecheck = FALSE)
    check_presence(alphas_low, saturated)
    check_presence(alphas_high, saturated)
    check_presence(gammas_low, saturated)
    check_presence(gammas_high, saturated)
    check_presence(thetas_low, saturated)
    check_presence(thetas_high, saturated)

    newmod <- list(predictors = predictors, saturated = saturated, adstocked = adstocked, dep_col = dep_col, date_col = date_col, alphas_low = alphas_low, alphas_high = alphas_high, gammas_low = gammas_low, gammas_high = gammas_high, thetas_low = thetas_low, thetas_high = thetas_high, seed=seed, country=country)
    class(newmod) <- c("mmmr", class(newmod))
    return(newmod)
}

#' print.mmmr
#'
#' Print basic stats about an mmmr model
#'
#' @param x An mmmr object
#' @param ... Not used
#' @return A character string
#' 
#' @export
print.mmmr <- function(x, ...){
    print(paste("Predictors:", paste(x$predictors, collapse = " "), collapse = " "))
    print(paste("Saturated:", paste(x$saturated, collapse = " "), collapse = " "))
    print(paste("Adstocked:", paste(x$adstocked, collapse = " "), collapse = " " ))    
}

#' fit.mmmr
#'
#' Fits an mmmr model to some data. Returns a `fit_mmmr` model object that contains the parameters and coefficients for the solution. A `fit_mmmr` object may be used with `predict` and `coef`.
#'
#' @param object The mmmr object to be used for fitting.
#' @param data A dataframe to be used for fitting.
#' @param maxiter Maximum number of iterations for the genetic algorithm. Passed to GA::de
#' @param ... Other argument passed to GA::de. The ... argument to GA::de are passed to the fitness function, which then gets passed to the glmnet::cv.glmnet function.
#' 
#' @return An S3 object of type mmmr_fit
#' 
#' @export
fit.mmmr <- function(object, data, maxiter = 10, ...){

    #Getting inputs for GA::de. 
    fit_funcs <- mmm_fitness_gen (data,
                                  object$dep_col,
                                  object$date_col,
                                  object$saturated,
                                  object$adstocked,
                                  alphas_low = object$alphas_low,
                                  alphas_high = object$alphas_high,
                                  gammas_low = object$gammas_low,
                                  gammas_high = object$gammas_high,
                                  thetas_low = object$thetas_low,
                                  thetas_high = object$thetas_high,
                                  seed = object$seed,
                                  country = object$country)

    ## Fitting with the genetic algorithm
    gen_model <- GA::de(fit_funcs[[3]], lower = fit_funcs[[1]][[1]], upper = fit_funcs[[1]][[2]], seed=object$seed, maxiter = maxiter, ...)

    ## Setting up the mmmr_fit object
    mod_fit <- object
    object$train <- data
    object$de <- gen_model
    object$hyps <- fit_funcs[[2]](gen_model@solution)
    ##print("hyps")
    ##print(object$hyps)
    gammaTrans <- unlist(purrr::map(object$saturated,
                                    .f=function(x){
                                        row <- dplyr::filter(object$hyps, .data$predictors == x)
                                        gamma_to_gammatrans(
                                            data[[x]],
                                            row$gammas,
                                            decay = ifelse(x %in% object$adstocked,
                                                           row$thetas[x], NA))
                                    }))
    
    names(gammaTrans) <- object$saturated
    object$hyps %<>% dplyr::left_join(data.frame(predictors = names(gammaTrans), gammaTrans = gammaTrans))
    
    if(!is.null(object$seed)){set.seed(object$seed)}
    glmlist <- fit_funcs[[3]](gen_model@solution, model=TRUE, seed=object$seed)
    object$glm <- glmlist[[1]]
    object$mod_df <- glmlist[[2]]
    object$proph <- attr(glmlist[[2]], "prph")
    attr(object$mod_df, "prph") <- NULL
    object$fitness <- fit_funcs[[3]]
    
    coef_frame <- data.frame(
        predictors = rownames(coef(object$glm)),
        # pull coefficients from the glm with lowest cross validated error
        coef = c(
            object$glm$glmnet.fit$a0[object$glm$index[1,1]],
            object$glm$glmnet.fit$beta[,object$glm$index[1,1]]
            ))
    invisible(object$hyps <- dplyr::full_join(coef_frame, object$hyps))
    object$cv_error <- min(object$glm$cvm)
    object$cv_mean_error <- object$cv_error / nrow(data)
    class(object) <- c("mmmr_fit", class(object))
    return(object)
}

#' print.mmmr_fit
#'
#' Print basic stats about a fitted mmmr model
#'
#' @param x An mmmr_fit object
#' @param ... Not used
#' @return A character string
#' 
#' @export
print.mmmr_fit <- function(x, ...){
    print("Fitted mmmr model")
    print("Hyperparameters and Coefficients: ")
    print(x$hyps)
    print(glue::glue("Mean Absolute Cross-Validated Error: {object$cv_mean_error}"))
    print.mmmr(x)
}

#' predict.mmmr_fit
#'
#' Get predictions from an mmmr_fit object
#'
#' @param object An mmmr object
#' @param newdata A dataframe to get new predictions from.
#' @param new_prophet If set to TRUE, trend, holidays, and seasonality columns will re-computed and added to the data. If FALSE, then the prophet model from the fit will be used to add trend, holidays, and seasonality.
#' @param compute_gammatrans If set to TRUE, new values of gammaTrans will be computed with the new data. If false, gammaTrans computed with the training data will be used.
#' @param full_table If set to TRUE, the return value will include the modified data as well as the predictions
#' @param ... Not currently used
#' 
#' @return An S3 object of type mmmr
#' 
#' @export
predict.mmmr_fit <- function(object, newdata = NULL, new_prophet = FALSE, compute_gammatrans = FALSE, full_table = FALSE, ...){

    if(is.null(newdata)){
        newdata <- object$train
    }

    if(new_prophet){
        newdata %<>% prophetize_df(
                         object$dep_col,
                         object$date_col,
                         predictors = object$predictors,
                         country = object$country)
    } else {
        prphdat <- newdata
        names(prphdat)[which(names(prphdat) == object$dep_col)] <- "y"
        names(prphdat)[which(names(prphdat) == object$date_col)] <- "ds"
        prphdat %<>% janitor::clean_names()

        prphdat <- stats::predict(object$proph, prphdat) %>%
            dplyr::select(dplyr::any_of(c("ds", "yearly", "trend", "holidays"))) %>%
            dplyr::mutate(ds = lubridate::as_date(.data$ds))

        names(prphdat)[1] <- date_col
        newdata %<>% dplyr::left_join(prphdat)

        }

    predictors <- c(object$predictors, "yearly", "trend", "holidays")
    
    newdata %<>% dplyr::select(dplyr::any_of(c(predictors)))

    hypsframe <- dplyr::filter(object$hyps, predictors %in% names(newdata))
    intercept <- object$hyps[1,2]
    newdata %<>% apply_media_transforms(object$hyps, compute_gammatrans = compute_gammatrans) %>%
        dplyr::mutate(
                   dplyr::across(
                              .cols = dplyr::any_of(predictors),
                              .fns = function(x){
                                  x *
                                      dplyr::filter(
                                                 object$hyps,
                                                 predictors == dplyr::cur_column()) %>%
                                      dplyr::pull(coef)})) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(.pred = sum(dplyr::c_across(dplyr::any_of(predictors))) + intercept) %>%
        dplyr::ungroup()
    
    if(full_table){return(newdata)}
    else{return(dplyr::select(newdata, dplyr::any_of(".pred")))}
    
    }

#' coef.mmmr_fit
#'
#' Get coefficients from an mmmr_fit object
#'
#' @param object An mmmr object
#' @param complete If TRUE, NA coefficients will be included
#' @param params If TRUE, a dataframe including the alpha, gamma, and theta parameters will be returned.
#' @param ... Not currently used
#' @return An S3 object of type mmmr
#' 
#' @export
coef.mmmr_fit <- function(object, complete = TRUE, params = FALSE, ...){
    return <- object$hyps
    if(!complete){
        return %<>% dplyr::filter(!is.na(coef))
    }
    if(params){
        return(return)
    } else {
        return(frame_to_named_vec(return))
    }
}


