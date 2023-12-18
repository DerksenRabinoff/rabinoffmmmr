###########################################################
################## MMM Plotting Functions #################
###########################################################

#' Plot Diminishing Returns Curve
#'
#' Produces a ggplot2 object of a diminishing returns curve for a specific media channel.
#'
#' @param object A fitted mmmr model
#' @param channel The name of the media channel to plot
#' @param rate If TRUE, the y axis will be the return on the next dollar spent instead of the return on the current spend level.
#' @param xy_only If TRUE, return a dataframe of the x-y points for the plot. FALSE by default.
#' @param inflection_point If TRUE, the plot will be annotated at the inflection point.
#' @param x_scale A scale function applied to the x-axis labels and the x value of the inflection point
#' @param y_scale A scale function applied to the y-axis labels and the y value of the inflection point
#' @param ... Arguments passed to the text geom with the inflection point
#' 
#' @return If xy_only is false, a ggplot2 object is returned. Otherwise, a dataframe with x-y coordinates is returned.
#' 
#' @export
plot_diminishing_returns <- function(object, channel, rate = FALSE, xy_only = FALSE, inflection_point = TRUE, x_scale = scales::dollar_format(), y_scale = scales::dollar_format(), ...){

    hyps <- coef(object, complete = TRUE, params = TRUE) %>%
        dplyr::filter(.data$predictors == channel)
    
    seqlen <- 200
    
    xs <- seq(from = 0, to = 3*hyps$gammaTrans, length.out = seqlen)
    xs <- sort(c(xs, hyps$gammaTrans))    

    ys <- saturation_hill_trans(input=xs, alpha=hyps$alphas, gammatrans=hyps$gammaTrans, na.rm = FALSE)*hyps$coef

    yrs <- saturation_hill_trans_deriv(input=xs, alpha=hyps$alphas, gammatrans=hyps$gammaTrans, na.rm = FALSE)*hyps$coef

    inflect_y <- max(yrs[2:length(yrs)])
    inflect_x <- which(yrs == inflect_y)
    inflect <- rep(FALSE, times = length(yrs))
    inflect[inflect_x] <- TRUE
    
    plotframe <- data.frame(Media = xs, Return = ys, `Return Rate` = yrs, `Inflection Point` = inflect)[2:length(xs),]
    
    if(rate){
        plotframe <- plotframe[,-2]
        names(plotframe) <- c(channel, "Return Rate", "Inflection Point")
        if(xy_only){return(plotframe)}
        plotexp <- substitute(
                ggplot2::ggplot(plotframe, ggplot2::aes(x = chan, y = `Return Rate`)),
            list(chan = as.symbol(channel)))
        g <- eval(plotexp)

    } else{
        plotframe <- plotframe[,-3]
        names(plotframe) <- c(channel, "Return", "Inflection Point")
        if(xy_only){return(plotframe)}
        g <- eval(
            substitute(
                ggplot2::ggplot(plotframe, ggplot2::aes(x = chan, y = Return)),
                list(chan = as.symbol(channel))))
    }
    
    g <- g +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste(names(plotframe)[1:2], collapse = " ")) +
        ggplot2::scale_x_continuous(labels = x_scale) +
        ggplot2::scale_y_continuous(labels = y_scale)         
    
    if(inflection_point){
        inflect_x <- plotframe[inflect,]
        
        g <- g + ggplot2::annotate(geom = "point", x = inflect_x[1,1], y = inflect_x[1,2]) +
            ggplot2::geom_text(data = inflect_x,
                              label = paste("(", x_scale(floor(inflect_x[1,1])), ",", y_scale(floor(inflect_x[1,2])), ")", collapse=""), ...)
        }
    
    return(g)

}

#' Plot Diminishing Returns Curves In A Facet Wrap
#'
#' Produces a ggplot2 object of a diminishing returns curve for a specific media channel.
#'
#' @param object A fitted mmmr model
#' @param channels The names of the channels to include. If NULL, all relevant channels will be included.
#' @param rate If TRUE, the y axis will be the return on the next dollar spent instead of the return on the current spend level.
#' @param xy_only If TRUE, return a dataframe of the x-y points for the plot. FALSE by default.
#' @param inflection_point If TRUE, the plot will be annotated at the inflection point.
#' @param x_scale A scale function applied to the x-axis labels and the x value of the inflection point
#' @param y_scale A scale function applied to the y-axis labels and the y value of the inflection point
#' @param nrow,ncol,scale,shrink,drop,dir,strip.position Arguments passed to ggplot2::facep_wrap
#' @param ... Arguments passed to the text geom with the inflection point
#' 
#' @return If xy_only is false, a ggplot2 object is returned. Otherwise, a dataframe with x-y coordinates is returned.
#' 
#' @export
plot_diminishing_returns_facet <- function(object, channels = NULL, rate = FALSE, xy_only = FALSE, inflection_point = TRUE, x_scale = scales::dollar_format(), y_scale = scales::dollar_format(), nrow = NULL, ncol = NULL, scale = "free", shrink = TRUE, drop = TRUE, dir = "h", strip.position = "top", ...){

    hyps <- coef(object, complete = TRUE, params = TRUE)
    
    if(is.null(channels)){
        channels <- hyps %>%
            dplyr::filter(!is.na(alphas) & !is.na(gammas) & coef > 0) %>%
            dplyr::pull(.data$predictors)
    } else{
        for(name in channels){
            check_presence(name, hyps$predictors, namecheck=FALSE)
        }
    }
    
    hyps %<>% dplyr::filter(.data$predictors %in% channels)

    total_data <- purrr::map(channels,
                             .f =
                                 function(chn){
                                     newtbl <- plot_diminishing_returns(object, chn, rate = rate, xy_only = TRUE, inflection_point = inflection_point, x_scale = x_scale, y_scale = y_scale, ...) %>%
                                         dplyr::mutate(channels = chn)                                 
                                     names(newtbl)[1] <- "Media"
                                     return(newtbl)
                                     })

    total_data %<>% purrr::reduce(.f=rbind)

    if(xy_only){return(total_data)}

    if(rate){
        names(total_data) <- c("Channel Exposure", "Return Rate", "Inflection Point", "channels")
        g <- ggplot2::ggplot(total_data, ggplot2::aes(x = .data$`Channel Exposure`, y = .data$`Return Rate`))
    } else{
        names(total_data) <- c("Channel Exposure", "Return", "Inflection Point", "channels")
        g <- ggplot2::ggplot(total_data, ggplot2::aes(x = .data$`Channel Exposure`, y = .data$Return))
    }
    
    g <- g +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste("Media Channel Return", ifelse(rate, "Rate", ""))) +
        ggplot2::scale_x_continuous(labels = x_scale) +
        ggplot2::scale_y_continuous(labels = y_scale) +
        ggplot2::facet_wrap(facets = ggplot2::vars(channels), nrow = nrow, ncol = ncol, scale = scale, shrink = shrink, drop = drop, dir = dir, strip.position = strip.position)
    
    if(inflection_point){

        
        inflect_x <- total_data %>%
            dplyr::filter(`Inflection Point`)

        if(rate){
            inflect_x$lab_text = paste("(",x_scale(inflect_x$`Channel Exposure`),", ", y_scale(inflect_x$`Return Rate`), ")", sep = "")
        } else{
            inflect_x$lab_text = paste("(",x_scale(inflect_x$`Channel Exposure`),", ", y_scale(inflect_x$`Return`), ")", sep = "")
        }
        
        g <- g + ggplot2::geom_point(data = inflect_x) +
            ggplot2::geom_text(data = inflect_x,
                              ggplot2::aes(label = .data$lab_text), ...)
        }
    
    return(g)

}




#' Plot Adstocking Curve
#'
#' Produces a ggplot2 object of an adstocking curve for a specific media channel.
#'
#' @param object A fitted mmmr model
#' @param channel The name of the media channel to plot
#' @param xy_only If TRUE, return a dataframe of the x-y points for the plot. FALSE by default.
#' @param start_value The starting exposure value for the left-most point on the plot.
#' @param ... Arguments passed to a theme object for the plot
#' 
#' @return If xy_only is false, a ggplot2 object is returned. Otherwise, a dataframe with x-y coordinates is returned.
#' 
#' @export
plot_adstocking <- function(object, channel, start_value = NULL, xy_only = FALSE, ...){

    hyps <- coef(object, complete = TRUE, params = TRUE) %>%
        dplyr::filter(.data$predictors == channel)
    
    seqlen <- 10

    if(is.null(start_value)){start_value <- hyps$gammaTrans}
    ys <- start_value

    for(i in 2:5){
        ys[i] <- ys[i-1]*hyps$thetas
        }

    plotframe <- data.frame(`Weeks Out` = 1:5, `Effective Exposure` = ys)
    names(plotframe) <- c("Weeks Out", "Effective Exposure")
    if(xy_only){return(plotframe)}
    
    g <- ggplot2::ggplot(plotframe, ggplot2::aes(x = .data$`Weeks Out`, y = .data$`Effective Exposure`)) +
        ggplot2::labs(title = paste("Effective Exposure Over Time for", channel, collapse = " ")) 
    
    g <- g + ggplot2::geom_line() +
        ggplot2::theme(...)

    return(g)

}

#' Plot Adstocking Curve In A Facet Wrap
#'
#' Produces a ggplot2 object of an adstocking curve for a specific media channel.
#'
#' @param object A fitted mmmr model
#' @param channels The name of the media channels to plot
#' @param start_values The starting exposure values for the left-most point on the plots.
#' @param xy_only If TRUE, return a dataframe of the x-y points for the plot. FALSE by default.
#' @param ... Arguments passed to the ggplot2::facet_wrap function
#' 
#' @return If xy_only is false, a ggplot2 object is returned. Otherwise, a dataframe with x-y coordinates is returned.
#' 
#' @export
plot_adstocking_facet <- function(object, channels = NULL, start_values = NULL, xy_only = FALSE, ...){

    if(is.null(channels)){
        channels <- dplyr::filter(object$hyps, !is.na(thetas) & coef > 0) %>%
            dplyr::pull(.data$predictors)
        } else{
        for(name in channels){
            check_presence(name,
                           dplyr::filter(hyps, !is.na(thetas) & coef > 0)$predictors,
                           namecheck=FALSE)
        }
    }
    
    hyps <- coef(object, complete = TRUE, params = TRUE) %>%
        dplyr::filter(.data$predictors %in% channels)

    if(is.null(start_values)){start_values <- rep(1000, times=length(channels))}
    if(length(start_values) == 1){start_values <- rep(start_values, times=length(channels))}

    plotframe <- purrr::reduce(
                      purrr::map(
                                 1:nrow(hyps),
                                 function(x){
                                     name <- channels[x]
                                     df <- plot_adstocking(object = object, channel = name, start_value = start_values[x], xy_only = TRUE)
                                     df$channels = name
                                     df
                                 }),
                      .f = rbind)
    
    if(xy_only){return(plotframe)}
    
    names(plotframe) <- c("Weeks Out", "Effective Exposure", "channels")
    
    g <- ggplot2::ggplot(plotframe, ggplot2::aes(x = .data$`Weeks Out`, y = .data$`Effective Exposure`)) +
        ggplot2::labs(title = "Effective Exposure Over Time") +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(facets = ggplot2::vars(channels)) +
        ggplot2::theme(...)

    return(g)

}
