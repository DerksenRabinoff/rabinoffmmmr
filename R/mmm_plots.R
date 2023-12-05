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
#' @param ... Arguments passed to a theme object for the plot
#' 
#' @return If xy_only is false, a ggplot2 object is returned. Otherwise, a dataframe with x-y coordinates is returned.
#' 
#' @export
plot_diminishing_returns <- function(object, channel, rate = FALSE, xy_only = FALSE, inflection_point = TRUE, ...){

    hyps <- coef(object, complete = TRUE, params = TRUE) %>%
        dplyr::filter(predictors == channel)

    print(length(hyps$gammaTrans))
    
    seqlen <- 200
    
    repeat{
        xs <- seq(from = 0, to = 2*hyps$gammaTrans, length.out = seqlen)

        ys <- saturation_hill_trans_deriv(xs, hyps$alphas, hyps$gammaTrans)*hyps$coef
        ys_diff <- ys[1:(length(ys)-1)] - ys[2:length(ys)]
        lim <- quantile(ys_diff, probs=.1, na.rm = TRUE)
        cut <- which(ys_diff < lim)
        if(length(cut) == 0){
            seqlen <- seqlen + 50
        } else{break}
    }

    cut <- cut[1]

    xs <- xs[1:cut]
    ys <- ys[1:cut]

    if(!rate){
        ys <- saturation_hill_trans(xs, hyps$alphas, hyps$gammaTrans)*hyps$coef
    }
    plotframe <- data.frame(Media = xs, Return = ys)[2:length(xs),]
    if(rate){
        names(plotframe) <- c(channel, "Return Rate")
        plotexp <- substitute(
                ggplot2::ggplot(plotframe, ggplot2::aes(x = chan, y = `Return Rate`)),
            list(chan = as.symbol(channel)))
        g <- eval(plotexp)

    } else{
        names(plotframe) <- c(channel, "Return")
        g <- eval(
            substitute(
                ggplot2::ggplot(plotframe, ggplot2::aes(x = chan, y = Return)),
                list(chan = as.symbol(channel))))
    }

    if(xy_only){return(plotframe)}
    
    g <- g +
        ggplot2::geom_line() +
        ggplot2::labs(title = paste(names(plotframe), collapse = " ")) +
        ggplot2::theme(...)

    print(hyps$gammaTrans)
    
    if(inflection_point){
        inflect_x <- plotframe[min(abs(xs - hyps$gammaTrans)),]
        g <- g + ggplot2::annotate(geom = "point", x = inflect_x[1,1], y = inflect_x[1,2]) +
            ggplot2::annotate(geom = "text",
                              x = inflect_x[1,1],
                              y = inflect_x[1,2],
                              label = paste("(", floor(inflect_x[1,1]), ",", floor(inflect_x[1,2]), ")", collapse=""))
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
#' @param ... Arguments passed to a theme object for the plot
#' 
#' @return If xy_only is false, a ggplot2 object is returned. Otherwise, a dataframe with x-y coordinates is returned.
#' 
#' @export
plot_adstocking <- function(object, channel, xy_only = FALSE, ...){

    hyps <- coef(object, complete = TRUE, params = TRUE) %>%
        dplyr::filter(predictors == channel)

    print(length(hyps$gammaTrans))
    
    seqlen <- 10

    xs <- rep(0, times = seqlen)

    xs[1] <- hyps$gammaTrans

    ys <- adstock_geometric(xs, hyps$thetas)

    plotframe <- data.frame(Media = xs, `Effective Exposure` = ys)
    g <- ggplot2::ggplot(ggplot2::aes(x = Media, y = `Effective Exposure`))        

    if(xy_only){return(plotframe)}
    
    g <- g + ggplot2::geom_line() +
        ggplot2::theme(...)

    return(g)

}
