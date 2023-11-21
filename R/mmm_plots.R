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
#' 
#' @return If xy_only is false, a ggplot2 object is returned. Otherwise, a dataframe with x-y coordinates is returned.
#' 
#' @export
plot_diminishing_returns <- function(object, channel, rate = FALSE, xy_only = FALSE){

    hyps <- coef(object, complete = TRUE, params = TRUE) %>%
        dplyr::filter(predictors = channel)

    xs <- seq(from = 0, to = 2*hyps$gammaTrans, length.out = 200)

    xs_diff <- xs[2:length(xs)] - xs[1:(length(xs)-1)]
                                     
    
}
