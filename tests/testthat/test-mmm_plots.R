test_that("plot_diminishing_returns works", {
    
    expect_s3_class(plot_diminishing_returns(model_fit, "TV Spend"), "ggplot")
    expect_s3_class(plot_diminishing_returns(model_fit, "TV Spend", rate = TRUE), "ggplot")

    dim_ret_plot <- plot_diminishing_returns(model_fit, "TV Spend")
    dim_ret_rate_plot <- plot_diminishing_returns(model_fit, "TV Spend", rate = TRUE)

    expect_s3_class(dim_ret_plot, "gg")
    expect_s3_class(dim_ret_rate_plot, "gg")
})
