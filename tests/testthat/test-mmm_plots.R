test_that("plot_diminishing_returns works", {
    
    expect_s3_class(plot_diminishing_returns(model_fit, "TV Spend"), "ggplot")
    expect_s3_class(plot_diminishing_returns(model_fit, "TV Spend", rate = TRUE), "ggplot")
    expect_s3_class(plot_diminishing_returns(model_fit, "TV Spend", rate = TRUE, xy_only = TRUE), "data.frame")
    expect_s3_class(plot_diminishing_returns(model_fit, "TV Spend", rate = TRUE, inflection_point=FALSE), "ggplot")

    dim_ret_plot <- plot_diminishing_returns(model_fit, "TV Spend")
    dim_ret_rate_plot <- plot_diminishing_returns(model_fit, "TV Spend", rate = TRUE)

    expect_s3_class(dim_ret_plot, "gg")
    expect_s3_class(dim_ret_rate_plot, "gg")
})

test_that("plot_diminishing_returns_facet works", {
    
    expect_s3_class(plot_diminishing_returns_facet(model_fit), "ggplot")
    expect_s3_class(plot_diminishing_returns_facet(model_fit, rate = TRUE), "ggplot")
    expect_s3_class(plot_diminishing_returns_facet(model_fit, xy_only = TRUE), "data.frame")

    dim_ret_plot <- plot_diminishing_returns_facet(model_fit)
    dim_ret_rate_plot <- plot_diminishing_returns_facet(model_fit, rate = TRUE)

    expect_s3_class(dim_ret_plot, "gg")
    expect_s3_class(dim_ret_rate_plot, "gg")
})

test_that("plot_adstocking works", {
    
    expect_s3_class(plot_adstocking(model_fit, "TV Spend"), "ggplot")
    expect_s3_class(plot_adstocking(model_fit, "TV Spend", xy_only = TRUE), "data.frame")

    dim_ret_plot <- plot_adstocking(model_fit, "TV Spend")
    dim_ret_rate_plot <- plot_adstocking(model_fit, "TV Spend", rate = TRUE)

    expect_s3_class(dim_ret_plot, "gg")
    expect_s3_class(dim_ret_rate_plot, "gg")
})

test_that("plot_adstocking_facet works", {
    
    expect_s3_class(plot_adstocking_facet(model_fit), "ggplot")
    expect_s3_class(plot_adstocking_facet(model_fit, xy_only = TRUE), "data.frame")

    dim_ret_plot <- plot_adstocking_facet(model_fit)
    dim_ret_rate_plot <- plot_adstocking_facet(model_fit, rate = TRUE)

    expect_s3_class(dim_ret_plot, "gg")
    expect_s3_class(dim_ret_rate_plot, "gg")
})
