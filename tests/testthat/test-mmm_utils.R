test_that("saturation_hill_trans works", {
    expect_equal( 
        saturation_hill_trans(c(1,2,3,4,5,6), .3, 4),
        c(0.39750106, 0.44820048, 0.47843723, 0.50000000, 0.51672952, 0.53037244), tolerance = testthat_tolerance())
    
    expect_equal(saturation_hill_trans(c(-1,-2,3,48,5,6), 3, 20),
                 c(-0.0001250156, -0.0010010010, 0.0033636477, 0.9325418241, 0.0153846154, 0.0262901655))

    rand_gamma <- stats::runif(n=10, min=.1, max=1)
    rand_alpha <- stats::runif(n=10, min=.1, max=5)
    rand_gamma_trans <- rand_gamma * stats::runif(n=1,min=10, max=10000)
    
    expect_true(all(saturation_hill_trans(runif(n=6, min=-10, max=500),
                                       rand_alpha[1], 0) == 1))
    expect_true(all(saturation_hill_trans(runif(n=6, min=-10, max=500),
                                       rand_alpha[2],
                                       1000000000000000000000) <= .0000001))
    expect_true(all(saturation_hill_trans(runif(n=6, min=-10, max=500),
                                       0,
                                       1000000000000000000000) == .5))

    rand_input <- sort(stats::runif(n=10, min=-10, max=1000))
    
    expect_equal(saturation_hill_trans(rand_input, rand_alpha[3], rand_gamma_trans[1]),
                 sort(saturation_hill_trans(rand_input, rand_alpha[3], rand_gamma_trans[1])))
    
})

test_that("saturation_hill_trans_deriv works", {

    rand_gamma <- stats::runif(n=10, min=.1, max=1)
    rand_alpha <- stats::runif(n=10, min=.1, max=5)
    rand_gamma_trans <- rand_gamma * stats::runif(n=1,min=10, max=10000)
    rand_input <- sort(stats::runif(n=100, min=-10, max=1000))
    e <- .1
    ((saturation_hill_trans(rand_input + e, rand_alpha[1], rand_gamma_trans[1]) -
      saturation_hill_trans(rand_input - e, rand_alpha[1], rand_gamma_trans[1]))/(2*e)) -
        saturation_hill_trans_deriv(rand_input, rand_alpha[1], rand_gamma_trans[1])
    
    for(gam in rand_gamma_trans){
        for(alph in rand_alpha){
            expect_equal((saturation_hill_trans(rand_input + e, alph, gam) -
                          saturation_hill_trans(rand_input - e, alph, gam))/(2*e),
                         saturation_hill_trans_deriv(rand_input, alph, gam),
                         tolerance = .001)
        }
    }

})

test_that("adstock_geometric works", {

    rand_input <- stats::runif(n=100, min=-10, max=1000)

    expect_equal(adstock_geometric(rand_input, 0), rand_input)

    expect_true(all(rand_input <= adstock_geometric(rand_input, stats::runif(n=1, min=0, max=1000))))
})

test_that("prophetize_df works", {

    prophetized <- prophetize_df(historical_ad_spends, dep_col = "Sales", date_col = "Week Starting")

    for(colname in names(historical_ad_spends)){
        expect_equal(historical_ad_spends[[colname]], prophetized[[colname]])
    }

    expect_named(prophetized, c(names(historical_ad_spends), "yearly", "trend", "holidays"))

    expect_true(is.numeric(prophetized$yearly))
    expect_true(is.numeric(prophetized$trend))
    expect_true(is.numeric(prophetized$holidays))
    
})

test_that("apply_media_transforms works", {

    hyps <- data.frame(
        predictors = c("TV Spend", "Radio Spend", "Online Video Spend", "Social Media Spend", "Search Ads Spend", "Direct Mail Spend"),
        alphas = c(.5, .7, 2, 3, NA, NA),
        gammas = c(.4, .3, .8, 1, NA, NA),
        thetas = c(NA, .2, .5, NA, NA, .9),
        gammaTrans = c(4444, 3333, 2222, 1111, NA, NA)
    )

    new_media_transform <- apply_media_transforms(historical_ad_spends, hyps)

    new_media_transform_gt <- apply_media_transforms(historical_ad_spends, hyps, compute_gammatrans=FALSE)
    
    expect_true(all(new_media_transform$`Search Ads Spend` == historical_ad_spends$`Search Ads Spend`))
    radio_adstocked <- adstock_geometric(historical_ad_spends$`Radio Spend`, .2)
    
    expect_true(all(new_media_transform$`Radio Spend` ==
                    radio_adstocked %>%
                    saturation_hill_trans(
                        .7,
                        gamma_to_gammatrans(radio_adstocked, .3))
                    ))

    expect_true(all(new_media_transform$`Direct Mail Spend` == adstock_geometric(historical_ad_spends$`Direct Mail Spend`, .9)))

    expect_true(all(new_media_transform$`TV Ads` ==
                    saturation_hill_trans(
                        historical_ad_spends$`TV Ads`,
                        .5,
                        gamma_to_gammatrans(historical_ad_spends$`TV Ads`, .4))))

    expect_true(all(new_media_transform_gt$`Radio Spend` ==
                    radio_adstocked %>%
                    saturation_hill_trans(
                        .7,
                        3333)
                    ))
    
})
