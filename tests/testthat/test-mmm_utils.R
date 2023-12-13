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
