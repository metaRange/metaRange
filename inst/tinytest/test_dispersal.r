n <- 10
n2 <- n^2
abu <- matrix(1:n2, nrow = n, ncol = n)
suitab <- matrix(seq(1, 0.01, length.out = n2), nrow = n, ncol = n)
kernel <- calculate_dispersal_kernel(
    max_dispersal_dist = 5,
    kfun = negative_exponential_function,
    mean_dispersal_dist = 1.2)
res1 <- dispersal(
    dispersal_kernel = kernel,
    abundance = abu
)
res2 <- dispersal(
    dispersal_kernel = kernel,
    abundance = abu,
    suitability = suitab
)
expect_equal(
    sum(res1),
    sum(res2),
    tolerance = 0.001,
    info = "dispersal with and without suitability redistribution results in the same sum of abundances."
)

suitab <- matrix(1, nrow = n, ncol = n)
res2 <- dispersal(
    dispersal_kernel = kernel,
    abundance = abu,
    suitability = suitab
)
expect_equal(
    res1,
    res2,
    tolerance = 0.001,
    info = "dispersal with a suitability of 1 gives the same result as when calling the function without it."
)

rm(n, n2, abu, suitab, kernel, res1, res2)
