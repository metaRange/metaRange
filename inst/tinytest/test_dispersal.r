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

abu <- matrix(0, nrow = n, ncol = n)
abu[n / 2, n / 2] <- 9
kernel <- matrix(1, nrow = 3, ncol = 3)
res1 <- dispersal(
    dispersal_kernel = kernel,
    abundance = abu
)
expect_equal(
    sum(abu),
    sum(res1),
    tolerance = 0.001,
    info = "dispersal doesn't loose idividuals."
)
expect_true(
    {
        m <- n / 2
        sd(res1[(m - 1) :(m + 1), (m - 1) : (m + 1)]) < 0.001
    },
    info = "kernel is correctly applied."
)

abu <- matrix(0, nrow = n, ncol = n)
abu[n / 2, 1] <- 6
abu[n / 2, 2:(n-1)] <- 9
abu[n / 2, n] <- 6
kernel <- matrix(1, nrow = 3, ncol = 3)
res1 <- dispersal(
    dispersal_kernel = kernel,
    abundance = abu
)
expect_true(
    {
        all(
            c(res1[n / 2, c(1,n)] == 2,
            res1[n / 2, 2:(n-1)] == 3)
        )
    },
    info = "edge effects are only because of missing influce cells beyond the edge."
)
rm(n, n2, abu, suitab, kernel, res1, res2)
