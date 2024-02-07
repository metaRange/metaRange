# Metabolic scaling
#
# A function to calculate the metabolic scaling of a parameter.
#
# @param normalization_constant `<numeric>` normalization constant.
# @param scaling_exponent `<numeric>` allometric scaling exponent of the mass.
# @param mass `<numeric>`  mean (individual) mass.
# @param temperature `<numeric>` temperature in kelvin (K).
# @param E `<numeric>` Activation energy in electronvolts (eV).
# @param k `<numeric>` Boltzmann's constant (eV / K).
# @details
# General notes:
#
# Reproduction rate is generally assumed to scale with an exponent of `-1/4`
# and an activation energy of `-0.65 eV` (important: it's negative).
#
# Carry capacity is generally assumed to scale with an exponent of `-3/4`
# and an activation energy of `0.65 eV` (important: it's positive).
# But read: (Brown et. al. 2004; Brown, Sibly and Kodric-Brown 2012) for an in-depth explanation.
#
# Notes on units:
#
# 1 electronvolt = 1.602176634 * 10^-19 Joule
#
# Boltzmann constant 1.380649 * 10^-23 Joule/Kelvin
#
# Boltzmann constant in eV/K = 8.617333e-05 = (1.380649 * 10^-23) / (1.602176634 * 10^-19)
# @references
# Brown, J.H., Gillooly, J.F., Allen, A.P., Savage, V.M. and West, G.B. (2004), Toward a Metabolic Theory of Ecology. Ecology, 85: 1771-1789. https://doi.org/10.1890/03-9000
#
# Brown, J.H., Sibly, R.M. and Kodric-Brown, A. (2012). Introduction: Metabolism as the Basis for a Theoretical Unification of Ecology. In Metabolic Ecology (eds R.M. Sibly, J.H. Brown and A. Kodric-Brown). https://doi.org/10.1002/9781119968535.ch
# @return The scaled parameter.
# @export
metabolic_scaling_R <- function(normalization_constant,
                                scaling_exponent,
                                mass,
                                temperature,
                                E = NULL,
                                k = 8.617333e-05) {
    checkmate::assert_numeric(normalization_constant, len = 1)
    checkmate::assert_numeric(scaling_exponent, len = 1)
    checkmate::assert_numeric(E, len = 1)
    checkmate::assert_numeric(k, len = 1)
    verbosity <- getOption("metaRange.verbose", default = FALSE)
    if (verbosity && scaling_exponent == -1 / 4 && E > 0) {
        message(
            "Respiratory rates are generally assumed to scale with:\n",
            "exponent ~ [-1/4]\n",
            "activation energy ~ [-0.65] eV.\n",
            "(Feel free to ignore this message if you know what you are doing)."
        )
    }
    if (verbosity && scaling_exponent == -3 / 4 && E < 0) {
        message(
            "Carrying capacitiy is generally assumed to scale with:\n",
            "exponent ~ [-3/4]\n",
            "activation energy ~ [0.65] eV.\n",
            "(Feel free to ignore this message if you know what you are doing)."
        )
    }
    normalization_constant * mass^scaling_exponent * exp(E / (k * temperature))
}





# ================ carrying capacity - temperature ==================

test_mte_carrying_capactiy_temerature <- function() {
    expon <- -3 / 4
    rtemp <- 273 + 20
    bmass <- 100
    E <- 0.65
    k <- 8.617333e-05
    val <- 1000
    const <- calculate_normalization_constant(
        parameter_value = val,
        scaling_exponent = expon,
        mass = bmass,
        reference_temperature = rtemp,
        E = E,
        k = k
    )


    higher_temp_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp + 10,
        E = E,
        k = k
    )

    ref_temp_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp,
        E = E,
        k = k
    )

    lower_temp_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp - 10,
        E = E,
        k = k
    )

    higher_temp_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp + 10),
        E = E,
        k = k
    )[1, 1]

    ref_temp_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )[1, 1]

    lower_temp_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp - 10),
        E = E,
        k = k
    )[1, 1]

    if (
        val == ref_temp_R &&
            higher_temp_R < ref_temp_R && # less individuals at higher temp
            lower_temp_R > ref_temp_R && # more individuals at lower temp

            val == ref_temp_cpp &&
            higher_temp_cpp < ref_temp_cpp && # less individuals at higher temp
            lower_temp_cpp > ref_temp_cpp && # more individuals at lower temp

            ref_temp_R == ref_temp_cpp &&
            higher_temp_R == higher_temp_cpp &&
            lower_temp_R == lower_temp_cpp
    ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# ================ carrying capacity - mass ==================
test_mte_carrying_capactiy_mass <- function() {
    expon <- -3 / 4
    rtemp <- 273 + 20
    bmass <- 100
    E <- 0.65
    k <- 8.617333e-05
    val <- 1000
    const <- metaRange::calculate_normalization_constant(
        parameter_value = val,
        scaling_exponent = expon,
        mass = bmass,
        reference_temperature = rtemp,
        E = E,
        k = k
    )


    higher_mass_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass + 10,
        temperature = rtemp,
        E = E,
        k = k
    )

    ref_mass_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp,
        E = E,
        k = k
    )

    lower_mass_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass - 10,
        temperature = rtemp,
        E = E,
        k = k
    )


    higher_mass_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass + 10),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )

    ref_mass_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )

    lower_mass_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass - 10),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )

    if (
        val == ref_mass_R &&
            higher_mass_R < ref_mass_R && # less individuals at higher mass
            lower_mass_R > ref_mass_R && # more individuals at lower mass

            val == ref_mass_cpp &&
            higher_mass_cpp < ref_mass_cpp && # less individuals at higher mass
            lower_mass_cpp > ref_mass_cpp && # more individuals at lower mass

            ref_mass_R == ref_mass_cpp &&
            higher_mass_R == higher_mass_cpp &&
            lower_mass_R == lower_mass_cpp
    ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# ================ reproduction rate - temperature ==================
test_mte_reproduction_rate_temerature <- function() {
    expon <- -1 / 4
    rtemp <- 273 + 20
    bmass <- 1
    E <- -0.65
    k <- 8.617333e-05
    val <- 0.2
    const <- metaRange::calculate_normalization_constant(
        parameter_value = val,
        scaling_exponent = expon,
        mass = bmass,
        reference_temperature = rtemp,
        E = E,
        k = k
    )


    higher_temp_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp + 10,
        E = E,
        k = k
    )

    ref_temp_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp,
        E = E,
        k = k
    )

    lower_temp_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp - 10,
        E = E,
        k = k
    )

    higher_temp_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp + 10),
        E = E,
        k = k
    )[1, 1]

    ref_temp_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )[1, 1]

    lower_temp_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp - 10),
        E = E,
        k = k
    )[1, 1]

    if (
        val == ref_temp_R &&
            higher_temp_R > ref_temp_R && # higher reproduction at higher temp
            lower_temp_R < ref_temp_R && # lower reproduction at lower temp

            val == ref_temp_cpp &&
            higher_temp_cpp > ref_temp_cpp && # higher reproduction at higher temp
            lower_temp_cpp < ref_temp_cpp && # lower reproduction at lower temp

            ref_temp_R == ref_temp_cpp &&
            higher_temp_R == higher_temp_cpp &&
            lower_temp_R == lower_temp_cpp
    ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
# ================ reproduction rate - mass ==================
test_mte_reproduction_rate_mass <- function() {
    expon <- -1 / 4
    rtemp <- 273 + 20
    bmass <- 100
    E <- -0.65
    k <- 8.617333e-05
    val <- 1000
    const <- metaRange::calculate_normalization_constant(
        parameter_value = val,
        scaling_exponent = expon,
        mass = bmass,
        reference_temperature = rtemp,
        E = E,
        k = k
    )


    higher_mass_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass + 10,
        temperature = rtemp,
        E = E,
        k = k
    )

    ref_mass_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass,
        temperature = rtemp,
        E = E,
        k = k
    )

    lower_mass_R <- metabolic_scaling_R(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = bmass - 10,
        temperature = rtemp,
        E = E,
        k = k
    )


    higher_mass_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass + 10),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )

    ref_mass_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )

    lower_mass_cpp <- metabolic_scaling(
        normalization_constant = const,
        scaling_exponent = expon,
        mass = matrix(bmass - 10),
        temperature = matrix(rtemp),
        E = E,
        k = k
    )

    if (
        val == ref_mass_R &&
            higher_mass_R < ref_mass_R && # lower reproduction with higher mass
            lower_mass_R > ref_mass_R && # higher reproduction with lower mass

            val == ref_mass_cpp &&
            higher_mass_cpp < ref_mass_cpp && # lower reproduction with higher mass
            lower_mass_cpp > ref_mass_cpp && # higher reproduction with lower mass

            ref_mass_R == ref_mass_cpp &&
            higher_mass_R == higher_mass_cpp &&
            lower_mass_R == lower_mass_cpp
    ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

expect_true(test_mte_carrying_capactiy_temerature(),
    info = "testing that carying capacity scales correct with temp"
)
expect_true(test_mte_carrying_capactiy_mass(),
    info = "testing that carying capacity scales correct with mass"
)
expect_true(test_mte_reproduction_rate_temerature(),
    info = "testing that reproduction rate scales correct with temp"
)
expect_true(test_mte_reproduction_rate_mass(),
    info = "testing that reproduction rate scales correct with mass"
)

expect_error(
    metabolic_scaling(
        normalization_constant = 1,
        scaling_exponent = -1 / 4,
        mass = c(1, 2),
        temperature = 1,
        E = 0.65,
        k = 8.617333e-05
    ),
    info = "error for different sizes of mass & temperature"
)

expect_error(
    metabolic_scaling(
        normalization_constant = 1,
        scaling_exponent = c(2,2),
        mass = 1,
        temperature = 1,
        E = 0.65,
        k = 8.617333e-05
    ),
    info = "error for single number as scaling exponent"
)

expect_error(
    metabolic_scaling(
        normalization_constant = 1,
        scaling_exponent = -1 / 4,
        mass = 1,
        temperature = 1,
        E = c(0.65, 0.65),
        k = 8.617333e-05
    ),
    info = "error for single number as E"
)

expect_error(
    metabolic_scaling(
        normalization_constant = 1,
        scaling_exponent = -1 / 4,
        mass = 1,
        temperature = 1,
        E = 0.65,
        k = c(8.617333e-05, 1)
    ),
    info = "error for single number as k"
)


expect_message(
    calculate_normalization_constant(
        parameter_value = 1,
        scaling_exponent = -1 / 4,
        mass = 1,
        reference_temperature = 273.15,
        E = 0.65,
        warn_if_possibly_false_input = TRUE
    )
)

expect_message(
    calculate_normalization_constant(
        parameter_value = 1,
        scaling_exponent = -3 / 4,
        mass = 1,
        reference_temperature = 273.15,
        E = -0.65,
        warn_if_possibly_false_input = TRUE
    )
)

expon <- -3 / 4
rtemp <- 273 + 20
bmass <- 100
E <- 0.65
k <- 8.617333e-05
val <- 1000
const <- calculate_normalization_constant(
    parameter_value = val,
    scaling_exponent = expon,
    mass = bmass,
    reference_temperature = rtemp,
    E = E,
    k = k
)
test_mass <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
test_temp <- matrix(rtemp:(rtemp + 3), nrow = 2, ncol = 2)
res <- metabolic_scaling(
    normalization_constant = const,
    scaling_exponent = expon,
    mass = test_mass,
    temperature = test_temp,
    E = E,
    k = k
)

expect_true(
    all(dim(res) == c(2, 2)),
    info = "testing that dimensionality is preserved"
)

res <- metabolic_scaling(
    normalization_constant = const,
    scaling_exponent = expon,
    mass = bmass,
    temperature = rtemp + 10,
    E = E,
    k = k
)
expect_true(
    is.null(dim(res)),
    info = "testing that dimensionality is preserved pt.2"
)
