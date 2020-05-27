hmp_value <- function(x, y, n_tests=50, k=0) {

    if (dim(x)[1] != dim(y)[1]) {
        stop("X and Y have different number of observations!")
    }

    d_x <- dim(x)[2]
    d_y <- dim(y)[2]
    n <- dim(x)[1]



    # R passes all parameters as pointers, the function called must be able to handle this.
    ret <- .C("hmp_value_r",
        as.numeric(as.vector(t(x))),
        as.numeric(as.vector(t(y))),
        as.integer(d_x),
        as.integer(d_y),
        as.integer(n),
        hmp_x=as.numeric(0),
        avg_zeta_x=as.numeric(0),
        hmp_y=as.numeric(0),
        avg_zeta_y=as.numeric(0),
        as.integer(n_tests),
        as.integer(k)
    )

    return(list("hmp_x" = ret$hmp_x, "avg_zeta_x" = ret$avg_zeta_x, "hmp_y" = ret$hmp_y, "avg_zeta_y" = ret$avg_zeta_y))

}