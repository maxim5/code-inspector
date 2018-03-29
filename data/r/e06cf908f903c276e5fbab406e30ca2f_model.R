YodzisInnesDyDt <- function(time, B, params)
{
    # Implements the Yodzis and Innes model equations.
    # Returns dydt.

    # Delegates to the fast C implementation
    # My implementation in C requires growth and respiration vectors and 
    # consumption and assimilation matrices.
    if(any(is.na(B) | is.infinite(B)))
    {
        msg <- 'At least one value in B is NA or Inf'
        save(msg, time, B, params, 
             file=paste(.UTCTimeString(), 'YI.model.error.out', sep='-'))
        stop(msg)
    }

    res <- .C('YodzisInnesState', params$n.species, params$K, params$a, 
              params$q, params$d, params$W, 
              params$producers.c, params$n.producers, 
              params$consumers.c, params$n.consumers,
              params$rho, params$x, params$y, params$e, params$fe, B, 
              dydt=numeric(params$n.species), 
              numeric(params$n.species), # growth
              numeric(params$n.species), # respiration
              numeric(params$n.species*params$n.species), # assimilation
              numeric(params$n.species*params$n.species), # consumption
              NAOK=TRUE, DUP=FALSE)

    # We don't do anything with the last 4 output parameters - we are only 
    # interested in dydt.
    dydt <- res$dydt

    if(any(is.na(dydt) | is.infinite(dydt)))
    {
        msg <- 'At least one value in dydt is NA or Inf'
        save(msg, time, B, params, dydt, 
             file=paste(.UTCTimeString(), 'YI.model.error.out', sep='-'))
        stop(msg)
    }

    # Have any species come back from the dead?
    if(any(dydt!=0.0 & B==0.0))
    {
        msg <- 'One or more species have come back from the dead'
        save(msg, time, B, params, dydt, 
             file=paste(.UTCTimeString(), 'YI.model.error.out', sep='-'))
        stop(msg)
    }

    return (list(dydt=dydt,globals=NULL))
}

YodzisInnesFlux <- function(B, params)
{
    # Returns growth, respiration, consumption and assimilation terms
    res <- .C('YodzisInnesState', params$n.species, params$K, params$a, 
              params$q, params$d, params$W, 
              params$producers.c, params$n.producers, 
              params$consumers.c, params$n.consumers,
              params$rho, params$x, params$y, params$e, params$fe, B, 
              dydt=numeric(params$n.species), 
              growth=numeric(params$n.species), 
              respiration=numeric(params$n.species), 
              assimilation=numeric(params$n.species*params$n.species), 
              consumption=numeric(params$n.species*params$n.species), 
              NAOK=TRUE, DUP=FALSE)

    dim(res$assimilation) <- dim(res$consumption) <- 
            c(params$n.species, params$n.species)
    # Not interested in dydt here.
    return(res[c('growth', 'respiration', 'assimilation', 'consumption')])
}

YodzisInnesDyDt_R <- function(time, B, params)
{
    # Implements the Yodzis and Innes model equations.
    # Returns dydt.

    # A slower implementation in R.
    # Used to test the C implementation. 

    # Call YodzisInnesFlux() - helper function at the bottom of this file  - 
    # that returns a list containing:
    #   growth - gains from growth, as in eq 2.17
    #   respiration - losses to respiration, as in q 2.18
    #   consumption - losses to being eaten by others, eq 2.17 and 2.18
    #   assimilation - gains from eating by others, eq 2.18

    if(any(is.na(B) | is.infinite(B)))
    {
        msg <- 'At least one value in B is NA or Inf'
        save(msg, time, B, params, 
             file=paste(.UTCTimeString(), 'YI.model.error.out', sep='-'))
        stop(msg)
    }

    flux <- YodzisInnesFlux_R(B, params)

    dydt <- rep(NA, length(B))
    dydt[params$producers] <- flux$growth
    dydt[params$consumers] <- flux$respiration

    dydt <- dydt - rowSums(flux$consumption, na.rm=TRUE) + 
            colSums(flux$assimilation, na.rm=TRUE)

    if(any(is.na(dydt) | is.infinite(dydt)))
    {
        msg <- 'At least one value in dydt is NA or Inf'
        save(msg, time, B, params, flux, dydt, 
             file=paste(.UTCTimeString(), 'YI.model.error.out', sep='-'))
        stop(msg)
    }

    # Have any species come back from the dead?
    if(any(dydt!=0.0 & B==0.0))
    {
        msg <- 'One or more species have come back from the dead'
        save(msg, time, B, params, flux, dydt, 
             file=paste(.UTCTimeString(), 'YI.model.error.out', sep='-'))
        stop(msg)
    }

    return(list(dydt=dydt,globals=NULL))
}

YodzisInnesFlux_R <- function(B, params)
{
    # Returns growth, respiration, consumption and assimilation terms
    # There is C function that returns flux
    with(params, 
    {
        growth <- rho[producers] * B[producers] * 
                  (1-(a[producers,producers]%*%B[producers])/K)
        respiration <- -x[consumers] * B[consumers]

        # An n x n matrix
        fr.numerator <- (matrix(B, ncol=n.species, nrow=n.species)/W)^(1+q)
        fr.denominator <- 1 + d*B + colSums(fr.numerator, na.rm=TRUE)
        fr.denominator <- matrix(fr.denominator, nrow=n.species, 
                                 ncol=n.species, byrow=TRUE)
        functional.response <- fr.numerator/fr.denominator

        # Each non NA element shows the amount of biomass lost by i to j
        assimilation <- matrix(x, nrow=n.species, ncol=n.species, byrow=TRUE)*
                        matrix(B, nrow=n.species, ncol=n.species, byrow=TRUE)*
                        y * 
                        functional.response

        consumption <- assimilation / (fe*e)

        return(list(growth=growth, 
                    respiration=respiration,
                    consumption=consumption,
                    assimilation=assimilation))
    })
}

