RunSim <- function(model, params, max.time=1000, sampling.interval=0.1, 
                   B0=Biomass(params$community))
{
    # A testing helper that run a model and returns the resulting time series.
    simulation <- ODESimulation(model=model, 
                                params=params, 
                                sampling.interval=sampling.interval,
                                extinction.threshold=1e-20)

    # Collect simulation results in memory
    collector <- CollectChunksObserver()
    res <- RunSimulation(initial.state=B0, 
                         simulation=simulation,
                         controller=MaxTimeController(max.time=max.time), 
                         observers=list(collector))

    return (GetTimeSeries(collector))
}

TestGrowthModel <- function()
{
    # Test R and C growth model implementations
    GrowthDyDt <- function(time, y, params)
    {
        # Two producer growth model implementation
        R1 <- y[1]
        R2 <- y[2]

        with(params, 
        {
            return (list(c(R1=rho1 * R1 * (1-(a11*R1 + a21*R2)/K), 
                           R2=rho2 * R2 * (1-(a12*R1 + a22*R2)/K)), 
                         globals=NULL))
        })
    }

    # SCENARIO 1 - all competition coefficients equal
    # Run simulation using noddy implementation
    community <- Community(nodes=data.frame(node=c('R1','R2'), M=c(1,1), 
                                            N=c(150,100), 
                                            category=rep('producer', 2)), 
                           properties=list(title='Two producers motif',
                                           M.units='kg', N.units='m^-2'))

    params <- list(rho1=1, rho2=1, a11=1, a12=1, a21=1, a22=1, K=500, 
                   community=community)
    tseries1 <- RunSim(GrowthDyDt, params)

    # Run simulation using YodzisInnes C implementation
    spec <- ModelParamsSpec()
    params <- IntermediateModelParams(community, spec)
    params <- BuildModelParams(community, params) # containing rho,x,z etc
    tseries2 <- RunSim(YodzisInnesDyDt, params)

    # Run simulation using YodzisInnes R implementation
    tseries3 <- RunSim(YodzisInnesDyDt_R, params)

    stopifnot(isTRUE(all.equal(tseries1, tseries2)))
    stopifnot(isTRUE(all.equal(tseries1, tseries3)))

    # SCENARIO 2 - advantage for R1
    # Run simulation using noddy implementation
    params <- list(rho1=1, rho2=1, a11=1, a12=1.1, a21=1, a22=1, K=500, 
                   community=community)
    tseries1 <- RunSim(GrowthDyDt, params)

    # Run simulation using YodzisInnes C implementation
    spec <- ModelParamsSpec()
    params <- IntermediateModelParams(community, spec)
    params <- BuildModelParams(community, params) # containing rho,x,z etc
    params$a['R2','R1'] <- 1.1
    tseries2 <- RunSim(YodzisInnesDyDt, params)

    # Run simulation using YodzisInnes R implementation
    tseries3 <- RunSim(YodzisInnesDyDt_R, params)

    stopifnot(isTRUE(all.equal(tseries1, tseries2)))
    stopifnot(isTRUE(all.equal(tseries1, tseries3)))

    # SCENARIO 3 - advantage for R1 with a different order of producers. 
    # This is to test the matrix multiplication stuff in the C implementation.
    community <- Community(nodes=data.frame(node=c('R2','R1'), M=c(1,1), 
                                            N=c(100,150), 
                                            category=rep('producer', 2)), 
                           properties=list(title='Two producers motif',
                                           M.units='kg', N.units='m^-2'))

    # Run simulation using YodzisInnes C implementation
    spec <- ModelParamsSpec()
    params <- IntermediateModelParams(community, spec)
    params <- BuildModelParams(community, params) # containing rho,x,z etc
    params$a['R2','R1'] <- 1.1
    tseries2 <- RunSim(YodzisInnesDyDt, params)

    # Run simulation using YodzisInnes R implementation
    tseries3 <- RunSim(YodzisInnesDyDt_R, params)

    stopifnot(isTRUE(all.equal(tseries1, tseries2[,c('time','R1','R2')])))
    stopifnot(isTRUE(all.equal(tseries1, tseries3[,c('time','R1','R2')])))


    # SCENARIO 4 - advantage for R1 with some non-connected consumers and a 
    # different order of producers. This is to test the matrix multiplication 
    # stuff in the C implementation.
    community <- Community(nodes=data.frame(node=c('C1','R2','C2','R1','C3'), 
                                            M=rep(1, 5), 
                                            N=c(1,100,2,150,3), 
                                            category=c('invertebrate',
                                                       'producer',
                                                       'invertebrate',
                                                       'producer',
                                                       'invertebrate')), 
                           properties=list(title='Two producers motif',
                                           M.units='kg', N.units='m^-2'))

    # Run simulation using YodzisInnes C implementation
    spec <- ModelParamsSpec()
    params <- IntermediateModelParams(community, spec)
    params <- BuildModelParams(community, params) # containing rho,x,z etc
    params$a['R2','R1'] <- 1.1
    tseries2 <- RunSim(YodzisInnesDyDt, params)

    # Run simulation using YodzisInnes R implementation
    tseries3 <- RunSim(YodzisInnesDyDt_R, params)

    stopifnot(isTRUE(all.equal(tseries1[,], tseries2[,c('time','R1','R2')])))
    stopifnot(isTRUE(all.equal(tseries1[,], tseries3[,c('time','R1','R2')])))
}

TestYodzisInnesModelMotifs <- function()
{
    # Ensures that the R and C model implementations both give the same time
    # series for a number of motifs.
    SingleProducer <- function()
    {
        community <- Community(nodes=data.frame(node='R', category='producer', 
                                                M=10, N=10), 
                               properties=list(title='Single producer', 
                                               M.units='kg', N.units='m^-2'))
        # Model parameters
        spec <- ModelParamsSpec()
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        equilibria <- c(R=params$K)

        return (list(params=params, equilibria=equilibria))
    }

    TwoProducer <- function()
    {
        community <- Community(nodes=data.frame(node=c('R1','R2'), M=c(1,1), 
                                                N=c(150,100), 
                                                category=rep('producer', 2)), 
                               properties=list(title='Two producers motif',
                                               M.units='kg', N.units='m^-2'))

        # Model parameters
        spec <- ModelParamsSpec()
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        # as.numeric() to get rid of names
        aR1R1 <- with(params, a['R1','R1'])
        aR1R2 <- with(params, a['R1','R2'])
        aR2R1 <- with(params, a['R2','R1'])
        aR2R2 <- with(params, a['R2','R2'])
        rhoR1 <- with(params, as.numeric(rho['R1']))
        rhoR2 <- with(params, as.numeric(rho['R2']))
        K <- with(params, K)

        # Equilibria calculated by Mathematica 8 - silly
        equilibria <- c(R1e=-(-aR2R1*K+aR2R2*K) / (aR1R2*aR2R1-aR1R1*aR2R2), 
                        R2e=-( aR1R1*K-aR1R2*K) / (aR1R2*aR2R1-aR1R1*aR2R2))

        N <- NP(community, 'N')
        Ntot <- unname(N['R1'] + N['R2'])
        ratio <- unname(N['R1'] / Ntot)
        equilibria <- c(R1e=params$K * unname(N['R1']) / Ntot, 
                        R2e=params$K * unname(N['R2']) / Ntot)

        return (list(params=params, equilibria=equilibria))
    }

    ResourceConsumer <- function()
    {
        community <- Community(nodes=data.frame(node=c('R','C'), 
                                                M=c(1,5), 
                                                N=c(150,50), 
                                                category=c('producer', 
                                                           'invertebrate')), 
                               trophic.links=data.frame(resource='R', 
                                                        consumer='C'),
                               properties=list(title='Resource-consumer motif', 
                                               M.units='kg', N.units='m^-2'))

        # Model parameters
        spec <- ModelParamsSpec(f.constants=AllFConstantsEqual())
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        # as.numeric() to get rid of names
        rhoR <- with(params, as.numeric(rho['R']))
        K <- with(params, K)
        xC <- with(params, as.numeric(x['C']))
        yRC <- with(params, as.numeric(y['R','C']))
        feRC <- with(params, as.numeric(fe['R','C']))
        eRC <- with(params, as.numeric(e['R','C']))
        WRC <- with(params, as.numeric(W['R','C']))
        q <- with(params, as.numeric(q))

        # Equilibria: eqns 12 and 13 of Yodzis and Innes (1992) on p.1160 
        # using x and y given in eqns 10 and 11, p 1156.
        Re <- WRC / (yRC-1)
        yi.equilibria <- c(Re=Re, 
                           Ce=(feRC*eRC / xC) * Re * (1-Re/K))

        # Equilibria calculated by Mathematica 8. 
        equilibria <- c(Re=WRC*(1/(-1+yRC)) ^ (1/(1+q)), 
                        Ce=-WRC*eRC*feRC*rhoR * 
                           (-K + WRC*(-1/(1-yRC))^(1/(1+q))) * 
                           (-1/(1-yRC))^(1/(1+q)) / (K*xC) )


        # Equilibria from Rall et al 2008 Oikos, equations 7 and 8?

        stopifnot(isTRUE(all.equal(equilibria, yi.equilibria)))

        return (list(params=params, equilibria=equilibria))
    }

    ThreeSpeciesChain <- function()
    {
        community <- Community(nodes=data.frame(node=c('R','C1','C2'), 
                                            M=c(1,5,100), 
                                            N=c(200,20,1), 
                                            category=c('producer', 
                                                       rep('invertebrate', 2))),
                           trophic.links=data.frame(resource=c( 'R', 'C1'), 
                                                    consumer=c('C1', 'C2')), 
                           properties=list(title='Three-species chain motif', 
                                           M.units='kg', N.units='m^-2'))

        # Model parameters
        spec <- ModelParamsSpec(f.constants=AllFConstantsEqual())
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        # Link-specific deviations that will be seen in equilibria
        params$W['R','C1'] <- 5000
        params$fe['R','C1'] <- 0.6

        # Equilibria calculated by Mathematica 8. 
        # as.numeric() to get rid of names
        rhoR <- with(params, as.numeric(rho['R']))
        K <- with(params, K)
        xC1 <- with(params, as.numeric(x['C1']))
        xC2 <- with(params, as.numeric(x['C2']))
        yRC1 <-  with(params, as.numeric(y['R', 'C1']))
        yC1C2 <- with(params, as.numeric(y['C1','C2']))
        feRC1 <-  with(params, as.numeric(fe['R', 'C1']))
        feC1C2 <- with(params, as.numeric(fe['C1','C2']))
        eRC1 <-  with(params, as.numeric(e['R', 'C1']))
        eC1C2 <- with(params, as.numeric(e['C1','C2']))
        WRC1 <-  with(params, as.numeric(W['R', 'C1']))
        WC1C2 <- with(params, as.numeric(W['C1','C2']))
        q <- with(params, as.numeric(q))

        equilibria <- c(Re=WRC1/(-1 + yRC1), 
                        C1e=-((WRC1*eRC1*feRC1*rhoR*(WRC1 + K - K*yRC1)) / 
                             (K*xC1*(-1 + yRC1)^2)),
                        C2e=0)

        return (list(params=params, equilibria=equilibria))
    }

    IGP <- function()
    {
        community <- Community(nodes=data.frame(node=c('R','C1','C2'), 
                                            M=c(1,5,10), 
                                            N=c(150,50,1), 
                                            category=c('producer', 
                                                       rep('invertebrate', 2))),
                           trophic.links=data.frame(resource=c( 'R', 'R','C1'), 
                                                    consumer=c('C1','C2','C2')),
                           properties=list(title='IGP motif', 
                                           M.units='kg', N.units='m^-2'))

        # Model parameters
        spec <- ModelParamsSpec()
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        # Link-specific deviations that will be seen in equilibria
        params$W['R','C1'] <- 5000
        params$W['R','C2'] <- 500
        params$fe['R','C2'] <- 0.6

        # Equilibria calculated by Mathematica 8. 
        # q and d must be 0. 
        # as.numeric() to get rid of names
        rhoR <- with(params, as.numeric(rho['R']))
        K <- with(params, K)
        xC1 <- with(params, as.numeric(x['C1']))
        xC2 <- with(params, as.numeric(x['C2']))
        yRC1 <-  with(params, y['R', 'C1'])
        yRC2 <-  with(params, y['R', 'C2'])
        yC1C2 <- with(params, y['C1','C2'])
        WRC1 <-  with(params, W['R', 'C1'])
        WRC2 <-  with(params, W['R', 'C2'])
        WC1C2 <- with(params, W['C2','C2'])
        eRC1 <-  with(params, e['R', 'C1'])
        eRC2 <-  with(params, e['R', 'C2'])
        eC1C2 <- with(params, e['C1','C2'])
        feRC1 <-  with(params, fe['R', 'C1'])
        feRC2 <-  with(params, fe['R', 'C2'])
        feC1C2 <- with(params, fe['C1','C2'])

        equilibria <- c(Re=WRC2 / (-1+yRC2), 
                        C1e=0, 
                        C2e=-((WRC2*eRC2*feRC2*rhoR*(WRC2 + K - K*yRC2)) / 
                              (K*xC2*(-1 + yRC2)^2)) )

        return (list(params=params, equilibria=equilibria))
    }

    ExploitativeCompetition <- function()
    {
        community <- Community(nodes=data.frame(node=c('R','C1','C2'), 
                                            M=c(1,5,10), 
                                            N=c(150,50,1), 
                                            category=c('producer', 
                                                       rep('invertebrate', 2))),
                           trophic.links=data.frame(resource=c( 'R', 'R'), 
                                                    consumer=c('C1','C2')),
                           properties=list(title='Exploitative competition motif', 
                                           M.units='kg', N.units='m^-2'))

        # Model parameters
        spec <- ModelParamsSpec()
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        # Link-specific deviations that will be seen in equilibria
        params$W['R','C1'] <- 5000
        params$W['R','C2'] <- 500
        params$fe['R','C2'] <- 0.6

        # Equilibria calculated by Mathematica 8. 
        # q and d must be 0. 
        # as.numeric() to get rid of names
        rhoR <- with(params, as.numeric(rho['R']))
        K <- with(params, K)
        xC1 <- with(params, as.numeric(x['C1']))
        xC2 <- with(params, as.numeric(x['C2']))
        yRC1 <-  with(params, y['R', 'C1'])
        yRC2 <-  with(params, y['R', 'C2'])
        WRC1 <-  with(params, W['R', 'C1'])
        WRC2 <-  with(params, W['R', 'C2'])
        eRC1 <-  with(params, e['R', 'C1'])
        eRC2 <-  with(params, e['R', 'C2'])
        feRC1 <-  with(params, fe['R', 'C1'])
        feRC2 <-  with(params, fe['R', 'C2'])

        equilibria <- c(Re=WRC2 / (-1+yRC2), 
                        C1e=0, 
                        C2e=-((WRC2*eRC2*feRC2*rhoR*(WRC2 + K - K*yRC2)) / 
                              (K*xC2*(-1 + yRC2)^2)) )
        return (list(params=params, equilibria=equilibria))
    }

    ApparentCompetition <- function()
    {
        community <- Community(nodes=data.frame(node=c('R1','R2','C'), 
                                            M=c(1,5,10), 
                                            N=c(150,150,20), 
                                            category=c(rep('producer', 2), 
                                                       'invertebrate')), 
                           trophic.links=data.frame(resource=c('R1', 'R2'), 
                                                    consumer=c('C',  'C')),
                           properties=list(title='Apparent competition motif', 
                                           M.units='kg', N.units='m^-2'))

        spec <- ModelParamsSpec()
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        # Link-specific deviations that will be seen in equilibria
        params$W['R1','C'] <- 1100
        params$W['R2','C'] <- 900
        params$fe['R2','C'] <- 0.6

        # Equilibria calculated by Mathematica 8. 
        # q and d must be 0. 
        # as.numeric() to get rid of names
        aR1R1 <- with(params, a['R1','R1'])
        aR1R2 <- with(params, a['R1','R2'])
        aR2R1 <- with(params, a['R2','R1'])
        aR2R2 <- with(params, a['R2','R2'])
        rhoR1 <- with(params, as.numeric(rho['R1']))
        rhoR2 <- with(params, as.numeric(rho['R2']))
        rhoR1 <- with(params, as.numeric(rho['R1']))
        rhoR2 <- with(params, as.numeric(rho['R2']))
        K <- with(params, K)
        xC <- with(params, as.numeric(x['C']))
        yR1C <- with(params, y['R1','C'])
        yR2C <- with(params, y['R2','C'])
        WR1C <- with(params, W['R1','C'])
        WR2C <- with(params, W['R2','C'])
        eR1C <- with(params, e['R1','C'])
        eR2C <- with(params, e['R2','C'])
        feR1C <- with(params, fe['R1','C'])
        feR2C <- with(params, fe['R2','C'])

        equilibria <-c(R1e=WR1C/(-1 + yR1C),
                       R2e=0, 
                       Ce=-((WR1C*eR1C*feR1C*rhoR1*(aR1R1*WR1C + K - K*yR1C))/
                          (K*xC*(-1 + yR1C)^2)) )

        return (list(params=params, equilibria=equilibria))
    }

    ExploitativeAndApparentCompetition <- function()
    {
        community <- Community(nodes=data.frame(node=c('R','C1', 'C2', 'C3'), 
                            M=c(1,2,10,100), 
                            N=c(150,50,50,1), 
                            category=c('producer', 
                                       rep('invertebrate', 3))),
           trophic.links=data.frame(resource=c( 'R', 'R', 'C1', 'C2'), 
                                    consumer=c('C1','C2', 'C3', 'C3')),
           properties=list(title='Exploitative and apparent competition motif',
                           M.units='kg', N.units='m^-2'))
        # Model parameters
        spec <- ModelParamsSpec()
        params <- IntermediateModelParams(community, spec)
        params <- BuildModelParams(community, params) # containing rho,x,z etc

        # Link-specific deviations that will be seen in equilibria
        params$W['R','C2'] <- 900
        params$fe['R','C2'] <- 0.9

        # Equilibria calculated by Mathematica 8. 
        # q and d must be 0. 
        # as.numeric() to get rid of names
        rhoR <- with(params, as.numeric(rho['R']))
        K <- with(params, K)
        xC1 <- with(params, as.numeric(x['C1']))
        xC2 <- with(params, as.numeric(x['C2']))
        xC3 <- with(params, as.numeric(x['C3']))
        yRC1 <-   with(params, y['R',  'C1'])
        yRC2 <-   with(params, y['R',  'C2'])
        yC1C3 <-  with(params, y['C1','C3'])
        yC2C3 <-  with(params, y['C2','C3'])
        WRC1 <-  with(params, W['R', 'C1'])
        WRC2 <-  with(params, W['R', 'C2'])
        WC1C3 <- with(params, W['C1', 'C3'])
        WC2C3 <- with(params, W['C2', 'C3'])
        eRC1 <-   with(params, e['R', 'C1'])
        eRC2 <-   with(params, e['R', 'C2'])
        eC1C3 <-  with(params, e['C1', 'C3'])
        eC2C3 <-  with(params, e['C2', 'C3'])
        feRC1 <-   with(params, fe['R', 'C1'])
        feRC2 <-   with(params, fe['R', 'C2'])
        feC1C3 <-  with(params, fe['C1', 'C3'])
        feC2C3 <-  with(params, fe['C2', 'C3'])

        equilibria <- c(Re=WRC2/(-1 + yRC2), 
                        C1e=0, 
                        C2e=-((WRC2*eRC2*feRC2*rhoR*(WRC2 + K - K*yRC2)) / 
                              (K*xC2*(-1 + yRC2)^2)),
                        C3e=0)

        return (list(params=params, equilibria=equilibria))
    }

    tests <- ls()
    tests <- tests[sapply(tests, function(t) is.function(eval(parse(text=t))))]
    for(test in tests)
    {
        cat('    TestModel running [', test, ']\n', sep='')
        t <- do.call(test, args=list())

        tseriesC <- RunSim(YodzisInnesDyDt, t$params)
        tseriesR <- RunSim(YodzisInnesDyDt_R, t$params)

        # TODO Why do some tests require this higher tolerance?
        if(!isTRUE(all.equal(tseriesC, tseriesR)))
        {
            cat('        Fails at default tolerance\n')
            stopifnot(isTRUE(all.equal(tseriesC, tseriesR, tolerance=1e-6)))
            cat('        Succeeds at lower tolerance\n')
        }

        Bfinal <- as.vector(tail(tseriesC, 1)[,-1])
        stopifnot(isTRUE(all.equal(as.numeric(t$equilibria), 
                                   as.numeric(Bfinal))))
    }
}

