

## Adds FMTC constraint, F-multiplier to total catch. Should be the same for all
backcorrected_modelforecast.msam <- function(fit,
                                        constraints = NULL,
                                        fscale = NULL,
                                        catchval = NULL,
                                        fval = NULL,
                                        nextssb = NULL,
                                        landval = NULL,
                                        nosim = 0,
                                        ...,
                                        Fdefault = 0.1, seed = NULL){
    if(is.null(seed)){
        set.seed(NULL)
        seed <- .Random.seed
    }
    if(nosim == 0) stop("Back correction is not needed for nosim=0")
    if(any(!is.null(fscale) | !is.null(catchval) | !is.null(fval) | !is.null(nextssb) | !is.null(landval)))
        stop("Please use the constraints argument")

    ## Helper functions
    nStocks <- length(fit)    
    constraintsOrig <- multiStockassessment:::targetToList(constraints, nStocks, character)
    stripEst <- function(cc){
        gsub("(.+\\()(F|SSB|C|FMTC)([^=]+)(=.+)","\\2\\4",cc)
    }
    getEst <- function(cc){
        ## Insert median if no method
        hasNoEst <- grepl("^(F|SSB|C|FMTC)=",cc)
        cc[hasNoEst] <- gsub("(F|SSB|C|FMTC)(=.+)","median(\\1)\\2",cc[hasNoEst])
        parse(text=gsub("(.+\\()(F|SSB|C|FMTC)([^=]+)(=.+)","\\1vvx\\3",cc))
    }
    estMethod <- constraints <- vector("list",length(fit))
    for(i in seq_along(constraints)){
        ## Check for estimation method
        constraints[[i]] <- stripEst(constraintsOrig[[i]])
        if(!all(grepl("^(F|SSB|C|FMTC)=",constraints[[i]])) && !any(!grepl("\\*",constraints[[i]])))
            stop("The back correction is currently only implemented for F, SSB, and C constraints without restrictions.")
        if(grepl("^SSB",tail(constraints[[i]],1)))
            stop("The last constraint cannot be for next years SSB. Please add an arbitrary constraint as the last")
        estMethod[[i]] <- getEst(constraintsOrig[[i]])
    }
    ## Check constraints have the same length!

    set.seed(seed)
    tmpCon <- lapply(constraints, function(x){
        c0 <- rep(sprintf("F=%f",Fdefault),length(x))
        c0[is.na(x)] <- NA
        c0
    })
    ##tmpCon[grepl("^F=.+\\*",constraints)] <- constraints[grepl("^F=.+\\*",constraints)]
    cat("Initial forecast...\n")
    F0 <- modelforecast(fit,tmpCon,nosim=nosim,fastFixedF=TRUE,...)
    rep <- attr(fit,"m_rep")

    ## Base year correction of fbar
    FBT <- fbartable(fit,returnList=TRUE)
    fbarBY <- sapply(seq_along(fit),function(s) FBT[[s]][rownames(attr(F0[[s]],"tab"))[1],1])
    fbarRS <- sapply(F0, function(x) median(x[[1]]$fbar))
    fbarCorrect <- log(fbarBY) - log(fbarRS)
    cat("Correcting base year F...\n")
    ## cat("F BY:",fbarBY,"\n")
    ## cat("F RS:",fbarRS,"\n")
    ## cat("log F correction:",log(fbarBY) - log(fbarRS),"\n")
    set.seed(seed)
    F0 <- modelforecast(fit,tmpCon,nosim=nosim,fastFixedF=TRUE, resampleFcorrection = fbarCorrect, ...)
    ## cat("Corrected F:",sapply(F0, function(x) median(x[[1]]$fbar)),"\n")
    rep <- attr(fit,"m_rep")
    bcList <- list()
    for(y in seq_along(constraints[[1]])){
        ## Extract simulations
        NN <- sapply(fit,function(s) s$conf$maxAge-s$conf$minAge+1)
        sim <- lapply(F0,function(x) lapply(x,function(y) y$sim))
        logN <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,1:NN[s]]))
        logF <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y){
            log(Reduce("+",lapply(split(fit[[s]]$conf$keyLogFsta,slice.index(fit[[s]]$conf$keyLogFsta,1)),function(ii){
                exp(y[,-(1:NN[s])][,pmax(ii+1,1)]) * matrix(as.numeric(ii>=0),nrow(y),length(ii),byrow=TRUE)
        })))}))
        M <- lapply(F0,function(x) lapply(x, function(y) (y$bio_natMor)))
        PropMat <- lapply(F0,function(x) lapply(x, function(y) (y$bio_propMat)))
        SW <- lapply(F0,function(x) lapply(x, function(y) (y$bio_stockMeanWeight)))
        CW <- lapply(F0,function(x) lapply(x, function(y) (y$bio_catchMeanWeight)))
        logHazard_M_breakpoints <- lapply(F0, function(x) lapply(x, function(y) y$logHazard_M_breakpoints))
        logHazard_F_breakpoints <- lapply(F0, function(x) lapply(x, function(y) y$logHazard_F_breakpoints))
        activeHazard_F <- lapply(rep$mort, function(x) x$activeHazard_F)
        activeHazard_breakpoints <- lapply(rep$mort, function(x) x$activeHazard_breakpoints)
        sampleTimesStart <- lapply(fit, function(x) x$data$sampleTimesStart)
        sampleTimesEnd <- lapply(fit, function(x) x$data$sampleTimesEnd)
        activeHazardMap_risk <- lapply(rep$mort, function(x) x$activeHazardMap_risk)

        logspace_add <- function(logx,logy){
            r <- pmax(logx,logy)
            ii <- is.finite(r)
            r[ii] <- pmax(logx[ii],logy[ii]) + log1p(exp(-abs(logx[ii]-logy[ii])))
            r
        }
        logspace_sum <- function(logx){
            if(length(logx)<=1) return(logx)
            if(length(logx)==2) return(logspace_add(logx[1],logx[2]))
            Mx <- max(logx)
            Mx + log(sum(exp(logx-Mx)))
        }
        logspace_1m <- function(logx) log1p(-exp(logx))
        logspace_1p <- function(logx) log1p(exp(logx))
        getFbar <- function(eta,s,y){
            fr <- fit[[s]]$conf$fbarRange - fit[[s]]$conf$minAge + 1
            rowMeans(exp(logF[[s]][[y]][,fr[1]:fr[2]] + eta))    
        }
        getNextSSB <- function(eta,s,y){
            R <- logN[[s]][[y+1]][,1]
            logPredN0 <-  logN[[s]][[y]] - exp(logF[[s]][[y]]) - t(M[[s]][[y]])
            logPredN0 <- cbind(R,logPredN0[,1:(ncol(logPredN0)-2)],log(rowSums(exp(logPredN0[,-(1:(ncol(logPredN0)-2))]))))
            eps <- logN[[s]][[y+1]]-logPredN0
            logPredN <- logN[[s]][[y]] - exp(logF[[s]][[y]]+eta) - t(M[[s]][[y]])
            logPredN <- cbind(R,logPredN[,1:(ncol(logPredN)-2)],log(rowSums(exp(logPredN[,-(1:(ncol(logPredN)-2))])))) + eps
            ## Survival before SSB
            if((sum(fit[[s]]$data$propM) + sum(fit[[s]]$data$propF)) > 0)
                stop("Backcorrection of SSB targets are only available if SSB is calculated in the beginning of the year")
            rowSums(exp(logPredN) * t(PropMat[[s]][[y+1]]) * t(SW[[s]][[y+1]]))
        }
        getCatch <- function(eta,s, y){
            ## logN(a,y) + mort.logFleetSurvival_before(a,y,f-1) + mort.fleetLogCumulativeIncidence(a,y,f-1);
            ## Need to report logHazard_M_breakpoints and logHazard_F_breakpoints
            ## Apply eta to logHazard_F_breakpoints
            ## Calculate survival + CIF
            ## Need non-F hazards + seasonal effect + F
            doOne <- function(i){
                lN <- logN[[s]][[y]][i,]
                lHM <- logHazard_M_breakpoints[[s]][[y]][,,,,i,drop=FALSE]
                dim(lHM) <- head(dim(logHazard_M_breakpoints[[s]][[y]]),-1)
                lHF <- logHazard_F_breakpoints[[s]][[y]][,,,,i,drop=FALSE] + eta
                dim(lHF) <- head(dim(logHazard_F_breakpoints[[s]][[y]]),-1)

                
                logHazard_breakpoints <- logspace_add(apply(lHM,c(1,4),logspace_sum), apply(lHF,c(1,4),logspace_sum))
                logSurvBefore <- matrix(0,nrow(logHazard_breakpoints),dim(lHF)[3])
                for(f in 1:ncol(logSurvBefore)){
                    t0 <- 0
                    t1 <- sampleTimesStart[[s]][f]
                    logS0 <- logSurvBefore[,f]
                    for(t in head(seq_along(activeHazard_breakpoints[[s]]),-1)){
                        ## Skip interval if it ends before time interval
                        if(activeHazard_breakpoints[[s]][t+1] <= t0)
                            next;
                        ## Break loop if interval starts after time interval
                        if(activeHazard_breakpoints[[s]][t] >= t1)
                            break;
                        ## Otherwise, look at overlap between intervals	   
                        Astart = pmax(activeHazard_breakpoints[[s]][t],t0)
                        Aend = pmin(activeHazard_breakpoints[[s]][t+1],t1)
                        ## Hazard is constant, so cumulative hazard is hazard times interval length
                        logS0 = logS0 - exp(logHazard_breakpoints[,t]) * (Aend - Astart)
                    }
                    logSurvBefore[,f] <- logS0
                }
                logCIF <- matrix(-Inf,nrow(logHazard_breakpoints),dim(lHF)[3])
                for(f in 1:ncol(logSurvBefore)){
                    t0 <- sampleTimesStart[[s]][f]
                    t1 <- sampleTimesEnd[[s]][f]
                    logS0 <- numeric(nrow(logCIF))
                    vlogCIF <- rep(-Inf,nrow(logCIF))
                    for(t in head(seq_along(activeHazard_breakpoints[[s]]),-1)){
                        ## Skip interval if it ends before time interval
                        if(activeHazard_breakpoints[[s]][t+1] <= t0)
                            next;
                        ## Break loop if interval starts after time interval
                        if(activeHazard_breakpoints[[s]][t] >= t1)
                            break;
                        ## Otherwise, look at overlap between intervals	   
                        ## Full interval
                        Astart = pmax(activeHazard_breakpoints[[s]][t],t0)
                        Aend = pmin(activeHazard_breakpoints[[s]][t+1],t1)
                        lCIF_F_brk <- lHF[,1,f,t] - logHazard_breakpoints[,t] + logspace_1m(-exp(logHazard_breakpoints[,t])*(Aend-Astart))
                        tmp <- logS0 + lCIF_F_brk
                        vlogCIF = logspace_add(vlogCIF, tmp)
                        logS0 <- logS0 - exp(logHazard_breakpoints[,t])*(Aend-Astart)
                    }
                    logCIF[,f] <- vlogCIF
                }
                ## Output catch, sum of all fleets
                ## logN(a,y) + mort.logFleetSurvival_before(a,y,f-1) + mort.fleetLogCumulativeIncidence(a,y,f-1);
                exp(logspace_sum(lN + logSurvBefore + logCIF + log(t(CW[[s]][[y]])[i,])))            
            }
            sapply(seq_len(nrow(logN[[s]][[y]])),doOne)
        }

        cstr <- sapply(constraints,function(x) x[y])

        Target <- as.numeric(gsub("(.+=)([^\\*]+)(\\*?)","\\2",cstr))
        isRel <- grepl("\\*",cstr)
        summarize <- function(s,y,v){
            eval(estMethod[[s]][[y]], list(vvx = unname(v)))
        }
        bc_eta <- nlminb(numeric(nStocks), function(ee){
            v <- 0
            if(any(grepl("^FMTC=",cstr))){
                if(!all(grepl("^FMTC=",cstr)) || length(unique(Target)) != 1)
                    warning("When using FMTC targets, all stocks should use the same target! Only the first FMTC target will be used.")
                sX <- min(which(grepl("^FMTC=",cstr)))
                ## Sum of median catches, use first multiplier - combine with constraint ensuring relative F change is the same for all 
                ## Catch constraint               
                if(isRel[sX]){
                    cOld <- sum(sapply(seq_len(nStocks), function(s) summarize(s,y,getCatch(0,s,y))))
                    cNew <- sum(sapply(seq_len(nStocks), function(s) summarize(s,y,getCatch(ee[s],s,y+1))))
                    v <- v + (cNew/cOld-Target[sX])^2
                }else{
                    cNew <- sum(sapply(seq_len(nStocks), function(s) summarize(s,y,getCatch(ee[s],s,y+1))))
                    v <- v + (cNew-Target[sX])^2
                }
                ## Force F multipliers to be equal (not the multiplier is relative to F=Fdefault
                ## v <- v + sum((ee-ee[1])^2)
                rf <- sapply(seq_len(nStocks), function(s) summarize(s,y,getFbar(ee[s],s,y+1))/summarize(s,y,getFbar(0,s,y)))
                v <- v + sum((tail(rf,-1) - head(rf,1))^2) * Target[sX] * 100
            }else{
                for(s in seq_len(nStocks)){
                    if(grepl("^F=",cstr[s])){
                        ## F constraint
                        if(isRel[s]){
                            v <- v + (summarize(s,y,getFbar(ee[s],s,y+1))/summarize(s,y,getFbar(0,s,y))-Target[s])^2
                        }else{
                            v <- v + (summarize(s,y,getFbar(ee[s],s,y+1))-Target[s])^2
                        }
                    }else if(grepl("^SSB=",cstr[s])){
                        ## SSB constraint
                        if(isRel[s]){
                            v <- v + (summarize(s,y,getNextSSB(ee[s],s,y+1))/summarize(s,y,getNextSSB(0,s,y))-Target[s])^2
                        }else{
                            v <- v + (summarize(s,y,getNextSSB(ee[s],s,y+1))-Target[s])^2
                        }
                    }else if(grepl("^C=",cstr[s])){
                        ## Catch constraint
                        if(isRel[s]){
                            v <- v + (summarize(s,y,getCatch(ee[s],s,y+1))/summarize(s,y,getCatch(0,s,y))-Target[s])^2
                        }else{
                            v <- v + (summarize(s,y,getCatch(ee[s],s,y+1))-Target[s])^2
                        }
                    }else{
                        stop("The constraint cannot currently be back corrected")
                    }
                }
            }
            v
        }, control=list(iter.max= 10000, eval.max=10000))
        bcList[[y]] <- bc_eta
        cat(sprintf("Correcting forecast year %d...", y),"\n")
        for(s in seq_along(tmpCon))
            tmpCon[[s]][y] <- sprintf("F=%f",pmax(exp(bc_eta$par[s]) * Fdefault,1e-5))
        set.seed(seed)
        F0 <- modelforecast(fit,tmpCon,nosim=nosim,fastFixedF=TRUE, resampleFcorrection = fbarCorrect,...)
    }
    attr(F0,"bcOpt") <- bcList
    attr(F0,"tmpCon") <- tmpCon
    F0
}


