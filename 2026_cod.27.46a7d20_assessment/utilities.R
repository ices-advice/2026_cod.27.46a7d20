na2zero <- function(x){
  x[is.na(x)] <- 0
  x
}

reidx <- function(x){
  if(any(x >= (-0.5) & !is.na(x))){
    xx <- x[x >= (-.5) & !is.na(x)]
    x[x >= (-.5) &!is.na(x)] <- match(xx,sort(unique(xx)))-1
  }
  x
}

####' Function to cut years and ages from data by summing
####'
####' Years outside the range (minYear,maxYear) are removed.
####' Ages are summed to form a new plus group at age maxAge
####' @param x matrix with data
####' @return matrix with new data
####' @author Christoffer Moesgaard Albertsen
cutSum <- function(x){
  a <- as.numeric(colnames(x))
  y <- as.numeric(rownames(x))
  if(all(a <= maxAge) && all(y >= minYear) && all(y <= maxYear)) return(x)
  at <- attributes(x)
  v <- cbind(x[y>=minYear & y<=maxYear,a<maxAge,drop=FALSE],rowSums(x[y>=minYear & y<=maxYear,a>=maxAge,drop=FALSE]))
  colnames(v) <- a[a<=maxAge]
  nms <- setdiff(names(at),names(attributes(v)))
  attributes(v)[nms] <- at[nms]
  v
}

####' Function to cut years and ages from data by dropping
####'
####' Years outside the range (minYear,maxYear) are removed.
####' Ages are removed to form a new plus group at age maxAge
####' @param x matrix with data
####' @return matrix with new data
####' @author Christoffer Moesgaard Albertsen
cutDrop <- function(x, maxA = maxAge){
  a <- as.numeric(colnames(x))
  y <- as.numeric(rownames(x))
  if(all(a <= maxA) && all(y >= minYear) && all(y <= maxYear)) return(x)
  at <- attributes(x)
  v <- x[y>=minYear & y<=maxYear,a<=maxA,drop=FALSE]
  nms <- setdiff(names(at),names(attributes(v)))
  attributes(v)[nms] <- at[nms]
  v
}

####' Function to cut years and ages from data by averaging
####'
####' Years outside the range (minYear,maxYear) are removed.
####' Ages are averages to form a new plus group at age maxAge
####' @param x matrix with data
####' @return matrix with new data
####' @author Christoffer Moesgaard Albertsen
cutMean <- function(x, w = matrix(1,nrow(x),ncol(x))){
  a <- as.numeric(colnames(x))
  y <- as.numeric(rownames(x))
  if(all(a <= maxAge) && all(y >= minYear) && all(y <= maxYear)) return(x)
  at <- attributes(x)
  v1 <- x[y>=minYear & y<=maxYear,a<maxAge,drop=FALSE]
  x2 <- x[y>=minYear & y<=maxYear,a>=maxAge,drop=FALSE]
  w2 <- w[y>=minYear & y<=maxYear,a>=maxAge,drop=FALSE]
  rsw2 <- rowSums(w2)
  v2 <- rowSums(x2*w2) / rsw2
  v2[is.na(rsw2) | rsw2 == 0] <- NA
  v <- cbind(v1,v2)
  colnames(v) <- a[a<=maxAge]
  rownames(v) <- y[y>=minYear & y<=maxYear]
  nms <- setdiff(names(at),names(attributes(v)))
  attributes(v)[nms] <- at[nms]
  v
}


runFit <- function(dat, cfg, starting = list(),pls = list(), map=list(),fullDerived=FALSE,sim.keepRec=TRUE,phForm = as.formula(~poly(Age,3)),
                   shared_selectivity = as.integer(4),
                   shared_oneFScalePars = TRUE,
                   rm.unidentified=FALSE,
                   initN = as.integer(2),
                   shared_keys = c("keyLogFpar", #1
                                   "keyQpow", #2
                                   "keyVarF", #3
                                   "keyVarLogN", #4
                                   "keyVarObs", #5
                                   "keyCorObs", #6
                                   "seasonRho","seasonSd", # 7,8,9
                                   "keyLogFmu","keyLogFrho", #10,11
                                   "keyStockWeightObsVar","procSdSW","phiSW",#12,13,14
                                   "procSdMO","phiMO",#15,16,17
                                   "keyMortalityObsVar","procSdNM","phiNM",#18,19,20
                                   "keyMortalityMean",#21
                                   "sigmaObsParUS",
                                   "rec_transphi",#22
                                   "predVarObsLink",
                                   "keyXtraSd"
                   ),
                   newtonsteps = 3,
                   lower = list(initLogN = 10, shared_lfsSd = -5, logSdLogObs = -5, sigmaObsParUS=-3, transfIRARdist=-3, rec_pars = -20),
                   upper = list(initLogN = 20, transfIRARdist = 5, logSdMO = 5, logSdLogObs = 2, sigmaObsParUS=3),
                   ...){
  par <- lapply(names(dat$Stock_data), function(snm){
    par <- defpar(dat$Stock_data[[snm]],cfg[[snm]])
    if(any(dat$Stock_data[[snm]]$fleetTypes == 80)){
      keys <- cfg[[snm]]$keyVarObs[dat$Stock_data[[snm]]$fleetTypes == 80]+1
      par$logSdLogObs[keys[keys>0]] <- -2
    }
    if(any(dat$Stock_data[[snm]]$fleetTypes == 90)){
      keys <- cfg[[snm]]$keyVarObs[dat$Stock_data[[snm]]$fleetTypes == 90]+1
      par$logSdLogObs[keys[keys>0]] <- 0
    }
    if(length(pls) > 0)
      for(nm in intersect(names(par),names(pls)))
        par[[nm]][] <- pls[[nm]][]
    par
  })
  names(par) <- names(dat$Stock_data)
  
  singleStockList <- do.call("c",lapply(names(dat$Stock_data), function(snm){
    dd <- dat$Stock_data[[snm]]
    lo <- dd$logobs
    lo[is.na(lo)] <- 0
    dd$logobs[] <- NA
    suppressWarnings(f <- sam.fit(dd, cfg[[snm]], par[[snm]],map=map, run = FALSE, pre.clean=FALSE, fullDerived = fullDerived,sim.keepRec=sim.keepRec))
    f$sdrep <- list(value = numeric(0))
    f$pl$missing[] <- lo
    class(f) <- "sam"
    f
  }))
  names(singleStockList) <- names(dat$Stock_data)
  ## Baseline fit
  dat$Shared_data$covCombine <- as.integer(2)
  try({multisam.fit(singleStockList, newtonsteps=newtonsteps, rm.unidentified = rm.unidentified,#run=TRUE,
                    shared_data = dat$Shared_data,
                    ## shared_keys=c("keyLogFpar", #1
                    ##               "keyQpow", #2
                    ##               "keyVarF", #3
                    ##               "keyVarLogN", #4
                    ##               "keyVarObs", #5
                    ##               "keyCorObs", #6
                    ##               "seasonMu","seasonRho","seasonSd", # 7,8,9
                    ##               "keyLogFmu","keyLogFrho", #10,11
                    ##               "keyStockWeightObsVar","procSdSW","phiSW",#12,13,14
                    ##               "procSdMO","phiMO","sdMO",#15,16,17
                    ##               "keyMortalityObsVar","procSdNM","phiNM",#18,19,20
                    ##               "keyMortalityMean",#21
                    ##               "sigmaObsParUS",
                    ##               "rec_transphi",#22
                    ##               "predVarObsLink",
                    ##               "keyXtraSd"
                    ## )[c(TRUE, #1
                    ##     TRUE, #2
                    ##     TRUE, #3
                    ##     TRUE, #4
                    ##     TRUE, #5
                    ##     TRUE, #6
                    ##     FALSE,TRUE,TRUE, #7,8,9
                    ##     TRUE,TRUE, #10,11
                    ##     TRUE,TRUE,TRUE, #12,13,14
                    ##     TRUE,TRUE,FALSE, #15,16,17
                    ##     TRUE,TRUE,TRUE, #18,19,20
                    ##     TRUE, #21
                    ##     TRUE,
                    ##     TRUE,#22
                    ##     TRUE,
                    ##     TRUE)],
                    shared_keys = shared_keys,
                    ## 0: Not shared, 1: vector AR1, 2: scalar RW/AR1, 3: parametric, 4: parametric + scalar RW
                    shared_selectivity = shared_selectivity,
                    shared_seasonality = as.integer(0),
                    shared_oneFScalePars = shared_oneFScalePars,
                    shared_proportionalHazard = phForm,
                    shared_initN = FALSE,
                    skip_stock_observations = TRUE,
                    initN = initN,
                    initF = FALSE,
                    lower = lower,
                    upper = upper,
                    starting = starting,
                    fullDerived = fullDerived,
                    shared_stockrecruitment=FALSE,
                    inner.control = list(maxit = 2000),
                    ...
  )})
}