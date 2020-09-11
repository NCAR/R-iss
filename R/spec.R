testplots <- function()
{
    pdf("/tmp/mapr_spec.pdf",width=8,height=10.5)
    par(mfrow=c(6,4))
    plotspec("01-Oct-2019_23-45-01.spc.nc", reorder=TRUE)
    dev.off()
    pdf("/tmp/nima_spec.pdf",width=8,height=10.5)
    par(mfrow=c(6,4))
    plotspec("spc_20191001.nc", reorder=FALSE)
    dev.off()
    specdiff(f1="01-Oct-2019_23-45-01.spc.nc", f1reorder=TRUE,
        f2="spc_20191001.nc", f2reorder=FALSE)
}

readspec <- function(file="01-Oct-2019_23-45-01.spc.nc",
    dir=Sys.getenv("NETCDF_DIR"), reorder=TRUE)
{
    require("eolts")

    options(time.zone="UTC")
    # dpar(start=utime("2019 10 1 23:45"),lenmin=15)
    dpar(start=utime("2018 10 1 23:45"),lenday=10000)
    iod <- netcdf(file=file,dir=dir)
    on.exit(close(iod), add=TRUE)

    #vars <- variables(iod)
    #vars <- variables(iod, all=TRUE)

    # in netcdf, rightmost, last dimension varies most rapidly
    # numSpecAvg:  20
    # numCohInteg:  30
    # interPulsePer  55000 nanosecs
    # gateSpacing  1000 nanosecs
    # heights(time=26,height=40),  325 to 6175 by 150, stored in R as height,time with
    # first dimension, height, varying most rapidly
    # spectraDbs(time=26,height=40,nfft=256), time, height, nfft, stored in R as nfft,height,time

    # fill value = -9999
    spec <- readnc(iod,"spectraDbs")
    hts <- readnc(iod,"heights")
    ipp <- readnc(iod,"interPulsePer") * 1.e-9  # convert from nsec to sec
    nci <- readnc(iod,"numCohIntegs")
    # freqset <- readnc(iod,"frequency_set")
    freq <- readnc(iod,"frequency") * 1.e6  # convert from MHz to Hz

    if (reorder) {
        nfft <- dim(spec)[1]
        mid <- trunc(nfft/2)
        spec <- spec[c((mid+2):nfft,1:(mid+1)),,]
    }

    list(spec=spec, hts=hts, ipp=ipp, nci=nci, freq=freq)
}

readmom <- function(file="01-Oct-2019_23-45-01.spc.nc",
    dir=Sys.getenv("NETCDF_DIR"))
{
    require("eolts")

    # options(time.zone="UTC")
    # dpar(start=utime("2019 10 1 23:45"),lenmin=15)
    # dpar(start=utime("2018 10 1 23:45"),lenday=10000)
    iod <- netcdf(file=file,dir=dir)
    on.exit(close(iod), add=TRUE)

    vel <- readnc(iod,"vel")
    specWid <- readnc(iod,"specWid")

    list(vel=vel, specWid=specWid)
}

nyquistvel <- function(ipp,nci,freq)
{
    # compute nyquist (highest resolvable) velocity in m/s
    # from inter-pulse period in nanosec, number of coherent
    # integrations and the transmitter frequency in MHz.
    
    vel <- 1 / (2.0 * nci * ipp) * 2.998e8 / (2.0 * freq)
}

plotspec <- function(
    spcfile="01-Oct-2019_23-45-01.spc.nc",
    momfile="moments_20191001.nc",
    dir=Sys.getenv("NETCDF_DIR"),
    reorder=TRUE, xscale=1.0)
{

    x <- readspec(file=spcfile,dir=dir,reorder=reorder)
    xm <- readmom(file=momfile,dir=dir)

    nfft <- dim(x$spec)[1]
    nhts <- dim(x$hts)[1]
    ntimes <- dim(x$hts)[2]

    mid <- trunc(nfft/2)
    vel <- (-mid+1):mid

    # tighten margins
    par(mar=c(2.0,2,1.0,0.2), mgp=c(1.0,0.3,0))

    for (it in 1:ntimes) {
        nyvel <- nyquistvel(x$ipp[it], x$nci[it], x$freq)
        df <- nyvel / nfft * 2
        # if (it == 1) browser()
        vel <- -nyvel + df * 0:(nfft-1)
        dhts <- diff(x$hts[, it])
        hmin <- x$hts[1,it] - dhts[1] * 0.30
        hmax <- x$hts[nhts,it] + dhts[length(dhts)] * 1.30
        # set up plot scales and plot the axes but no data
        plot(c(-nyvel,nyvel)*xscale,c(hmin,hmax), type="n", 
            xlab="vel (m/s)", xaxs="i",
            ylab="", yaxs="i") 
        for (iht in 1:nhts) {
            y0 <- x$hts[iht, it]
            dh <- dhts[min(iht,length(dhts))]
            sd <- x$spec[,iht,it]
            ds <- diff(range(sd, na.rm=TRUE))
            sd <- y0 + dh * sd / ds
            lines(vel,sd)

            dopvel <- xm$vel[iht, it]
            points(dopvel, y0 + dh * 0.5, pch="+", col="red")
            velwid <- c(dopvel-0.5*xm$specWid[iht,it],
                dopvel+0.5*xm$specWid[iht,it])
            barpts <- c(rep(velwid[1],3),rep(velwid[2],3))
            ymid <- y0 + dh * 0.5
            ypts <- c(y0, y0+dh, ymid, ymid, y0, y0+dh)
            lines(barpts, ypts, col="red")
        }
        # write file name at top of page
        if (all(par("mfg")[1:2] == c(1,1)))
            mtext(paste(spcfile,momfile,sep=", "),
                side=3,line=-0.9,outer=TRUE,cex=0.8)
    }
}

specdiff <- function(f1="01-Oct-2019_23-45-01.spc.nc",f1reorder=TRUE,
    f2="spcmom_20191001.nc",f2reorder=FALSE,
    dir="/home/maclean/ncar/nima/test")
{

    x1 <- readspec(file=f1,dir=dir,reorder=f1reorder)
    x2 <- readspec(file=f2,dir=dir,reorder=f2reorder)

    cat(paste(f1,", ",f2,": max diff=",
        max(x1$spec-x2$spec, na.rm=TRUE),"\n",sep=""))
    cat(paste(f1,", ",f2,": NAs equal=",
        all(is.na(x1$spec) == is.na(x2$spec)),"\n",sep=""))
}
