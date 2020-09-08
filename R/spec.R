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

    if (reorder) {
        nfft <- dim(spec)[1]
        mid <- trunc(nfft/2)
        spec <- spec[c((mid+2):nfft,1:(mid+1)),,]
    }

    list(spec=spec, hts=hts)
}

plotspec <- function(file="01-Oct-2019_23-45-01.spc.nc",
    dir="/home/maclean/ncar/nima/test",
    reorder=TRUE, xscale=1.0)
{

    x <- readspec(file=file,dir=dir,reorder=reorder)

    nfft <- dim(x$spec)[1]
    nhts <- dim(x$hts)[1]
    ntimes <- dim(x$hts)[2]

    mid <- trunc(nfft/2)
    vel <- (-mid+1):mid

    # tighten margins
    par(mar=c(2.0,2,1.0,0.2), mgp=c(1.0,0.3,0))

    for (it in 1:ntimes) {
        dhts <- diff(x$hts[, it])
        hmin <- x$hts[1,it] - dhts[1] * 0.30
        hmax <- x$hts[nhts,it] + dhts[length(dhts)] * 1.30
        # set up plot scales and plot the axes but no data
        plot(vel[c(1,length(vel))]*xscale,c(hmin,hmax), type="n", 
            xlab="vel", xaxs="i",
            ylab="", yaxs="i") 
        for (iht in 1:nhts) {
            y0 <- x$hts[iht, it]
            dh <- dhts[min(iht,length(dhts))]
            sd <- x$spec[,iht,it]
            ds <- diff(range(sd, na.rm=TRUE))
            sd <- y0 + dh * sd / ds
            lines(vel,sd)
        }
        # write file name at top of page
        if (all(par("mfg")[1:2] == c(1,1)))
            mtext(file,side=3,line=-0.9,outer=TRUE,cex=0.8)
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
