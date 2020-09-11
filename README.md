# R-iss
Some R code for ISS, initially a couple of functions to read and plot spectra.

## To use a built version on EOL servers

The $RCODE environment varaible is used below to load the compiled functions into an R session.

bash:

    export RCODE=~maclean/git/R-iss/R/.RData

csh:

    setenv RCODE ~maclean/git/R-iss/R/.RData

## To clone and roll your own

### Clone

    cd my_git_place
    git clone git@github.com:ncareol/R-iss

### Build it

    cd R-iss/R
    make

### Set RCODE environment variable

bash:

    export RCODE=$PWD/.RData

csh:

    setenv RCODE $PWD/.RData
    
## set NETCDF_DIR environment variable

This is the directory containing the spectral NetCDF files, which are read by the **plotspec** function.

    cd my_netcdf_dir

bash:

    export NETCDF_DIR=$PWD

csh:

    setenv NETCDF_DIR $PWD

## Run R
The function that reads the NetCDF file uses the **eolts** package, which is installed on EOL servers.

    R
    attach(Sys.getenv("RCODE"))     # attach the built code

    # generate all spectra plots on interactive window
    plotspec(spcfile="01-Oct-2019_23-45-01.spc.nc",
            momfile="moments_20191001.nc")

    # same plots in a PDF
    pdf(file="/tmp/mapr_spec.pdf",width=8,height=10.5)
    par(mfrow=c(6,4))   # 6x4 layout on pdf
    plotspec(spcfile="01-Oct-2019_23-45-01.spc.nc",
            momfile="moments_20191001.nc")
    dev.off()           # close pdf, it can now be viewed

    # spectra plots from NIMA output, reorder=FALSE
    pdf(file="/tmp/nima_spec.pdf",width=8,height=10.5)
    par(mfrow=c(6,4))   # 6x4 layout on pdf
    plotspec(spcfile="01-Oct-2019_23-45-01.spc.nc",
            momfile="moments_20191001.nc")
    dev.off()           # close pdf

    # to see the arguments to plotspec
    args(plotspec)
    
    # exit (everything in R is a function)
    q()
    # then generally enter "y" to save history and objects
