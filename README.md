# R-iss
Some R code for ISS
Some R functions for spectral plotting

Clone

    cd my_git_place
    git clone git@github.com:ncareol/R-iss

Build it

    cd R-iss
    cd R
    make

bash:
    export RCODE=$PWD/.RData
csh:
    setenv RCODE $PWD/.RData
    
set NETCDF_DIR environment variable

    cd my_netcdf_dir

bash:
    export NETCDF_DIR=$PWD
csh:
    setenv NETCDF_DIR $PWD

    R
    attach(Sys.getenv("RCODE"))     # attach the built code

    # generate all spectra plots on interactive window
    plotspec(file="01-Oct-2019_23-45-01.spc.nc")

    # same plots in a PDF
    pdf(file="/tmp/mapr_spec.pdf",width=8,height=10.5)
    par(mfrow=c(6,4))   # 6x4 layout on pdf
    plotspec(file="01-Oct-2019_23-45-01.spc.nc")
    dev.off()           # close pdf, it can now be viewed

    # spectra plots from NIMA output, reorder=FALSE
    pdf(file="/tmp/nima_spec.pdf",width=8,height=10.5)
    par(mfrow=c(6,4))   # 6x4 layout on pdf
    plotspec(file="spc_20191001.nc",reorder=FALSE)
    dev.off()           # close pdf
    
    # exit (everything in R is a function)
    # then generally enter "y" to save history and objects
    q()


    
    
                                                              1,0-1         Top

