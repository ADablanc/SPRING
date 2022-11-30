# SPRING
This repository contain the files to launch SPRING as en executable on Windows platform without administrative rights. It contains already all the files required to launch the app. 

## Structure
### Structure of the Windows app
```
.
    ├── R-Portable            # A portable R application that can be found here : https://portableapps.com/node/32898
    │   └── ...
    ├── Rtools                # Rtools, needed to create excel files or compiling cpp scripts
    │   └── ...
    ├── SPRING                # R package, see below for the structure of it
    │   └── ...
    ├── pwiz                  # Proteowizard repository containing files used to convert LC-GC/MS files
    │   └── ...
    ├── renv                  # renv directory, used to manage library paths in order to isolate R dependencies. See https://rstudio.github.io/renv/articles/renv.html
    │   ├── .gitignore
    │   ├── activate.R        # activation script created by renv
    │   └── settings.dcf      # project settings (created by renv)
    ├── .gitignore
    ├── SPRING.ico
    ├── click_me.vbs          # vbs file, it launch an R script with the R-Portable executable
    ├── renv.lock             # lock file needed for renv, it describe all the R package used to launch the app
    └── rscript.R             # R script used to launch SPRING: first it install/load all packages needed with renv, then it build the package SPRING (some cpp files need compilation... no idea for now how to bypass this step) & launch the app by using the function `run_shiny_app()`
```

### Structure of the SPRING package
```
SPRING
├── R                             # contains all R code files 
    └── ...
├── inst                          # contains internal data
    ├── extdata
    |   ├── database              # repository containing all databases used by SPRING
    |   |   ├── global.csv        # database of lipids, see the section `Database`
    |   |   ├── test-neg.csv      # database only used for testing the package
    |   |   └── test.csv          # database only used for testing the package
    |   └── adducts.csv           # list of adducts, it is an aggregation of the lists of EnviPat & CAMERA R package
    ├── shiny_app                 # shiny app directory
    |   ├── server                # contains all files needed for the server side of the shiny app
    |   |   └── ...
    |   ├── tests                 # repository used to test the app with the shinytest package (which used phantomJS, a headless web browser)
    |   |   └── ...
    |   ├── ui                    # contains all files needed for the interface side of the shiny app
    |   |   └── ...
    |   ├── www
    |   |   ├── SPRING.css
    |   |   └── SPRING.js
    |   ├── server.R              # server file of the shiny app, it will call all the different files of the server directory
    |   └── ui.R                  # interface file of the shiny app, it will call all the different files of the interface directory
    └── testdata                  # contains only data files used to test the package
    |   └── ...
├── man                           # contains documentation for the package functions
    └── ...
├── src                           # contains cpp files used by the package, see the Rcpp package for more information
    └── ...
├── tests                         # testing directory used by the testthat package
    └── ...
├── .Rbuildignore
|── DESCRIPTION
├── LICENSE.md
├── NAMESPACE
└── README.md
```

## INSTALLATION
### Windows
SPRING contains its own R executable and all R package dependencies are managed with the renv package but it only works on Windows.
Just download/clone it. If you want to install it on your own R session see the sub section below.
### Other OS 
To use it on Linux or Mac you need to launch R & install the SPRING package by specifying the path to the SPRING package:
```r
install.packages("SPRING/SPRING", repos = NULL, type="source")
```

## USAGE 
### WINDOWS
To launch the app you only need to click on the file named "click_me.vbs". 
### Other OS
To use it on Linux or Mac you need to call the function & specifying the path to the proteowizard repository: 
```r
SPRING::run_shiny_app(pwiz = "~/Proteowizard 4.7.1/pwiz")
```
You can call the function without specifying the pwiz argument but the app will don't allow the conversion in that case (it will stop the workflow if your files are not in the format *mzXML*, *mzML* or *CDF*).

## Shiny app documentation
coming soon...

## Command line documentation
coming soon...
