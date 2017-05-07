#' Downloading and intalling the packages automatically from the crans
#'
#' @param pkg input the vector of the package to be installed #'
#' @return NULL
#' @export
kasa.instPak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#' Downloading and intalling the packages automatically from the Bioclite
#'
#' @param packages_bioc input the vector of the package to be installed #'
#' @return NULL
#' @export
kasa.instPak_bioc <- function(packages_bioc){
  new.pkg <- pkg_b[!(pkg_b %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    source("https://bioconductor.org/biocLite.R")
    biocLite(suppressUpdates=TRUE,suppressAutoUpdate=FALSE,ask=FALSE)
    biocLite(pkg=pkg_b,suppressUpdates=TRUE,suppressAutoUpdate=FALSE,ask=FALSE)
  }
  sapply(pkg_b, require, character.only = TRUE)
}
