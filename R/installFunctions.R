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
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
  BiocManager::install(update=FALSE,ask=FALSE)
  new.pkg <- packages_bioc[!(packages_bioc %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    BiocManager::install(pkg=new.pkg,update=FALSE,ask=FALSE)
  }
  sapply(packages_bioc, require, character.only = TRUE)
}
