#' The checking the data clean
#'
#' @param x input dataframe
#' @return results from checking
#' @export
kasa.dataCleaning <- function(x){
  res <- list()
  res$classes <- sapply(x,function(y) class(y))
  res$na<- sapply(x,function(y) sum(is.na(y)))
  res$unique <- sapply(x, function(y) length(unique(y)))
  res$dulplicated <- sapply(x, function(y) sum(duplicated(y)))
  res$map <- Amelia::missmap(x, main = "Missing values vs observed")
  return(res)
}

#' Duplicated value removal by SD
#'
#' @param x input dataframe of gene matrix
#' @return removed dataframe
#' @import dplyr
#' @export
kasa.duplicationRemovalBySD <- function(x){
  matrix_data <- as.matrix(x[,-c(1)])
  sd <- apply(matrix_data,1,sd)
  order_num <- seq(1:nrow(x))
  transformed <- cbind(order_num,sd,x)
  name_list <- colnames(transformed)
  colnames(transformed) <- paste0("var_",seq(1:ncol(transformed)))
  colnames(transformed)[1:3] <- c("order_num","sd","grouped")
  res <- transformed %>% arrange(desc(sd)) %>% group_by(grouped) %>% filter(row_number()==1) %>% ungroup() %>% arrange(order_num)
  colnames(res) <- name_list
  return(res[c(-1,-2)])
}
#' Transpose matrix
#'
#' @param x input dataframe of gene matrix
#' @param firstColumnName input String for the first column name
#' @return transposed matrix as dataframe
#' @export
kasa.transposeMatrix <- function(x, firstColumnName="sample"){
  col_names_1 <- t(x[1])
  raw_data <- t(x[-1])
  colnames(raw_data) <- col_names_1
  raw_data <- as.data.frame(raw_data)
  row_name_1 <- row.names(raw_data)
  raw_data <- cbind(row_name_1,raw_data)
  row.names(raw_data) <- NULL
  colnames(raw_data)[1] <- firstColumnName
  raw_data[,1] <- as.character(raw_data[,1])
  return(raw_data)
}

#' Median centering
#'
#' @param x input dataframe of gene matrix
#' @return Matrix as median centered
#' @export
kasa.geneMedianCentering <- function(x){
raw.data <- as.matrix(x[-1])
median.table <- apply(raw.data ,c(1),median,na.rm = T)
median_centered <- raw.data-median.table
return(cbind(x[1],median_centered))
}

#' Transfomr_NA_to_Median
#'
#' @param x input dataframe of gene matrix
#' @return Matrix with transformed NA values
#' @export
kasa.transform_na_to_median <- function(x) {
raw.data <- as.matrix(x[-1])
for (i in c(1:nrow(x))){
temp.row <- raw.data[i,]
median.temp <- median(temp.row,na.rm = T)
raw.data[i,is.na(raw.data[i,])] <- median.temp
}
res <- cbind(x[c(1)],raw.data)
return (res)
}

#' checkig NaN
#'
#' @param x datafram
#' @export
is.nan.data.frame <- function(x){
do.call(cbind, lapply(x, is.nan))}

#' gene Standardization
#'
#' @param x input dataframe of gene matrix
#' @return standarized genes
#' @export
kasa.geneStandardization <- function(x){
raw.data <- as.matrix(x[-1])
sd.table <- apply(raw.data,1,sd,na.rm = T)
res.table_1 <- raw.data/sd.table # divided by standard deviation
res <- cbind(x[1],res.table_1)
res[is.nan(res)] <- 0
return(res)
}
