#' Two dataset merging and devide into each sets
#'
#' @param dataframeX input the dataset X
#' @param dataframeY input the dataset Y
#' @param keycolX Key column in X
#' @param keycolY Key coulum in Y
#' @return List file contains each sets
#' @export
kasa.matchingRow <- function(dataframeX=x,dataframeY=y,keycolX="sample",keycolY="sample"){
  colnames(dataframeX)[which( colnames(dataframeX)==keycolX )] <- "temp.join.key.X"
  colnames(dataframeY)[which( colnames(dataframeY)==keycolY )] <- "temp.join.key.Y"
  temp.join <- inner_join(dataframeX,dataframeY,by=c("temp.join.key.X"="temp.join.key.Y"))
  res <- list()
  res[["dataframeX"]] <- temp.join[,1:ncol(dataframeX)]
  res[["dataframeY"]] <- cbind(temp.join[,"temp.join.key.X"],temp.join[,(1+ncol(dataframeX)):ncol(temp.join)])
  colnames(res$dataframeX)[which( colnames(res$dataframeX)== "temp.join.key.X")] <- keycolX
  colnames(res$dataframeY)[1] <- keycolY
  return(res)
}
