#' Aggregate numeric and categorical variables by an ID
#'
#' The \code{Aggregate} function (not to be confounded with aggregate) prepares a data frame for merging by computing the sum, mean and variance of all continuous (integer and numeric) variables by a given ID variable. It also creates dummies for all categorical variables (character and factor) and subsequently computes the sum by a given ID variable. This functions aims at maximum information extraction with a minimum amount of code.
#'
#' @param x A data frame without the ID. Categorical variables have to be of type character or factor and continuous variables have to be of type integer or numeric.
#' @param by A vector containing IDs.
#' @param ... Extra parameters to be passed to the \code{dummy} function in the \code{dummy} package.
#'
#' @return A data frame with the aforementioned variables aggregated by the given ID variables
#'
#' @author Authors: Matthias Bogaert, Michel Ballings, Dirk Van den Poel, Maintainer: \email{matthias.bogaert@@UGent.be}
#' @examples
#' # Example
#' # Create some data
#' data <- data.frame(V1=as.factor(c('yes','no','no','yes','yes','yes','yes')),
#'                    V2=as.character(c(1,2,3,4,4,4,4)),V3=c(1:7),V4=as.numeric(c(7:1)))
#' ID=as.character(c(1,1,1,1,2,2,2))

#' Aggregate(x=data,by=ID)
#'
#' # Examples of how to use the ... argument. See package dummy for details.
#' # library(dummmy)
#' # Aggregate(x=data,by=ID,object=categories(data))
#' # Aggregate(x=data,by=ID,p=2)

Aggregate <- function (x, by, ...) {
    if (!is.data.frame(x)) stop("x need to be a data frame")

    categoricals <- sapply(x, is.factor) | sapply(x, is.character)
    if (any(categoricals == TRUE)) {
      dummies <- dummy(x[,categoricals,drop=FALSE], int=TRUE, ...)
      dummies_sum <- aggregate(dummies, list(ID = by), sum)
      dummies_tail <- aggregate(dummies, list(ID = by), tail, n = 1)
      ID <- dummies_tail$ID
      dummies_sum$ID <- dummies_tail$ID <- NULL
      names(dummies_sum) <- paste(names(dummies_sum), "_sum", sep = "")
      names(dummies_tail) <- paste(names(dummies_tail), "_last", sep = "")
      dummies <- data.frame(ID, dummies_sum, dummies_tail)
    }

    numerics <- sapply(x, is.numeric)
    if (any(numerics == TRUE)) {
      numerics_sum <- aggregate(x[,numerics,drop=FALSE], by = list(ID = by), sum)
      numerics_mean <- aggregate(x[,numerics,drop=FALSE], by = list(ID = by), mean)
      numerics_var <- aggregate(x[,numerics,drop=FALSE], by = list(ID = by), var)
      ID <- numerics_sum$ID
      numerics_sum$ID <- numerics_mean$ID <- numerics_var$ID <- NULL

      names(numerics_sum) <- paste(names(numerics_sum), "_sum", sep = "")
      names(numerics_mean) <- paste(names(numerics_mean), "_mean", sep = "")
      names(numerics_var) <- paste(names(numerics_var), "_var", sep = "")
      numerics <- data.frame(numerics_sum, numerics_mean, numerics_var)
    }


    if (any(categoricals == TRUE) && any(numerics == TRUE)) {
      final <- data.frame(dummies, numerics)
    }else if (any(categoricals == TRUE)) {
      final <- dummies
    }else if (any(numerics == TRUE)) {
      final <- numerics
    }
    final
}
