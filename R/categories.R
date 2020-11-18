#' Extraction of Categorical Values as a Preprocessing Step for Making Dummy Variables
#'
#'  \code{categories} stores all the categorical values that are present in the factors and character vectors of a data frame. Numeric and integer vectors are ignored. It is a preprocessing step for the \code{dummy} function. This function is appropriate for settings in which the user only wants to compute dummies for the categorical values that were present in another data set. This is especially useful in predictive modeling, when the new (test) data has more or other categories than the training data.
#'
#' @param x data frame or data table containing factors or character vectors that need to be transformed to dummies. Numerics, dates and integers will be ignored.
#' @param p select the top p values in terms of frequency. Either "all" (all categories in all variables), an integer scalar (top p categories in all variables), or a vector of integers (number of top categories per variable in order of appearance.
#' @examples
#' #create toy data
#' (traindata <- data.frame(var1=as.factor(c("a","b","b","c")),
#'                          var2=as.factor(c(1,1,2,3)),
#'                          var3=c("val1","val2","val3","val3"),
#'                          stringsAsFactors=FALSE))
#' (newdata <- data.frame(var1=as.factor(c("a","b","b","c","d","d")),
#'                        var2=as.factor(c(1,1,2,3,4,5)),
#'                        var3=c("val1","val2","val3","val3","val4","val4"),
#'                        stringsAsFactors=FALSE))
#'
#' categories(x=traindata,p="all")
#' categories(x=traindata,p=2)
#' categories(x=traindata,p=c(2,1,3))
#' @seealso \code{\link{dummy}}
#' @return  A list containing the variable names and the categories
#' @author Authors: Michel Ballings, and Dirk Van den Poel, Maintainer: \email{Michel.Ballings@@GMail.com}
#' @export


categories <- function(x,p="all"){
  colnames(x) <- make.names(colnames(x),TRUE)
  categoricals <- which(sapply(x,function(x) is.factor(x) || is.character(x)))


      if (!any(class(x)=="data.table")){ #if data.frame
          x <- x[,categoricals, drop=FALSE]
      } else if (any(class(x)=="data.table")){ #if data.table
          x <- x[,categoricals, with=FALSE]
      }

      cats <- sapply(1:ncol(x),function(z) {

        if (!any(class(x)=="data.table")){  #if data.frame
            cats <- table(x[,z])
        } else if (any(class(x)=="data.table")){ #if data.table
            cats <- table(x[,z, with=FALSE])
        }
        if(is.numeric(p) && length(p) == 1) {
          names(sort(cats,decreasing=TRUE)[1:if(length(cats) <= p) length(cats) else p])
        } else if (is.numeric(p) && length(p) > 1) {
          names(sort(cats,decreasing=TRUE)[1:if(length(cats) <= p[z]) length(cats) else p[z]])
        } else if (p=="all") {
          names(cats)
        }
      },simplify=FALSE)
      names(cats) <- names(x)
      return(cats)
}
