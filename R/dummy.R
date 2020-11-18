#' Fast-automatic Dummy Variable Creation with Support for Predictive Contexts
#'
#' \code{dummy} creates dummy variables of all the factors and character vectors in a data frame or data table. It also supports settings in which the user only wants to compute dummies for the categorical values that were present in another data set. This is especially useful in the context of predictive modeling, in which the new (test) data has more or other categories than the training data.For computational speed, the code is written in \code{data.table}.
#'
#' @param x a data frame or data table containing at least one factor or character vector
#' @param p Only relevant if object is NULL. Select the top p values in terms of frequency. Either "all" (all categories in all variables), an integer scalar (top p categories in all variables), or a vector of integers (number of top categories per variable in order of appearance).
#' @param object output of the \code{categories} function. This parameter is to be used when dummies should be created only of categories present in another data set (e.g., training set)
#' @param num should the dummies be of class numeric (TRUE) or factor (FALSE). Setting this to TRUE will speed up execution considerably.
#' @param verbose logical. Used to show progress. Does not work when \code{parallel="variable"}.
#' @param ref logical. Only relevant when x is a data.table. If TRUE x will be overwritten by the dummy output (called transformed x), and a reference (i.e., not a copy) to the transformed x will be returned invisibly. If FALSE, x will be left untouched, and the output will be returned as usual. The difference between ref=TRUE and ref=FALSE is that the former uses less memory equal to the amount of the original x (not transformed x). If x=TRUE only the transformed x survives the function. If x=FALSE both the original x and the output (equal in size as transformed x) will survive. The difference is hence the size of the original x, and therefore ref=TRUE is more memory efficient.
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
#' #create dummies of training set
#' (dummies_train <- dummy(x=traindata))
#' #create dummies of new set
#' (dummies_new <- dummy(x=newdata))
#'
#' #how many new dummy variables should not have been created?
#' sum(! colnames(dummies_new) %in% colnames(dummies_train))
#'
#' #create dummies of new set using categories found in training set
#' (dummies_new <- dummy(x=newdata,object=categories(traindata,p="all")))
#'
#' #how many new dummy variables should not have be created?
#' sum(! colnames(dummies_new) %in% colnames(dummies_train))
#'
#'
#' #create dummies of training set,
#' #using the top 2 categories of all variables found in the training data
#' dummy(x=traindata,p=2)
#'
#' #create dummies of training set,
#' #using respectively the top 2,3 and 1 categories of the three
#' #variables found in training data
#' dummy(x=traindata,p=c(2,3,1))
#'
#' #create all dummies of training data
#' dummy(x=traindata)
#'
#' \dontrun{
#' #######################
#' #example ref parameter
#'
#' #ref=TRUE, example 1
#' (DT = data.table(a=c("a","b"),b=c("c","c")))
#' dummy(DT,ref=TRUE)
#' DT[] #DT has changed
#'
#' #ref=TRUE, example 2
#' #uses exactly same amount of memory as example 1
#' (DT = data.table(a=c("a","b"),b=c("c","c")))
#' d1 <- dummy(DT,ref=TRUE)
#' DT[] #DT has changed
#' d1[] #d1 is a reference (not a copy) to DT
#'
#' #ref=FALSE, example 3
#' #example 1 and 2 are more memory efficient than example 3
#' (DT = data.table(a=c("a","b"),b=c("c","c")))
#' d2 <- dummy(DT, ref=FALSE)
#' DT[] #DT has not changed
#' d[]
#' # deleting DT after dummy finishes would result in the same final
#' # memory footprint as example 1 and 2, except that in example 3
#' # memory usage is higher when dummy is being executed, and this may be
#' # problematic when DT is large.
#' }
#' @seealso \code{\link{categories}}
#' @return  A data frame or data table containing dummy variables. If ref=TRUE then the output will be invisible and x will contain the output. NOTE: data.table currently has a print bug. In some cases the output does not print. Running the output object multiple times or running it once with [] appended will make it print. In either case, the output will be produced. str() also always works.
#' @author Authors: Michel Ballings, and Dirk Van den Poel, Maintainer: \email{Michel.Ballings@@GMail.com}
#' @export

dummy <- function(x,
                  p="all",
                  object=NULL,
                  num=TRUE,
                  verbose=FALSE,
                  ref=FALSE){


      if(all(class(x)=="data.frame") || ref==FALSE) {
        # for data.frame
        # will break reference if used on data.table
        # (i.e., operate on object by value)
        colnames(x) <- make.names(colnames(x),TRUE)
      } else {
        # for data.table when we want to keep
        # the reference (i.e., operate on object by reference)
        # (note that the solution for data.frame
        # would work too, but it would break the reference)
        setnames(x,names(x),make.names(names(x),TRUE))
      }


      if(is.null(object)) object <- categories(x,p=p)
      ans <- list()

      len <- length(object)

      #error handling
      if (!any(class(x) %in% c("data.table","data.frame"))) stop("x needs to be either a data.frame or data.table")

      #store class of x at input
      if(all(class(x)=="data.frame")) cl <- "data.frame" else cl <- "data.table"

      #change to data.table if x was a data.frame at input
      if (!any(class(x)=="data.table")) setDT(x)

      if (verbose) cat("Start\n")
      for (i in 1:len){

      #for each value an ifelse

      ii <- 0
      envir <- environment()
      for (z in object[[i]]){

          if (num==FALSE) {
            x[, make.names(paste0(names(object)[i],"_",z),TRUE) :=  as.factor(ifelse(get(names(object)[i]) == get("z",envir= envir),1,0))]
          } else if (num==TRUE) {
            x[, make.names(paste0(names(object)[i],"_",z),TRUE) :=  ifelse(get(names(object)[i]) == get("z",envir= envir),1,0)]
          }


        if (verbose) {
            if (ii != 0 && w==z) ii <- 0
            ii <- ii + 1
            cat("   ",round((ii*100)/length(object[[i]]),0),"% of categories processed \n")
          }
        w <- z

      }

      if (verbose) cat(round((i*100)/len,0),"% of variables processed \n")
    }

    #remove original categoricals
    for (i in 1:len){
      x[ , names(object)[i]:= NULL ]
    }

    #change back to data.frame if x was a data.frame at input
    if (cl=="data.frame") setDF(x)
    #use x[] instead of x to make sure it prints
    if (any(class(x)=="data.table") && ref==TRUE){
      return(invisible(x))
    } else {
      return(x)
    }
}
