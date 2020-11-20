#' Aggregate numeric, Date and categorical variables
#'
#' The \code{Aggregate} function (not to be confounded with aggregate) prepares a data frame or data table for merging by computing the sum, mean and variance of all continuous (integer and numeric) variables by a given variable. For all categorical variabes (character and factor), it creates dummies and subsequently computes the sum and the mode by a given variable. For all Date variables, it computes the recency and duration by a given variable with repsect the an end date variable. For computational speed, all the calculations are done with \code{data.table}. This functions aims at maximum information extraction with a minimum amount of code.
#'
#' @param x A data frame or data table. Categorical variables have to be of type character or factor and continuous variables have to be of type integer or numeric. Date variables should be in the Date format.
#' @param by A character string specifying the variable on which to aggregate the results. Note that 'by' should be a variable of the table 'x'.
#' @param end_ind A Date object, or something which can be coerced by \code{as.Date(origin, ...)} to such an object. If not specified,  we take the \code{Sys.Date()} as end date.
#' @param format A character string. If not specified, the ISO 8601 international standard which expresses a day "\%Y-\%m-\%d" is taken.
#' @param tibble Should the output be a tibble, data frame or data table? By default, the function returns a data frame or data table depending on the input. To return a tibble, the user must set the tibble = TRUE.
#' @param verbose indicator Used to show the progress.
#' @param object Parameter related to the \code{dummy} function. See ?\code{dummy} for more information.
#' @param p Parameter related to the \code{dummy} function. See ?\code{dummy} for more information.
#'
#' @return A data frame, data table or tibble with the aforementioned variables aggregated by the given ID variables. If the input is a data frame, a data frame is returned else a data table is returned.
#'
#' @author Authors: Matthias Bogaert, Michel Ballings, Dirk Van den Poel, Maintainer: \email{matthias.bogaert@@UGent.be}
#' @examples
#' # Example
#' # Create some data
#' data <- data.frame(V1=sample(as.factor(c('yes','no')), 200000, TRUE),
#'                   V2=sample(as.character(c(1,2,3,4,5)),200000, TRUE),
#'                   V3=sample(1:20000,200000, TRUE),
#'                   V4=sample(300:1000, 200000, TRUE),
#'                   V5 = sample(as.Date(as.Date('2014-12-09'):Sys.Date()-1,
#'                   origin = "1970-01-01"),200000,TRUE),
#'                   ID=sample(x = as.character(1:4), size = 200000, replace = TRUE))
#'
#' Aggregate(x=data,by='ID')
#'
#' # Examples of how to use the object and p argument. See dummy and categories function for details.
#' # Aggregate(x=data,by='ID',object=categories(data))
#' # Aggregate(x=data,by='ID',p=2)
#' @import stats methods data.table NCmisc
#' @importFrom utils tail
#' @importFrom tibble as_tibble
#' @export

# From now on the x parameter needs to be a data frame/data table WITH the by column.
# From now on the by parameter needs to be the name of the by column in quotes
# From now on all the analyses are done in data.table and then transformed to data.frame or data.table
# Allow print out in a tibble by setting tibble = TRUE. Default = FALSE
Aggregate <- function (x, by, end_ind = Sys.Date(), format = '%Y-%m-%d', tibble = FALSE, verbose = TRUE,
                       object = NULL, p = "all") {

  options(stringsAsFactors = FALSE)

  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # problem handling
  if (!any(sapply(by,is.character))) stop('by-variable must be a character!')
  if (class(end_ind) != 'Date') stop('The end_ind should be of class Date')
  if (!any(class(x) %in% c("data.table", "data.frame")))stop("x needs to be either a data.frame or data.table")
  if (all(class(x) == "data.frame")) cl <- "data.frame" else cl <- "data.table"
  if (!any(class(x) == "data.table")) setDT(x)

  ###NEW: Assume x is data table for speed issues

    categoricals <- (sapply(x, is.factor) | sapply(x, is.character)) & colnames(x) != by ###NEW
    if (any(categoricals == TRUE)) {
      if(verbose == TRUE) cat('Calculating categorical variables ... \n')

      if(!is.null(object)) {
        ind <- which(names(object) == by)
        object <- object[-ind]
      }

      dummies_df <- dummy(x = x[,categoricals,with=FALSE],num = TRUE, ref = TRUE,
                          object = object, p = p) ### Works with the new dummy function
      dummies_df <- cbind(x[,by, with = FALSE], dummies_df)
      dummies_df <- dummies_df[,as.list(unlist(lapply(.SD, function(x) list(sum=sum(x),
                                                                              mode=Mode(x))))),
                                 by=eval(by)]###NEW
      names(dummies_df) <- gsub('[.]','_',names(dummies_df))
    }



    numerics <- sapply(x, is.numeric) | sapply (x, is.integer)
    if (any(numerics==TRUE)) {
      if(verbose == TRUE) cat('Calculating numerical variables ... \n')
      numerics <- numerics | colnames(x) == by
      numerics_df <- x[, numerics, with = FALSE]
      numerics_df <- numerics_df[,as.list(unlist(lapply(.SD, function(x) list(sum=sum(x),
                                                                                mean=mean(x),
                                                                                var=var(x))))),
                                   by=eval(by)]
      names(numerics_df) <- gsub('[.]','_',names(numerics_df))

    }

    dates <- sapply(x,function(z) is(z,"Date"))
    if(any(dates == TRUE)) {
      if(verbose == TRUE) cat('Calculating date variables ... \n')
      end_ind <- as.Date(end_ind, format = format)
      dates <- dates | colnames(x) == by
      dates_df <- x[,dates, with = FALSE]
      dates_df <- dates_df[, as.list(unlist(lapply(.SD, function (x) list(duration = end_ind - min(x),
                                                                            recency = end_ind - max(x) )))),
                             by = eval(by)]
      names(dates_df) <- gsub('[.]','_',names(dates_df))
    }


    if (any(dates == TRUE) && any(categoricals == TRUE) && any(numerics == TRUE)) {
      mergelist <- list(dummies_df, numerics_df, dates_df)
      final <- Reduce(function(x,y) merge(x,y, by = by), mergelist)
      if (cl == 'data.frame') final <- data.frame(final)
      else final <- data.table (final)
    } else if (any(categoricals == TRUE) && any(numerics == TRUE)) {
      final <- merge(dummies_df, numerics_df, by = by)
      if (cl == 'data.frame') final <- data.frame(final)
      else final <- data.table (final)
    } else if (any(categoricals == TRUE) && any(dates == TRUE)) {
      final <- merge(dummies_df, dates_df, by = by)
      if (cl == 'data.frame') final <- data.frame(final)
      else final <- data.table (final)
    } else if (any(dates == TRUE) && any(numerics == TRUE)) {
      final <- merge(numerics_df, dates_df, by = by)
      if (cl == 'data.frame') final <- data.frame(final)
      else final <- data.table (final)
    } else if (any(categoricals == TRUE)) {
      if (cl == 'data.frame') final <- data.frame(dummies_df)
      else final <- data.table (dummies_df)
    } else if (any(numerics == TRUE)) {
      if (cl == 'data.frame') final <- data.frame(numerics_df)
      else final <- data.table (numerics_df)
    } else if (any(dates == TRUE)) {
      if (cl == 'data.frame') final <- data.frame(dates_df)
      else final <- data.table (dates_df)
    }

    if(tibble) as_tibble(final) else final
}

