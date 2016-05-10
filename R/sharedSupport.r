#' Returns the number of non-missing values in the vector, 
#' 
#' Returns the number of non-misingused to determine sample size.  
#' @param x vector to determine n
#' @return n
count.notna <- function(x) { 
    sum(!is.na(x))
}

count <- count.notna

#' Calculate the geometric mean
#' 
#' Calculates a geometric mean for a numeric vector.
#'
#' @param x numeric vectr
#' @return the geometric mean of the vector
gmean <- function(x){
  exp(mean(log(x)))
}

#' Interdecile range
#' 
#' Returns interdecile range (90%-10% of population, assuming gaussian
#' distribution) of the specified data.  Quantile calculation type 2 matches
#' SAS to within 10^-6 100% of the time (for 2008 data); this is at odds with
#' the R documentation which specifies type 3 imitates SAS.  FWIW, type 5 
#' differs from SAS only 7% of the time under the same conditions.
#' @param x numeric vector
#' @return the interdecile range
idr <- function(x, method=2){
  ans <- quantile(x, probs = c(0.1, 0.9), na.rm = T, 
                  names = F, type = method)
  diff(ans)
}

#' Interquartile range with default type = 2
#' 
#' Returns interquartile range (75%-25% of population, assuming gaussian
#' distribution) of the specified data.  Quantile calculation type 2 matches
#' SAS to within 10^-6 100% of the time (for 2008 data); this is at odds with
#' the R documentation which specifies type 3 imitates SAS.  FWIW, type 5 
#' differs from SAS only 7% of the time under the same conditions.
#' @param x numeric vector
#' @return the interquartile range
iqr <- function(x, method=2){
  IQR(x, na.rm = T, type = method)
}

#' Is the set a subset of another set.
#' 
#' Returns TRUE if elements in vector a are a subset of the elements in vector 
#' b, or FALSE otherwise.
#'
#' @param a a set
#' @param b the set to test a agains
#' @return logical, \code{TRUE} if a is a subset of b
is.subset <- function(a,b){
  all(is.element(a, b))
}

# removed lag which can be replaced by zoo:::lag

#' Determines most common classes, based on counts for each class.  Counts are
#' expected to occur in the same row (i.e. long format).  This function is
#' intended to be called from within a loop.  In the case of ties, all modal
#' classes will be included, separated by a comma and a space. e.g.
#'
#'     foo<-subset(df, select='uid')
#'     foo$mode <- NA
#'     for(i in 1:nrow(df)) {
#'       foo$mode[i] <- modalClass(df[i]
#'                                , c('count1','count2','count3'...)
#'                                , c('ones','twos','threes',...)
#'                                )
#'     }
#'
#'
#' ASSUMPTIONS
#' Column names listed by values arg occur in x.
#' The list of class names is in the same order as the list of 
#' The number of elements in values is the number of elements in classes.
#'
#' @param x named values of interest, e.g. a single row of a dataframe
#' @param values list of column names in x containing cover or other values on
#'          which the modal class will be based.
#' @param classes list of class names associated with the columns, given in the order
#'          that the values are listed.
#' @return a character vector
modalClass <- function(x, values, classes){
  # separate quantities of interest in the row, just as a shortcut
  qq <- x[values]

  if(all(is.na(qq))) {
      # Mode is undefined if all values in row are missing.
      modalClasses <- NA

  } else {
      # determine locations of maximum value as a series of boolean values
      # and change any NA to FALSE
      bb <- qq==max(qq, na.rm=TRUE)
      bb <- bb==TRUE & !is.na(bb)

      # select the most common class names based on the maxima of the values
      # which were just selected.  
      modalClasses <- paste(classes[bb], collapse=', ')
  }

  return(modalClasses)
}

#' Return the value which represents the mode
#' 
#' Jason rewrote on 2011-11-02 and changed the test to reflect that the mode
#' will always be the smallest value if there are ties.  One test value returned
#' 7 for a case with a 7 and 2 with three values each.
#'
#' @param x vector of values of interest
#' @param na.rm if TRUE, will remove NA values from consideration
#' @return the value of the mode
modalvalue <- function(x, na.rm=FALSE){
  ans <- names(sort(-table(x, useNA = if(na.rm) 'no' else 'ifany')))[1]
  if (is.numeric(x)){
    ans <- as.numeric(ans)
  }
  ans
}

#' Which value is the max 
#' 
#' From package nnet
#' @param x numeric vector
#' @return the index of the maximum
which.is.max <- function (x){
    y <- seq_along(x)[x == max(x)]
    if (length(y) > 1L) 
        sample(y, 1L)
    else y
}


#' A shortcut for the common idiom to turn factors back into the numeric values
#' represented in their labels (rather than the integers used to encode the
#' factor as as.numeric would do.  See ?factor.
#' @param x a factor
#' @return a numeric vector
#' @examples
#' convertFactorToNumeric(factor(rnorm(10)))
convertFactorToNumeric <- function(x){
  stopifnot(is.factor(x))
  as.numeric(levels(x))[x]
}

#' Turns all character variables in a data frame to factors or vice versa.
#' 
#' Turns all character variables in a data frame to factors.
#' @param x a data frame
#' @return a data frame where all the characters have been turned into factors or vice versa.
#' @examples
#' data(iris)
#' str(iris)
#' str(iris <- allFacToChar(iris))
#' str(allCharToFac(iris))
allCharToFac <- function(x){
  if(!is.data.frame(x)){
    stop('Expecting a datafame.')
  }
  ischar    <- vapply(x, is.character, logical(1))
  x[ischar] <- lapply(x[ischar], as.factor)
  x
}

#' @rdname allCharToFac
allFacToChar <- function(x){
  if(!is.data.frame(x)){
    stop('Expecting a datafame.')
  }
  isfac <- vapply(x, is.factor, logical(1))
  x[isfac] <- lapply(x[isfac], as.character)
  x
}

#' Protect rowSums from a row which is all NA; return NA instead of 0.  
#' 
#' Does essentially what \code{rowSums(., na.rm = T))} does except that it returns NA
#' for any row that is all NA.  Uses a matrix oriented approach which is very fast.
#' @param x a matrix
#' @return the sum of the rows
#' @examples
#' m <- matrix(c(NA), ncol = 6, nrow = 1)
#' m
#' rowSums(m, na.rm = T)
#' rowSumsProtectNA(m)
rowSumsProtectNA <- function(x){
  nas    <- is.na(x)
  sums   <- rowSums(x, na.rm = T)
  # only check if there are rows with all NAs if there is at least one NA.  That
  # way we can avoid the rowSum if possible.
  if(any(nas)){
    ncols  <- ncol(x)
    all.na <- rowSums(nas) == ncols
    is.na(sums) <- all.na
  }
  sums
}

#' An AND operator that returns T for NA & T
#' 
#' An AND operator that returns NA for NA & NA NA %&% NA and returns TRUE for 
#' T %&% NA, mimicking the behavior of
#' function(x) {if(all(is.na(x))) { NA } else {all(x, na.rm=TRUE) } }
#' @param e1 logical vector
#' @param e2 logical vector
#' @return logical vector
#' @examples
#' e <- expand.grid(e1 = c(NA, T, F), e2 = c(NA, T, F))
#' e$e1 %&% e$e2
'%&%' <- function(e1, e2){
  ans <- pmin(e1, e2, na.rm = T)
  as.logical(ans)
}

allProtectNA <- function(x){
  if (all(is.na(x))){
    return(NA)
  } else {
    return(all(x, na.rm=T))
  }
}

quick.table <- function(x){
  if (is.factor(x)){
    tbl <- tabulate(x, nlevels(x))
    tbl <- tbl / sum(tbl)
    names(tbl) <- levels(x)
    return(tbl)
  } else {
    return(prop.table(table(x)))
  }
}

testData <- function(filename){
  f <- system.file('unitTests', 'data', filename,
                   package = 'nrsa')
  stopifnot(file.exists(f))
  varname <- gsub('.[Rr][Dd][Aa][Tt][Aa]', '', filename)
  load(f)
  val <- get(varname)
  return(val)
}

summary.nrsa <- function(x, probs = c(0.16, 0.25, 0.5, 0.75, 0.84), na.rm = F){
  qnts <- quantile(x, probs = probs, type = 2, names = F, na.rm = na.rm)
  mn <- mean(x, na.rm = na.rm)
  std <- sd(x, na.rm = na.rm) 
  iqr <- iqr(x)
  return(c(qnts, mn, std, iqr))
}

modalClass2 <- function(x, multiple = T, ...){
  col.names <- names(x)
  if (multiple){
    rowmax <- do.call(pmax, x)
    isMode <- which(x == rowmax, arr.ind = T)
    class.mode <- tapply(X     = col.names[isMode[,2]], 
                         INDEX = isMode[,1], 
                         FUN   = paste, collapse = ', ')
    class.mode <- as.character(class.mode)
  } else {
    class.mode <- col.names[max.col(x, ...)]
  }
  return(class.mode)
}

addNA2 <- function(x, name){
  x <- addNA(x)
  levels(x)[nlevels(x)] <- name
  x
}

dfCompare2 <- function(x, y){
  names(x) <- tolower(names(x))
  names(y) <- tolower(names(y))
  x <- arrange(x, uid, metric)
  y <- arrange(y, uid, metric)
  x$uid <- as.character(x$uid)
  y$uid <- as.character(y$uid)
  x$metric <- as.character(x$metric)
  y$metric <- as.character(y$metric)
  all.equal(x, y)
}

rowmean <- function(x, group, reorder = TRUE, ...){
  rs <- rowsum(x, group, reorder, ...)
  nas <- !is.na(x)
  mode(nas) <- 'double'
  n  <- rowsum(nas, group, reorder)
  rs / n
}

vswitch <- function(x, f, ...){
  funlist <- list(...)
  if (is.list(funlist[[1]])){
    funlist <- funlist[[1]]
  }
  if(is.list(f)){
    f <- interaction(f)
  } else {
    f   <- as.factor(f)
  }
  lev <- setNames(levels(f), levels(f))
  xs  <- split(x, f)
  stopifnot(length(setdiff(lev, names(funlist))) < 1)
  ret <- lapply(lev, function(i) funlist[[i]](xs[[i]]))
  len <- sapply(ret, length)
  collapse <- identical(as.integer(sum(len)), length(x))
  if (collapse){
    ret <- unsplit(ret, f)
  } else {
    ret <- unlist(ret)
  }
  ret
}
# deprecated: use revalue
# replace.levels <- function(f, list){
#   lev <- levels(f)
#   for(i in 1:length(list)){
#     lev[match(list[[i]], lev)] <- names(list[i])
#   }
#   levels(f) <- lev
#   f
# }

# deprecated: use type.convert if needed
#CharacterToLogical <- function(x){
#  if (is.factor(x)){
#    x <- as.character(x)
#  }
#  as.logical(c('FALSE' = 0, 'TRUE' = 1)[x])
#}

# Deprecated use mapvalues()
# replace2 <- function(x, old, new){
#   replace(x, match(old, x), new)
# }
