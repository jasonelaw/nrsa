# interpolatePercentile.r
#
# 03/02/10 cws Created
#  3/25/10 cws Changed diff() calls to dfCompare().
#
interpolatePercentile <- function(df, classVar, percentile, pcltVar, classBounds){
# Estimate size at a specified percentile within a sample based on recorded
# class memberships and the size boundaries of those classes at each UID.
# Originally developed for use with NRSA substrate particle diameters.
#
# Returns dataframe with columns UID and name specified by pctlVar argument
#
# ARGUMENTS
# df            dataframe with class data.
# classVar      Name of column in data containing the sampled class names, quoted.
# percentile    Real value ranging from 0 to 100 inclusive, specifying the
#                 population percentile to calculate
# pcltVar       Name of column in which calculated sizes are placed, quoted.
# classBounds   Dataframe describing max and min size of each class, containing
#                 three columns:
#                   - name specified in classVar argument
#                   min - minimum class size
#                   max - maximum class size
#
# ASSUMPTIONS:
# The dataframe df will have column UID specifying the site, and the column
#   with class information as specified by the classVar argument.
# The dataframe df will contain only those classes for which class size
#   information is made available in the classSizes argument.
  f <- function(x, classVar, percentile, pcltVar, classBounds){
    # Calculate cumulative percentages for each size class.  These will be the upper
    # bound of the percentage for that classes diameter range, and will be the lower
    # bound of the percentage for the next larger class.  The classes must be
    # ordered from small to large prior to this calculation,
    f.x <- sort(unique(c(classBounds$min, classBounds$max)))
    f.y <- c(0, cumsum(prop.table(table(x[,classVar]))))
    # Use linear interpolation to find the diameter at the specified percentile
    # of the population sample.
    ans <- approx(f.y, f.x, percentile, ties = 'ordered')$y
    names(ans) <- pcltVar
    ans
  }
  percentile <- percentile / 100
  o <- order(classBounds[, 2])
  df[,classVar] <- factor(df[, classVar], levels = classBounds[o, "CLASS"], ordered = T)
  ddply(df, .(UID), f, classVar, percentile, pcltVar, classBounds)
}

interpolatePercentile2 <- function(groups, breaks, percentile){
  # continuous variable broken into groups requires nbreaks = ngroups + 1 
  stopifnot(percentile <= 1, percentile >= 0)
  n <- length(groups)
  f.x <- breaks
  f.y <- c(0, cumsum(table(groups)) / n)
  ans <- approx(f.y, f.x, percentile, ties = 'ordered')$y
  return(ans)
}
