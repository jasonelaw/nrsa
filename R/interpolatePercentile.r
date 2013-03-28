# # interpolatePercentile.r
# #
# # 03/02/10 cws Created
# #  3/25/10 cws Changed diff() calls to dfCompare().
# #
# 
# require(RUnit)
# 
# interpolatePercentile <- function(df, classVar, percentile, pctlVar, classBounds)
# # Estimate size at a specified percentile within a sample based on recorded
# # class memberships and the size boundaries of those classes at each UID.
# Originally developed for use with NRSA substrate particle diameters.
# #
# # Returns dataframe with columns UID and name specified by pctlVar argument
# #
# # ARGUMENTS
# # df            dataframe with class data.
# # classVar      Name of column in data containing the sampled class names, quoted.
# # percentile    Real value ranging from 0 to 100 inclusive, specifying the
# #                 population percentile to calculate
# # pcltVar       Name of column in which calculated sizes are placed, quoted.
# # classBounds   Dataframe describing max and min size of each class, containing
# #                 three columns:
# #                   - name specified in classVar argument
# #                   min - minimum class size
# #                   max - maximum class size
# #
# # ASSUMPTIONS:
# # The dataframe df will have column UID specifying the site, and the column
# #   with class information as specified by the classVar argument.
# # The dataframe df will contain only those classes for which class size
# #   information is made available in the classSizes argument.
# {
#   # Count class occurrences and total sample sizes at each site, and calculate
#   # percent occurence of each class at a site.
#   df <- subset(df, !is.na(classVar))
#   classCounts <- aggregate(list('classCount'=df[[classVar]])
#                           ,list('UID'=df$UID, 'CLASS'=df[[classVar]])
#                           ,count
#                           )
#   sampleSizes <- aggregate(list('totalCount'=df[[classVar]])
#                           ,list('UID'=df$UID)
#                           ,count
#                           )
#   classPcts <- merge(classCounts, sampleSizes, by='UID')
#   classPcts$pct <- 100 * classPcts$classCount / classPcts$totalCount
# 
#   # Calculate cumulative percentages for each size class.  These will be the upper
#   # bound of the percentage for that classes diameter range, and will be the lower
#   # bound of the percentage for the next larger class.  The classes must be
#   # ordered from small to large prior to this calculation, so this is a good time
#   # to fold in the bounds for each class so that we can then order by ascending
#   # size.  (Note that while ave() does not require inclusion of UID in the order to
#   # to correctly calculate cumulative summations, it is required by lag() in the
#   # next step).
#   classPcts <- merge(classPcts, classBounds, by='CLASS', all.x=TRUE)
#   classPcts <- classPcts[order(classPcts$UID, classPcts$min),]
# 
#   classPcts$upperPct <- ave(classPcts$pct, classPcts$UID, FUN=cumsum)
#   classPcts <- first(classPcts, 'UID', 'start')
#   classPcts <- lag(classPcts, 'upperPct', 'lowerPct')
#   classPcts[classPcts$start,]$lowerPct <- 0
# 
#   # Use linear interpolation to find the diameter at the specified percentile
#   # of the population sample.
#   tt <- subset(classPcts, lowerPct < percentile & percentile <= upperPct)
#   tt[pctlVar] <- with(tt, min+ (max-min) * (percentile-lowerPct)/(upperPct-lowerPct))
#   tt <- tt[c('UID',pctlVar)]
# 
#   return(tt)
# }
# 
# 
