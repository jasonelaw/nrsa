# Temporary quick function to look for illegal/unexpected units


quickUnitsCheck <- function(df, meta.df)
# ARGUMENTS:
# df       - dataframe to check for illegal parameter-units combinations.
# meta.df  - parameter descriptions with parameter-units information
{
  if('UNITS' %in% names(df)) {
      # Standardize UNITS when missing
      df$UNITS[is.na(df$UNITS)] <- ''
      meta.df$UNITS[is.na(meta.df$UNITS)] <- ''

      # Count parameter-units pairs that are present in the dataframe and label
      # them as expected or not.
      expectedPairs <- unique(paste(meta.df$PARAMETER, meta.df$UNITS))
      presentPairs <- paste(df$PARAMETER, df$UNITS)

      counts <- table(presentPairs
                     ,ifelse(presentPairs %in% expectedPairs
                            ,'OK'
                            ,'Not OK'
                            )
                     , useNA='ifany'
                     )

      counts
  } else {
      print('This table does not have UNITS column')
  }
}
