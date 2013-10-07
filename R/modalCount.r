# modalCount.r
#
# 03/10/10 cws created
#

modalCount <- function(x)
# Returns the number of occurences as an integer of the most common (mode)
# element in x.
{
  v <- modalvalue(x)
  n <- sum(x==v)

  return(n)
}



# end of file