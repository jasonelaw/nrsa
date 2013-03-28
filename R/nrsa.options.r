nrsa.options <- function (...) 
{
    if (nargs() == 0) 
        return(.nrsa.Options)
    current <- .nrsa.Options
    temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg), list = temp <- arg, character = return(.nrsa.Options[arg]), 
            stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) 
        return(current)
    n <- names(temp)
    if (is.null(n)) 
        stop("options must be given by name")
    changed <- current[n]
    current[n] <- temp
    if (sys.parent() == 0) 
        env <- asNamespace("nrsa")
    else env <- parent.frame()
    assign(".nrsa.Options", current, envir = env)
    invisible(current)
}