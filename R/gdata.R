##' Create \code{gdata} object from long \code{data.frame}
##'
##' Takes an \code{data.frame} and specified design and outputs a
##' \code{gdata} object that can be used to create a GENOVA control
##' file.
##'
##' @rdname gdata
##' @param data The input \code{data.frame}.  This must be in long
##' format, with one row per subject x variable combination. Data MUST
##' be balanced and this is NOT checked by this function.
##' @param response The response variable (as a string)
##' @param gspecify A character vector where the names match column
##' names in \code{data}, and each character element consists of the
##' letter and colon combinations that specifies to GENOVA the design
##' that should be used.  See Details and Examples.
##' @param sampsizes The sample sizes for each facet.  By default, the
##' unique number of values in each facet are counted.  If you are
##' relying on the default, nested facets should use repeating values:
##' if 3 Items are nested in 2 Occasions, the sequence of identifiers
##' should be 1, 2, 3, 1, 2, 3, not 1, 2, 3, 4, 5, 6.  Otherwise facet
##' sample size will not be correctly counted.
##' @param popsizes Populations sizes. Default is 0 (which in GENOVA
##' means infinite populations).
##' @param digits How many digits to round all values to
##' @param object A single character that specifies which of the
##' gspecify main effects is the object of measurement.  Currently the
##' package does not support objects of measurement that are nested in facets.
##' @return A \code{gdata} object for creating GENOVA control files
##' @export
gdata <- function(data, response, gspecify, sampsizes = NA,
                  popsizes = rep(0, length(gspecify)), object, digits = 0) {
    fnames <- names(gspecify)
    facets <- data[, fnames] # extract just the facets

    if(is.na(sampsizes))
        sampsizes <- sapply(facets, function(x) length(unique(x))) # count number

    resp <- data[do.call(order, facets), response] # extract response and order by each facet

    resp.r <- round(resp, digits)
    resp.char <- format(resp.r, nsmall = digits, trim = FALSE, scientific = 999, justify = "right")

    widths <- nchar(resp.char)
    width <- widths[1]
    
    if(!all(widths == width))
        warning("String lengths for output are not all equal.  This will throw off GENOVA.")

    ## Put into character matrix by first facet
    resp.mat <- matrix(resp.char, nrow = sampsizes[1], byrow = T)

    ## Add one extra level of padding to front and between numbers to make readable
    resp.mat[,1] <- paste0(" ", resp.mat[,1])
    resp.out <- apply(resp.mat, MARGIN = 1, function(x) paste(x, collapse = " "))
    width <- width + 1 # since we've boosted the width in this process

    formatstr <- sprintf("(%iF%i.%i)", ncol(resp.mat), width, digits)

    structure(list(process = resp.out, gspecify = gspecify, 
                   object = object, formatstr = formatstr,
                   sampsizes = structure(sampsizes, names = fnames),
                   popsizes = structure(popsizes, names = fnames)),
              class = c("gdata", "list"))
}
