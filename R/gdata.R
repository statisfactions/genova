##' Create \code{gdata} object from long \code{data.frame}
##'
##' Takes an \code{data.frame} and specified design and outputs a
##' \code{gdata} object that can be used to create a GENOVA control
##' file.
##'
##' @rdname gdata
##' @param data The input \code{data.frame}.  This must be in long
##' format, with one row per subject x variable combination.
##' @param response The response variable (as a string)
##' @param gspecify A character vector where the names match column
##' names in \code{data}, and each character element consists of the
##' letter and colon combinations that specifies to GENOVA the design
##' that should be used.  See Details and Examples.
##' @param popsizes Populations sizes. Default is 0 (which in GENOVA
##' means infinite populations).
##' @param digits How many digits to round all values to
##' @param object A single character that specifies which of the
##' gspecify main effects is the object of measurement.  Currently the
##' package does not support objects of measurement that are nested in facets.
##' @return A \code{gdata} object for creating GENOVA control files
##' @export
gdata <- function(data, response, gspecify, popsizes = rep(0, length(gspecify)), object, digits = 0) {
    facets <- data[, names(gspecify)] # extract just the facets
    resp <- data[do.call(order, facets), response] # extract response and order by each facet

    resp.r <- round(resp, digits)
    resp.char <- format(resp.r, nsmall = digits, trim = FALSE, scientific = 999, justify = "right")

    widths <- nchar(resp.char)
    width <- widths[1]
    
    if(!all(widths == width))
        warning("String lengths for output are not all equal.  This will throw off GENOVA.")

    formatstr <- sprintf("(%iF%i.%i)", length(resp.char), width, digits)

    structure(list(process = resp.char, gspecify = gspecify, 
                   object = object, formatstr = formatstr,
                   popsizes = popsizes),
              class = c("gdata", "list"))
}
