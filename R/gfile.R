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
##' @param digits How many digits to round all values to
##'
##' @return A \code{gdata} object for creating GENOVA control files
gdata <- function(data, response, gspecify, digits = 0, width = 2) {
}


specify <- c(Person = "P", Rater = "R:P", Item = "I")



##' gdataChar 
gdataChar <- function(data, , value.var, digits = 0, width = 2) {
    ## Generate character matrix from data
    ## that is GENOVA-ready

    wide <- acast(data = data, formula = formula, value.var = value.var)
    wide.r <- round(wide, digits)

    wide.p <- format(wide.r, nsmall = digits, trim = FALSE, width = width)
}

gfile <- function(gdata, outfile, measure,
                  bottom = "/home/fortis/Dropbox/coursework/advanced-Measurement-EPSY-8222/Part-I/Project/Analysis/GENOVA-in/bottom") {

    top <- c(sprintf("STUDY       %s (R:F) X I DESIGN", measure), 
             "COMMENT\t    R = \"Rater\"", 
             "COMMENT\t    F = \"Family\"", "COMMENT\t    I = \"Item\"", "OPTIONS     RECORDS NONE   CORRELATION", 
             "EFFECT      * F 308 0", "EFFECT      + I 7 0", "EFFECT      + R:F 3 0", 
             "FORMAT      (21F2.0)", "PROCESS     ")
        ## Output & bind together top, middle, & bottom of GENOVA control file for ToNga analysis
     bottom <-
        c("DSTUDY        FIRST SET OF D STUDY CONTROL CARDS", "DEFFECT       $ F", 
          "DEFFECT         I 7", "DEFFECT         R:F 3", "ENDDSTUDY", 
          "FINISH", "            ")

    write.table(top, file = outfile, quote = FALSE, sep = "", row.names = F, col.names = F)
    write.table(gdata, file = outfile, quote = FALSE, sep = "", row.names = F, col.names = F, append = T)
    write.table(bottom, file = outfile, quote = FALSE, sep = "", row.names = F, col.names = F, append = T)
}

grun <- function(input, output,
                 gpath = "/home/fortis/Dropbox/coursework/advanced-Measurement-EPSY-8222/Part-I/Project/Analysis/genova36.exe"
                 ) {
    system2("wine", args = c(gpath, input, output))
}

outputPDF <- function(input, output) {
    system(sprintf("enscript %s -r -f Courier8 -a 3- -b '' --output=- | ps2pdf - > %s", input, output))
}
