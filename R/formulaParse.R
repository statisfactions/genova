
library(reshape2)
library(testthat)
library(stringr)
library(dplyr)

initialClean <- function(formula) {
    formchar <- as.character(formula)
    rhs <- formchar[3]
    rhs <- str_replace_all(rhs, " ", "") # get rid of all spaces
}

rhs <- initialClean(ex2)

##' Split by top level of parens (can extend this function to be more
##' general, once I figure out how to do more parens.
##' 
##' Parentheses are ALWAYS interpreted as being between different
##' operations; no extraneous ones (e.g. p*(r*q) will confuse this
##' operation
##' ALSO, does not currently handle designs with two levels
##' of parentheses e.g. ((i:p)xj):n (can extend soon by recursive
##' logic)
parenSplit <- function(x) {
    str_split(rhs, "[\\)\\(]")[[1]][-1]
}

spl <- parenSplit(rhs)
cstarts <- str_sub(spl, 1, 1) == ":"
neststart <- which(diff(cstarts) == 1)

neststop <- which(diff(cstarts) == -1)

if(length(neststart) > length(neststop))
    ## This happens if the last item(s) are nested
    neststop <- c(neststop, length(spl))

nests <- cbind(neststart, neststop)

##' Idea: isn't everything organized from right to left?  Doesn't that
##' ordering basically always work? Then, the only tricky part is
##' converting the specification to GENOVA format.
##'
##' Everything within an "nest" needs to be explicitly specified.
##' Hypothesis: this is the EFFECT card for the leftmost variable
##' within the nest.
##'
##' But, I believe each level of the "nest" needs to be specified,
##' too. This is doable, (for i in (neststop-1):neststart), do every
##' term working backwards.  Each of these is a "leftmost"

## termlist <- str_split(gterms, ":")
## termlist <- lapply(termlist, function(x) data.frame(var = x, pos = 1:length(x)))
## names(termlist) <- gterms

## termm <- melt(termlist)
## termm <- arrange(termm, -value)
## sortvars <- unique(as.character(termm$var))
## specif <- gterms


##' This function parses the formula specified in a Brennan-like way
##' and outputs the variables in the order that they should appear in GENOVA
##' The function sorts first by "mainness", then by the order in which they appear
formConvert <- function(formula) {
    formchar <- as.character(formula)
    value.var <- formchar[2]
    
}
