library(reshape2)
library(testthat)
library(stringr)
library(dplyr)

## Reload files
files <- dir("../R/", ".+R$", full.names = T)
lapply(files, source)

## NEW IDEA:
ex1 <-  ~ (Family/Rater) * Item
ex2 <-  ~  Family / (Item * Rater) 
ex3 <-  ~ Family/Rater/Item
ex4 <-  ~ (Family * Item)/Rater
ex5 <- ~ Family * Item * Rater


getFact <- function(x) attr(terms(x), "factors")

getLefts <- function(x) {
    fact <- getFact(x)
    facetstr <- sapply(1:nrow(fact), function(x) {
        oner <- fact[x,]
        names(oner)[min(which(oner == 1))]
    })
    structure(facetstr, names = rownames(fact))
}

toLetters <- function(left) {
    ## after doing getlefts, reverse the names around the colons
    ## Replace by Letters
    facets <- names(left)
    facetletters <- assignLetters(facets)
    letform <- left

    for(i in 1:length(facetletters)) {
        letform <- str_replace_all(letform, names(facetletters)[i], facetletters[i])
    }

    ## Reverse letters to prepare for GENOVA
    sapply(strsplit(letform, ""), function(x) 
       paste0(rev(x), collapse = ""))
}

lf3 <- toLetters(left)
lf4 <- toLetters(getLefts(ex4)) ## Example where I need to delete colon
colons <- structure(str_locate_all(lf4, ":"), names = names(lf4))

lf <- lf4

colRemove <- function(lf) {
    for(i in names(lf)) {
        ## We work backwards and ignore first one
        curcols <- rev(colons[[i]][-1,1]) 

        for(j in curcols) {
            ## Look at expression around this colon

            expr <- substr(lf[[i]], j-1, j+1)

            ## Does this appear in terms besides this one? If not,
            ## Delete this colon
            inlf <- length(grep(expr, lf)) > 1 
            if(!inlf)
                substr(lf[[i]], j, j) <- ""
        }
    }
    lf
}

## rhs <- initialClean(ex2)

## spl <- parenSplit(rhs)
## cstarts <- str_sub(spl, 1, 1) == ":"
## neststart <- which(diff(cstarts) == 1)

## neststop <- which(diff(cstarts) == -1)

## if(length(neststart) > length(neststop))
##     ## This happens if the last item(s) are nested
##     neststop <- c(neststop, length(spl))

## nests <- cbind(neststart, neststop)

context("gformula")


ex1 <- value ~ (Item:Task) * Person

getFacetNames(initialClean(ex1))

ex1.test <- structure(list(value.var = "value",
                           gspecify =
                           c(Person = "P",
                             Task = "T",
                             Item = "I:T")))

## Desired behavior: choose first letter; if duplicates
## just start from beginning of alphabet for unused letter

test_that("G-formula is usefully parsed", {

})

test1 <- read.csv("/home/fortis/genova/inst/testdata/test1.csv")
test1.reordered <- test1[order(test1$Item, test1$Task),]
doit <- gdata(data = test1.reordered, response = "value",
              gspecify = c(Person = "P", Task = "T", Item = "I:T"),
              object = "P", digits = 0)

          
