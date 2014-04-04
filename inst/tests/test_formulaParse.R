library(reshape2)
library(testthat)
library(stringr)
library(dplyr)

context("gformula")


test_that("G-formula is usefully parsed", {
    ex1 <- Enmesh ~ (Rater:Family) * Item
    ex2 <- Enmesh ~ (Item * Rater) : Family
    ex3 <- Enmesh ~ Item:Rater:Family

    out.ex1 <- (list(sortvars = c("Family", "Rater", "Item"),
                    specif = c("Family", "Rater:Family", "Item"),
                    value.var = "Enmesh"))
    out.ex2 <- list(sortvars = c("Family", "Rater", "Item"),
                    specif = c("Family", "Rater", "ItemRater:Family",
                        value.var = "Enmesh"))
    out.ex3 <- list(sortvars = c("Family", "Rater", "Item"),
                    specif = c("Family", "Item:Family", "Rater:Item:Family")
    expect_equal(ex1, out.ex1)
}
