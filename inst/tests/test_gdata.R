context("gdata")

write.csv(melty, file = "/home/fortis/genova/inst/testdata/test1.csv", row.names = F)

test1 <- read.csv("/home/fortis/genova/inst/testdata/test1.csv")

test1.reordered <- test1[order(test1$Item, test1$Task),]


out <- structure(list(process = as.character(test1$value),
                      gspecify = c(Person = "P",  Task = "T", Item = "I:T"),
                      object = "P",
                      formatstr = "(120F1.0)",
                      sampsizes = c(Person = 10, Task = 3, Item = 4),
                      popsizes = c(Person = 0, Task = 0, Item = 0)),
                 class = c("gdata", "list"))

    doit <- gdata(data = test1.reordered, response = "value",
                  gspecify = c(Person = "P", Task = "T", Item = "I:T"),
                  object = "P", digits = 0)

test_that("gdata correctly converts GENOVA simple run 1 data", {
    expect_equal(doit, out)
}
          )

  
testeff <- c("EFFECT      + P 10 0", "EFFECT      + T 3 0", "EFFECT      * I:T 4 0")

test_that("gEffect matches expected effects", {
    expect_equal(gEffect(doit), testeff)
})
                 



