library(xpose4)

context("Minimal example")

test_that("Minimal example works",{
  
  plot1 <- basic.gof(simpraz.xpdb)
  expect_equal(class(plot1)[1],"xpose.multiple.plot")
  
  plot2 <- cwres.vs.cov(simpraz.xpdb)
  expect_equal(class(plot2)[1],"xpose.multiple.plot")
  
  plot3 <- cwres.vs.idv(simpraz.xpdb)
  expect_equal(class(plot3),"trellis")
})


