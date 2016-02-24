library(xpose4)

context("Reading data")

test_that("readr methods work",{
  skip_on_cran()
  if(!run_local_tests) skip("Examples with local file dependencies")
 
  compare_xpdb <- function (xpdb_1, xpdb_2) {
    expect_equal(xpdb_1@SData,xpdb_2@SData)
    expect_equal(xpdb_1@Data,xpdb_2@Data)
  }
  
   
  setwd("/Users/ahooker/Documents/_PROJECTS/Xpose/Examples/other_examples/Andy_Simpraz")
  
  sink("tmp.txt")
  xpdb <- xpose.data(5)
  xpdb_1 <- xpose.data(5,method="slow") 
  #xpdb_2 <- xpose.data(5,method="readr_2") 
  #xpdb_3 <- xpose.data(5,method="readr_3") 
  sink()
  file.remove("tmp.txt")
  
  compare_xpdb(xpdb,xpdb_1)
  #compare_xpdb(xpdb,xpdb_2)
  #compare_xpdb(xpdb,xpdb_3)
  
  setwd("/Users/ahooker/Documents/_PROJECTS/Xpose/Examples/other_examples/final_954")
  
  sink("tmp.txt")
  xpdb <- xpose.data(954)
  xpdb_1 <- xpose.data(954,method="slow") 
  sink()
  file.remove("tmp.txt")
  
  compare_xpdb(xpdb,xpdb_1)
  
  setwd("/Users/ahooker/Documents/_PROJECTS/Xpose/Examples/other_examples/hands_on_2_solutions")
  sink("tmp.txt")
  xpdb <- xpose.data(12)
  xpdb_1 <- xpose.data(12,method="slow") 
  sink()
  file.remove("tmp.txt")
  
  compare_xpdb(xpdb,xpdb_1)
  
})