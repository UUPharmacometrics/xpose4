library(Rd2roxygen)
rd.file <- "man/xpose.data.Rd"
str(info <- parse_file(rd.file))
cat(create_roxygen(info), sep = "\n")
