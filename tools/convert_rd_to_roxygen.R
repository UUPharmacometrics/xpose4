library(Rd2roxygen)
rd.file <- "man/read.nm.tables.Rd"
str(info <- parse_file(rd.file))
cat(create_roxygen(info), sep = "\n")
