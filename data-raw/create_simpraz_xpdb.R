od = setwd(tempdir()) # move to a temp directory
(cur.files <- dir()) # current files in temp directory

simprazExample(overwrite=TRUE) # write files
(new.files <- dir()[!(dir() %in% cur.files)])  # what files are new here?

system("source ~/.bashrc; execute run1.mod")

(new.files <- dir()[!(dir() %in% cur.files)])  # what files are new here now?

xpdb <- xpose.data(1)

xvardef("covariates",xpdb)
change.xvardef(xpdb,var="covariates") <- c("SEX",  "RACE", "SMOK", "HCTZ", "PROP", "CON",  "OCC",  "AGE",  "HT",   "WT",   "SECR")
xvardef("covariates",xpdb)

runsum(xpdb)
cwres.vs.cov(xpdb)

#save old file
simpraz.xpdb.old <- simpraz.xpdb
save(simpraz.xpdb.old,file = "~/Documents/_PROJECTS/Xpose/repos/xpose4/data/simpraz.xpdb.old.RData")

# save new version
simpraz.xpdb <- xpdb
save(simpraz.xpdb,file = "~/Documents/_PROJECTS/Xpose/repos/xpose4/data/simpraz.xpdb.RData")

(new.files <- dir()[!(dir() %in% cur.files)])  # what files are new here now?
unlink(new.files,recursive = T) # remove these files
setwd(od)  # restore working directory
