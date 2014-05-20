data(simpraz.xpdb)
xpdb <- simpraz.xpdb

## A vanilla plot
ind.plots(xpdb)

## Monochrome, suitable for manuscript or report
ind.plots(xpdb, 
          subset="ID>40 & ID<57", 
          col=c(1,1,1), 
          lty=c(0,2,3), 
          strip=function(..., bg) 
            strip.default(..., bg="grey"))

## if only one point per individual
## here I manipulate the data to create this type of plot
tmp <- subset(xpdb@Data,ID>43 & ID<57 & TIME<2)
tmp$ID  <- tmp$ID+100
tmp.2 <- subset(xpdb@Data,ID>40 & ID<43)
tmp.3 <- rbind(tmp.2,tmp)
Data(xpdb) <- tmp.3
ind.plots(xpdb)
ind.plots(xpdb,pch.ip.sp=c(21,1,2),cex.ip.sp=c(1.5,1,1))
