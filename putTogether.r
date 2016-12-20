################################
# Peter Pank 
# 16-12-19
# 
################################


setwd("/Volumes/hd2/mlakz/work/")

refDate <- as.Date("2016-11-25")

library(timeDate)


d0 <- read.csv("./PADASA3.txt")
d0$dateTime <- as.POSIXlt(strptime(substr(d0$EVENTTIME,1,19), "%d/%m/%Y %H:%M:%S")) 
d0$mfm <- d0$dateTime$hour * 60 + d0$dateTime$min
dRef <- d0[as.Date(d0$dateTime)==refDate,] 
dRef2  <-  cbind(dRef$HOUSEKEY,dRef$mfm)
colnames(dRef2) <- c("hk","target")
for (i in 1:10){
    if (i == 1) {
        d  <- read.csv("out1.csv")
    } else {
        d  <-  rbind(d,read.csv(paste("out",i,".csv",sep="")))
    }
}
###########################
dOut <-  merge(d,dRef,by.x="hkList" , by.y="hk")
write.csv(d,"outAll.csv")
write.csv(dRef,"target.csv")

