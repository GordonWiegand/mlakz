setwd("/Volumes/hd2/mlakz")
rm(list=ls())

refDate <- as.Date("2016-11-25")
daysBack <- 20
noNeigh <- 5

library(timeDate)
d0 <- read.csv("./work/PADASA3.txt")
dZub <- read.csv("./work/ZUBOFI3.txt")
dZub$AKZ_VALID_TO <- as.POSIXlt(strptime(dZub$AKZ_VALID_TO , "%d/%m/%Y"))
dZub <- dZub[is.na(dZub$AKZ_VALID_TO),]

d0$dateTime <- as.POSIXlt(strptime(substr(d0$EVENTTIME,1,19), "%d/%m/%Y %H:%M:%S"))

d0$mfm <- d0$dateTime$hour * 60 + d0$dateTime$min

d1 <- d0[(as.Date(d0$dateTime) > (refDate-daysBack)) & as.Date(d0$dateTime) < refDate,]
dRef <- d0[as.Date(d0$dateTime)==refDate,]

noWeeks <- as.integer(ceiling(difftime(max(d0$dateTime), min(d0$dateTime),units="weeks")))

hkList <- unique(dRef$HOUSEKEY)
out <- as.data.frame(matrix(nrow=length(hkList),ncol=14))
out2 <- as.data.frame(matrix(nrow=length(hkList),ncol=14))
zubFail <- logical(length(hkList))
for (i in 1:length(hkList)){ 
    dHouse <- d1[d1$HOUSEKEY == hkList[i],]
    outLoopInHouse <- numeric()
    if (exists("namesWeekDays")) {rm(namesWeekDays)}
    for (id in 1:daysBack){

        if (!exists("namesWeekDays")){
            week <- 0
            namesWeekDays <- paste(dayOfWeek(timeDate(refDate - id)),week,sep="-")
        } else {
            if (isWeekday(timeDate(refDate - id))){
                namesWeekDays <- c(namesWeekDays ,paste(dayOfWeek(timeDate(refDate - id)),week,sep="-"))
            }
            if (dayOfWeek(timeDate(refDate - id)) == "Fri"){
                week <- week + 1
            } 
        }
        index <- id - week * 2
        if (isWeekday(timeDate(refDate - id))){
            outLoopInHouse[index] <- median(dHouse$mfm[as.Date(dHouse$dateTime)==(refDate-id)])
        } 

    }
    out[i,] <- outLoopInHouse
    colnames(out) <- paste("in",namesWeekDays,sep="")

    ###############################################3
    tour <- dRef$TOUR[dRef$HOUSEKEY == hkList[i]][1]
    distZip <- dRef$DISTRIBUTIONZIP[dRef$HOUSEKEY == hkList[i]][1]
    if (hkList[i] %in% dZub$AKZ_HAUSKEY[(dZub$AKZ_DISTR_ZIP == distZip) & (dZub$AKZ_TOUR_NR == tour)]) {
            posMax <- max(dZub$AKZ_POS[(dZub$AKZ_DISTR_ZIP == distZip) & (dZub$AKZ_TOUR_NR == tour)])
            posAct <- dZub$AKZ_POS[dZub$AKZ_HAUSKEY == hkList[i]][1]



            if ((posAct > noNeigh) & (posAct < posMax - noNeigh)) {
                dTourZub <- dZub[((dZub$AKZ_DISTR_ZIP == distZip) & (dZub$AKZ_TOUR_NR == tour)),]
                dTourZub <- dTourZub[((dTourZub$AKZ_POS >= (posAct - noNeigh)) & (dTourZub$AKZ_POS <= (posAct + noNeigh))),] 
                dTourZub <- dTourZub[(dTourZub$AKZ_POS != posAct),]
                lNeigh <- dTourZub$AKZ_HAUSKEY
            }

            if (posAct < noNeigh) {
                dTourZub <- dZub[((dZub$AKZ_DISTR_ZIP == distZip) & (dZub$AKZ_TOUR_NR == tour)),] 
                dTourZub <- dTourZub[(dTourZub$AKZ_POS %in% 1:10 ),] 
                dTourZub <- dTourZub[(dTourZub$AKZ_POS != posAct),]
                lNeigh <- dTourZub$AKZ_HAUSKEY
            }


            if (posAct > posMax - noNeigh) {
                dTourZub <- dZub[((dZub$AKZ_DISTR_ZIP == distZip) & (dZub$AKZ_TOUR_NR == tour)),] 
                dTourZub <- dTourZub[(dTourZub$AKZ_POS %in% ((posMax - 10):posMax)),] 
                dTourZub <- dTourZub[(dTourZub$AKZ_POS != posAct),]
                lNeigh <- dTourZub$AKZ_HAUSKEY
            }

            dHouse <- d1[d1$HOUSEKEY %in% lNeigh,]
            outLoopOutHouse <- numeric()
            rm(namesWeekDays)

            for (id in 1:daysBack){

                if (!exists("namesWeekDays")){
                    week <- 0
                    namesWeekDays <- paste(dayOfWeek(timeDate(refDate - id)),week,sep="-")
                } else {
                    if (isWeekday(timeDate(refDate - id))){
                        namesWeekDays <- c(namesWeekDays ,paste(dayOfWeek(timeDate(refDate - id)),week,sep="-"))
                    }
                    if (dayOfWeek(timeDate(refDate - id)) == "Fri"){
                        week <- week + 1
                    } 
                }
                index <- id - week * 2
                if (isWeekday(timeDate(refDate - id))){
                    outLoopInHouse[index] <- median(dHouse$mfm[as.Date(dHouse$dateTime)==(refDate-id)])
                } 

            }
            out2[i,] <- outLoopInHouse
            colnames(out2) <- paste("neigh",namesWeekDays,sep="")
}else {
    zubFail[i] <- T
}
print(paste(i,"von",length(hkList)))
    ###############################################3
}
outFinal <- cbind(hkList,zubFail,out,out2)
write.csv(outFinal,"./work/out.csv")







