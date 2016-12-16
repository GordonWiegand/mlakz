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
for (i in length(hkList)){ 
    dHouse <- d1[d1$HOUSEKEY == hkList[i],]
    outLoopInHouse <- numeric()
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
    
##########################3
Hier überprüfen!!



    dHouse <- d1[d1$HOUSEKEY %in% lNeigh,]
    outLoopOutHouse <- numeric()
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
    ###############################################3
}

tour <- dRef$TOUR[dRef$HOUSEKEY == hkList[i]][1]    
distzip <- dRef$DISTRIBUTIONZIP[dRef$HOUSEKEY== hkList[i]][1]    
inRoute <- (d1$TOUR==tour & d1$DISTRIBUTIONZIP==distzip & d1$HOUSEKEY !=hkList[i])

dLoopNei <- d1[inRoute,]




out



