	
	
d0 <- d0[(d0$DISTRIBUTIONZIP == unique(d0$DISTRIBUTIONZIP[1])),]  
dim(d0)	

# _____________________________________________
dZub[ dZub$AKZ_HAUSKEY == max(table(dZub$AKZ_HAUSKEY)),]
dim(dZub)
colnames(dZub)
table(dZub$AKZ_POS,exclude=NULL)
dZub[dZub$AKZ_HAUSKEY %in% lNeigh,]
table(dZub$AKZ_HAUSKEY==lNeigh)
typeof(dZub$AKZ_HAUSKEY)
table(is.element(dZub$AKZ_HAUSKEY,lNeigh))
# _____________________________________________
# _____________________________________________
tour <- d1$TOUR[dRef$HOUSEKEY == hkList[i]][1]
distZip <- d1$DISTRIBUTIONZIP[dRef$HOUSEKEY == hkList[i]][1]

dOutHouse <- d1[((d1$DISTRIBUTIONZIP == distZip) & 
		(d1$HOUSEKEY != hkList[i]) &
		(d1$TOUR == tour)),]


# _____________________________________________
# _____________________________________________

dTourZub$AKZ_VALID_TO <- as.POSIXlt(strptime(dTourZub$AKZ_VALID_TO , "%d/%m/%Y"))
dTourZub <- dTourZub[is.na(dTourZub$AKZ_VALID_TO),]
dTourZub <- dTourZub[(dTourZub$AKZ_POS != posAct),]


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
            outLoopInHouse[index] <- median(dOutHouse$mfm[as.Date(dOutHouse$dateTime)==(refDate-id)])
        } 

    }
    out[i,] <- outLoopInHouse
    colnames(out) <- paste("in",namesWeekDays,sep="")

# _____________________________________________
    #  [1] "AKZ_DISTR_ZIP"    "AKZ_DISTR_OFFICE" "AKZ_DISTR_L"      "AKZ_DISTR_P"      "AKZ_TOUR_NR"      "AKZ_TOUR_LOG_KEY" "AKZ_ETP_NR"       "AKZ_ADRESS_ZIP"   "AKZ_HAUSKEY"      "AKZ_VALID_FROM"
    # [11] "AKZ_VALID_TO"     "AKZ_POS"          "AKZ_DISTR_TYPE"
    
    
    
    dZub <- read.csv2('zubofi_oktober2.csv')

            #Change NULL aof valid to to refDate
    #             dZub$ZUB_AKZ_CHANGE_TO <- as.character(dZub$ZUB_AKZ_CHANGE_TO)
    #             dZub$ZUB_AKZ_CHANGE_TO[dZub$ZUB_AKZ_CHANGE_TO=="NULL"] <- as.character(refDate)

            dZub <- dZub[as.Date(dZub$ZUB_AKZ_CHANGE_TO) > refDate-21,]
            selector <-  as.numeric(names(table(dZub$ZUB_AKZ_HAUSKEY)>1))
            dZub$ZUB_AKZ_CHANGE_TO <- as.Date(dZub$ZUB_AKZ_CHANGE_TO)
            dZub$ZUB_AKZ_DATE_CHANGE <- as.Date(dZub$ZUB_AKZ_DATE_CHANGE)

            for (ii in 1:length(selector)){
                dateMax <- max(dZub$ZUB_AKZ_CHANGE_TO[dZub$ZUB_AKZ_HAUSKEY==selector[ii]])
                dZub <- dZub[!(dZub$ZUB_AKZ_HAUSKEY==selector[ii] & dZub$ZUB_AKZ_CHANGE_TO != dateMax),]
            }
            #only events after last reorganisation
            d$PAD_EVENTDATE <- as.Date(d$PAD_EVENTDATE)
            d$selector <- rep(F,nrow(d))
            for (ii in 1:nrow(d)){
                if (sum(dZub$ZUB_AKZ_HAUSKEY==d$PAD_HOUSEKEY[ii]) > 0) {
                    d$selector[ii] <- d$PAD_EVENTDATE[ii]>dZub$ZUB_AKZ_DATE_CHANGE[dZub$ZUB_AKZ_HAUSKEY==d$PAD_HOUSEKEY[ii]]
                }
            }
            d <- d[d$selector,]

            outFrame <- as.data.frame(matrix(ncol=13,nrow=nrow(dZub)))#nrow(d_ref)))



