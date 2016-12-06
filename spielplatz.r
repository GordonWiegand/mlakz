
i <- 1
id <- 2

for (i in length(hkList)){ 
	dHouse <- d1[dRef$HOUSEKEY == hkList[i],]
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

}
colnames(out) <- paste("in",namesWeekDays,sep="")
rm(week)
rm(namesWeekDays)
