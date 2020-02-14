rm(list=ls())
tw50 <- read.csv("file:///Users/chengyahan/desktop/work/sunny/taiwan/tw50.new.csv", stringsAsFactors = F)

company.return <- data.frame(id = rownames(table(tw50$id)))
company.return$industry <- NA

for (i in 1:nrow(company.return)){
  index <- unique(which(tw50$id==company.return$id[i]))
  company.return$industry[i] <- tw50$industry[index]
}

for (i in 1991:2018){
  for (j in 1:12){
    if (i == 2018 & j > 10 ) {next}
    time.series <- if(j <10) {paste(i,"-0",j,sep="")} else{paste(i,"-",j,sep="")}
    company.return[,time.series] <- NA
  }
}

for(i in colnames(company.return)){
  ym <- as.numeric(paste(substring(i,1,4),substring(i,6,7),sep=""))
  for (j in 1:nrow(company.return)){
    index <- which(tw50$id == company.return$id[j] & tw50$ym == ym)
    if(length(index)==0){next}
    company.return[j,i] <- tw50$month.return[index]
  }
}

write.csv(company.return,"company.return.csv", row.names = F)
