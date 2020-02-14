library(quantmod)


# Get treasury data
for (i in c("DGS3MO", "DGS30")){
  tempt <- tryCatch({
    getSymbols(i, src = "FRED", auto.assign = FALSE)
    }, error = function(cond) "skip")
  if (tempt == "skip") {next}
  ?getSymbols
  colnames(tempt) <- "monthly.returns"
  
  tempt <- data.frame(tempt)
  
  tempt <- na.omit(tempt)
  
  for (j in 1:nrow(tempt)){
    tempt$year[j] <- as.numeric(substring(rownames(tempt)[j], 1, 4))
    tempt$month[j] <- as.numeric(substring(rownames(tempt)[j], 6, 7))
  }
  
  return.rate <- matrix(NA, ncol = 4, nrow = length(table(tempt$year)) * length(table(tempt$month)))
  colnames(return.rate) <- c("year", "month", "monthly.returns", "treasury")
  
  
  loop = 1
  
  for (k in rownames(table(tempt$year))){
    for (j in rownames(table(tempt$month))){
    index <- which(tempt$year == k & tempt$month == j)
    return.rate[loop, "monthly.returns"] <- mean(tempt$monthly.returns[index])
    return.rate[loop, "year"] <- k
    return.rate[loop, "month"] <- j
    return.rate[loop, "treasury"] <- i
    loop = loop + 1
  }
}
  
  return.rate <- data.frame(return.rate)
  if (i == "DGS3MO"){stock.return <- return.rate} else {stock.return <- rbind(stock.return, return.rate)}
}

stock.return$monthly.returns <- as.numeric(as.character(stock.return$monthly.returns)) / 100



# put treasury into tw50
tw50 <- read.csv("tw50.new.csv", stringsAsFactors = F)
tw50$treasury <- NA
i = 1
for(i in 1:nrow(tw50)){
  index <- which(stock.return$year == tw50$year[i] & stock.return$month == tw50$month[i] & 
                 stock.return$treasury == "DGS3MO")
  tw50$treasury[i] <- ((stock.return$monthly.returns[index] + 1) ^ (1/12) - 1) * 100
}

tw50$mktprm <- tw50$mkt - tw50$treasury
tw50$cmpprm <- tw50$month.return - tw50$treasury

tw50$long.treasury <- NA

for(i in 1:nrow(tw50)){
  index <- which(stock.return$year == tw50$year[i] & stock.return$month == tw50$month[i] & 
                   stock.return$treasury == "DGS30")
  if(length(index) == 0) {next}
  tw50$long.treasury[i] <- ((stock.return$monthly.returns[index] + 1) ^ (1/12) - 1) * 100
}

tw50$long.minus.short <- tw50$long.treasury - tw50$treasury

write.csv(tw50, "tw50.new.csv", row.names = F)


