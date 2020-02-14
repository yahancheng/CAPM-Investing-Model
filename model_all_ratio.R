# clean environment and read data
rm(list=ls())
tw50 <- read.csv("tw50.new.csv", stringsAsFactors = F)
alpha.d <- read.csv("all.alpha (ratio).csv", stringsAsFactors = FALSE, check.names = F)
beta.d <- read.csv("all.beta (ratio).csv", stringsAsFactors = FALSE, check.names = F)
company.return <- read.csv("company.return.csv", stringsAsFactors = FALSE, check.names = F)
d.0050 <- read.csv("0050.csv", stringsAsFactors = F)

library("matrixStats")

com.d <- company.return[c(-4:-50),c(-2,-3:-219, -336:-340)]
for (i in rownames(table(colnames(com.d)))){
  com.d[,i] <- NA
}

com.d$id[1] <- "company duplicated"
com.d$id[2] <- "company quantity"
com.d$id[3] <- "commission fee"


# functions
find.y <- function (index, year, month){
  y.vector <- tw50$cmpprm[which(tw50$id %in% index & tw50$year == year & tw50$month == month)]
  return(y.vector)
}


pick.cell <- function(matri){
  index.vector <- vector("numeric")
  for (i in 1:3){
    maximum = max(matri[i,])
    if (is.na(maximum) == TRUE) {next}
    index <- which(matri == maximum)
    index.vector <- append(index.vector, index)
  }
  for (j in 1:3){
    maximum = max(matri[,j])
    if (is.na(maximum) == TRUE) {next}
    index <- which(matri == maximum)
    index.vector <- append(index.vector, index)
  }
  index.vector <- unique(index.vector)
  return(index.vector)
}


y.vector <- list()
for (loop in 1:9){
  y.vector[[loop]] <- vector("numeric")
} 


# picked data
picked.data <- tw50
picked.data <- tw50[-c(1:nrow(tw50)),]
firm.vector.ex <- vector("numeric")


for (i in colnames(alpha.d[,2:117])){ 
  
  if (i == "2018-01") {break}
  # create empty matrice to store mean and sd of return
  nine.grid.mean <- matrix(NA, ncol = 3, nrow = 3)
  colnames(nine.grid.mean) <- c("b.low", "b.medium", "b.high")
  rownames(nine.grid.mean) <- c("a.low", "a.medium", "a.high")
  
  nine.grid.sd <- matrix(NA, ncol = 3, nrow = 3)
  colnames(nine.grid.sd) <- c("b.low", "b.medium", "b.high")
  rownames(nine.grid.sd) <- c("a.low", "a.medium", "a.high")
  
  a <- alpha.d[, i] 
  a <- na.omit(a)
  b <- beta.d[, i]
  b <- na.omit(b)
  
  a.3 <- quantile(a, 0.3)
  a.7 <- quantile(a, 0.7)
  b.3 <- quantile(b, 0.3)
  b.7 <- quantile(b, 0.7)
  
  
  
  index.list <- list()
  for (loop in 1:9){
    index.list[[loop]] <- vector("numeric")
  }
  
  # order: bycol = TRUE
  for (j in 1:nrow(alpha.d)){
    if (is.na(alpha.d[j, i]) == TRUE | is.na(beta.d[j,i] == TRUE)){
      next
    } else if (alpha.d[j, i] <= a.3 & beta.d[j,i] <= b.3){
      index.list[[1]] <- append(index.list[[1]], alpha.d$id[j])
    } else if (a.3 < alpha.d[j, i] & alpha.d[j, i] < a.7 & beta.d[j, i] <= b.3){
      index.list[[2]] <- append(index.list[[2]], alpha.d$id[j])
    } else if (alpha.d[j, i] >= a.7 & beta.d[j, i] <= b.3){
      index.list[[3]] <- append(index.list[[3]], alpha.d$id[j])
    } else if (alpha.d[j, i] <= a.3 & b.3 < beta.d[j, i] & beta.d[j, i] < b.7){
      index.list[[4]] <- append(index.list[[4]], alpha.d$id[j])
    } else if (a.3 < alpha.d[j, i] & alpha.d[j, i] < a.7 & b.3 < beta.d[j, i] & beta.d[j, i] < b.7){
      index.list[[5]] <- append(index.list[[5]], alpha.d$id[j])
    } else if (alpha.d[j, i] >= a.7 & b.3 < beta.d[j, i] & beta.d[j, i] < b.7){
      index.list[[6]] <- append(index.list[[6]], alpha.d$id[j])
    } else if (alpha.d[j, i] <= a.3 & beta.d[j, i] >= b.7){
      index.list[[7]] <- append(index.list[[7]], alpha.d$id[j])
    } else if (a.3 < alpha.d[j, i] & alpha.d[j, i] < a.7 & beta.d[j, i] >= b.7){
      index.list[[8]] <- append(index.list[[8]], alpha.d$id[j])
    } else if (alpha.d[j, i] >= a.7 & beta.d[j, i] >= b.7){
      index.list[[9]] <- append(index.list[[9]], alpha.d$id[j])
    }
  }
  
  year = as.numeric(substring(i, 1, 4))
  month = as.numeric(substring(i, 6, 7))
  for (loop in 1:9){
    y.vector[[loop]] <- append(y.vector[[loop]], find.y(index.list[[loop]], year, month))
    nine.grid.mean[loop] <- mean(y.vector[[loop]], na.rm = TRUE)
    nine.grid.sd[loop] <- sd(y.vector[[loop]], na.rm = TRUE)
  }
  
  sharpe.ratio.matrix <- nine.grid.mean / nine.grid.sd
  mean.sd.matrix <- nine.grid.mean - nine.grid.sd
  
  # pick cell by sharpe ratio and mean - sd
  sharpe.cell <- pick.cell(sharpe.ratio.matrix)
  mean.sd.cell <- pick.cell(mean.sd.matrix)
  
  intersec.cell <- intersect(sharpe.cell, mean.sd.cell)
  
  # firm picked
  firm.vector <- vector("numeric")
  
  ## Calculate the sharpe ratio and mean - sd to pick top 10% companies
  company.return$y.mean <- rowMeans(company.return[, 3 : which(colnames(company.return) == i)], na.rm = TRUE)
  company.return$y.sd <- rowSds(as.matrix(company.return[, 3 : which(colnames(company.return) == i)]), na.rm = TRUE)
  
  company.return$sharp.ratio <- company.return$y.mean / company.return$y.sd
  company.return$mean.minus.sd <- company.return$y.mean - company.return$y.sd
  
  for (j in intersec.cell){
    cell.d <- subset(company.return, id %in% index.list[[j]]
                     , select = c(id, sharp.ratio, mean.minus.sd))
    cell.d <- na.omit(cell.d)
    
    mm <- mean(cell.d$mean.minus.sd, na.rm = TRUE)
    ms <- sd(cell.d$mean.minus.sd, na.rm = TRUE)
    cell.d$z.mean.minus.sd <- (cell.d$mean.minus.sd - mm) / ms
    
    sm <- mean(cell.d$sharp.ratio, na.rm = TRUE)
    ss <- sd(cell.d$sharp.ratio, na.rm = TRUE)
    cell.d$z.sharp.ratio <- (cell.d$sharp.ratio - sm) / ss
    
    cell.d$score <- cell.d$z.mean.minus.sd * 0.5 + cell.d$z.sharp.ratio * 0.5
    
    ## pick top 10% companies or pick the first companies in each picked cell
    top10 <- quantile(cell.d$score, 0.9, na.rm = TRUE)
    check = 0
    for (row in 1:nrow(cell.d)){
      if (nrow(cell.d)==0){next}
      if (is.na(top10) != T & cell.d$score[row] >= top10){
        firm.vector <- append(firm.vector, cell.d$id[row])
        check = 1
      }
    }
    if (nrow(cell.d)==1){
      firm.vector <- append(firm.vector, cell.d$id[1])
      check = 1
    }
    if (check == 0){ 
      index = which(cell.d$score == max(cell.d$score))
      firm.vector <- append(firm.vector, cell.d$id[index])
    }
  }
  
  if (month < 12) {
    month <- month + 1
  } else if (month==12){
    year <- year + 1
    month <- 1
  }
  
  for (j in firm.vector){
    index <- which(tw50$id == j & tw50$year == year & tw50$month == month)
    picked.data <- rbind(picked.data, tw50[index,])
  }
  
  # calculate firm which was also in previous period
  cmp <- 0
  for (j in firm.vector){
    if (j %in% firm.vector.ex){
      cmp <- cmp +1
    }
  }
  
  time.series <- if (month<10) {paste(year,month,sep="-0")} else if (month>9) {paste(year,month,sep="-")}
  
  com.d[1,time.series] <- cmp
  com.d[2,time.series] <- length(firm.vector)
  cmp <- 0
  
  #store firm list
  firm.vector.ex <- firm.vector
  
}


picked.data$ym <- as.numeric(substring(picked.data$date, 1, 6))


## portfolio return without commission fee
# form 2009-02 - 2018-09
plot.index <- vector("numeric")
plot.mean <- vector("numeric")
for (i in rownames(table(picked.data$ym))){
  tempt <- subset(picked.data, ym == i)
  verify.mean <- mean(tempt$month.return)
  plot.index <- append(plot.index, i)
  plot.mean <- append(plot.mean, verify.mean)
}


fund <- 1000000
for (i in plot.mean){
  fund <- fund*(i/100 +1)
}

p.fund1 <- fund
p1.ear <- ((p.fund1/1000000)^(12/116)-1)*100
p1.sd <- sd(plot.mean) * (12**(0.5))


# index return
fund <- 1000000

for (i in 116:1){
  fund <- fund*(d.0050$return[i]/100 +1)
}

i.fund <- fund
i.ear <- ((i.fund/1000000)^(12/116)-1)*100
i.sd <- sd(d.0050$return[1:116]) * (12**(0.5))



## portfolio return with commission fee
fund <- 1000000

aux.sell <- 0
aux.buy <- 0
q.company.last <- 0


for (i in colnames(company.return)[220:335]){
  commission <- 0
  
  # no duplicate firm
  if (com.d[1,i]==0){ 
    aux.sell <- fund*1.45*(1/1000) # commission of selling stocks
    aux.buy <- fund*1.45*(1/1000) # commission of buying stocks
    if (aux.sell < 20){aux.sell = 20} # the minimun of commission fee is NT$20
    if (aux.buy < 20) {aux.buy = 20}
    
    commission <- commission + aux.sell + aux.buy
  } 
  if (com.d[1,i]==com.d[2,i] & com.d[1,i] != 0 ){
    commission <- commission #portfolio remains unchanged
  } 
  if (com.d[1,i]!=0 & com.d[1,i] != com.d[2,i]){ # change in portfolio
    
    # no change in company quantity
    if (com.d[2,i]==q.company.last){
      # sell and buy different firms
      q.new.firm <- com.d[2,i] - com.d[1,i] # quantity of new firms
      aux.sell <- (q.new.firm/com.d[2,i])*fund*1.45*(1/1000)
      aux.buy <- (q.new.firm/com.d[2,i])*fund*1.45*(1/1000)
      if(aux.sell < 20){aux.sell = 20}
      if(aux.buy < 20){aux.buy = 20}
      commission <- commission + aux.sell + aux.buy
    }
    
    
    # change in company quantity
    if (com.d[2,i] < q.company.last){ # increase in percentage of every portion
      aux.sell <- ((q.company.last-com.d[1,i])*(1/q.company.last))*fund*1.45*(1/1000)
      aux.buy <- (com.d[1,i]*(1/com.d[2,i] - 1/q.company.last) + (com.d[2,i] - com.d[1,i])*(1/com.d[2,i]))*fund*1.45*(1/1000)        
      if(aux.sell < 20){aux.sell = 20}
      if(aux.buy < 20){aux.buy = 20}
      commission <- commission + aux.sell + aux.buy
    }
    
    if (com.d[2,i] > q.company.last){ # decrease in percentage of every portion
      aux.sell <- (com.d[1,i]*(1/q.company.last - 1/com.d[2,i]) + (q.company.last-com.d[1,i])*(1/com.d[2,i]))*fund*1.45*(1/1000)
      aux.buy <- ((com.d[2,i]-com.d[1,i])*(1/com.d[2,i]))*fund*1.45*(1/1000)
      if(aux.sell < 20){aux.sell = 20}
      if(aux.buy < 20){aux.buy = 20}
      commission <- commission + aux.sell + aux.buy
    }
  }
  
  q.company.last <- com.d[2,i] # the number of firm last month
  date <- paste(substring(i,1,4),substring(i,6,7),sep="")
  index <- which(plot.index==date)
  if (length(index)==0){next}
  return.rate <- plot.mean[index] # find corresponding return rate
  fund <- fund*(return.rate/100 +1) - commission
  com.d[3,i] <- commission
  
}

# calculate EAR (with commision fee)
fund <- 1e6
commission.sum <- rowSums(com.d[3,-1], na.rm = T)
i = 100
for (i in plot.mean){
  fund <- fund * (1 + i/100)
}
p.fund2 <- fund - commission.sum
p2.ear <- ((p.fund2/1000000)^(12/116)-1)*100




# winning percentage
win <- 0
count <- 0
d.0050$ym <- as.numeric(substring(d.0050$date,1,6))


for (i in colnames(company.return)[220:335]){
  year.month <- paste(substring(i,1,4),substring(i,6,7),sep="")
  index1 <- which(plot.index==year.month)
  portfolio.return <- plot.mean[index1]
  index2 <- which(d.0050$ym==as.numeric(year.month))
  d.0050.return <- d.0050$return[index2]
  if (length(portfolio.return)==0 | length(d.0050.return)==0){next}
  if (portfolio.return > d.0050.return){
    win = win +1
  }
  count = count +1
}

win.percentage <- win/count






print(paste("p1.ear:", p1.ear, sep=" "))
print(paste("p1.sd:", p1.sd, sep=" "))
print(paste("p1.ear-p1.sd:", p1.ear-p1.sd, sep=" "))
print(paste("p1.ear+p1.sd:", p1.ear+p1.sd, sep=" "))
print(paste("i.ear:", i.ear, sep=" "))
print(paste("i.sd:", i.sd, sep=" "))
print(paste("p2.ear:", p2.ear, sep=" "))
print(paste("winning percentage:",win.percentage, sep=" " ))
