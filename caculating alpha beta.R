
tw50 <- read.csv("tw50.new.csv", stringsAsFactors = FALSE)


#create new data frame
alpha.d <- data.frame("id" = rownames(table(tw50$id)))
beta.d <- data.frame("id" = rownames(table(tw50$id)))


for (i in 1992:2018){
  for (j in 1:12){
    if (i == 2018 & j > 10 ){next}
    time.series <- if (j < 10){paste(i, "-0", j, sep = "")} else {paste(i, "-", j, sep = "")}
    alpha.d[,time.series] <- NA
    beta.d[,time.series] <- NA
  }
}


for (i in 1992:2018){
  for (j in 1:12){
    if (i == 2018 & j > 10){next}
    for (row in 1:nrow(alpha.d)){
      train.d <- subset(tw50, id == alpha.d$id[row] & (year < i | (year == i & month < j)), select = c(cmpprm,mktprm))
      if(nrow(train.d)==0){next}
      
      for (col in 1:2){
        train.d[,col] <- scale(train.d[,col])
        na.index <- which( is.na(train.d[,col]) == T )
        train.d[na.index, col] <- mean(train.d[, col], na.rm = T)
      }
      
      linear.regression <- tryCatch({  # try catch error
        lm(cmpprm ~ mktprm , data = train.d)
      } , error = function(cond) "skip")
      if(linear.regression == "skip"){next}
      
      a = linear.regression$coefficients[1]
      b = linear.regression$coefficients[2]
      time.series <- if (j < 10){paste(i, "-0", j, sep = "")} else {paste(i, "-", j, sep = "")}
      alpha.d[row, time.series] <- a
      beta.d[row, time.series] <- b
    }
  }
}



write.csv(alpha.d, "alpha.d(scale).csv", row.names = F)
write.csv(beta.d, "beta.d(scale).csv", row.names = F)
