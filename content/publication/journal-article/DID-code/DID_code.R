library(dplyr)
library(splines)
library(mgcv)
library(gnm)

mydata <- read.csv("example.csv")

for (i in c(8,27,36,53)){
        model <- gnm(mydata[, i] ~ bs(pm2_5) + year + bs(win_temp_mean,4) + bs(sum_temp_mean,4) +win_temp_sd + sum_temp_sd + SEIFA,
                     data=mydata, family=poisson, offset = log(population),
                     eliminate=factor(postcode),na.action = "na.exclude")
        result <- summary(model)
        #print(result)
        print(names(mydata[i]))
        print(paste("pm2.5=", result$coefficients[1,1]))
        print(paste("95%CI=",result$coefficients[1,1] + c(-1, 1) * qt(.975, df = model$df) * result$coefficients[1, 2]))
        print(paste("p =",result$coefficients[1,4]))
        ifelse(as.numeric(result$coefficients[1,4])<=0.05,print("*****"),print("---"))
}