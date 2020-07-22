####========
# Conditions for using the code
#
# If you publish any work using or referring the code,
# please cite the publication (Yu et al., 2020) in the following recommended format:
#
# Yu, Wenhua, Yuming Guo, Liuhua Shi, and Shanshan Li. "The association
# between long-term exposure to low-level PM2. 5 and mortality in the
# state of Queensland, Australia: A modelling study with the difference-in-differences
# approach." PLoS medicine 17, no. 6 (2020): e1003141.
#========###

library(dplyr)
library(splines)
library(mgcv)
library(gnm)
#load the example data
mydata <- read.csv("example.csv")
# model establishment
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
