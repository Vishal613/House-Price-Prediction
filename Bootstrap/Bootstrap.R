library(boot)
library(tidyverse)

set.seed(1)
wd <- getwd()
setwd(wd)  
kcHousing <- read.csv("../Data/kc_final.csv")

lr.fit <- glm(price ~ . - id, data = kcHousing, family = 'binomial')
summary(lr.fit)
# 
# boot.fn <- function(data = Default, index) {
#   model <- glm(price ~ . - id, data = data[index,], family = 'binomial') 
#   coef(model)
# }
# 
# boot_res <- boot(data = Default, boot.fn, R = 250)
# boot_res
# 
# apply(boot_res$t, 2, function(x) quantile(x, c(0.025, 0.975))) %>% t()
# 
# confint(lr.fit)

