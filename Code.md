```r
library(rio)
library(dplyr)
library(MASS)
url <- "https://datadryad.org/stash/downloads/file_stream/30857"
raw.data <- import(url, format = "xls") %>% as.data.frame()
strata <- "country"
data.list <- split(raw.data, f = as.factor(raw.data[, strata]))
predictors <- c("resp_rate", "SpO2", "BPS", "HR", "temp", "age")
outcome <- "ICU"
simulate_data <- function(dataset, outcome, predictors, size = 10000) {
    y <- dataset[, outcome]
    x <- dataset[predictors]
    fit <- glm(y ~ ., family = binomial, data = cbind(y, x))
    cov.matrix <- cov(x)
    sim.data <- mvrnorm(size, sapply(x, mean), cov.matrix, empirical = TRUE) %>% as.data.frame()
    sim.data$yhat <- predict(fit, newdata = sim.data, type = "response")
    sim.data$y <- rbinom(nrow(sim.data), 1, prob = sim.data$yhat)
    sim.data
}
simulated.data.list <- lapply(data.list, simulate_data, outcome = outcome, predictors = predictors)
simulated.data.list <- lapply(names(data.list), function(name) {
    dataset <- simulated.data.list[[name]]
    dataset$strata <- name
    dataset
})
simulated.data <- do.call(rbind, simulated.data.list) %>% as.data.frame()
```
#kod skriven i R

```{r, echo = FALSE}
knitr::opts_chunk$set(comment = NA)
##install.packages('readxl')
#install.packages('httr')
library(readxl)
library(httr)
#packageVersion('readxl')

GET("https://datadryad.org/stash/downloads/file_stream/30857", write_disk(tf <- tempfile(fileext = '.xls')))
df <- read_excel(tf, 1L)


#install.packages('tableone')
library(tableone)
#install.packages('survival')
library(survival)

colnames(df)[1] <- "Hospital"
colnames(df)[2] <- "Country"
colnames(df)[3] <- "Respiratory rate (per min)"
colnames(df)[4] <- "Confusion"
colnames(df)[5] <- "Gender"
colnames(df)[7] <- "Peripheral oxygen saturation (%)"
colnames(df)[8] <- "Systolic blood pressure (mm Hg)"
colnames(df)[10] <- "Pulse (bpm)"
colnames(df)[11] <- "Temperature (°C)"
colnames(df)[16] <- "Age"
colnames(df)[17] <- "ICU admission"

#dput(names(df))

df <- df[ -c(1, 6, 9, 12, 13, 14, 15)]

#install.packages('dplyr')
library(dplyr)
df$Gender[df$Gender == 'm'] <- 'Male'
df$Gender[df$Gender == 'm'] <- 'Female'

df$Confusion[df$Confusion == 1] <- 'Yes'
df$Confusion[df$Confusion == 0] <- 'No'

df$`ICU admission`[df$`ICU admission` == 1] <- 'Yes'
df$`ICU admission`[df$`ICU admission` == 0] <- 'No'

catVars <- c("Confusion", "ICU admission")
biomarkers <- c("Respiratory rate (per min)", "Peripheral oxygen saturation (%)", "")
#summary(Pctable) to get skewed variables
#dput(names(df))

Pctable <- CreateTableOne(data = df, factorVars = catVars)
#dput(names(df))

df$Gender[df$Gender == 'm'] <- 'Male'
df$Gender[df$Gender == 'f'] <- 'Female'

df$Confusion[df$Confusion == 'Yes'] <- 1
df$Confusion[df$Confusion == 'No'] <- 0

df$`ICU admission`[df$`ICU admission` == 'Yes'] <- 1
df$`ICU admission`[df$`ICU admission` == 'No'] <- 0

df$Confusion <- as.numeric(df$Confusion)
df$`ICU admission` <- as.numeric(df$`ICU admission`)

df_USA <- df[1:940, ]
df_France <- df[941:1295, ]
df_Swizerland <- df[1296:1303, ]

#---------------------------------------------
pe_dev <- c()
pe_tval <- c()
pe_pval <- c()
pe_tval_dev <- c()
pe_pval_dev <- c()
pe_diff_diff <- c()
#---------------------------------------------
#1
devsample <- df_USA
valsample <- df_France

logreg <- glm(`ICU admission` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = devsample, family = binomial)
probabilities <- logreg %>% predict(devsample, type = "response")
predict.classesdev <- ifelse(probabilities > 0.5, 1, 0)

x <- mean(predict.classesdev == devsample$`ICU admission`) * 100

probabilities <- logreg %>% predict(valsample, type = "response")
predict.classesval <- ifelse(probabilities > 0.5, 1, 0)

y <- mean(predict.classesval == valsample$`ICU admission`) * 100

devsample['devval'] <- 1
valsample['devval'] <- 0

df_pooled <- rbind(devsample, valsample)

logregi <- glm(`devval` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = df_pooled, family = binomial)
probabilities <- logregi %>% predict(df_pooled, type = "response")
predict.classespool <- ifelse(probabilities > 0.5, 1, 0)
tf <- predict.classespool == df_pooled$`devval`

missmatch = c()
listn <- c(1:nrow(devsample))

for (i in listn) {
  if (tf[i] == TRUE) {
    missmatch <- c(missmatch, i)
  }
}

df_segment <-devsample[missmatch, ]

probabilities <- logreg %>% predict(df_segment, type = "response")
predict.classessegment <- ifelse(probabilities > 0.5, 1, 0)

z <- mean(predict.classessegment == df_segment$`ICU admission`) * 100

pe_dev <- c(pe_dev, x)
pe_tval <- c(pe_tval, y)
pe_pval <- c(pe_pval, z)
pe_tval_dev <- c(pe_tval_dev, (y - x))
pe_pval_dev <- c(pe_pval_dev, (z - x))
pe_diff_diff <- c(pe_diff_diff, ((z - x) - (y - x)))
#---------------------------------------------
#2
devsample <- df_USA
valsample <- df_Swizerland

logreg <- glm(`ICU admission` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = devsample, family = binomial)
probabilities <- logreg %>% predict(devsample, type = "response")
predict.classesdev <- ifelse(probabilities > 0.5, 1, 0)

x <- mean(predict.classesdev == devsample$`ICU admission`) * 100

probabilities <- logreg %>% predict(valsample, type = "response")
predict.classesval <- ifelse(probabilities > 0.5, 1, 0)

y <- mean(predict.classesval == valsample$`ICU admission`) * 100

devsample['devval'] <- 1
valsample['devval'] <- 0

df_pooled <- rbind(devsample, valsample)

logregi <- glm(`devval` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = df_pooled, family = binomial)
probabilities <- logregi %>% predict(df_pooled, type = "response")
predict.classespool <- ifelse(probabilities > 0.5, 1, 0)
tf <- predict.classespool == df_pooled$`devval`

missmatch = c()
listn <- c(1:nrow(devsample))

for (i in listn) {
  if (tf[i] == TRUE) {
    missmatch <- c(missmatch, i)
  }
}

df_segment <-devsample[missmatch, ]

probabilities <- logreg %>% predict(df_segment, type = "response")
predict.classessegment <- ifelse(probabilities > 0.5, 1, 0)

z <- mean(predict.classessegment == df_segment$`ICU admission`) * 100

pe_dev <- c(pe_dev, x)
pe_tval <- c(pe_tval, y)
pe_pval <- c(pe_pval, z)
pe_tval_dev <- c(pe_tval_dev, (y - x))
pe_pval_dev <- c(pe_pval_dev, (z - x))
pe_diff_diff <- c(pe_diff_diff, ((z - x) - (y - x)))
#---------------------------------------------
#3
devsample <- df_France
valsample <- df_USA

logreg <- glm(`ICU admission` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = devsample, family = binomial)
probabilities <- logreg %>% predict(devsample, type = "response")
predict.classesdev <- ifelse(probabilities > 0.5, 1, 0)

x <- mean(predict.classesdev == devsample$`ICU admission`) * 100

probabilities <- logreg %>% predict(valsample, type = "response")
predict.classesval <- ifelse(probabilities > 0.5, 1, 0)

y <- mean(predict.classesval == valsample$`ICU admission`) * 100

devsample['devval'] <- 1
valsample['devval'] <- 0

df_pooled <- rbind(devsample, valsample)

logregi <- glm(`devval` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = df_pooled, family = binomial)
probabilities <- logregi %>% predict(df_pooled, type = "response")
predict.classespool <- ifelse(probabilities > 0.5, 1, 0)
tf <- predict.classespool == df_pooled$`devval`

missmatch = c()
listn <- c(1:nrow(devsample))

for (i in listn) {
  if (tf[i] == TRUE) {
    missmatch <- c(missmatch, i)
  }
}

df_segment <-devsample[missmatch, ]

probabilities <- logreg %>% predict(df_segment, type = "response")
predict.classessegment <- ifelse(probabilities > 0.5, 1, 0)

z <- mean(predict.classessegment == df_segment$`ICU admission`) * 100

pe_dev <- c(pe_dev, x)
pe_tval <- c(pe_tval, y)
pe_pval <- c(pe_pval, z)
pe_tval_dev <- c(pe_tval_dev, (y - x))
pe_pval_dev <- c(pe_pval_dev, (z - x))
pe_diff_diff <- c(pe_diff_diff, ((z - x) - (y - x)))
#---------------------------------------------
#4
devsample <- df_France
valsample <- df_Swizerland

logreg <- glm(`ICU admission` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = devsample, family = binomial)
probabilities <- logreg %>% predict(devsample, type = "response")
predict.classesdev <- ifelse(probabilities > 0.5, 1, 0)

x <- mean(predict.classesdev == devsample$`ICU admission`) * 100

probabilities <- logreg %>% predict(valsample, type = "response")
predict.classesval <- ifelse(probabilities > 0.5, 1, 0)

y <- mean(predict.classesval == valsample$`ICU admission`) * 100

devsample['devval'] <- 1
valsample['devval'] <- 0

df_pooled <- rbind(devsample, valsample)

logregi <- glm(`devval` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = df_pooled, family = binomial)
probabilities <- logregi %>% predict(df_pooled, type = "response")
predict.classespool <- ifelse(probabilities > 0.5, 1, 0)
tf <- predict.classespool == df_pooled$`devval`

missmatch = c()
listn <- c(1:nrow(devsample))

for (i in listn) {
  if (tf[i] == TRUE) {
    missmatch <- c(missmatch, i)
  }
}

df_segment <-devsample[missmatch, ]

probabilities <- logreg %>% predict(df_segment, type = "response")
predict.classessegment <- ifelse(probabilities > 0.5, 1, 0)

z <- mean(predict.classessegment == df_segment$`ICU admission`) * 100

pe_dev <- c(pe_dev, x)
pe_tval <- c(pe_tval, y)
pe_pval <- c(pe_pval, z)
pe_tval_dev <- c(pe_tval_dev, (y - x))
pe_pval_dev <- c(pe_pval_dev, (z - x))
pe_diff_diff <- c(pe_diff_diff, ((z - x) - (y - x)))
#---------------------------------------------
#5
#devsample <- df_Swizerland
#valsample <- df_USA

#logreg <- glm(`ICU admission` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = devsample, family = binomial)
#probabilities <- logreg %>% predict(devsample, type = "response")
#predict.classesdev <- ifelse(probabilities > 0.5, 1, 0)

#x <- mean(predict.classesdev == devsample$`ICU admission`) * 100

#probabilities <- logreg %>% predict(valsample, type = "response")
#predict.classesval <- ifelse(probabilities > 0.5, 1, 0)

#y <- mean(predict.classesval == valsample$`ICU admission`) * 100

#devsample['devval'] <- 1
#valsample['devval'] <- 0

#df_pooled <- rbind(devsample, valsample)

#logregi <- glm(`devval` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = df_pooled, family = binomial)
#probabilities <- logregi %>% predict(df_pooled, type = "response")
#predict.classespool <- ifelse(probabilities > 0.5, 1, 0)
#tf <- predict.classespool == df_pooled$`devval`

#missmatch = c()
#listn <- c(1:nrow(devsample))

#for (i in listn) {
#  if (tf[i] == TRUE) {
#    missmatch <- c(missmatch, i)
#  }
#}

#df_segment <-devsample[missmatch, ]

#probabilities <- logreg %>% predict(df_segment, type = "response")
#predict.classessegment <- ifelse(probabilities > 0.5, 1, 0)

#z <- mean(predict.classessegment == df_segment$`ICU admission`) * 100

#pe_dev <- c(pe_dev, x)
#pe_tval <- c(pe_tval, y)
#pe_pval <- c(pe_pval, z)
#pe_tval_dev <- c(pe_tval_dev, (y - x))
#pe_pval_dev <- c(pe_pval_dev, (z - x))
#pe_diff_diff <- c(pe_diff_diff, ((z - x) - (y - x)))
#---------------------------------------------
#6
devsample <- df_Swizerland
valsample <- df_France

logreg <- glm(`ICU admission` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = devsample, family = binomial)
probabilities <- logreg %>% predict(devsample, type = "response")
predict.classesdev <- ifelse(probabilities > 0.5, 1, 0)

x <- mean(predict.classesdev == devsample$`ICU admission`) * 100

probabilities <- logreg %>% predict(valsample, type = "response")
predict.classesval <- ifelse(probabilities > 0.5, 1, 0)

y <- mean(predict.classesval == valsample$`ICU admission`) * 100

devsample['devval'] <- 1
valsample['devval'] <- 0

df_pooled <- rbind(devsample, valsample)

logregi <- glm(`devval` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = df_pooled, family = binomial)
probabilities <- logregi %>% predict(df_pooled, type = "response")
predict.classespool <- ifelse(probabilities > 0.5, 1, 0)
tf <- predict.classespool == df_pooled$`devval`

missmatch = c()
listn <- c(1:nrow(devsample))

for (i in listn) {
  if (tf[i] == TRUE) {
    missmatch <- c(missmatch, i)
  }
}

df_segment <-devsample[missmatch, ]

probabilities <- logreg %>% predict(df_segment, type = "response")
predict.classessegment <- ifelse(probabilities > 0.5, 1, 0)

z <- mean(predict.classessegment == df_segment$`ICU admission`) * 100

pe_dev <- c(pe_dev, x)
pe_tval <- c(pe_tval, y)
pe_pval <- c(pe_pval, z)
pe_tval_dev <- c(pe_tval_dev, (y - x))
pe_pval_dev <- c(pe_pval_dev, (z - x))
pe_diff_diff <- c(pe_diff_diff, ((z - x) - (y - x)))
#---------------------------------------------
dev <- c()
tval <- c()
pval <- c()
tval_dev <- c()
pval_dev <- c()
diff_diff <- c()
#---------------------------------------------
bootnumber <- 1000
z = 1
while (z <= bootnumber) {
  devbootstrap <- sample_n(df, 1303, replace = TRUE)
  valbootstrap <- sample_n(df, 1303, replace = TRUE)

  logreg <- glm(`ICU admission` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = 
                  devbootstrap, family = binomial)
  probabilities <- logreg %>% predict(devbootstrap, type = "response")
  predict.classes <- ifelse(probabilities > 0.5, 1, 0)

  dev <- c(dev, (mean(predict.classes == devbootstrap$`ICU admission`) * 100))

  probabilities <- logreg %>% predict(valbootstrap, type = "response")
  predict.classes <- ifelse(probabilities > 0.5, 1, 0)

  tval <- c(tval, (mean(predict.classes == valbootstrap$`ICU admission`) * 100))

  devbootstrap['devval'] <- 1
  valbootstrap['devval'] <- 0

  df_pooledboot <- rbind(devbootstrap, valbootstrap)

  logregi <- glm(`devval` ~ `Confusion` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` , data = 
                   df_pooledboot, family = binomial)
  probabilities <- logregi %>% predict(df_pooledboot, type = "response")
  predict.classes <- ifelse(probabilities > 0.5, 1, 0)
  tfboot <- predict.classes == df_pooledboot$`devval`

  missmatch = c()
  listn <- c(1:nrow(devbootstrap))

  for (i in listn) {
    if (tfboot[i] == TRUE) {
      missmatch <- c(missmatch, i)
    }
  }

  df_segmentboot <-devbootstrap[missmatch, ]

  probabilities <- logreg %>% predict(df_segmentboot, type = "response")
  predict.classes <- ifelse(probabilities > 0.5, 1, 0)

  pval <- c(pval, (mean(predict.classes == df_segmentboot$`ICU admission`) * 100))
  
  z <- z + 1

}

k <- 1
while (k <= bootnumber) {
  tval_dev <- c(tval_dev, (tval[k] - dev[k]))
  pval_dev <- c(pval_dev, (pval[k] - dev[k]))
  diff_diff <- c(diff_diff, (pval_dev[k] - tval_dev[k]))
  k = k + 1
}
#---------------------------------------------
dev1 <- c()
dev2 <- c()
dev3 <- c()
dev4 <- c()
dev5 <- c()
#dev6 <- c()
tval1 <- c()
tval2 <- c()
tval3 <- c()
tval4 <- c()
tval5 <- c()
#tval6 <- c()
pval1 <- c()
pval2 <- c()
pval3 <- c()
pval4 <- c()
pval5 <- c()
#pval6 <- c()
tval_dev1 <- c()
tval_dev2 <- c()
tval_dev3 <- c()
tval_dev4 <- c()
tval_dev5 <- c()
#tval_dev6 <- c()
pval_dev1 <- c()
pval_dev2 <- c()
pval_dev3 <- c()
pval_dev4 <- c()
pval_dev5 <- c()
#pval_dev6 <- c()
diff_diff1 <- c()
diff_diff2 <- c()
diff_diff3 <- c()
diff_diff4 <- c()
diff_diff5 <- c()
#diff_diff6 <- c()


k <- 1
while (k <= bootnumber) {
  dev1 <- c(dev1, (dev[k] - pe_dev[1]))
  dev2 <- c(dev2, (dev[k] - pe_dev[2]))
  dev3 <- c(dev3, (dev[k] - pe_dev[3]))
  dev4 <- c(dev4, (dev[k] - pe_dev[4]))
  dev5 <- c(dev5, (dev[k] - pe_dev[5]))
  #dev6 <- c(dev6, (dev[k] - pe_dev[1]))
  tval1 <- c(tval1, (tval[k] - pe_tval[1]))
  tval2 <- c(tval2, (tval[k] - pe_tval[2]))
  tval3 <- c(tval3, (tval[k] - pe_tval[3]))
  tval4 <- c(tval4, (tval[k] - pe_tval[4]))
  tval5 <- c(tval5, (tval[k] - pe_tval[5]))
  #tval6 <- c(tval6, (tval[k] - pe_tval[6]))
  pval1 <- c(pval1, (pval[k] - pe_pval[1]))
  pval2 <- c(pval2, (pval[k] - pe_pval[2]))
  pval3 <- c(pval3, (pval[k] - pe_pval[3]))
  pval4 <- c(pval4, (pval[k] - pe_pval[4]))
  pval5 <- c(pval5, (pval[k] - pe_pval[5]))
  #pval6 <- c(pval6, (pval[k] - pe_pval[6]))
  tval_dev1 <- c(tval_dev1, (tval_dev[k] - pe_tval_dev[1]))
  tval_dev2 <- c(tval_dev2, (tval_dev[k] - pe_tval_dev[2]))
  tval_dev3 <- c(tval_dev3, (tval_dev[k] - pe_tval_dev[3]))
  tval_dev4 <- c(tval_dev4, (tval_dev[k] - pe_tval_dev[4]))
  tval_dev5 <- c(tval_dev5, (tval_dev[k] - pe_tval_dev[5]))
  #tval_dev6 <- c(tval_dev6, (tval_dev[k] - pe_tval_dev[1]))
  pval_dev1 <- c(pval_dev1, (pval_dev[k] - pe_pval_dev[1]))
  pval_dev2 <- c(pval_dev2, (pval_dev[k] - pe_pval_dev[2]))
  pval_dev3 <- c(pval_dev3, (pval_dev[k] - pe_pval_dev[3]))
  pval_dev4 <- c(pval_dev4, (pval_dev[k] - pe_pval_dev[4]))
  pval_dev5 <- c(pval_dev5, (pval_dev[k] - pe_pval_dev[5]))
  #pval_dev6 <- c(pval_dev6, (pval_dev[k] - pe_pval_dev[6]))
  diff_diff1 <- c(diff_diff1, (diff_diff[k] - pe_diff_diff[1]))
  diff_diff2 <- c(diff_diff2, (diff_diff[k] - pe_diff_diff[2]))
  diff_diff3 <- c(diff_diff3, (diff_diff[k] - pe_diff_diff[3]))
  diff_diff4 <- c(diff_diff4, (diff_diff[k] - pe_diff_diff[4]))
  diff_diff5 <- c(diff_diff5, (diff_diff[k] - pe_diff_diff[5]))
  #diff_diff6 <- c(diff_diff6, (diff_diff[k] - pe_diff_diff[6]))
  k = k + 1
}

dev1 <- sort(dev1, decreasing = FALSE)
dev2 <- sort(dev2, decreasing = FALSE)
dev3 <- sort(dev3, decreasing = FALSE)
dev4 <- sort(dev4, decreasing = FALSE)
dev5 <- sort(dev5, decreasing = FALSE)
#dev6 <- sort(dev6, decreasing = FALSE)
tval1 <- sort(tval1, decreasing = FALSE)
tval2 <- sort(tval2, decreasing = FALSE)
tval3 <- sort(tval3, decreasing = FALSE)
tval4 <- sort(tval4, decreasing = FALSE)
tval5 <- sort(tval5, decreasing = FALSE)
#tval6 <- sort(tval6, decreasing = FALSE)
pval1 <- sort(pval1, decreasing = FALSE)
pval2 <- sort(pval2, decreasing = FALSE)
pval3 <- sort(pval3, decreasing = FALSE)
pval4 <- sort(pval4, decreasing = FALSE)
pval5 <- sort(pval5, decreasing = FALSE)
#pval6 <- sort(pval6, decreasing = FALSE)
tval_dev1 <- sort(tval_dev1, decreasing = FALSE)
tval_dev2 <- sort(tval_dev2, decreasing = FALSE)
tval_dev3 <- sort(tval_dev3, decreasing = FALSE)
tval_dev4 <- sort(tval_dev4, decreasing = FALSE)
tval_dev5 <- sort(tval_dev5, decreasing = FALSE)
#tval_dev6 <- sort(tval_dev6, decreasing = FALSE)
pval_dev1 <- sort(pval_dev1, decreasing = FALSE)
pval_dev2 <- sort(pval_dev2, decreasing = FALSE)
pval_dev3 <- sort(pval_dev3, decreasing = FALSE)
pval_dev4 <- sort(pval_dev4, decreasing = FALSE)
pval_dev5 <- sort(pval_dev5, decreasing = FALSE)
#pval_dev6 <- sort(pval_dev6, decreasing = FALSE)
diff_diff1 <- sort(diff_diff1, decreasing = FALSE)
diff_diff2 <- sort(diff_diff2, decreasing = FALSE)
diff_diff3 <- sort(diff_diff3, decreasing = FALSE)
diff_diff4 <- sort(diff_diff4, decreasing = FALSE)
diff_diff5 <- sort(diff_diff5, decreasing = FALSE)
#diff_diff6 <- sort(diff_diff6, decreasing = FALSE)

p975 <- bootnumber * 0.975
p025 <- bootnumber * 0.025

CI_dev1 <- c((pe_dev[1] - dev1[p975]), (pe_dev[1] - dev1[p025]))
CI_dev2 <- c((pe_dev[2] - dev2[p975]), (pe_dev[2] - dev2[p025]))
CI_dev3 <- c((pe_dev[3] - dev3[p975]), (pe_dev[3] - dev3[p025]))
CI_dev4 <- c((pe_dev[4] - dev4[p975]), (pe_dev[4] - dev4[p025]))
CI_dev5 <- c((pe_dev[5] - dev5[p975]), (pe_dev[5] - dev5[p025]))
#CI_dev6 <- c((pe_dev[6] - dev6[p975]), (pe_dev[6] - dev6[p025]))

CI_tval1 <- c((pe_tval[1] - tval1[p975]), (pe_tval[1] - tval1[p025]))
CI_tval2 <- c((pe_tval[2] - tval2[p975]), (pe_tval[2] - tval2[p025]))
CI_tval3 <- c((pe_tval[3] - tval3[p975]), (pe_tval[3] - tval3[p025]))
CI_tval4 <- c((pe_tval[4] - tval4[p975]), (pe_tval[4] - tval4[p025]))
CI_tval5 <- c((pe_tval[5] - tval5[p975]), (pe_tval[5] - tval5[p025]))
#CI_tval6 <- c((pe_tval[6] - tval6[p975]), (pe_tval[6] - tval6[p025]))

CI_pval1 <- c((pe_pval[1] - pval1[p975]), (pe_pval[1] - pval1[p025]))
CI_pval2 <- c((pe_pval[2] - pval2[p975]), (pe_pval[2] - pval2[p025]))
CI_pval3 <- c((pe_pval[3] - pval3[p975]), (pe_pval[3] - pval3[p025]))
CI_pval4 <- c((pe_pval[4] - pval4[p975]), (pe_pval[4] - pval4[p025]))
CI_pval5 <- c((pe_pval[5] - pval5[p975]), (pe_pval[5] - pval5[p025]))
#CI_pval6 <- c((pe_pval[6] - pval6[p975]), (pe_pval[6] - pval6[p025]))

CI_tval_dev1 <- c((pe_tval_dev[1] - tval_dev1[p975]), (pe_tval_dev[1] - tval_dev1[p025]))
CI_tval_dev2 <- c((pe_tval_dev[2] - tval_dev2[p975]), (pe_tval_dev[2] - tval_dev2[p025]))
CI_tval_dev3 <- c((pe_tval_dev[3] - tval_dev3[p975]), (pe_tval_dev[3] - tval_dev3[p025]))
CI_tval_dev4 <- c((pe_tval_dev[4] - tval_dev4[p975]), (pe_tval_dev[4] - tval_dev4[p025]))
CI_tval_dev5 <- c((pe_tval_dev[5] - tval_dev5[p975]), (pe_tval_dev[5] - tval_dev5[p025]))
#CI_tval_dev6 <- c((pe_tval_dev[6] - tval_dev6[p975]), (pe_tval_dev[6] - tval_dev6[p025]))

CI_pval_dev1 <- c((pe_pval_dev[1] - pval_dev1[p975]), (pe_pval_dev[1] - pval_dev1[p025]))
CI_pval_dev2 <- c((pe_pval_dev[2] - pval_dev2[p975]), (pe_pval_dev[2] - pval_dev2[p025]))
CI_pval_dev3 <- c((pe_pval_dev[3] - pval_dev3[p975]), (pe_pval_dev[3] - pval_dev3[p025]))
CI_pval_dev4 <- c((pe_pval_dev[4] - pval_dev4[p975]), (pe_pval_dev[4] - pval_dev4[p025]))
CI_pval_dev5 <- c((pe_pval_dev[5] - pval_dev5[p975]), (pe_pval_dev[5] - pval_dev5[p025]))
#CI_pval_dev6 <- c((pe_pval_dev[6] - pval_dev6[p975]), (pe_pval_dev[6] - pval_dev6[p025]))

CI_diff_diff1 <- c((pe_diff_diff[1] - diff_diff1[p975]), (pe_diff_diff[1] - diff_diff1[p025]))
CI_diff_diff2 <- c((pe_diff_diff[2] - diff_diff2[p975]), (pe_diff_diff[2] - diff_diff2[p025]))
CI_diff_diff3 <- c((pe_diff_diff[3] - diff_diff3[p975]), (pe_diff_diff[3] - diff_diff3[p025]))
CI_diff_diff4 <- c((pe_diff_diff[4] - diff_diff4[p975]), (pe_diff_diff[4] - diff_diff4[p025]))
CI_diff_diff5 <- c((pe_diff_diff[5] - diff_diff5[p975]), (pe_diff_diff[5] - diff_diff5[p025]))
#CI_diff_diff6 <- c((pe_diff_diff[6] - diff_diff6[p975]), (pe_diff_diff[6] - diff_diff6[p025]))

df1 <- data.frame('Point estimate' = c(pe_dev[1], pe_tval[1], pe_pval[1], pe_tval_dev[1], pe_pval_dev[1], pe_diff_diff[1]),
                  'Lower 95% CI' = c(CI_dev1[1], CI_tval1[1], CI_pval1[1], CI_tval_dev1[1], CI_pval_dev1[1], CI_diff_diff1[1]),
                  'Upper 95% CI' = c(CI_dev1[2], CI_tval1[2], CI_pval1[2], CI_tval_dev1[2], CI_pval_dev1[2], CI_diff_diff1[2]))


df2 <- data.frame('Point estimate' = c(pe_dev[2], pe_tval[2], pe_pval[2], pe_tval_dev[2], pe_pval_dev[2], pe_diff_diff[2]),
                  'Lower 95% CI' = c(CI_dev2[1], CI_tval2[1], CI_pval2[1], CI_tval_dev2[1], CI_pval_dev2[1], CI_diff_diff2[1]),
                  'Upper 95% CI' = c(CI_dev2[2], CI_tval2[2], CI_pval2[2], CI_tval_dev2[2], CI_pval_dev2[2], CI_diff_diff2[2]))

df3 <- data.frame('Point estimate' = c(pe_dev[3], pe_tval[3], pe_pval[3], pe_tval_dev[3], pe_pval_dev[3], pe_diff_diff[3]),
                  'Lower 95% CI' = c(CI_dev3[1], CI_tval3[1], CI_pval3[1], CI_tval_dev3[1], CI_pval_dev3[1], CI_diff_diff3[1]),
                  'Upper 95% CI' = c(CI_dev3[2], CI_tval3[2], CI_pval3[2], CI_tval_dev3[2], CI_pval_dev3[2], CI_diff_diff3[2]))

df4 <- data.frame('Point estimate' = c(pe_dev[4], pe_tval[4], pe_pval[4], pe_tval_dev[4], pe_pval_dev[4], pe_diff_diff[4]),
                  'Lower 95% CI' = c(CI_dev4[1], CI_tval4[1], CI_pval4[1], CI_tval_dev4[1], CI_pval_dev4[1], CI_diff_diff4[1]),
                  'Upper 95% CI' = c(CI_dev4[2], CI_tval4[2], CI_pval4[2], CI_tval_dev4[2], CI_pval_dev4[2], CI_diff_diff4[2]))

df5 <- data.frame('Point estimate' = c(pe_dev[5], pe_tval[5], pe_pval[5], pe_tval_dev[5], pe_pval_dev[5], pe_diff_diff[5]),
                  'Lower 95% CI' = c(CI_dev5[1], CI_tval5[1], CI_pval5[1], CI_tval_dev5[1], CI_pval_dev5[1], CI_diff_diff5[1]),
                  'Upper 95% CI' = c(CI_dev5[2], CI_tval5[2], CI_pval5[2], CI_tval_dev5[2], CI_pval_dev5[2], CI_diff_diff5[2]))

#df6 <- data.frame('Point estimate' = c(pe_dev[6], pe_tval[6], pe_pval[6], pe_tval_dev[6], pe_pval_dev[6], pe_diff_diff[6]),
#                  'Lower 95% CI' = c(CI_dev6[1], CI_tval6[1], CI_pval6[1], CI_tval_dev6[1], CI_pval_dev6[1], CI_diff_diff6[1]),
#                  'Upper 95% CI' = c(CI_dev6[2], CI_tval6[2], CI_pval6[2], CI_tval_dev6[2], CI_pval_dev6[2], CI_diff_diff6[2]))

rownames(df1) <- c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', 
                   '4: Difference 2 and 1 (Naive apporach)', '5: Difference 3 and 1 (Segmented apporach)', '6: Difference 5 and 4 (Approch difference)')
rownames(df2) <- c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', 
                   '4: Difference 2 and 1 (Naive apporach)', '5: Difference 3 and 1 (Segmented apporach)', '6: Difference 5 and 4 (Approch difference)')
rownames(df3) <- c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', 
                   '4: Difference 2 and 1 (Naive apporach)', '5: Difference 3 and 1 (Segmented apporach)', '6: Difference 5 and 4 (Approch difference)')
rownames(df4) <- c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', 
                   '4: Difference 2 and 1 (Naive apporach)', '5: Difference 3 and 1 (Segmented apporach)', '6: Difference 5 and 4 (Approch difference)')
rownames(df5) <- c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', 
                   '4: Difference 2 and 1 (Naive apporach)', '5: Difference 3 and 1 (Segmented apporach)', '6: Difference 5 and 4 (Approch difference)')
#rownames(df6) <- c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', 
#                   '4: Difference 2 and 1 (Naive apporach)', '5: Difference 3 and 1 (Segmented apporach)', '6: Difference 5 and 4 (Approch difference)')

print(Pctable, nonnormal = biomarkers) 

library(knitr)

kable(df1)
kable(df2)
kable(df3)
kable(df4)
kable(df5)
```




# Skrivit all kod i R. Tycker det är enklare att implementera det i R-markdown (men behåller koden i python ifall det behövs)
```{r setup, include=False}
knitr::opts_chunk$set(comment = NA)
library(reticulate)
use_python("C:/Users/shamy/Miniconda3" , required = TRUE)
```

```{python, echo =FALSE}
# import library
import pandas as pd
from sklearn.linear_model import LogisticRegression
from tableone import TableOne
from sklearn.utils import resample
import random

# Importing data set from https://doi.org/10.5061/dryad.d22q6vh with pandas in python
df = pd.read_excel("https://datadryad.org/stash/downloads/file_stream/30857")

# Renaming columns to be more representative names
df.rename(columns={'resp_rate': 'Respiratory rate (per min)',
                   'BPS': 'Systolic blood pressure (mm Hg)',
                   'HR': 'Pulse (bpm)',
                   'temp': 'Temperature (°C)',
                   'SpO2': 'SpO2 (%)',
                   'confusion': 'Confusion',
                   'gender': 'Gender',
                   'age': 'Age',
                   'ICU': 'ICU admission'},
          inplace=True)

# Renaming indexes: f to Female, m to Male, ICU 1 to Admission, ICU 0 to no admission. confusion 1 to Confusion, confusion 0 to no confusion.
df['Gender'] = df['Gender'].replace({'f': 'Female', 'm': 'Male'})
df['ICU admission'] = df['ICU admission'].replace({1: 'Admission', 0: 'No admission'})
df['Confusion'] = df['Confusion'].replace({1: 'Confusion', 0: 'No confusion'})

# Creating table of characterisitcs (we can remove p-vals and missing values if we want (just letting it be as it is right now)
columns = ['ICU admission', 'Age', 'Gender', 'Systolic blood pressure (mm Hg)', 'Confusion', 'Pulse (bpm)',
           'Respiratory rate (per min)', 'SpO2 (%)', 'Temperature (°C)']
groupby = 'country'
nonnormal = ['Respiratory rate (per min)', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)', 'Temperature (°C)',
             'SpO2 (%)']

# Table of characteristics presented as: pctable
pctable = TableOne(df, columns=columns, groupby=groupby, nonnormal=nonnormal, missing = False)

# Renaming back indexes to original values just because my code is writton on that...
df['Gender'] = df['Gender'].replace({'Female': 'f', 'Male': 'm'})
df['ICU admission'] = df['ICU admission'].replace({'Admission': 1, 'No admission': 0})
df['Confusion'] = df['Confusion'].replace({'Confusion': 1, 'No confusion': 0})

# Splitting data based on country of origin
df_USA = df[df['country'] == 'USA']
df_France = df[df['country'] == 'France']
df_Switzerland = df[df['country'] == 'Switzerland']

# Assigning development sample and validation sample
combinations = [[df_USA, df_France], [df_USA, df_Switzerland], [df_France, df_USA], [df_Switzerland, df_USA], [df_Switzerland, df_France]]
name = ['USA_to_France', 'USA_to_Switzerland', 'France_to_USA', 'Switzerland_to_USA', 'Switzerland_to_France']

# removes SettingWithCopyWarning 
pd.options.mode.chained_assignment = None

u = 0
while u < len(combinations):
    devsample = combinations[u][0]
    valsample = combinations[u][1]

    feature_cols = ['Respiratory rate (per min)', 'Confusion', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)',
                    'Temperature (°C)', 'SpO2 (%)']
    Xd = devsample[feature_cols]
    yd = devsample['ICU admission']

    # Developing prediction model with the development sample
    logreg = LogisticRegression(max_iter=10000).fit(Xd, yd)

    # Assigning independet variables and dependent variables to the validation sample
    Xv = valsample[feature_cols]
    yv = valsample['ICU admission']

    # Assigning "country of origin index" devsamp = 1, valsample = 0.
    devsample['devval'] = 1
    valsample['devval'] = 0

    # Pooling devsample and valsample
    pooled = pd.concat([devsample, valsample])

    # Assigning independent variables and dependent variables in the pooled sample
    Xp = pooled[feature_cols]
    yp = pooled['devval']

    # Developing propensity model based on variables in pooled sample
    propensity = LogisticRegression(max_iter=10000).fit(Xp, yp)

    # Predicting origin of data from pooled sample and creating a list
    yp_pred = propensity.predict(Xp).tolist()

    # Creating the true origin of data from pooled sample as a list
    yp_true = pooled['devval'].tolist()

    # Comparing predicted and true origin of data lists in order to identify missmatched development sample data in list: missmatch
    missmatch = []
    listn = list(range(0, len(devsample)))

    for i in listn:
        if yp_pred[i] == 0:
            missmatch.append(i)

    # Making new segment with only missmatched development samples
    df_segment = devsample.iloc[missmatch]

    # Assinging independent and dependent variables in segmented samples
    Xt = df_segment[feature_cols]
    yt = df_segment['ICU admission']

    # Creating new lists for point estimates (PE):
    # PE in development sample
    pe_acc_dev = []
    # PE in true validation sample
    pe_acc_tval = []
    # PE in predicted validation sample
    pe_acc_pval = []
    # PE in difference between true val and dev
    pe_tval_dev = []
    # PE in difference between predicted val and dev
    pe_pval_dev = []
    # PE in difference between the differences pred val dev true val dev.
    pe_diff_diff = []

    # PE accuracy in development sample added to PE list
    pe_acc_dev.append(logreg.score(Xd, yd) * 100)
    # PE accuracy in validation sample added to PE list
    pe_acc_tval.append(logreg.score(Xv, yv) * 100)
    # PE prediction of true accuracy in validation sample added to PE list
    pe_acc_pval.append(logreg.score(Xt, yt) * 100)
    # PE difference accuracy between true val and dev added to PE list
    pe_tval_dev.append(pe_acc_tval[0] - pe_acc_dev[0])
    # PE difference accuracy between pred val and dev added to PE list
    pe_pval_dev.append(pe_acc_pval[0] - pe_acc_dev[0])
    # PE difference accuracy between differences added to PE list
    pe_diff_diff.append(pe_pval_dev[0] - pe_tval_dev[0])

    # list of accuracys and differences that will be calculated from the bootstrapping
    # 1: accuracy in development sample
    list_acc_dev = []
    # 2: true accuracy in validation sample
    list_acc_tval = []
    # 3: predicted accuracy in validation sample with segment
    list_acc_pval = []
    # 4: accuracy difference between 2 and 1 (Naive approach)
    list_acc_diff_tval_dev = []
    # 5: accuracy difference between 3 and 1 (Segmented approach)
    list_acc_diff_pval_dev = []
    # 6: accuracy difference between 5 and 4 (Approach difference)
    list_acc_diff_diff = []

    # resample with replacement from devsample and valsample and do the exact same processes as before but with bootstrapped amount of times to develop 95% confidence intervalls
    # assigning amount of bootstraps performed (we are doing 1000 in our study, can be changed in order to just see if it works)
    bootstrap = 20

    ### while looping everything to be able to boostrap confidence intervalls
    z = 0
    while z < bootstrap:
        # assigning random interger to resample seed
        randint = random.randint(1, 1000000)
        randinz = random.randint(1, 1000000)

        # creating new resamples of development sample and validation sample
        devsamp = resample(df, n_samples=len(df), replace=True, random_state=randint)
        valsamp = resample(df, n_samples=len(df), replace=True, random_state=randinz)

        # assigning independent variables and dependent variables to the development sample
        feature_cols = ['Respiratory rate (per min)', 'Confusion', 'Systolic blood pressure (mm Hg)',
                        'Pulse (bpm)', 'Temperature (°C)', 'SpO2 (%)']
        Xd = devsamp[feature_cols]
        yd = devsamp['ICU admission']

        # Developing prediction model with the development sample
        # If error occurs, restarts iteration, if not it will continue with the process
        # Error is a value error, happens due to risk for only 1:s or 0:s as outcome value => cannot fit logistic regression with that.
        try:
            logreg = LogisticRegression(max_iter=10000).fit(Xd, yd)
        except:
            pass
        else:
            # Assigning independet variables and dependent variables to the validation sample
            Xv = valsamp[feature_cols]
            yv = valsamp['ICU admission']

            # Assigning "country of origin index" devsamp = 1, valsamp = 0.


            devsamp['devval'] = 1
            valsamp['devval'] = 0


            # Pooling devsamp and valsamp
            pooled = pd.concat([devsamp, valsamp])

            # Assigning independent variables and dependent variables in the pooled sample
            Xp = pooled[feature_cols]
            yp = pooled['devval']

            # Developing propensity model based on variables in pooled sample
            # If error occurs, restarts iteration, if not it will continue with the process
            # Error is a value error, happens due to risk for only 1:s or 0:s as outcome values => cannot fit logistic regression with that
            try:
                propensity = LogisticRegression(max_iter=10000).fit(Xp, yp)
            except:
                pass
            else:
                # Predicting origin of data from pooled sample and creating a list
                yp_pred = propensity.predict(Xp).tolist()

                # Creating the true origin of data from pooled sample as a list
                yp_true = pooled['devval'].tolist()

                # Comparing predicted and true origin of data lists in order to identify missmatched development sample data in list: missmatch
                missmatch = []
                listn = list(range(0, len(devsamp)))

                for i in listn:
                    if yp_pred[i] == 0:
                        missmatch.append(i)

                # If 0 missmatched development sample are identified an error will occur during score counting, will therefore
                if len(missmatch) == 0:
                    pass
                else:
                    # Making new segment with only missmatched development samples
                    df_segment = devsamp.iloc[missmatch]

                    # Assinging independent and dependent variables in segmented samples
                    Xt = df_segment[feature_cols]
                    yt = df_segment['ICU admission']

                    # Predicting performance in development sample and storing accuracy in: list_acc_dev
                    list_acc_dev.append(logreg.score(Xd, yd) * 100)
                    # "true" performance predicted in validation sample and storing accuracy in: list_acc_tval
                    list_acc_tval.append(logreg.score(Xv, yv) * 100)
                    # "predicted" performance of prediction model in validation sample based on prediction made on segmented sample and storing accuracy in: list_acc_pval
                    list_acc_pval.append(logreg.score(Xt, yt) * 100)

                    # rerunning itterations untill satistifed with bootstrap
                    z += 1

    # Calculating difference between true performance accuracy and development accuracy (naive apporach) for each bootstrap and adding to: list_acc_diff_tval_dev
    # Calculating difference between predicted performance accuracy and development accuracy (segmented approach) for each bootstrap and adding to: list_acc_diff_pval_dev
    k = 0
    while k < bootstrap:
        list_acc_diff_tval_dev.append(list_acc_tval[k] - list_acc_dev[k])
        list_acc_diff_pval_dev.append(list_acc_pval[k] - list_acc_dev[k])
        list_acc_diff_diff.append(list_acc_diff_pval_dev[k] - list_acc_diff_tval_dev[k])
        k += 1

    # Creating lists for empirical bootstrap that we are going to choose 97,5 percentile and 2,5 percentile from
    # List for difference between PE dev and bootstrapped devs
    dev = []
    # List for difference between PE tval and bootstrapped tvals
    tval = []
    # List for difference between PE pval and bootstrapped pvals
    pval = []
    # List for difference between PE tval minus dev and bootstrapped tval minus devs (naive approach empirical difference)
    tvaldev = []
    # List for difference between PE pval minus dev and bootstrapped pval minus devs (segmented approach empirical difference)
    pvaldev = []
    # List for difference between segmented approach empirical difference and naive approach empirical difference
    diffdiff = []

    # Adding values to lists with differences between point estimates and bootstraps
    k = 0
    while k < bootstrap:
        dev.append(list_acc_dev[k] - pe_acc_dev[0])
        tval.append(list_acc_tval[k] - pe_acc_tval[0])
        pval.append(list_acc_pval[k] - pe_acc_pval[0])
        tvaldev.append(list_acc_diff_tval_dev[k] - pe_tval_dev[0])
        pvaldev.append(list_acc_diff_pval_dev[k] - pe_pval_dev[0])
        diffdiff.append(list_acc_diff_diff[k] - pe_diff_diff[0])
        k += 1

    # Sorting lists with differences between point estimates and bootstraps
    dev.sort()
    tval.sort()
    pval.sort()
    tvaldev.sort()
    pvaldev.sort()
    diffdiff.sort()

    # Assigning 97,5th percentile index
    percentile975 = int((len(list_acc_dev)) * 0.975 - 1)
    # Assigning 2,5th percentile index
    percentile25 = int((len(list_acc_dev)) * 0.025)

    # Assinging upper and lower confidence intervals for development sample accuracy (u = upper, l = lower)
    ci_dev_u = round(pe_acc_dev[0] - dev[percentile25], 2)
    ci_dev_l = round(pe_acc_dev[0] - dev[percentile975], 2)
    # Assinging upper and lower confidence intervals for true validation sample accuracy (u = upper, l = lower)
    ci_tval_u = round(pe_acc_tval[0] - tval[percentile25], 2)
    ci_tval_l = round(pe_acc_tval[0] - tval[percentile975], 2)
    # Assinging upper and lower confidence intervals for predicted validation sample accuracy (u = upper, l = lower)
    ci_pval_u = round(pe_acc_pval[0] - pval[percentile25], 2)
    ci_pval_l = round(pe_acc_pval[0] - pval[percentile975], 2)
    # Assinging upper and lower confidence intervals for difference between true validation accuracy and development sample accuracy (u = upper, l = lower)
    ci_tval_dev_u = round(pe_tval_dev[0] - tvaldev[percentile25], 2)
    ci_tval_dev_l = round(pe_tval_dev[0] - tvaldev[percentile975], 2)
    # Assinging upper and lower confidence intervals for difference between predicted validation accuracy and development sample accuracy (u = upper, l = lower)
    ci_pval_dev_u = round(pe_pval_dev[0] - pvaldev[percentile25], 2)
    ci_pval_dev_l = round(pe_pval_dev[0] - pvaldev[percentile975], 2)
    # Assinging upper and lower confidence intervals for difference between the 2 previous differences described (u = upper, l = lower)
    ci_diff_diff_u = round(pe_diff_diff[0] - diffdiff[percentile25], 2)
    ci_diff_diff_l = round(pe_diff_diff[0] - diffdiff[percentile975], 2)

    # developing dataframe of confidence intervals and means in order to make markdown table with them
    data = {
        'Point estimate': [str(round(pe_acc_dev[0], 2)), str(round(pe_acc_tval[0], 2)), str(round(pe_acc_pval[0], 2)),
                           str(round(pe_tval_dev[0], 2)), str(round(pe_pval_dev[0], 2)),
                           str(round(pe_diff_diff[0], 2))],
        '95% Confidence Interval': ['[' + str(ci_dev_l) + ', ' + str(ci_dev_u) + ']',
                                    '[' + str(ci_tval_l) + ', ' + str(ci_tval_u) + ']',
                                    '[' + str(ci_pval_l) + ', ' + str(ci_pval_u) + ']',
                                    '[' + str(ci_tval_dev_l) + ', ' + str(ci_tval_dev_u) + ']',
                                    '[' + str(ci_pval_dev_l) + ', ' + str(ci_pval_dev_u) + ']',
                                    '[' + str(ci_diff_diff_l) + ', ' + str(ci_diff_diff_u) + ']']}

    dfci = pd.DataFrame(data,
                        columns=['Point estimate', '95% Confidence Interval'],
                        index=['1: Accuracy in development sample',
                               '2: Accuracy in validation sample',
                               '3: Accuracy in segmented sample',
                               '4: Difference 2 and 1 (naive approach)',
                               '5: Difference 3 and 1 (segmented approach)',
                               '6: Difference 5 and 4 (approach difference)'])

    vars()[name[u]] = dfci

    u += 1

# printing patient characterisitcs
print('Patient Characteristics')
print(pctable.tabulate(tablefmt="markdown"))
print(' ')
# printing USA transfer to France
print(name[0])
print(USA_to_France.to_markdown())
print(' ')
# printing USA transfer to Switzerland
print(name[1])
print(USA_to_Switzerland.to_markdown())
print(' ')
# printing France to USA
print(name[2])
print(France_to_USA.to_markdown())
print(' ')
# printing Switzerland to USA
print(name[3])
print(Switzerland_to_USA.to_markdown())
print(' ')
# printing Switzerland to France
print(name[4])
print(Switzerland_to_France.to_markdown())
```
