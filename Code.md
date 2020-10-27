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
df <- simulated.data

#install.packages('tableone')
library(tableone)
#install.packages('survival')
library(survival)
#install.packages('dplyr')
library(dplyr)
#install.packages('boot')
library(boot)

new.colnames <- c(strata = "Country",
                  age = "Age",                  
                  resp_rate = "Respiratory rate (per min)",
                  SpO2 = "Peripheral oxygen saturation (%)",
                  BPS = "Systolic blood pressure (mm Hg)",
                  HR = "Pulse (bpm)",
                  temp = "Temperature (°C)",
                  y = "ICU admission")
df <- df[names(new.colnames)]
colnames(df) <- new.colnames

df$`ICU admission`[df$`ICU admission` == 1] <- 'Yes'
df$`ICU admission`[df$`ICU admission` == 0] <- 'No'

strata <- "Country"
vars <- colnames(df)[!(colnames(df) %in% strata)]
Pctable <- CreateTableOne(vars = vars, data = df, strata = strata, test = FALSE)

df$`ICU admission`[df$`ICU admission` == 'Yes'] <- 1
df$`ICU admission`[df$`ICU admission` == 'No'] <- 0

df$`ICU admission` <- as.numeric(df$`ICU admission`)

df_USA <- df[df$Country == "USA", ]
df_France <- df[df$Country == "France", ]
df_Swizerland <- df[df$Country == "Switzerland", ]

#---------------------------------------------
pe_dev <- c()
pe_tval <- c()
pe_pval <- c()
pe_tval_dev <- c()
pe_pval_dev <- c()
pe_diff_diff <- c()
#---------------------------------------------
#1

combinations <- expand.grid(rep(list(unique(df[, strata])), 2))
strata.combinations <- t(combinations[combinations$Var1 != combinations$Var2, ]) %>% as.data.frame()

estimate_performance <- function(strata.combination, df) {
    devsample <- df[df[, strata] == strata.combination[1], ]
    valsample <- df[df[, strata] == strata.combination[2], ]

    logreg <- glm(`ICU admission` ~ `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)`, data = devsample, family = binomial)
    probabilities <- logreg %>% predict(devsample, type = "response")
    predict.classesdev <- ifelse(probabilities > 0.5, 1, 0)

    ## Accuracy in development sample
    x <- mean(predict.classesdev == devsample$`ICU admission`) * 100

    probabilities <- logreg %>% predict(valsample, type = "response")
    predict.classesval <- ifelse(probabilities > 0.5, 1, 0)

    ## Accuracy in validation sample
    y <- mean(predict.classesval == valsample$`ICU admission`) * 100

    ## Assign data "origin" as new variable and combine data
    devsample['devval'] <- 1
    valsample['devval'] <- 0

    df_pooled <- rbind(devsample, valsample)

    ## Create propensity model
    logregi <- glm(`devval` ~ `Age` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` + `Respiratory rate (per min)`, data = df_pooled, family = binomial)
    probabilities <- logregi %>% predict(df_pooled, type = "response")
    predict.classespool <- ifelse(probabilities > 0.5, 1, 0)

    ## Identify the segment of observation in the development data that
    ## are "most similar" to the observations in the validation data
    missmatch <- predict.classespool == 0 & df_pooled$devval == 1
    df_segment <-devsample[missmatch, ]

    ## Accuracy in segment
    probabilities <- logreg %>% predict(df_segment, type = "response")
    predict.classessegment <- ifelse(probabilities > 0.5, 1, 0)

    z <- mean(predict.classessegment == df_segment$`ICU admission`) * 100

    ## Redefined some of these measures. Now a positive diff_diff
    ## means that the segmented approach works better than the naive
    ## approach
    pe_dev <- x
    pe_tval <- y
    pe_pval <- z
    pe_tval_dev <- abs(y - x)
    pe_pval_dev <- abs(z - x)
    pe_diff_diff <- pe_tval_dev - pe_pval_dev 

    stats <- c(pe_dev = pe_dev,
               pe_tval = pe_tval,
               pe_pval = pe_pval,
               pe_tval_dev = pe_tval_dev,
               pe_pval_dev = pe_pval_dev,
               pe_diff_diff = pe_diff_diff)
    return (stats)
}

run_simulation <- function(df, rows, strata.combinations) {
    boot.data <- df[rows, ]
    performance.estimates <- lapply(strata.combinations, estimate_performance, df = boot.data)
    names(performance.estimates) <- sapply(strata.combinations, paste0, collapse = ".to.")
    results <- unlist(performance.estimates)
    return (results)
}

n.bootstraps <- 5
boot.results <- boot(df, run_simulation, R = n.bootstraps, strata.combinations = strata.combinations)
cis <- lapply(seq_along(boot.results$t0), function(i) boot.ci(boot.results, index = i, type = "norm"))
pes.with.cis <- lapply(cis, function(ci) c(pe = ci$t0, lb = ci$normal[2], ub = ci$normal[3]))

## The above should be enough to get you the point estimates with 95% CIs (increse the number of bootstraps to 1000) for all relavant combinations of transfers

#------------------ I created these to straify for each accuracy mode instead for each country transfer, this way it is easier to create error bars.
i = 31
df_pr_dev <- data.frame(`Point estimate (95% CI)` = c(round(pes.with.cis[[i-30]][[1]],2), round(pes.with.cis[[i-24]][[1]],2), 
                                                            round(pes.with.cis[[i-18]][[1]],2), round(pes.with.cis[[i-12]][[1]],2),
                                                            round(pes.with.cis[[i-6]][[1]],2), round(pes.with.cis[[i]][[1]],2)),
                              
                    LCI = c(round(pes.with.cis[[i-30]][[2]],2), round(pes.with.cis[[i-24]][[2]],2), 
                            round(pes.with.cis[[i-18]][[2]],2), round(pes.with.cis[[i-12]][[2]],2),
                            round(pes.with.cis[[i-6]][[2]],2), round(pes.with.cis[[i]][[2]],2)),
                    
                    UCI = c(round(pes.with.cis[[i-30]][[3]],2), round(pes.with.cis[[i-24]][[3]],2), 
                            round(pes.with.cis[[i-18]][[3]],2), round(pes.with.cis[[i-12]][[3]],2),
                            round(pes.with.cis[[i-6]][[3]],2), round(pes.with.cis[[i]][[3]],2)),
                    check.names = FALSE,
                    `Country transfer` = c(paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '),
                                           paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '),
                                           paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '),
                                           paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '),
                                           paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '),
                                           paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to ')))

i = 32
df_pr_tval <- data.frame(`Point estimate (95% CI)` = c(round(pes.with.cis[[i-30]][[1]],2), round(pes.with.cis[[i-24]][[1]],2), 
                                                            round(pes.with.cis[[i-18]][[1]],2), round(pes.with.cis[[i-12]][[1]],2),
                                                            round(pes.with.cis[[i-6]][[1]],2), round(pes.with.cis[[i]][[1]],2)),
                              
                    LCI = c(round(pes.with.cis[[i-30]][[2]],2), round(pes.with.cis[[i-24]][[2]],2), 
                            round(pes.with.cis[[i-18]][[2]],2), round(pes.with.cis[[i-12]][[2]],2),
                            round(pes.with.cis[[i-6]][[2]],2), round(pes.with.cis[[i]][[2]],2)),
                    
                    UCI = c(round(pes.with.cis[[i-30]][[3]],2), round(pes.with.cis[[i-24]][[3]],2), 
                            round(pes.with.cis[[i-18]][[3]],2), round(pes.with.cis[[i-12]][[3]],2),
                            round(pes.with.cis[[i-6]][[3]],2), round(pes.with.cis[[i]][[3]],2)),
                    check.names = FALSE,
                    `Country transfer` = c(paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '),
                                           paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '),
                                           paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '),
                                           paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '),
                                           paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '),
                                           paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to ')))
i = 33
df_pr_pval <- data.frame(`Point estimate (95% CI)` = c(round(pes.with.cis[[i-30]][[1]],2), round(pes.with.cis[[i-24]][[1]],2), 
                                                            round(pes.with.cis[[i-18]][[1]],2), round(pes.with.cis[[i-12]][[1]],2),
                                                            round(pes.with.cis[[i-6]][[1]],2), round(pes.with.cis[[i]][[1]],2)),
                              
                    LCI = c(round(pes.with.cis[[i-30]][[2]],2), round(pes.with.cis[[i-24]][[2]],2), 
                            round(pes.with.cis[[i-18]][[2]],2), round(pes.with.cis[[i-12]][[2]],2),
                            round(pes.with.cis[[i-6]][[2]],2), round(pes.with.cis[[i]][[2]],2)),
                    
                    UCI = c(round(pes.with.cis[[i-30]][[3]],2), round(pes.with.cis[[i-24]][[3]],2), 
                            round(pes.with.cis[[i-18]][[3]],2), round(pes.with.cis[[i-12]][[3]],2),
                            round(pes.with.cis[[i-6]][[3]],2), round(pes.with.cis[[i]][[3]],2)),
                    check.names = FALSE,
                    `Country transfer` = c(paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '),
                                           paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '),
                                           paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '),
                                           paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '),
                                           paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '),
                                           paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to ')))

i = 34
df_pr_tval_dev <- data.frame(`Point estimate (95% CI)` = c(round(pes.with.cis[[i-30]][[1]],2), round(pes.with.cis[[i-24]][[1]],2), 
                                                            round(pes.with.cis[[i-18]][[1]],2), round(pes.with.cis[[i-12]][[1]],2),
                                                            round(pes.with.cis[[i-6]][[1]],2), round(pes.with.cis[[i]][[1]],2)),
                              
                    LCI = c(round(pes.with.cis[[i-30]][[2]],2), round(pes.with.cis[[i-24]][[2]],2), 
                            round(pes.with.cis[[i-18]][[2]],2), round(pes.with.cis[[i-12]][[2]],2),
                            round(pes.with.cis[[i-6]][[2]],2), round(pes.with.cis[[i]][[2]],2)),
                    
                    UCI = c(round(pes.with.cis[[i-30]][[3]],2), round(pes.with.cis[[i-24]][[3]],2), 
                            round(pes.with.cis[[i-18]][[3]],2), round(pes.with.cis[[i-12]][[3]],2),
                            round(pes.with.cis[[i-6]][[3]],2), round(pes.with.cis[[i]][[3]],2)),
                    check.names = FALSE,
                    `Country transfer` = c(paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '),
                                           paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '),
                                           paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '),
                                           paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '),
                                           paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '),
                                           paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to ')))

i = 35
df_pr_pval_dev <- data.frame(`Point estimate (95% CI)` = c(round(pes.with.cis[[i-30]][[1]],2), round(pes.with.cis[[i-24]][[1]],2), 
                                                            round(pes.with.cis[[i-18]][[1]],2), round(pes.with.cis[[i-12]][[1]],2),
                                                            round(pes.with.cis[[i-6]][[1]],2), round(pes.with.cis[[i]][[1]],2)),
                              
                    LCI = c(round(pes.with.cis[[i-30]][[2]],2), round(pes.with.cis[[i-24]][[2]],2), 
                            round(pes.with.cis[[i-18]][[2]],2), round(pes.with.cis[[i-12]][[2]],2),
                            round(pes.with.cis[[i-6]][[2]],2), round(pes.with.cis[[i]][[2]],2)),
                    
                    UCI = c(round(pes.with.cis[[i-30]][[3]],2), round(pes.with.cis[[i-24]][[3]],2), 
                            round(pes.with.cis[[i-18]][[3]],2), round(pes.with.cis[[i-12]][[3]],2),
                            round(pes.with.cis[[i-6]][[3]],2), round(pes.with.cis[[i]][[3]],2)),
                    check.names = FALSE,
                    `Country transfer` = c(paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '),
                                           paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '),
                                           paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '),
                                           paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '),
                                           paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '),
                                           paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to ')))
i = 36
df_pr_diff_diff <- data.frame(`Point estimate (95% CI)` = c(round(pes.with.cis[[i-30]][[1]],2), round(pes.with.cis[[i-24]][[1]],2), 
                                                            round(pes.with.cis[[i-18]][[1]],2), round(pes.with.cis[[i-12]][[1]],2),
                                                            round(pes.with.cis[[i-6]][[1]],2), round(pes.with.cis[[i]][[1]],2)),
                              
                    LCI = c(round(pes.with.cis[[i-30]][[2]],2), round(pes.with.cis[[i-24]][[2]],2), 
                            round(pes.with.cis[[i-18]][[2]],2), round(pes.with.cis[[i-12]][[2]],2),
                            round(pes.with.cis[[i-6]][[2]],2), round(pes.with.cis[[i]][[2]],2)),
                    
                    UCI = c(round(pes.with.cis[[i-30]][[3]],2), round(pes.with.cis[[i-24]][[3]],2), 
                            round(pes.with.cis[[i-18]][[3]],2), round(pes.with.cis[[i-12]][[3]],2),
                            round(pes.with.cis[[i-6]][[3]],2), round(pes.with.cis[[i]][[3]],2)),
                    check.names = FALSE,
                    `Country transfer` = c(paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '),
                                           paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '),
                                           paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '),
                                           paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '),
                                           paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '),
                                           paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to ')))
                                           
#------------- labeling significance to those that do not span over 0. (differences)
sig_tval_dev <- c()
sig_pval_dev <- c()
sig_diff_diff <- c()

i = 1
while (i <= 6) {
    if (between(0, df_pr_tval_dev$LCI[i], df_pr_tval_dev$UCI[i]) == FALSE) {
    sig_tval_dev <- c(sig_tval_dev, '*')
    } else {sig_tval_dev <- c(sig_tval_dev, '')}
    i = i+1
}
i = 1
while (i <= 6) {
    if (between(0, df_pr_pval_dev$LCI[i], df_pr_pval_dev$UCI[i]) == FALSE) {
    sig_pval_dev <- c(sig_pval_dev, '*')
    } else {sig_pval_dev <- c(sig_pval_dev, '')}
    i = i+1
}
i = 1
while (i <= 6) {
    if (between(0, df_pr_diff_diff$LCI[i], df_pr_diff_diff$UCI[i]) == FALSE) {
    sig_diff_diff <- c(sig_diff_diff, '*')
    } else {sig_diff_diff <- c(sig_diff_diff, '')}
    i = i+1
}

#---------- creating ggplot error bars with pointrange in order to visually show the 95% CI
#install.packages('ggplot2')
library(ggplot2)
#errorbar + pointrange (could add singificance label with a triangle above singificant differences)
ggplot() +
    geom_errorbar(data=df_pr_dev, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), width=0.2, size=0.7, color="black") +
    geom_pointrange(data=df_pr_dev, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), size=0.2, color="black", fill="black",
                    shape=22) +
    ylab('Accuracy in development sample') +
    coord_flip() +
    theme_test() 

ggplot() +
    geom_errorbar(data=df_pr_tval, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), width=0.2, size=0.7, color="black") +
    geom_pointrange(data=df_pr_tval, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), size=0.2, color="black", fill="black",
                    shape=22) +
    ylab('Accuracy in validation sample') +
    coord_flip() +
    theme_test() 

ggplot() +
    geom_errorbar(data=df_pr_pval, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), width=0.2, size=0.7, color="black") +
    geom_pointrange(data=df_pr_pval, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), size=0.2, color="black", fill="black",
                    shape=22) +
    ylab('Accuracy in segmented sample') +
    coord_flip() +
    theme_test() 

ggplot(df_pr_tval_dev, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`)) +
    geom_errorbar(mapping=aes(ymin=LCI, ymax=UCI), width=0.2, size=0.7, color="black") +
    geom_pointrange(data=df_pr_tval_dev, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), size=0.2, color="black", fill="black",
                    shape=22) +
    ylab('Accuracy difference naive approach') +
    geom_text(aes(label = sig_tval_dev), nudge_x = 0.3) +
    geom_hline(yintercept = 0, alpha = 0.4) +
    coord_flip() +
    theme_test() 

ggplot(df_pr_pval_dev, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`)) +
    geom_errorbar(mapping=aes(ymin=LCI, ymax=UCI), width=0.2, size=0.7, color="black") +
    geom_pointrange(data=df_pr_pval_dev, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), size=0.2, color="black", fill="black",
                    shape=22) +
    ylab('Accuracy difference segmented approach') +
    geom_text(aes(label = sig_pval_dev), nudge_x = 0.3) +
    geom_hline(yintercept = 0, alpha = 0.4) +
    coord_flip() +
    theme_test() 
    

ggplot(df_pr_diff_diff, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`)) +
    geom_errorbar(mapping=aes(ymin=LCI, ymax=UCI), width=0.2, size=0.7, color="black") +
    geom_pointrange(data=df_pr_diff_diff, mapping=aes(x =`Country transfer`, y=`Point estimate (95% CI)`, ymin=LCI, ymax=UCI), size=0.2, color="black", fill="black",
                    shape=22) +
    ylab('Approach accuracy difference') +
    geom_text(aes(label = sig_diff_diff), nudge_x = 0.3) +
    geom_hline(yintercept = 0, alpha = 0.4) +
    coord_flip() +
    theme_test()


