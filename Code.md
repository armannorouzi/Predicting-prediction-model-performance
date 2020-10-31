```r, echo = FALSE
knitr::opts_chunk$set(comment = NA)
library(rio)
library(dplyr)
library(MASS)
library(tableone)
library(survival)
library(dplyr)
library(boot)
url <- "https://datadryad.org/stash/downloads/file_stream/30857"
raw.data <- import(url, format = "xls") %>% as.data.frame()

## This function creates a simulated dataset, i.e. simulates data for
## each level of a given strata variables and combines these data into
## a single data.frame. The size is for each level of strata.
create_simulated_dataset <- function(raw.data, strata, outcome, predictors, size = 10000) {
    data.list <- split(raw.data, f = as.factor(raw.data[, strata]))
    simulated.data.list <- lapply(data.list, simulate_data,
                                  outcome = outcome, predictors = predictors,
                                  size = size)
    simulated.data.list <- lapply(names(data.list), function(name) {
        dataset <- simulated.data.list[[name]]
        dataset$strata <- name
        dataset
    })
    simulated.dataset <- do.call(rbind, simulated.data.list) %>% as.data.frame()
    simulated.dataset
}

## This function simulates a dataset with continuous predictors and a
## binary outcome. It achieves this based on a "template" dataset
## using its covariance matrix and fitting a logistic regression model
## with the true data. 
simulate_data <- function(dataset, outcome, predictors, size) {
    y <- dataset[, outcome]
    x <- dataset[predictors]
    fit <- glm(y ~ ., family = binomial, data = cbind(y, x))
    cov.matrix <- cov(x)
    sim.data <- mvrnorm(size, sapply(x, mean), cov.matrix, empirical = TRUE) %>% as.data.frame()
    sim.data$yhat <- predict(fit, newdata = sim.data, type = "response")
    sim.data$y <- rbinom(nrow(sim.data), 1, prob = sim.data$yhat)
    sim.data
}

## This function estimates the performance of each approach in a
## specific strata combination, as well as the differences between
## approaches
estimate_performance <- function(strata.combination, strata, df) {
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

    ## We are interested in the "error" associated with each
    ## approach. By taking the absolute difference we weigh positive
    ## and negative errors equally, i.e. we say that it is equally
    ## wrong to overestimate accuracy as it is to underestimate. This
    ## simplifies the calculations. Consider the following example, if
    ## the accuracy of model A is 0.7 in the development sample and
    ## the true accuracy in the validation sample is 0.6. Then the
    ## error is 0.1. If the accuracy in the segmented sample is 0.75
    ## then the error of this approach is -.05. When we are
    ## calculating the difference in error we get -0.15, which is not
    ## what we want, because we can see that the difference in error
    ## is really 0.05. One easy solution is then to work with absolute
    ## differences. Hope this makes sense.
    
    stats <- c(pe_dev = pe_dev,
               pe_tval = pe_tval,
               pe_pval = pe_pval,
               pe_tval_dev = pe_tval_dev,
               pe_pval_dev = pe_pval_dev,
               pe_diff_diff = pe_diff_diff)
    return (stats)
}

## This function runs one simulation
run_simulation <- function(raw.data, strata, strata.combinations, outcome, predictors, size = 10000) {
    df <- create_simulated_dataset(raw.data, strata, outcome, predictors, size = size)
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
    ## Pctable <- CreateTableOne(vars = vars, data = df, strata = strata, test = FALSE)
    df$`ICU admission`[df$`ICU admission` == 'Yes'] <- 1
    df$`ICU admission`[df$`ICU admission` == 'No'] <- 0
    df$`ICU admission` <- as.numeric(df$`ICU admission`)
    df_USA <- df[df$Country == "USA", ]
    df_France <- df[df$Country == "France", ]
    df_Swizerland <- df[df$Country == "Switzerland", ]
    performance.estimates <- lapply(strata.combinations, estimate_performance, strata = strata, df = df)
    names(performance.estimates) <- sapply(strata.combinations, paste0, collapse = ".to.")
    results <- unlist(performance.estimates)
    return (results)
}

## Okay, so instead of creting one simulated dataset and bootstrapping
## that I've revised the code to repeat the simulation process
## multiple times and then using the mean across simulations as the
## point estimate and the 2.5% and 97.5% percentiles as measures of
## uncertainty
strata <- "country"
predictors <- c("resp_rate", "SpO2", "BPS", "HR", "temp", "age")
outcome <- "ICU"
size <- 10000
combinations <- expand.grid(rep(list(unique(raw.data[, strata])), 2))
strata.combinations <- t(combinations[combinations$Var1 != combinations$Var2, ]) %>% as.data.frame()
n.simulations <- 5
simulation.results <- lapply(seq_len(n.simulations), function(i) run_simulation(
                                                                     raw.data = raw.data,
                                                                     strata = strata,
                                                                     strata.combinations = strata.combinations,
                                                                     outcome = outcome,
                                                                     predictors = predictors,
                                                                     size = size))
simulation.data <- do.call(rbind, simulation.results)
pe <- colMeans(simulation.data)
cis <- lapply(as.data.frame(simulation.data), function(x) {
    quantiles <- quantile(x, probs = c(0.025, 0.975))
    names(quantiles) <- NULL
    c(lb = quantiles[1], ub = quantiles[2])
}) %>% as.data.frame() %>% t()
pes.with.cis <- cbind(pe, cis) %>% as.data.frame() %>% split(f = as.factor(rownames(cis)))

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
#------------------------------------------------
#tables med våra värden
i = 1
sig1.3 <- c()
while (i <= 6) {
    if (i <= 3) {
        sig1.3 <- c(sig1.3, '')
    } else { if (between(0, pes.with.cis[[i]][[2]], pes.with.cis[[i]][[3]]) == FALSE) {
        sig1.3 <- c(sig1.3, '*')
    } else {sig1.3 <- c(sig1.3, '')}
    }
    i = i+1
}
i = 1    
df1.3 <- data.frame(`Point estimate` = c(round(pes.with.cis[[i]][[1]],2), round(pes.with.cis[[i+1]][[1]],2), round(pes.with.cis[[i+2]][[1]],2), round(pes.with.cis[[i+3]][[1]],2),
                                         round(pes.with.cis[[i+4]][[1]],2), round(pes.with.cis[[i+5]][[1]],2)),
                    `[95% CI]` = c(paste('[', toString(round(pes.with.cis[[i]][[2]],2)), ' - ', toString(round(pes.with.cis[[i]][[3]],2)), ']', sig1.3[1], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+1]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+1]][[3]],2)), ']', sig1.3[2], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+2]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+2]][[3]],2)), ']', sig1.3[3], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+3]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+3]][[3]],2)), ']', sig1.3[4], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+4]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+4]][[3]],2)), ']', sig1.3[5], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+5]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+5]][[3]],2)), ']', sig1.3[6], sep ='')),
                    row.names = c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', '4: Difference 2 and 1 (Naive apporach)',
                                  '5: Difference 3 and 1 (Segmented approach)', '6: Difference 5 and 4 (Approach difference)'),
                    check.names = FALSE)

i = 1
sig2.3 <- c()
while (i <= 6) {
    if (i <= 3) {
        sig2.3 <- c(sig2.3, '')
    } else { if (between(0, pes.with.cis[[i+6]][[2]], pes.with.cis[[i+6]][[3]]) == FALSE) {
        sig2.3 <- c(sig2.3, '*')
    } else { sig2.3 <- c(sig2.3, '')}
    }
    i = i+1
}
i = 7    
df2.3 <- data.frame(`Point estimate` = c(round(pes.with.cis[[i]][[1]],2), round(pes.with.cis[[i+1]][[1]],2), round(pes.with.cis[[i+2]][[1]],2), round(pes.with.cis[[i+3]][[1]],2),
                                         round(pes.with.cis[[i+4]][[1]],2), round(pes.with.cis[[i+5]][[1]],2)),
                    `[95% CI]` = c(paste('[', toString(round(pes.with.cis[[i]][[2]],2)), ' - ', toString(round(pes.with.cis[[i]][[3]],2)), ']', sig1.3[1], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+1]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+1]][[3]],2)), ']', sig2.3[2], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+2]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+2]][[3]],2)), ']', sig2.3[3], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+3]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+3]][[3]],2)), ']', sig2.3[4], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+4]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+4]][[3]],2)), ']', sig2.3[5], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+5]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+5]][[3]],2)), ']', sig2.3[6], sep ='')),
                    row.names = c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', '4: Difference 2 and 1 (Naive apporach)',
                                  '5: Difference 3 and 1 (Segmented approach)', '6: Difference 5 and 4 (Approach difference)'),
                    check.names = FALSE)

i = 1
sig3.3 <- c()
while (i <= 6) {
    if (i <= 3) {
        sig3.3 <- c(sig3.3, '')
    } else { if (between(0, pes.with.cis[[i+12+3]][[2]], pes.with.cis[[i+12+3]][[3]]) == FALSE) {
        sig3.3 <- c(sig3.3, '*')
    } else { sig3.3 <- c(sig3.3, '')}
    }
    i = i+1
}
i = 13    
df3.3 <- data.frame(`Point estimate` = c(round(pes.with.cis[[i]][[1]],2), round(pes.with.cis[[i+1]][[1]],2), round(pes.with.cis[[i+2]][[1]],2), round(pes.with.cis[[i+3]][[1]],2),
                                         round(pes.with.cis[[i+4]][[1]],2), round(pes.with.cis[[i+5]][[1]],2)),
                    `[95% CI]` = c(paste('[', toString(round(pes.with.cis[[i]][[2]],2)), ' - ', toString(round(pes.with.cis[[i]][[3]],2)), ']', sig3.3[1], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+1]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+1]][[3]],2)), ']', sig3.3[2], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+2]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+2]][[3]],2)), ']', sig3.3[3], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+3]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+3]][[3]],2)), ']', sig3.3[4], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+4]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+4]][[3]],2)), ']', sig3.3[5], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+5]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+5]][[3]],2)), ']', sig3.3[6], sep ='')),
                    row.names = c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', '4: Difference 2 and 1 (Naive apporach)',
                                  '5: Difference 3 and 1 (Segmented approach)', '6: Difference 5 and 4 (Approach difference)'),
                    check.names = FALSE)

i = 1
sig4.3 <- c()
while (i <= 6) {
    if (i <= 3) {
        sig4.3 <- c(sig4.3, '')
    } else { if (between(0, pes.with.cis[[i+18]][[2]], pes.with.cis[[i+18]][[3]]) == FALSE) {
        sig4.3 <- c(sig4.3, '*')
    } else { sig4.3 <- c(sig4.3, '')}
    }
    i = i+1
}
i = 19   
df4.3 <- data.frame(`Point estimate` = c(round(pes.with.cis[[i]][[1]],2), round(pes.with.cis[[i+1]][[1]],2), round(pes.with.cis[[i+2]][[1]],2), round(pes.with.cis[[i+3]][[1]],2),
                                         round(pes.with.cis[[i+4]][[1]],2), round(pes.with.cis[[i+5]][[1]],2)),
                    `[95% CI]` = c(paste('[', toString(round(pes.with.cis[[i]][[2]],2)), ' - ', toString(round(pes.with.cis[[i]][[3]],2)), ']', sig4.3[1], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+1]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+1]][[3]],2)), ']', sig4.3[2], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+2]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+2]][[3]],2)), ']', sig4.3[3], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+3]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+3]][[3]],2)), ']', sig4.3[4], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+4]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+4]][[3]],2)), ']', sig4.3[5], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+5]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+5]][[3]],2)), ']', sig4.3[6], sep ='')),
                    row.names = c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', '4: Difference 2 and 1 (Naive apporach)',
                                  '5: Difference 3 and 1 (Segmented approach)', '6: Difference 5 and 4 (Approach difference)'),
                    check.names = FALSE)

i = 1
sig5.3 <- c()
while (i <= 6) {
    if (i <= 3) {
        sig5.3 <- c(sig5.3, '')
    } else { if (between(0, pes.with.cis[[i+24]][[2]], pes.with.cis[[i+24]][[3]]) == FALSE) {
        sig5.3 <- c(sig5.3, '*')
    } else { sig5.3 <- c(sig5.3, '')}
    }
    i = i+1
}
i = 25    
df5.3 <- data.frame(`Point estimate` = c(round(pes.with.cis[[i]][[1]],2), round(pes.with.cis[[i+1]][[1]],2), round(pes.with.cis[[i+2]][[1]],2), round(pes.with.cis[[i+3]][[1]],2),
                                         round(pes.with.cis[[i+4]][[1]],2), round(pes.with.cis[[i+5]][[1]],2)),
                    `[95% CI]` = c(paste('[', toString(round(pes.with.cis[[i]][[2]],2)), ' - ', toString(round(pes.with.cis[[i]][[3]],2)), ']', sig5.3[1], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+1]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+1]][[3]],2)), ']', sig5.3[2], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+2]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+2]][[3]],2)), ']', sig5.3[3], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+3]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+3]][[3]],2)), ']', sig5.3[4], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+4]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+4]][[3]],2)), ']', sig5.3[5], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+5]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+5]][[3]],2)), ']', sig5.3[6], sep ='')),
                    row.names = c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', '4: Difference 2 and 1 (Naive apporach)',
                                  '5: Difference 3 and 1 (Segmented approach)', '6: Difference 5 and 4 (Approach difference)'),
                    check.names = FALSE)

i = 1
sig6.3 <- c()
while (i <= 6) {
    if (i <= 3) {
        sig6.3 <- c(sig6.3, '')
    } else { if (between(0, pes.with.cis[[i+30]][[2]], pes.with.cis[[i+30]][[3]]) == FALSE) {
        sig6.3 <- c(sig6.3, '*')
    } else { sig6.3 <- c(sig6.3, '')}
    }
    i = i+1
}
i = 31    
df6.3 <- data.frame(`Point estimate` = c(round(pes.with.cis[[i]][[1]],2), round(pes.with.cis[[i+1]][[1]],2), round(pes.with.cis[[i+2]][[1]],2), round(pes.with.cis[[i+3]][[1]],2),
                                         round(pes.with.cis[[i+4]][[1]],2), round(pes.with.cis[[i+5]][[1]],2)),
                    `[95% CI]` = c(paste('[', toString(round(pes.with.cis[[i]][[2]],2)), ' - ', toString(round(pes.with.cis[[i]][[3]],2)), ']', sig6.3[1], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+1]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+1]][[3]],2)), ']', sig6.3[2], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+2]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+2]][[3]],2)), ']', sig6.3[3], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+3]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+3]][[3]],2)), ']', sig6.3[4], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+4]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+4]][[3]],2)), ']', sig6.3[5], sep =''),
                                   paste('[', toString(round(pes.with.cis[[i+5]][[2]],2)), ' - ', toString(round(pes.with.cis[[i+5]][[3]],2)), ']', sig6.3[6], sep ='')),
                    row.names = c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', '4: Difference 2 and 1 (Naive apporach)',
                                  '5: Difference 3 and 1 (Segmented approach)', '6: Difference 5 and 4 (Approach difference)'),
                    check.names = FALSE)
                                   

library(knitr)
kable(df1.3, align = 'cc', caption = paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '))
kable(df2.3, align = 'cc', caption = paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '))
kable(df3.3, align = 'cc',caption = paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '))
kable(df4.3, align = 'cc', caption = paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '))
kable(df5.3, align = 'cc', caption = paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '))
kable(df6.3, align = 'cc', caption = paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to '))


