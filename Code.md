```{r, echo = FALSE}
knitr::opts_chunk$set(comment = NA)
library(rio)
library(dplyr)
library(MASS)
library(tableone)
library(survival)
library(dplyr)
library(boot)
library(ggplot2)
library(knitr)
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

#function for plotting jitterplot+errorbars, n = (1,2,3,4,5,6), each number for specific accuracy (dev, tval, pval, tval-dev, pval-dev, diffdiff)
jitterplot <- function(n) {
    i = 0
    trans <- c()
    values <- c()
    cislb <- c()
    cisub <- c()
    ppe <- c()
    signi <-c()
    while (i <= 5) {
        #all transfer names in a list in order to label all values with correct transfers
        trans <- c(trans, rep(colnames(simulation.data)[n+i*6], n.simulations))
        
        #all specific point estimate for certain accuracy in 1 list
        values <- c(values, simulation.data[,n+i*6])
        
        #confidence intervals to list in order to call easier in ggplot
        cislb <- c(cislb, rep(cis[n+i*6], n.simulations))
        cisub <- c(cisub, rep(cis[n+36+i*6], n.simulations))
        
        #all point estimates means added in list in order to call easier in ggplot
        ppe <- c(ppe, rep(pe[[n+i*6]], n.simulations))
        
        #assigns significance in list
        signi <- c(signi, rep(between(0, cis[n+i*6], cis[n+36+i*6]), n.simulations))
        i = i+1
    }
    
    # creating dataframe with all our important values needed to create plots
    tempdf <- data.frame('Transfers' = trans,
                         'Values' =values,
                         'Significance' = signi)
    
    #converting boolean TRUE/FALSE to axtris or not for significance
    tempdf$Significance[tempdf$Significance == FALSE] <- '*'
    tempdf$Significance[tempdf$Significance == TRUE] <- ''
    
    #new xlabels and y labes
    newxlabels <- c('France to USA', 'Switzerland to USA', 'USA to France', 'Switzerland to France', 'USA to Switzerland', 'France to Switzerland')
    newylabels <- c('Development sample accuracy', 
                    'True validation accuracy', 
                    'Predicted validation accuracy', 
                    'Naive approach absolute difference',
                    'Segmented approach absolute difference',
                    'Difference between naive and segmented approach')
    
    #ploting jitterplot + errorbars + mean pointestimate and significance axtris
    p <- ggplot() +
        geom_jitter(data = tempdf, aes(x = Transfers, y = Values), alpha = 0.09, position=position_jitter(0.2)) +
        geom_errorbar(data = tempdf, aes(x = Transfers, y = Values, ymin = cislb, ymax = cisub), size = 0.1, width=0.5, color = "red") +
        geom_point(data = tempdf, aes(x = Transfers, y = ppe), color= "red") +
        scale_x_discrete(breaks=c(unique(tempdf$Transfers)), 
                         labels= newxlabels) +
        ylab(newylabels[n]) +
        coord_flip() +
        theme_test() 
        if (n >= 4) {
            p <- p + geom_hline(yintercept = 0, alpha = 0.4)
            p <- p + geom_text(data = tempdf, aes(x = Transfers, y = ppe, label = Significance), nudge_x = 0.3, alpha = 0.015)
        }
    #returns our plot
    return(p)
    
}
#plotting jitter plots
jitterplot(1)
jitterplot(2)
jitterplot(3)
jitterplot(4)
jitterplot(5)
jitterplot(6)


#function for returning tableplots
dftable <- function(n) {
    #assigning trans as rownames
    trans <- c('1: Accuracy in development sample', '2: Accuracy in validation sample', '3: Accuracy in segmented sample', '4: Difference 2 and 1 (Naive apporach)',
                   '5: Difference 3 and 1 (Segmented approach)', '6: Difference 5 and 4 (Approach difference)')
    i = 1
    pem <- c()
    cislb <- c()
    cisub <- c()
    signi <- c()
    stringci <- c()
    while (i <= 6) {
        #getting pointestiamte means
        pem <- c(pem, round(pe[[(n-1)*6+i]], 2))
        
        #getting confidence intervals
        cislb <- c(cislb, round(cis[[(n-1)*6+i]],2))
        cisub <- c(cisub, round(cis[[(n-1)*6+36+i]],2))
        
        #assigning if significant
        if (i >= 4) {
            signi <- c(signi, between(0, cislb[i], cisub[i]))
        } else { signi <- c(signi, '')}
        
        #stringing together a good looking CI that I can use in dataframe
        stringci <- c(stringci, paste('[', toString(cislb[i]),' - ', toString(cisub[i]), ']',toString(signi[i]), sep = ''))
        i = i+1
    }
    #changing FALSE/TRUE to axtris or not
    stringci <- gsub('FALSE', '*', stringci)
    stringci <- gsub('TRUE', '', stringci)
    
    #assigning table with specific column names
    tablep <- data.frame('Transfers' = trans,
                        'Point estimate mean' = pem,
                        '95% CI' = stringci,
                        check.names = FALSE)
    
    #returning table i just created
    return(tablep)
}
#ploting different tables with knitr
kable(dftable(1), align = 'lcc', caption = paste(strata.combinations$`2`[[1]], strata.combinations$`2`[2], sep = ' to '))
kable(dftable(2), align = 'lcc', caption = paste(strata.combinations$`3`[[1]], strata.combinations$`3`[2], sep = ' to '))
kable(dftable(3), align = 'lcc',caption = paste(strata.combinations$`4`[[1]], strata.combinations$`4`[2], sep = ' to '))
kable(dftable(4), align = 'lcc', caption = paste(strata.combinations$`6`[[1]], strata.combinations$`6`[2], sep = ' to '))
kable(dftable(5), align = 'lcc', caption = paste(strata.combinations$`7`[[1]], strata.combinations$`7`[2], sep = ' to '))
kable(dftable(6), align = 'lcc', caption = paste(strata.combinations$`8`[[1]], strata.combinations$`8`[2], sep = ' to '))
```


