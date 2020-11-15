---
title: A Segmented Approach to Predicting Prediction Model Performance After Transfer Using Unlabelled Data
author: Arman Norouzi
bibliography: cite.bib
csl: bmcemerg.csl
---

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
library(caret)
url <- "https://datadryad.org/stash/downloads/file_stream/30857"
raw.data <- import(url, format = "xls") %>% as.data.frame()


#------------------------------------------
pctabledf <- data.frame(raw.data)

listk <- c(2, 3, 5, 7, 8, 10, 11, 16, 17)
listc <- c()
for (i in listk) {
    listc <- c(listc, colnames(pctabledf[i]))
}

pctabledf <- subset(pctabledf, select = listc)
colnames(pctabledf) <- c('Country', 'Respiratory rate (per min)', 'Gender', 'Peripheral oxygen saturation (%)', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)', 'Temperature (°C)', 'Age', 'ICU admission')

pctabledf$`ICU admission`[pctabledf$`ICU admission` == 1] <- 'Admission'
pctabledf$`ICU admission`[pctabledf$`ICU admission` == 0] <- 'No admission'

pctabledf$Gender[pctabledf$Gender == 'f'] <- 'Female'
pctabledf$Gender[pctabledf$Gender == 'm'] <- 'Male'

strata <- "Country"
vars <- colnames(pctabledf)[!(colnames(pctabledf) %in% strata)]

listk <- c(2,3,4,6,7)
listc <- c()
for (i in listk) {
    listc <- c(listc, colnames(pctabledf)[i])
}
biomarkers <- c(listc)

Pctable <- CreateTableOne(vars = vars, data = pctabledf, strata = strata, test = FALSE, addOverall = TRUE)

#------------------------------------------


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

    devsample$`ICU admission`[devsample$`ICU admission` == 1] <- 'Admission'
    devsample$`ICU admission`[devsample$`ICU admission` == 0] <- 'No admission'
    
    valsample$`ICU admission`[valsample$`ICU admission` == 1] <- 'Admission'
    valsample$`ICU admission`[valsample$`ICU admission` == 0] <- 'No admission'
    
    #Adjusting for overfitting with oneSE caret package
    train.control <- trainControl(method = 'cv', number = 5, selectionFunction = 'oneSE')

    #logreg is now our prediction model that is chosen through cross-validation 80-20 ?
    logreg <- train(`ICU admission` ~ `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` + `Respiratory rate (per min)`, 
                    data = devsample, method = 'glm', trControl = train.control)
    
    # x calcualted as correct prediction in the development sample
    probabilities <- predict(logreg, newdata = devsample, type = 'prob')$Admission
    predict.classesdev <- ifelse(probabilities > 0.5, 'Admission', 'No admission')

    x <- mean(predict.classesdev == devsample$`ICU admission`) * 100
    
    
    # y calculated as the correct predictions in the validation sample
    probabilities <-predict(logreg, newdata = valsample, type = 'prob')$Admission
    predict.classesval <- ifelse(probabilities > 0.5, 'Admission', 'No admission')
    
    y <- mean(predict.classesval == valsample$`ICU admission`) * 100

    ## Assign data "origin" as new variable and combine data
    devsample['devval'] <- 1
    valsample['devval'] <- 0

    df_pooled <- rbind(devsample, valsample)

    ## Create propensity model
    logregi <- glm(`devval` ~ `Age` + `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` + `Respiratory rate (per min)`, 
                   data = df_pooled, family = binomial)
    probabilities <- logregi %>% predict(df_pooled, type = "response")
    predict.classespool <- ifelse(probabilities > 0.5, 1, 0)

    ## Identify the segment of observation in the development data that
    ## are "most similar" to the observations in the validation data
    missmatch <- predict.classespool == 0 & df_pooled$devval == 1
    df_segment <- devsample[missmatch, ]

    # z calculated as the correct predictions in the segment created
    df_segment$`ICU admission`[df_segment$`ICU admission` == 1] <- 'Admission'
    df_segment$`ICU admission`[df_segment$`ICU admission` == 0] <- 'No admission'
    
    probabilities <- predict(logreg, newdata = df_segment, type = 'prob')$Admission
    predict.classessegment <- ifelse(probabilities > 0.5, 'Admission', 'No admission')
    
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
listdf <- c()

## This function runs one simulation, and returns all accuracys and differences along with the dataframe simulated
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
    strata <- "Country"
    vars <- colnames(df)[!(colnames(df) %in% strata)]
    df$`ICU admission` <- as.numeric(df$`ICU admission`)
    df_USA <- df[df$Country == "USA", ]
    df_France <- df[df$Country == "France", ]
    df_Swizerland <- df[df$Country == "Switzerland", ]
    performance.estimates <- lapply(strata.combinations, estimate_performance, strata = strata, df = df)
    names(performance.estimates) <- sapply(strata.combinations, paste0, collapse = ".to.")
    results <- unlist(performance.estimates)
    results <- c(results, data.frame(df))
    return (results)

}


## Okay, so instead of creating one simulated dataset and bootstrapping
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
n.simulations <- 10
simulation.results <- lapply(seq_len(n.simulations), function(i) run_simulation(
                                                                     raw.data = raw.data,
                                                                     strata = strata,
                                                                     strata.combinations = strata.combinations,
                                                                     outcome = outcome,
                                                                     predictors = predictors,
                                                                     size = size))

# This gives us the dataframe for each of the simulations and the accuracys in (simulation.restults1 and list.simulated.dfs) 
list.simulated.dfs <- data.frame()
simulation.results1 <- c()
for (i in 1:n.simulations) {
    simulation.results1 <- c(simulation.results1, list(unlist(simulation.results[[i]][1:36])))
    list.simulated.dfs <- rbind(list.simulated.dfs, simulation.results[[i]][37:44] %>% as.data.frame())
}

#accuracys as we did before
simulation.results <- simulation.results1
simulation.data <- do.call(rbind, simulation.results)
pe <- colMeans(simulation.data)
cis <- lapply(as.data.frame(simulation.data), function(x) {
    quantiles <- quantile(x, probs = c(0.025, 0.975))
    names(quantiles) <- NULL
    c(lb = quantiles[1], ub = quantiles[2])
}) %>% as.data.frame() %>% t()
pes.with.cis <- cbind(pe, cis) %>% as.data.frame() %>% split(f = as.factor(rownames(cis)))

# Creating a tableone for all simulated dataframes
colnames(list.simulated.dfs) <- c('Country', 'Age', 'Respiratory rate (per min)', 'Peripheral oxygen saturation (%)', 
                                  'Systolic blood pressure (mm Hg)', 'Pulse (bpm)', 'Temperature (°C)', 'ICU admission')

list.simulated.dfs$`ICU admission`[list.simulated.dfs$`ICU admission` == 1] <- 'Admission'
list.simulated.dfs$`ICU admission`[list.simulated.dfs$`ICU admission` == 0] <- 'No admission'

strata <- "Country"
vars <- colnames(list.simulated.dfs)[!(colnames(list.simulated.dfs) %in% strata)]

Pctablesim <- CreateTableOne(vars = vars, data = list.simulated.dfs, strata = strata, test = FALSE, addOverall = TRUE)

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
    newylabels <- c('Development sample performance (%)', 
                    'True validation performance (%)', 
                    'Predicted validation performance (%)', 
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
        if (n >= 6) {
            p <- p + geom_hline(yintercept = 0, alpha = 0.4)
            p <- p + geom_text(data = tempdf, aes(x = Transfers, y = ppe, label = Significance), nudge_x = 0.3, alpha = 0.015)
        }
    #returns our plot
    return(p)
    
}

#function for returning tableplots
dftable <- function(n) {
    #assigning trans as rownames
    trans <- c('1: Performance in development sample', 
               '2: Performance in validation sample', 
               '3: Performance in segmented sample', 
               '4: Absolute difference 2 and 1 (Naive apporach)',
               '5: Absolute difference 3 and 1 (Segmented approach)', 
               '6: Difference 5 and 4 (Approach difference)')
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
        if (i >= 6) {
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
    tablep <- data.frame('Performances' = trans,
                        'Point estimate mean' = pem,
                        '95% CI' = stringci,
                        check.names = FALSE)
    
    #returning table i just created
    return(tablep)
}
#ploting different tables with knitr

#kable(dftable(1), align = 'lcc', caption = paste(strata.combinations[[1]][1], strata.combinations[[1]][2], sep = ' to '))
#kable(dftable(2), align = 'lcc', caption = paste(strata.combinations[[2]][1], strata.combinations[[2]][2], sep = ' to '))
#kable(dftable(3), align = 'lcc',caption = paste(strata.combinations[[3]][1], strata.combinations[[3]][2], sep = ' to '))
#kable(dftable(4), align = 'lcc', caption = paste(strata.combinations[[4]][1], strata.combinations[[4]][2], sep = ' to '))
#kable(dftable(5), align = 'lcc', caption = paste(strata.combinations[[5]][1], strata.combinations[[5]][2], sep = ' to '))
#kable(dftable(6), align = 'lcc', caption = paste(strata.combinations[[6]][1], strata.combinations[[6]][2], sep = ' to '))


#kableone(Pctable)
```
# Introduction
In medicine, healthcare professionals are daily confronted with a wide range of information that needs to be processed in order to make informed clinical decisions. To help healthcare professionals make such informed decisions, prediction models (also referred to as prediction score or prediction rules) have been implemented in healthcare [@steyerberg2013prognosis] [@moons2009prognosis]. These prediction models can be defined as statistical algorithms that predict the risk of a specific outcome in an individual [@moons2009prognosis] [@collins2015transparent]. The prediction models are capable of this task due to being trained in finding patterns in predictor data that has been labelled with outcome. These patterns can then be used in order to predict outcome based on new unlabelled predictor data [@deo2015machine].

The risk that is predicted by the prediction models is generally based on multiple predictors and the outcome could either be a disease (diagnostic model) or an event that will occur in the future (prognostic model) [@collins2015transparent] [@riley2013prognosis]. In the diagnostic model, the predicted risk can be used to reassure the patient that their symptoms are not caused by a serious disease, refer the patient to further testing or to initiate treatment [@collins2015transparent]. A great example of a diagnostic model is the Ottawa Ankle Rules. This prediction model helps healthcare professionals by predicting the risk of a fracture in the ankle or the foot, in patients with acute ankle injuries. To predict this risk, the prediction model utilizes predictor data such as bone tenderness at different locations and the inability to bear weight on the injured foot immediately after injury and in the emergency department (ED). Based on the predicted risk, the healthcare professionals can decide whether the patient is in need of x-ray imaging [@shell1993decision].

In the prognostic model, the predicted risk can be used to choose between therapeutic options, plan lifestyle changes and to risk-stratify patients in therapeutic clinical trials [@moons2009prognosis] [@steyerberg2009practical] [@dorresteijn2011estimating] [@hayward2006multivariable]. A great example of a prognostic model is the CHA2DS2-VASc score. This prediction model helps healthcare professionals by predicting the annual risk of developing an ischemic stroke in patients with atrial fibrillation. To predict this risk, the prediction model utilizes predictor data such as history for congestive heart failure, hypertension, age >74, diabetes, stroke/transiet ischemic attack/thromboembolism, vascular disease, age 65-74 and female sex [@lip2010refining]. Based on the predicted risk, the healthcare professionals can decide whether the patient is in need for anticoagulation treatment [@kirchhof20162016].

There are many uses for prediction models within the fields of medicine, where the Ottawa Ankle Rules and the CHA2DS-VASc score are just a few examples that have been implemented in clinical practice. In order to develop and implement such useful models within healthcare, a few steps are needed to be carried out. These steps include model development studies, model validation studies and model impact studies [@royston2009prognosis] [@moons2012risk].

In the first step consisting of the model development study, the aim is to develop a prediction model [@royston2009prognosis]. The prediction model is developed by selecting a statistical algorithm and a development sample. There are a couple of algorithms to choose from, but usually when the development sample is limited, a simpler algorithm is utilized such as logistic regression [@deo2015machine]. The development sample consists of relevant outcome labelled predictor data, which is used to train the algorithm in finding patterns between the outcomes and the predictors [@deo2015machine] [@royston2009prognosis]. When the algorithm has been trained, it usually tends to be optimistic in its predictive performance when predicting outcome in the development sample [@steyerberg2003internal]. It is therefore important to quantify such optimism through internal validation techniques [@steyerberg2009practical]. The quantified optimism can thereafter be adjusted for by applying shrinkage or penalization to the model [@steyerberg2001application].

In the second step consisting of the model validation study, the aim is to assess the predictive performance of the prediction model within a validation sample. The validation sample consists of new individuals, with outcome labelled predictor data, that differ in various ways from the individuals in the development sample. These individuals may differ in the time in which their data were collected (temporal validation) or from which country or hospital their data were collected (geographical validation). The latter validation technique assesses the transportability of the prediction model. Such external validation is important to perform, due to internally validated prediction models tendency to perform worse in new sets of individuals. If the performance is poor within the validation sample, the model is of no value [@moons2012risk].

In the third and final step consisting of the model impact study, the aim is to assess the prediction models impact, ideally in a randomised trial. The impact of the model is assessed in variables such as decision making changes in healthcare professionals, patient health outcomes and/or cost-effectiveness of care. These impact studies are carried out to prove that the prediction model is of value in clinical practice [@moons2012risk].

Carrying out these prediction model studies can be complex as problems may arise during the time in which they are carried out. One such problem may occur during the model validation study. In this study, in order to obtain the predictive performance of the prediction model within the validation sample, available predictor and outcome data is needed from that sample [@moons2012risk]. This data is not always available retrospectively and can present a problem that is both time inefficient and expensive, if the data is difficult to access when collecting it prospectively. 

This specific problem would present itself if data for the Framingham Risk Score were to be collected prospectively, in order to perform a model validation study. The predictor data in this prediction model are cheap blood samples and simple demographics while the outcome data is cardiovascular disease within 10 years [@anderson1991cardiovascular]. The predictor data for this prediction model may be easily accessible. But the outcome data is only accessible after 10 years of follow up. Due to the difficulties in collecting the outcome data prospectively, this specific problem would be presented.

It would therefore be desirable to have a method that can estimate the predictive performance of a prediction model when it is transferred by only utilizing unlabelled predictor data from the validation sample. Such a method could in theory, simplify the process of implementing prediction models in clinical practice and therefore indirectly improve decision making changes in healthcare professionals, patient health outcomes and/or cost-effectiveness of care. At present, no such methods exist which presents a substantial knowledge gap. Therefore, the aim of this study was to develop and test a new method for predicting prediction model performance after transfer while using unlabelled data.

# Aim
The aim of this study was to develop and test a new method for predicting prediction model performance after transfer using unlabelled data. 

The hypothesis was that the new method, which uses a segmented approach, will have a performance that is higher or as good as current naive ways of assessing the predictive performance within the validation sample. This hypothesis was based on the fact that the prediction model, which is developed with the development sample, is the one predicting the predictive performance within the validation sample by predicting outcome in the segmented sample. This segmented sample consists of selected data from the development sample which should in theory lead to an optimistic performance.

# Methods and Materials
## Study design
The study design was an analytical registry based study. To perform the analysis, a dataset was used that has been made freely reusable by the multinational observational study by Eckert A et al in the Dryad Digital Repository [@eckart2019combination] (18). This dataset was chosen due to consisting of patient data from different countries with available patient parameters that can be linked to a patient outcome.

## Participants
The participants enrolled in the dataset were all patients seeking ED care between March 2013 and October 2014 within three tertiary care centers in the USA (Clearwater Hospital), France (Hôspital de la Salpêtrière) and Switzerland (Kontonsspital Aaura). The data that was registered for each participant included the hospital and country that the patient seeked, vital signs, laboratory assessments, age, discharge location, length of stay, intensive care unit (ICU) admission and death within 30 days. The inclusion criteria to be enrolled in the dataset was that an initial blood sample was taken. The exclusion criteria were paediatric or surgical patient [@eckart2019combination].

## Variables
### Model predictors
The model predictors that were used to develop prediction models and simulate new data in the statistical analysis included respiratory rate (per min), peripheral oxygen saturation (%), systolic blood pressure (mm Hg), heart rate (beats per min), temperature (°C) and age. How these variables were measured was not mentioned in the original study that publicized them. These variables were chosen at random.

### Model outcomes
The model outcomes that were used to develop prediction models in the statistical analysis included ICU admission and the country from which the patient seeked ED care. ICU admission was chosen as the outcome for the prediction model due being more frequent than death within 30 days.

### Sample size
The final sample size used in this study was 1303 participants which included all of the participants from the dataset.

### Missing data
Because of the dataset already being filtered to mostly containing no missing data, a complete case analysis was carried out.

## Statistical analysis
### Dataset
The dataset previously mentioned in the study design was divided based on the country from which the participants seeked ED care which resulted in three different samples (USA sample, France sample and Switzerland sample)

### Sequence of analysis
Analysis in this study was performed in the programming language R (19). The sequence of analysis performed were sample simulation, sample assignment, prediction model development, development sample performance, true validation sample performance, propensity model development, predicted validation sample performance and approach comparison.

### Sample simulation
To increase the number of participants in the samples, 10000 new participants were simulated for each of the divided samples. The process of simulation included a model predictor simulation and a model outcome simulation. The model predictors that were used to simulate new model predictors included respiratory rate, peripheral oxygen saturation, systolic blood pressure, heart rate, temperature and age from the divided samples. The model outcome that was used to simulate new model outcomes included ICU admission from the divided samples.

To simulate new model predictors for one of the divided samples, the mvrnorm function implemented in the MASS package was used (20). The function utilized the mean and the covariance of the model predictors in the divided sample to simulate new model predictors. To simulate new model outcomes for the simulated model predictors, the glm function implemented in R was used to develop a logistic regression model (21). This model was trained with the model predictors and model outcome from the divided sample. The model was then used to predict outcome based on the simulated model predictors. These predictions were set as the outcome for the simulated model predictors in order to create simulated samples. This simulation process was performed for each of the divided samples.

### Sample assignment
To simulate the transfer of a prediction model from one country to another, one of the simulated samples, that including simulated model predictors and model outcomes, was noted as the development sample while one of the two remaining simulated samples was noted as the validation sample. The development sample represented data from the country in which the prediction model was created, while the validation sample represented data from the country in which the prediction model was transferred to.

### Prediction model development
In the prediction model development step, a prediction model was developed by training a logistic regression model with the development sample. To adjust for optimism, cross-validation techniques were used to chose the simplest model within one standard error of the best model. The cross-validation technique was based on a 80% training and 20% testing. The model predictors that were used to train the model included respiratory rate, peripheral oxygen saturation, systolic blood pressure, heart rate, temperature and age. The model outcome that was used to train the model included ICU admission.

### Development sample performance
To assess the predictive performance of the prediction model within the country it was developed, the model developed in the prediction model development step was used to predict outcome within the development sample.

### True validation sample performance
To assess the true performance of the prediction model within the country it was transferred to, the model developed in the prediction model development step was used to predict outcome within the validation sample.

### Propensity model development
In the propensity model development step, the data from the development sample and the validation sample were pooled into one sample. This aggregated sample was then used to develop a propensity model, also a prediction model, using logistic regression. The propensity model was then used to predict from which sample the data in the aggregated sample originated from. Observations from the development sample that were missclassified as validation observations, were used to create a segmented sample. The model predictors that were used to train the propensity model included respiratory rate, peripheral oxygen saturation, systolic blood pressure, heart rate, temperature and age. The model outcome that was used to train the propensity model was the country in which the participant seeked ED care.

### Predicted validation sample performance
To assess the predicted performance of the prediction model within the country it was transferred to, the model developed in the prediction model development step was used to predict outcome within the segmented sample.

### Approach comparison
To assess the performance of the naive approach, the absolute difference between the development sample performance step and the true validation sample performance step was calculated. To assess the performance of the segmented approach, the absolute difference between the predicted validation performance step and the development sample performance step was calculated. To assess which approach performed best, the difference between the naive approach and the segmented approach was calculated.

### Sequence repetition
To obtain 95% confidence intervals (CI) around the performances and the differences, the sequence of analysis will be repeated 1000 times. These repetitions will be performed for each available combination in the sample assignment step assignment step.

## Ethical considerations
### Principle of autonomy
The dataset that was used in this study has been made freely reusable in Dryad Digital Repository (18). Therefore, the principle of autonomy is upheld due to there not being any requirement for informed consent.

### Principle of beneficence
This study attempted to act in the best interest of future analytical research and patients, by developing and testing a new method for predicting prediction model performance after transfer using unlabelled data. Such a method could in theory, simplify the process of implementing prediction models in clinical practice and therefore indirectly improve decision making changes in healthcare professionals, patient health outcomes and/or cost-effectiveness of care.

### Principle of nonmaleficence
The method developed in this study will be made without the intention of harm, intentionally or unintentionally. To nullify the risk of patient identification leakage, we used a dataset that has already been depersonalized and made freely reusable. By taking these actions we determine that the risk to the population was minimal.

### Principle of justice
Due to this study being analytical, the principle of justice does not prevail. However, the data in the study will be treated equally.

### Ethical permit
Because of this study being analytical and based on a freely reusable and public database, the need for an ethical permit was not required.

# Results
## Original Sample description
All 1303 participants (`r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,1][[1]]` `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,2][[1]]`, `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,1][[2]]` `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,2][[2]]`, `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,1][[3]]` `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,2][[3]]`) in the original sample had complete data and were used to simulate new participants. The mean age of the participants in the original sample was `r round(mean(raw.data$age),2)` years and `r round(table(raw.data$gender)[[2]] / length(raw.data$gender) * 100, 2)` % were male. Baseline characteristics of the original sample stratified by country are shown in table 1.

```{r, echo = FALSE}
kableone(Pctable, caption = 'Baseline characteristics of the original sample used in the final analysis stratified by country.')
```

## Simulation sample description
With the 1303 participants in the original sample, `r length(list.simulated.dfs$Country) / 3` new participants were simulated for each country. The mean age of the participants in the simulated sample was `r round(mean(list.simulated.dfs$Age),2)` years. Baseline characteristics of the simulated sample stratified by country are shown in table 2.

```{r, echo = FALSE}
kableone(Pctablesim, caption = 'Baseline characteristics of the simulated sample used in the final analysis stratified by country.')
```

## Development sample performance
When assessing the predictive performance within the development sample for each transfer combination, we found the mean predictive performance to be 
`r dftable(1)[[2]][1]` % (95% CI `r round(cis[1],2)` to `r round(cis[1+36],2)`) within the `r strata.combinations[[1]][1]` to `r strata.combinations[[1]][2]` transfer, `r dftable(2)[[2]][1]` % (95% CI `r round(cis[1+6],2)` to `r round(cis[1+36+6],2)`) within the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer, `r dftable(3)[[2]][1]` % (95% CI `r round(cis[1+12],2)` to `r round(cis[1+36+12],2)`) within the `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]` transfer, `r dftable(4)[[2]][1]` % (95% CI `r round(cis[1+18],2)` to `r round(cis[1+36+18],2)`) within the `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]` transfer, `r dftable(5)[[2]][1]` % (95% CI `r round(cis[1+24],2)` to `r round(cis[1+36+24],2)`) within the `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` transfer, `r dftable(6)[[2]][1]` % (95% CI `r round(cis[1+30],2)` to `r round(cis[1+36+30],2)`) within the `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]` transfer. All development sample performances, mean performances and CI for each transfer combination are shown in figure 1.

```{r, echo = FALSE, fig.cap = 'Development sample performances as percantage for transfer combinations. Each black dot represents the development sample performance in one simulated sample with the red dot representing the mean performance across all simulations. The bars indicate the 95% CI'}
jitterplot(1)

#ordning i dftable
#dftable(1)
#dftable(1)[[2]][1]

#strata.combinations[[1]][1]
#strata.combinations[[1]][2]

#dftable(2)
#strata.combinations[[2]][1]
#strata.combinations[[2]][2]

#dftable(3)
#strata.combinations[[3]][1]
#strata.combinations[[3]][2]

#dftable(4)
#strata.combinations[[4]][1]
#strata.combinations[[4]][2]

#dftable(5)
#strata.combinations[[5]][1]
#strata.combinations[[5]][2]

#dftable(6)
#strata.combinations[[6]][1]
#strata.combinations[[6]][2]

#ordningen i jitterplot
#strata.combinations[[5]][1]
#strata.combinations[[5]][2]

#strata.combinations[[3]][1]
#strata.combinations[[3]][2]

#strata.combinations[[2]][1]
#strata.combinations[[2]][2]

#strata.combinations[[4]][1]
#strata.combinations[[4]][2]

#strata.combinations[[1]][1]
#strata.combinations[[1]][2]

#strata.combinations[[6]][1]
#strata.combinations[[6]][2]

#strata.combinations
```

## True validation sample performance
When assessing the true predictive performance within the validation sample for each transfer combination, we found the mean predictive performance to be 
`r dftable(1)[[2]][2]` % (95% CI `r round(cis[2],2)` to `r round(cis[2+36],2)`) within the `r strata.combinations[[1]][1]` to `r strata.combinations[[1]][2]` transfer, `r dftable(2)[[2]][2]` % (95% CI `r round(cis[2+6],2)` to `r round(cis[2+36+6],2)`) within the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer, `r dftable(3)[[2]][2]` % (95% CI `r round(cis[2+12],2)` to `r round(cis[2+36+12],2)`) within the `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]` transfer, `r dftable(4)[[2]][2]` % (95% CI `r round(cis[2+18],2)` to `r round(cis[2+36+18],2)`) within the `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]` transfer, `r dftable(5)[[2]][2]` % (95% CI `r round(cis[2+24],2)` to `r round(cis[2+36+24],2)`) within the `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` transfer, `r dftable(6)[[2]][2]` % (95% CI `r round(cis[2+30],2)` to `r round(cis[2+36+30],2)`) within the `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]` transfer. All true validation sample performances, mean performances and CI for each transfer combination are shown in figure 2.


```{r, echo = FALSE, fig.cap = 'True validation sample performances as percantage for all transfer combinations. Each black dot represents the true validation sample performance in one simulated sample with the red dot representing the mean performance across all simulations. The bars indicate the 95% CI.'}
jitterplot(2)
```

## Predicted validation sample performance
When assessing the predicted predictive performance within the validation sample for each transfer combination, we found the mean predictive performance to be 
`r dftable(1)[[2]][3]` % (95% CI `r round(cis[3],2)` to `r round(cis[3+36],2)`) within the `r strata.combinations[[1]][1]` to `r strata.combinations[[1]][2]` transfer, `r dftable(2)[[2]][3]` % (95% CI `r round(cis[3+6],2)` to `r round(cis[3+36+6],2)`) within the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer, `r dftable(3)[[2]][3]` % (95% CI `r round(cis[3+12],2)` to `r round(cis[3+36+12],2)`) within the `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]` transfer, `r dftable(4)[[2]][3]` % (95% CI `r round(cis[3+18],2)` to `r round(cis[3+36+18],2)`) within the `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]` transfer, `r dftable(5)[[2]][3]` % (95% CI `r round(cis[3+24],2)` to `r round(cis[3+36+24],2)`) within the `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` transfer, `r dftable(6)[[2]][3]` % (95% CI `r round(cis[3+30],2)` to `r round(cis[3+36+30],2)`) within the `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]` transfer. All predicted validation sample performances, mean performances and CI for each transfer combination are shown in figure 3.

```{r, echo = FALSE, fig.cap = 'Predicted validation sample performances as percantage for all transfer combinations. Each black dot represents the predicted validation sample performance in one simulated sample with the red dot representing the mean performance across all simulations. The bars indicate the 95% CI.'}
jitterplot(3)
```

## Naive approach performance
When assessing the naive approach performance for each transfer combination, we found the mean performance difference to be 
`r dftable(1)[[2]][4]` % (95% CI `r round(cis[4],2)` to `r round(cis[4+36],2)`) within the `r strata.combinations[[1]][1]` to `r strata.combinations[[1]][2]` transfer, `r dftable(2)[[2]][4]` (95% CI `r round(cis[4+6],2)` to `r round(cis[4+36+6],2)`) within the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer, `r dftable(3)[[2]][4]` (95% CI `r round(cis[4+12],2)` to `r round(cis[4+36+12],2)`) within the `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]` transfer, `r dftable(4)[[2]][4]` (95% CI `r round(cis[4+18],2)` to `r round(cis[4+36+18],2)`) within the `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]` transfer, `r dftable(5)[[2]][4]` (95% CI `r round(cis[4+24],2)` to `r round(cis[4+36+24],2)`) within the `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` transfer, `r dftable(6)[[2]][4]` (95% CI `r round(cis[4+30],2)` to `r round(cis[4+36+30],2)`) within the `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]` transfer. All naive approach differences, mean differences and CI for each transfer combination are shown in figure 4.

```{r, echo = FALSE, fig.cap = 'Naive approach absolute difference as percantage point for all transfer combinations. Each black dot represents the naive apporach absolute difference in one simulated sample with the red dot representing the mean difference across all simulations. The bars indicate the 95% CI.'}
jitterplot(4)
```

## Segmented approach results
When assessing the segmented approach performance for each transfer combination, we found the mean performance difference to be 
`r dftable(1)[[2]][5]` (95% CI `r round(cis[5],2)` to `r round(cis[5+36],2)`) within the `r strata.combinations[[1]][1]` to `r strata.combinations[[1]][2]` transfer, `r dftable(2)[[2]][5]` (95% CI `r round(cis[5+6],2)` to `r round(cis[5+36+6],2)`) within the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer, `r dftable(3)[[2]][5]` (95% CI `r round(cis[5+12],2)` to `r round(cis[5+36+12],2)`) within the `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]` transfer, `r dftable(4)[[2]][5]` (95% CI `r round(cis[5+18],2)` to `r round(cis[5+36+18],2)`) within the `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]` transfer, `r dftable(5)[[2]][5]` (95% CI `r round(cis[5+24],2)` to `r round(cis[5+36+24],2)`) within the `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` transfer, `r dftable(6)[[2]][5]` (95% CI `r round(cis[5+30],2)` to `r round(cis[5+36+30],2)`) within the `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]` transfer. All segmented approach differences, mean differences and CI for each transfer combination are shown in figure 5.

```{r, echo = FALSE, fig.cap = 'Segmented apporach absolute difference for all transfer combinations. Each black dot represents the segmented approach absolute difference in one simulated sample with the red dot representing the mean difference across all simulations. The bars indicate the 95% CI.'}
jitterplot(5)
```

## Approach comparison
When assessing the performance difference between the naive approach and the segmented approach in the `r strata.combinations[[1]][1]` to `r strata.combinations[[1]][2]` transfer, the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer, the `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]` transfer, the `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` and `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]` transfer, we found significantly higher performance in the segmented approach than the naive approach with a mean difference of `r dftable(1)[[2]][6]` (95% CI `r round(cis[6],2)` to `r round(cis[6+36],2)`), `r dftable(2)[[2]][6]` (95% CI `r round(cis[6+6],2)` to `r round(cis[6+36+6],2)`), `r dftable(4)[[2]][6]` (95% CI `r round(cis[6+18],2)` to `r round(cis[6+36+18],2)`), `r dftable(5)[[2]][6]` (95% CI `r round(cis[6+24],2)` to `r round(cis[6+36+24],2)`), `r dftable(6)[[2]][6]` (95% CI `r round(cis[6+30],2)` to `r round(cis[6+36+30],2)`) respectively. When assessing the performance difference between the naive approach and the segmented approach in the `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]` transfer, we found no significant performance difference between the naive approach and the segmented approach with a mean difference of `r dftable(3)[[2]][6]` (95% CI `r round(cis[6+12],2)` to `r round(cis[6+36+12],2)`). 

```{r, echo = FALSE, fig.cap = 'Approach differences for all transfer combinations. Each black dot represents the difference between the naive and the segmented approach in one simulated sample with the red dot representing the mean approach difference across all simulations. The bars indicate the 95% CI. astrix (*) above the mean approach difference indicates statistical significant difference.'}
jitterplot(6)
```
