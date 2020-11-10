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

#------------------------------------------
pctabledf <- data.frame(raw.data)

listk <- c(2, 3, 7, 8, 10, 11, 16, 17)
listc <- c()
for (i in listk) {
    listc <- c(listc, colnames(pctabledf[i]))
}

pctabledf <- subset(pctabledf, select = listc)
colnames(pctabledf) <- c('Country', 'Respiratory rate (per min)', 'Peripheral oxygen saturation (%)', 'Systolic blood pressure (mm Hg)', 'Pulse (bpm)', 'Temperature (°C)', 'Age', 'ICU admission')

pctabledf$`ICU admission`[pctabledf$`ICU admission` == 1] <- 'Admission'
pctabledf$`ICU admission`[pctabledf$`ICU admission` == 0] <- 'No admission'

strata <- "Country"
vars <- colnames(pctabledf)[!(colnames(pctabledf) %in% strata)]

listk <- c(2,3,4,6,7)
listc <- c()
for (i in listk) {
    listc <- c(listc, colnames(pctabledf)[i])
}
biomarkers <- c(listc)

Pctable <- CreateTableOne(vars = vars, data = pctabledf, strata = strata, test = FALSE)
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

    logreg <- glm(`ICU admission` ~ `Systolic blood pressure (mm Hg)` + `Pulse (bpm)` + `Temperature (°C)` + `Peripheral oxygen saturation (%)` + `Respiratory rate (per min)`, data = devsample, family = binomial)
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
n.simulations <- 10
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

#jitterplot(1)
#jitterplot(2)
#jitterplot(3)
#jitterplot(4)
#jitterplot(5)
#jitterplot(6)


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
In medicine, healthcare professionals are daily confronted with a wide range of information that needs to be processed in order to make informed clinical decisions. To help healthcare professionals make such informed decisions, prediction models (also referred to as prediction score or prediction rules) have been implemented in healthcare (1, 2). These prediction models can be defined as statistical algorithms that predict the risk of a specific outcome in an individual (2, 3). The prediction models are capable of this task due to being trained in finding patterns in predictor data that has been labelled with outcome. These patterns can then be used in order to predict outcome based on new unlabelled predictor data (4).

The risk that is predicted by the prediction models is generally based on multiple predictors and the outcome could either be a disease (diagnostic model) or an event that will occur in the future (prognostic model) (3, 5). In the diagnostic model, the predicted risk can be used to reassure the patient that their symptoms are not caused by a serious disease, refer the patient to further testing or to initiate treatment (3). A great example of a diagnostic model is the Ottawa Ankle Rules. This prediction model helps healthcare professionals by predicting the risk of a fracture in the ankle or the foot, in patients with acute ankle injuries. To predict this risk, the prediction model utilizes predictor data such as bone tenderness at different locations and the inability to bear weight on the injured foot immediately after injury and in the emergency department (ED). Based on the predicted risk, the healthcare professionals can decide whether the patient is in need of x-ray imaging (6).

In the prognostic model, the predicted risk can be used to choose between therapeutic options, plan lifestyle changes and to risk-stratify patients in therapeutic clinical trials (2, 7, 8, 9). A great example of a prognostic model is the CHA2DS2-VASc score. This prediction model helps healthcare professionals by predicting the annual risk of developing an ischemic stroke in patients with atrial fibrillation. To predict this risk, the prediction model utilizes predictor data such as history for congestive heart failure, hypertension, age >74, diabetes, stroke/transiet ischemic attack/thromboembolism, vascular disease, age 65-74 and female sex (10). Based on the predicted risk, the healthcare professionals can decide whether the patient is in need for anticoagulation treatment (11).

There are many uses for prediction models within the fields of medicine, where the Ottawa Ankle Rules and the CHA2DS-VASc score are just a few examples that have been implemented in clinical practice. In order to develop and implement such useful models within healthcare, a few steps are needed to be carried out. These steps include model development studies, model validation studies and model impact studies (12, 13).

In the first step consisting of the model development study, the aim is to develop a prediction model (12). The prediction model is developed by selecting a statistical algorithm and a development sample. There are a couple of algorithms to choose from, but usually when the development sample is limited, a simpler algorithm is utilized such as logistic regression (4). The development sample consists of relevant outcome labelled predictor data, which is used to train the algorithm in finding patterns between the outcomes and the predictors (4, 12). When the algorithm has been trained, it usually tends to be optimistic in its predictive performance when predicting outcome in the development sample (14). It is therefore important to quantify such optimism through internal validation techniques (7). The quantified optimism can thereafter be adjusted for by applying shrinkage or penalization to the model (15).

In the second step consisting of the model validation study, the aim is to assess the predictive performance of the prediction model within a validation sample. The validation sample consists of new individuals, with outcome labelled predictor data, that differ in various ways from the individuals in the development sample. These individuals may differ in the time in which their data were collected (temporal validation) or from which country or hospital their data were collected (geographical validation). The latter validation technique assesses the transportability of the prediction model. Such external validation is important to perform, due to internally validated prediction models tendency to perform worse in new sets of individuals. If the performance is poor within the validation sample, the model is of no value (13).

In the third and final step consisting of the model impact study, the aim is to assess the prediction models impact, ideally in a randomised trial. The impact of the model is assessed in variables such as decision making changes in healthcare professionals, patient health outcomes and/or cost-effectiveness of care. These impact studies are carried out to prove that the prediction model is of value in clinical practice (13).

Carrying out these prediction model studies can be complex as problems may arise during the time in which they are carried out. One such problem may occur during the model validation study. In this study, in order to obtain the predictive performance of the prediction model within the validation sample, available predictor and outcome data is needed from that sample (13). This data is not always available retrospectively and can present a problem that is both time inefficient and expensive, if the data is difficult to access when collecting it prospectively. 

This specific problem would present itself if data for the Framingham Risk Score were to be collected prospectively, in order to perform a model validation study. The predictor data in this prediction model are cheap blood samples and simple demographics while the outcome data is cardiovascular disease within 10 years (16). The predictor data for this prediction model may be easily accessible. But the outcome data is only accessible after 10 years of follow up. Due to the difficulties in collecting the outcome data prospectively, this specific problem would be presented.

It would therefore be desirable to have a method that can estimate the predictive performance of a prediction model when it is transferred by only utilizing unlabelled predictor data from the validation sample. Such a method could in theory, simplify the process of implementing prediction models in clinical practice and therefore indirectly improve decision making changes in healthcare professionals, patient health outcomes and/or cost-effectiveness of care. At present, no such methods exist which presents a substantial knowledge gap. Therefore, the aim of this study was to develop and test a new method for predicting prediction model performance after transfer while using unlabelled data.

# Aim
The aim of this study was to develop and test a new method for predicting prediction model performance after transfer using unlabelled data. 

The hypothesis was that the new method, which uses a segmented approach, will have a performance that is higher or as good as current naive ways of assessing the predictive performance within the validation sample. This hypothesis was based on the fact that the prediction model, which is developed with the development sample, is the one predicting the predictive performance within the validation sample by predicting outcome in the segmented sample. This segmented sample consists of selected data from the development sample which should in theory lead to an optimistic performance.

# Methods and Materials
## Study design
The study design was an analytical registry based study. To perform the analysis, a dataset was used that has been made freely reusable by the multinational observational study by Eckert A et al in the Dryad Digital Repository (17, 18). This dataset was chosen due to consisting of patient data from different countries with available patient parameters that can be linked to a patient outcome.

## Participants
The participants enrolled in the dataset were all patients seeking ED care between March 2013 and October 2014 within three tertiary care centers in the USA (Clearwater Hospital), France (Hôspital de la Salpêtrière) and Switzerland (Kontonsspital Aaura). The data that was registered for each participant included the hospital and country that the patient seeked, vital signs, laboratory assessments, age, discharge location, length of stay, intensive care unit (ICU) admission and death within 30 days. The inclusion criteria to be enrolled in the dataset was that an initial blood sample was taken. The exclusion criteria were paediatric or surgical patient (17).

## Variables
### Model predictors
The model predictors that were used to develop prediction models and simulate new data in the statistical analysis included respiratory rate (per min), peripheral oxygen saturation (%), systolic blood pressure (mm Hg), heart rate (beats per min), temperature (°C) and age. How these variables were measured was not mentioned in the original study that publicized them. These variables were chosen at random.

### Model outcomes
The model outcomes that were used to develop prediction models in the statistical analysis included ICU admission and the country from which the patient seeked ED care. ICU admission was chosen as the outcome for the prediction model due being more frequent than death within 30 days.

### Sample size
The final sample size used in this study was 1303 patients which included all of the patients from the dataset.

### Missing data
Because of the dataset already being filtered to mostly containing no missing data, a complete case analysis was carried out.

## Statistical analysis
### Dataset
The dataset previously mentioned in the study design was split based on the country from which the participants seeked ED care.

### Sequence of analysis
Analysis in this study was performed in the programming language R (19). The sequence of analysis performed were dataset simulation, dataset assignment, prediction model development, development sample performance, true validation sample performance, propensity model development, predicted validation sample performance and approach comparison.

### Dataset simulation
To increase the number of participants, 10 000 new patients were simulated for each of the datasets. The process of simulation included model predictor simulation and model outcome simulation. The model predictors used in this process included respiratory rate, peripheral oxygen saturation, systolic blood pressure, heart rate, temperature and age. The model outcome used in this process included ICU admission.

To simulate new model predictors for one of the datasets, the mvrnorm function implemented in the MASS package was used (20). The function utilized the model predictors from the dataset in order to develop new simulated model predictors. To simulate model outcomes for the simulated model predictors, the glm function implemented in R was used to develop a logistic regression model (21). This model was trained with model predictors and model outcomes from the same dataset used to simulate model predictors. The trained model was then used to predict outcome in the simulated model predictors and the predicted outcome was set as the simulated outcome. This process of simulation was repeated for each of the datasets.

### Dataset assignment
To simulate the transfer of a prediction model from one country to another, one of the simulated datasets, that including simulated model predictors and model outcomes, was noted as the development sample while one of the two remaining simulated datasets was noted as the validation sample. The development sample represented data from the country in which the prediction model was created, while the validation sample represents data from the country in which the prediction model was transferred to.

### Prediction model development
In the prediction model development step, a prediction model was developed by training a logistic regression model with the development sample. The model predictors that were used to train the model included respiratory rate, peripheral oxygen saturation, systolic blood pressure, heart rate, temperature and age. The model outcome that was used to train the model included ICU admission.

### Development sample performance
To assess the predictive performance of the prediction model within the country in which it was developed, the model developed in the prediction model development step was used to predict outcome within the development sample.

### True validation sample performance
To assess the true performance of the prediction model within the country in which it was transferred to, the model developed in the prediction model development step was used to predict outcome within the validation sample.

### Propensity model development
In the propensity model development step, the data from the development sample and the validation sample were pooled into one sample. This aggregated sample was then used to develop a propensity model, also a prediction model, using logistic regression. The model predictors that were used to train the model included respiratory rate, peripheral oxygen saturation, systolic blood pressure, heart rate, temperature and age. The model outcome that was used to train the model was the country in which the participant seeked ED care. The propensity model was then used to predict from which sample the data in the aggregated sample originated from. Observations from the development sample that were misclassified as validation observations, were used to create a segmented sample.

### Predicted validation sample performance
To assess the predicted performance of the prediction model within the country in which it was transferred to, the model developed in the prediction model development  step was used to predict outcome within the segmented sample.

### Approach comparison
To assess the performance of the naive approach, the absolute difference between the development sample performance step and the true validation sample performance step was calculated. To assess the performance of the segmented approach, the absolute difference between the predicted validation performance step and the development sample performance step was calculated. To assess which approach performed best, the difference between the naive approach and the segmented approach was calculated.

### Sequence repetition
To obtain 95% confidence intervals (CI) around the performances and the differences, the sequence of analysis will be repeated 1000 times. These repetitions will be performed for each available combination in the dataset assignment step.

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
## Sample description
All 1303 participants in the sample had complete data and were included in the final analysis (`r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,1][[1]]` `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,2][[1]]`, `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,1][[2]]` `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,2][[2]]`, `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,1][[3]]` `r data.frame(sort(table(pctabledf$Country), decreasing = TRUE))[,2][[3]]`).<!-- Well, no observations were included in any other sense than that they were used to create the simulated data. I think that you should also present some summary statistics of the simulated data, similar to Table 1.--> Baseline characteristics for of the original sample stratified by country are shown in table 1.<!-- Add some general descriptors of the dataset, for example gender and age distribution etc-->

`r kableone(Pctable, caption = Original data stratified by country')`

<!-- Instead of presenting each transfer separately I suggest that you present the results in the same sequence as you present the methods, i.e. first model development, then validation, then the performance of each approach and finally the difference in performance between the approaches -->
## `r strata.combinations[[1]][1]` to `r strata.combinations[[1]][2]`
When developing the prediction model in `r strata.combinations[[1]][1]` and transferring it to `r strata.combinations[[2]][2]`, we found a significantly higher performance in the segmented approach than the naive approach with a mean difference of `r dftable(1)[,2][6]` (95% CI `r cis[6]` to `r cis[6+36]`). Table 2 shows the mean performance and 95% CI for the performances and performance differences within the `r strata.combinations[[1]][1]` to `r strata.combinations[[2]][2]` transfer.

`r kable(dftable(1), align = 'lcc', caption = 'Performances and perforamnce differences for French model transfered to USA')`

## `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]`
When developing the prediction model in `r strata.combinations[[2]][1]` and transferring it to `r strata.combinations[[2]][2]`, we found a significantly higher performance in the segmented approach than the naive approach with a mean difference of `r dftable(2)[,2][6]` (95% CI `r cis[12]` to `r cis[36+12]`). Table 3 shows the mean performances and 95% CI for the different performances and performance differences within the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer.

`r kable(dftable(2), align = 'lcc', caption = paste(strata.combinations[[2]][1], strata.combinations[[2]][2], sep = ' to '))`

## `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]`
When developing the prediction model in `r strata.combinations[[3]][1]` and transferring it to `r strata.combinations[[3]][2]`, we found no significant performance difference between the segmented approach and the naive approach with a mean difference of `r dftable(3)[,2][6]` (95% CI `r cis[18]` to `r cis[36+18]`). Table 4 shows the mean performances and 95% CI for the different performances and performance differences within the `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]` transfer.

`r kable(dftable(3), align = 'lcc',caption = paste(strata.combinations[[3]][1], strata.combinations[[3]][2], sep = ' to '))`

## `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]`
When developing the prediction model in `r strata.combinations[[4]][1]` and transferring it to `r strata.combinations[[4]][2]`, we found a significantly higher performance in the segmented approach than the naive approach with a mean difference of `r dftable(4)[,2][6]` (95% CI `r cis[24]` to `r cis[36+24]`). Table 5 shows the mean performance and 95% CI for the performances and performance differences within the `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]` transfer.

`r kable(dftable(4), align = 'lcc', caption = paste(strata.combinations[[4]][1], strata.combinations[[4]][2], sep = ' to '))`

## `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]`
When developing the prediction model in `r strata.combinations[[5]][1]` and transferring it to `r strata.combinations[[5]][2]`, we found a significantly higher performance in the segmented approach than the naive approach with a mean difference of `r dftable(5)[,2][6]` (95% CI `r cis[30]` to `r cis[36+30]`). Table 6 shows the mean performance and 95% CI for the performances and performance differences within the `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` transfer.

`r kable(dftable(5), align = 'lcc', caption = paste(strata.combinations[[5]][1], strata.combinations[[5]][2], sep = ' to '))`

## `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]`
When developing the prediction model in `r strata.combinations[[6]][1]` and transferring it to `r strata.combinations[[6]][2]`, we found a significantly higher performance in the segmented approach than the naive approach with a mean difference of `r dftable(6)[,2][6]` (95% CI `r cis[36]` to `r cis[36+36]`). Table 7 shows the mean performance and 95% CI for the performances and performance differences within the `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]` transfer.

`r kable(dftable(6), align = 'lcc', caption = paste(strata.combinations[[6]][1], strata.combinations[[6]][2], sep = ' to '))`

## Naive vs segmented approach
When we transferred the prediction model developed in `r strata.combinations[[1]][1]` to `r strata.combinations[[2]][2]`, in `r strata.combinations[[2]][1]` to `r strata.combinations[[2]][2]`, in `r strata.combinations[[4]][1]` to `r strata.combinations[[4]][2]`, in `r strata.combinations[[5]][1]` to `r strata.combinations[[5]][2]` and in `r strata.combinations[[6]][1]` to `r strata.combinations[[6]][2]`, we found significantly higher performance in the segmented approach than the naive approach with mean differences of `r dftable(1)[,2][6]` (95% CI `r round(cis[6], 2)` to `r round(cis[6+36],2)`), `r dftable(2)[,2][6]` (95% CI `r round(cis[12],2)` to `r round(cis[36+12],2)`), `r dftable(4)[,2][6]` (95% CI `r round(cis[24],2)` to `r round(cis[36+24],2)`), `r dftable(5)[,2][6]` (95% CI `r round(cis[30],2)` to `r round(cis[36+30],2)`) and `r dftable(6)[,2][6]` (95% CI `r round(cis[36],2)` to `r round(cis[36+36],2)`) respectively. When we transferred the prediction model developed in `r strata.combinations[[3]][1]` to `r strata.combinations[[3]][2]`, we found no significant performance difference between the segmented approach and the naive approach with a mean difference of `r dftable(3)[,2][6]` (95% CI `r round(cis[18],2)` to `r round(cis[36+18],2)`). Figure 1 illustrates the differences between the naive and segmented approach for all of the transfers.

```{r, echo = FALSE} 
print(jitterplot(6), caption = 'jitter')
```
