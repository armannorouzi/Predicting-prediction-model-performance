---
title: Study plan
author: Arman Norouzi
bibliography: cite.bib
csl: bmcemerg.csl
---

# A Segmented Approach to Predicting Prediction Model Performance After Transfer Using Unlabelled Data
# Introduction
Prediction models within the fields of medicine can be defined to be a statistical method that utilizes predictor data to estimate the risk of a specific outcome for a patient (1, 2, 3, 4). The outcome is generally estimated based on multiple prediction variables (3, 4) and could either be predicting a disease (diagnostic model) or an event that will occur in the future (prognostic model) (5).

The implementation of prediction models is closely associated with the movement towards stratified medicine, where clinical decisions are based on each individual's profile of predictor data (4). In diagnostic models the information acquired can be used to reassure patients that their symptoms are not caused by a serious disease, to refer the patients for further testing and to initiate treatment (5). In prognostic models the information acquired can be used to plan lifestyle changes and therapeutic options based on the risk of outcome within the observed period (1, 2). 

A great example of a well known clinical prediction model is the Framingham Risk Score, where the model estimates the risk of developing a cardiovascular disease within 10-years based on multiple patient predictor data (6). To develop such a prediction model a number of steps are required. A necessary step is to validate the model within the setting that the model was created (2, 4, 5, 7). After such internal validation, it is highly recommended to validate the model within a setting similar but different from the one that was used to develop the model (5, 7).  This is because internally validated prediction models usually drop in performance when they are transferred to a new setting (7).

Carrying out such validation requires that you use the original model to estimate the outcome based on prediction data from the new setting, which is then compared with the observed outcome (7, 8). Only by then can one comment on how well the prediction model performance is in a setting different from the one used to develop the model. If the performance of the model is poor, adjustments could be made accordingly with the data collected in the transfer setting (7). 

But collecting outcome data from a new setting to estimate the performance of the prediction model in a transfer setting to eventually adjust it can be difficult in various ways. In the case of the Framingham Risk Score, outcome data can only be collected after 10-years of follow-up (6). This presents a problem that is both time inefficient and expensive. It would therefore be desirable to have a method that could predict how well a prediction model performs in a transfer setting while only utilizing predictor data.

At present, the only way to estimate prediction model performance in a transfer setting is to have a complete data set from the transfer setting with both predictor and outcome data. Therefore the aim of this study is to develop and test different methods to predict prediction model performance while only utilizing predictor data.

# Methods
## Study design
The study that we will conduct is an analytical registry-based study. The database used for analysis has been made publicly available by the multinational, observational study by Eckert A et al, 2019 (9).

## Participants
The patients enrolled in the public database were all seeking emergency department (ED) care between March 2013 and October 2014 within three tertiary care centers located in USA (Clearwater Hospital), France (Hôspital de la Salpêtrière) and Switzerland (Kontonsspital Aarau). To be included in the registry an initial blood sample was required and the patients could not be paediatric or surgical patients.

## Sample Size
The sample size arrived at in this study includes the total amount of participants (N=1303) available in the previously mentioned database in study design. These 1303 participants were selected from a total of 7132 participants based on having complete National Early Warning Score (NEWS) parameters (9).

## Variables
*Model predictor*  
The predictor data used to estimate outcomes in our study are respiratory rate (RR), confusion, peripheral oxygen saturation (SpO2), systolic blood pressure (BPS), heart rate (HR), temperature, midregional-proadrenomedullin (MR-proADM) and procalcitonin (PCT). The methods used to measure the different vital parameters were not specified in the original study.
  
*Model outcome*  
The outcome of this study is referral to the intensive care unit (ICU) within the time that the patients are in the hospital.

## Missing data
Due to the database only consisting of participants with complete NEWS parameters we have decided to account for missing data by conducting a complete case analysis. 

## Statistical methods
*Dataset*  
The publicly available register that has been previously mentioned in the study design will be divided into three separate data sets based on which country (USA, France and Switzerland) the data was collected.

*Development sample and validation sample*  
Each of the data sets will then be split into a development sample and a validation sample with the train test split function implemented in the scikit-learn package in python (10). The development sample representing the development setting will contain 80 percent of the dataset and the validation sample representing the transfer setting will contain the remaining 20 percent.
 
*Sequence of analysis*  
Analysis in this study is all performed in the programming language python (11). The sequence of analysis to conduct this study is model development, model validation, propensity model development, propensity model validation and model comparison. 

*Model development*  
During this step a prediction model is developed with the development sample in each data set by using logistic regression implemented in the scikit-learn package in python (12). To avoid overfitting the model we will utilize a bootstrap to estimate a linear shrinkage factor that will be applied to the model coefficients (13). The shrunk model will then be used to calculate the accuracy of the model within the development sample.

*Model validation*  
To examine the accuracy of the model in another setting the prediction model that has been developed in the previous stage will be used to estimate the outcome within the validation sample.

*Propensity model development*  
During this step, the predictor data from the development and the validation sample will be combined to train a propensity model to discriminate between observations from the two samples. The propensity model will then be used to identify similarities (ex: propensity score difference < 0,05) between the two samples.

*Propensity model validation*  
The segment of participants in the validation sample that have been identified as similarities are then included in a new validation sample. This new validation sample is then used to calculate the accuracy of the prediction model previously mentioned in model development.

*Model comparison*  
Finally, the difference in accuracy between model development and model validation is calculated and the same is done between model development and propensity model validation. To evaluate the different methods of estimating outcome in the validation sample the difference between the differences are calculated. Both the accuracy and the differences are bootstrapped to estimate 95% confidence intervals (CI). The bootstrap procedures used will bootstrap 1000 samples with replacements based on the same size as the original samples.

## Ethical considerations
*Principle of autonomy*
The data used in the original  observational study has been approved by the institutional review boards of the three hospitals and has since been published for general use. Therefore the respect of autonomy is upheld.

*Principle of beneficence*
This study will attempt to act in the best interest of future analytical research and indirectly patients, in an attempt to find new ways of predicting prediction model performance in a transfer setting while only utilizing predictor data.

*Principle of nonmaleficence*
The attempts made in this study will be made without the intention of harm, intentionally or unintentionally. Due to the database being depersonalized and publicized there is no way of identification leakage from this study.

*Principle of Justice*
Because this study is analytical, the principle of justice does not prevail. The data in the study will be treated equally.

*Ethical permit*
Due to this study being an analytical study based on a public database the need for an ethical permit is not required. 

# References
[1]   
[2]   
[3]   
[4]   
[5]   
[6]   
[7]   
[8]   
[9]   
[10]  
[11]  
[12]  
[13]  
