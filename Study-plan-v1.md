---
title: Study plan
author: Arman Norouzi
bibliography: cite.bib
csl: bmcemerg.csl
---

# A Segmented Approach to Predicting Prediction Model Performance After Transfer Using Unlabelled Data
# Introduction
\vspace{-0.1cm}
Prediction models can be defined to be a statistical method that utilizes multiple predictor data which may be used to estimate a risk of a specific outcome in an individual. The outcome that is estimated could be a disease or a condition, referred to as a diagnostic model, or a specific event that will occur in the future, referred to as a prognostic model [@collins2015transparent].  

The implementation of prediction models within the field of medicine are strongly linked to the movement towards stratified medicine. Stratified medicine aims to aid the health care workers with their decision making depending on each individual’s own profile of prediction factors [@steyerberg2013prognosis]. The use of diagnostic models can help clinicians with reassuring the patients that their symptoms are not due to a serious disease, referral of patients to further testing or initiation of treatment. Prognostic models can help clinicians to help change patient lifestyles or with therapeutic decisions within a given period [@moons2009prognosis][@bookforreference].  

One of the more well known clinical prediction models is the Framingham Risk Score. The prediction model estimates the absolute risk of developing a cardiovascular disease within 10-years based on multiple patient predictor data [@anderson1991cardiovascular]. To implement such a prediction model to a new setting that is different from where the model was developed, one has to perform an external validation of the model. This is due to the performance drop in internally validated prediction models when applied to a new set of individuals [@moons2012risk]. To validate a model that is transferred to another setting, one has to gather both predictor and outcome data from the new setting. 

Currently there are no methods for external validation without the use of outcome data when transferring a prediction model to another setting. This represents a substantial knowledge gap. Therefore, the aim of this study is to develop a method for predicting prediction model performance after transferring to a new setting while only utilizing predictor data.

# Methods
\vspace{-0.1cm}
## Study design
\vspace{-0.5cm}
The design of the study that we will conduct is a analytical registry study based on a public database made available by the retrospective analysis of a multinational observational study, “Combination of the National Early Warning Score (NEWS) and inflammatory biomarkers for early risk stratification in emergency department patients: results of a multinational, observational study” [@eckart2019combination].

## Participants
\vspace{-0.5cm}
The patients enrolled in the database were all seeking emergency department (ED) care between March 2013 and October 2014 within three tertiary care centers located in USA (Clearwater Hospital), France (Hôspital de la Salpêtrière) and Switzerland (Kontonsspital Aarau). Inclusion criteria for participating in the registry was that an initial blood sample was taken. Paediatric and surgical patients were excluded. 

## Sample Size
\vspace{-0.5cm}
The total number of participants available in the public database is 1303 which is the sample size arrived at in this study. In the original multinational observational study there were a total of 7132 participants registered. However only 1303 of them had fully registered NEWS parameters which is the reason for the sample size.

## Variables
\vspace{-0.5cm}
*Model predictor*  
The predictors used in our study to estimate the outcome are respiratory rate (RR), confusion, peripheral oxygen saturation (SpO2), systolic blood pressure (BPS), diastolic blood pressure (DPS), heart rate (HR), temperature (temp), midregional-proadrenomedullin (MR-proADM) and procalcitonin (PCT). Sex (male or female), discharge location, length of stay (LOS) and age were also registered.The methods used to measure predictor data such as HR, BPS, BPD, RR, Temp were not mentioned in the original study.  

*Model outcome*  
The primary outcome in our study is death within 30 days. The secondary outcome is whether or not the patients are referred to the intensive care unit (ICU) during their time in the hospital.

## Missing data
\vspace{-0.5cm}
To account for missing data we are using multivariate imputation by chained equations which is implemented in the R package mice [@buuren2010mice]. The missing percentage of data will be equal to the number of imputations made. Qualitative variables and quantitative variables are imputed using logistic regression and predictive mean matching respectively. 

## Statistical methods
\vspace{-0.5cm}
*Dataset*  
In our study the dataset will consist of the publicly available register that has been previously mentioned.

*Development sample and validation sample*  
The dataset will be split into a development sample and a validation sample with a test-train split function implemented in R. The development sample will represent the prediction model development setting, and the validation sample the transfer setting. To obtain stable coefficient estimates, the amount of events (where the outcome occurs) in the development setting is set to (x*10) were x is the amount of predictor parameters [@peduzzi1996simulation]. For the validation sample a minimum of 100 events will be included and 100 or more non-events [@vergouwe2005substantial].
 
*Sequence of analysis*  
Analysis in this study is all performed in the programming language R [@internet1]. The sequence of analysis that will be performed to evaluate our method is model development, model validation, method testing and model comparison.

*Model development*  
During model development a prediction model is developed by using the development sample to fit the model and test it. The accuracy of this model is calculated using logistic regression as implemented in R function glm. To compensate for the risk of overfitting the model a bootstrap procedure is used to estimate the linear shrinkage factor [@steyerberg2001application]. The shrinkage factor is then implemented to the prediction model coefficients .

*Model validation*  
To examine the accuracy of the model in another setting the prediction model that has been developed in the previous stage will be used to estimate the outcome within the validation sample.

*Method testing models*  
To evaluate the effect of our various methods of increasing model performance based solely on predictor data, we will calculate the accuracy of them predicting outcome in the validation sample. The methods that we will evaluate are:

Stacked propensity matching (13)
other 

*Model comparison*  
Finally, during this step, the difference in accuracy in the development model and validation model is calculated, while the same is done between the development model and the different method testing models. The differences are then compared to each other to evaluate whether the method testing models increase performance in  estimating outcome in the transfer setting. 

## Ethical considerations
\vspace{-0.5cm}
*Principle of autonomy*  
The data used in the original  observational study has been approved by the institutional review boards of the three hospitals and has since been published for general use. Therefore the respect of autonomy is upheld.  

*Principle of beneficence*  
This study will attempt to act in the best interest of future analytical research and indirectly patients, in an attempt to find new ways of increasing external validity in prediction models based on only predictor data.  

*Principle of nonmaleficence*  
The database used in this study has already been depersonalized and publicized. Therefore no ways of identification leakage is possible. All analytical data 
Due to the data already being depersonalized and publicized there is no way for patient identification to be leaked. All analytical data collected in the study is not meant to harm intentionally or unintentionally.  

*Principle of Justice*  
Because this study is analytical, the principle of justice does not prevail. The data in the study will be treated equally.  

*Ethical permit*  
Due to this study being an analytical study based on a public database the need for an ethical permit is not required.  
