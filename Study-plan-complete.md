---
title: Study plan
author: Arman Norouzi
bibliography: cite.bib
csl: bmcemerg.csl
---

# A Segmented Approach to Predicting Prediction Model Performance After Transfer Using Unlabelled Data
# Introduction
In medicine, healthcare professionals are confronted daily with a wide range of information that needs to be processed in order to make clinical decisions. Due to the vast amount of information, prediction models (also known as “prediction rules”, “prognostic models” or “risk scores” (1)) have been implemented with the intention to aid in decision-making (2). The way that prediction models help healthcare professionals is by utilizing statistical methods to inform about the risk of a specific outcome based on prediction variables (3). This type of decision making is closely associated with the movement towards stratified medicine, where decisions are based on each individual’s profile of prediction data (1).

The predictions that are made with a prediction model are generally based on multiple variables, and the outcome could either be a disease (diagnostic model) or an event that will occur in the future (prognostic model) (3, 4). With the estimated risk of a specific outcome within these models, decision making could be aided in various ways. In a diagnostic model, the predictions can be used to inform patients that their symptoms are not caused by a serious disease, to refer the patients for further testing and to initiate treatment (3). In a prognostic model, the predictions made can be used to choose between therapeutic options and to plan lifestyle changes within the period that has been observed (2, 5). This type of prediction could also be utilized to risk-stratify patients in therapeutic clinical trials (6, 7). 

There are several well-known clinical prediction models that are daily being used within healthcare. The Ottawa Ankle Rules, CHA2DS2-VASc score and the Framingham Risk Score are just a few examples (8, 9, 10). To implement such models in healthcare, a few steps are required. 

The first step is to conduct a model development study (11). In these studies, a logistic regression model is developed by using a data set, defined as the development sample, that includes both predictor and outcome data from the development setting (11). When the model is developed, it usually has a tendency to be optimistic when estimating performance within the development sample and needs to therefore be quantified for optimism by internal validation techniques (3). The quantified optimism can thereafter be adjusted for by applying shrinkage or penalisation to the original model (12)

The second step is to perform a validation study with or without model updating (13). During this step the prediction model is tested within a validation sample that represents a setting that is similar but different from the setting in which the model was created (13). This is done because developed and internally validated prediction models generally drop in performance when it is introduced to a new set of individuals, that differ from the setting in which the model was developed (13). If the performance of the model is poor within the validation sample, the model is of no use (14). However, adjustments could be made accordingly with the data collected from the validation sample, which is also more preferable than developing a new prediction model from scratch (13).

The final step is to perform an impact study (13, 14). During this step the impact of the prediction model is quantified and compared to not using the model, Ideally in a randomised trial, in variables such as behavioural changes in patients and healthcare professionals, patient health outcomes or cost-effectiveness of care (13, 14). If the model has no impact, it is of no use (14).

After completing all these steps, you can be more certain of whether or not the prediction model is applicable to the healthcare system from which you have developed the model. This however is easier said than done. As we previously mentioned in the validation study step,  developed and internally validated prediction models generally perform poorly in the transfer setting (13). In order to even be able to obtain this information, one needs to collect both predictor and outcome data from the validation sample (13). This can be a problem that is both time inefficient and expensive if the data is difficult to access. In the case of the Framingham Risk Score, this problem is presented because of the outcome data only being accessible after 10 years of follow up (10).

It would therefore be desirable to have a method that can estimate the performance of a prediction model by only utilizing predictor data, while attempting to reduce the performance drop that occurs when the model is transferred to another setting. Such a method would in theory lead to the possibility of applying internally validated prediction models to new settings without the need of a validation study that requires outcome data. In a healthcare system where the possibility exists to collect outcome data in the validation sample, this method would reduce the resource intensiveness of implementing the model. In a healthcare system where the possibility does not exist to collect outcome data in the validation sample, this method would give another opportunity to implement the model because of only requiring predictor data. In summary, this method would simplify the process of reaching the impact study step and hence indirectly behavioral changes in patients and healthcare professionals, patient health outcomes and cost-effectiveness of care.

At present, no such methods exist. Therefore, the aim of this study is to develop and test a new method for estimating the performance of prediction models by only utilizing prediction data, when the model is transferred from one setting to another, in an attempt to reduce the performance drop that usually occurs. 

Our hypothesis is that the new method that is developed and tested in our study will be more adapted to the validation sample and will therefore be more accurate in predicting outcome in that sample than what current methods are capable of.

# Aim
The aim of this study is to develop and test a method for predicting prediction model performance after transfer to a new setting utilizing only predictor data, in an attempt to reduce the performance drop that usually occurs. The method will focus on identifying segments of observations in the development and the validation sample that are similar in order to create a segment of misclassified development samples. That segment is then used to estimate performance within the validation sample.

# Methods
## Study design
The study that we will conduct is an analytical registry-based study. The database used for analysis has been made publicly available by the multinational, observational study by Eckert A et al, 2019 (15).

## Participants
The patients enrolled in the public database were all seeking emergency department (ED) care between March 2013 and October 2014, within three tertiary care centers located in USA (Clearwater Hospital), France (Hôspital de la Salpêtrière) and Switzerland (Kontonsspital Aarau). To be included in the registry an initial blood sample was required and the patients could not be paediatric or surgical patients.

## Sample Size
The sample size arrived at in this study includes the total amount of participants (N=1303) available in the previously mentioned database in study design. These 1303 participants were selected from a total of 7132 participants based on having complete National Early Warning Score (NEWS) parameters (15).

## Variables
*Model predictor*  
The predictor data used to estimate outcomes in our study are respiratory rate (RR), confusion, peripheral oxygen saturation (SpO2), systolic blood pressure (BPS), heart rate (HR), temperature, midregional-proadrenomedullin (MR-proADM) and procalcitonin (PCT). The methods used to measure the different vital parameters were not specified in the original study.
  
*Model outcome*  
The outcome of this study is referral to the intensive care unit (ICU) within the time that the patients are in the hospital.

## Missing data
Because of our data set largely consisting of participants with complete data and therefore only having a few observations missing, we have decided to carry out a complete case analysis (16).  

## Statistical methods
*Dataset*  
The publicly available register that has been previously mentioned in the study design will be divided into three separate data sets based on which country (USA, France and Switzerland) the data was collected.

*Development sample and validation sample*  
Each of the data sets will then be split into a development sample and a validation sample with the train test split function implemented in the scikit-learn package in python (17). The development sample representing the development setting will contain 80 percent of the dataset and the validation sample representing the transfer setting will contain the remaining 20 percent.
 
*Sequence of analysis*  
Analysis in this study is all performed in the programming language python (18). The sequence of analysis to conduct this study is model development, model validation, propensity method model development, propensity method model validation and model comparison. 

*Model development*  
During this step a prediction model is developed with the development sample in each data set by using logistic regression implemented in the scikit-learn package in python (19). To avoid overfitting the model we will utilize a bootstrap to estimate a linear shrinkage factor that will be applied to the model coefficients (20). The shrunk model will then be used to calculate the accuracy of the model within the development sample.

*Model validation*  
To examine the accuracy of the model in another setting the prediction model that has been developed in the previous stage will be used to estimate the outcome within the validation sample.

*Propensity method model development*  
During this step, data from the development and validation sample are pooled into one sample. This aggregated sample is then used to train a propensity model to distinguish which data belongs to which sample. Data from the development sample that are misclassified as validation data, are used to form a separate segment. The segment is then used to develop a prediction model.

*Propensity method model validation*  
The prediction model developed with the segmented data is then used to calculate the accuracy of predicting outcome within the validation sample.

*Model comparison*  
Finally, the difference in accuracy between model development and model validation is calculated and the same is done between model development and propensity method model validation. To evaluate the different methods of estimating outcome in the validation sample the difference between the differences are calculated. Both the accuracy and the differences are bootstrapped to estimate 95% confidence intervals (CI). The bootstrap procedures used will bootstrap 1000 samples with replacements based on the same size as the original samples.


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
[1][@steyerberg2013prognosis]   Steyerberg EW, Moons KG, van der Windt DA, Hayden JA, Perel P, Schroter S, Riley RD, Hemingway H, Altman DG, PROGRESS Group. Prognosis Research Strategy (PROGRESS) 3: prognostic model research. PLoS Med. 2013 Feb 5;10(2):e1001381.  
[2][@moons2009prognosis]   Moons KG, Royston P, Vergouwe Y, Grobbee DE, Altman DG. Prognosis and prognostic research: what, why, and how?. Bmj. 2009 Feb 23;338:b375.  
[3][@collins2015transparent]   Collins GS, Reitsma JB, Altman DG, Moons KG. Transparent Reporting of a Multivariable Prediction Model for Individual Prognosis or Diagnosis (TRIPOD) The TRIPOD Statement. Circulation. 2015 Jan 13;131(2):211-9.  
[4][@riley2013prognosis]   Riley RD, Hayden JA, Steyerberg EW, Moons KG, Abrams K, Kyzas PA, Malats N, Briggs A, Schroter S, Altman DG, Hemingway H. Prognosis Research Strategy (PROGRESS) 2: prognostic factor research. PLoS Med. 2013 Feb 5;10(2):e1001380.  
[5][@bookforreference1]   Steyerberg EW. Clinical prediction models: a practical approach to development, validation, and updating. Springer, 2009.  
**obs selfmade reference in cite.bib**  
[6][@dorresteijn2011estimating]   Dorresteijn JA, Visseren FL, Ridker PM, Wassink AM, Paynter NP, Steyerberg EW, van der Graaf Y, Cook NR. Estimating treatment effects for individual patients based on the results of randomised clinical trials. Bmj. 2011 Oct 3;343:d5888.  
[7][@hayward2006multivariable]   Hayward RA, Kent DM, Vijan S, Hofer TP. Multivariable risk prediction can greatly enhance the statistical power of clinical trial subgroup analysis. BMC medical research methodology. 2006 Dec 1;6(1):18.  
[8][@stiell1992study]   Stiell IG, Greenberg GH, McKnight RD, Nair RC, McDowell I, Worthington JR. A study to develop clinical decision rules for the use of radiography in acute ankle injuries. Annals of emergency medicine. 1992 Apr 1;21(4):384-90.  
[9][@lip2010refining]   Lip GY, Nieuwlaat R, Pisters R, Lane DA, Crijns HJ. Refining clinical risk stratification for predicting stroke and thromboembolism in atrial fibrillation using a novel risk factor-based approach: the euro heart survey on atrial fibrillation. Chest. 2010 Feb 1;137(2):263-72.  
[10][@anderson1991cardiovascular]  Anderson KM, Odell PM, Wilson PW, Kannel WB. Cardiovascular disease risk profiles. American heart journal. 1991 Jan 1;121(1):293-8.  
[11][@royston2009prognosis]  Royston P, Moons KG, Altman DG, Vergouwe Y. Prognosis and prognostic research: developing a prognostic model. Bmj. 2009 Mar 31;338.  
[12][@steyerberg2001application]  Steyerberg EW, Eijkemans MJ, Habbema JD. Application of shrinkage techniques in logistic regression analysis: a case study. Statistica Neerlandica. 2001 Mar;55(1):76-88.  
[13][@moons2012risk]  Moons KG, Kengne AP, Grobbee DE, Royston P, Vergouwe Y, Altman DG, Woodward M. Risk prediction models: II. External validation, model updating, and impact assessment. Heart. 2012 May 1;98(9):691-8.  
[14][@moons2009prognosis]  Moons KG, Altman DG, Vergouwe Y, Royston P. Prognosis and prognostic research: application and impact of prognostic models in clinical practice. Bmj. 2009 Jun 4;338:b606.  
**obs bad reference, similar to reference 2**  
[15][@eckart2019combination]  Eckart A, Hauser SI, Kutz A, Haubitz S, Hausfater P, Amin D, Amin A, Huber A, Mueller B, Schuetz P. Combination of the National Early Warning Score (NEWS) and inflammatory biomarkers for early risk stratification in emergency department patients: results of a multinational, observational study. BMJ open. 2019 Jan 1;9(1):e024636.  
[16][@harrell2015regression]  Harrell Jr FE. Regression modeling strategies: with applications to linear models, logistic and ordinal regression, and survival analysis. Springer; 2015 Aug 14.  
[17][@traintestsplit]  Scikit-learn, sklearn.model_selection.train_test_split [internet]. Scikit-learn; [cited 2020-09-20]. Available from: https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.train_test_split.html  
**obs selfmade reference in cite.bib**  
[18][@python]  Python [internet]. Python; [cited 2020-09-20]. Available from: https://www.python.org/  
**obs selfmade reference in cite.bib**  
[19][@logisticregression]  Scikit-learn, sklearn.linear_model.LogisticRegression [internet]. Scikit-learn; [cited 2020-09-20]. Available from: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html  
**obs selfmade reference in cite.bib**  
