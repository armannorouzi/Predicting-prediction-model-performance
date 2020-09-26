---
title: Study plan
author: Arman Norouzi
bibliography: cite.bib
csl: bmcemerg.csl
---

# A Segmented Approach to Predicting Prediction Model Performance After Transfer Using Unlabelled Data
# Introduction
In medicine, healthcare workers are confronted daily with a wide range of information that needs to be processed in order to make clinical decisions. Due to the vast amount of information, prediction models (also referred to as risk scores or prediction rules) have been implemented in healthcare to aid in informed decision making (1, 2). The prediction models do this by providing a calculated risk of a specific outcome in a patient, based on their predictor variables (2, 3). The prediction models are capable of such tasks due to consisting of a machine learning algorithm (4). These algorithms are trained to find relationships in a dataset, consisting of both prediction data and outcome data, in order to assess the risk for the outcome in new unlabeled predictor data (4).

The risk that is provided by the prediction models are generally based on multiple variables and the outcome could either be a disease (diagnostic model) or an event that will occur in the future (prognostic model) (4, 5).  In a diagnostic model, the information provided can be used to inform the patients that their symptoms are caused by a serious disease, to refer the patients to further testing or to initiate treatment (3). In a prognostic model, the information provided can be used to choose between therapeutic options and to plan lifestyle changes within the period that has been observed (2, 6). The prognostic models could also be utilized to risk-stratify patients in therapeutic clinical trials (7, 8).

The areas of use for prediction models are many within healthcare and there are several well-known prediction models that have been placed in clinical practice. The Ottowa Ankle Rules, CHA2DS2-VASc score and the Framingham Risk Score are just a few examples (9, 10, 11). To implement such models into healthcare, a few steps are required. The first step is to conduct a model development study (12). In these studies, a machine learning algorithm is utilized to develop a prediction model with a development sample, consisting of both predictor and outcome data which represents the development setting (4, 12). Depending on the algorithm chosen, the performance of the prediction model may vary (4). However, usually when the data is limited, a simpler algorithm is chosen such as a logistic regression (4). When the algorithm is chosen and the prediction model has been developed, it usually tends to be optimistic when estimating performance within the development sample and need to therefore be quantified for by internal validation techniques (3). The quantified optimism can thereafter be adjusted for by applying shrinkage or penalization to the model (13).

The second step is to conduct a validation study with or without model updating (14). In these studies, the prediction model is tested within a validation sample, representing a transfer setting, which contains new individuals that are similar but differ from those used in the development sample (14). This type of validation is performed because internally validated prediction models tend to lose performance in a new set of individuals and if the performance is poor, the model is of no use (14, 15). However, adjustments could be made accordingly with the data collected from the validation sample, which is also preferable than to developing a new prediction model from scratch (14).

The final step is to conduct an impact study (14, 15). During this step the impact of the prediction model is quantified and compared to not using the model, Ideally in a randomised trial, in variables such as behavioural changes in patients and healthcare workers, patient health outcomes or cost-effectiveness of care (14, 15). If the impact of the prediction model is poor, it is of no use (15).

After completing these steps, you can be more certain of whether the prediction model is applicable to the healthcare system from which you have developed the model. This however is easier said than done. As we previously mentioned during validation studies, developed and internally validated prediction models generally perform worse in the validation sample (14). In order to even be able to obtain this information, one needs to collect both predictor and outcome data from the transfer setting (14). This can be a problem that is both time inefficient and expensive if the data is difficult to access. In the case of the Framingham Risk Score, where the predictor data are cheap blood tests and simple demographics and the outcome data cardiovascular disease within 10 years, this problem is presented (11). This is because of the outcome data only being accessible after 10 years of follow up (11).

It would therefore be desirable to have a method that can estimate the performance of a prediction model when it is transferred to another setting by only utilizing unlabeled predictor data. Such a method would, in theory, if the estimated performance is good, lead to the possibility of implementing internally validated prediction models to a new setting without the need for a validation study that requires outcome data. In a healthcare system where the possibility exists to collect outcome data in the validation sample, this method would reduce the resource intensiveness of implementing the prediction model. In a healthcare system where the possibility does not exist to collect outcome data, this method would give another opportunity to implement the model due to only requiring unlabeled predictor data. In summary, this method would simplify the process of reaching the impact study step and hence indirectly improve, if the model has a positive impact, the behaviour in patients and healthcare professionals, patient health outcomes and cost-effectiveness of care.

At present, no such methods exist. Therefore, the aim of this study was to develop and test a new method for estimating the performance of prediction models in a transfer setting while only utilizing unlabeled data. The method will focus on identifying similarities in the development setting and the transfer setting in order to distinguish which data belongs to which setting. Observations from the development setting that are misclassified as transfer setting observations, will be used to form a separate segment in which the true model performance is predicted.

Our hypothesis is that the new method for estimating prediction model performance within a transfer setting by only utilizing unlabeled data, will be as good as current methods for calculating the performance within the transfer setting by utilizing outcome data. 

# Aim
The aim of this study was to develop and test a new method for predicting prediction model performance in a transfer setting while only utilizing unlabeled data.

# Methods
## Study design
We will conduct an analytical registry-based study. The database used for analysis has been made publicly available by the multinational, observation study by Eckert A et al, 2019, in the DRYAD repository (16, 17).

## Participants
The patients enrolled in the public database were all seeking emergency department care between March 2013 and October 2014 within three tertiary care centers in USA (Clearwater Hospital), France (Hôspital de la Salpêtrière) and Switzerland (Kontonsspital Aaura). The inclusion criteria to be enrolled in the database were that an initial blood sample was taken. The exclusion criteria were that the patients could not be paediatric or surgical patients.

## Sample Size
The final sample size arrived at in this study will include the total number of participants with complete data. Complete data is defined as having no missing model predictor or model outcome data in the database used to conduct this study. 

## Variables
### Model predictor  
The predictor data used to estimate outcome in our prediction models are respiratory rate, confusion, peripheral oxygen saturation (SpO2), systolic blood pressure (BPS), heart rate and temperature.

### Model outcome  
The outcome of the prediction models is referral to the intensive care unit (ICU) within the time that the patients are in the hospital.

### Patient characteristics
To describe the participants in the database, we will in addition to model predictor and model outcome describe age and gender.

## Missing data
To account for missing data in the database, we will carry out a complete case analysis because only missing a few observations (18).

## Statistical methods  
### Statistical analysis  
Analysis in this study is all performed in the programming language python (19). P-values of <0,05 are considered significant. 
**jag förstår inte hur vi ska jämföra propensity metoden mot den riktiga performancen av modellen i transfer setting*

### Dataset splitting  
The publicly available dataset will be divided into three separate datasets based on which country the data was collected from.

### Sequence of analysis  
The sequence of analysis to conduct this study is dataset assignment, prediction model development, prediction model performance, propensity method development and propensity method performance. 

### Dataset assignment
Each of the datasets will take turn in being assigned as the development setting and transfer setting. The development setting will represent the country in which the model is developed while the transfer setting represents the country in which the model is transferred to.

### Prediction model development
A prediction model is developed using the development setting dataset, by using logistic regression implemented in the scikit-learn package in python (20). To avoid overfitting a penalization is applied to the model (13). The penalized prediction model will then be used to calculate the accuracy of the model within the development sample.

### Prediction model performance
To assess the true performance of the model in another setting, the prediction models that have been developed in the previous stage will be used to predict the outcome within the transfer setting.

### Propensity method development
During this stage, the data from the development setting and the predictor data from the transfer setting are pooled into one sample. This aggregated sample is then used to train a propensity model - also using logistic regression - to distinguish which data belongs to which setting. Observations from the development setting that are misclassified as transfer setting, will be used to form a separate segment.

### Propensity method performance
The penalized prediction model is then used to estimate the outcome within the segment that has been created in the previous stage. We use this performance estimate as the prediction of the true performance in the transfer setting.

### Model comparison  (måste ändra innehåll)
Finally, the difference in accuracy in the model development and model validation is calculated and the same is done between the accuracy in model development and propensity method model development. Both the accuracy and the differences are bootstrapped to estimate 95% confidence intervals (CI). The bootstrap procedures used will bootstrap 1000 samples with replacements based on the same size as the original samples.

#### Development sample and validation sample splitting  (ska denna vara kvar? förstår inte direkt inte varför vi gör detta steg)
Each of the datasets will then be split into a development sample and a validation sample with the train test split function implemented in the scikit-learn package in python (21). The development sample representing the development setting will contain 80 percent of the dataset and the validation sample representing the transfer setting will contain the remaining 20 percent.

## Ethical considerations
### Principle of autonomy  
The research data used in this study has been made freely reusable and citable in Dryad Digital Repository (17). Therefore, the principle of autonomy is upheld due to there not being any requirement for informed consent.

### Principle of beneficence  
This study will attempt to act in the best interest of future analytical research and indirectly patients, by developing and testing a new method for predicting prediction model performance in a transfer setting while only utilizing predictor data.

### Principle of nonmaleficence  
The method developed in this study will be made without the intention of harm intentionally or unintentionally. To nullify the risk of patient identification leakage, we have considered to use a dataset that has already been depersonalized and made freely reusable and for general use. By taking these actions we determine that the risk to the population is minimal.

### Principle of Justice  
Due to this study being analytical, the principle of justice does not prevail. However, the data in the study will be treated equally.

### Ethical permit
Because of this study being analytical and based on a freely reusable and public database, the need for an ethical permit was not required.

# References
1.	Steyerberg EW, Moons KG, van der Windt DA, Hayden JA, Perel P, Schroter S, Riley RD, Hemingway H, Altman DG, PROGRESS Group. Prognosis Research Strategy (PROGRESS) 3: prognostic model research. PLoS Med. 2013 Feb 5;10(2):e1001381.

2.	Moons KG, Royston P, Vergouwe Y, Grobbee DE, Altman DG. Prognosis and prognostic research: what, why, and how?. Bmj. 2009 Feb 23;338:b375.

3.	Collins GS, Reitsma JB, Altman DG, Moons KG. Transparent Reporting of a Multivariable Prediction Model for Individual Prognosis or Diagnosis (TRIPOD) The TRIPOD Statement. Circulation. 2015 Jan 13;131(2):211-9.

4.	Deo RC. Machine learning in medicine. Circulation. 2015 Nov 17;132(20):1920-30.

5.	Riley RD, Hayden JA, Steyerberg EW, Moons KG, Abrams K, Kyzas PA, Malats N, Briggs A, Schroter S, Altman DG, Hemingway H. Prognosis Research Strategy (PROGRESS) 2: prognostic factor research. PLoS Med. 2013 Feb 5;10(2):e1001380.

6.	Steyerberg EW. Clinical prediction models: a practical approach to development, validation, and updating. Springer, 2009.

7.	Dorresteijn JA, Visseren FL, Ridker PM, Wassink AM, Paynter NP, Steyerberg EW, van der Graaf Y, Cook NR. Estimating treatment effects for individual patients based on the results of randomised clinical trials. Bmj. 2011 Oct 3;343:d5888.

8.	Hayward RA, Kent DM, Vijan S, Hofer TP. Multivariable risk prediction can greatly enhance the statistical power of clinical trial subgroup analysis. BMC medical research methodology. 2006 Dec 1;6(1):18.

9.	Stiell IG, Greenberg GH, McKnight RD, Nair RC, McDowell I, Worthington JR. A study to develop clinical decision rules for the use of radiography in acute ankle injuries. Annals of emergency medicine. 1992 Apr 1;21(4):384-90.

10.	Lip GY, Nieuwlaat R, Pisters R, Lane DA, Crijns HJ. Refining clinical risk stratification for predicting stroke and thromboembolism in atrial fibrillation using a novel risk factor-based approach: the euro heart survey on atrial fibrillation. Chest. 2010 Feb 1;137(2):263-72.

11.	Anderson KM, Odell PM, Wilson PW, Kannel WB. Cardiovascular disease risk profiles. American heart journal. 1991 Jan 1;121(1):293-8.

12.	Royston P, Moons KG, Altman DG, Vergouwe Y. Prognosis and prognostic research: developing a prognostic model. Bmj. 2009 Mar 31;338.

13.	Steyerberg EW, Eijkemans MJ, Habbema JD. Application of shrinkage techniques in logistic regression analysis: a case study. Statistica Neerlandica. 2001 Mar;55(1):76-88.

14.	Moons KG, Kengne AP, Grobbee DE, Royston P, Vergouwe Y, Altman DG, Woodward M. Risk prediction models: II. External validation, model updating, and impact assessment. Heart. 2012 May 1;98(9):691-8.

15.	Moons KG, Altman DG, Vergouwe Y, Royston P. Prognosis and prognostic research: application and impact of prognostic models in clinical practice. Bmj. 2009 Jun 4;338:b606.

16.	Eckart A, Hauser SI, Kutz A, Haubitz S, Hausfater P, Amin D, Amin A, Huber A, Mueller B, Schuetz P. Combination of the National Early Warning Score (NEWS) and inflammatory biomarkers for early risk stratification in emergency department patients: results of a multinational, observational study. BMJ open. 2019 Jan 1;9(1):e024636.

17.	DRYAD [internet]. DRYAD; [cited 2020-09-20]. Available from: https://datadryad.org/stash/

18.	Harrell Jr FE. Regression modeling strategies: with applications to linear models, logistic and ordinal regression, and survival analysis. Springer; 2015 Aug 14.

19.	Python [internet]. Python; [cited 2020-09-20]. Available from: https://www.python.org/

20.	Scikit-learn, sklearn.linear_model.LogisticRegression [internet]. Scikit-learn; [cited 2020-09-20]. Available from: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html

21.	Scikit-learn, sklearn.model_selection.train_test_split [internet]. Scikit-learn; [cited 2020-09-20]. Available from: https://scikit-learn.org/stable/modules/generated/sklearn.model_selection.train_test_split.html

