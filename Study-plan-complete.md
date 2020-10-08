---
title: Study plan
author: Arman Norouzi
bibliography: cite.bib
csl: bmcemerg.csl
---

# A Segmented Approach to Predicting Prediction Model Performance After Transfer Using Unlabelled Data

# Introduction
In medicine, healthcare professionals are confronted daily with a wide range of information that needs to be processed in order to make informed clinical decisions. To help healthcare professionals make such informed decisions, prediction models (also referred to as prediction score or prediction rules) have been implemented in healthcare (1, 2). These prediction models can be defined as statistical algorithms that predict the risk of a specific outcome in an individual based on their unlabelled predictor data (2, 3). The algorithms are capable of this task due to being trained in finding patterns between independent predictor data that have been labelled with dependent outcome data (4).

The risk that is predicted by the prediction models is generally based on multiple independent predictors and the dependent outcome could either be a disease (diagnostic model) or an event that will occur in the future (prognostic model) (3, 5). In the diagnostic model, the predicted risk can be used to reassure the patient that their symptoms are not caused by a serious disease, refer the patient to further testing or to initiate treatment (3). A great example of a diagnostic model is the Ottawa Ankle Rules (6). This prediction model helps healthcare professionals by predicting the risk for a fracture in the foot or the ankle, in patients with acute ankle injuries (6). To predict this risk, the prediction model utilizes predictor data such as bond tenderness at different locations and the inability to bear weight on the injured foot immediately after injury and in the emergency department (ED) (6). Based on the risk, the healthcare professionals can decide whether the patient is in need for a series of x-ray imaging (6).

In the prognostic model, the predicted risk can be used to choose between therapeutic options and to plan lifestyle measures within the period that has been observed (2, 7). This type of risk prediction can also be utilized to risk stratify patients in therapeutic clinical trials (8, 9). A great example of a prognostic model is the CHA2DS2-VASc score (10). This prediction model helps healthcare professionals by predicting the annual risk for developing an ischemic stroke in patients with atrial fibrillation (10). To predict this risk, the prediction model utilizes predictor data such as history for congestive heart failure, hypertension, age above 74, diabetes, stroke/transiet ischemic attack/thromboembolism, vascular disease, age between 65 and 74 and female sex (10). Based on the risk, the healthcare professionals can decide whether the patient is in need for anticoagulation treatment or not (11).

The areas of use for prediction models within the field of medicine are many and the Ottawa Ankle Rules and the CHA2DS-VASc score are just a few examples that have been placed in clinical practice. In order to develop and implement such useful models within healthcare, a few steps are needed to be carried out. These steps include model development studies, model validation studies and impact studies (12, 13). Each of these studies focuses on different aspects of the prediction model to ultimately determine whether the model is applicable in clinical practice or not.

In the first step which consists of a model development study, the aim is to develop a prediction model (12). The prediction model is developed by selecting a statistical algorithm and a development sample, consisting of relevant outcome labelled predictor data, on which the algorithm is trained on (4, 12). There are a couple of algorithms to choose from, but usually when the development sample is limited, a simpler algorithm is utilized such as logistic regression (4). When the model has been developed it usually tends to be optimistic in its predictive performance within the development sample due to data selection and overfitting (14). It is therefore important to quantify such optimism through internal validation techniques such as bootstrapping or cross-validation (7). The quantified optimism can thereafter be adjusted for by applying shrinkage or penalization to the model (15).

In the second step which consists of a model development study, the aim is to assess the predictive performance of the prediction model within a validation sample (13). The validation sample consists of new individuals that are similar but different from those used in the development sample (13). This type of validation can be used to examine how well the prediction model performs after being transferred to another hospital or country (13). Such external validation is important because internally validated prediction models tend to perform worse in a new set of individuals and if the performance is poor, the model is of no use (13).

In the third and final step which consists of a model impact study, the aim is to assess the prediction models impact, ideally in a randomised trial (13). The impact of the model is assessed in variables such as behavioural changes in healthcare professionals, patient health outcomes and cost-effectiveness of care (13). These impact studies are carried out because prediction models that have no impact are also not of any use (13).

To carry out these studies is easier said than done as problems may arise during the time in which they are carried out. One such problem may occur during the validation studies. During these studies, in order to even be able to obtain the predictive performance of the prediction model within the validation sample, one needs to have available predictor and outcome data from that sample (13). This data is not always available retrospectively and can present a problem that is both time inefficient and expensive if the data is difficult to access when colleting it prospectively. In the case of the Framingham Risk Score, where the predictor data is cheap blood tests and simple demographics and where the outcome data is cardiovascular disease within 10 years, this problem would be presented because of the outcome data only being accessible after 10 years of follow up (16).

It would therefore be desirable to have a method that can estimate the predictive performance of a prediction model when it is transferred by only utilizing unlabelled predictor data. Such a method would in theory, simplify the process of implementing prediction models in clinical practice and therefore indirectly improve behavioural changes in healthcare professionals, patient health outcomes and cost-effectiveness of care, if the impact of the prediction model is good.

At present, no such methods exist which presents a substantial knowledge gap. Therefore, the aim of this study is to develop and test a new method for predicting prediction model performance after transfer while only utilizing unlabelled data. The segmented approach that will be taken in the new method, will focus on identifying similarities in the development sample and the validation sample in order to distinguish which data belongs to which sample. Observations from the development sample that are misclassified as validation sample observations will be used to form a separate segment. This segment will be used to predict the performance of the prediction model in the validation sample.

Our hypothesis is that the segmented approach taken with the new method for estimating prediction model performance after transfer while only utilizing unlabelled data, will be as good as or better than current naive methods that utilize outcome data.

# Aim
The aim of this study was to develop and test a new method for predicting prediction model performance after transfer using unlabelled data.

# Methods and Materials
## Study design
We will conduct an analytical registry-based study. The dataset used for analysis has been made publicly available and freely reusable by the multinational, observation study by Eckert A et al in the DRYAD repository (17, 18).

## Participants
The patients enrolled in the public dataset were all seeking ED care between March 2013 and October 2014 within three tertiary care centers in the USA (Clearwater Hospital), France (Hôspital de la Salpêtrière) and Switzerland (Kontonsspital Aaura) (17). The inclusion criteria to be enrolled in the dataset were that an initial blood sample was taken, and the exclusion criteria were that the patients could not be paediatric or surgical patients (17).

## Variables
### Model predictor
The independent predictor variables that will be used to develop the prediction model and the propensity model will be respiratory rate (RR), confusion, peripheral oxygen saturation (SpO2), systolic blood pressure (BPS), heart rate and temperature.

### Model outcome
The dependent variable that will be used to develop the prediction model will be whether the participant is admitted to the intensive care unit (ICU) during their time in the hospital. The dependent variable that will be used to develop the propensity model will be the country of origin of the participant.

### Patient characteristics
To describe the participants in the dataset, we will report the variables presented in model predictors and model outcomes together with age and gender. These characteristics will be presented for the whole dataset and for each country individually.

## Sample size
The final sample size arrived at in this study will include the total number of participants with complete data. Complete data is defined as having no missing model predictors and model outcomes.

## Missing data
Because the dataset has already been filtered to contain only complete data by Eckert A et al, we will carry out a complete case analysis (17).

## Statistical methods
### Dataset
The complete dataset described in the study design will be split into three separate sets of data based on which country the data was collected from. US dataset, French dataset and Swiss dataset.

### Dataset assignment
To simulate the transfer of a prediction model from one country to another, one of the datasets will be assigned as the development sample while one of the two remaining datasets will be assigned as the validation sample. The development sample will represent data from the country in which the model is developed while the validation sample will represent data from the country in which the prediction model is transferred to.

### Sequence of analysis
Analysis in this study will be performed in the programming language python (19). The sequence of analysis performed will be prediction model development, prediction model validation, propensity model development, segment performance and approach comparison.

### Prediction model development
In the model development step, a prediction model will be developed with the development sample. The model will be developed using logistic regression as implemented in the scikit-learn package in python (20). The dependent and independent variables that will be used in the development sample to develop the prediction model have been previously specified in variables. The developed prediction model will then be used to assess the performance of the model within the country in which it was developed by predicting outcome in the development sample.

### Prediction model validation
To assess the true performance of the prediction model within the country in which it is transferred to, the developed prediction model will be used to predict outcome within the validation sample.

### Propensity model development
In the propensity model development step, the data from the development sample and the validation sample will be pooled into one sample. This aggregated sample is then used to develop a propensity model also using logistic regression. The dependent and independent variables that will be used in the aggregated sample to develop the propensity model have been previously specified in variables. The propensity model will then be used to predict which sample the data in the aggregated sample belongs to. Observations from the development sample that are misclassified as validation observations, will be used to form a separate segment.

### Segment performance
To assess the estimate of the prediction models performance within the country in which it is transferred to, the developed prediction model will be used to predict outcome within the segment previously created.

### Approach comparis
In the final approach comparison step, to assess the performance of the naive approach, we will calculate the difference in performance between the prediction model validation step and the prediction model development step. We will also assess the performance of the segmented approach, by calculating the difference in performance between the segment performance step and the prediction model development step. To assess which approach performed best, will calculate the difference between the second difference and the first difference. Empirical bootstrap will be used to estimate 95% confidence intervals (CI) around the performances and the differences. The bootstrap procedure will use 1000 bootstrap samples drawn with replacements of the same size as the original samples. The sequence of analysis will be repeated until all combinations of dataset assignment have been performed.

Example: France model in the US  
1. Develop prediction model: respiratory rate, confusion, SpO2, BPS, HR temp to predict ICU admission
2. Assess prediction model performance in French data.
3. Assess "true" prediction model performance in US data
4. Estimate the performance of the "naive" approach by calculating the difference between 3 and 2
5. Merge the predictor data from the French and US datasets and label France as 1 and US as 0
6. Develop a propensity model using the data in 5. to predict the origin of the data
7. Identify observations that the propensity model missclassified as 0 when they were 1
8. Assess the performance of the prediction model in the segment of observations identified in 7.
9. Estimate the performance of the "segmented approach" by calculating the difference between 3 and 8
10. Calculate the difference between 9 and 4 to see what approach performed best
11. Repeat 1-10 1000 times to estimate the uncertainty (confidence intervals) of the estimates in 2, 3, 4, 8, 9 and 10 (empirical bootstrap)

## Ethical considerations
### Principle of autonomy 
The dataset used in this study has been made freely reusable and citable in Dryad Digital Repository (18). Therefore, the principle of autonomy is upheld due to there not being any requirement for informed consent.

### Principle of beneficence
This study will attempt to act in the best interest of future analytical research and indirectly patients, by developing and testing a new method for predicting prediction model performance in a transfer setting while only utilizing predictor data.

### Principle of nonmaleficence
The method developed in this study will be made without the intention of harm intentionally or unintentionally. To nullify the risk of patient identification leakage, we have considered to use a dataset that has already been depersonalized and made freely reusable. By taking these actions we determine that the risk to the population is minimal.

### Principle of Justice
Due to this study being analytical, the principle of justice does not prevail. However, the data in the study will be treated equally.

### Ethical permit
Because of this study being analytical and based on a freely reusable and public database, the need for an ethical permit was not required.

# References
1.	Steyerberg EW, Moons KG, van der Windt DA, Hayden JA, Perel P, Schroter S, Riley RD, Hemingway H, Altman DG, PROGRESS Group. Prognosis Research Strategy (PROGRESS) 3: prognostic model research. PLoS Med. 2013 Feb 5;10(2):e1001381.

2. 	Moons KG, Royston P, Vergouwe Y, Grobbee DE, Altman DG. Prognosis and prognostic research: what, why, and how?. Bmj. 2009 Feb 23;338:b375.

3.  Collins GS, Reitsma JB, Altman DG, Moons KG. Transparent Reporting of a Multivariable Prediction Model for Individual Prognosis or Diagnosis (TRIPOD) The TRIPOD Statement. Circulation. 2015 Jan 13;131(2):211-9.

4. 	Deo RC. Machine learning in medicine. Circulation. 2015 Nov 17;132(20):1920-30.

5.   Riley RD, Hayden JA, Steyerberg EW, Moons KG, Abrams K, Kyzas PA, Malats N, Briggs A, Schroter S, Altman DG, Hemingway H. Prognosis Research Strategy (PROGRESS) 2: prognostic factor research. PLoS Med. 2013 Feb 5;10(2):e1001380.

6.   Shell IG, Greenberg GH, McKnight RD, Nair RC, McDowell I, Reardon M, Stewart JP, Maloney J. Decision rules for the use of radiography in acute ankle injuries: refinement and prospective validation. Jama. 1993 Mar 3;269(9):1127-32.

7.  	Steyerberg EW. Clinical prediction models: a practical approach to development, validation, and updating. Springer, 2009.

8. 	Dorresteijn JA, Visseren FL, Ridker PM, Wassink AM, Paynter NP, Steyerberg EW, van der Graaf Y, Cook NR. Estimating treatment effects for individual patients based on the results of randomised clinical trials. Bmj. 2011 Oct 3;343:d5888.

9. 	Hayward RA, Kent DM, Vijan S, Hofer TP. Multivariable risk prediction can greatly enhance the statistical power of clinical trial subgroup analysis. BMC medical research methodology. 2006 Dec 1;6(1):18.

10.	Lip GY, Nieuwlaat R, Pisters R, Lane DA, Crijns HJ. Refining clinical risk stratification for predicting stroke and thromboembolism in atrial fibrillation using a novel risk factor-based approach: the euro heart survey on atrial fibrillation. Chest. 2010 Feb 1;137(2):263-72.

11.	Kirchhof P, Benussi S, Kotecha D, Ahlsson A, Atar D, Casadei B, Castella M, Diener HC, Heidbuchel H, Hendriks J, Hindricks G. 2016 ESC Guidelines for the management of atrial fibrillation developed in collaboration with EACTS. European journal of cardio-thoracic surgery. 2016 Nov 1;50(5):e1-88.

12.	Royston P, Moons KG, Altman DG, Vergouwe Y. Prognosis and prognostic research: developing a prognostic model. Bmj. 2009 Mar 31;338.

13.	Moons KG, Kengne AP, Grobbee DE, Royston P, Vergouwe Y, Altman DG, Woodward M. Risk prediction models: II. External validation, model updating, and impact assessment. Heart. 2012 May 1;98(9):691-8.

14.	Steyerberg EW, Bleeker SE, Moll HA, Grobbee DE, Moons KG. Internal and external validation of predictive models: a simulation study of bias and precision in small samples. Journal of clinical epidemiology. 2003 May 1;56(5):441-7.

15.	Steyerberg EW, Eijkemans MJ, Habbema JD. Application of shrinkage techniques in logistic regression analysis: a case study. Statistica Neerlandica. 2001 Mar;55(1):76-88.

16.	Anderson KM, Odell PM, Wilson PW, Kannel WB. Cardiovascular disease risk profiles. American heart journal. 1991 Jan 1;121(1):293-8.

17. Eckart A, Hauser SI, Kutz A, Haubitz S, Hausfater P, Amin D, Amin A, Huber A, Mueller B, Schuetz P. Combination of the National Early Warning Score (NEWS) and inflammatory biomarkers for early risk stratification in emergency department patients: results of a multinational, observational study. BMJ open. 2019 Jan 1;9(1):e024636.

18.	DRYAD [internet]. DRYAD; [cited 2020-09-20]. Available from: https://datadryad.org/stash/

19.	Python [internet]. Python; [cited 2020-09-20]. Available from: https://www.python.org/

20.	Scikit-learn, sklearn.linear_model.LogisticRegression [internet]. Scikit-learn; [cited 2020-09-20]. Available from: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html

