# AEMLScotland-Final-2025.07.07

**Predicting Accidents & Emergencies (A&E) Waiting Time Adherence: A Supervised Machine Learning Approach to Meeting the 4-Hour Standard in Scottish Hospitals**  
Author: Jason V. Alacapa | MSc Data Science for Health and Social Care  
University of Edinburgh | Submitted: 7 July 2025

---

## 📄 Overview

This repository contains the final R scripts and supporting datasets used in the MSc dissertation analyzing monthly adherence to the Scottish Government’s 4-hour A&E waiting time standard. The study applies supervised machine learning models (GLM, Random Forest, XGBoost) to board-level data from Public Health Scotland (PHS), spanning January 2018 to December 2023.

---

## 📁 Repository Contents

### 📂 Public Health Scotland Datasets

The following datasets were **directly downloaded from the official Public Health Scotland open data portal** in May 2025. No modifications or cleaning were performed on these raw files prior to integration.

- `2025-05-06-ae-monthly-attendance-and-waiting-times.xlsx`
- `2025-05-20-ed-weekly-attendance-and-waiting-times.xlsx`
- `2025-05-06-dischargedestination.xlsx`
- `2025-05-06-multipleattendances.xlsx`
- `2025-05-06-multipleattendances-demographics.xlsx`
- `2025-05-06-referralsource.xlsx`
- `2025-05-06-whenpeopleattend-arrivalhour.xlsx`
- `2025-05-06-whenpeopleattend-dayofweek.xlsx`
- `2025-05-06-whenpeopleattend-inoutofhours.xlsx`
- `2025-05-06-whoattends-agegroup.xlsx`
- `2025-05-06-whoattends-deprivation.xlsx`
- `2025-05-06-whoattends-sex.xlsx`

These datasets were integrated into the modeling pipeline and transformed programmatically in R.

---

### 📊 R Scripts

- `AE2025_1_DataPrep_EDA_2025.07.07.R` – Exploratory data analysis and initial transformation  
- `AE2025_2_DataPrep_Merging_2025.07.07.R` – Merging and full outer joins to create the wide-format dataset  
- `AE2025_3_DataPrep_Modeling_2025.07.07.R` – Feature engineering, VIF filtering, binary outcome creation  
- `AE2025_4_Analysis_Descriptive_2025.07.07.R` – Descriptive summaries and data visualization  
- `AE2025_5_Modeling_2025.07.07.R` – Supervised ML modeling (GLM, RF, XGBoost), evaluation, and plotting  

---

## 🛠 Technical Notes

- Models trained using `caret`, `ranger`, `xgboost`, `yardstick`, `ggplot2`, and `patchwork`  
- COVID phase variables coded according to Teoh et al. (2025), reflecting operational disruptions in five pandemic phases  
- Evaluation metrics included: Accuracy, Precision, Recall, F1-Score, and ROC-AUC  
- Outputs generated during runtime and saved as `.csv` and `.png` files  

---

## 🔓 License & Use

All materials in this repository are shared under a [CC BY-NC 4.0 license](https://creativecommons.org/licenses/by-nc/4.0/).  
This repository is intended for academic, research, and non-commercial use only.

---

## 📬 Contact

Jason V. Alacapa  
📧 s2464728@ed.ac.uk  
🔗 [LinkedIn](https://www.linkedin.com/in/jasonalacapa/)
