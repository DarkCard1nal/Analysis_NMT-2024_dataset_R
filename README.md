# Analysis_NMT-2024_dataset_R

_Created for the course "Models and technologies for data analytics" V. N. Karazin Kharkiv National University_

Analysis of the NMT-2024 dataset in R.

---

# Project Overview

This project aims to conduct a comprehensive data analysis using **R**, covering all essential stages of data analytics — from initial data exploration to building predictive models — with detailed interpretation at each step.

The dataset used for analysis includes **results of the National Multi-Subject Test (NMT) 2024**, officially published by the **Ukrainian Center for Educational Quality Assessment (UCEQA)**. The data includes performance scores, demographic features, and school-level information, providing valuable insights into educational trends and factors influencing academic success.

## Project Structure

### 1. Data Cleaning and Preparation
This step involves handling missing values using appropriate imputation methods depending on variable type and context. Outliers are detected and processed. Variable transformations such as normalization, standardization, and log transformations are applied where needed. Factor variables are recoded accordingly.

### 2. Exploratory Data Analysis (EDA)
We begin by exploring the dataset structure, including the number of observations and variables. Functions like `summary()`, `str()`, and `glimpse()` are used to understand variable types and distributions. Visualizations such as histograms, boxplots, and scatter plots illustrate key patterns. A correlation matrix is created to analyze variable relationships.

### 3. Hypothesis Formulation and Testing
We formulate at least three hypotheses based on patterns identified in the dataset. Statistical tests like **t-tests**, **ANOVA**, and **Chi-squared tests** are used to evaluate hypotheses, depending on variable types and assumptions. Each method's usage is explained and results interpreted.

### 4. Linear Regression Analysis
A linear regression model is built to explain a continuous outcome variable based on selected predictors. Model diagnostics include regression coefficients, R², F-statistics, and residual analysis. Feature selection and transformations are applied to optimize the model.

### 5. Logistic Regression Analysis
A binary outcome variable is selected or derived from the dataset. Logistic regression is used to model binary relationships. Model performance is assessed via **AUC**, **ROC curves**, and metrics such as accuracy and F1-score.

### 6. Machine Learning Models
We implement **Decision Tree**, **Random Forest**, and **XGBoost** models to solve both regression and classification problems, using the same variables as in linear and logistic models. **5-fold cross-validation** is applied to avoid overfitting, and **hyperparameter tuning** is performed (using `GridSearchCV` or similar tools). Model quality is evaluated using MAE, RMSE, R² for regression tasks, and Accuracy, Precision, Recall, F1-score, AUC for classification tasks.

A summary table compares all models, helping to determine the best-performing solution for each task.

## Repository Contents

- `project_nmt-2024_analysis.R` – R scripts for full analysis
- `data/` – Raw and processed datasets

---
