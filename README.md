# PEPP-Database
## Overview
- standardize preprocessing & curation of dataset
- control version of dataset
- Qc dataset and analyses
- streamline basic analyses and result reports
- share project-specific variables and analyses 
- learn R language by examples
- share documentation & ressources
- promote good data management & coding practices
- promote lab collaboration & efficiency

## Dataset Information
### [PEPP2](PEPP2/README.md)
### [PEPP10+](PEPP10+/README.md) (work in progress)

## Setup
One-time only setup:
- download & install Rstudio
- fork or clone https://github.com/OlivierPDS/PEPP-Database.git

Each time:
- open github & sync/pull origin branch
- load Rproject (i.e., PEPP2.Rproj or PEPP10.Rproj)

## Features 
### R 
- save & load working session
- read & write .sav & .xslx data
- produce variables catalog
- create vectors of interest

### Data preprocessing
- add, rename, recode, reformat & relocate variables
- subset dataset by project-specific inclusion/exclusion criteria
- reshape dataset (i.e., wide to long) 

### Data transformation
- compute total scores across timepoints 
- compute variables of interest
- z-normalization & centering

### Data analyses
- missing data analysis
- descriptive statistics

#### Future features (short- to long-term):
- trajectory analysis (MplusAutomation)
- multiple implementation (mice)
- multilevel modelling (lme4)
- multiple regressions (lm)
- power/sample size analysis (pwr)
- assumption diagnostics (plots)
- mediation analysis (lavaan)
- PCA (laavan)

## How to contribute
Instructions for contributing to the data repository:
- do not alter the master database in anyway
- document any changes made to the PEPP2/PEPP10 datasets in the R script
- do not make any "manual" changes in SPSS/excel or other softwares
- only push to origin (main branch) the final/working version of your code
- do not change variable names except to make it consistent across timepoints, or shorter than 8 characters (for Mplus users)
- naming convention to define
- if you do change variable name, change every occurence of the name across the script you are working on
- comment your code (e.g, informative title, description, explanation, warnings, instructions, reference)
- try using tidyverse or base R functions
- describe your commits

### Discussion tab:
- discuss ideas
- make request
- ask questions / help 
- share ressources & information
- report any bug & error 

