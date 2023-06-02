# PEPP-Database
## Description
- standardize preprocessing & curation of dataset
- control version of dataset
- Qc dataset and analyses
- streamline basic analyses 
- learn coding by examples 
- share project-specific variables and analyses 
- promote lab collaboration & efficiency

## Dataset Information
### [PEPP2](PEPP2/README.md)
### [PEPP10+](PEPP10+/README.md) (work in progress)

## Installation
One-time only setup:
- download & instal Rstudio
- download & instal GitHub desktop
- fork or clone https://github.com/OlivierPDS/PEPP-Database.git

Each time:
- load Rproject (i.e., PEPP2.Rproj or PEPP10.Rproj)
- sync/pull origin branch

## Usage 
- Compute total score for PAS, SAPS, SANS, CDS, HAS, YMRS, SUMD across times-points
- assumption diagnostic
- descriptives stats
- missing data handeling

Medium- to long-term additions:
- trajectory analyses (MplusAutomation)
- multiple implementation (mice)
- multilevel modelling (lme4)
- multiple regression (lm)
- power/sample size analyses (pwr)
- mediation analyses (lavaan)
- PCA (laavan)

## How to contribute
Instructions for contributing to the data repository:
- do not alter the master database in anyway
- document any changes made to the PEPP2/PEPP10 datasets in the R script
- do not make any "manual" changes in SPSS/excel or other softwares
- only push to origin (main branch) the final/working version of your code
- do not change variable names except to make it consistent across timepoints, or shorter than 8 characters (for Mplus users)
- naming convention to define
- if you do change a variable name, change every occurence of that variable across the script you are working on
- comment your code (e.g, concise title, description, explenation, warnings, instructions, reference)
- describe your commits concisely

### Discussion tab:
- discuss ideas 
- make request
- ask questions / help 
- report any bug, error 

