# PEPP2
## Dataset overview
PEPP2_df has been created by coliging data from 4 different sources:

  name  |       id      |   years |  n | var |source|    author    |  version |format|                        description                         |
--------|---------------|---------|----|-----|------|--------------|----------|------|------------------------------------------------------------|
OG_df   |pin = 1-992    |2003-2020|764 |3770 |shared|Nicole Pawliuk|06/01/2020|.sav  |Subset of PEPP assessment protocol from baseline to month 24|
CORS_df |pin = 1-992    |2003-2020|764 |233  |shared|Nicole Pawliuk|06/01/2020|.sav  |Baseline sociodemographics and circumstance of onset        |
COG_df  |PEPP_ID = 1-850|2003-2018|1008|409  |Ydrive|CRISP lab     |16/09/2021|.xlsx |Neuropsy assesmment at month 3-6                            |
CPZeq_df|pin = 1-769    |2003-2018|769 |61   |Ydrive|Michael Bodnar|16/09/2021|.xlsx |Chlorpromazine equivalent from baseline to month 24         |

## Data Collection
![](https://github.com/OlivierPDS/PEPP-Database/blob/d760fcf3ccddbce57a2f13c18949d3badac7df68/Documentation/PEPP2_protocol.pdf?raw=TRUE)

## Data Preprocessing
- selected variables of interest (i.e., SD, clinical info, Dx, Funct, Sx, Cog, Rx)
- renamed variables with long (i.e., > 8 characters) or inconsistant names across timepoints
- recoded undefined missing values (i.e., 77, 99) into NA
- recoded categorical variables as factors
- rounded numeric variables (2 decimals)
- recoded categorical variables with unused or inconsistant levels across timepoints
- restructured dataset

## Data Transformation
- computed total scores (i.e., PAS, SAPS, SANS, CDS, HAS, YMRS & SUMD)
- compute variables of interest (e.g., antecedent, SUD, remission, recovery) 
- recoded categorical variables (collapsed levels into groups of interest) (e.g., SD)
- computed individually-varying time of observation 

