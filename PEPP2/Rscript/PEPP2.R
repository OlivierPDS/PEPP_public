# LOAD PACKAGES -----------------------------------------------------------
library (tidyverse) #collection of R packages (dplyr, tidyr, stringr, purrr, forcats etc.)
library (magrittr) #pipes %>% 
library(glue) #dynamic paste function
library (haven) #work with spss files
library(readxl) #work with excel files
library(labelled) #work with labelled data
library (lubridate) #work with dates and times

# VARIABLES OF INTEREST --------------------------------------------------------
## Sociodemographics
SD_num <- c('ageentry', 'FIQ', 'holltotp', 'EDUC_num')
SD_cat <- c('gender', 'Vmin', 'NEET', 'INDPT', 'REL')
SD <- c('DOB', SD_num, SD_cat)

## Clinical information
CHRONO <- c('date_pr','dt2stpsy', 'onset', 'datecont', 'doe')
CLIN_num <- c('ageonset', 'duponset', 'dui', 'conv', 'prevTX_dur', 'prevAP_dur')
CLIN_cat <-c('prev_EP', 'evercont', 'ever_ap', 'txiscm', 'mode', 'CAYRconv', 'ip_op', 'referral', 'Txsitn', 'transfR') #'inRCT', 'RCTgroup'
CLIN <- c(CHRONO, CLIN_num, CLIN_cat)

## Diagnosis
DXII <- paste0('secdx', c(1, 3:6))
DXIIpc <- paste0('secdx', c(1, 3:6), 'pc')
DX <- c('dx_0', 'dx_b2', 'dx_1year', 'dx2', DXII, DXIIpc, 'SUD')

## Cognition
COGSTATE <- c('verbm_z', 'wm_z', 'ef_z', 'sop_z', 'vism_z', 'va_z', 'sc_z')
COG <- c('Battery', COGSTATE)

## Functioning
PAS <- c('PAS_c', 'PAS_ea', 'PAS_la', 'PAS_a', 'PAS_tot2', 'PAS_tot3', 'PAS_tot4')
FUNC_cat <- c('vismin', 'EDUC_cat', 'newwork', 'othwork', 'newliving', 'othliving', 'marital')
EDUC <- paste('EDUC', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
WRK1 <- paste('WRK1', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
HOUS1 <- paste('HOUS1', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
HOUS2 <- paste('HOUS2', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SOFAS <- paste('SOFAS', c(0, 12, 24), sep = '_')
FUNC <- c(PAS, FUNC_cat, 'Years_of_education', EDUC, WRK1, HOUS1, HOUS2, SOFAS)

## Psychopathology
SAPS <- paste('SAPS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SANS <- paste('SANS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
HAS <- paste('HAS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
CDS <- paste('CDS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
YMRS <- paste('YMRS', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')

## Insight 
SUMD1 <- paste('SUMD1', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SUMD2 <- paste('SUMD2', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SUMD3 <- paste('SUMD3', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SUMD <- c(SUMD1, SUMD2, SUMD3)

## Suicide
SCD <- paste('cd8', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')

## Remission & Recovery
PSR <- paste('PSR', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
NSR <- paste('NSR', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
JSR <- paste('JSR', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
SR_0 <- paste(c('PSR', 'NSR', 'JSR'), '0', sep = '_')
SR_24 <- paste(c('PSR', 'NSR', 'JSR'), '24', sep = '_')
SR_t <- paste(c('PSR', 'NSR', 'JSR'), 't', sep = '_')
SR_1st <- paste(c('PSR', 'NSR', 'JSR'), '1st', sep = '_')
SR_cat <- c(PSR, NSR, JSR, SR_t, SR_1st, 'JSR_by3', 'RECOV_24')
SR <- c(SR_cat, 'JSR_24C')

## Medication
adh <- paste('comp', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
ord <- paste0('txm', c(0, 1, 2, 3, 6, 9, 12, 18, 24), 'co')
CPZw <- paste('CPZw', c(0, 1, 2, 3, 6, 9, 12, 18, 24), sep = '_')
RX_0 <- c('comp_0', 'txm0co', 'CPZw_0')
RX_24 <- c('comp_24', 'txm24co', 'CPZw_24')
RX <-  c(ord, adh, CPZw)
# AP_yn <- starts_with('anti')
# AP_type <- starts_with('atype')

## Trajectory 
# C <- c('C_SOFAS', 'C_SAPS', 'C_SANS')
# CP <- c('CP1_SOFAS', 'CP2_SOFAS', 'CP1_SAPS', 'CP2_SAPS', 'CP1_SANS', 'CP2_SANS', 'CP3_SANS')

## Summary
SX_0 <- paste(c('SAPS', 'SANS', 'HAS', 'CDS', 'YMRS','SUMD1', 'SUMD2', 'SUMD3'), '0', sep = '_')
SX_24 <- paste(c('SAPS', 'SANS', 'HAS', 'CDS', 'YMRS','SUMD1', 'SUMD2', 'SUMD3'), '24', sep = '_')
SX <- c(SAPS, SANS, HAS, CDS, YMRS, SUMD)

## Miscellaneous
t <- paste0('t', c(0, 1, 2, 3, 6, 9, 12, 18, 24))
# items <- sap1_0:umd_6b_24

## Categorical variables to recode as factors
cat <- c(SD_cat, WRK1, HOUS1, HOUS2, CLIN_cat, FUNC_cat, DX, SR_cat, ord)

# DATABASE ----------------------------------------------------------------
OG_df <- paste(getwd(), 'Data', 'Master', 'Merged_25Feb2022.sav', sep = '/') %>%
  read_spss()

CORS_df <- paste(getwd(), 'Data', 'Master', 'Cors-Demo_March 2020.sav', sep = '/') %>%
  read_spss() %>% 
  rename(EDUC_cat = educ)

COG_df <- paste(getwd(), 'Data', 'Master', '2021-09-16_neuropsyc_PEPP.sav', sep = '/') %>% 
  read_spss() %>% 
  rename(pin = PEPP_ID)

CPZeq_df <- paste(getwd(), 'Data', 'Master', 'CPZeq_2016-10-17.xlsx', sep = '/') %>% 
  read_excel(na = '.')

database <- list(OG_df, CORS_df, COG_df, CPZeq_df) %>% 
  reduce(left_join, by = 'pin', suffix = c('', '.y')) %>% #use full_join to keep all observations
  select(-ends_with('.y')) #drop duplicate variables

## Labels
labels_OG <- look_for(OG_df)
labels_CORS <- look_for(CORS_df)
labels_COG <- look_for(COG_df)
labels_CPZeq <- look_for(COG_df)
labels_db <- look_for(database)

# LOAD PEPP2 -------------------------------------------------------------------
## Working session
.ws <- list.files(
  paste(getwd(), 'Rproj', sep = '/'), full.names = TRUE) %>%
  file.info() %>%
  filter(isdir == FALSE) %>% 
  slice_max(mtime) %>% # get the last modified workspace
  rownames()

load(.ws)

## Dataset 
PEPP2_df <- list.files(
  paste(getwd(), 'Data', 'SAV', sep = '/'), full.names = TRUE) %>%
  file.info() %>%
  filter(isdir == FALSE) %>% 
  slice_max(mtime) %>% # get the last modified file
  rownames() %>%
  read_sav()

# PREPROCESSING ---------------------------------------------------------
## Select variables
PEPP2_df <- database %>%
  select('pin',
    c('Txsitn', 'Transfcat', 'referral', 'txordb', 'txordm1', 'secdx', 'CRsofas12'),
    matches(paste('educ(b', '2', '3', '6', '9', '12', '18', '24)?$', sep = '|')),
    matches(paste('employ(b', '1', '2', '3', '6', '9', '12', '18', '24)$', sep = '|')),
    starts_with('livetyp'),
    starts_with('livewho'),
    num_range('secdx', 3:6),
    num_range('secdx', c(1, 3:6), 'pc'),
    starts_with('datedue'),
    starts_with('dsfs'),
    num_range('pa', 1:17),
    matches('^sofas(_)?(b|\\d)'),
    matches('sap(\\d)+_(.)+'),
    matches('san(\\d)+_(.)+'),
    matches('ha(\\d)+_(.)+'),
    matches('cd(\\d)+_(.)+'),
    matches('ymrs(\\d)+(_)?(.)+'),
    matches('(u)?md_(.)+_.'),
    matches('CogState_.*_z$'),
    any_of(c(SD, CLIN, DX, FUNC, SX, SR, COG, RX)))

## Add variables  
  PEPP2_df <- database %>%
  select(c('dx_b', 'comp_b'),
    any_of(c(SD, CLIN, DX, FUNC, SX, SR, COG, RX)),
    -any_of(c(as.vector(names(PEPP2_df)))), #Prevent duplicate variables
    'pin') %>% 
  merge(PEPP2_df, ., all.x = TRUE, by = 'pin')

## Remove variables
# PEPP2_df <- PEPP2_df %>%
#   select(-<varname>, -<vecname>)

## Retrieve variable & value labels
  for (var in names(PEPP2_df)) {
    if (var %in% names(database)) {
        var_label(PEPP2_df[[var]]) <- var_label(database[[var]])
        val_labels(PEPP2_df[[var]]) <- val_labels(database[[var]])
    }
  }

labels_PEPP2 <- look_for(PEPP2_df)

## Rename variables (inconsistent / > 8 characters)
names(PEPP2_df) %>% grep('.{9,}', ., value = TRUE)
names(PEPP2_df) %>% grep('_b$', ., value = TRUE)

PEPP2_df <- PEPP2_df %>%
  setNames(., gsub('_b$', '_0', names(.))) %>%
  rename(transfR = Transfcat) %>%
  rename(txm0co = txordb, txm1co = txordm1) %>%
  rename_with(~ EDUC, matches(paste('educ(b', '2', '3', '6', '9', '12', '18', '24)?$', sep = '|'))) %>%
  rename_with(~ WRK1, matches(paste('employ(b', '1', '2', '3', '6', '9', '12', '18', '24)$', sep = '|'))) %>%
  rename_with(~ HOUS1, starts_with('livetyp')) %>%
  rename_with(~ HOUS2, starts_with('livewho')) %>%
  rename_with(~ DXII, c('secdx', num_range('secdx', 3:6))) %>%
  rename_with(~ DXIIpc, num_range('secdx', c(1, 3:6), 'pc')) %>%
  rename_with(~ SOFAS, starts_with('sofas')) %>%
  rename_with(~ c('ymrs10_12', 'ymrs10_18', 'ymrs10_24'), c('ymrs1012', 'ymrs1018', 'ymrs1024')) %>%
  rename_with(~ c('ymrs11_12', 'ymrs11_18', 'ymrs11_24'), c('ymrs1112', 'ymrs1118', 'ymrs1124')) %>%
  rename_with(~ unlist(map(c(0, 1, 2, 3, 6, 9, 12, 18, 24), ~ paste0('umd', c(1, 2, '2b', 3, '3b', 4, '4b', 5, '5b', 6, '6b'), '_', .x))), matches('(u)?md_(.)+_.')) %>%
  setNames(., str_remove(names(.), 'CogState_'))

### Recode missings
PEPP2_df <- PEPP2_df %>%
  mutate(across(all_of(SOFAS), ~ na_if(., 0))) %>%  #recode 'no info' as missing
  # mutate(across(starts_with('umd'), ~ na_if(., 99))) %>%
  # mutate(across(starts_with('umd'), ~ na_if(., 77))) %>%
  # mutate(across(starts_with('umd'), ~ na_if(., 0))) %>%  #recode "Cannot be assessed/item not relevant" as missing
  # mutate(across(c('CAYRconv', all_of(ord)), ~ coalesce(.x, 0))) %>%  #replace missing with 0
  # mutate(across(where(is.Date), ~ replace(.x, .x < 1900-01-01, NA))) %>% #replace nonsensical date with NA
  # mutate(SOFAS_12 = coalesce(SOFAS_12, CRsofas12)) #replace missing on SOFAS_12 with data from case review

### Reformat variables
PEPP2_df <- PEPP2_df %>%
  mutate(mutate(across(all_of(SOFAS), ~ as.numeric(as.character(.)))))
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) #round numeric variables
  # modify_at(cat, ~ as_factor(levels = 'default')) #recode categorical variables using labels where available, otherwise the values

## Relocate variables 
PEPP2_df <- PEPP2_df %>% 
  select(c('pin', 'Txsitn'),
         all_of(c(SD, CLIN, DX, COG, FUNC)), 
         starts_with('datedue'), starts_with('dsfs'),
         starts_with(c('pa', 'sap', 'san', 'ha', 'cd', 'ymrs', 'umd')),
         starts_with('miss'),
         all_of(c(SX, SR, RX)))

# PEPP2_df <- PEPP2_df %>%
  # relocate(<var>, .before = <var>) %>%
  # relocate(<var>, .after = <var>)

# COMPUTE VARIABLES ------------------------------------------------------------
## Age (today)
PEPP2_df <- PEPP2_df %>% 
  mutate(age = as.numeric(difftime(Sys.Date(), DOB, 'days') / 365.25))

## Ethnicity -visible minority
PEPP2_df <- PEPP2_df %>% 
  mutate(Vmin = vismin) %>%
  mutate(Vmin = case_when(
    Vmin == 0 ~ 0, 
    Vmin >= 1 ~ 1)) %>% 
  mutate(Vmin = labelled(Vmin, labels = c('not a visible minority' = 0, 'visible minority' = 1), label = c('Visible minority status')))

## Education
PEPP2_df <- PEPP2_df %>% 
  mutate(EDUC_num = EDUC_cat) %>%
  mutate(EDUC_num = case_when(
    EDUC_num == 15 ~ 16, 
    EDUC_num == 0 ~ 17.5, 
    EDUC_num == 6 ~ 18,
    EDUC_num == 16 ~ 19.5,
    EDUC_num == 17 ~ NA, 
    EDUC_num == 18 ~ NA)) %>% 
  mutate(EDUC_num = coalesce(EDUC_num, Years_of_education)) #fill missings with data from cognitive assessment (3-6 months follow-up)

## Employement -NEET
PEPP2_df <- PEPP2_df %>% 
  mutate(NEET = newwork) %>%
  mutate(NEET = case_when(
    NEET == 1 ~ 0, 
    NEET == 2 ~ 0, 
    NEET == 4 ~ 0, 
    NEET == 5 ~ 0, 
    NEET == 0 ~ 1,
    NEET == 3 ~ 1, 
    NEET == 6 ~ 1)) %>% #Approximative, should be recoded with information in 'othwork'
  mutate(NEET = labelled(NEET, labels = c('non-NEET' = 0, 'NEET' = 1), label = 'Not in education, employement or training status'))

## Housing -independent living in community
PEPP2_df <- PEPP2_df %>% 
  mutate(INDPT = newliving) %>%
  mutate(INDPT = case_when(
    INDPT == 1 & str_detect(othliving, paste('parent', 'father', 'dad', 'step[fdm]', 'mother', 'm[ou]m', 'grand', 'aunt', 'uncle', 'family', 'famille', sep = '|'))  ~ 0, #'family' is debatable -could be coded NA
    INDPT == 3 ~ 0,
    INDPT == 4 ~ 0,
    INDPT == 5 & str_detect(othliving, paste('shelter', 'Refuge', 'Center', 'residence', sep = '|'))  ~ 0,
    INDPT == 1 & str_detect(othliving, paste('parent', 'father', 'dad', 'step[fdm]', 'mother', 'm[ou]m', 'grand', 'aunt', 'uncle', 'family', 'famille', sep = '|'), negate =  TRUE)  ~ 1,
    INDPT == 2 ~ 1,
    INDPT == 5 & str_detect(othliving, paste('shelter', 'Refuge', 'Center', 'residence', sep = '|'), negate = TRUE)  ~ 1,
    INDPT == 6 ~ 1, 
    .default = NA)) %>% 
  mutate(INDPT = labelled(INDPT, labels = c('dependent' = 0, 'independent' =1), label = 'Independent comunity living status'))

## Relationship
PEPP2_df <- PEPP2_df %>% 
  mutate(REL = marital) %>%
  mutate(REL = case_when(
    REL == 0 ~ 0,
    REL == 4 ~ 0,
    REL == 5 ~ 0,
    REL == 6 ~ 0,
    REL == 1 ~ 1,
    REL == 2 ~ 1,
    REL == 3 ~ 1, 
    .default = NA)) %>% 
  mutate(REL = labelled(REL, labels = c('single' = 0, 'not single' = 1), label = 'Relationship status'))

## Previous episode
PEPP2_df <- PEPP2_df %>% 
  mutate(prev_EP = case_when(dt2stpsy < onset ~ 1, 
                             dt2stpsy >= onset ~ 0)) %>% 
  mutate(prev_EP = labelled(prev_EP, labels = c('no' = 0, 'yes' = 1), label = 'Previous episode of psychosis'))

## Treatment duration
PEPP2_df <- PEPP2_df %>%   
  mutate(prevTX_dur = as.numeric(difftime(doe, datecont, units = 'days'))) %>% 
  mutate(prevTX_dur = case_when(prevTX_dur < 0 ~ 0, .default = prevTX_dur))

## Antipsychotics duration
PEPP2_df <- PEPP2_df %>% 
  mutate(prevAP_dur = case_when(prevTX_dur == 0 ~ 0, 
                                ever_ap == 0 | evercont !=1 | txiscm == 1 ~ 0,
                                evercont == 1 ~ prevTX_dur))

## Mode of onset
PEPP2_df <- PEPP2_df %>% 
  mutate(conv = as.numeric(difftime(onset, date_pr, units = 'days'))) %>% 
  mutate(mode = case_when(conv <= 30 ~ 1,
                          conv > 30 ~ 2)) %>% 
  mutate(mode = labelled(mode, labels = c('acute' = 1, 'insidious' = 2), label = 'Mode of onset'))

## SUD dx
.scid <- c(seq(from = 303.90, to = 305.90, 0.1), 999.99)

PEPP2_df <- PEPP2_df %>% 
  {pmap_dfc(list(
    select(., all_of(DXII)), 
    select(., all_of(DXIIpc)), 
    1:5), 
    \(x, y, z) {mutate(., 'SUD{z}' := case_when(
      !is.na(x) & (x %in% .scid) & (y == 1) ~ 2,
      !is.na(x) & (x %in% .scid) & (y == 2) ~ 1,
      !is.na(x) & !(x %in% .scid) ~ 0,
      TRUE ~ NA)) %>%
        select(contains('SUD'))})} %>% 
  mutate(SUD = case_when(
    if_any(everything(), ~ .x == 1) ~ 1,
    if_any(everything(), ~ .x == 2) ~ 2,
    if_any(everything(), ~ .x == 0) ~ 0,
    TRUE ~ NA)) %>% 
  select('SUD') %>% 
  mutate(SUD = labelled(SUD, labels = c('no SUD' = 0, 'current SUD' = 1, 'past SUD' = 2), label = 'Substance use disorder diangosis')) %>% 
  bind_cols(PEPP2_df, .)

## Total scores 
### PAS
PEPP2_df <- PEPP2_df %>% 
  mutate(PAS_c = rowSums(across(num_range('pa', 1:4))) / (rowSums(!is.na(across(num_range('pa', 1:4)))) * 6)) %>% 
  mutate(PAS_ea = rowSums(across(num_range('pa', 5:9))) / (rowSums(!is.na(across(num_range('pa', 5:9)))) * 6)) %>% 
  mutate(PAS_la = rowSums(across(num_range('pa', 10:14))) / (rowSums(!is.na(across(num_range('pa', 10:14)))) * 6)) %>% 
  mutate(PAS_a = rowSums(across(num_range('pa', 15:17))) / (rowSums(!is.na(across(num_range('pa', 15:17)))) * 6)) %>% 
  mutate(PAS_tot2 = rowMeans(across(c(PAS_c, PAS_ea)))) %>% 
  mutate(PAS_tot3 = rowMeans(across(c(PAS_c, PAS_ea, PAS_la)))) %>%
  mutate(PAS_tot4 = rowMeans(across(c(PAS_c, PAS_ea, PAS_la, PAS_a)))) %>% 
  mutate(across(all_of(PAS), ~ round(.x, digits = 2)))

### Sx
for (i in c(0, 1, 2, 3, 6, 9, 12, 18, 24)) {
  PEPP2_df <- PEPP2_df %>%
    mutate(!!str_c('SAPS_', i) := rowSums(across(all_of(str_c(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), i))))) %>%
    mutate(!!str_c('SANS_', i) := rowSums(across(all_of(str_c(c('san8_', 'san13_', 'san17_', 'san22_'), i))))) %>%
    mutate(!!str_c('HAS_', i) := rowSums(across(all_of(str_c('ha', c(1:13), '_', i))))) %>%
    mutate(!!str_c('CDS_', i) := rowSums(across(all_of(str_c('cd', c(1:9), '_', i))))) %>%
    mutate(!!str_c('YMRS_', i) := rowSums(across(all_of(str_c('ymrs', c(1:11), '_', i))))) %>% 
    mutate(!!str_c('SUMD1_', i) := rowMeans(across(all_of(str_c(c('umd1_', 'umd2_', 'umd2b_'), i))))) %>% 
    mutate(!!str_c('SUMD2_', i) := rowMeans(across(all_of(str_c(c('umd3_', 'umd3b_', 'umd4_', 'umd4b_'), i))))) %>% 
    mutate(!!str_c('SUMD3_', i) := rowMeans(across(all_of(str_c(c('umd5_', 'umd5b_', 'umd6_', 'umd6b_'), i)))))
}

## Individually varying time of observation 
### Replace missing in date of assessment with due date 
DMY_df <- PEPP2_df %>% 
  {list(select(., starts_with('dsfs')), select(., all_of(SAPS)), select(., 'dsfs_0', starts_with('datedue')), c(0, 1, 2, 3, 6, 9, 12, 18, 24))} %>% 
  pmap_dfc(
    \(x, y, z, n) {mutate(PEPP2_df, 'dmy_{n}' := case_when(!is.na(y) ~ coalesce(x, z))) %>% 
        select(starts_with('dmy'))})

### Compute individually varying time of observation t0:t24 
T_df <- PEPP2_df %>%
  select(., starts_with('dsfs')) %>% 
  {map2_dfc(., 
            c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(x, y) {mutate(., 't{y}' := as.numeric(difftime(x, dsfs_0, 'days'))) %>%
                select(glue('t{y}'))})}

### Replace missing with mean timescores (Need to subset df first)
TSCORES_df <- T_df %>% 
  {pmap_dfc(list(., 
                 select(PEPP2_df, all_of(SAPS)),
                 c(0, 1, 2, 3, 6, 9, 12, 18, 24)), 
            \(x, y, z) {mutate(., 'tscores_{z}' := case_when(!is.na(y) ~ replace_na(x, mean(x, na.rm = TRUE)))) %>% 
                select(glue('tscores_{z}'))
            })}

PEPP2_df <- PEPP2_df %>% 
  bind_cols(T_df)
# merge(PEPP2_df, TSCORES_df, by = 'pin', all.x = TRUE)

## Sx remission
### Positive symptoms remission
PSR_df <-  map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(t) mutate(PEPP2_df, 'PSR_{t}' := case_when(
              if_all(paste0(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), t), ~ .x < 3) ~ 1,
              if_any(paste0(c('sap7_', 'sap20_', 'sap25_', 'sap34_'), t), ~ .x > 2) ~ 0,
              TRUE ~ NA))) %>% 
  map_dfc(\(x) select(x, starts_with('PSR'))) %>% 
  bind_cols(select(PEPP2_df, 'pin'), T_df, .)

PSR_df <- PSR_df %>% 
  pivot_longer(cols = starts_with('PSR'), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(PSR_t = time[which(PSR == 1)[1]]) %>% 
  ungroup()

PSR_df <- PSR_df %>%
  pivot_wider(names_from = time,
              names_glue = 'PSR_{time}',
              values_from = PSR)

PSR_df <- PSR_df %>% rowwise %>% 
  mutate(PSR_1st = map_dbl(PSR_t, \(x) ifelse(!is.na(x), get(glue('t{x}')), NA))) %>% 
  select(-all_of(t))
           
### Negative symptoms remission
NSR_df <- map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
            \(t) mutate(PEPP2_df, 'NSR_{t}' := case_when(
              if_all(paste0(c('san8_', 'san13_', 'san17_', 'san22_'), t), ~ .x < 3) ~ 1,
              if_any(paste0(c('san8_', 'san13_', 'san17_', 'san22_'), t), ~ .x > 2) ~ 0,
              TRUE ~ NA))) %>% 
  map_dfc(\(x) select(x, starts_with('NSR'))) %>% 
  bind_cols(select(PEPP2_df, 'pin'), T_df, .)

NSR_df <- NSR_df %>% 
  pivot_longer(cols = starts_with('NSR'), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(NSR_t = time[which(NSR == 1)[1]]) %>% 
  ungroup()

NSR_df <- NSR_df %>%
  pivot_wider(names_from = time,
              names_glue = 'NSR_{time}',
              values_from = NSR)

NSR_df <- NSR_df %>% 
  mutate(NSR_1st = map_dbl(NSR_t, \(x) ifelse(!is.na(x), get(glue('t{x}')), NA))) %>% 
  select(-all_of(t))
  
### Joint symptoms remission
JSR_df <- merge(PSR_df, NSR_df, all = TRUE)

JSR_df <- map(c(0, 1, 2, 3, 6, 9, 12, 18, 24),
               \(t) mutate(JSR_df, 'JSR_{t}' := case_when(
                 if_all(paste0(c('PSR_', 'NSR_'), t), ~ .x == 1) ~ 1,
                 if_any(paste0(c('PSR_', 'NSR_'), t), ~ .x == 0) ~ 0,
                 TRUE ~ NA))) %>% 
  map_dfc(\(x) select(x, starts_with('JSR'))) %>% 
  bind_cols(select(PEPP2_df, 'pin'), T_df, .)

JSR_df <- JSR_df %>% 
  pivot_longer(cols = starts_with('JSR'), 
               names_to = c('.value', 'time'),
               names_pattern = '(.+)_(\\d+)') %>% 
  group_by(pin) %>% 
  mutate(JSR_t = time[which(JSR == 1)[1]]) %>% 
  ungroup()

JSR_df <- JSR_df %>%
  pivot_wider(names_from = time,
              names_glue = 'JSR_{time}',
              values_from = JSR)

JSR_df <- JSR_df %>% 
  mutate(JSR_1st = map_dbl(JSR_t, \(x) ifelse(!is.na(x), get(glue('t{x}')), NA))) %>% 
  mutate(JSR_by3 = case_when(
    if_any(num_range('JSR_', 1:3), ~ .x == 1) ~ 1,
    if_all(num_range('JSR_', 1:3), ~ .x == 0) ~ 0,
    TRUE ~ NA)) %>% 
  rowwise() %>% 
  mutate(JSR_24C = sum(c_across(all_of(JSR)), na.rm = TRUE)) %>% #INCLUDE NA
  select(-all_of(t))
  
PEPP2_df <- list(PEPP2_df, PSR_df, NSR_df, JSR_df) %>% 
  reduce(merge, all = TRUE) %>% 
  mutate(across(all_of(c(PSR, NSR, JSR)), ~ labelled(.x, labels = c('no remission' = 0, 'remission' = 1), label = 'Remission of positive, negative or both positive and negative symptoms at each timepoints'))) %>% 
  mutate(JSR_by3 = labelled(JSR_by3, labels = c('no remission' = 0, 'remission' = 1), label = 'Joint remission of positive and negative symptoms by month 3'))

## Recovery
PEPP2_df <- PEPP2_df %>% 
  mutate(RECOV_24 = case_when(JSR_24 == 1 & SOFAS_24 >= 61 ~ 1, 
                              JSR_24 == 0 | SOFAS_24 < 61 ~ 0, 
                              .default = NA)) %>% 
  mutate(RECOV_24 = labelled(RECOV_24, labels = c('no recovery' = 0, 'recovery' = 1), label = 'Recovery status at month 24'))


## Data transformations (Need to subset df first)
### Z transformation
SDz_df <- SD_df %>%
  mutate(across(all_of(c(SD_num, CLIN_num, SX, CPZw)), 
                ~ scale(.x), 
                .names = '{.col}_z'),
         .keep = "none")

### Centering
SDc_df <- SD_df %>%
  mutate(across(all_of(c(SD_num, CLIN_num, SX, CPZw)), 
                ~ scale(.x, scale = FALSE)
                .names = '{.col}_c'),
         .keep = "none")

# SUBSET DATASET ----------------------------------------------------------
## Inclusion/exclusion criteria (to edit based on your own project)
SD_df <- PEPP2_df %>%
  filter(pin <= 857) %>% #75
  filter(ageentry >= 14 & ageentry < 36) %>% #1
  filter(FIQ >= 70 | is.na(FIQ)) %>% #7
  filter(dx_b2 !=3 | is.na(dx_b2)) %>% #5
  filter(prev_EP == 0 | is.na(prev_EP)) %>% #35
  filter(prevAP_dur < 30 | is.na(prevAP_dur)) %>% #7
  filter(case_when(prevTX_dur >= 30 & is.na(prevAP_dur) ~ FALSE, .default = TRUE)) %>% #59
  filter(is.na(Txsitn)) #43

SD_df <- SD_df %>% 
  select('pin', all_of(c(SD, CLIN, 'dx_b2', 'SUD', COGSTATE, FUNC, SX, SR, RX)))

# RESHAPE DATASET ---------------------------------------------------------
## Wide to long form
long_df <- SD_df %>% 
  pivot_longer(
    cols = all_of(c(SX, CPZw)), 
    names_to = c('.value', 'time'),
    names_pattern = '(.+)_(\\d+)')


# MISSING DATA ANALYSES ---------------------------------------------------
## Little's MCAR test (only test if MCAR, not if normally distributed)
library(MissMech)
SD_df %>% 
  select(all_of(SOFAS)) %>% 
  TestMCARNormality()

## Average percentage of missing data 
library(misty)
PEPP2_df %>% 
  # select(all_of(SOFAS)) %>% 
  na.descript()

library(VIM)
SD_df %>% 
  select(all_of(SOFAS)) %>% 
  aggr() %>% 
  summary()

## Percent of missing data per variable 
miss_db <- database %>% 
  map_dfc(\(x) sum(is.na(x)) / nrow(.) * 100) %>% 
  pivot_longer(cols = everything(),
               values_to = '%miss')

## Missing count per outcome 
PEPP2_df <- PEPP2_df %>% #or rowwise() and sum()
  mutate(miss_SOFAS = rowSums(is.na(across(all_of(SOFAS))))) %>%
  mutate(miss_SAPS = rowSums(is.na(across(all_of(SAPS))))) %>%
  mutate(miss_SANS = rowSums(is.na(across(all_of(SANS)))))

# DESCRIPTIVES STATS ------------------------------------------------------
library(psych) #A package for psychological research

## Dataframe description
# PEPP2_df %>%
  # str()
  # describe()
  # describeBy(group = 'gender') #categorical var

## Sociodemographics
library(gtsummary)
library(flextable)
  
  desc_tb <- SD_df %>% 
  select(all_of(c(SD_num, SD_cat, 'ageonset', 'duponset', 'dx_b2', 'SUD', 'PAS_tot2', 'SOFAS_0', SX_0, 'CPZw_0')), -all_of(c(SUMD2, SUMD3))) %>% 
    mutate(across(where(is.labelled), ~ as_factor(., levels = 'default'))) %>% 
    mutate(dx_b2 = fct_drop(dx_b2)) %>% 
    mutate(gender = fct_collapse(gender, male = 'Male', female = c('Female', 'other'))) %>% 
  tbl_summary(
    label = list(
      ageentry ~ 'Age',
      gender ~ 'Sex',
      Vmin ~ 'Ethnicity',
      FIQ ~ 'IQ',
      EDUC_num ~ 'Education ',
      NEET ~ 'Employement',
      INDPT ~ 'Housing',
      REL ~ 'Relationship',
      holltotp ~ 'SES',
      PAS_tot2 ~ 'PAS',
      ageonset ~ 'Onset',
      duponset ~ 'DUP',
      dx_b2 ~ 'Diagnosis',
      SUD ~ 'SUD',
      SOFAS_0 ~ 'SOFAS',
      SAPS_0 ~ 'SAPS',
      SANS_0 ~ 'SANS',
      HAS_0 ~ 'HAS',
      CDS_0 ~ 'CDS',
      YMRS_0 ~ 'YMRS',
      SUMD1_0 ~ 'SUMD',
      CPZw_0 ~ 'CPZeq'),
    statistic = list(
      all_continuous() ~ '{mean} ({sd})', 
      all_categorical() ~ '{n} ({p}%)' ), 
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ 0),
    missing = 'no') %>% 
  as_flex_table()
  # theme_apa() # apply last

# SAVE DATASET -----------------------------------------------------------
write_sav(PEPP2_df, glue(getwd(), 'Data', 'SAV', 'PEPP2_{today()}.sav', .sep = '/')) #SAV file
save.image(glue(getwd(), 'Rproj', 'PEPP2_{today()}.RData', .sep = '/')) #Environment




