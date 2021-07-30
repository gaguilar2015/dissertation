#Install all necessary libraries
# install.packages('psych')
# install.packages('lavaan')
# install.packages('tidyverse')
# install.packages('haven')
# install.packages('foreign')
# install.packages('xlsx')
# install.packages('httr')
# install.packages('request')
# install.packages('dplyr')
# install.packages('stringr')
# install.packages('readxl')
# install.packages('janitor')
# install.packages('psych')
# install.packages('aod')
# install.packages('ResourceSelection')
# install.packages('fmsb')
# install.packages('summarytools')
# install.packages('effects')
# install.packages('car')
# install.packages('PerformanceAnalytics')
# install.packages('lmtest')
# install.packages('scales')

#Load all necessary libraries
library(psych)
library(lavaan)
library(tidyverse)
library(haven)
library(foreign)
library(xlsx)
library(httr)
library(request)
library(dplyr)
library(stringr)
library(readxl)
library(janitor)
library(psych)
library(aod)
library(ResourceSelection)
library(fmsb)
library(summarytools)
library(effects)
library(car)
library(PerformanceAnalytics)
library(lmtest)
library(scales)

#' ----- PRELIM: VARIABLE NAMES DECLARATION
#' 
#' Create Vectors to store different groups of variable names.
#' Variable names grouped by topic to be able to 
#' ...easily extract a subset of the dataframe if needed

other_vars <- c('interview__key', 'district', 'ed', 'cluster', 'urban_rural',
                'rel_to_head', 'age', 'sex', 'ethnicity', 'employment_status', 'employment_type',
                'education_level', 'assets_mobile', 'assets_tablet', 'assets_computer', 'access_internet_athome', 
                'access_type', 'access_other', 'internetaccess_elsewhere', 'mobile_access', 
                'mobile_access_frequency', 'credit_card', 'debit_card', 'hh_males', 'hh_females',
                'result', 'result_other'
)

gov_service_vars <- c('government_service', 'government_service_channel', 
                      'government_service_satisfaction')

language_vars <- c("language_spoken__1", "language_spoken__2", "language_spoken__3", "language_spoken__4",
                   "language_spoken__5", "language_spoken__6", "language_spoken__7", 
                   "language_spoken__8", "language_spoken__9", "language_spoken__10", 
                   "language_spoken__88", "language_spoken__999999", "language_spoken_other")

service_improvement_vars <- c('service_improvement__1', 'service_improvement__2', 'service_improvement__3',
                              'service_improvement__4', 'service_improvement__6', 'service_improvement__5',
                              'service_improvement__7', 'service_improvement__8', 'service_improvement__9',
                              'service_improvement__10', 'service_improvement__88')
eservice_vars <- c('eservice_experience__1', 'eservice_experience__2', 'eservice_experience__3',
                   'eservice_experience__4', 'eservice_experience__5', 'epe1_eservices', 'epe2_eservices',
                   'epe3_eservices', 'epe4_eservices', 'epu1_eservices', 'epu2_eservices', 
                   'epu3_eservices', 'epu4_eservices')

egov_vars <- c("eou1_egov", "eou2_gov", "eou3_egov", "eou4_egov", "pu1_egov", "pu2_egov", 
               "pu3_egov", "pu4_egov", "t1_egov", "t2_egov", "t3_egov", "t4_egov",
               "pbc1_egov", "pbc2_egov", "pbc3_egov", "pbc4_egov", "intention")

sib_vars <- c("ever_interviewed", "interviewed_modality", "uses_email", "email_frequency",
              "participated_online", "willing_web", "favorite_modality")

#OBJECT MERGING ALL VARIABLE NAMES
main_file_vars <- c(other_vars, gov_service_vars, eservice_vars, egov_vars, sib_vars)


#-----------------------
# COLORS AND THEMING
#-----------------------
primaryColor <- 'steelblue3'
secondaryColor <- 'grey70'
errorColor <- 'salmon2'

#Customized Ggplot theme objects
theme_blank <- theme_bw()  + theme(panel.border = element_blank(), 
                                   panel.grid.minor = element_blank(), 
                                   panel.grid.major = element_blank(),
                                   axis.line = element_line(colour = "grey30"),
                                   text = element_text(size = 20),
                                   legend.position = 'bottom', 
                                   legend.direction = 'horizontal'
)  

theme_blank_xblank <- theme_blank + theme(
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank()
)

theme_blank_yblank <- theme_blank + theme(
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank()
)

theme_dotplot <- theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    text = element_text(size = 23),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line(colour = "grey40")
  )


theme_effects <- theme_bw()  + theme(panel.border = element_blank(), 
                                     panel.grid.minor = element_blank(), 
                                     panel.grid.major = element_blank(),
                                     axis.line.y = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.line = element_line(colour = "grey40"),
                                     text = element_text(size = 22),
) 



#' ----- 1. FILE IMPORTING AND READING
#' A. Create strings containing the relative paths to the survey files.
#' B. Read them and save them using both Haven and Foreign
#' C. Remove unnecessary variables by selecting only the files saved in
#'    main_file_vars above
#' D. Rename some variables to make analysis easier
#' 

select <- dplyr::select
#1A. File paths
base_path <- './egovfiles/egov.sav'
diagnostics_path <- './egovfiles/interview__diagnostics.sav' #will not use for now

#1B. READ/IMPORT files using both Foreign and Haven
eafs <- read.spss(base_path, to.data.frame = TRUE, add.undeclared.levels = "no") 


#1C. Select the only the main variables
eafs <- eafs %>% select(main_file_vars)




#' ----- 2. DATA PREPARATION
#' A. Select and save only complete cases, save in EAFS object.
#'    * Complete cases are those where: 
#'    * result = "Complete", all EGOV variables are present, and age > 18
#' B. Create age and education group variables
#' C. Transform variables into appropriate factor types
#'    * Example: Convert ordinal variables to ordered factors
#' 
#' Note, for this version of the script, we will use the Foreign dataset

#Save complete cases
eafs <- eafs[complete.cases(eafs %>% select(egov_vars)),] %>% 
  filter(result == "Complete", age >= 18)

#Rename some relevant variables for easier analysis
#Rename the Foreign dataset
names(eafs)[names(eafs) == 'eou1_egov'] <- 'eou1'
names(eafs)[names(eafs) == 'eou2_gov'] <- 'eou2'
names(eafs)[names(eafs) == 'eou3_egov'] <- 'eou3'
names(eafs)[names(eafs) == 'eou4_egov'] <- 'eou4'
names(eafs)[names(eafs) == 'pu1_egov'] <- 'pu1'
names(eafs)[names(eafs) == 'pu2_egov'] <- 'pu2'
names(eafs)[names(eafs) == 'pu3_egov'] <- 'pu3'
names(eafs)[names(eafs) == 'pu4_egov'] <- 'pu4'
names(eafs)[names(eafs) == 't1_egov'] <- 't1'
names(eafs)[names(eafs) == 't2_egov'] <- 't2'
names(eafs)[names(eafs) == 't3_egov'] <- 't3'
names(eafs)[names(eafs) == 't4_egov'] <- 't4'
names(eafs)[names(eafs) == 'pbc1_egov'] <- 'pbc1'
names(eafs)[names(eafs) == 'pbc2_egov'] <- 'pbc2'
names(eafs)[names(eafs) == 'pbc3_egov'] <- 'pbc3'
names(eafs)[names(eafs) == 'pbc4_egov'] <- 'pbc4'


#Create age and education group variables
eafs <- eafs %>% 
  mutate(
    age_group = case_when(
      age >= 70 ~ "70+",
      age >= 60 ~ "60-69",
      age >= 50 ~ "50-59",
      age >= 40 ~ "40-49",
      age >= 30 ~ "30-39",
      age >= 18 ~ "18-29"
    ),
    education_group = case_when(
      as.integer(education_level) %in% c(1:7, 20, 21, 999999) ~ "None or Some Primary",
      as.integer(education_level) %in% 8:11 ~ "Primary",
      as.integer(education_level) == 12 ~ "High School",
      as.integer(education_level) %in% 17:19 ~ "University",
      as.integer(education_level) %in% c(13:16, 888888) ~ "Other",
      TRUE ~ 'Other'
    )
  ) 


#Convert group variables to factors. Education is ordinal
eafs$age_group <- factor(eafs$age_group)
eafs$education_group <- factor(eafs$education_group, 
                               levels = c('None or Some Primary', 
                                          'Primary', 
                                          'High School',
                                          'University',
                                          'Other'), 
                               ordered = TRUE)

eafs$eou1 <- factor(eafs$eou1, ordered = TRUE)
eafs$eou2 <- factor(eafs$eou2, ordered = TRUE)
eafs$eou3 <- factor(eafs$eou3, ordered = TRUE)
eafs$eou4 <- factor(eafs$eou4, ordered = TRUE)
eafs$pu1 <- factor(eafs$pu1, ordered = TRUE)
eafs$pu2 <- factor(eafs$pu2, ordered = TRUE)
eafs$pu3 <- factor(eafs$pu3, ordered = TRUE)
eafs$pu4 <- factor(eafs$pu4, ordered = TRUE)
eafs$t1 <- factor(eafs$t1, ordered = TRUE)
eafs$t2 <- factor(eafs$t2, ordered = TRUE)
eafs$t3 <- factor(eafs$t3, ordered = TRUE)
eafs$t4 <- factor(eafs$t4, ordered = TRUE)
eafs$pbc1 <- factor(eafs$pbc1, ordered = TRUE)
eafs$pbc2 <- factor(eafs$pbc2, ordered = TRUE)
eafs$pbc3 <- factor(eafs$pbc3, ordered = TRUE)
eafs$pbc4 <- factor(eafs$pbc4, ordered = TRUE)
eafs$intention <- factor(eafs$intention, ordered = TRUE)



#' ----- 3. DESCRIPTIVE STATISTICS
#' A. Create frequency statistics table
#'    * Derive the different components separately and save on different ojbects
#'    * Merge object and export to Excel 
#' B. Create Graphs and Charts
#' 
#' 

#-----------------------
# FREQUENCIES
#-----------------------
fsex <- freq(eafs$sex, headings = FALSE, style = 'grid') %>% 
  as.data.frame() %>% 
  select(1, Count = Freq, Proportion = `% Valid`, Cumul = `% Valid Cum.`)
fage <- freq(eafs$age_group, headings = FALSE, style = 'grid') %>% 
  as.data.frame() %>% 
  select(1, Count = Freq, Proportion = `% Valid`, Cumul = `% Valid Cum.`)
feducation <- freq(eafs$education_group, headings = FALSE, style = 'grid') %>% 
  as.data.frame() %>% 
  select(1, Count = Freq, Proportion = `% Valid`, Cumul = `% Valid Cum.`)
fdistrict <- freq(eafs$district, headings = FALSE, style = 'grid') %>% 
  as.data.frame() %>% 
  select(1, Count = Freq, Proportion = `% Valid`, Cumul = `% Valid Cum.`)
farea <- freq(eafs$urban_rural, headings = FALSE, style = 'grid') %>% 
  as.data.frame() %>% 
  select(1, Count = Freq, Proportion = `% Valid`, Cumul = `% Valid Cum.`)
finternethome <- freq(eafs$access_internet_athome, headings = FALSE, style = 'grid') %>% 
  as.data.frame() %>% 
  select(1, Count = Freq, Proportion = `% Valid`, Cumul = `% Valid Cum.`)
finternetelsewhere <- freq(eafs$internetaccess_elsewhere, headings = FALSE, style = 'grid') %>% 
  as.data.frame() %>% 
  select(1, Count = Freq, Proportion = `% Valid`, Cumul = `% Valid Cum.`)


freqObject <- rbind(fsex, fage, feducation, fdistrict, farea, 
              finternethome, finternetelsewhere)
freqObject <- freqObject %>% 
  filter(!(Count %in% c(0,606)))

#write.xlsx(freqObject, './descriptiveFreqs.xlsx')



#Chart: Respondents by District and Area
resByDisAreaPlot <- eafs %>% ggplot(aes(x=district, fill=urban_rural)) +
  geom_bar(position = 'stack') +
  geom_text(aes( y=..count.., label=..count..), size = 5, color = 'white',
            stat="count", position=position_stack(0.5), vjust=0.5) +
  coord_flip() +
  scale_fill_discrete(type = c(secondaryColor, primaryColor)) +
  labs(x = 'District', y='') +
  guides(fill=guide_legend(title="Area")) +
  theme_blank_xblank

#Chart: Internet at home by Distirct
internetByDisPlot <- eafs %>% 
  ggplot(aes(x=district, fill=access_internet_athome, label = district)) +
  geom_bar(position = 'fill') +
  geom_text(aes(y=..count../tapply(..count.., ..x.. ,sum)[..x..], 
                 label=percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            size = 5.5, color = 'white',
            stat="count", position=position_fill(0.5), vjust=0.5, hjust = 0.4) +
  scale_fill_discrete(type = c(secondaryColor, primaryColor)) +
  labs(x = 'District', y='') +
  guides(fill=guide_legend(title="Access")) +
  theme_blank_yblank



#' ----- 3. METHODOLOGY: CRONBACH'S ALPHA
#' A. Convert ordinal data to numerical
#' B. Run Cronbach's alpha on all scale items
#' C. Run Conrbach's alpha after removing 3 items
#'    * These items were removed based on CFA
#' 

eafs_num <- eafs

eafs_num$eou1 <- as.numeric(eafs_num$eou1)
eafs_num$eou2 <- as.numeric(eafs_num$eou2)
eafs_num$eou3 <- as.numeric(eafs_num$eou3)
eafs_num$eou4 <- as.numeric(eafs_num$eou4)
eafs_num$pu1 <- as.numeric(eafs_num$pu1)
eafs_num$pu2 <- as.numeric(eafs_num$pu2)
eafs_num$pu3 <- as.numeric(eafs_num$pu3)
eafs_num$pu4 <- as.numeric(eafs_num$pu4)
eafs_num$t1 <- as.numeric(eafs_num$t1)
eafs_num$t2 <- as.numeric(eafs_num$t2)
eafs_num$t3 <- as.numeric(eafs_num$t3)
eafs_num$t4 <- as.numeric(eafs_num$t4)
eafs_num$pbc1 <- as.numeric(eafs_num$pbc1)
eafs_num$pbc2 <- as.numeric(eafs_num$pbc2)
eafs_num$pbc3 <- as.numeric(eafs_num$pbc3)
eafs_num$pbc4 <- as.numeric(eafs_num$pbc4)
eafs_num$intention <- as.numeric(eafs_num$intention)

#Alpha on full set of items
psych::alpha(eafs_num %>% select(eou1, eou2, eou3, eou4))
psych::alpha(eafs_num %>% select(pu1, pu2, pu3, pu4))
psych::alpha(eafs_num %>% select(t1, t2, t3, t4))
psych::alpha(eafs_num %>% select(pbc1, pbc2, pbc3, pbc4))


#psych::describe(eafs_sum %>% select(peou_sum, pu_sum, teg_sum, pbc_sum))


#' ----- 4. Analysis: CONFIRMATORY FACTOR ANALYSIS
#' A. DEFINE two models:
#'    * A full model with all items in the questionnaire
#'    * A lean model removing the items with lowest factor loadings
#' B. Fit and summarize each model
#' C. Predict and save individual factor scores using each model
#' D. Append individual factor scores to the main eafs object
#'    * Save each model prediction in different objects
#' E. Run histograms on the factor scores distributions.
#'    * Run for each construct within each model
#' 

#Define full and lean models
full_model <- '
  PEOU =~ eou1 + eou2 + eou3 + eou4
  PU =~ pu1 + pu2 + pu3 + pu4
  TEG =~ t1 + t2 + t3 + t4
  PBC =~ pbc1 + pbc2 + pbc3 + pbc4
'

lean_model <- '
  PEOU =~ eou2 + eou3 + eou4
  PU =~ pu1 + pu2 + pu3 + pu4
  TEG =~ t1 + t2 + t3 + t4
  PBC =~ pbc1 + pbc2 + pbc3
'


# Fit and summarize models
full_fit <- cfa(full_model, data=eafs)
summary(full_fit, standardized=TRUE, fit.measures=TRUE)

lean_fit <- cfa(lean_model, data=eafs)
summary(lean_fit, standardized=TRUE, fit.measures=TRUE)

#Extract standardized paramters and fit measures and write to Excel
lean_mod_std <- standardizedSolution(lean_fit, ci = F) %>% as.data.frame()
full_mod_std <- standardizedSolution(full_fit, ci = F) %>% as.data.frame()
fitMeasures(lean_fit, c("chisq.scaled", "df.scaled",  "rmsea.scaled","srmr", "cfi.scaled", "nfi.scaled")) %>% 
  as.data.frame()
# write.xlsx(full_mod_std, './fit_loadings.xlsx')
# write.xlsx(lean_mod_std, './fit_loadings.xlsx')

#Predict individual factor scores for model to use
factor_scores <- lavPredict(lean_fit, method = 'regression')
#factor_scores2 <- lavPredict(full_fit, method = 'regression')

#Append to main eafs file
eafs <- cbind(eafs, factor_scores)
#eafs2 <- cbind(eafs, factor_scores2)


#Run frequency histograms for the constructs from each model
#Second model
peou <- eafs %>% select(PEOU) %>% pull()
pu <- eafs %>% select(PU) %>% pull()
teg <- eafs %>% select(TEG) %>% pull()
pbc <- eafs %>% select(PBC) %>% pull()
hist(peou)
hist(pu)
hist(teg)
hist(pbc)




#' ----- 5. REGRESSION PREPARATION
#' A. Create two grouping variables for each construct, based on individual factor scores:
#'    * A grouping based on score intervals
#'    * A grouping based on quantiles (0-25%, 26-50%, 51-75%, 76-100%)
#' B. Run frequencies on each grouping variable
#' C. Convert grouping variables to factor
#' 

eafs <- eafs %>% 
  mutate(
    peou = case_when(
      PEOU <= -1 ~ 1,
      PEOU <=  0 ~ 2,
      PEOU <=  1 ~ 3,
      PEOU  >  1 ~ 4,
    ),
    pu = case_when(
      PU <= -1 ~ 1,
      PU <=  0 ~ 2,
      PU <=  1 ~ 3,
      PU >  1 ~ 4,
    ),
    teg = case_when(
      TEG <= -1 ~ 1,
      TEG <=  0 ~ 2,
      TEG <=  1 ~ 3,
      TEG  >  1 ~ 4,
    ),
    pbc = case_when(
      PBC <= -1 ~ 1,
      PBC <=  0 ~ 2,
      PBC <=  1 ~ 3,
      PBC  >  1 ~ 4,
    ),
    intention_num = as.numeric(intention),
    intention_bin = ifelse(
      as.numeric(intention) >= 4, 1, 0
    )
  ) 

# Convert all grouping variables to factors
eafs$peou <- factor(eafs$peou)
levels(eafs$peou)[levels(eafs$peou)==1] <- "Low"
levels(eafs$peou)[levels(eafs$peou)==2] <- "Moderate"
levels(eafs$peou)[levels(eafs$peou)==3] <- "High"
levels(eafs$peou)[levels(eafs$peou)==4] <- "Very High"

eafs$pu <- factor(eafs$pu)
levels(eafs$pu)[levels(eafs$pu)==1] <- "Low"
levels(eafs$pu)[levels(eafs$pu)==2] <- "Moderate"
levels(eafs$pu)[levels(eafs$pu)==3] <- "High"
levels(eafs$pu)[levels(eafs$pu)==4] <- "Very High"

eafs$teg <- factor(eafs$teg)
levels(eafs$teg)[levels(eafs$teg)==1] <- "Low"
levels(eafs$teg)[levels(eafs$teg)==2] <- "Moderate"
levels(eafs$teg)[levels(eafs$teg)==3] <- "High"
levels(eafs$teg)[levels(eafs$teg)==4] <- "Very High"

eafs$pbc <- factor(eafs$pbc)
levels(eafs$pbc)[levels(eafs$pbc)==1] <- "Low"
levels(eafs$pbc)[levels(eafs$pbc)==2] <- "Moderate"
levels(eafs$pbc)[levels(eafs$pbc)==3] <- "High"
levels(eafs$pbc)[levels(eafs$pbc)==4] <- "Very High"

eafs$intention_bin <- factor(eafs$intention_bin)
levels(eafs$intention_bin)[levels(eafs$intention_bin)==0] <- "Low"
levels(eafs$intention_bin)[levels(eafs$intention_bin)==1] <- "High"

#Chart: dotplot of peou x intention
ggplot(eafs, aes(x=peou, y=intention_bin)) + 
  geom_jitter(width = 0.25, height = 0.2, color=primaryColor, size = 2) + 
  labs(y='Intention') +
  theme_dotplot

#Chart: dotplot of pu x intention
ggplot(eafs, aes(x=pu, y=intention_bin)) + 
  geom_jitter(width = 0.25, height = 0.2, color=primaryColor) + 
  labs(y='Intention') +
  theme_dotplot

#Chart: dotplot of teg x intention
ggplot(eafs, aes(x=teg, y=intention_bin)) + 
  geom_jitter(width = 0.25, height = 0.2, color=primaryColor) + 
  labs(y='Intention') +
  theme_dotplot

#Chart: dotplot of pbc x intention
ggplot(eafs, aes(x=pbc, y=intention_bin)) + 
  geom_jitter(width = 0.25, height = 0.2, color=primaryColor) + 
  labs(y='Intention') +
  theme_dotplot


# Frequencie plots for the factor variables
eafs %>% ggplot(aes(x=peou)) +
  geom_bar(fill=primaryColor) +
  labs(x='Perceived Ease of Use (PEOU)', y='') +
  geom_text(aes( y=..count.., label=..count..), size = 6, color = 'white',
            stat="count", position=position_stack(0.5), vjust=0.5) + theme_blank_yblank

eafs %>% ggplot(aes(x=pu)) +
  geom_bar(fill=primaryColor) +
  labs(x='Perceived Usefulness (PU)', y='') +
  geom_text(aes( y=..count.., label=..count..), size = 6, color = 'white',
            stat="count", position=position_stack(0.5), vjust=0.5) + theme_blank_yblank

eafs %>% ggplot(aes(x=teg)) +
  geom_bar( fill=primaryColor) +
  labs(x='Trust in E-Government (TEG)',y='') +
  geom_text(aes( y=..count.., label=..count..), size = 6, color = 'white',
            stat="count", position=position_stack(0.5), vjust=0.5) + theme_blank_yblank

eafs %>% ggplot(aes(x=pbc)) +
  geom_bar(fill=primaryColor) +
  labs(x='Perceived Behavioural Control (PBC)', y='') +
  geom_text(aes( y=..count.., label=..count..), size = 6, color = 'white',
            stat="count", position=position_stack(0.5), vjust=0.5) + theme_blank_yblank



#######################
# CORRELATION TESTS
#######################
eafs_cor <- eafs %>% 
  select(peou, pu, teg, pbc)

eafs_cor <- eafs_cor %>% 
  mutate(
    peou = as.numeric(peou),
    pu = as.numeric(pu),
    teg = as.numeric(teg),
    pbc = as.numeric(pbc)
  )

cor(eafs_cor, method = "spearman")
chart.Correlation(eafs_cor, histogram=TRUE, pch=19, method = 'spearman', cex=20)

#' ----- 6. LOGISTIC REGRESSION
#' BASE PREDICTORS TO USE: grouping variables for the constructs (peou_group, pu_group, etc.)
#' A. Run logistic regression, predicting intention by each factor/construct
#'    * Run logistic regression predicting each factor, by background variables
#' A. Run logistic regression using 'Full' model:
#'    * Full model uses all constructs (peou, pu, teg, pbc) as predictors
#'    * Run fit and diagnostic tests on full model
#' B. Run logistic regression on 'lean' model:
#'    * Better model removes PEOU to improve model and fit
#'    * Run fit and diagnostics and compare with full model
#'    * TODO: Run multi-collinearity test on peou. It is suspected to be collinear with pu
#' C. (TODO) Improve the 'Better' model by adding adding other useful predictors
#' 

### 
### Model 1: PEOU + PU + TEG + PBC
###

#1. Fit the model
#2. Output logit summary
#3. Expoentiate to get odds ratio of coefficients
model1 <- glm(intention_bin~ peou + pu + teg + pbc, 
                  data = eafs, family = "binomial")
summary(model1)
exp(coef(model1))

#4. Significance tests
with(model1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
lrtest(model1)

#GOODNESS OF FIT TESTS
#Log likelihood, hoslem and nagelkerke tests
nullmod_peou <- glm(intention_bin~1, data = eafs, family="binomial")
1-logLik(model1)/logLik(nullmod_peou)

hoslem.test(model1$y, fitted(model1), g=10)
NagelkerkeR2(model1)


#Significance test for PEOU
wald.test(b = coef(model1), Sigma = vcov(model1), Terms = 1:3)
#Overall significance of PEOU in the model
noPeouMod <- glm(intention_bin~ teg + pbc + pu, data = eafs, family = "binomial")
anova(model1, noPeouMod, test="LRT")

#Overall significance of PBC in the model
noPbcMod <- glm(intention_bin~ teg + peou + pu, data = eafs, family = "binomial")
anova(model1, noPbcMod, test="LRT")

#Variance Inflaction Factor
vif(model1)


### 
### MODEL 2: PU + TEG + PBC
###

#1. Fit the model
#2. Output logit summary
#3. Expoentiate to get odds ratio of coefficients
model2 <- glm(intention_bin ~ pu + teg + pbc, 
                  data = eafs, family = "binomial")
summary(model2)
exp(coef(model2))

#4. X^2 significance test
with(model2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
anova(model1, noPbcMod, test="LRT")

#GOODNESS OF FIT TESTS
#5a: Log likelihood (better)
#5b. Hoslem-Lemeshow test (not as good)
nullmod_peou <- glm(intention_bin ~ 1, data = eafs, family="binomial")
1-logLik(model2)/logLik(nullmod_peou)

hoslem.test(model2$y, fitted(model2), g=10)
NagelkerkeR2(model2)

#Variance Inflaction Factor
vif(model2)



####
### PU ANALYSIS AND PREDICTIONS
##

#significance test for PU
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 1:3)
#likelihood ratio test for PU
noPuMod <- glm(intention_bin~ teg + pbc, data = eafs, family = "binomial")
anova(model2, noPuMod, test="LRT")


### PREDICTIONS --- ALL LOW AND ONE VARIES
ilink <- family(model2)$linkinv

newdata_pu_low <- with(eafs, data.frame(pbc = "Low", 
                                 teg = "Low", 
                                 pu = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                             levels = c('Low', 'Moderate', 'High', 'Very High'))
))


##Predictions when all factors are low
newdata_pu_low <- add_column(newdata_pu_low, fit = predict(model2, newdata = newdata_pu_low, type = 'response'))
pred_pu_low <- bind_cols(newdata_pu_low, setNames(as_tibble(predict(model2, newdata_pu_low, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
pred_pu_low <- mutate(pred_pu_low,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)),
                other_factors = 'Other Factors: Low')

##Predictions when all factors are moderate
newdata_pu_mod <- with(eafs, data.frame(pbc = "Moderate", 
                                        teg = "Moderate", 
                                        pu = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                    levels = c('Low', 'Moderate', 'High', 'Very High'))))
                       
newdata_pu_mod <- add_column(newdata_pu_mod, fit = predict(model2, newdata = newdata_pu_mod, type = 'response'))
pred_pu_mod <- bind_cols(newdata_pu_mod, setNames(as_tibble(predict(model2, newdata_pu_mod, se.fit = TRUE)[1:2]),
                                                  c('fit_link','se_link')))
## create the interval and backtransform
pred_pu_mod <- mutate(pred_pu_mod,
                      fit_resp  = ilink(fit_link),
                      right_upr = ilink(fit_link + (2 * se_link)),
                      right_lwr = ilink(fit_link - (2 * se_link)),
                      other_factors = 'Other Factors: Moderate')

##Predictions when all factors are high
newdata_pu_high <- with(eafs, data.frame(pbc = "High", 
                                        teg = "High", 
                                        pu = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                    levels = c('Low', 'Moderate', 'High', 'Very High'))))
                       
newdata_pu_high <- add_column(newdata_pu_high, fit = predict(model2, newdata = newdata_pu_high, type = 'response'))
pred_pu_high <- bind_cols(newdata_pu_high, setNames(as_tibble(predict(model2, newdata_pu_high, se.fit = TRUE)[1:2]),
                                                  c('fit_link','se_link')))
## create the interval and backtransform
pred_pu_high <- mutate(pred_pu_high,
                      fit_resp  = ilink(fit_link),
                      right_upr = ilink(fit_link + (2 * se_link)),
                      right_lwr = ilink(fit_link - (2 * se_link)),
                      other_factors = 'Other Factors: High')


##Predictions when all factors are very high
newdata_pu_vhigh <- with(eafs, data.frame(pbc = "Very High", 
                                        teg = "Very High", 
                                        pu = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                    levels = c('Low', 'Moderate', 'High', 'Very High'))))
                       
newdata_pu_vhigh <- add_column(newdata_pu_vhigh, fit = predict(model2, newdata = newdata_pu_vhigh, type = 'response'))
pred_pu_vhigh <- bind_cols(newdata_pu_vhigh, setNames(as_tibble(predict(model2, newdata_pu_vhigh, se.fit = TRUE)[1:2]),
                                                  c('fit_link','se_link')))
## create the interval and backtransform
pred_pu_vhigh <- mutate(pred_pu_vhigh,
                      fit_resp  = ilink(fit_link),
                      right_upr = ilink(fit_link + (2 * se_link)),
                      right_lwr = ilink(fit_link - (2 * se_link)),
                      other_factors = 'Other Factors: Very High')

#Join all predictions
pred_pu_all <- rbind(pred_pu_low, pred_pu_mod, pred_pu_high, pred_pu_vhigh)
pred_pu_all$other_factors <- as.factor(pred_pu_all$other_factors)
pred_pu_all$other_factors <- factor(pred_pu_all$other_factors, 
                                    levels = (c('Other Factors: Low', 'Other Factors: Moderate',
                                             'Other Factors: High', 'Other Factors: Very High')))


#EFFECTS PLOT FOR PU
ggplot(pred_pu_all, aes(pu,fit_resp, group = 1)) +
  geom_line(color=primaryColor, size=1) + theme_effects +
  geom_point(color = primaryColor, size=2)+
  geom_errorbar(aes(ymin=right_lwr, ymax=right_upr),  color=errorColor, width=0.2,  size=0.6,
                position=position_dodge(0.05)) + 
  geom_text(aes(label=round(fit_resp,2)),hjust=-0.2, vjust=0.6, color = 'grey50', size = 6) +
  labs(y='', x='Perceived Usefulness (PU)') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~other_factors, nrow = 2)






####
### TEG ANALYSIS AND PREDICTIONS
##

#significance test for PU
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 4:6)
#likelihood ratio test for PU
noTegMod <- glm(intention_bin~ pu + pbc, data = eafs, family = "binomial")
anova(model2, noTegMod, test="LRT")


### PREDICTIONS --- ALL LOW AND ONE VARIES

newdata_teg_low <- with(eafs, data.frame(pbc = "Low", 
                                        pu = "Low", 
                                        teg = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                    levels = c('Low', 'Moderate', 'High', 'Very High'))
))


##Predictions when all factors are low
newdata_teg_low <- add_column(newdata_teg_low, fit = predict(model2, newdata = newdata_teg_low, type = 'response'))
pred_teg_low <- bind_cols(newdata_teg_low, setNames(as_tibble(predict(model2, newdata_teg_low, se.fit = TRUE)[1:2]),
                                                  c('fit_link','se_link')))
## create the interval and backtransform
pred_teg_low <- mutate(pred_teg_low,
                      fit_resp  = ilink(fit_link),
                      right_upr = ilink(fit_link + (2 * se_link)),
                      right_lwr = ilink(fit_link - (2 * se_link)),
                      other_factors = 'Other Factors: Low')

##Predictions when all factors are moderate
newdata_teg_mod <- with(eafs, data.frame(pbc = "Moderate", 
                                        pu = "Moderate", 
                                        teg = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                    levels = c('Low', 'Moderate', 'High', 'Very High'))))

newdata_teg_mod <- add_column(newdata_teg_mod, fit = predict(model2, newdata = newdata_teg_mod, type = 'response'))
pred_teg_mod <- bind_cols(newdata_teg_mod, setNames(as_tibble(predict(model2, newdata_teg_mod, se.fit = TRUE)[1:2]),
                                                  c('fit_link','se_link')))
## create the interval and backtransform
pred_teg_mod <- mutate(pred_teg_mod,
                      fit_resp  = ilink(fit_link),
                      right_upr = ilink(fit_link + (2 * se_link)),
                      right_lwr = ilink(fit_link - (2 * se_link)),
                      other_factors = 'Other Factors: Moderate')

##Predictions when all factors are high
newdata_teg_high <- with(eafs, data.frame(pbc = "High", 
                                         pu = "High", 
                                         teg = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                     levels = c('Low', 'Moderate', 'High', 'Very High'))))

newdata_teg_high <- add_column(newdata_teg_high, fit = predict(model2, newdata = newdata_teg_high, type = 'response'))
pred_teg_high <- bind_cols(newdata_teg_high, setNames(as_tibble(predict(model2, newdata_teg_high, se.fit = TRUE)[1:2]),
                                                    c('fit_link','se_link')))
## create the interval and backtransform
pred_teg_high <- mutate(pred_teg_high,
                       fit_resp  = ilink(fit_link),
                       right_upr = ilink(fit_link + (2 * se_link)),
                       right_lwr = ilink(fit_link - (2 * se_link)),
                       other_factors = 'Other Factors: High')


##Predictions when all factors are very high
newdata_teg_vhigh <- with(eafs, data.frame(pbc = "Very High", 
                                          pu = "Very High", 
                                          teg = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                      levels = c('Low', 'Moderate', 'High', 'Very High'))))

newdata_teg_vhigh <- add_column(newdata_teg_vhigh, fit = predict(model2, newdata = newdata_teg_vhigh, type = 'response'))
pred_teg_vhigh <- bind_cols(newdata_teg_vhigh, setNames(as_tibble(predict(model2, newdata_teg_vhigh, se.fit = TRUE)[1:2]),
                                                      c('fit_link','se_link')))
## create the interval and backtransform
pred_teg_vhigh <- mutate(pred_teg_vhigh,
                        fit_resp  = ilink(fit_link),
                        right_upr = ilink(fit_link + (2 * se_link)),
                        right_lwr = ilink(fit_link - (2 * se_link)),
                        other_factors = 'Other Factors: Very High')

#Join all predictions together
pred_teg_all <- rbind(pred_teg_low, pred_teg_mod, pred_teg_high, pred_teg_vhigh)
pred_teg_all$other_factors <- as.factor(pred_teg_all$other_factors)
pred_teg_all$other_factors <- factor(pred_teg_all$other_factors, 
                                    levels = (c('Other Factors: Low', 'Other Factors: Moderate',
                                                'Other Factors: High', 'Other Factors: Very High')))


#EFFECTS PLOT FOR TEG
ggplot(pred_teg_all, aes(teg,fit_resp, group = 1)) +
  geom_line(color=primaryColor, size=1) + theme_effects +
  geom_point(color = primaryColor, size=2)+
  geom_errorbar(aes(ymin=right_lwr, ymax=right_upr),  color=errorColor, width=0.2,  size=0.6,
                position=position_dodge(0.05)) + 
  geom_text(aes(label=round(fit_resp,2)),hjust=-0.2, vjust=1, color = 'grey50', size = 6) +
  labs(y='', x='Trust in E-Government (TEG)') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~other_factors, nrow = 2)






####
### PBC ANALYSIS AND PREDICTIONS
##

#signficance test for PBC
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 7:9)
#likelihood ratio test for PU
nopbcMod <- glm(intention_bin~ pu + teg, data = eafs, family = "binomial")
anova(model2, nopbcMod, test="LRT")


### PREDICTIONS --- ALL LOW AND ONE VARIES

newdata_pbc_low <- with(eafs, data.frame(teg = "Low", 
                                         pu = "Low", 
                                         pbc = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                      levels = c('Low', 'Moderate', 'High', 'Very High'))
))


##Predictions when all factors are low
newdata_pbc_low <- add_column(newdata_pbc_low, fit = predict(model2, newdata = newdata_pbc_low, type = 'response'))
pred_pbc_low <- bind_cols(newdata_pbc_low, setNames(as_tibble(predict(model2, newdata_pbc_low, se.fit = TRUE)[1:2]),
                                                    c('fit_link','se_link')))
## create the interval and backtransform
pred_pbc_low <- mutate(pred_pbc_low,
                       fit_resp  = ilink(fit_link),
                       right_upr = ilink(fit_link + (2 * se_link)),
                       right_lwr = ilink(fit_link - (2 * se_link)),
                       other_factors = 'Other Factors: Low')

##Predictions when all factors are moderate
newdata_pbc_mod <- with(eafs, data.frame(teg = "Moderate", 
                                         pu = "Moderate", 
                                         pbc = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                      levels = c('Low', 'Moderate', 'High', 'Very High'))))

newdata_pbc_mod <- add_column(newdata_pbc_mod, fit = predict(model2, newdata = newdata_pbc_mod, type = 'response'))
pred_pbc_mod <- bind_cols(newdata_pbc_mod, setNames(as_tibble(predict(model2, newdata_pbc_mod, se.fit = TRUE)[1:2]),
                                                    c('fit_link','se_link')))
## create the interval and backtransform
pred_pbc_mod <- mutate(pred_pbc_mod,
                       fit_resp  = ilink(fit_link),
                       right_upr = ilink(fit_link + (2 * se_link)),
                       right_lwr = ilink(fit_link - (2 * se_link)),
                       other_factors = 'Other Factors: Moderate')

##Predictions when all factors are high
newdata_pbc_high <- with(eafs, data.frame(teg = "High", 
                                          pu = "High", 
                                          pbc = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                       levels = c('Low', 'Moderate', 'High', 'Very High'))))

newdata_pbc_high <- add_column(newdata_pbc_high, fit = predict(model2, newdata = newdata_pbc_high, type = 'response'))
pred_pbc_high <- bind_cols(newdata_pbc_high, setNames(as_tibble(predict(model2, newdata_pbc_high, se.fit = TRUE)[1:2]),
                                                      c('fit_link','se_link')))
## create the interval and backtransform
pred_pbc_high <- mutate(pred_pbc_high,
                        fit_resp  = ilink(fit_link),
                        right_upr = ilink(fit_link + (2 * se_link)),
                        right_lwr = ilink(fit_link - (2 * se_link)),
                        other_factors = 'Other Factors: High')


##Predictions when all factors are very high
newdata_pbc_vhigh <- with(eafs, data.frame(teg = "Very High", 
                                           pu = "Very High", 
                                           pbc = factor(c('Low', 'Moderate', 'High', 'Very High'), 
                                                        levels = c('Low', 'Moderate', 'High', 'Very High'))))

newdata_pbc_vhigh <- add_column(newdata_pbc_vhigh, fit = predict(model2, newdata = newdata_pbc_vhigh, type = 'response'))
pred_pbc_vhigh <- bind_cols(newdata_pbc_vhigh, setNames(as_tibble(predict(model2, newdata_pbc_vhigh, se.fit = TRUE)[1:2]),
                                                        c('fit_link','se_link')))
## create the interval and backtransform
pred_pbc_vhigh <- mutate(pred_pbc_vhigh,
                         fit_resp  = ilink(fit_link),
                         right_upr = ilink(fit_link + (2 * se_link)),
                         right_lwr = ilink(fit_link - (2 * se_link)),
                         other_factors = 'Other Factors: Very High')

#Join all PBC predictions
pred_pbc_all <- rbind(pred_pbc_low, pred_pbc_mod, pred_pbc_high, pred_pbc_vhigh)
pred_pbc_all$other_factors <- as.factor(pred_pbc_all$other_factors)
pred_pbc_all$other_factors <- factor(pred_pbc_all$other_factors, 
                                     levels = (c('Other Factors: Low', 'Other Factors: Moderate',
                                                 'Other Factors: High', 'Other Factors: Very High')))


#EFFECTS PLOT FOR PBC
ggplot(pred_pbc_all, aes(pbc,fit_resp, group = 1)) +
  geom_line(color=primaryColor, size=1) + theme_effects +
  geom_point(color = primaryColor, size=2)+
  geom_errorbar(aes(ymin=right_lwr, ymax=right_upr),  color=errorColor, width=0.2,  size=0.6,
                position=position_dodge(0.05)) + 
  geom_text(aes(label=round(fit_resp,2)),hjust=-0.2, vjust=1, color = 'grey50', size = 6) +
  labs(y='', x='Perceived Behavioural Control (PBC)') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  facet_wrap(~other_factors, nrow = 2)



