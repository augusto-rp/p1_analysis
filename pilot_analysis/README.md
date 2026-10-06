

# README.md

## Overview

This repository contains R code for processing, validating, and analyzing data from a survey study that is piloting different stimuli for a future study on political humor. The study involves participants rating pairs of stimuli (memes and serious vignettes) on dimensions including funniness, polemicality (controversy), and similarity of message content.
 
**this is an ongoing work, so not everything is in its optimal form, nor are all the analysis already done**

Apart from this line and the previous one, this README was written using an AI assistant. Im sorry for this [heresy](https://www.youtube.com/watch?v=d8MByH0ELSo&list=RDd8MByH0ELSo&start_radio=1)
but i was kind of hungy and wanted to do a quick description of what to expect
I'll probably make a more human like version once I have the definited results


## Project Structure

The analysis is contained in a single R script  organized into the following major sections:

### 1. Data Preparation

- **Data Import**: Reads an Excel file containing survey responses.
- **Cleaning**: 
  - Filters out participants above the accepted age threshold (SD04_01 > 36).
  - Converts missing value codes (-9) to `NA`.
- **Metadata Definition**: Identifies columns containing survey metadata (timestamps, page times, status variables) to be excluded from substantive analyses.
- **Variable Definitions**: Creates string vectors for:
  - `ck_`: Stimuli rating columns (CK08–CK47, with suffixes _01 through _05 representing different rating dimensions).
  - `pi_check`: Columns checking whether stimuli are perceived as political.
  - `pi_ideology`: Columns checking perceived ideological orientation of stimuli (right-wing, left-wing, or neither).

### 2. Missing Data Analysis

- **MCAR Testing**: Because participants were randomly assigned to one of four conditions (AS02), the data is subset by condition. Each subset undergoes:
  - Removal of all-NA columns, metadata columns, and zero-variance columns.
  - Selection of only stimuli rating variables (CK columns).
  - Little's MCAR test (`na.test` from the `misty` package).
- **Attrition Analysis**:
  - Creates a `total_p` variable counting how many questionnaire pages each participant viewed.
  - Cross-tabulations of page completion by political affiliation and experimental condition.
  - Linear model examining whether average funniness ratings predict survey completion.

### 3. Descriptive Statistics

- **Sample Characteristics**: 
  - Political affiliation distribution (SD06).
  - Gender (SD01) and education (SD07) frequencies.
  - Bar chart of political identity.
- **Individual-Level Averages**: Creates `avg_01` through `avg_05` columns representing each participant's average rating across all stimuli for each rating dimension. This aids in identifying suspicious responding patterns (e.g., straight-lining).

### 4. Stimuli Descriptives

- **Funniness Ratings**: Descriptive statistics and raw frequency distributions for humorous memes (CK*_03) and serious vignettes (CK*_04).
- **Polemicality Ratings**: Descriptive statistics for perceived controversy of memes (CK*_01) and serious vignettes (CK*_02).
- **Similarity Ratings**: Descriptive statistics for perceived message similarity between meme and serious versions (CK*_05).
- **Consolidated Table**: A single descriptive table (`desc_ck`) containing n, mean, SD, skewness, kurtosis, and standard error for all stimuli rating variables.

### 5. Data Restructuring for Mixed Models

- **Variable Selection**: Selects gender, political identification, experimental condition, total pages viewed, and all CK rating columns.
- **Long Format Transformation**: Converts data from wide to long format, with columns:
  - `id`: Participant identifier.
  - `ck_`: Stimulus identifier.
  - `question`: Rating dimension (1–5).
  - `ck_value`: The rating value.

### 6. Stimuli Selection Criteria

The script implements a multi-step procedure for selecting which stimuli pairs to retain for analysis:

#### Step 0: Fixed and Random Effects on Funniness
- Fits a linear mixed model (`lmer`) predicting funniness from image version (Serious vs. Humorous), with:
  - Random intercepts for participant.
  - Random intercepts and slopes for version by stimulus pair.
- Examines variance components and BLUPs to understand between-pair and within-participant variability.

#### Step 1: Pairwise Differences in Funniness
- Fits a model with the interaction between stimulus pair and version.
- Uses estimated marginal means (`emmeans`) to test whether each humorous meme is rated significantly funnier than its serious counterpart.
- Applies Benjamini-Hochberg (FDR) correction for multiple comparisons.

#### Step 2: Comparison Against Average Funniness
- Computes each stimulus's deviation from the grand mean funniness for its version (H or S).
- Applies selection criteria:
  - Humorous memes should be at or above the mean funniness of all humorous memes.
  - Serious vignettes should be at or below the mean funniness of all serious vignettes.
- Classifies each stimulus as PASS, CHECK, or FAIL based on point estimates and confidence intervals.

#### Step 3: Similarity of Message Content
- Computes mean similarity ratings per stimulus pair.
- Fits a linear mixed model to partition variance in similarity ratings between participants and stimulus pairs.
- Concludes that similarity is uniformly high and does not meaningfully differ across pairs.

#### Step 4: Political Nature and Ideological Position
- Uses `pi_check` columns to verify that stimuli are perceived as political.
- Uses `pi_ideology` columns to tabulate the proportion of participants categorizing each stimulus as right-wing, left-wing, or neither.

#### Step 5: Level of Offensiveness
- Placeholder for selecting stimuli with medium levels of offensiveness (not yet implemented).

### 7. Cynicism Scale Analysis

- **Agree/Disagree Scale** (`cinis_ad`): 
  - Selects six items (VD03_01 through VD03_06).
  - Removes participants not assigned to this condition.
  - Computes Spearman correlations and Cronbach's alpha.
  - Notes need to transform -1 values to NA.
- **Frequency Scale** (`cinis_fr`): 
  - Selects six items (VD05_01 through VD10_01).
  - Removes participants not assigned to this condition.
  - Inverts item VD09_01 (6 - x).
  - Computes Spearman correlations and Cronbach's alpha.

### 8. Pending Tasks

- Investigate whether overall perceived funniness/controversy predicts survey completion.
- Check for halo effects in ratings.
- Consolidate descriptive outputs into a single exportable table with human-readable stimulus labels.
- Implement Step 5 of stimuli selection (offensiveness criteria).

## Dependencies

The script requires the following R packages:

- `readxl` — reading Excel files
- `psych` — descriptive statistics, skewness, kurtosis
- `tidyverse` — data manipulation and visualization
- `ggplot2` — plotting
- `dplyr` — data manipulation
- `lme4` — linear mixed-effects models
- `lmerTest` — p-values for mixed models
- `emmeans` — estimated marginal means and contrasts
- `merTools` — tools for mixed model diagnostics
- `ltm` — latent trait models (Cronbach's alpha)
- `misty` — missing data analysis (Little's MCAR test)

## Usage

1. Ensure all dependencies are installed.
2. Place the survey data file in an accessible location.
3. The script will produce descriptive statistics, mixed model results, and stimuli selection classifications.

## Notes

- The script uses `file.choose()` for data import, so the file path is not hard-coded.
- Missing values are coded as -9 in the raw data and converted to `NA`.
- The analysis accounts for the nested structure of the data (ratings within participants and within stimulus pairs) using mixed-effects models.
- The stimuli selection procedure is designed to be conservative, prioritizing stimuli that clearly meet criteria for funniness differentiation, message similarity, and political content.