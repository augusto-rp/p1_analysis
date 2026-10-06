# github 


# Librerias
library(readxl)
library(psych)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(merTools)
library(ltm)
library(misty)

# DATA PREPARATION --------------------------------------------------------

bd <- read_excel(file.choose())
bd <- bd %>% filter(SD04_01 <= 36) #erase people above accepted age
bd[bd == -9]<-NA #transform -9 into NA

# Definition of metadata columns
meta_cols <- c("CASE","SERIAL", "REF","QUESTNNR", "MODE", "AS02","ASO2_CP", "STATUS", "Q_VIEWER", "STARTED","AS01_CP", "AS01", "CI03",
               "VV02_01", "TIME_SUM", "LASTDATA", "STATUS","FINISHED", "Q_VIEWER", "LASTPAGE","MAXPAGE","MISSING", "MISSREL", "TIME_RSI",
               "TIME001","TIME002","TIME003","TIME004","TIME005","TIME006","TIME007","TIME008", "TIME009", 
               "TIME010", "TIME011", "TIME012", "TIME013", "TIME014", "TIME015", "TIME016", "TIME017", "TIME018", "TIME019", "TIME020", "TIME021", "TIME022", 
               "TIME023", "TIME024", "TIME025", "TIME026", "TIME027", "TIME028", "TIME029", "TIME030", "TIME031", "TIME032", "TIME033", "TIME034", "TIME035",
               "TIME036", "TIME037", "TIME038", "TIME039", "TIME040", "TIME041", "TIME042", "TIME043", "TIME044", "TIME045", "TIME046", "TIME047") 


# Definition of variables related to Stimuli Ratings (Funniness, Poelemicality and Similarity )
ck_ <- unlist(lapply(8:47, function(i) {
  paste0("CK", sprintf("%02d", i), "_", c("01", "02", "03", "04", "05"))
}))

pi_check <-paste0("PI", sprintf("%02d",1:40),"_01") #string with columns that are check of whether stimuli are perceived as political or not

pi_ideology <-paste0("PI", sprintf("%02d",41:80), "_01") #string with columns that are check of whether stimuli are perceived as right wing, left wing or neither


# MCAR TEST, as participants are randomly assigned to 1 of 4 conditions (AS02) I have to create subsets to test it

# Test Group 1
group1_data <- bd %>% filter(AS02 == 1)%>%
  select(where(~ !all(is.na(.)))) %>% #to remove variables no one saw in this group
  select(-any_of(meta_cols))  %>% # remove metadata
  select(where(~ var(., na.rm = TRUE) > 0)) %>%  #remove columns with no variance as na.test wont work
  select(starts_with("CK")) #just select variables related to stimuli rating

na.test(group1_data)

# Test Group 2
group2_data <- bd %>% filter(AS02 == 2)%>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-any_of(meta_cols))  %>% 
  select(where(~ var(., na.rm = TRUE) > 0)) %>% 
  select(starts_with("CK"))

na.test(group2_data)



# Test Group 3
group3_data <- bd %>% filter(AS02 == 3)%>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-any_of(meta_cols))  %>% 
  select(where(~ var(., na.rm = TRUE) > 0)) %>% 
  select(starts_with("CK"))

na.test(group3_data) #full data

# Test Group 4
group4_data <- bd %>% filter(AS02 == 4)%>%
  select(where(~ !all(is.na(.)))) %>% 
  select(-any_of(meta_cols))  %>% 
  select(where(~ var(., na.rm = TRUE) > 0)) %>% 
  select(starts_with("CK"))

na.test(group4_data)


# delete created objects
rm(group1_data, group2_data, group3_data, group4_data)


# A person that sees the complete questionnaire is expected to see a total of 14 pages. The following code create a variable "total_p" that indicates how much each person sees
bd <- bd %>%
  mutate(
    total_p = rowSums(!is.na(select(., num_range("TIME", 01:045, width = 3))))
  )

# A person that viewed 5 pages abandoned the survey after seeing the first set of images

# this will help to create regression to know whether some variable is related with missing data


# Lets see if there is some common characteristic among the people that abandoned the survey before the stimuli

# By political affiliation
table(bd$total_p, bd$SD06)

# Does certaing sets of stimuli are related to greater attrition?
table(bd$AS02, bd$total_p)

# Maybe overall perceived funniness (and controvery) of stimuli might be related to that

lm_image<-lm(total_p~+avg_03, data=base)
summary(lm_image)

rm(lm_image)

# Maybe this should be a logistic regression rather than a linearl, grouping people with less than X number os seen images



# DESCRIPTIVE:SAMPLE CHARACTERISTICS --------------------------------------

# First characterize by
# Political Affiliation
describe(bd$SD06)
bd %>%
  filter(SD06 >= 1, SD06 <= 9) %>%
  ggplot(aes(x = factor(SD06))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Identidad Politica", y = "Numero") +
  theme_minimal()

# Gender
table(bd$SD01)

#Education
table(bd$SD07) #overwhelming majority of university educated people

# Overall ratings by individual
# lets create for each individual a column that shows the average score among _01, _02, _03, _04, _5
suffixes <- c("01", "02", "03", "04", "05")

for (s in suffixes) {
  #1. Generate all column names that end with this suffix (from CK08 to CK47)
  cols_to_avg <- paste0("CK", sprintf("%02d", 8:47), "_", s)
  
  #2. Calculate the row-wise mean and assign it to a new column (e.g., avg_01)
  base[[paste0("avg_", s)]] <- rowMeans(base[, cols_to_avg], na.rm = TRUE)
}


rm(suffixes)

# Now for each individual I have an average of the overall rating of each stimuli variable
# this also provides a quick way to search for responses that seems suspicious
# as someone that answers the same in each question will have the same rating on all 5 columns



# DESCRIPTIVE:HUMOR/POLEMIC/SIMILARITY ------------------------------------

## Humorous Stimuli -meme descriptives #####

# Lets create a df of the ratings of funniness
cols_f_meme <- paste0("CK", sprintf("%02d", 8:47), "_03") 
describe(bd[, cols_f_meme])

# Let's check raw frequency. As extreme deviations are expected due to ideological affinity to the content of the memes
freq_f_meme <- bd %>%
  select(all_of(cols_f_meme)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "response") %>%
  filter(!is.na(response)) %>%              # drop NA responses BEFORE counting because participant see just a selection of the memes
  mutate(response = as.integer(response)) %>%
  count(item, response) %>%
  complete(item, response = 1:5, fill = list(n = 0)) %>%
  pivot_wider(names_from = response,
              values_from = n,
              values_fill = 0) %>%
  mutate(total = rowSums(across(`1`:`5`)))  


# Lets create a df of ratings of perceived Polemicality

cols_p_meme <- paste0("CK", sprintf("%02d", 8:47), "_01") #ck##_01 is the code for polemic of the meme images
describe(bd[, cols_p_meme])

## Non Humorous Stimuli -vignette descriptives ######

# Lets create a df of the ratings of funniness
cols_f_serio <- paste0("CK", sprintf("%02d", 8:47), "_04") #creation of a separate df of the ratings of funiness
describe(bd[, cols_f_serio])

# Raw Frequencies
freq_f_serio <- bd %>%
  select(all_of(cols_f_serio)) %>%
  pivot_longer(everything(), names_to = "item", values_to = "response") %>%
  filter(!is.na(response)) %>%              # drop NA responses BEFORE counting because participant see just a selection of the memes
  mutate(response = as.integer(response)) %>%
  count(item, response) %>%
  complete(item, response = 1:5, fill = list(n = 0)) %>%
  pivot_wider(names_from = response,
              values_from = n,
              values_fill = 0) %>%
  mutate(total = rowSums(across(`1`:`5`)))  # add total per item

# Lets create a df of ratings of perceived Polemicality
cols_p_serio <- paste0("CK", sprintf("%02d", 8:47), "_02") #ck##_02 is the code for polemic of the "serious images
describe(bd[, cols_p_serio])

## Similarity between memes and serious vignettes #####
cols_similitud <- paste0("CK", sprintf("%02d", 8:47), "_05") #creation of a separate df of the ratings of funniness
describe(bd[, cols_similitud])

# All of this is a non parsimonious way of doing this analysis. But for my mind it works as a way to focus on just one descriptive variable
# But I should probably consolidate all of this in a single df
# Now lets create a df to see all the descriptives of ck##_01, 02,03, 04 and 0,5 in a single
# Lets use n, mean, sd, skew, kurtosis, se, a

cols_ck <- c(cols_p_meme,
             cols_p_serio,
             cols_f_meme,
             cols_f_serio,
             cols_similitud)
desc_ck <- bd %>%
  select(all_of(cols_ck)) %>%
  # make sure everything is numeric
  mutate(across(everything(), ~ as.numeric(as.character(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n        = sum(!is.na(value)),
    mean     = mean(value, na.rm = TRUE),
    sd       = sd(value, na.rm = TRUE),
    skew     = psych::skew(value, na.rm = TRUE),
    kurtosis = psych::kurtosi(value, na.rm = TRUE),
    se       = sd / sqrt(n),
    .groups  = "drop"
  ) %>%
  arrange(variable) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) #just 3 decimals

# I end up with a nice table of description of all the descriptives of the stimuli
# However it would be a good idea to transform this into an excel with the names of the topic and question rather than CK08_03

rm(cols_ck, cols_f_meme, cols_f_serio, cols_p_meme, cols_p_serio, cols_similitud, freq_f_meme, freq_f_serio) #lets save RAM



# Formatting Data for Analysis of Stimuli ---------------------------------

# Given that ratings are nested in pairs of images and individual, MLM is a better way to assess effects of humorness than just mean comparisions

# This means I need to transform data into long format

# I want to select just some variables
# Gender, Political Identification, Set of Assignment, Total pages viewed, ck_

# selection of variables to be cut from bd
selected_vars <- c("SD01", "SD06","AS02","total_p", ck_)  

#and now cut that from bd
base <- bd[, selected_vars]

# Long format
base<-base %>%
  mutate(id=row_number())%>%
  pivot_longer(
    cols = all_of(ck_),
    names_to = c("ck_","question"), 
    names_pattern = "^CK([0-9]+)_([0-9]+)$",
    values_to = "ck_value"
  ) %>%
  mutate(question = as.integer(question)) 


# reorder
base <- base %>% 
  select(id, SD01, SD06,AS02,total_p, ck_, question, ck_value)

base$SD01<-as.factor(base$SD01) #gender as a factor  
str(base)

# CRITERIA FOR SELECTION OF STIMULI ---------------------------------------

# 1  Check significant differences between the meme version and serious version on perceived funniness
#if non significant, the pair of images on which said image belongs is not considered for further analysis. Also the topic cluster is discarded
#for example: image 8 and 9 talk about the same topic but from ideologically different positions. Both need to have significant differences between version 
#if significant go to step 2

# 2 Check that perceived funniness of the humorous version is equal to or greater than the average of all the humorous version: Check Descriptive
# &
#Check that the perceived funniness of the serious version is equal to or smaller than the average of all the serious version: Check Descriptive

#if a expected funny image average is clearly below the average is should be discarded
#if a expected unfunny image is clearly above the average is should be discarded

# 3 Check that the funny and serious version of the same message are viewed as having a similar message: Equivalence testing (TOST)
#if either case fails the whole topic to which the image belong is discarded, if it the condition is met go to step 4

# 4 Check that the images that belong to the same topic are viewed as :expressing political opinions and expressing opposing points of view
# So for example image 8 should be viewed as expressing a right wing opinion and image 9 a left wing opinion

# 5 With the remaining topics of images select those with medium levels of offensiveness compare to the mean

# Step 0: Fixed and Random Effects on Funniness ---------------------------

##### The following analysis is to view the fixed and random effects on the perceived funniness of the images

fun_df <- base %>%
  filter(question %in% c(3, 4)) %>% #values of 3(meme version) and 4 (serious version) in column question
  mutate(
    version   = factor(if_else(question == 3, "H", "S"), levels = c("S", "H")), #this establishes serious as the baseline factor 
    id        = factor(id),
    ck_id     = factor(ck_),
    funniness = as.numeric(ck_value)
  ) %>%
  select(id, ck_, version, funniness)

fun_df$ck_  <- factor(fun_df$ck_)

# Fit
m_fun <- lmer(funniness ~ version + (version | ck_) + (1 | id),  data = fun_df,  REML = TRUE) 

#Funniness is modeled using the image version (S vrs H) as a fixed effect.
#With random intercepts for participant and random intercepts and slopes for version by pair

summary(m_fun)



#This gives an estimate of effect of type of images on the perceived funniness 

##extreme residuals at the min, but honestly that is kind of expected given the topic
# HUGE DIFFERENCES BETWEEN Humorous and Serious images


#Negative correlation means that funnier pairs of images have smaller H-S differences --->BETWEEN CORRELATION
#Worth checking whether there is a halo effect: within participant effect. Participant with higher H should also rate higher S than predicted?


# How each intercept (1.73) and slope (1.84) varies from the model (BLUPS)

emmeans(m_fun, pairwise ~ version)
ranef(m_fun)$ck_ 

# Step 1:  Differences of funniness between versions ----------------------

### Using a less conservative estimate: Benjamini-Hochberg (BH)

f_pares <- lmer(funniness ~ ck_ * version + (1 | id), data = fun_df) #funiness is modeled using the interaction between overall funnines of pair of images * meme version and adding intercept by individual

emm_pares <- emmeans(f_pares, ~ version | ck_)
con_pares <- contrast(emm_pares, method = "revpairwise")
res_pares <- summary(con_pares, by = NULL, infer = TRUE, adjust = "fdr") #also worth checking when using holm
summary(res_pares)



# Step 2: Comparision of average Humorous and Serious Stimuli across Means

# This is a descriptive step of the data validation
# A meme can be significantly more humorous that the serious version of the same message and still be perceived as unfunny
# Therefore any meme that is below the mean perception of funniness will be discarded
# Similarly serious messages that are above the funniness average will also be discarded 


emm_ver <- update(emm_pares, by = "version") #grouping by version instead than pairs of images

dev_df <- contrast(emm_ver, method = "eff") |>
  summary(infer = TRUE, adjust = "none") |>   # no adjustment: descriptive, not a formal test
  as.data.frame() |>
  mutate(ck_id = sub(" effect$", "", contrast)) |>
  select(ck_id, version, dev = estimate, SE, lower.CL, upper.CL)


rm(fun_check)

fun_check <- dev_df |>
  pivot_wider(names_from = version,
              values_from = c(dev, SE, lower.CL, upper.CL)) |>
  mutate(
    # Point-estimate criteria
    H_pass = dev_H >= 0,    # iH at or above the mean of all iH
    S_pass = dev_S <= 0,    # iS at or below the mean of all iS
    both_pass = H_pass & S_pass,
    
    # How sure are we about each criterion?
    H_status = case_when(lower.CL_H >= 0 ~ "clearly above",
                         upper.CL_H <  0 ~ "clearly below",
                         TRUE            ~ "ambiguous"),
    S_status = case_when(upper.CL_S <= 0 ~ "clearly below",
                         lower.CL_S >  0 ~ "clearly above",
                         TRUE            ~ "ambiguous"),
    
    # Selection decision
    selection = case_when(
      H_status == "clearly above" & S_status != "clearly above" ~ "PASS",
      H_status == "ambiguous"     & S_status != "clearly above" ~ "CHECK-H",
      H_status == "clearly above" & S_status == "clearly above" ~ "CHECK-S",
      H_status == "clearly below"                               ~ "FAIL",
      H_status == "ambiguous"     & S_status == "clearly above" ~ "FAIL",
      TRUE                                                       ~ NA_character_
    )
  )


fun_check |> count(H_status, S_status, selection) |> print(n = Inf)

rm(dev_df, fun_check, f_pares, emm_pares, emm_ver, fun_df, res_pares)

# Given the bias in the sample one should not immediatly discard certain cases
# For example the serious version of ck31 is seen as more fun that the mean of the non serious version.
# But this particular image is a left wing commentary on right wing hypocrisy so maybe this is an effect of the sample


# I can also use a quite strict cut off of no deviation greater than 0.3 in the likert scale as a criteria to select memes

# Step 3:  Similarity of Message Content

similitud <- aggregate(ck_value ~ ck_, 
                       data = subset(base, question == 5), 
                       FUN = function(x) mean(x, na.rm = TRUE))

#All pairs had a mean similarity rating ≥ 4 (on a 1–5 scale), indicating that every pair was judged to express the same content. 
#No further test of this was performed because the effect was unambiguous."

#However I can still test whether some pairs are significant different from other ones in similarity

base %>%
  filter(question == 5, !is.na(ck_)) %>%
  group_by(ck_) %>%
  summarise(m = mean(ck_value, na.rm = TRUE),
            sd = sd(ck_value, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  summarise(range = max(m) - min(m),
            sd_of_means = sd(m))

# A range of less than a point (0.867). A tiny sd


# Is there any variation across pairs? Considering the existing nesting
# How much variance is due to participant? Is there an effect worth exploring?

m_simil<- lmer(ck_value ~ 1 + (1 | id) + (1 | ck_),
               data = subset(base, question == 5 & !is.na(ck_)))
summary(m_simil)
ranova(m_simil)


# There is some differences in how individuals rate, but as a whole memes are being viewed similarly
# Similarity ratings were analysed with a linear mixed model with random intercepts for participant and image pair.
#Participant identity accounted for 38.9% of the variance (SD = 0.51), indicating substantial individual differences in scale use.
#Image pair accounted for only 0.9% of the variance (SD = 0.08), confirming that pairs did not differ meaningfully in perceived similarity.
#The grand mean rating was 4.53 (SE = 0.07), well within the agreement range, supporting the conclusion that all pairs were perceived as expressing the same content.



# Step 4: Check of message nature and ideological position

# PI01 to PI40 are checks that the images are perceived as political (2) they should all be 2 to little to no deviation
#now im going to use pi_check
describe(bd[pi_check]) #PI33 to PI4O should be closer to 1, the rest seem to work just fine


# PI41 to PI80 question whether the image is perceived as right wing (1) left wing (2) or neither (3)
#now im going to use pi_ideology

#rather than means i want to use raw frequency as here 



orientacion_stimuli<-do.call(rbind, lapply(bd[pi_ideology], function(x) {
  t <- table(factor(x, levels = 1:3), useNA = "no")
  c(t, prop.table(t) * 100)
}))


orientacion_stimuli <- as.data.frame(orientacion_stimuli)

#Interetingly the memes of being broke because of going to a concert are seen as right wing
# Most stimuli seen to be perceived as intended


# Step 5: Level of offensiveness



# ...and the winners are... -----------------------------------------------





# Analysis of Cynicism Scales ---------------------------------------------


# AGREE/DISAGREE SCALE
cinis_ad<-bd[,c("VD03_01","VD03_02","VD03_03","VD03_04","VD03_05","VD03_06")]  
cinis_ad<-cinis_ad[rowSums(is.na(cinis_ad))<ncol(cinis_ad),] #remove cases that wer enot assigned to this condition-->all NA, should be half the total

cor(cinis_ad, use = "everything", method = c("spearman"))
cronbach.alpha(cinis_ad, CI=T)

describe(cinis_ad) # i have to transform -1 into NA




# FREQUENCY SCALE
cinis_fr<-bd[,c("VD05_01","VD06_01","VD07_01","VD08_01","VD09_01","VD10_01")] #frequency VD09 is inverted
cinis_fr<-cinis_fr[rowSums(is.na(cinis_fr))<ncol(cinis_fr),]


describe(cinis_fr) # check descriptives before inverting to check everything is alright


cinis_fr<-cinis_fr%>%
  mutate(VD09_01=6-VD09_01)


cor(cinis_fr, use = "everything", method = c("spearman")) #WHAT


cronbach.alpha(cinis_fr, CI=T) 



#just lets see correlations between scaes


# PENDING TASKS -----------------------------------------------------------


# Check if overall perception of funniness/controverys is related to greater degree of survey completion

## Regarding analyses #####
# check for halo effects
