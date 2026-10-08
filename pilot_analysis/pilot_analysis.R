
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
library(stargazer)

# DATA PREPARATION --------------------------------------------------------

options(scipen = 999)

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


# A person that sees the complete questionnaire is expected to see a total of 14 pages. The following code create a variable "total_p" that indicates how much each person sees
bd <- bd %>%
  mutate(
    total_p = rowSums(!is.na(pick(num_range("TIME", 01:045, width = 3))))
  )

#Lets also create a dichotomic variable that differentiates between people who responden <50% of images (9 pages in total)
bd$total_p_d <- ifelse(bd$total_p <= 9, 0, 1)


# Overall ratings by individual
# lets create for each individual a column that shows the average score among _01, _02, _03, _04, _5
suffixes <- c("01", "02", "03", "04", "05")

for (s in suffixes) {
  #1. Generate all column names that end with this suffix (from CK08 to CK47)
  cols_to_avg <- paste0("CK", sprintf("%02d", 8:47), "_",s)
  
  #2. Calculate the row-wise mean and assign it to a new column (e.g., avg_01)
  bd[[paste0("avg_", s)]] <- rowMeans(bd[, cols_to_avg], na.rm = TRUE)
}


rm(suffixes,cols_to_avg,s)

# Now for each individual I have an average of the overall rating of each stimuli variable
# this also provides a quick way to search for responses that seems suspicious
# as someone that answers the same in each question will have the same rating on all 5 columns


# Formatting Data for Analysis of Stimuli ---------------------------------

# Given that ratings are nested in pairs of images and individual, MLM is a better way to assess effects of humorness than just mean comparisions

# This means I need to transform data into long format

# I want to select just some variables
# Gender, Political Identification, Set of Assignment, Total pages viewed, ck_

# selection of variables to be cut from bd
selected_vars <- c("SD01", "SD06","AS02","total_p","total_p_d", ck_)  

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
  dplyr::select(id, SD01, SD06,AS02,total_p,total_p_d,ck_, question, ck_value)



base$SD01<-as.factor(base$SD01) #gender as a factor
base$total_p_d<-as.factor(base$total_p_d) #1 is <=9 pages
str(base)

rm(selected_vars)


# Missing Data Analysis ---------------------------------------------------


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



# A person that viewed 5 pages abandoned the survey after seeing the first set of images

# this will help to create regression to know whether some variable is related with missing data

# Lets see if there is some common characteristic among the people that abandoned the survey before the stimuli

# By political affiliation
table(bd$total_p, bd$SD06)

# Does certaing sets of stimuli are related to greater attrition?
table(bd$AS02, bd$total_p)

# Maybe overall perceived funniness (and controvery) of stimuli might be related to that

lm_image<-lm(total_p~+avg_03, data=bd) #First you need to run code that cretes avg_03  in the following section
summary(lm_image) 

# Logistic Regression

gl_image_null<-glm(total_p_d~1, data = bd, family="binomial") #null model
summary(gl_image_null)

gl_image<-glm(total_p_d~avg_03, data = bd, family="binomial")
summary(gl_image) #funinesss doesnt appear to influence wheter someone sees more than 50% of images

stargazer(gl_image_null, gl_image, type="text")

rm(lm_image, gl_image, gl_image_null)


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




#average time of completion
describe(bd$TIME_SUM) 


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
  mutate(total = rowSums(across(`1`:`5`)))  # add total per item

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


## Joint Descriptives #####
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


# Some Graphs -------------------------------------------------------------

# Funniness: Comparasion of memes and serious version

base %>%
  filter(question %in% c(3, 4)) %>%
  group_by(question) %>%
  summarise(
    mean     = mean(ck_value, na.rm = TRUE),
    median   = median(ck_value, na.rm = TRUE),
    sd       = sd(ck_value, na.rm = TRUE),
    skew     = psych::skew(ck_value, na.rm = TRUE),
    kurtosis = psych::kurtosi(ck_value, na.rm = TRUE),
    n        = n() # sample size for each question
  )


base_fun <- base %>%
  filter(question %in% c(3, 4)) %>%
  mutate(question = factor(question, levels = c(3, 4), labels = c("V. Meme", "V. Seria")))

# 2. Density Plot Superimpossed
ggplot(base_fun, aes(x = ck_value, fill = question, color = question)) +
  geom_density(alpha = 0.4, linewidth = 1) + # alpha = 0.4 creates the transparent overlap
  labs(
    title = "Comparison of Distributions for Funniness",
    x = "CK Value (Rating)",
    y = "Density",
    fill = "Question",
    color = "Question"
  ) +
  theme_minimal()




# Polemicality: Comparision of memes and serious version

base %>%
  filter(question %in% c(1, 2)) %>%
  group_by(question) %>%
  summarise(
    mean     = mean(ck_value, na.rm = TRUE),
    median   = median(ck_value, na.rm = TRUE),
    sd       = sd(ck_value, na.rm = TRUE),
    skew     = psych::skew(ck_value, na.rm = TRUE),
    kurtosis = psych::kurtosi(ck_value, na.rm = TRUE),
    n        = n() # sample size for each question
  )


base_polemic <- base %>%
  filter(question %in% c(1, 2)) %>%
  mutate(question = factor(question, levels = c(1, 2), labels = c("V. Meme", "V. Seria")))

# 2. Density Plot Superimpossed
ggplot(base_polemic, aes(x = ck_value, fill = question, color = question)) +
  geom_density(alpha = 0.4, linewidth = 1) + # alpha = 0.4 creates the transparent overlap
  labs(
    title = "Comparison of Distributions for Polemicality",
    x = "CK Value (Rating)",
    y = "Density",
    fill = "Question",
    color = "Question"
  ) +
  theme_minimal()


rm(base_fun, base_polemic)



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

##### The following analysis is to view the fixed effect of version and random effects on the perceived funniness of the images

fun_df <- base %>%
  filter(question %in% c(3, 4)) %>% #values of 3(meme version) and 4 (serious version) in column question
  mutate(
    version   = factor(if_else(question == 3, "H", "S"), levels = c("S", "H")), #this establishes serious as the baseline factor 
    id        = factor(id),
    ck_id     = factor(ck_),
    funniness = as.numeric(ck_value)
  ) %>%
  dplyr::select(id, ck_, version, funniness)

fun_df$ck_  <- as.factor(fun_df$ck_)

# Linear models of perceveid funniness

#just intercepts by individual 
m0 <- lmer(funniness ~ version + (1 | id) , data = fun_df, REML = TRUE)  #REML as im going to compare different models with the same fixed effect
summary(m0)


#just intercepts by individual and intercept by image pair
m1 <- lmer(funniness ~ version + (1 | id) + (1 | ck_), data = fun_df, REML = T)
summary(m1)

# Pair differences in baseline funniness and pair differences in humor effect. Plus slope for humor effects within individual that has more data points

m2 <- lmer(funniness ~ version + (1+ version | id) + (1 | ck_), data = fun_df, REML = T) # || to not use correlation as it doesnt cONVERGE IF I USE REML AND use | instead of ||
summary(m2)
#even participants to weak response to version H still see a positive increase intercept-(1*std versionH)



# Maximal effect model that does not converge
m3<- lmer(funniness ~ version + (1 + version | id) + (1 + version | ck_), data = fun_df, REML = TRUE)


anova(m0,m1,m2) #m2 seems the best option
isSingular(m2)      # #
VarCorr(m2)


#I should center with sum-coding so the intercept b


rm(m0,m1,m3)


#This gives an estimate of effect of type of images on the perceived funniness 

# HUGE DIFFERENCES BETWEEN Humorous and Serious images


#Negative correlation means that funnier pairs of images have smaller H-S differences --->BETWEEN CORRELATION

# lets see the BLUPS
ranef(m2)$ck_ 

# Step 1:  Differences of funniness between versions across pairs ----------------------


### Using a less conservative estimate: Benjamini-Hochberg (BH)

# I should recenter to +0,5 and -0,5 but if i do so if affects the analyses i do from step 2 onwards

# fun_df$version_c <- ifelse(fun_df$version == "H", 0.5, -0.5)

#set contrast to the grand mean not ck_08
ck_levels <- levels(fun_df$ck_) #to avoid contrast() changing the names of ck
ck_contrasts <- contr.sum(length(ck_levels))
dimnames(ck_contrasts) <- list(ck_levels, ck_levels[1:(length(ck_levels) - 1)])
contrasts(fun_df$ck_) <- ck_contrasts #setting contrast to grand mean

#First model with no random effects, 
f_pares <- lm(funniness ~ ck_ *version_c , data = fun_df) 
summary(f_pares)

# Intercept for participant
f_pares1 <- lmer(funniness ~ ck_ * version + (1 | id),
                 data = fun_df, REML = TRUE)

# Intercept + slope for participant. I cant add intercepts for ck_
f_pares2 <- lmer(funniness ~  ck_ * version+ (1+ version | id),
                 data = fun_df, REML = TRUE)
summary(f_pares2)
# no centering so beware of interpretation

# ck_ effects : Across the baseline non-humorous version ($S$), pair nn is rated $  $ points higher in funniness than the average image pair
# version_H: For the average image pair, switching from version $S$ to version $H$ increases perceived funniness by $  $ point
# ck_nn::version_c: The humor boost ($H - S$) for pair nn is $ $ points smaller/bigger than the overall average humor effect (resulting in a net increase of $versionH - $CK_NN::versionH value

# Because it is dummy coded
# The main effects of ck_ reflect differences specifically within the baseline version ($S$), rather than averaged across both versions.
# The main effect of versionH reflects the humor boost for a typical pair (because ck_ is sum-coded to the grand mean).

anova(f_pares1, f_pares2, refit=FALSE)


# this is wordy but i need to do it this way to have the necessary objects for the next section
emm_pares <- emmeans(f_pares2, ~ version | ck_)
con_pares <- contrast(emm_pares, method = "revpairwise")
res_pares <- summary(con_pares, by = NULL, infer = TRUE, adjust = "fdr") #also worth checking when using holm
summary(res_pares)




print(VarCorr(f_pares), comp = "Variance")
print(VarCorr(f_pares2), comp = "Variance")
print(VarCorr(f_pares3), comp = "Variance") #to compare where is variance explanined



rm(f_pares, f_pares1, con_pares, res_pares)


# Step 2: Comparision of average Humorous and Serious Stimuli across Means ----------------------

# This is a descriptive step of the data validation
# A meme can be significantly more humorous that the serious version of the same message and still be perceived as unfunny
# Therefore any meme that is below the mean perception of funniness will be discarded
# Similarly serious messages that are above the funniness average will also be discarded 




emm_ver <- update(emm_pares, by = "version") #grouping by version instead than pairs of images #comparision of each pair agiants overall mean

dev_df <- contrast(emm_ver, method = "eff") |>
  summary(infer = TRUE, adjust = "none") |>   # no adjustment: descriptive, not a formal test
  as.data.frame() |>
  mutate(ck_id = sub(" effect$", "", contrast)) |>
  dplyr::select(ck_id, version, dev = estimate, SE, lower.CL, upper.CL)




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

rm(dev_df)



# Given the bias in the sample one should not immediatly discard certain cases
# For example the serious version of ck31 is seen as more fun that the mean of the non serious version.
# But this particular image is a left wing commentary on right wing hypocrisy so maybe this is an effect of the sample


# I can also use a quite strict cut off of no deviation greater than 0.3 in the likert scale as a criteria to select memes

# Step 3:  Similarity of Message Content ----------------------

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

# A range of less than a point (0.875). A tiny sd


# Is there any variation across pairs? Considering the existing nesting
# How much variance is due to participant? Is there an effect worth exploring?

d_sim <- subset(base, question == 5 & !is.na(ck_)) #subset of interest



#lets add just mean of participants
simil1<-lmer(ck_value ~ 1 + (1 | id), data = d_sim, REML = TRUE)


#lets add just mean of pai of images
simil2<-lmer(ck_value ~ 1 + (1 | ck_), data = d_sim, REML = TRUE)       


#lets add both pair and individual
simil3<- lmer(ck_value ~ 1 + (1 | id) + (1 | ck_),
               data = d_sim, REML=T)
summary(simil3)

anova(simil1, simil3, refit=FALSE) #this keeps the REML that are useful for identical fixed effects
#simil1 is more parsimonious but simil3 includes a more complext strucyure

anova(simil2, simil3, refit=FALSE) 

VarCorr(simil3)

rm(simil1,simil2, similitud)

# From previous analyses
# There is some differences in how individuals rate, but as a whole memes are being viewed similarly
# Similarity ratings were analysed with a linear mixed model with random intercepts for participant and image pair.
#Participant identity accounted for 38.9% of the variance (SD = 0.51), indicating substantial individual differences in scale use.
#Image pair accounted for only 0.9% of the variance (SD = 0.08), confirming that pairs did not differ meaningfully in perceived similarity.
#The grand mean rating was 4.53 (SE = 0.07), well within the agreement range, supporting the conclusion that all pairs were perceived as expressing the same content.

# Step 4: Check of message nature and ideological position ----------------

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


rm(orientacion_stimuli)

# Step 5: Level of offensiveness ------------------------------------------


#Similar to step 1 lets first create a df with ratings of controversy

polemic_df <- base %>%
  filter(question %in% c(1, 2)) %>% #values of 1(controversy of meme version) and 2 (controversy of serious version) in column question
  mutate(
    version   = factor(if_else(question == 1, "H", "S"), levels = c("S", "H")), #this establishes serious as the baseline factor 
    id        = factor(id),
    ck_id     = factor(ck_),
    controversial = as.numeric(ck_value)
  ) %>%
  select(id, ck_, version, controversial)

polemic_df$ck_  <- as.factor(polemic_df$ck_)


ck_levels <- levels(polemic_df$ck_) #to avoid contrast() changing the names of ck
ck_contrasts <- contr.sum(length(ck_levels))
dimnames(ck_contrasts) <- list(ck_levels, ck_levels[1:(length(ck_levels) - 1)])
contrasts(polemic_df$ck_) <- ck_contrasts #setting contrast to grand mean



# no centering so beware of interpretation

# ck_ effects : Across the baseline non-humorous version ($S$), pair nn is rated $  $ points higher in polemicality than the average image pair
# version_H: For the average image pair, switching from version $S$ to version $H$ increases perceived controversiality by $  $ point
# ck_nn::version_c: The humor boost ($H - S$) for pair nn is $ $ points smaller/bigger than the overall average humor effect (resulting in a net increase of $versionH - $CK_NN::versionH value

# Because it is dummy coded
# The main effects of ck_ reflect differences specifically within the baseline version ($S$), rather than averaged across both versions.
# The main effect of versionH reflects the humor boost for a typical pair (because ck_ is sum-coded to the grand mean).

#First model with no random effects, 
c_pares <- lm(controversial ~ ck_ * version , data = polemic_df) #controversy/polemicality is modeled using the interaction between overall controversy of pair of images * meme version and adding intercept by individual
summary(c_pares)

# Intercept for participant
c_pares1 <- lmer(controversial ~ ck_ * version + (1 | id),
                 data = polemic_df, REML=T)
summary(c_pares1)#humor version actually increases perceived polemicality


# Intercept + slope for participant. I cant add intercepts for ck_
c_pares2 <- lmer(controversial ~ ck_ * version + (1+ version | id),
                 data = polemic_df, REML=T)


anova(c_pares1, c_pares2) #better fit by c_pares1 difference for 2 is significant but look at BIC


print(VarCorr(c_pares), comp = "Variance")
print(VarCorr(c_pares1), comp = "Variance")
print(VarCorr(c_pares2), comp = "Variance")

#Comparing funniness models and polemicality


c_pares_comp <- emmeans(c_pares1, ~ version | ck_) %>%
  contrast(method = "revpairwise") %>%
  summary(by = NULL, infer = TRUE, adjust = "fdr")
summary(c_pares_comp) #not a single difference

# This is weird c_pares1 suggest a signficiant effect of humor on perception a topic is controversial, but not a single pairwise comparision shows it
# This means the effect is diffuse and small across stimuli
# This indicates that humor exerts a small, widespread, and uniform increase on perceived controversialness across the battery, rather than being concentrated in specific image pairs.


#This suggest that I can select the pairs of images I'm gooing to use base just in their ranking of polemicality. Given Im guaranteed no matter which one I select there are similarly rated on this dimension

rm(c_pares, cpares_2)


# ...and the winners are... -----------------------------------------------






# Analysis of Cynicism Scales ---------------------------------------------


# AGREE/DISAGREE SCALE
cinis_ad<-bd[,c("VD03_01","VD03_02","VD03_03","VD03_04","VD03_05","VD03_06")]  
cinis_ad<-cinis_ad[rowSums(is.na(cinis_ad))<ncol(cinis_ad),] #remove cases that were not assigned to this condition-->all NA, should be half the total

cor(cinis_ad, use = "everything", method = c("spearman"))
cronbach.alpha(cinis_ad, CI=T)

describe(cinis_ad) # i have to transform -1 into NA

#CFA
cinis_ad[cinis_ad == -1]<-NA
cinis_ad<-na.omit(cinis_ad[1:6])
cinis_ad_cor<-cor(cinis_ad)

str(cinis_ad)

library(lavaan) #i have insufficient data for a proper CFA
mod1<- 'cinismo=~VD03_01+VD03_02+VD03_03+VD03_04+VD03_05+VD03_06'
fit<-cfa(mod1,std.lv=T, estimator ="MLR", data=cinis_ad)
summary(fit, fit.measures=T, standarized=T)


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

 
