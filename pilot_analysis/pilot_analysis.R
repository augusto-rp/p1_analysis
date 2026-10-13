#github


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
library(ltm) # attaches MASS after dplyr so i use dplyr::select when needed
library(misty)
library(stargazer)
library(purrr)
library(tibble)

# DATA PREPARATION --------------------------------------------------------

options(scipen = 999)

bd <- read_excel(file.choose())
bd[bd == -9]<-NA #transform -9 into NA
bd <- bd %>% filter(SD04_01 <= 36) #erase people above accepted age


# Definition of metadata columns
meta_cols <- c("CASE","SERIAL", "REF","QUESTNNR", "MODE", "AS02","AS02_CP", "STATUS", "Q_VIEWER", "STARTED","AS01_CP", "AS01", "CI03",
               "VV02_01", "TIME_SUM", "LASTDATA", "STATUS","FINISHED", "Q_VIEWER", "LASTPAGE","MAXPAGE","MISSING", "MISSREL", "TIME_RSI",
               "TIME001","TIME002","TIME003","TIME004","TIME005","TIME006","TIME007","TIME008", "TIME009", 
               "TIME010", "TIME011", "TIME012", "TIME013", "TIME014", "TIME015", "TIME016", "TIME017", "TIME018", "TIME019", "TIME020", "TIME021", "TIME022", 
               "TIME023", "TIME024", "TIME025", "TIME026", "TIME027", "TIME028", "TIME029", "TIME030", "TIME031", "TIME032", "TIME033", "TIME034", "TIME035",
               "TIME036", "TIME037", "TIME038", "TIME039", "TIME040", "TIME041", "TIME042", "TIME043", "TIME044", "TIME045") 


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
base$total_p_d<-as.factor(base$total_p_d) #0 is <=9 pages
str(base)

rm(selected_vars)

# Missing Data Analysis ---------------------------------------------------


# MCAR TEST, as participants are randomly assigned to 1 of 4 conditions (AS02) I have to create subsets to test it

# Test Group 1
group1_data <- bd %>% filter(AS02 == 1)%>%
  dplyr::select(where(~ !all(is.na(.)))) %>% #to remove variables no one saw in this group
  dplyr::select(-any_of(meta_cols))  %>% # remove metadata
  dplyr::select(where(~ var(., na.rm = TRUE) > 0)) %>%  #remove columns with no variance as na.test wont work
  dplyr::select(starts_with("CK")) #just select variables related to stimuli rating

na.test(group1_data)



# Test Group 2
group2_data <- bd %>% filter(AS02 == 2)%>%
  dplyr::select(where(~ !all(is.na(.)))) %>% 
  dplyr::select(-any_of(meta_cols))  %>% 
  dplyr::select(where(~ var(., na.rm = TRUE) > 0)) %>% 
  dplyr::select(starts_with("CK"))

na.test(group2_data)



# Test Group 3
group3_data <- bd %>% filter(AS02 == 3)%>%
  dplyr::select(where(~ !all(is.na(.)))) %>% 
  dplyr::select(-any_of(meta_cols))  %>% 
  dplyr::select(where(~ var(., na.rm = TRUE) > 0)) %>% 
  dplyr::select(starts_with("CK"))

na.test(group3_data) #full data

# Test Group 4
group4_data <- bd %>% filter(AS02 == 4)%>%
  dplyr::select(where(~ !all(is.na(.)))) %>% 
  dplyr::select(-any_of(meta_cols))  %>% 
  dplyr::select(where(~ var(., na.rm = TRUE) > 0)) %>% 
  dplyr::select(starts_with("CK"))

na.test(group4_data)


# delete created objects
rm(group1_data, group2_data, group3_data, group4_data)





# Lets see if there is some common characteristic among the people that abandoned the survey before the stimuli

# By political affiliation
table(bd$total_p_d, bd$SD06)

# Does certain sets of stimuli are related to greater attrition?
table(bd$AS02, bd$total_p_d)


# Lets see how the averages of pereceptions of polemicality, funniness and similarity varies across assignmetn groups
# The assignment of images to each set was random, as was the assignment to condition. SO any difference between groups is pure chance

# Polemicality of memes
bd %>%
  group_by(AS02) %>%
  dplyr::summarise(
    mean     = mean(avg_01, na.rm = TRUE),
    median   = median(avg_01, na.rm = TRUE),
    sd       = sd(avg_01, na.rm = TRUE),
    skew     = psych::skew(avg_01, na.rm = TRUE),
    kurtosis = psych::kurtosi(avg_01, na.rm = TRUE),
    n        = sum(!is.na(avg_01)),
    se       = sd / sqrt(n),
    count = n(),
    .groups = 'drop'
  ) # Difference between count and N is because some people in a group has answers for the cynicism scale but didnt saw any image SO nor rating for them.I mantaing this data for analysis of the cynicism scales


# Polemicality of serious stimuli
bd %>%
  group_by(AS02) %>%
  dplyr::summarise(
    mean     = mean(avg_02, na.rm = TRUE),
    median   = median(avg_02, na.rm = TRUE),
    sd       = sd(avg_02, na.rm = TRUE),
    skew     = psych::skew(avg_02, na.rm = TRUE),
    kurtosis = psych::kurtosi(avg_02, na.rm = TRUE),
    n        = sum(!is.na(avg_02)),
    se       = sd / sqrt(n),
    count = n(),
    .groups = 'drop'
  )


# Funniness of memes
bd %>%
  group_by(AS02) %>%
  dplyr::summarise(
    mean     = mean(avg_03, na.rm = TRUE),
    median   = median(avg_03, na.rm = TRUE),
    sd       = sd(avg_03, na.rm = TRUE),
    skew     = psych::skew(avg_03, na.rm = TRUE),
    kurtosis = psych::kurtosi(avg_03, na.rm = TRUE),
    n        = sum(!is.na(avg_03)),
    se       = sd / sqrt(n),
    count = n(),
    .groups = 'drop'
  )

# Funniness of serious
bd %>%
  group_by(AS02) %>%
  dplyr::summarise(
    mean     = mean(avg_04, na.rm = TRUE),
    median   = median(avg_04, na.rm = TRUE),
    sd       = sd(avg_04, na.rm = TRUE),
    skew     = psych::skew(avg_04, na.rm = TRUE),
    kurtosis = psych::kurtosi(avg_04, na.rm = TRUE),
    n        = sum(!is.na(avg_04)),
    se       = sd / sqrt(n),
    count = n(),
    .groups = 'drop'
  )  

# Similarity between versions of same image
bd %>%
  group_by(AS02) %>%
  dplyr::summarise(
    mean     = mean(avg_05, na.rm = TRUE),
    median   = median(avg_05, na.rm = TRUE),
    sd       = sd(avg_05, na.rm = TRUE),
    skew     = psych::skew(avg_05, na.rm = TRUE),
    kurtosis = psych::kurtosi(avg_05, na.rm = TRUE),
    n        = sum(!is.na(avg_05)),
    se       = sd / sqrt(n),
    count = n(),
    .groups = 'drop'
  ) 




# Maybe overall perceived funniness (and controvery) of stimuli might be related to that

lm_image<-lm(total_p~+avg_03, data=bd) #this doesnt makes sense for people with total_p < 4 as they didnt saw any images
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

table(bd$SD06)

# Gender
table(bd$SD01) # 1 men, 2 female 3 non binary

#Education
table(bd$SD07) #overwhelming majority of university educated people




#average time of completion
describe(bd$TIME_SUM) 


# DESCRIPTIVE:HUMOR/POLEMIC/SIMILARITY ------------------------------------

# Lets see correlations among overall ratings

avg_cor<-bd[,c("avg_01","avg_02","avg_03","avg_04","avg_05")]  
cor.plot(avg_cor)
cor.test(bd$avg_02, bd$avg_04) # what wea, however this could be due to low variance at low ends of distribution

ggplot(bd, aes(x=avg_02, y=avg_04)) + 
  geom_point()# yeah shouldt read to much into this correlation





## Humorous Stimuli -meme descriptives #####

# Lets create a df of the ratings of funniness
cols_f_meme <- paste0("CK", sprintf("%02d", 8:47), "_03") 
describe(bd[, cols_f_meme])

# Let's check raw frequency. As extreme deviations are expected due to ideological affinity to the content of the memes
freq_f_meme <- bd %>%
  dplyr::select(all_of(cols_f_meme)) %>%
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
  dplyr::select(all_of(cols_f_serio)) %>%
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
  dplyr::select(all_of(cols_ck)) %>%
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
    n        = sum(!is.na(ck_value)) # sample size for each question
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
    n        = sum(!is.na(ck_value)) # sample size for each question
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
  

# 2 Check that perceived funniness of the humorous version is equal to or greater than the average of all the humorous version: Check Descriptive
# &
#Check that the perceived funniness of the serious version is equal to or smaller than the average of all the serious version: Check Descriptive

  #if a expected funny image average is clearly below the average is should be discarded
  #if a expected unfunny image is clearly above the average is should be discarded

# 3 Check that the funny and serious version of the same message are viewed as having a similar message
  # A rating >4 is required to pass

# 4 Check that the images that belong to the same topic are viewed as :expressing political opinions (mean=2 for PI1 TO PI42, and mean=1 FROM PI33 TO PI40) 
  # In order to validate that images are perceived as intended
  # : lower bound of 95% CI has a majority of people rating it as intended
  # : no more than a 15% of  people cross categorizes it (so for a right wing meme no more than 15% of people view it as leftwing)

# For a pair to pass both conditions should pass: For example image 8 should be viewed as expressing a right wing opinion and image 9 a left wing opinion. Both should be viewed as expressing a political opinion

# 5 First check if there are significant differences within pairs in the level of polemicality -> there should be any! 
  # Also select pairs of images near the mean on the level of polemicality. Similar to step 2




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

# Linear models of perceived funniness by use of humorous cues (ingoring interaction with topic)

# Maximal effect model that does not converge
m3<- lmer(funniness ~ version*ck_ + (version | id) + (version | ck_), data = fun_df, REML = TRUE) #Singular
#REML as im going to compare different models with the same fixed effect

# Pair differences in baseline funniness and pair differences in humor effect. Plus slope for humor effects within individual that has more data points
m2 <- lmer(funniness ~ version + (version | id) + (1 | ck_), data = fun_df, REML = T) 
summary(m2, ddf="Kenward-Roger")
# even participants to weak response to version H still see a positive increase: intercept-(1*std versionH)
# im accounting for differences in how individuals rate funniness (highter grouping factor), deleting factor with lower variance (ck_) - Signmann & Kellen, 2020 recommendation

#just intercepts by individual and intercept by image pair
m1 <- lmer(funniness ~ version + (1 | id) + (1 | ck_), data = fun_df, REML = T)
summary(m1, ddf="Kenward-Roger")

#just intercepts by individual 
m0 <- lmer(funniness ~ version + (1 | id) , data = fun_df, REML = TRUE)  
summary(m0)

anova(m0,m1,m2, refit=F) #m2 seems the best option


anova(m2,ddf="Kenward-Roger")
ranova(m2) #to get p values for random effects
# both random effects are quite significant
# HUGE DIFFERENCES BETWEEN Humorous and Serious images, that's good
# Negative correlation means that funnier pairs of images have smaller H-S differences --->BETWEEN CORRELATION


rm(m0,m1,m3)


# lets see the BLUPS
ranef(m2)$ck_ # i dont get slopes for ck_

# Step 1:  Differences of funniness between versions across pairs ----------------------


### Using a less conservative estimate: Benjamini-Hochberg (BH)

# I tis recommended to center levels to -0.5 and +0.5 to interpret the estiamtes. However Im' interested in the simple effects as Im comparing within pairs. So I wont to the centering

# fun_df$version_c <- ifelse(fun_df$version == "H", 0.5, -0.5)

#set contrast to the grand mean not ck_08
ck_levels <- levels(fun_df$ck_) #to avoid contrast() changing the names of ck
ck_contrasts <- contr.sum(length(ck_levels))
dimnames(ck_contrasts) <- list(ck_levels, ck_levels[1:(length(ck_levels) - 1)])
contrasts(fun_df$ck_) <- ck_contrasts #setting contrast to grand mean



# Following previous results I now that the maximal effect structure model will not covnerge. Specially given I'm now going to add interaction between version*ck_


# Intercept + slope for participant. And intercept for ck_ THIS MODEL DOESNT MAKES SENS AS ITEM BASELIEN VARIANCE IS ABSORBED BY FIXED EFFECT
# f_pares3<- lmer(funniness ~  ck_ * version+ (version | id)+(1|ck_),
#                 data = fun_df, REML = TRUE)
# summary(f_pares3, ddf="Kenward-Roger")

# Intercept + slope for participant. No intercept for ck_
f_pares2<- lmer(funniness ~  ck_ * version+ (version | id),
                data = fun_df, REML = TRUE)



# Intercept for participant
f_pares1 <- lmer(funniness ~ ck_ * version + (1 | id),
                 data = fun_df, REML = TRUE)


anova(f_pares1, f_pares2, refit=FALSE) # Chose f_pares2 to do pairwise comparitiosn


anova(f_pares2,ddf="Kenward-Roger")
summary(f_pares2, ddf="Kenward-Roger")
ranova(f_pares2) #great within individual variation in humor effect

rm(f_pares1)


# no centering so beware of interpretation

# ck_ effects : Across the baseline non-humorous version ($S$), pair nn is rated $  $ points higher in funniness than the average image pair
# version_H: For the average image pair, switching from version $S$ to version $H$ increases perceived funniness by $  $ point
# ck_nn::version_c: The humor boost ($H - S$) for pair nn is $ $ points smaller/bigger than the overall average humor effect (resulting in a net increase of $versionH - $CK_NN::versionH value

# Because it is dummy coded
# The effects of ck_ reflect differences specifically within the baseline version ($S$), rather than averaged across both versions.
# The effect of versionH reflects the humor boost for a typical pair (because ck_ is sum-coded to the grand mean).


# this is wordy but i need to do it this way to have the necessary objects for the next section
emm_pares <- emmeans(f_pares2, ~ version | ck_)
con_pares <- contrast(emm_pares, method = "revpairwise")
res_pares <- summary(con_pares, by = NULL, infer = TRUE, adjust = "fdr") #also worth checking when using holm
summary(res_pares)


rm(con_pares, res_pares)



# Step 2: Comparision of average Humorous and Serious Stimuli across Means ----------------------

  # This is a descriptive step of the data validation
  # A meme can be significantly more humorous that the serious version of the same message and still be perceived as unfunny
  # Therefore any meme that is clearly below the mean perception of funniness will be discarded
  # Similarly serious messages that are above the funniness average will also be discarded 


emm_ver <- update(emm_pares, by = "version") #grouping by version instead than pairs of images #comparision of each pair againts overall mean

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




rm(dev_df, emm_ver, fun_check)



  # Given the bias in the sample one should not immediately discard certain cases
  # For example the serious version of ck31 is seen as more fun that the mean of the non serious version.
  # But this particular image is a left wing commentary on right wing hypocrisy so maybe this is an effect of the sample




# Step 3:  Similarity of Message Content ----------------------

similitud <- aggregate(ck_value ~ ck_, 
                       data = subset(base, question == 5), 
                       FUN = function(x) mean(x, na.rm = TRUE))

#All pairs had a mean similarity rating ≥ 4 (on a 1–5 scale), indicating that every pair was judged to express the same content. 
#No further test of this was performed because the effect was unambiguous

#However I can still test whether some pairs are significant different from other ones in similarity

base %>%
  filter(question == 5) %>%
  group_by(ck_) %>%
  summarise(m = mean(ck_value, na.rm = TRUE),
            sd = sd(ck_value, na.rm = TRUE),
            n = sum(!is.na(ck_value)), .groups = "drop") %>%
  summarise(range = max(m) - min(m),
            sd_of_means = sd(m))

# A range of less than a point (0.875) but big given the sacle is 1-5. sd of means is 0.2


# Is there any variation across pairs? Considering the existing nesting
# How much variance is due to participant? Is there an effect worth exploring?

d_sim <- subset(base, question == 5 & !is.na(ck_)) #subset of interest

#lets add both pair and individual intercepts
simil3<- lmer(ck_value ~ 1 + (1 | id) + (1 | ck_),
              data = d_sim, REML=T)

#lets add just mean of pair of images
simil2<-lmer(ck_value ~ 1 + (1 | ck_), data = d_sim, REML = TRUE)  


#lets add just mean of participants
simil1<-lmer(ck_value ~ 1 + (1 | id), data = d_sim, REML = TRUE)

anova(simil1, simil3, refit=FALSE)
anova(simil2, simil3, refit=FALSE)

#simil3 it is ck_ does not influences similairty to a degrese as much as individual variation


summary(simil1, ddf="Kenward-Roger")
ranova(simil3)


rm(simil2,simil3, similitud, d_sim)

# From previous analyses
# There is some differences in how individuals rate similairty, but as a whole memes are being viewed similarly
# Similarity ratings were analysed with a linear mixed model with random intercepts for participant and image pair.
# Participant identity accounted for 39.6% of the variance (SD = 0.51), indicating substantial individual differences in scale use.
# The grand mean rating was 4.54 (SE = 0.07), well within the agreement range, supporting the conclusion that all pairs were perceived as expressing the same content.
# When included into the model image did not significantly accounted for variation in the ratings


# Step 4: Check of message nature and ideological position ----------------

# PI01 to PI40 are checks that the images are perceived as political (2) they should all be 2 to little to no deviation
#now im going to use pi_check
psych::describe(bd[pi_check]) #PI33 to PI4O should be closer to 1, the rest seem to work just fine


# PI41 to PI80 question whether the image is perceived as right wing (1) left wing (2) or neither (3)
#now im going to use pi_ideology

#rather than means i want to use raw frequency as here 


orientacion_stimuli<-do.call(rbind, lapply(bd[pi_ideology], function(x) {
  t <- table(factor(x, levels = 1:3), useNA = "no")
  c(t, prop.table(t) * 100)
}))


orientacion_stimuli <- as.data.frame(orientacion_stimuli)


names(orientacion_stimuli) <- c(
  "n_right", "n_left", "n_neither",   # raw counts
  "%_right",   "%_left",   "%_neither"       # percentages
)

#Interetingly the memes of being broke because of going to a concert are seen as right wing
# Most stimuli seen to be perceived as intended, lets see the CI


# Intended categorization of images
right_wing <- c("PI41_01","PI43_01","PI45_01","PI47_01","PI49_01",
                "PI51_01","PI53_01","PI55_01","PI57_01","PI59_01",
                "PI61_01","PI63_01","PI65_01","PI67_01","PI69_01","PI71_01")

left_wing  <- c("PI42_01","PI44_01","PI46_01","PI48_01","PI50_01",
                "PI52_01","PI54_01","PI56_01","PI58_01","PI60_01",
                "PI62_01","PI64_01","PI66_01","PI68_01","PI70_01","PI72_01")

neither    <- c("PI73_01","PI74_01","PI75_01","PI76_01","PI77_01",
                "PI78_01","PI79_01","PI80_01")


int_map <- bind_rows(
  tibble(stimulus = right_wing, intended = "right"),
  tibble(stimulus = left_wing,  intended = "left"),
  tibble(stimulus = neither,    intended = "neither")
)

names(orientacion_stimuli)
rownames(orientacion_stimuli)

orientacion_stimuli_long <- orientacion_stimuli %>%
  tibble::rownames_to_column("stimulus") %>%
  select(stimulus, n_right, n_left, n_neither) %>%
  mutate(n_total = n_right + n_left + n_neither)

# defining function to test intervals
wilson_ci <- function(x, n, conf = 0.95) {
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom  <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half   <- (z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / denom
  c(lower = center - half, upper = center + half)
}


perception_intended <- orientacion_stimuli_long %>%
  left_join(int_map, by = "stimulus") %>%
  rowwise() %>%
  mutate(
    prop_right   = n_right   / n_total,
    prop_left    = n_left    / n_total,
    prop_neither = n_neither / n_total,
    
    # intended proportion
    prop_intended = case_when(
      intended == "right"   ~ prop_right,
      intended == "left"    ~ prop_left,
      intended == "neither" ~ prop_neither
    ),
    n_intended = case_when(
      intended == "right"   ~ n_right,
      intended == "left"    ~ n_left,
      intended == "neither" ~ n_neither
    ),
    
    # Wilson CI on the intended proportion
    ci = list(wilson_ci(n_intended, n_total)),
    ci_lower = ci["lower"],
    ci_upper = ci["upper"],
    
    # one-sided z-test: H0: p <= 0.5  vs  H1: p > 0.5
    se_intended = sqrt(prop_intended * (1 - prop_intended) / n_total),
    z_one_sided = (prop_intended - 0.50) / se_intended,
    p_one_sided = 1 - pnorm(z_one_sided),
    
    # ---- Misclassification checks (criterion 2) ----
    # right-intended: <= 15% left
    # left-intended:  <= 15% right
    # neither-intended: combined (left+right) <= 30%
    crit2 = case_when(
      intended == "right"   ~ prop_left  <= 0.15,
      intended == "left"    ~ prop_right <= 0.15,
      intended == "neither" ~ (prop_right + prop_left) <= 0.30,
      TRUE ~ NA
    ),
    misclass_value = case_when(
      intended == "right"   ~ prop_left,
      intended == "left"    ~ prop_right,
      intended == "neither" ~ prop_right + prop_left
    ),
    misclass_cap = case_when(
      intended == "right"   ~ 0.15,
      intended == "left"    ~ 0.15,
      intended == "neither" ~ 0.30
    )
  ) %>%
  ungroup()

rm(results)

rm(orientacion_stimuli, int_map, left_wing, right_wing, neither)

# Step 5: Level of offensiveness ------------------------------------------


#Similar to step 1 lets first create a df with ratings of controversy

polemic_df <- base %>%
  filter(question %in% c(1, 2)) %>% #values of 1(controversy of meme version) and 2 (controversy of serious version) in column question
  mutate(
    version   = factor(if_else(question == 1, "H", "S"), levels = c("S", "H")), #this establishes serious as the baseline factor 
    id        = factor(id),
    ck_     = factor(ck_),
    controversial = as.numeric(ck_value)
  ) %>%
  dplyr::select(id, ck_, version, controversial)

polemic_df$ck_  <- as.factor(polemic_df$ck_)


ck_levels <- levels(polemic_df$ck_) #to avoid contrast() changing the names of ck
ck_contrasts <- contr.sum(length(ck_levels))
dimnames(ck_contrasts) <- list(ck_levels, ck_levels[1:(length(ck_levels) - 1)])
contrasts(polemic_df$ck_) <- ck_contrasts #setting contrast to grand mean


# no centering so beware of interpretation

# Intercept + slope for participant. I cant add intercepts for ck_
c_pares2 <- lmer(controversial ~ ck_ * version + (version | id),
                 data = polemic_df, REML=T) # this is a similar structure to the test on funniness, but this models is not as good as c_pares1


# Intercept for participant
c_pares1 <- lmer(controversial ~ ck_ * version + (1 | id),
                 data = polemic_df, REML=T)
#humor version actually increases perceived polemicality


anova(c_pares1, c_pares2, refit=F) #better fit by c_pares1 difference for 2 is significant but look at BIC
# adding a slope by version doesnt really increases model fit compare to just using intercept

rm(c_pares2)


ranova(c_pares1)
summary(c_pares1, ddf="Kenward-Roger")


c_pares_comp <- emmeans(c_pares1, ~ version | ck_) %>%
  contrast(method = "revpairwise") %>%
  summary(by = NULL, infer = TRUE, adjust = "fdr")
summary(c_pares_comp) #not a single difference within pairs among H and S version on perceived polemicality

# This is weird c_pares1 suggest a signficiant effect of humor on perception a topic is controversial, but not a single pairwise comparision shows it
# This means the effect is diffuse and small across stimuli
# This indicates that humor exerts a small, widespread, and uniform increase on perceived polemicality across the battery, rather than being concentrated in specific image pairs.


joint_tests(c_pares1) # how controversy varies across ck_, version and interaction
# there are iamges pairs that are significantly more polemical than others


#Pairs differ in controversy
#On average across pairs, H is rated more controversial than S
#no evidence that the effect of humor on controversy varies across pairs.

# Lets see how each images compares on its mean to the average
emm_c <- emmeans(c_pares1, ~ ck_)   # one model base mean by pair averagedover version 
dev_c <- contrast(emm_c, method = "eff") |>  #mean - unweighted mean of all pairs
  summary(infer = TRUE, adjust = "none") |> # id changes a lot if use "none" or  "fdr" as adjustment if i use "none" (more strict) the rejected pairs are kind of the same that failed previous tests
  as.data.frame() |>
  mutate(ck_id = sub(" effect$", "", contrast),
         status = case_when(lower.CL > 0 ~ "above",  # Fair confidence that the lower bound of the true mean lies above the unweighted average mean
                            upper.CL < 0 ~ "below",
                            TRUE         ~ "not different")) |>
  arrange(abs(estimate))

# look at estimates!! as a pair can be clearly above but just by little ck_22 is above by an estimate of 0,47 and ck_18 is above by 1.14

# As of 13/10 with a majority of leftwing respondents most of the pairs of images above average levels of polemicality  are right wing

# ...and the winners are... -----------------------------------------------

# When I have enough data I will add a summary of the eliminated pairs of images




# Analysis of Cynicism Scales ---------------------------------------------


# AGREE/DISAGREE SCALE
cinis_ad<-bd[,c("VD03_01","VD03_02","VD03_03","VD03_04","VD03_05","VD03_06")]  
cinis_ad[cinis_ad == -1]<-NA

cinis_ad<-cinis_ad[rowSums(is.na(cinis_ad))<ncol(cinis_ad),] #remove cases that were not assigned to this condition-->all NA, should be half the total
cinis_ad<-na.omit(cinis_ad[1:6])

cor(cinis_ad, use = "everything", method = c("spearman"))
cronbach.alpha(cinis_ad, CI=T)

describe(cinis_ad) 

#CFA
cinis_ad[cinis_ad == -1]<-NA
cinis_ad<-na.omit(cinis_ad[1:6])
cinis_ad_cor<-cor(cinis_ad)

str(cinis_ad)

library(lavaan) #i have insufficient data for a proper CFA
mod1<- 'cinismo=~VD03_01+VD03_02+VD03_03+VD03_04+VD03_05+VD03_06'
fit<-cfa(mod1,std.lv=T, estimator ="MLR", data=cinis_ad)
summary(fit, fit.measures=T, standarize=T)


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

 
