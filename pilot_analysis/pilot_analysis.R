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
library(lavaan)
library(semTools)

options(scipen = 999)
emm_options(lmerTest.limit = 15000, pbkrtest.limit = 15000) # to calculate df and have t-based CI for the different analyses, even if exceed N
set.seed(2010) # in case there is any randomization to be doned 



# 1. Data Preparation -----------------------------------------------------


bd <- read_excel("pilot_analysis/pilot_data.xlsx")
bd[bd == -9]<-NA # transform -9 into NA
bd <- bd %>% filter(SD04_01 <= 36) # erase people above accepted age

#There are some cases that need to be deleted as they are not true participants, but rather tests of the coding of the redirecting towards the panel
# Luckikly identiying them is pretty easy
# They satisfy two conditions:
# Variable QUESTNNR = piloto 2
# They were answered before the survey was send to the panel 2026-10-16 23:00:39
# Those cases are CASE = 1661, 1668,1671,1672,1673, 1677

bd <- bd %>%
  filter(!CASE %in% c("1661", "1668", "1671", "1672", "1673", "1677")) 


# Delete people with TIME_RSI >2 Relative Speed Index above 2. Recommended by Leiner(2019): http://dx.doi.org/10.18148/srm/2019.v13i3.7403
bd <- bd %>% filter(TIME_RSI < 2) 


# 1.1 Definitions of columns
  # Metadata columns
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

  # Variables that are cynicism
cinismo <- c("VD03_01","VD03_02","VD03_03","VD03_04","VD03_05","VD03_06")

  # String with columns that are check of whether stimuli are perceived as political or not
pi_check <-paste0("PI", sprintf("%02d",1:40),"_01")
  # String with columns that are check of whether stimuli are perceived as right wing, left wing or neither
pi_ideology <-paste0("PI", sprintf("%02d",41:80), "_01")

  # Intended categorization of images
right_wing <- c("PI41_01","PI43_01","PI45_01","PI47_01","PI49_01",
                "PI51_01","PI53_01","PI55_01","PI57_01","PI59_01",
                "PI61_01","PI63_01","PI65_01","PI67_01","PI69_01","PI71_01")

left_wing  <- c("PI42_01","PI44_01","PI46_01","PI48_01","PI50_01",
                "PI52_01","PI54_01","PI56_01","PI58_01","PI60_01",
                "PI62_01","PI64_01","PI66_01","PI68_01","PI70_01","PI72_01")

neither    <- c("PI73_01","PI74_01","PI75_01","PI76_01","PI77_01",
                "PI78_01","PI79_01","PI80_01")

  # List of variables that measure funniness of leftwing, rightwing, and non political memes

f_rw_meme <- c("CK08_03", "CK10_03", "CK12_03", "CK14_03", "CK16_03", "CK18_03", "CK20_03", "CK22_03", "CK24_03", "CK26_03", "CK28_03", "CK30_03", "CK32_03", "CK34_03", "CK36_03", "CK38_03")

f_lw_meme  <- c("CK09_03", "CK11_03", "CK13_03", "CK15_03", "CK17_03", "CK19_03", "CK21_03", "CK23_03", "CK25_03", "CK27_03", "CK29_03", "CK31_03", "CK33_03", "CK35_03", "CK37_03", "CK39_03")

f_np_meme    <- c("CK40_03","CK41_03","CK42_03","CK43_03","CK44_03","CK45_03","CK46_03","CK47_03")

# 1.2 Creation of New Variables
  # A person that sees the complete questionnaire is expected to see a total of 14 pages. The following code create a variable "total_p" that indicates how  pagesmuch each person sees
  bd <- bd %>%
  mutate(
    total_p = rowSums(!is.na(pick(num_range("TIME", 01:045, width = 3))))
  )

  # Someone that saw less than 6 pages didnt saw a single image, so there is no rating of any single stimuli.
  # RUN ANALYSIS FOR CYNICISM BEFORE THIS !!!!
    # This way there is a little bit more data for the CFA. Some people abandoned the survey before seeing images but responded the cynicism scale
 # bd <- bd %>% filter(total_p >= 6) 
  
  #Lets also create a dichotomic variable that differentiates between people who respondend <50% of images (9 pages in total)
  bd$total_p_d <- ifelse(bd$total_p <= 9, 0, 1)
  
  # Grouping by political orientation
    # I know grouping center/independents and non political together might not be ideal, bu given the info this is the main solution
  bd <- bd %>%
    mutate(
      id_pol_3 = case_when(
        SD06 %in% c(1, 2, 3, 4) ~ 1, #leftwing
        SD06 %in% c(6, 7, 8, 9) ~ 2, #rw
        SD06 %in% c(-1, -2, 5)  ~ 3, #neither
        TRUE ~ NA_real_         # Keeps any other values or original NAs as NA
      )
    )
  
  bd$id_pol_3<-as.factor(bd$id_pol_3)
  
  #  Overall ratings of funniness, polemicality and similarity among images by individual
  # lets create for each individual a column that shows the average score among _01, _02, _03, _04, _5
    # this also provides a quick way to search for responses that seems suspicious, as someone that answers the same in each question will have the same rating on all 5 columns
  suffixes <- c("01", "02", "03", "04", "05")
  
  for (s in suffixes) {
    #1. Generate all column names that end with this suffix (from CK08 to CK47)
    cols_to_avg <- paste0("CK", sprintf("%02d", 8:47), "_",s)
    
    #2. Calculate the row-wise mean and assign it to a new column (e.g., avg_01)
    bd[[paste0("avg_", s)]] <- rowMeans(bd[, cols_to_avg], na.rm = TRUE)
  }
  
  
  # Lets add the average of perceived funniness for leftwing, rightwing and non political memes
  
  bd <- bd %>%
    rowwise() %>%
    mutate(
      mean_right   = mean(c_across(all_of(f_rw_meme)), na.rm = TRUE),
      mean_left    = mean(c_across(all_of(f_lw_meme)), na.rm = TRUE),
      mean_neither = mean(c_across(all_of(f_np_meme)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Lets save Ram
  rm(suffixes,cols_to_avg,s)
  

  
  
# 2. Data Formatting ------------------------------------------------------

  # Given that ratings are nested in pairs of images and individual, MLM is a better way to assess effects of humor than just mean comparisons
  # This means I need to transform data into long format
  selected_vars <- c("SD01", "SD06","AS02","total_p","total_p_d","id_pol_3","mean_right","mean_left","mean_neither", ck_) 
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
    dplyr::select(id, SD01, SD06,id_pol_3,AS02,total_p,total_p_d,ck_, question, ck_value, mean_left, mean_right, mean_neither)
  
  base$SD01<-as.factor(base$SD01) #gender as a factor 1=men
  base$total_p_d<-as.factor(base$total_p_d) #0 is <=9 pages
  base$id_pol_3<-as.factor(base$id_pol_3)  # 1 is leftwing, 2 is rightwing, 3 is neither
  

  
  
# 3. Descriptive: Sample --------------------------------------------------

  # Political Orientation  
  table(bd$SD06) # 1 Very leftwing (lw)  to 9 very rightwing (rw) -1 neither, -2 dont know
  table(bd$id_pol_3)
  round(100 * prop.table(table(bd$id_pol_3)), 2) #as percentages lw= 40,621 rw=24.87% center/none=34.92%
  describe(bd$SD06[bd$SD06 >= 1])  #mean without counting independents 4.45
  
  bd %>%
    filter(SD06 >= 1, SD06 <= 9) %>%
    ggplot(aes(x = factor(SD06))) +
    geom_bar(fill = "steelblue") +
    labs(x = "Identidad Politica", y = "Numero") +
    theme_minimal()
  
  # Gender
  table(bd$SD01)
  round(100 * prop.table(table(bd$SD01)), 2) # men 38.62 female 58.12 nonbinary 1.1
  
  # Education
  table(bd$SD07) #overwhelming majority of university educated people
  round(100 * prop.table(table(bd$SD07)), 2)
  
  #3	Educación Media Incompleta 1.06%
  #4	Educación Media Completa 15.34%
  #5	Instituto Técnico Profesional/Centro de Educación Técnica Incompleta 4.76%
  #7	Educación Superior Universitaria Incompleta 26.98%
  #6	Instituto Técnico Profesional/Centro de Educación Técnica Completa 11.11%
  #8	Educación Superior Universitaria Completa 40.74%
  
  #average time of completion
  describe(bd$TIME_SUM) #528 SECONDS, however there are some cases that just responded the first 4 pages (no images stimuli. so that probably bias this value)
  

  
# 4. Descriptive: Variables of Interest ----------------------------------

  # Lets see how the averages of pereceptions of polemicality, funniness and similarity varies across assignment groups
  # The assignment of images to each set was random, as was the assignment to condition. So any difference between groups is pure chance
  # In README there is description  of which topics (pair of iamges) were presented in each group
  
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
  
  # Lets see overall rating of funniness across ideological groups
  
  f_means_id <- bd %>%
    group_by(SD06) %>%
    summarise(mean_avg_03 = mean(avg_03, na.rm = TRUE)) %>% #average of funniness of memes
    filter(!is.na(SD06))
  
  
  ggplot(f_means_id, aes(x = as.factor(SD06), y = mean_avg_03, fill = as.factor(SD06))) +
    geom_col(show.legend = FALSE) +
    labs(
      title = "Promedio Comicidad en Grupos Político",
      x = "Orientacion Política",
      y = "Comicidad"
    ) +
    theme_minimal()

  # Im pretty sure (but i havent tested) that extremely left wing have significantly lower values of funniness perception on the memes 
  
  # Lets see correlations among overall ratings
  avg_cor<-bd[,c("avg_01","avg_02","avg_03","avg_04","avg_05")]  
  cor.plot(avg_cor)
 
# Dont look to much into the correlatio of funniness of serious images with others. It is probably a funcion of hghet density towards the lower end of scale
  ggplot(bd, aes(x=avg_04, y=avg_02)) + 
    geom_point()
  
  rm(avg_cor, f_means_id)
  

  
# 5. Some Graphs ----------------------------------------------------------

    # 5.1 Funniness: Comparison memes and serious versions -> % responses by value
  
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
    drop_na(ck_value, question) %>%
    filter(question %in% c(3, 4)) %>%
    mutate(question = factor(question, levels = c(3, 4), labels = c("V. Meme", "V. Seria")))
  
  
  ggplot(base_fun, aes(x = factor(ck_value), fill = question, y = after_stat(prop), group = question)) +
    geom_bar(position = "dodge", alpha = 0.85) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "% de Evaluación de comicidad",
      x = "Acuerdo con que estimulo es Cómico",
      y = "%",
      fill = "Condición"
    ) +
    theme_minimal()
  
  # 5.2 Polemicality: Comparison memes and serious versions  -> % responses by value
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
    drop_na(ck_value, question) %>%
    filter(question %in% c(1, 2)) %>%
    mutate(question = factor(question, levels = c(1, 2), labels = c("V. Meme", "V. Seria")))
  
  
  
  ggplot(base_polemic, aes(x = factor(ck_value), fill = question, y = after_stat(prop), group = question)) +
    geom_bar(position = "dodge", alpha = 0.85) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "% de Evaluación de Polémica",
      x = "Acuerdo con que estimulo es Polémico",
      y = "%",
      fill = "Condición"
    ) +
    theme_minimal()
  
  
  # 5.3 Perceived funniness of lw, rw and non political memes by political orientation
  
   base_means <- base %>%
    distinct(id, id_pol_3, mean_left, mean_right, mean_neither)
  #deduplicate long df
  
  # transform into long format:  one row per id × orientation
  base_long <- base_means %>%
    pivot_longer(
      cols = c(mean_right, mean_left, mean_neither),
      names_to = "orientation",
      values_to = "mean_value"
    ) %>%
    mutate(
      orientation = factor(orientation,
                           levels = c("mean_right", "mean_left", "mean_neither"),
                           labels = c("Derecha", "Izquierda", "No Politico")),
      id_pol_3 = factor(id_pol_3, levels = c("1", "2", "3"))
      
    )
  
  #compute mean and 95% CI per id_pol
  summary_df <- base_long %>%
    group_by(id_pol_3, orientation) %>%
    summarise(
      mean_val = mean(mean_value, na.rm = TRUE),
      sd_val   = sd(mean_value,   na.rm = TRUE),
      n        = sum(!is.na(mean_value)),
      se       = sd_val / sqrt(n),
      ci_low   = mean_val - qt(0.975, df = n - 1) * se,
      ci_high  = mean_val + qt(0.975, df = n - 1) * se,
      .groups  = "drop"
    )
  
  # create graph
  
  ggplot(summary_df, aes(x = id_pol_3, y = mean_val, fill = orientation)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  position = position_dodge(width = 0.8),
                  width = 0.2) +
    scale_fill_manual(values = c(
      "Derecha"     = "gold", 
      "Izquierda"   = "#E41A1C",
      "No Politico" = "lightblue"  
    )) +
    labs(
      x = "Orientacion politica",
      y = "Comicidad (1-5)",
      fill = "Orientación",
      title = "Promedio Comicidad (95% CI) según Orientación"
    ) +
    theme_minimal()
  
  
  # 5.4 Differences on overall funniness of means across political orientation
  
  describe(bd$mean_left) #overall mean for lw memes
  describe(bd$mean_right) #overall mean for rw memes
  describe(bd$mean_neither) #overall mean for non political memes
  
  # Perceived Funniness of LW memes
  ggplot(bd, aes(x = mean_left, fill = factor(id_pol_3))) +
    geom_density(alpha = 0.4) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Comicidad Memes de Izquierda de Acuerdo a Orientación",
      x = "Promedio Comicidad (1 - 5)",
      y = "Densidad",
      caption = "Nota: 1-Izquierda, 2-Derecha 3-Centro/Independientes",
      fill = "Orientacion Politica"
    ) +
    theme_minimal()
  
  # Perceived Funniness of RW memes
  ggplot(bd, aes(x = mean_right, fill = factor(id_pol_3))) +
    geom_density(alpha = 0.4) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Comicidad Memes de Derecha de Acuerdo a Orientación",
      x = "Promedio Comicidad (1 - 5)",
      y = "Densidad",
      caption = "Nota: 1-Izquierda, 2-Derecha 3-Centro/Independientes",
      fill = "Orientacion Politica"
    ) +
    theme_minimal()
  
  # Perceived Funniness of Non-Political memes
  ggplot(bd, aes(x = mean_neither, fill = factor(id_pol_3))) +
    geom_density(alpha = 0.4) +
    scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    labs(
      title = "Comicidad Memes de Apoliticos de Acuerdo a Orientación",
      x = "Promedio Comicidad (1 - 5)",
      y = "Densidad",
      caption = "Nota: 1-Izquierda, 2-Derecha 3-Centro/Independientes",
      fill = "Orientacion Politica"
    ) +
    theme_minimal()
  
  
  rm(base_fun, base_polemic,base_means,base_long, summary_df)
  
  
# 6. Criteria for Selection of Stimuli ------------------------------------
# These Criteria are ordered from more important to less important. More important means less flexibility on what might pass a valid stimuli

  # 1  Check significant differences between the meme version and serious version on perceived funniness
  #if non significant, the pair of images on which said image belongs is not considered for further analysis. Also the topic cluster is discarded
  #for example: image 8 and 9 talk about the same topic but from ideologically different positions. Both need to have significant differences between version 
  
  
  # 2 Check that perceived funniness of the humorous version is equal to or greater than the average of all the humorous version: Check Descriptive
  # &
  #Check that the perceived funniness of the serious version is equal to or smaller than the average of all the serious version: Check Descriptive
  
  #if a expected funny image average is clearly below the average is should be discarded
  #if a expected unfunny image is clearly above the average is should be discarded
  
  # 3 Check that the funny and serious version of the same message are viewed as having a similar message
  # A rating >=4 is required to pass
  
  # 4 Check that the images that belong to the same topic are viewed as :expressing political opinions (mean=2 for PI1 TO PI42, and mean=1 FROM PI33 TO PI40) 
  # In order to validate that images are perceived as intended (Strict Criteria)
    # : lower bound of 95% CI has a majority of people rating it as intended
    # : no more than a 15% of  people cross categorizes it (so for a right wing meme no more than 15% of people view it as leftwing)
  # Relaxed Criteria
    # More than 60% people classify it as intended
  
  # For a pair to pass both conditions should pass: For example image 8 should be viewed as expressing a right wing opinion and image 9 a left wing opinion. Both should be viewed as expressing a political opinion
  
  # 5 First check if there are significant differences within pairs in the level of polemicality -> there shouldnt be any! 
  # Also select pairs of images near the mean on the level of polemicality. Similar to step 2
  

# 6.x Step 0 - Modelling of Fixed and Random Effects ----------------------

  # Creating a df for the analyses
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
  
# Linear models of perceived funniness by use of humorous cues (ignoring interaction with topic)
  # Maximal effect model (Not Converges)
    # m3<- lmer(funniness ~ version*ck_ + (version | id) + (version | id:ck_), data = fun_df, REML = TRUE)
  #Singular as well as (funniness ~ version + (version | id) + (version | ck_), data = fun_df, REML = TRUE)
  #REML as im going to compare different models with the same fixed effect
  
  # Pair differences in baseline funniness and pair differences in pair + Plus slope for humor effects 
  # I'm deleting factor with lower variance (ck_) - Signmann & Kellen, 2020 recommendation
  m2 <- lmer(funniness ~ version + (version | id) + (1 | ck_), data = fun_df, REML = T) 
  
  #just intercepts by individual and intercept by image pair
  m1 <- lmer(funniness ~ version + (1 | id) + (1 | ck_), data = fun_df, REML = T)
  
  #just intercepts by individual 
  m0 <- lmer(funniness ~ version + (1 | id) , data = fun_df, REML = TRUE)  
  
  anova(m0,m1,m2, refit=F)  #m2 seems the best option
  
  summary(m2, ddf="Kenward-Roger") # Recommended method for evaluating significance by Luke (2019) : doi: 10.3758/s13428-016-0809-y. 
  anova(m2,ddf="Kenward-Roger")
  ranova(m2)
  
  # lets see the BLUPS
  ranef(m2)$ck_
  
  # Both random effects are quite significant
  # HUGE DIFFERENCES BETWEEN Humorous and Serious images, that's good
  # Negative correlation means that funnier pairs of images have smaller H-S differences --->BETWEEN CORRELATION
  # Even participants to weak response to version H still see a positive increase: intercept-(1*std versionH)
  
  rm(m0,m1)
  
  

# 6.1 Step 1 - Funniness Differences within image pair --------

# Several comparisions (40) will be made, so I'm using a less conservative estimate: Benjamini-Hochberg (BH)
# It is recommended to center levels to -0.5 and +0.5 to interpret the estimates. However I'm interested in the simple effects as I'm comparing within pairs. So I wont be half-centering
  # fun_df$version_c <- ifelse(fun_df$version == "H", 0.5, -0.5)
  
  #set contrast to the grand mean of ck_ not ck_08
  ck_levels <- levels(fun_df$ck_) #to avoid contrast() changing the names of ck
  ck_contrasts <- contr.sum(length(ck_levels)) #contrast against the gran mean across all ck_ (all topics)
  dimnames(ck_contrasts) <- list(ck_levels, ck_levels[1:(length(ck_levels) - 1)])
  contrasts(fun_df$ck_) <- ck_contrasts #setting contrasts in df
  
# Following previous results I now that the maximal effect structure model will not converge. Specially given I'm now going to add interaction between version*ck_
# Also theoretically ck_ makes more sense as a random effect: however in order to conduct the comparision within pairs of images i need to set it as fixed
  
  # Intercept + slope of funniness for participant. + Intercept of pair of images by individual
  f_pares3<- lmer(funniness ~  ck_ * version+ (version | id)+(1|id:ck_),
                  data = fun_df, REML = TRUE)
  
  
    # Intercept + slope for participant. No intercept for ck_
  f_pares2<- lmer(funniness ~  ck_ * version+ (version | id),
                  data = fun_df, REML = TRUE)
  
  
  # Intercept for participant
  f_pares1 <- lmer(funniness ~ ck_ * version + (1 | id),
                   data = fun_df, REML = TRUE)
  
    anova(f_pares1, f_pares2, f_pares3, refit=FALSE) # Chose f_pares3 to do pairwise comparitiosn

summary(f_pares3, ddf="Kenward-Roger") 
anova(f_pares3,ddf="Kenward-Roger")
ranova(f_pares3) # great within individual variation in humor effect

# Notes on the interpretation

# ck_ effects : Across the baseline non-humorous version ($S$), pair nn is rated $  $ points higher in funniness than the average image pair
# version_H: For the average image pair, switching from version $S$ to version $H$ increases perceived funniness by 1.07 points
# ck_nn::version_c: The humor boost ($H - S$) for pair nn is $ $ points smaller/bigger than the overall average humor effect (resulting in a net increase of $versionH - $CK_NN::versionH value

# Because it is dummy coded
  # The effects of ck_ reflect differences specifically within the baseline version ($S$), rather than averaged across both versions.
  # The effect of versionH reflects the humor boost for a typical pair (because ck_ is sum-coded to the grand mean)

# Checking variation withih pairs of images (versionH vrs versionS for each ck_level-topic)
emm_pares <- emmeans(f_pares3, ~ version | ck_)
con_pares <- contrast(emm_pares, method = "revpairwise")
res_pares <- summary(con_pares, by = NULL, infer = TRUE, adjust = "fdr") # all are significant differences using holm doesnt change that
res_pares # across all pairs the humorous version is perceived as funnier than the serious one 

rm(f_pares1, f_pares2, con_pares, res_pares)



# 6.2 Step 2 - H/S Comparison across Mean ---------------------------------
  # This is a descriptive step of the data validation
  # A meme can be significantly more humorous that the serious version of the same message and still be perceived as unfunnycompared with the mean
  # Therefore any meme that is clearly below the mean perception of funniness will be discarded
  # Similarly serious messages that are clearly above the funniness average will also be discarded 

#grouping by version instead than pairs of images 
emm_ver <- update(emm_pares, by = "version")

dev_df <- contrast(emm_ver, method = "eff") |>
  summary(infer = TRUE, adjust = "none") |>   # no adjustment: descriptive, not a formal test
  as.data.frame() |>
  mutate(ck_id = sub(" effect$", "", contrast)) |>
  dplyr::select(ck_id, version, dev = estimate, SE, lower.CL, upper.CL)
# in order to run this code make sure to run pbkrtest.limit = 15000
# if not, it not going to calculate t distribution intervarls and get no lower and upper.cl

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

# ck44, ck45, ck45 serious versions are clearly above means, but so are the h version. So they don't fail this criteria
# Also given the bias in the sample one should not immediately discard certain cases: That's the role of the "ambiguous" tag

rm(dev_df, emm_ver, emm_pares)

# 6.3 Step 3 - Similarity of Message Content ------------------------------

similitud <- aggregate(ck_value ~ ck_, 
                       data = subset(base, question == 5), 
                       FUN = function(x) mean(x, na.rm = TRUE))

#All pairs (except CK43-->NON POLITICAL) had a mean similarity rating ≥ 4 (on a 1–5 scale), indicating that every pair was judged to express the same content.

# However I can still test whether some pairs are significant different from other ones in similarity

base %>%
  filter(question == 5) %>%
  group_by(ck_) %>%
  summarise(m = mean(ck_value, na.rm = TRUE),
            sd = sd(ck_value, na.rm = TRUE),
            n = sum(!is.na(ck_value)), .groups = "drop") %>%
  summarise(range = max(m) - min(m),
            sd_of_means = sd(m))

# A range of less than a point (0.644) but big given the scale is 1-5. sd of means is 0.144

# How much variance is due to participant? Is there an effect worth exploring?
d_sim <- subset(base, question == 5 & !is.na(ck_)) #subset of interest
# rather than comparing to left wing let each coefficient be the deviation of the grand mean
contrasts(d_sim$id_pol_3) <- contr.sum(nlevels(d_sim$id_pol_3))

# Models including random participant intercept + random item  intercept
simil1<-lmer(ck_value ~ 1 + (1 | id), data = d_sim, REML = TRUE)
simil2<-lmer(ck_value ~ 1 + (1 | id)+(1|ck_), data = d_sim, REML = TRUE)

anova(simil2, simil1, refit=F) # Just simil1

summary(simil1, ddf="Kenward-Roger")
ranova(simil1)

# Great variance across individual regarding how similar are pairs of images rated: Individual differences account for 47.95% of the total variance
# The grand mean rating was 4.28 (SE = 0.05), well within the agreement range, supporting the conclusion that all pairs were perceived as expressing the same content.
# When included into the model image did not significantly accounted for variation in the ratings




# The following is just exploratory, I was an idea I had out of nowhere. I don't have any theory to back this doubt, I just wanted to explore it
# Therefore It might be just a spurius effect. DOnt look much into it
  # Does political orientation influences the degree of perceived similarity?
simil_id<-lmer(ck_value ~ id_pol_3 + (1 | id), data = d_sim, REML = TRUE)
summary(simil_id) 

# How effects vary across groups?
simil_id_all <- emmeans(simil_id, ~ id_pol_3) 
contrast(simil_id_all, method = "eff", adjust = "none") |>
  summary(infer = TRUE) #Infer uses Kenward-Roger adjusments, it changes If i set it to  contrast(simil_id_all, method = "eff") 

#Comparison between groups
pairs(simil_id_all, adjust = "tukey") 
# For some reason centrist and independents find less similarity across pairs images

rm(simil2, simil_id, simil_id_all, similitud, d_sim)

# 6.4 Step 4 - Image classification ---------------------------------------

  # 6.4.1 Are political memes perceived as political? Are non-political memes eprceived as non-political?
  # PI01 to PI40 are checks that the images are perceived as political --> That is what pi_check is for
   # 1= Non Political 2= Political
psych::describe(bd[pi_check])

# Two non political memes are perceived as political, lets see the % of rating: Memes about spending money in concerts
round(100 * prop.table(table(bd$PI35_01)), 2) # Majority selected 2!
round(100 * prop.table(table(bd$PI36_01)), 2) # # Majority selected 2!

  # 6.4.2 Are memes classified as being the political orientation they were intended to?
  # # PI41 to PI80 question whether the image is perceived as right wing (1) left wing (2) or neither (3) --> That is what pi_ideology is for

orientacion_stimuli<-do.call(rbind, lapply(bd[pi_ideology], function(x) {
  t <- table(factor(x, levels = 1:3), useNA = "no")
  c(t, prop.table(t) * 100)
}))

orientacion_stimuli <- as.data.frame(orientacion_stimuli)

names(orientacion_stimuli) <- c(
  "n_right", "n_left", "n_neither",   # raw counts
  "%_right",   "%_left",   "%_neither"       # percentages
)

int_map <- bind_rows(
  tibble(stimulus = right_wing, intended = "right"),
  tibble(stimulus = left_wing,  intended = "left"),
  tibble(stimulus = neither,    intended = "neither")
)


orientacion_stimuli_long <- orientacion_stimuli %>%
  tibble::rownames_to_column("stimulus") %>%
  dplyr::select(stimulus, n_right, n_left, n_neither) %>%
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
    
    # ---- Misclassification checks (criterion 2) 
    # right-intended: <= 15% left
    # left-intended:  <= 15% right
    # neither-intended: combined (left+right) <= 30%
    crit2 = case_when(
      intended == "right"   ~ prop_left  <= 0.15,#if i add 0.155 some memes are not discarded due to misclassification relax that criteria if needed
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


# This is an strict test 
  # Based on this criteria basically no pair of images works, apart from the non-political
  # Therefore, I have to take a decision

# Honestly, I think the wording of these items was not the best, and it might have influenced the results
  # " Las imágenes expresan una opinión típica del sector político" / "Images express a typical opinion from the following political sector"
  # It wordy, and kind of confusing. 32 years on this earth and I havent been able to solve this problem of mine

# However the overall tendency is that memes were perceived as expected: In the sense that left wing memes were in general funnier for lw, and rw memes for rw
  # This is a good sign
  # The high levels of the cross classifications of some memes might be a sign that  some people understand the question as "Who is this meme talking about"
  # And classification as "neither" might mean that memes critical of a lw or rw position are not viewed as unique to the opposing side: I can critique the lw while not being rw


#Considering that maybe I have to use a more relaxed criteria:
  #Intended correct categorization is above 60% ignoring cross categorization

perception_intended <- orientacion_stimuli_long %>%
  left_join(int_map, by = "stimulus") %>%
  mutate(
    prop_right   = n_right   / n_total,
    prop_left    = n_left    / n_total,
    prop_neither = n_neither / n_total,
    
    n_intended = case_when(
      intended == "right"   ~ n_right,
      intended == "left"    ~ n_left,
      intended == "neither" ~ n_neither
    ),
    prop_intended = n_intended / n_total,
    
    # Wilson CI on the intended proportion
    ci        = map2(n_intended, n_total, wilson_ci),
    ci_lower  = map_dbl(ci, "lower"),
    ci_upper  = map_dbl(ci, "upper"),
    
    # One-sided z-test: H0: p <= 0.5  vs  H1: p > 0.5
    se_intended = sqrt(prop_intended * (1 - prop_intended) / n_total),
    z_one_sided = (prop_intended - 0.50) / se_intended,
    p_one_sided = pnorm(z_one_sided, lower.tail = FALSE),
    
    passes = prop_intended > 0.60
  ) 


rm(orientacion_stimuli,orientacion_stimuli_long, int_map, left_wing, right_wing, neither, perception_intended) # RAM is a rare possession


# 6.5 Step 5 - Level of Polemicality --------------------------------------

# Similar to step 1 lets first create a df with ratings of controversy
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
contrasts(polemic_df$ck_) <- ck_contrasts 


#Model with intercept for _ck by individual + slope and intercept of version by individual
c_pares3 <- lmer(controversial ~ ck_ * version + (version | id) + (1|id:ck_),
                 data = polemic_df, REML=T)

# Intercept + slope for participant. without intercepts for ck_
c_pares2 <- lmer(controversial ~ ck_ * version + (version | id),
                 data = polemic_df, REML=T) 

# Intercept for participant
c_pares1 <- lmer(controversial ~ ck_ * version + (1 | id),
                 data = polemic_df, REML=T)

anova(c_pares1, c_pares2, c_pares3, refit=F) # 3 wins once again


summary(c_pares3, ddf="Kenward-Roger")
anova(c_pares)
ranova(c_pares3)
# Humor increases perception of polemicality but really slightly 0.10
# Some topics are quite more polemic (more than half a point) LOOK AT CK38: right critizing values of the left


c_pares_comp <- emmeans(c_pares3, ~ version | ck_) %>%
  contrast(method = "revpairwise") %>%
  summary(by = NULL, infer = TRUE, adjust = "fdr")
summary(c_pares_comp) # No significant differences between H-S version in polemicality


# This is weird c_pares3 suggest a signficiant effect of humor on perception a topic is controversial, but not a single pairwise comparision shows it
# This means the effect is diffuse and small across stimuli
# This indicates that humor exerts a small, widespread, and uniform increase on perceived polemicality across the battery, rather than being concentrated in specific image pairs.


# Lets see how each images compares on its mean to the average
emm_c <- emmeans(c_pares3, ~ ck_)   # one model base mean by pair averagedover version 
dev_c <- contrast(emm_c, method = "eff") |>  #mean - unweighted mean of all pairs
  summary(infer = TRUE, adjust = "none") |> # id changes a lot if use "none" or  "fdr" as adjustment if i use "none" (more strict) the rejected pairs are kind of the same that failed previous tests
  as.data.frame() |>
  mutate(ck_id = sub(" effect$", "", contrast),
         status = case_when(lower.CL > 0 ~ "above",  # Fair confidence that the lower bound of the true mean lies above the unweighted average mean
                            upper.CL < 0 ~ "below",
                            TRUE         ~ "not different")) |>
  arrange(abs(estimate))

# This table (dev_c) helps identifying pairs of images where polemicality is above the average  
  # This allows me to control whether I show more or less controversial memes
  # However I should treat this with caution given winner's curse

# Not look just into the status. A pair can be clearly above just by little as in CK13 or by much as in ck18

# Given the majority of LW respondents most of the pairs of images above average levels of polemicality are right wing



# For this particular test it makes sense to use half-coding. As I want to compare the polemicality to the grand mean, not just to S as a baseline
  # Use this df to select images pairs! It makes more sense to use half-coding for this step


polemic_df$version_c <- ifelse(fun_df$version == "H", 0.5, -0.5)

c_pares3_half <- lmer(controversial ~ ck_ * version_c + (version_c | id) + (1|id:ck_),
                      data = polemic_df, REML=T)
summary(c_pares3_half, ddf="Kenward-Roger")


emm_c_half <- emmeans(c_pares3_half, ~ ck_) 

dev_c_half <- contrast(emm_c_half, method = "eff") |>  #mean - unweighted mean of all pairs
  summary(infer = TRUE, adjust = "none") |> # id changes a lot if use "none" or  "fdr" as adjustment if i use "none" (more strict) the rejected pairs are kind of the same that failed previous tests
  as.data.frame() |>
  mutate(ck_id = sub(" effect$", "", contrast),
         status = case_when(lower.CL > 0 ~ "above",  # Fair confidence that the lower bound of the true mean lies above the unweighted average mean
                            upper.CL < 0 ~ "below",
                            TRUE         ~ "not different")) |>
  arrange(abs(estimate))



rm(c_pares1, c_pares2, polemic_df,c_pares_comp, emm_c, emm_c_half, c_pares)



# 6.6 ... and the winners are!!!! -----------------------------------------
  # Using a relaxed criteria in Step 4
    # CK10 and CK11 -> Identity Control Policies  (group 4)
    # CK24 and CK25 -> Protest  (group 3)
    # CK26 and CK27 -> Spending Cuts (group 1)
    # CK28 and CK29 -> Register of Incivilities  (group 4)
  # and all non political except the ones regarding Whatsapp audios
    # doubts regarding the concerts ones





# 7. Analysis of Political Cynicism Scales ------------------------------------------

# Originally I wanted to test two differente phrasing of the same cynicism scale based on Patty et al. (2012): https://journals.sagepub.com/doi/10.1002/per.858
  # Lothamer (2025) build the same scale but worded in terms of frequency rather than agreement/disagreement: https://uh-ir.tdl.org/items/53028c50-16f3-41e0-8f06-0273b728a718

# So I wanted to test whether both worked, and which one created more normal distributions
  # The first 80 answers of the pilot are equally distributed between both scales (AS01=1 view frequency AS01=2 view a/d)
  # However it was pretty soon clear that the Lothamer scale was not working
  # Based on that I decided that the remaining responses were just on the a/d scale, so I have enough data to conduct a CFA


# 7.1 AGREE/DISAGREE SCALE - Cronbach
cinis_ad<-bd[(cinismo)] 
cinis_ad<-cinis_ad[rowSums(is.na(cinis_ad))<ncol(cinis_ad),] #remove cases that were not assigned to this condition
cinis_ad[cinis_ad == -1]<-NA # -1 is actually "I prefer not to answer", but let's treat it as NA

cinis_ad <- cinis_ad %>%
  filter(!if_all(1:6, is.na)) #delete cases were all the columns are NA

cor(cinis_ad, use = "pairwise.complete.obs", method = c("spearman"))

#sometimes ltm doesnt properly charges, so charge it before running this 
cronbach.alpha(cinis_ad,na.rm = TRUE, CI=T,B = 10000) #0,79 cronbach

# 7.2 CFA
mod1<- 'cinismo=~VD03_01+VD03_02+VD03_03+VD03_04+VD03_05+VD03_06'
fit<-cfa(mod1,std.lv=T, estimator ="MLR", data=cinis_ad)
summary(fit, fit.measures=T, standardize=TRUE)
#kind of a big RMSEA, other indicators are good

modindices(fit, sort.=T) #huge dependencies between VD03_01 and 04


# Residual dependencies between items 1 and 4 (manipulation by politicians) and _05 and _06 (immorality) make sense
  # item 1: People are very frequently manipulated by politicians <-> item 4:Our political leaders are prepared to lie to us whenever it suits their purposes
  # item 5: No man can hope to stay honest once he enters politics <-> item 6: Almost all politicians will sell out their ideals or break their promises if it will increase their power
  # There is evidence that political cynicism can also be divided into several first order dimensions (this was actually a theoretical headache when reading literature on the topic), so I will create a new model

# Model with dependencies
mod1_adjust <- '
  cinismo =~ VD03_01 + VD03_02 + VD03_03 + VD03_04 + VD03_05 + VD03_06
  VD03_01 ~~ VD03_04
  VD03_05 ~~ VD03_06'
fit_adjust<-cfa(mod1_adjust,std.lv=T, estimator ="MLR", data=cinis_ad)
summary(fit_adjust, fit.measures=T, standardize=TRUE)

  # Lets check configural invariance by ideological group (Number of factors and item loadings)
config_adjust <- cfa(mod1_adjust, data=bd, std.lv= T, estimator = "MLR",missing="FIML", group="id_pol_3")
config_adjust
summary(config_adjust)
fitMeasures(config_adjust, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))

  #  Weak Invariance Metric --> FACTOR LOADINGS EQUAL ACROOS GROUPS?
weak <- cfa(mod1_adjust,data=bd,std.lv= T, estimator = "MLR", group="id_pol_3", group.equal="loadings")
weak
summary(weak)
fitMeasures(weak, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))

  # # Strong invariance (intercepts) - Scalar Invariance NOPE
strong <- cfa(mod1_adjust,data = bd, std.lv = TRUE, estimator = "MLR", missing = "FIML", group = "id_pol_3", group.equal = c("loadings", "intercepts"))
fitMeasures(strong, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
# NO SCALAR INVARIANCE This means there even when cinismo levels are similar, some items are rated more high by some groups than other

# lets compare weak and configural

compare_cfa<-compareFit(config_adjust, weak)
summary(compare_cfa)

# Lets see how cinismo differs across groups
bd <- bd %>%
  mutate(cinismo_mean = rowMeans(bd[, cinismo], na.rm = TRUE))

group_summary <- bd %>%
  filter(!is.na(id_pol_3), !is.na(cinismo_mean)) %>%
  group_by(id_pol_3) %>%
  summarise(
    n = n(),
    mean_cinismo = mean(cinismo_mean, na.rm = TRUE),
    sd_cinismo = sd(cinismo_mean, na.rm = TRUE),
    se = sd_cinismo / sqrt(n),
    ci_lower = mean_cinismo - qt(0.975, df = n - 1) * se,
    ci_upper = mean_cinismo + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )


# Create Bar Plot with 95% CI Error Bars
ggplot(group_summary, aes(x = factor(id_pol_3), y = mean_cinismo, fill = factor(id_pol_3))) +
  geom_col(width = 0.6, alpha = 0.85, color = "black") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, linewidth = 0.8) +
  scale_fill_manual(values = c(
    "2"     = "gold", # or "yellow" / "#E69F00"
    "1"   = "#E41A1C", # or "red"
    "3" = "lightblue"  # or "purple"
  )) +
  labs(
    title = "Cinismo Promedio entre Orientación Política",
    subtitle = "IC 95% ",
    x = "Orientación Política",
    y = "Promedio Cinismo",
    caption = "Nota: Intercepto de Items no muestra invarianza entre grupos"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

# However this values are biased due to non-strong invariance across groups
# therefore means should not be calculated as unweighted 


rm(fit, compare_cfa, group_summary,fit, fit_adjust, strong, weak)

######################### FREQUENCY SCALE 
# 7.3 This scale was discarded after the first 45 measurements as it had EXTREMELY POOR CRONBACH


cinis_fr<-bd[,c("VD05_01","VD06_01","VD07_01","VD08_01","VD09_01","VD10_01")] #frequency VD09 is inverted
cinis_fr<-cinis_fr[rowSums(is.na(cinis_fr))<ncol(cinis_fr),]


describe(cinis_fr) # check descriptives before inverting to check everything is alright


cinis_fr<-cinis_fr%>%
  mutate(VD09_01=6-VD09_01)


cor(cinis_fr, use = "everything", method = c("spearman")) #WHAT


cronbach.alpha(cinis_fr, CI=T) 

