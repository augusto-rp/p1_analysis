
# testing the difference

#Github ready

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
library(purrr)
library(tibble)
library(lavaan)
library(semTools)


# ltm attaches MASS after dplyr so i use dplyr::select when needed

options(scipen = 999)
emm_options(lmerTest.limit = 15000, pbkrtest.limit = 15000) # to calculate df and have t-based CI for the different analyses, even if exceed N
set.seed(2010) # in case there is any randomization to be doned 



# 1. Data Preparation -----------------------------------------------------

#bd <- read_excel(file.choose())
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

  # Definition of variables related to Stimuli Ratings (Funniness, Polemicality and Similarity )
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


  # List of variables that measure polemicality of lw, rw, and np

p_rw_meme <- c("CK08_01", "CK10_01", "CK12_01", "CK14_01", "CK16_01", "CK18_01", "CK20_01", "CK22_01", "CK24_01", "CK26_01", "CK28_01", "CK30_01", "CK32_01", "CK34_01", "CK36_01", "CK38_01")

p_lw_meme  <- c("CK09_01", "CK11_01", "CK13_01", "CK15_01", "CK17_01", "CK19_01", "CK21_01", "CK23_01", "CK25_01", "CK27_01", "CK29_01", "CK31_01", "CK33_01", "CK35_01", "CK37_01", "CK39_01")

p_np_meme    <- c("CK40_01","CK41_01","CK42_01","CK43_01","CK44_01","CK45_01","CK46_01","CK47_01")

# 1.2 Creation of New Variables
  # A person that sees the complete questionnaire is expected to see a total of 14 pages. The following code create a variable "total_p" that indicates how  pagesmuch each person sees
  bd <- bd %>%
  mutate(
    total_p = rowSums(!is.na(pick(num_range("TIME", 01:045, width = 3))))
  )


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
  
  bd$AS02<-as.factor(bd$AS02)
  
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
  
  
  
  # Lets check if there is any significane differene across any mean
  manova_means<-manova(cbind(avg_01, avg_02, avg_03, avg_04, avg_05)~AS02, data=bd)
  summary(manova_means) ###yay

  
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
  cor.matrix(avg_cor, method="spearman", alpha=0.05, p.adj = "bonferroni", print="all")
  cor.plot(avg_cor)
  

 
# Dont look to much into the correlatio of funniness of serious images with others. It is probably a funcion of hghet density towards the lower end of scale
  ggplot(bd, aes(x=avg_04, y=avg_02)) + 
    geom_point()
  

  
  
  
  
  rm(avg_cor, f_means_id, avg_cor)
  
  
  

  
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
      title = "Promedio Comicidad (95% CI) según Orientación",
      caption = "Nota: 1-Izquierda, 2-Derecha 3-Centro/Independientes"
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
  
  
  # 5.5 Perceived polemicality of lw, rw and non political memes by political orientation
  
  
  bd <- bd %>%
    rowwise() %>%
    mutate(
      p_right   = mean(c_across(all_of(p_rw_meme)), na.rm = TRUE),
      p_left    = mean(c_across(all_of(p_lw_meme)), na.rm = TRUE),
      p_neither = mean(c_across(all_of(p_np_meme)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  
  base_polemic <- bd %>%
    distinct(CASE, id_pol_3, p_left, p_right, p_neither)
  #deduplicate long df
  
  # transform into long format:  one row per id × orientation
  base_long <- base_polemic %>%
    pivot_longer(
      cols = c(p_right, p_left, p_neither),
      names_to = "orientation",
      values_to = "mean_value"
    ) %>%
    mutate(
      orientation = factor(orientation,
                           levels = c("p_right", "p_left", "p_neither"),
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
      y = "Polemicidad (1-5)",
      fill = "Orientación",
      title = "Promedio Polemicidad (95% CI) según Orientación",
      caption = "Nota: 1-Izquierda, 2-Derecha 3-Centro/Independientes"
    ) +
    theme_minimal()
  
  
  
  
# 6. Criteria for Selection of Stimuli ------------------------------------
# These Criteria are ordered from more important to less important. More important means less flexibility on what might pass a valid stimuli

  # 1  Check significant differences between the meme version and serious version on perceived funniness
  #if non significant, the pair of images on which said image belongs is not considered for further analysis. Also the topic cluster is discarded
  #for example: image 8 and 9 talk about the same topic but from ideologically different positions. Both need to have significant differences between version. If 9 fails, so do 8 
  
  
  # 2 Check that perceived funniness of the humorous version is equal to or greater than the average of all the humorous version: Check Descriptive
  # &
  #Check that the perceived funniness of the serious version is equal to or smaller than the average of all the serious version: Check Descriptive
  
  #if a expected funny image average is clearly below the average is should be discarded
  #if a expected unfunny image is clearly above the average is should be discarded
  
  # 3 Check that the pairs of images are oerceived as having similar message (C##_05)
    # A rating >=4 is required to pass
  
  # 4 Check that the images that belong to the same topic are viewed as :expressing political opinions (mean=2 for PI1 TO PI42, and mean=1 FROM PI33 TO PI40) 
  # In order to validate that images are perceived as intended (Strict Criteria)
    # : lower bound of 95% CI has a majority (>50%) of people rating it as intended
    # : no more than a 15% of  people cross categorizes it (so for a right wing meme no more than 15% of people view it as leftwing)
  # Relaxed Criteria
    # More than 60% people classify it as intended
  
  # For a pair to pass both conditions should pass: For example image 8 should be viewed as expressing a right wing opinion and image 9 a left wing opinion. Both should be viewed as expressing a political opinion
  
  # 5 Check if
    # there a clear H-S difference in polemicality?
    # the pair's mean polemicality is clearly above or belowthe mean of the other pairs?

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
  m2 <- lmer(funniness ~ version + (version | id) + (1 | ck_), data = fun_df, REML = T)  #Check if REML IS APPROPIATRE GIVEN ORDINAL DATA, kind of as funninness is composed of several measures
  
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
  # A meme can be significantly more humorous that the serious version of the same message and still be perceived as unfunny compared with the mean
  # Therefore any meme that is clearly below the mean perception of funniness will be discarded
  # Similarly serious messages that are clearly above the funniness average will also be discarded 

#grouping by version instead than pairs of images 
emm_ver <- update(emm_pares, by = "version")

# The comparison has to be made within political or non political memes. If all is together the mean of comparison for political memes will include values from non -political ones (which is higher)
# see last section of code
# I should have the same consideration in step 5

lv       <- levels(fun_df$ck_)
stopifnot(identical(lv, as.character(emm_ver@levels$ck_)))
dim_vec  <- ifelse(parse_number(lv) <= 39, "political", "non-political")
stopifnot(!anyNA(dim_vec))

eff_within <- lapply(seq_along(lv), function(i) {
  same   <- dim_vec == dim_vec[i]
  others <- same & (seq_along(lv) != i)   # exclude the cell being judged
  w <- rep(0, length(lv))
  w[others] <- -1 / sum(others)           # negative weights on the OTHER cells only
  w[i]      <- 1                          # +1 on the cell itself
  w
})

names(eff_within) <- lv   # one contrast per ck_ level


dev_df <- contrast(emm_ver, method = eff_within) |>
  summary(infer = TRUE, adjust = "none") |>
  as.data.frame() |>
  mutate(
    ck_id     = as.character(contrast),
    dimension = ifelse(parse_number(ck_id) <= 39, "political", "non-political")
  ) |>
  dplyr::select(ck_id, dimension, version, dev = estimate, SE, lower.CL, upper.CL)

dev_df <- dev_df |>
  mutate(
    # 95% CI (make explicit; assumes emmeans default level = 0.95)
    ci_level = 0.95,
    
    # distance of the CI bound from zero, in SE units
    # (how many SEs is the bound away from 0?)
    z_upper = upper.CL / SE,
    z_lower = lower.CL / SE,
    
    status = case_when(
      version == "H" & upper.CL < 0 ~ "fail",
      version == "H" & lower.CL > 0 ~ "above_H_fun",
      version == "S" & upper.CL < 0 ~ "below_S_fun",
      version == "S" & lower.CL > 0 ~ "above_S_fun",
      TRUE ~ "inconclusive" # Ci contains 0
    ),
    
    # flag borderline cases where the CI barely excludes 0
    # e.g. bound within 0.5 SE of zero, or |bound| < some margin
    borderline = (version == "H" & upper.CL < 0 & (abs(upper.CL) / SE) < 0.5) |
      (version == "S" & upper.CL < 0 & (abs(upper.CL) / SE) < 0.5)
  )



rm(dim_vec , emm_ver, emm_pares, eff_within, lv)

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

  # 6.4.1 Are political memes perceived as political? Are non-political memes perceived as non-political?
  # PI01 to PI40 are checks that the images are perceived as political --> That is what pi_check is for
   # 1= Non Political 2= Political
psych::describe(bd[pi_check])

# At first glance it seems all political images pass this test, and two non political are failing.

# Let's see the proportion fo participants that selected 2 in each images

pi_props <- lapply(bd[pi_check], function(x) {
  round(100 * prop.table(table(x)), 2)
})

names(pi_props) <- pi_check # name the list elements for clarity

pi_summary <- map_df(pi_props, ~ {
  tibble(
    political_pct = ifelse("2" %in% names(.), .[["2"]], 0),
    non_political_pct = ifelse("1" %in% names(.), .[["1"]], 0)
  )
}) %>%
  mutate(stimulus = pi_check)

rm(pi_props)


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
  mutate(passes_all = (ci_lower > 0.50) & crit2)%>%
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
anova(c_pares3, ddf="Kenward-Roger")
ranova(c_pares3, ddf="Kenward-Roger")
# Humor increases perception of polemicality but really slightly 0.10
# Some topics are quite more polemic (more than half a point) LOOK AT CK38: right critizing values of the left


c_pares_comp <- emmeans(c_pares3, ~ version | ck_) %>%
  contrast(method = "revpairwise") %>%
  summary(by = NULL, infer = TRUE, adjust = "fdr")
summary(c_pares_comp) # No significant differences between H-S version in polemicality


# This is weird c_pares3 suggest a signficant effect of humor on perception a topic is controversial, but not a single pairwise comparision shows it
# This means the effect is diffuse and small across stimuli
# This indicates that humor exerts a small, widespread, and uniform increase on perceived polemicality across the battery, rather than being concentrated in specific image pairs.


# Lets compare the mean of each image to the overall mean per topic

c_pares_comp <- emmeans(c_pares3, ~ version | ck_)
emm_ver_polemic <- update(c_pares_comp, by = "version")


lv_polemic    <- levels(polemic_df$ck_)
stopifnot(identical(lv_polemic, as.character(emm_ver_polemic@levels$ck_)))  # weights must follow the grid order
dim_vec_polemic <- ifelse(parse_number(lv_polemic) <= 39, "political", "non-political") #over 39 are not political


eff_within_polemic <- lapply(seq_along(lv_polemic), function(i) {
  same   <- dim_vec_polemic == dim_vec_polemic[i]
  others <- same & (seq_along(lv_polemic) != i)
  w <- rep(0, length(lv_polemic))
  w[others] <- -1 / sum(others)
  w[i]      <- 1
  w
})


names(eff_within_polemic) <- lv_polemic   # one contrast per ck_ level


dev_df_polemic <- contrast(emm_ver_polemic, method = eff_within_polemic) |>
  summary(infer = TRUE, adjust = "none") |>
  as.data.frame() |>
  mutate(
    ck_id     = as.character(contrast),
    dimension = ifelse(parse_number(ck_id) <= 39, "political", "non-political")
  ) |>
  dplyr::select(ck_id, dimension, version, dev = estimate, SE, lower.CL, upper.CL)

dev_df_polemic <- dev_df_polemic |>
  mutate(
    ci_level = 0.95,
    status = case_when(
      lower.CL > 0 ~ "above_topic",
      upper.CL < 0 ~ "below_topic",
      TRUE         ~ "inconclusive"
    ),
    borderline = (upper.CL < 0 & abs(upper.CL) / SE < 0.5) |
      (lower.CL > 0 & abs(lower.CL) / SE < 0.5)
  )



# This table (dev_c) helps identifying pairs of images where polemicality is above the average  
  # This allows me to control whether I show more or less controversial memes
  # However I should treat this with caution given winner's curse

# Not look just into the status. A pair can be clearly above just by little as in CK13 or by much as in ck18

# Given the majority of LW respondents most of the pairs of images above average levels of polemicality are right wing



# For this particular test it makes sense to use half-coding. As I want to compare the polemicality to the grand mean, not just to S as a baseline
  # Is the same model, just with different paramets. But results will mainly be the same
  # Im just interested in seeing how the contrast changes. As now Im not looking using the mean of S verions as baseline bur rather the average of the images across both versions


polemic_df$version_c <- ifelse(polemic_df$version == "H", 0.5, -0.5)

c_pares3_half <- lmer(controversial ~ ck_ * version_c + (version_c | id) + (1|id:ck_),
                      data = polemic_df, REML=T)
summary(c_pares3_half, ddf="Kenward-Roger")

emm_c_half <- emmeans(c_pares3_half, ~ ck_) 

# Establish comparisons with political and non political topic
lv_polemic <- levels(polemic_df$ck_)
stopifnot(identical(lv_polemic, as.character(emm_c_half@levels$ck_)))

dim_vec_polemic <- ifelse(parse_number(lv_polemic) <= 39,
                          "political", "non-political")
stopifnot(!anyNA(dim_vec_polemic))


eff_within_half <- lapply(seq_along(lv_polemic), function(i) {
  same   <- dim_vec_polemic == dim_vec_polemic[i]
  others <- same & (seq_along(lv_polemic) != i)   # exclude i from the reference
  w <- rep(0, length(lv_polemic))
  names(w) <- lv_polemic
  w[others] <- -1 / sum(others)                   # only the OTHER cells get -1/(m-1)
  w[i]      <- 1                                  # i itself gets +1
  w
})
names(eff_within_half) <- lv_polemic



dev_df_half <- contrast(emm_c_half, method = eff_within_half) |>
  summary(infer = TRUE, adjust = "none") |>   # descriptive, not a formal test
  as.data.frame() |>
  mutate(
    ck_id     = as.character(contrast),
    dimension = ifelse(parse_number(ck_id) <= 39, "political", "non-political")
  ) |>
  dplyr::select(ck_id, dimension, dev = estimate, SE, lower.CL, upper.CL)

dev_df_half <- dev_df_half |>
  mutate(
    ci_level = 0.95,
    status = case_when(
      lower.CL > 0 ~ "above_topic",
      upper.CL < 0 ~ "below_topic",
      TRUE         ~ "inconclusive"
    ),
    borderline = (upper.CL < 0 & abs(upper.CL) / SE < 0.5) |
      (lower.CL > 0 & abs(lower.CL) / SE < 0.5)
  ) |>
  arrange(dimension, ck_id)

# This responds: is this image, on average across its two versions, more or less polemical than the average image? 
# The previous analysis (dev_df_polemic) answers: is this version of this image above/below the other versions of the same topic, at the same version level?
# It Is different to say H versions of X images is more polemical than saying X image is more polemical
# I like the first option as I can see differences beetween versions which is important. If a S versions is well below the average and the H verions is well above maybe it is not an ideal candidate to be sue
  # There are really no much changes excepts that CK33is not below the average in this new comparisons, while in the first one the H verions is marginally below

rm(c_pares1,c_pares2, eff_within_half,dim_vec_polemic, lv_polemic,eff_within_polemic,emm_ver_polemic,emm_c_half)



# 6.6 ... and the winners are!!!! -----------------------------------------
  # Using a relaxed criteria in Step 4
    # CK10 and CK11 -> Identity Control Policies  (group 4)
    # CK24 and CK25 -> Protest  (group 3)
    # CK26 and CK27 -> Spending Cuts (group 1)
    # CK28 and CK29 -> Register of Incivilities  (group 4)
  # and all non political except the ones regarding Whatsapp audios
    # doubts regarding the concerts ones





# 7. Analysis of Political Cynicism Scales ------------------------------------------

# Originally I wanted to test two differents phrasing of the same political cynicism scale based on Patty et al. (2012): https://journals.sagepub.com/doi/10.1002/per.858
  # Lothamer (2025) build the same scale but worded in terms of frequency rather than agreement/disagreement: https://uh-ir.tdl.org/items/53028c50-16f3-41e0-8f06-0273b728a718

# So I wanted to test whether both worked, and which one created more normal distributions. But I didnt, why?
  # The first 80 answers of the pilot were equally distributed between both scales (AS01=1 view frequency AS01=2 view a/d)
  # However it was pretty soon clear that the Lothamer scale was not working, really low correlations among items
  # Based on that I decided that the remaining responses were just on the a/d scale, so I have enough data to conduct a CFA


cinis_ad <- bd[, c(cinismo, "id_pol_3")]
#cinis_ad<-cinis_ad[rowSums(is.na(cinis_ad))<ncol(cinis_ad),] #remove cases that were not assigned to this condition
cinis_ad[cinis_ad == -1]<-NA # -1 is actually "I prefer not to answer", but let's treat it as NA

cinis_ad <- cinis_ad %>%
  filter(!if_all(1:6, is.na)) #delete cases were all the columns are NA (cases not assigned to this condition)

# Contingency table: Invariance testing using ordinal data requires data in all levels of each cynicism question
lapply(cinis_ad 
       %>% dplyr::select(starts_with("VD03")), function(x) table(cinis_ad$id_pol_3, x))
# This shows this is not the case, which means I wont be able to do invariance testing across groups
  # Unless I merge 1-2 categories of response
  # Also group sizes are quite small which creates unstable 
  
table(cinis_ad$VD03_01)# Las personas muy frecuentemente son manipuladas por los políticos
table(cinis_ad$VD03_02) # Los políticos solo están interesados en obtener y mantener poder.
table(cinis_ad$VD03_03) # Los políticos fingen preocuparse más de la gente de lo que realmente lo hacen.
table(cinis_ad$VD03_04) #  Nuestros líderes políticos están preparados para mentirnos cuando les sea útil a sus propósitos.
table(cinis_ad$VD03_05) # Ninguna persona puede esperar mantenerse honesta al entrar a política.
table(cinis_ad$VD03_06) #  Casi todos los políticos venderían sus ideales o romperían sus promesas para aumentar su poder.
describe(cinis_ad) 

# Factorizing variables to use WLSMV estimator
cinis_ad$VD03_01<-as.factor(cinis_ad$VD03_01) 
cinis_ad$VD03_02<-as.factor(cinis_ad$VD03_02)
cinis_ad$VD03_03<-as.factor(cinis_ad$VD03_03)
cinis_ad$VD03_04<-as.factor(cinis_ad$VD03_04)
cinis_ad$VD03_05<-as.factor(cinis_ad$VD03_05)
cinis_ad$VD03_06<-as.factor(cinis_ad$VD03_06)
str(cinis_ad)

# 7.1 CFA

# When using MLR the loading mean the change in item when latenT factor increases 1SD, and WLSMB: he loading represents the expected change in the latent continuous response variable y* (in standard deviation units) for a 1 standard deviation increase in the latent factor "cinismo
# Thresholds: They define the cutpoints on the latent continuous variable y* that separate the ordinal categories
  #Non significance mean that the data cannot precisely locate the cutpoint between categories

mod1<- 'cinismo=~VD03_01+VD03_02+VD03_03+VD03_04+VD03_05+VD03_06'  
fit<-cfa(mod1,std.lv=T, estimator ="WLSMV", data=cinis_ad, ordered = c("VD03_01", "VD03_02", "VD03_03", "VD03_04", "VD03_05", "VD03_06") )
summary(fit, fit.measures=T, standardize=TRUE)
fitMeasures(fit, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
# About the warning I get: this can be due to an item with really few responses, given ceiling effects

# Negative threshold values (like $-2.192$ and $-1.722$) indicate that very low levels of the latent trait are needed for respondents to transition from category 1 to category 2

modindices(fit, sort.=T) #huge dependencies between VD03_01 and 04

#version with _01: cf.robus=0.913 tli.robus=0.856 rmsea.robust=0.153 srmr=0.067
# version without _01:  cf.robus=0.972 tli.robus=0.943 rmsea.robust=0.106 srmr=0.037

# Residual dependencies between items 1 and 4 (manipulation by politicians)  make sense
  # item 1: People are very frequently manipulated by politicians <-> item 4:Our political leaders are prepared to lie to us whenever it suits their purposes
  # There is evidence that political cynicism can also be divided into several first order dimensions (this was actually a theoretical headache when reading literature on the topic), so I will create a new model

# Model with dependencies
mod1_adjust <- '
  cinismo =~ VD03_01 + VD03_02 + VD03_03 + VD03_04 + VD03_05 + VD03_06
  VD03_01 ~~ VD03_04'
fit_adjust<-cfa(mod1_adjust,std.lv=T,estimator ="WLSMV", data=cinis_ad, ordered = c("VD03_01", "VD03_02", "VD03_03", "VD03_04", "VD03_05", "VD03_06") )
summary(fit_adjust, fit.measures=T, standardize=TRUE)
fitMeasures(fit_adjust, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
lavPredict(fit_adjust, method="EBM") #shows position of each person relative to latent factor based on SD from mean


# Weighted mean 0,039·VD03_01 + 0,384·VD03_02 + 0,176·VD03_03 + 0,093·VD03_04 + 0,112·VD03_05 + 0,195·VD03_06
# (0.039*4 + 0.384*5 + 0.176*5 + 0.093*5 + 0.112*3 + 0.195*4) weighted 4.537 unweighted 4.3
# (0.039*3 + 0.384*4 + 0.176*4 + 0.093*4 + 0.112*2 + 0.195*2) weighted 3.43 unweighted 3.16




# Invariance testing across ideological groups was not conducted because the pilot sample (N = 143; n = 47, 39, 57 per group) provides insufficient power for multi-group ordinal CFA, 
# particularly given the ceiling effects in the items. Group-specific threshold estimates would be unstable and ΔCFI/ΔRMSEA comparisons unreliable. 
# Invariance will be tested in the full study (N ≈ 500).



# Lets see how cinismo differs across groups
cinis_ad <- cinis_ad %>%
  mutate(
    cinismo_mean = rowMeans(
      sapply(pick(all_of(cinismo)), function(x) as.numeric(as.character(x))),
      na.rm = TRUE
    )
  ) #im using cinis_ad and not bd as it already transformed -1 into NA JUST for these particular items. Given thatn in bd -1 means something else in other items



group_summary <- cinis_ad %>%
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
    caption = "Izquierda=1 Derecha=2 Centro/Independientes/Apoliticos=3"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

table(cinis_ad$cinismo_mean, cinis_ad$id_pol_3)

# Density Plot Cisnismo Scores

ggplot(cinis_ad, aes(x = cinismo_mean)) +
  geom_density(fill = "steelblue", alpha = 0.5, color = "darkblue", linewidth = 1) +
  labs(
    title = "Grafido de Densidad Cinismo",
    x = "Promedio Cinismo (Promedio Simple)",
    y = "Densidad"
  ) +
  theme_minimal()

# 7.2 AGREE/DISAGREE SCALE - Cronbach


  # Given there is some correlation between measurement errors and no Tau equivalence Cronbach's Alpha is not be the best indicator
  # Run a McDonald's Omega

omega_cinismo <- omega(sapply(cinis_ad[, cinismo], as.numeric),nfactors = 1,poly = FALSE)  # i have to revert the transformation of the values to numeric
print(omega_cinismo) # 0.82 very nice
# g explains load into single factor / h2 is g^2 which is proportion of items variance explained by latent factor / u2 is residual variance

cor(sapply(cinis_ad[, cinismo], as.numeric), use = "pairwise.complete.obs", method = c("spearman"))

# i can also run the Omega's with the same correlation of errors identified in modnindices

omega_cinismo_cor <- cfa(model = mod1_adjust,data = cinis_ad,estimator = "WLSMV",ordered = cinismo)

# Compute composite reliability (Omega)
compRelSEM(omega_cinismo_cor)

# Due to the violation of tau-equivalence ($p < .05$) and the presence of significant residual covariances between item pairs (VD03_01 with VD03_04, and VD03_05 with VD03_06), 
# standard Cronbach’s alpha ($\alpha = .82$) overstated scale reliability. 
# To account for these measurement error dependencies, model-based McDonald’s omega total was calculated using CFA in lavaan and semTools. 
# The adjusted reliability estimate was $\omega = 792$, indicating acceptable internal consistency for the cynicism scale after controlling for method variance."

# The pilot provides preliminary support for the six-item scale as a unidimensional measure of political cynicism, with acceptable internal consistency and good fit in the pooled sample. 
# These results justify proceeding to the full study, where invariance across ideological groups and the ceiling effects observed here will be addressed


rm(group_summary)

######################### FREQUENCY SCALE 
# 7.3 This scale was discarded after the first 45 measurements as it had EXTREMELY POOR CRONBACH


cinis_fr<-bd[,c("VD05_01","VD06_01","VD07_01","VD08_01","VD09_01","VD10_01")] #frequency VD09 is inverted
cinis_fr<-cinis_fr[rowSums(is.na(cinis_fr))<ncol(cinis_fr),]


describe(cinis_fr) # check descriptives before inverting to check everything is alright


cinis_fr<-cinis_fr%>%
  mutate(VD09_01=6-VD09_01)


cor(cinis_fr, use = "everything", method = c("spearman")) #WHAT
# really really really low correlations



# z. Codes and Ideas Testing ----------------------------------------------



######################## Changes to comparision of ¿iamges to means ( differentiating between political means and non political means)
# The comparision has to be made within political or non political memes. If all is together the mean of comparison for political memes will include values from non -political ones (which is higher)
# see last section of code
# I should have the same consideration in step 5

lv      <- levels(fun_df$ck_)
stopifnot(identical(lv, as.character(emm_ver@levels$ck_)))  # weights must follow the grid order
dim_vec <- ifelse(parse_number(lv) <= 39, "political", "non-political") #over 39 are not political

eff_within <- lapply(seq_along(lv), function(i) {
  same <- dim_vec == dim_vec[i]
  w <- rep(0, length(lv))
  w[same] <- -1 / sum(same)
  w[i] <- w[i] + 1
  w
})
names(eff_within) <- lv

dev_df <- contrast(emm_ver, method = eff_within) |>   # computed separately for H and S
  summary(infer = TRUE, adjust = "none") |>           # no adjustment: descriptive, not a formal test
  as.data.frame() |>
  mutate(ck_id     = as.character(contrast),
         dimension = ifelse(parse_number(ck_id) <= 39, "political", "non-political")) |>
  dplyr::select(ck_id, dimension, version, dev = estimate, SE, lower.CL, upper.CL)



dev_pooled <- contrast(emm_ver, method = "eff") |>
  summary(infer = TRUE, adjust = "none") |>
  as.data.frame() |>
  mutate(ck_id = sub(" effect$", "", contrast)) |>
  dplyr::select(ck_id, version, dev_pooled = estimate, SE_pooled = SE)

dev_df |>
  left_join(dev_pooled, by = c("ck_id", "version")) |>
  group_by(dimension, version) |>
  summarise(shift    = mean(dev - dev_pooled),  # dimension mean minus overall mean, sign reversed
            sd_shift = sd(dev - dev_pooled),    # should be about 0 (a constant shift)
            .groups = "drop")






######################## Using ordinal estimators for the LMM
library(ordinal)
fun_df$funniness <- factor(fun_df$funniness, ordered = TRUE)
m2_ordinal <- clmm(funniness ~ version + (version | id) + (1 | ck_), data = fun_df)
summary(m2_ordinal)

#e^[value in estimate] gives the odd ratio (odds to chose higher rating)
# i cant get p values for clmm but can make comparisons of LRT to get significance of improvement

# Not truly necessary as 5-point ordinal items with multiple repeated measurements per condition yield create kind of continuos aggregated response distributions












######################## Invariance testing (cant be done due to few numbers per group, and some cynicism items without values)

# to do just subset of id_pol_3 add data = subset(cinis_ad, id_pol_3 == 2),

#### In order to calculate this i would have to aggregate  values of 1-2

# Lets check configural invariance by ideological group (Number of factors and item loadings)
config_adjust <- cfa(mod1_adjust, std.lv= T, ,estimator ="WLSMV", data=cinis_ad, ordered = c("VD03_01", "VD03_02", "VD03_03", "VD03_04", "VD03_05", "VD03_06"), group="id_pol_3" ) 
config_adjust
summary(config_adjust)
fitMeasures(config_adjust, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))

#  Weak Invariance Metric --> FACTOR LOADINGS EQUAL ACROOS GROUPS?
weak <- cfa(mod1_adjust,data=cinis_ad,std.lv= T, estimator = "WLSMV", ordered = c("VD03_01", "VD03_02", "VD03_03", "VD03_04", "VD03_05", "VD03_06"), group.equal="thresholds")
weak
summary(weak)
fitMeasures(weak, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))

# # Strong invariance (intercepts) - Scalar Invariance NOPE
strong <- cfa(mod1_adjust,data=cinis_ad,std.lv= T, estimator = "WLSMV", ordered = c("VD03_01", "VD03_02", "VD03_03", "VD03_04", "VD03_05", "VD03_06"), group.equal = c("loadings", "thresholds"))
fitMeasures(strong, c("cfi.robust", "tli.robust", "rmsea.robust", "srmr"))
# NO SCALAR INVARIANCE This means there even when cinismo levels are similar, some items are rated more high by some groups than other

# lets compare weak and configural

compare_cfa<-compareFit(config_adjust, weak)
summary(compare_cfa)

#### COMPARISIONS WITH PANEL 




# Info for the panel regarding data

panel <- bd %>%
  filter(QUESTNNR %in% c("piloto2")) #108


panel <- panel %>% filter(total_p >= 6) #-3



# Delete people with TIME_RSI >2 Relative Speed Index above 2

panel <- panel %>% filter(TIME_RSI < 2) #-5





# revisar distribucion de caracteristicas

# Political Affiliation

panel %>%
  filter(SD06 >= 1, SD06 <= 9) %>%
  ggplot(aes(x = factor(SD06))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Identidad Politica", y = "Numero") +
  theme_minimal()


table(panel$SD06)
table(panel$id_pol_3)
round(100 * prop.table(table(panel$id_pol_3)), 2) #as percentages

# Gender
table(panel$SD01) # 1 men, 2 female 3 non binary

#Education
table(panel$SD07) 

#1	Educación Básica Incompleta
#2	Educación Básica Completa
#3	Educación Media Incompleta
#4	Educación Media Completa
#5	Instituto Técnico Profesional/Centro de Educación Técnica Incompleta
#7	Educación Superior Universitaria Incompleta
#6	Instituto Técnico Profesional/Centro de Educación Técnica Completa
#8	Educación Superior Universitaria Completa
#-1	No contesta
#-9	Not answered




#average time of completion
describe(panel$TIME_SUM) 


#### JSON testing other wording

library(jsonlite)
library(stringr)

opciones_cinismo<- c(
  "Totalmente en desacuerdo"    = 1,
  "Moderadamente en desacuerdo" = 2,
  "Ligeramente en desacuerdo"   = 3,
  "Ni de acuerdo ni en desacuerdo" = 4,
  "Ligeramente de acuerdo"      = 5,
  "Moderadamente de acuerdo"    = 6,
  "Totalmente de acuerdo"       = 7
)

preguntas <- c(
  "01a173d8-f00a-7336-9777-63f87fc8db53" = "ck01", # Desconfío
  "01a173d8-f02f-719e-a401-ce23ad772fe8" = "ck02", # Poco honestos
  "01a173d8-f02f-719e-a401-d25af7a9eb11" = "ck03", # Incompetentes
  "01a173d8-f02f-719e-a401-d6db1724839f" = "ck04", # Inmorales
  "01a173d8-f02f-719e-a401-db6e8820fcef" = "ck05", # Egoístas
  "01a173d8-f030-727d-b5ec-dd0a4943df8e" = "ck06", # Fuera de contacto
  "01a17de3-510d-72cb-98f1-5dc39e8122d2" = "id_pol" # Identificación política
)



ruta<-file.choose()
lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
lineas <- lineas[nzchar(trimws(lineas))]
subs   <- lapply(lineas, fromJSON, simplifyVector = FALSE)

fila <- function(s) {
  vals <- map_chr(names(preguntas), function(id) {
    v <- s$responses[[id]]$values
    if (is.null(v) || length(v) == 0) NA_character_
    else paste(unlist(v), collapse = "; ")
  })
  names(vals) <- unname(preguntas)
  
  as_tibble_row(c(
    submission_id  = s$submission_id,
    participant_id = s$participant_id,
    created_at     = s$created_at,
    vals
  ))
}

df <- map_dfr(subs, fila)

df <- df |>
  mutate(
    created_at = as.POSIXct(created_at, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    across(starts_with("ck"), ~ {
      n <- suppressWarnings(as.numeric(.x))
      ifelse(is.na(n), unname(opciones_cinismo[.x]), n)
    })
  )

df <- df |>
  mutate(
    id_pol_txt = id_pol,   # keep the original text
    id_pol = case_when(
      str_detect(id_pol_txt, regex("^\\s*Centro",  ignore_case = TRUE)) ~ 5,
      str_detect(id_pol_txt, regex("^\\s*Ninguno", ignore_case = TRUE)) ~ 10,
      str_detect(id_pol_txt, regex("^\\s*No s[eé]", ignore_case = TRUE)) ~ 11,
      TRUE ~ as.numeric(str_extract(id_pol_txt, "^\\s*\\d+"))
    )
  )



table(df$id_pol)
describe(df)

(5.5+4.8+4+4.25+4.3+4.75)/6

cor_ri<-df[,c("ck01","ck02","ck03","ck04","ck05","ck06")]  

corr.test(cor_ri, method="spearman")


var_ri<-c("ck01", "ck02", "ck03", "ck04", "ck05", "ck06")
plots <- map(var_ri, function(v) {
  df |>
    count(.data[[v]]) |>
    ggplot(aes(x = factor(.data[[v]]), y = n)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = n), vjust = -0.3, size = 3) +
    labs(
      title = paste("Frequency of", v),
      x = v,
      y = "Count"
    ) +
    theme_minimal()
})


library(patchwork)
wrap_plots(plots, ncol = 3)
