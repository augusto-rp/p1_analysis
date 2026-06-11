usethis::use_github()
usethis::use_git()


library(usethis)

##################CALCULO DE PODER ESTADISTICO##################

#Due to limited data on the topic this is a analysis heavely dependent of suppositions. Mainly ot the estiamted effect sizes
#Sorry for the mix between english and spanish. I try to write mainly in english, but some times i need a vygotskian auto-scaffolding by writing in spanish



library(Superpower)

# Assuming a crossover interaction pattern and an effect size of d=0.242 in every factor

mu_ordinal <- c(0.484, 0.121,  # Grupo 1 entre condiciones :efecto de interaccion [primer nivel de primer factor, primer nivel segundo factor] 
                0.121, 0)      # Grupo 2 entre condiciones :sin interaccion, o mas leve que en grupo 1   [segundo nivel del primer factor, segundo nivel del segundo factor]  

# This values are selected as a way to create the estimated principal and itneraction effect sizes.

mu_conservador<-mu_ordinal*0.8

#Definir diseño anona
diseno <- ANOVA_design(design = "2b*2w",
                       n = 250, # Calculo inicial, va a ir variando despues, N is between factors
                       mu = mu_ordinal,
                       sd = 1, #Asume homogeneidad de varianza, en LMM esto no se cumple. Pero esto es un calculo mas limitado pues aun no hay piloto
                       r = 0.5, #se pueden ver mas correlaciones, ver documentacion pero vamos a mantener solo una
                       labelnames = c("claves", "humor", "nohumor",
                                      "tema", "politico", "nopolitico"),
                       plot = TRUE)




#Sobre correlaciones: The number of possible comparisons is the product of the levels of all factors squared minus the product of all factors, divided by two. 
#For a 2x2 design where each factor has two levels, this is: (((2*2)^2)-(2*2))/2

#Hay dos formas de calcular el poder usando esta libreria 
#ANOVA_power que toma modelo anterior y hace nsimulaciones
#ANOVA_exact que permite hacer estimaciones en base a un dataset :usar este más adelante

#En cualquiera de estos casos tendria que ir probando una y otra vez distintos N hasta encontrar el valor necesario.
#Peeeero tambien puedo plotear el poder y ver como varia

poder_resultados <- plot_power(diseno,
                               min_n = 150,
                               max_n = 300,
                               desired_power = 80,
                               exact = TRUE, 
                               plot = TRUE)

#This takes around 4 minutes, so be patient. Go make yourself a coffee

#I would need at minimum 540 participants, but money is scarce. And at best i can get 500 total

n_valores<-poder_resultados$power_df #aca puedo ver valores de poder para cada n desde 150 a 500


#Montecarlo Sim of power with 250 participants (500 total)

set.seed(888)
simulacion_250<- ANOVA_power(diseno, 
                             nsims = 5000, 
                             alpha_level = 0.05,
                             verbose = FALSE)

simulacion_250

#Given a 300 sample and assuming the aforementioend effec sizes, what are the MDE?
library(ggplot2)
library(tidyverse)


# Function to get power for a given effect size multiplier with 500 total 
get_power_curve <- function(scale_multiplier, n_per_group = 250) {  # 250 per between-group
  mu_scaled <- mu_ordinal * scale_multiplier
  
  diseno_temp <- ANOVA_design(design = "2b*2w",
                              n = n_per_group * 2,  # Total N
                              mu = mu_scaled,
                              sd = 1,
                              r = 0.5,
                              labelnames = c("claves", "humor", "nohumor",
                                             "tema", "politico", "nopolitico"),
                              plot = FALSE)
  
  sim_temp <- ANOVA_power(diseno_temp, 
                          nsims = 1000,  # Balance speed and accuracy
                          alpha_level = 0.05,
                          verbose = FALSE)
  
  return(data.frame(
    scale = scale_multiplier,
    claves = sim_temp$main_results$power[1],
    tema = sim_temp$main_results$power[2],
    interaction = sim_temp$main_results$power[3]
  ))
}


scale_values <- seq(0, 0.9, by = 0.1)
power_curves <- map_df(scale_values, get_power_curve)

print(power_curves)

# Plot power curves
ggplot(power_curves, aes(x = scale)) +
  geom_line(aes(y = claves, color = "claves"), size = 1.2) +
  geom_line(aes(y = tema, color = "tema"), size = 1.2) +
  geom_line(aes(y = interaction, color = "interaction"), size = 1.2) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
  labs(title = "Power Curves by Effect Size Multiplier",
       x = "Effect Size Multiplier (relative to original pattern d=0.242)",
       y = "Power (%)",
       color = "Effect") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(0, 0.9), breaks = seq(0, 0.9, 0.1))

#this graph shows how statistical power decreases as the effect size is proportionaly lower than orinally estimated
#so if the effect size is an 80% of the simulated effect size, this are the statistical powers:
# between factor =
# with in factor =
# interaction =