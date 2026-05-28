usethis::use_github()
usethis::use_git()


library(usethis)

##################CALCULO DE PODER ESTADISTICO##################

#Due to limited data on the topic this is a analysis heavely dependent of suppositions. Mainly ot the estiamted effect sizes
#Sorry for the mix between english and spanish. I try to write mainly in english, but some times i need a vygotskian auto-scaffolding by writing in spanish


library(Superpower)

mu_ordinal <- c(0.484, 0.121,  # Grupo 1 entre condiciones :efecto de interaccion [primer nivel de primer factor, primer nivel segundo factor] 
                0.121, 0)      # Grupo 2 entre condiciones :sin interaccion, o mas leve que en grupo 1   [segundo nivel del primer factor, segundo nivel del segundo factor]  

# This values are selected as a way to create the estimated principal and itneraction effect sizes.


#Definir diseño anona
diseno <- ANOVA_design(design = "2b*2w",
                       n = 300, # Calculo inicial, va a ir variando despues
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
                               max_n = 500,
                               desired_power = 80,
                               exact = TRUE, 
                               plot = TRUE)

#This takes around 4 minutes, so be patient. Go make yourself a coffee


n_valores<-poder_resultados$power_df #aca puedo ver valores de poder para cada n desde 150 a 500


#Simulacion de Monte Carlo con 300

set.seed(888)
simulacion_300<- ANOVA_power(diseno, 
                             nsims = 5000, 
                             alpha_level = 0.05,
                             verbose = FALSE)

simulacion_300