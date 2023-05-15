library(tidyverse) #Carga de librerias previamente instaladas
library(janitor)
WorldHappiness_Corruption_2015_2020 <- read.csv("WorldHappiness_Corruption_2015_2020.csv")
c_wh <- clean_names(WorldHappiness_Corruption_2015_2020) #Limpieza de nombres de columnas

summary(c_wh) #Resumen estadístico

o_wh <- c_wh %>% #Pipe con data set
  select(-c(family:cpi_score)) %>% #Se eliminan columnas no relevantes al estudio
  arrange(desc(gdp_per_capita)) %>% #Ordenado de dataset de manera por GDP de manera descendente 
  mutate(gdp_happiness_level = if_else(gdp_per_capita >= 0.9198 & happiness_score >= 5.495, 'high', 'low')) 
  #Se crea una columna que dependiendo el gdp y el nivel de felicidad es alta o baja
  o_wh$gdp_happiness_level <- as.factor(o_wh$gdp_happiness_level) #Se cambia la variable creada en factores
o_wh %>% 
  ggplot(aes(gdp_per_capita, happiness_score, colour = gdp_happiness_level)) + geom_point(mapping = aes(x = gdp_per_capita, y = happiness_score)) + geom_smooth(method=lm, se = FALSE, linewidth = 1, col = 'black')
  #Grafica con linea de tendencia, se hace diferencia con base a gdp_happiness_level
summary(o_wh)

hypo <- count(o_wh,gdp_happiness_level, name = 'total') %>% 
  mutate(percent = total/1056)
print(hypo)

binom.test(428,1056,p=0.4,alternative="greater")
