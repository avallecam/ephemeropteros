library(tidyverse)
library(readxl)

ephedata <- 
  read_excel("data-raw/Matriz total depurada_vacios.xlsx") %>% 
  janitor::clean_names()

# ephedata %>% view()

ephedata %>% glimpse()

ephedata %>% 
  select(starts_with("anch"))

#' critica constructiva
#' rio_ancho_m
#' ephe_ancho_mm

ephedata %>% count(especie)

ephedata %>%
  # filtrar por especie
  filter(especie == "Baetodes traverae") %>%
  # filtrar por valor en histograma
  filter(ancho_cabeza > 1.5) %>%
  # seleccionar variables
  select(muestra,individuo,individuo_colecta,
         especie,ancho_cabeza,largo_total) %>%
  arrange(desc(ancho_cabeza))

ephedata_clean <- 
  ephedata %>% 
  # retirar outliers
  filter(!(ancho_cabeza > 1.5 & especie == "Baetodes traverae"))

ephedata_clean %>% 
  ggplot(aes(x = ancho_cabeza)) +
  geom_histogram(bins = 120) +
  facet_wrap(~especie, scales = "free")

ephedata_clean %>% 
  ggplot(aes(x = largo_total)) +
  geom_histogram(bins = 120) +
  facet_wrap(~especie, scales = "free")

ephedata_clean %>% 
  ggplot(aes(x = ancho_cabeza, y = largo_total)) +
  geom_point() +
  facet_wrap(~especie, scales = "free")

ephedata_clean %>% 
  mutate(
    mes = 
      fct_relevel(
        mes,"Enero","Febrero","Marzo", "Abril",
        "Mayo", "Junio", "Julio", "Agosto",
        "Setiembre", "Octubre", "Noviembre", 
        "Diciembre")) %>%  
  ggplot(aes(x = mes, y = ancho_cabeza)) +
  geom_violin() +
  facet_grid(especie~., scales = "free")

ephedata_clean %>% 
  select(ancho_cabeza,precipitacion_mensual)