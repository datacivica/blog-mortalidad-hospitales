#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez, OE
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# ------------------------------------------------------
# blog-mortalidad-hospitales/clean-data/src/clean-data.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here)

files <- list(input = here("import/output/covid.rds"),
              output = here("clean-data/output/covid.rds")
              )

covid <- readRDS(files$input) %>%
  filter(resultado_lab==1) %>% 
  mutate(sector = case_when(sector==12 ~ "Secretaría de Salud",
                            sector==4 ~ "IMSS",
                            sector==6 ~ "ISSSTE",
                            sector==9 ~ "Hospital privado",
                            sector==3 ~ "Sistema de salud estatal",
                            T ~ "Otros (menores al 1%)"),
         sector = factor(sector, levels = c("Secretaría de Salud","IMSS",
                                            "ISSSTE","Hospital privado",
                                            "Sistema de salud estatal",
                                            "Otros (menores al 1%)")),
         sexo = factor(sexo, levels = 1:2, labels = c("Mujer", "Hombre")),
         paciente = case_when(tipo_paciente==1 ~ "Ambulatorio",
                              tipo_paciente==2 & intubado!=1 ~ "Hospitalizado",
                              tipo_paciente==2 & intubado==1 ~ "Intubado"),
         paciente = factor(paciente, levels = c("Ambulatorio", "Hospitalizado", "Intubado")),
         tipo_paciente = factor(tipo_paciente, levels = 1:2, labels = c("Ambulatorio", "Hospitalizado")),
         grupo_edad = case_when(edad<18 ~ "Menores de 18 años",
                                edad %in% 18:29 ~ "De 18 a 29 años",
                                edad %in% 30:44 ~ "De 30 a 44 años",
                                edad %in% 45:60 ~ "De 45 a 60 años",
                                edad>60 ~ "Más de 60 años"),
         grupo_edad = factor(grupo_edad, levels = c("Menores de 18 años",
                                                    "De 18 a 29 años",
                                                    "De 30 a 44 años",
                                                    "De 45 a 60 años",
                                                    "Más de 60 años")),
         muerto = ifelse(!is.na(fecha_def), 1, 0)) %>%
  rowwise() %>% 
  mutate(total_com = sum(c_across(diabetes:tabaquismo) == 1),
         cuatro_com = sum(c_across(c(hipertension, diabetes, obesidad, tabaquismo)) == 1)) %>% 
  ungroup() %>% 
  mutate(tabaquismo_s = ifelse(total_com==1 & tabaquismo==1, 1, 0),
         obesidad_s = ifelse(total_com==1 & obesidad==1, 1, 0),
         diabetes_s = ifelse(total_com==1 & diabetes==1, 1, 0),
         hipertension_s = ifelse(total_com==1 & hipertension==1, 1, 0),
         cuatro_com = case_when(cuatro_com==0 ~ "Sin ninguna de estas comorbilidades",
                                cuatro_com %in% 1:2 ~ "Una o dos comorbilidades \n (Hipertensión, diabetes, tabaquismo u obesidad)",
                                cuatro_com>=3 ~ "Tres o cuatro comorbilidades \n (Hipertensión, diabetes, tabaquismo u obesidad)"),
         cuatro_com = factor(cuatro_com, levels = c("Sin ninguna de estas comorbilidades",
                                                    "Una o dos comorbilidades \n (Hipertensión, diabetes, tabaquismo u obesidad)",
                                                    "Tres o cuatro comorbilidades \n (Hipertensión, diabetes, tabaquismo u obesidad)")),
         m60 = ifelse(grupo_edad=="Más de 60 años", 1, 0),
         m60 = factor(m60, levels = 0:1, labels = c("Menos de 60 años", "Más de 60 años")))

saveRDS(covid, files$output)

# done.
