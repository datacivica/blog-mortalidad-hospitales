#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------
# git/hospitales_covid/import/import.R

#######################################
#¿Qué tipo de personas ingresan a cada tipo de hospital?
#¿Cuál es la tasa de mortalidad por edad y comorbilidades? 
#¿Cómo cambia la tasa de letalidad por región del sistema público del país? 
#######################################

rm(list=ls())
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, data.table, readxl, here)

files <- list(input_covid = here("/import/input/201024COVID19MEXICO.csv"),
              input_names = here("/import/input/Nombres_Estados.xlsx"),
              output_covid = here("/import/output/covid.rds")
              )

names <- read_excel(files$input_names)%>%
         clean_names()

data <- read.csv(files$input_covid)%>%
        clean_names()%>%
        select(-fecha_actualizacion, -origen, -entidad_nac)%>%
        left_join(., names, by=c("entidad_res"="cve_ent"))

saveRDS(data, files$output_covid)
