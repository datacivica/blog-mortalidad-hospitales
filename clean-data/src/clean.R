#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------
# git/hospitales_covid/clean-data/clean.R

rm(list=ls())
options(scipen=999)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, data.table, readxl, here, zoo)


#### Archivos y funciones ####
files <- list(input_covid = here("import/output/covid.rds"),
              output_covid = here("clean-data/output/covid.rds")
)

sino <- function(x) {
  x <- gsub(2,0,x)
}

#### Limpiar ####
data <- readRDS(files$input_covid)
data[data>96] <- NA
data$fecha_def[data$fecha_def=="9999-99-99"] <- NA

data <- mutate(data, sector=ifelse(sector==12, 1,
                                   ifelse(sector==4, 2,
                                          ifelse(sector==6, 3,
                                                 ifelse(sector==9, 4,
                                                        ifelse(sector==3, 5,6))))),
               order=sector, 
               sector=factor(sector, levels=c(1:6), labels=c("Secretaría de Salud", "IMSS",
                                                             "ISSSTE", "Hospital privado",
                                                             "Sistema de salud \n estatal", 
                                                             "Otros \n (menores al 1%)")),
               sexo=as.numeric(as.character(sexo)),
               sexo=factor(sexo, levels=c(1:2), labels=c("Mujer", "Hombre")),
               paciente=ifelse(tipo_paciente==1, 1,
                               ifelse(tipo_paciente==2 & intubado==2, 2,
                                      ifelse(tipo_paciente==2 & intubado==1, 3, NA))),
               paciente=factor(paciente, levels=c(1:3), labels=c("Ambulatorio", "Hospitalizado",
                                                                 "Intubado")),
               tipo_paciente=factor(tipo_paciente, levels=c(1:2), labels=c("Ambulatorio", "Hospitalizado")),
               nacionalidad=factor(nacionalidad, levels=c(1:2), labels=c("Mexicana", "Extranjera")))%>%
  mutate_at(vars(c("intubado", "neumonia", "embarazo", "indigena", "diabetes", "epoc", 
                   "asma", "inmusupr", "hipertension", "otra_com", "cardiovascular", 
                   "obesidad", "renal_cronica", "tabaquismo", "otro_caso", "migrante", 
                   "uci")), sino)%>%
  mutate_at(vars(c("intubado", "neumonia", "embarazo", "indigena", "diabetes", "epoc", 
                   "asma", "inmusupr", "hipertension", "otra_com", "cardiovascular", 
                   "obesidad", "renal_cronica", "tabaquismo", "otro_caso", "migrante", 
                   "uci")), as.numeric)%>%
  mutate_at(vars(c("fecha_ingreso", "fecha_sintomas", "fecha_def")), as.character)%>%
  mutate(fecha_ingreso =gsub("-", "/", fecha_ingreso),
         fecha_sintomas=gsub("-", "/", fecha_sintomas),
         fecha_def=gsub("-", "/",fecha_def),
         fecha_ingreso=as.Date(fecha_ingreso),
         fecha_sintomas=as.Date(fecha_sintomas),
         fecha_def=as.Date(fecha_def))%>%
  mutate(grupo_edad=ifelse(edad<18, 1, 
                           ifelse(edad>17 & edad<30, 2,
                                  ifelse(edad>29 & edad<45, 3,
                                         ifelse(edad>44 & edad<61, 4,
                                                ifelse(edad>60, 5, NA))))),
         order_edad=grupo_edad, 
         grupo_edad=factor(grupo_edad, levels=c(1:5), labels=c("Menores de 18 años",
                                                               "De 18 a 29 años",
                                                               "De 30 a 44 años",
                                                               "De 46 a 60 años",
                                                               "Más de 60 años")),
         mujer=ifelse(sexo=="Mujer", 1, 0),
         muerto=ifelse(!is.na(fecha_def), 1,0),
         total= rowSums(.[17:25], na.rm=T),
         tabaquismo_s=ifelse(total==1 & tabaquismo==1, 1, 0),
         obesidad_s=ifelse(total==1 & obesidad==1, 1, 0),
         diabetes_s=ifelse(total==1 & diabetes==1, 1, 0),
         hipertension_s=ifelse(total==1 & hipertension==1, 1, 0),
         cuatro= rowSums(.[c("hipertension","diabetes","obesidad","tabaquismo")], na.rm=T),
         cuatro=ifelse(cuatro==0, 1, 
                       ifelse(cuatro>0 & cuatro<3, 2, 
                              ifelse(cuatro>2, 3, NA))), 
         cuatro=factor(cuatro, levels=c(1:3), labels=c("Sin ninguna de estas comorbilidades",
                                                       "Una o dos comorbilidades \n (Hipertensión, diabetes, tabaquismo u obesidad)",
                                                       "Tres o cuatro comorbilidades \n (Hipertensión, diabetes, tabaquismo u obesidad)")),
         m60 =ifelse(grupo_edad=="Más de 60 años", 1, 0),
         m60 = factor(m60, levels=c(0:1), labels=c("Menos de 60 años", "Más de 60 años")),
         region=factor(region, levels=c(1:4), labels=c("Norte", "Bajío", "Centro", "Sur")))%>%
         filter(resultado_lab==1)



saveRDS(data, files$output_covid)


 
