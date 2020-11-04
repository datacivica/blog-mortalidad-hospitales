#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez, OE
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------
# blog-mortalidad-hospitales/import/src/import.R

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, readxl, here)

files <- list(input_covid = here("import/input/201024COVID19MEXICO.csv.zip"),
              input_names = here("import/input/Nombres_Estados.xlsx"),
              output_covid = here("import/output/covid.rds")
              )

keep_vars <- c("sector", "entidad_um", "sexo", "edad", "entidad_res", "tipo_paciente", "fecha_ingreso",
               "fecha_sintomas", "fecha_def", "intubado", "diabetes", "epoc", "asma", "inmusupr", 
               "hipertension", "otra_com", "cardiovascular", "obesidad", "renal_cronica", "tabaquismo",
               "resultado_lab")

# ==== Nombres de entidad
names <- read_excel(files$input_names) %>%
  clean_names() %>% 
  mutate(cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0")) %>% 
  select(-region)

# ==== Base SSA COVID-19
covid <- read_csv(files$input_covid) %>%
  clean_names() %>%
  select(one_of(keep_vars)) %>%
  rename(cve_ent = entidad_res) %>% 
  left_join(names)

saveRDS(covid, files$output_covid)

# done.
