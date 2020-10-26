#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------
# git/hospitales_covid/grafs/src/grafsMortalidad.R


rm(list=ls())
if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, extrafont, here, ggrepel, treemapify, viridis)
extrafont::loadfonts(quiet = T)

#### Tasa de mortalidad por sexo, edad, población vulnerable, intubados, hospitalizados y comorbilidades ####
files <- list(output_covid = here("/clean-data/output/covid.rds"), 
              msexo_edad_s=here("/grafs/output/msexo_edad.svg"),
              msexo_edad_j=here("/grafs/output/msexo_edad.jpg"),
              mpaciente_s=here("/grafs/output/mpaciente.svg"),
              mpaciente_j=here("/grafs/output/mpaciente.jpg"),
              mcomorb_s=here("/grafs/output/mcomorb.svg"),
              mcomorb_j=here("/grafs/output/mcomorb.jpg"),
              mcomorb1_s=here("/grafs/output/mcomorb1.svg"),
              mcomorb1_j=here("/grafs/output/mcomorb1.jpg")
)

data <- readRDS(files$output_covid)

#### Cosas para gráficas####
tema <- theme_minimal() +
  theme(plot.title = element_text(size = 16, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 10, family = "Barlow Condensed", face = "bold"),
        axis.title = element_text(size = 12, family = "Barlow Condensed"),
        legend.text = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"),
        legend.position="top")

colores <- c("#0C0A3E", "#7B1E7A", "#B33F62", "#F9564F", "#F3C677", "#0B5D1E")

#### Sexo y edad ####
tempo <- group_by(data, sector, order, sexo, grupo_edad, muerto)%>%
  summarize(total=n())%>%
  ungroup()%>%
  group_by(sector, order, sexo, grupo_edad)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=(total/den)*100)%>%
  na.omit()%>%
  filter(muerto==1)

ggplot(tempo, aes(x=grupo_edad, y=reorder(sector, -order), fill=per)) +
  geom_tile(color="black") +
  scale_fill_viridis()+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según su sexo, edad y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(per, 2),"%")), size=5, hjust=.5, vjust=.5, color="white", 
            family = "Barlow Condensed")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top")+
  facet_wrap(~sexo)
ggsave(files$msexo_edad_s, width=16, height=8)
ggsave(files$msexo_edad_j, width=16, height=8)


#### Tipos de pacientes ####
tempo <- group_by(data, sector, order, paciente, muerto)%>%
  summarize(total=n())%>%
  ungroup()%>%
  group_by(sector, order, paciente)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=(total/den)*100)%>%
  na.omit()%>%
  filter(muerto==1)



ggplot(tempo, aes(y=reorder(sector, -order), x=paciente, fill=per)) +
  geom_tile(color="black") +
  scale_fill_viridis() + 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según el tipo de paciente y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 14 de septiembre de 2020",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(per, 2),"%")), size=5, hjust=.5, vjust=.5, color="white", 
            family = "Barlow Condensed")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top")
ggsave(files$mpaciente, width=16, height=8)


####  Comorbilidades y edad ####
tempo <- group_by(data, sector, order, muerto, cuatro_edad)%>%
  summarize(total=n())%>%
  na.omit()%>%
  group_by(sector, order, cuatro_edad)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(porcentaje=(total/den)*100)%>%
  filter(muerto==1)

ggplot(tempo, aes(x=reorder(sector, order), y=cuatro_edad, fill=porcentaje)) +
  geom_tile(color="black") +
  scale_fill_viridis() + 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según si tenían una comorbilidad (incluyendo edad) y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n
       *Dentro de las demás comorbilidades están EPOC, asma, enfermedad autoinmune, enfermedad cardiovascular y renal crónica.",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(porcentaje, 2),"%")), size=5, hjust=.5, vjust=.5, 
            color="white", 
            family = "Barlow Condensed")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top")
ggsave(files$mcomorb_s, width=16, height=8)
ggsave(files$mcomorb_j, width=16, height=8)

#### Perfiles ####
tempo <- group_by(data, sector, order, muerto, perfiles)%>%
  summarize(total=n())%>%
  na.omit()%>%
  group_by(sector, order, perfiles)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(porcentaje=(total/den)*100)%>%
  filter(muerto==1)

ggplot(tempo, aes(x=reorder(sector, order), y=perfiles, fill=porcentaje)) +
  geom_tile(color="black") +
  scale_fill_viridis() + 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según su perfil y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(porcentaje, 2),"%")), size=5, hjust=.5, vjust=.5,
            color="white", 
            family = "Barlow Condensed")+
  tema
ggsave(files$mcomorb1_s, width=16, height=8)
ggsave(files$mcomorb1_j, width=16, height=8)


