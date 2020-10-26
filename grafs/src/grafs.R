#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------
# git/hospitales_covid/grafs/src/grafs.R


rm(list=ls())
if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, extrafont, here, ggrepel, treemapify, viridis)
extrafont::loadfonts(quiet = T)

files <- list(output_covid = here("/clean-data/output/covid.rds"), 
              sexo1_s=here("/grafs/output/sexo1.svg"),
              sexo1_j=here("/grafs/output/sexo1.jpg"),
              sexo2=here("/grafs/output/sexo2.svg"),
              paciente_s=here("/grafs/output/paciente.svg"),
              paciente_j=here("/grafs/output/paciente.jpg"),
              comorb_s=here("/grafs/output/comorb.svg"),
              comorb_j=here("/grafs/output/comorb.jpg"),
              edad1_s=here("/grafs/output/edades1.svg"),
              edad1_j=here("/grafs/output/edades1.jpg")
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
  

#### Sexo ####
tempo <- filter(data, !is.na(sexo) | !is.na(sector))%>%
         group_by(sexo, sector)%>%
         summarize(total=n())%>%
         ungroup()%>%
         group_by(sector)%>%
         mutate(den=sum(total, na.rm=T))%>%
         ungroup()%>%
         mutate(per=(total/den)*100)%>%
         na.omit()

tempo2 <- select(tempo, sexo,sector, per)%>%
          pivot_wider(names_from=sexo, values_from=per)%>%
          mutate(dif=Mujer-Hombre)%>%
          select(sector, dif)

tempo <- left_join(tempo, tempo2, by="sector")

         
ggplot(tempo, aes(x = reorder(sector,-dif), y=per, fill = sexo)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(round(per,1), "%")),
            position=position_dodge(width=1), size=6, color="black", hjust=.5, vjust=-1.5,
             family = "Barlow Condensed")+
  labs(title="¿Qué sexo tienen las personas con COVID19 que atiende cada hospital?",
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020", 
       x="\n", y="\n Porcentaje \n", fill = "Sexo") +
  tema +
  scale_fill_manual(values = colores)+
  ylim(c(0,60))
ggsave(files$sexo1_s, width=16, height=8)
ggsave(files$sexo1_j, width=16, height=8)

#### Tipo de paciente ####
tempo <- group_by(data, paciente, sector)%>%
         summarize(total=n())%>%
         ungroup()%>%
         na.omit()%>%
         group_by(sector)%>%
         mutate(den=sum(total, na.rm=T))%>%
         ungroup()%>%
         mutate(per=(total/den)*100)%>%
         mutate(order2=as.numeric(paciente))

tempo2 <- filter(tempo, paciente=="Ambulatorio")%>%
          arrange(per)%>%
          mutate(n=1:6)%>%
          select(sector,n) 


tempo <- left_join(tempo, tempo2, by="sector")  

ggplot(tempo, aes(x = reorder(sector, n), y=per, group=-order2, 
                  fill = paciente)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(round(per,1), "%")),
            position = position_dodge(1), size=6, color="black", hjust=-.2,
            family = "Barlow Condensed")+
  labs(title="¿Qué tipo de pacientes atiende cada tipo de hospital?",
       subtitle="Porcentaje de total de pacientes",
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020", 
       x="\n", y="\n Porcentaje \n", fill = "Tipo de paciente") +
  tema +
  scale_fill_manual(values = colores)+
  coord_flip()+
  ylim(c(0,100))
ggsave(files$paciente_s, width=16, height=8)
ggsave(files$paciente_j, width=16, height=8)


#### Edades ####
tempo <- group_by(data,sector, order_edad, grupo_edad)%>%
  summarize(total=n())%>%
  ungroup()%>%
  na.omit()%>%
  group_by(sector)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=(total/den)*100)

tempo <- mutate(tempo, order=case_when(
         sector=="Secretaría de Salud" ~ 1,
         sector=="Sistema de salud estatal" ~ 2,
         sector=="Hospital privado" ~ 3,
         sector=="IMSS" ~ 4,
         sector=="Otros (menores al 1%)" ~ 5,
         sector=="ISSSTE" ~ 6,
))
tempo$order <- as.numeric(tempo$order)

ggplot(tempo, aes(x = reorder(sector, -order), y=per, group=-order_edad, 
                  fill = grupo_edad)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label=paste0(round(per,1), "%")),
            position = position_stack(1), size=6, color="white",hjust=1,
            family = "Barlow Condensed")+
  labs(title="¿Qué edad tienen las personas con COVID19 que atiende cada hospital?",
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020", 
       x="\n", y="\n Porcentaje \n", fill = "Grupo de edad") +
  tema +
  scale_fill_manual(values = colores)+
  coord_flip()
ggsave(files$edad1_s, width=16, height=8)
ggsave(files$edad1_j, width=16, height=8)

### Comorbilidades ####
tempo <- group_by(data, sector, order)%>%
  summarize(obesidad=sum(obesidad, na.rm=T),
            diabetes=sum(diabetes, na.rm=T),
            tabaquismo=sum(tabaquismo, na.rm=T),
            hipertension=sum(hipertension, na.rm=T),
            total=n())%>%
  na.omit()%>%
  group_by(sector, order)%>%
  mutate(obesidad=(obesidad/total)*100,
         diabetes=(diabetes/total)*100,
         tabaquismo=(tabaquismo/total)*100,
         hipertension=(hipertension/total)*100)%>%
  ungroup()%>%
  gather("enfermedad", "porcentaje",3:6)%>%
  mutate(enfermedad=gsub("obesidad", "Obesidad", enfermedad),
         enfermedad=gsub("diabetes", "Diabetes", enfermedad),
         enfermedad=gsub("tabaquismo", "Tabaquismo", enfermedad),
         enfermedad=gsub("hipertension", "Hipertensión", enfermedad))

tempo <- mutate(tempo, order=case_when(
  sector=="Secretaría de Salud" ~ 4,
  sector=="Sistema de salud estatal" ~ 5,
  sector=="Hospital privado" ~ 6,
  sector=="IMSS" ~ 2,
  sector=="Otros (menores al 1%)" ~ 3,
  sector=="ISSSTE" ~ 1,
))


ggplot(tempo, aes(x = enfermedad, y=porcentaje, 
                  fill = sector, group=order)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(round(porcentaje,1), "%")),
            position = position_dodge(1), size=5.5, color="black", hjust=.25, vjust=-1,
            family = "Barlow Condensed")+
  labs(title="¿Qué porcentaje de los pacientes que atiende cada hospital tienen...?",
       subtitle="Porcentaje de total de pacientes",
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020", 
       x="\n", y="\n Porcentaje \n", fill = "Tipo de hospital") +
  tema +
  scale_fill_manual(values = colores)+
  ylim(c(0,30))
ggsave(files$comorb_j, width=16, height=8)
ggsave(files$comorb_s, width=16, height=8)


