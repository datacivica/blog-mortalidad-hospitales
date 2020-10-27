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
              edad1_j=here("/grafs/output/edades1.jpg"),
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
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n *Sólo se consideran casos que ya dieron positivo", 
       x="\n", y="\n Porcentaje \n", fill = "Sexo") +
  tema +
  scale_fill_manual(values = colores)+
  ylim(c(0,65))
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
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n *Sólo se consideran pacientes que dieron positivo", 
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
  sector=="Sistema de salud \n estatal" ~ 2,
  sector=="Hospital privado" ~ 3,
  sector=="IMSS" ~ 4,
  sector=="Otros \n (menores al 1%)" ~ 5,
  sector=="ISSSTE" ~ 6,
))
tempo$order <- as.numeric(tempo$order)

ggplot(tempo, aes(x = reorder(sector, -order), y=per, group=-order_edad, 
                  fill = grupo_edad)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label=paste0(round(per,1), "%")),
            position = position_stack(1), size=5, color="white",hjust=1,
            family = "Barlow Condensed")+
  labs(title="¿Qué edad tienen las personas con COVID19 que atiende cada hospital?",
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n *Sólo se consideran pacientes que ya dieron positivo", 
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



ggplot(tempo, aes(x = sector, y=porcentaje, 
                  fill = enfermedad)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=paste0(round(porcentaje,1), "%")),
            position = position_dodge(1), size=5.5, color="black", hjust=.25, vjust=-1,
            family = "Barlow Condensed")+
  labs(title="¿Qué porcentaje de los pacientes que atiende cada hospital tienen...?",
       subtitle="Porcentaje de total de pacientes atendidos por COVID19",
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n *Los porcentajes no suman 100% porque una persona puede tener más de una comorbilidad y aquí no aparecen quienes no tienen comorbilidades \n**Sólo se consideran pacientes que ya dieron positivo", 
       x="\n", y="\n Porcentaje \n", fill = "") +
  tema +
  scale_fill_manual(values = colores)+
  ylim(c(0,30))
ggsave(files$comorb_j, width=16, height=8)
ggsave(files$comorb_s, width=16, height=8)


#### Sexo y edad ####
tempo <- group_by(data, sector, order, sexo, grupo_edad, muerto)%>%
  summarize(total=n())%>%
  ungroup()%>%
  group_by(sector, order, sexo, grupo_edad)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=(total/den)*100)%>%
  na.omit()%>%
  filter(muerto==1)%>%
  mutate(order_edad=as.numeric(grupo_edad))

ggplot(tempo, aes(x=reorder(sector, order), y=reorder(grupo_edad, -order_edad), fill=per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#fffffc", high="#023e8a")+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según su sexo, edad y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n*Sólo se consideran pacientes que ya dieron positivo",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(per, 2),"%")), size=5, hjust=.5, vjust=.5, color="black", 
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

ggplot(tempo, aes(x=reorder(sector, order), y=paciente, fill=per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#fffffc", high="#023e8a")+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según el tipo de paciente y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n**Sólo se consideran pacientes que ya dieron positivo",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(per, 2),"%")), size=5, hjust=.5, vjust=.5, color="black", 
            family = "Barlow Condensed")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top")
ggsave(files$mpaciente_s, width=16, height=8)
ggsave(files$mpaciente_j, width=16, height=8)


####  Comorbilidades y edad ####
tempo <- group_by(data, sector, order, muerto, cuatro, m60)%>%
  summarize(total=n())%>%
  na.omit()%>%
  group_by(sector, order, cuatro, m60)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(porcentaje=(total/den)*100)%>%
  filter(muerto==1)

ggplot(tempo, aes(x=reorder(sector, order), y=cuatro, fill=porcentaje)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#fffffc", high="#023e8a")+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según sus comorbilidades (incluyendo edad) y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020. \n**Sólo se consideran pacientes que ya dieron positivo",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(porcentaje, 2),"%")), size=5, hjust=.5, vjust=.5, 
            color="black", 
            family = "Barlow Condensed")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top")+
  facet_wrap(~m60)
ggsave(files$mcomorb_s, width=16, height=8)
ggsave(files$mcomorb_j, width=16, height=8)

#### Una comorb y distintass edadess ####
tempo <- group_by(data, sector, order, muerto, grupo_edad)%>%
  summarize(hipertension=sum(hipertension_s, na.rm=T),
            obesidad=sum(obesidad_s, na.rm=T),
            tabaquismo=sum(tabaquismo_s, na.rm=T),
            diabetes=sum(diabetes_s, na.rm=T))%>%
  na.omit()%>%
  pivot_longer(names_to="enfermedad", values_to="total", cols=5:8)%>%
  group_by(sector, order, grupo_edad, enfermedad)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=(total/den)*100)%>%
  filter(muerto==1)%>%
  filter(grupo_edad!="Menores de 18 años")%>%
  mutate(enfermedad=gsub("hipertension", "Hipertensión", enfermedad),
         enfermedad=gsub("diabetes", "Diabetes", enfermedad),
         enfermedad=gsub("obesidad", "Obesidad", enfermedad),
         enfermedad=gsub("tabaquismo", "Tabaquismo", enfermedad))

ggplot(tempo, aes(x=reorder(sector, order), y=enfermedad, fill=per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#fffffc", high="#023e8a")+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según su perfil y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 
       \n *Sólo se consideran personas con una comorbilidad \n**Sólo se consideran pacientes que ya dieron positivo",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(round(per, 2),"%")), size=5, hjust=.5, vjust=.5,
            color="black", 
            family = "Barlow Condensed")+
  tema+
  facet_wrap(~grupo_edad)
ggsave(files$mcomorb1_s, width=16, height=8)
ggsave(files$mcomorb1_j, width=16, height=8)


