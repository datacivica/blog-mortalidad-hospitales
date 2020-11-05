#
# Author: Georgina Jiménez
# Maintainers: Georgina Jiménez, OE
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------
# blog-mortalidad-hospitales/grafs/src/grafs.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, here)

files <- list(input = here("clean-data/output/covid.rds"), 
              sexo1 = here("grafs/output/sexo1."),
              paciente = here("grafs/output/paciente."),
              comorb = here("grafs/output/comorb."),
              edad1 = here("grafs/output/edades1."),
              msexo_edad = here("grafs/output/msexo_edad."),
              mpaciente = here("grafs/output/mpaciente."),
              mcomorb = here("grafs/output/mcomorb."),
              mcomorb1 = here("grafs/output/mcomorb1."),
              theme_setup = here("grafs/src/theme-setup.R")
              )

source(files$theme_setup)

covid <- readRDS(files$input) 

# ==== Sexo y tipo de hospital
covid %>% 
  group_by(sexo, sector) %>% 
  summarise(tot_sexo_sector = n()) %>% 
  ungroup() %>%
  group_by(sector) %>% 
  mutate(tot_sector = sum(tot_sexo_sector),
         pct_sexo_sector = round((tot_sexo_sector/tot_sector)*100, 1)) %>%
  ungroup() %>% 
  select(-c(tot_sexo_sector, tot_sector)) %>% 
  pivot_wider(id_cols = sector, names_from = sexo, values_from = pct_sexo_sector) %>% 
  mutate(diff = Hombre - Mujer, 
         sector = fct_reorder(sector, diff)) %>%
  select(-diff) %>% 
  pivot_longer(cols = -sector, names_to = "sexo", values_to = "pct_sexo_sector") %>% 
  {
    ggplot(data = ., aes(x = sector, y = pct_sexo_sector, fill = sexo)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste0(pct_sexo_sector, "%")),
                position=position_dodge(width=1), size=4, color="black", vjust=-1)+
      labs(title="¿Qué sexo tienen las personas con COVID19 que atiende cada hospital?",
           caption="Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx\nActualizada al 24 de octubre de 2020\n*Sólo se consideran casos que ya dieron positivo", 
           x = "", y = "Porcentaje", fill = "Sexo") +
      tema +
      scale_fill_manual(values = pal_6)+
      ylim(c(0, max(.$pct_sexo_sector)+5))
  }
  
walk(devices, ~ ggsave(filename = file.path(paste0(files$sexo1, .x)),
                       device = .x, width = 16, height = 8))

# ==== Tipo de paciente
covid %>% 
  group_by(paciente, sector) %>% 
  summarise(tot_pac_sector = n()) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(tot_sector = sum(tot_pac_sector),
         pct_pac_sector = round((tot_pac_sector/tot_sector)*100, 1)) %>% 
  ungroup() %>% 
  select(-c(tot_pac_sector, tot_sector)) %>% 
  pivot_wider(id_cols = sector, names_from = paciente, values_from = pct_pac_sector) %>% 
  mutate(sector = fct_reorder(sector, Ambulatorio)) %>% 
  pivot_longer(cols = -sector, names_to = "paciente", values_to = "pct_pac_sector") %>% 
  {
    ggplot(data = ., aes(x = sector, y = pct_pac_sector, fill = fct_rev(paciente))) +
      geom_col(position="dodge") +
      geom_text(aes(label=paste0(pct_pac_sector, "%")),
                position = position_dodge(1), size=4, color="black", hjust=-.2)+
      labs(title="¿Qué tipo de pacientes atiende cada tipo de hospital?",
           subtitle="Porcentaje de total de pacientes",
           caption="Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx\nActualizada al 24 de octubre de 2020\n*Sólo se consideran pacientes que dieron positivo", 
           x="", y="Porcentaje", fill = "Tipo de paciente") +
      tema +
      scale_fill_manual(values = pal_6)+
      coord_flip()+
      ylim(c(0,100))
  }

walk(devices, ~ ggsave(filename = file.path(paste0(files$paciente, .x)),
                       device = .x, width = 16, height = 8))

# ==== Edades
covid %>% 
  group_by(sector, grupo_edad) %>% 
  summarise(tot_age_sector = n()) %>% 
  ungroup() %>% 
  group_by(sector) %>% 
  mutate(tot_sector = sum(tot_age_sector),
         pct_age_sector = round((tot_age_sector/tot_sector)*100, 1)) %>% 
  ungroup() %>% 
  select(-c(tot_age_sector, tot_sector)) %>% 
  pivot_wider(id_cols = sector, names_from = grupo_edad, values_from = pct_age_sector) %>% 
  mutate(sector = fct_reorder(sector, -`Más de 60 años`)) %>% 
  pivot_longer(cols = -sector, names_to = "grupo_edad", values_to = "pct_age_sector") %>% 
  mutate(grupo_edad = factor(grupo_edad, levels = c("Menores de 18 años",
                                                    "De 18 a 29 años",
                                                    "De 30 a 44 años",
                                                    "De 45 a 60 años",
                                                    "Más de 60 años"))) %>% 
  {
    ggplot(data = ., aes(x = pct_age_sector, y = sector,  fill = fct_rev(grupo_edad))) +
      geom_col() +
      geom_text(aes(label=paste0(pct_age_sector, "%")),
                position=position_stack(vjust=0.5), size=3, color="white")+
      labs(title="¿Qué edad tienen las personas con COVID19 que atiende cada hospital?",
           caption="Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx\nActualizada al 24 de octubre de 2020\n*Sólo se consideran pacientes que ya dieron positivo", 
           x="", y="Porcentaje", fill = "Grupo de edad") +
      tema +
      scale_fill_manual(values = pal_6) +
      guides(fill = guide_legend(reverse = TRUE))
  }
  
walk(devices, ~ ggsave(filename = file.path(paste0(files$edad1, .x)),
                       device = .x, width = 16, height = 8))

# === Comorbilidades
covid%>%
  group_by(sector)%>%
  summarize(obesidad=sum(obesidad==1),
            diabetes=sum(diabetes==1),
            tabaquismo=sum(tabaquismo==1),
            hipertension=sum(hipertension==1),
            total=n())%>%
  na.omit()%>%
  ungroup()%>%
  mutate(obesidad=round((obesidad/total)*100, 1),
         diabetes=round((diabetes/total)*100,1),
         tabaquismo=round((tabaquismo/total)*100,1),
         hipertension=round((hipertension/total)*100, 1))%>%
  ungroup()%>%
  pivot_longer(cols = -c(sector, total), names_to = "enfermedad", values_to = "porcentaje")%>%
  mutate(enfermedad=gsub("obesidad", "Obesidad", enfermedad),
         enfermedad=gsub("diabetes", "Diabetes", enfermedad),
         enfermedad=gsub("tabaquismo", "Tabaquismo", enfermedad),
         enfermedad=gsub("hipertension", "Hipertensión", enfermedad))%>%
  {
   ggplot(data=., aes(x = sector, y=porcentaje, fill = enfermedad)) +
   geom_bar(stat="identity", position="dodge") +
   geom_text(aes(label=paste0(porcentaje, "%")),
            position = position_dodge(1), size=5.5, color="black", hjust=.25, vjust=-1,
            family = "Barlow Condensed")+
  labs(title="¿Qué porcentaje de los pacientes que atiende cada hospital tienen...?",
       subtitle="Porcentaje de total de pacientes atendidos por COVID19",
       caption="\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n *Los porcentajes no suman 100% porque una persona puede tener más de una comorbilidad y aquí no aparecen quienes no tienen comorbilidades \n**Sólo se consideran pacientes que ya dieron positivo", 
       x="\n", y="\n Porcentaje \n", fill = "") +
  tema +
  scale_fill_manual(values = pal_6)+
  ylim(c(0,max(.$porcentaje)+5))
  }   


walk(devices, ~ ggsave(filename = file.path(paste0(files$comorb, .x)),
                       device = .x, width = 16, height = 8))


#### Sexo y edad ####
  covid%>%
  group_by(sector, sexo, grupo_edad, muerto)%>%
  summarize(total=n())%>%
  ungroup()%>%
  group_by(sector, sexo, grupo_edad)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=round((total/den)*100, 1))%>%
  na.omit()%>%
  filter(muerto==1)%>%
  mutate(order_edad=as.numeric(grupo_edad))%>%
  {   
  ggplot(data=., aes(x=sector, y=reorder(grupo_edad, -order_edad), fill=per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#fffffc", high="#023e8a")+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según su sexo, edad y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n*Sólo se consideran pacientes que ya dieron positivo",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(per, "%")), size=5, hjust=.5, vjust=.5, color="black", 
            family = "Barlow Condensed")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top")+
  facet_wrap(~sexo)
  }

walk(devices, ~ ggsave(filename = file.path(paste0(files$msexo_edad, .x)),
                       device = .x, width = 16, height = 8))

#================= Tipos de pacientes
  covid%>%
  group_by(sector, paciente, muerto)%>%
  summarize(total=n())%>%
  ungroup()%>%
  group_by(sector, paciente)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=round((total/den)*100, 1))%>%
  na.omit()%>%
  filter(muerto==1)%>%
  
  {
  ggplot(data=., aes(x=sector, y=paciente, fill=per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#fffffc", high="#023e8a")+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según el tipo de paciente y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 \n**Sólo se consideran pacientes que ya dieron positivo",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(per, "%")), size=5, hjust=.5, vjust=.5, color="black", 
            family = "Barlow Condensed")+
  tema +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle=0)) +
  scale_x_discrete(position = "top")
  }

walk(devices, ~ ggsave(filename = file.path(paste0(files$mpaciente, .x)),
                       device = .x, width = 16, height = 8))


####  Comorbilidades y edad ####

covid%>%
  group_by(sector, muerto, cuatro_com, m60)%>%
  summarize(total=n())%>%
  na.omit()%>%
  group_by(sector, cuatro_com, m60)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(porcentaje=round((total/den)*100, 1))%>%
  filter(muerto==1)%>%
{
ggplot(data=., aes(x=sector, y=cuatro_com, fill=porcentaje)) +
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
}

walk(devices, ~ ggsave(filename = file.path(paste0(files$mcomorb, .x)),
                       device = .x, width = 16, height = 8))

#### Una comorb y distintass edadess ####

covid%>%
group_by(sector, muerto, grupo_edad)%>%
  summarize(hipertension=sum(hipertension_s==1),
            obesidad=sum(obesidad_s==1),
            tabaquismo=sum(tabaquismo_s==1),
            diabetes=sum(diabetes_s==1))%>%
  na.omit()%>%
  pivot_longer(names_to="enfermedad", values_to="total", cols=4:7)%>%
  group_by(sector, grupo_edad, enfermedad)%>%
  mutate(den=sum(total, na.rm=T))%>%
  ungroup()%>%
  mutate(per=round((total/den)*100, 1))%>%
  filter(muerto==1)%>%
  filter(grupo_edad!="Menores de 18 años")%>%
  mutate(enfermedad=gsub("hipertension", "Hipertensión", enfermedad),
         enfermedad=gsub("diabetes", "Diabetes", enfermedad),
         enfermedad=gsub("obesidad", "Obesidad", enfermedad),
         enfermedad=gsub("tabaquismo", "Tabaquismo", enfermedad))%>%
         {
ggplot(data=., aes(x=sector, y=enfermedad, fill=per)) +
  geom_tile(color="black") +
  scale_fill_gradient(low="#fffffc", high="#023e8a")+ 
  labs(title="¿Qué porcentaje de las personas que atienden por COVID19 han muerto?", 
       subtitle = "Según su perfil y el tipo de hospital en el que se atendieron",
       caption = "\n Fuente: Base de datos abiertos sobre casos de COVID en datos.gob.mx \n Actualizada al 24 de octubre de 2020 
       \n *Sólo se consideran personas con una comorbilidad \n**Sólo se consideran pacientes que ya dieron positivo",
       x="", y="", fill="") +
  geom_text(aes(label=paste0(per,"%")), size=5, hjust=.5, vjust=.5,
            color="black", 
            family = "Barlow Condensed")+
  tema+
  facet_wrap(~grupo_edad)
         }

walk(devices, ~ ggsave(filename = file.path(paste0(files$mcomorb1, .x)),
                       device = .x, width = 16, height = 8))

#done
