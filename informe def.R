library(haven)
library(tidyverse)
library(weights)
library(Weighted.Desc.Stat)
library(survey)
library(srvyr)
library(diagis)
base_junio <- read_dta("C:/Users/jesus/Downloads/base_covid_junio2020_limpia_full.dta")
base_mayo <- read_dta("C:/Users/jesus/Downloads/base_covid_mayo2020_limpia_full (1).dta")

# Menores----

## Mayo

base_mayo$menores <- ifelse(base_mayo$p73_aprende>0,
                            1,0)
table(base_mayo$menores)
prop.table(xtabs(facc_h_ocup~menores,
                 data=base_mayo))*100


## Junio

table(base_junio$ninos)
prop.table(xtabs(fac_h_ocup_norm~ninos,
                 data=base_junio))*100

# Ingresos----

base_mayo %>%
  group_by(menores)%>%
  summarise(weighted.mean(p95_socio,facc_h_ocup,na.rm=TRUE),
            weighted_se(as.numeric(p95_socio),facc_h_ocup,na.rm=TRUE),
            )

base_junio %>%
  group_by(ninos) %>%
  summarise(weighted.mean(p100_socio,fac_h_ocup_norm,na.rm=TRUE),
            weighted_se(p100_socio,fac_h_ocup_norm,na.rm=TRUE))

## ComparaciÃ³n

ingresos_mayo <- base_mayo[c(223,265,271)]
ingresos_mayo$mes <- "Mayo"

ingresos_junio <- base_junio[c(99,210,233)]
ingresos_junio$mes <- "Junio"

names(ingresos_mayo)[1]<-"Ingresos_hogar"
names(ingresos_mayo)[2]<-"fac_h_ocup_norm"
names(ingresos_mayo)[3]<-"ninos"
names(ingresos_junio)[2]<-"Ingresos_hogar"

ingresos <- rbind(ingresos_mayo,ingresos_junio)
ingresos$Ingresos_hogar[ingresos$Ingresos_hogar==0]<- NA

base_junio %>%
  group_by(ninos) %>%
  summarise(weighted.mean(tam_hog,fac_h_ocup_norm),
            weighted_se(as.numeric(tam_hog),fac_h_ocup,na.rm=TRUE))

# Comparacion informalidad ----

porcentaje_mayo <- as.data.frame(prop.table(xtabs(facc_i_ocup~informal+menores,data=base_mayo),1)*100)
porcentaje_junio <- as.data.frame(prop.table(xtabs(fac_i_ocup~informal+ninos,data=base_junio),1)*100)

#Mayo

base_mayo$formalidad <- ifelse(base_mayo$informal ==0,1,0)
base_mayo_e <- as_survey(base_mayo,id=id, weights = facc_i_ocup)

informalidad_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(informal = survey_mean(informal, proportion = TRUE, vartype = "ci"),
              se = survey_var(informal,na.rm=TRUE)))

informalidad_mayo$menores <- ifelse(formalidad_mayo$menores==1,"Con menores","Sin menores")
informalidad_mayo[3:5]<-informalidad_mayo[3:5]*100
informalidad_mayo$menores <- ifelse(informalidad_mayo$menores==1,"Con menores","Sin menores")

formalidad_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(formalidad = survey_mean(formalidad, proportion = TRUE, vartype = "ci"),
              se = survey_var(formalidad,na.rm=TRUE)))
formalidad_mayo$menores <- ifelse(formalidad_mayo$menores==1,"Con menores","Sin menores")
formalidad_mayo[3:5]<-formalidad_mayo[3:5]*100

informalidad_mayo$Sector <- "Informal"
formalidad_mayo$Sector <- "Formal"

names(informalidad_mayo)[3]<-"Proporcion"
names(informalidad_mayo)[4]<-"CI_inf"
names(informalidad_mayo)[5]<-"CI_sup"
names(formalidad_mayo)[3]<-"Proporcion"
names(formalidad_mayo)[4]<-"CI_inf"
names(formalidad_mayo)[5]<-"CI_sup"

sector_mayo <- rbind(informalidad_mayo,formalidad_mayo)

sector_mayo_grafica <- sector_mayo %>%
  ggplot(aes(x=Sector,y=Proporcion))+
  facet_grid(~menores)+
  geom_bar(stat="identity",aes(fill=Sector))+
  geom_errorbar(aes(x=Sector,ymin=CI_inf, ymax=CI_sup),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(Proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15))

# Junio

base_junio$formalidad <- ifelse(base_junio$informal ==0,1,0)
base_junio_e <- base_junio %>% as_survey(id=id,weights = fac_i_ocup)

informalidad_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(informal = survey_mean(informal, proportion = TRUE, vartype = "ci"),
              se = survey_var(informal,na.rm=TRUE)))
informalidad_junio$ninos <- ifelse(informalidad_junio$ninos==1,"Con menores","Sin menores")
informalidad_junio[3:5]<-informalidad_junio[3:5]*100

formalidad_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(formalidad = survey_mean(formalidad, proportion = TRUE, vartype = "ci"),
              se = survey_var(formalidad,na.rm=TRUE)))
formalidad_junio$ninos <- ifelse(formalidad_junio$ninos==1,"Con menores","Sin menores")
formalidad_junio[3:5]<-formalidad_junio[3:5]*100

informalidad_junio$Sector <- "Informal"
formalidad_junio$Sector <- "Formal"

names(informalidad_junio)[3]<-"Proporcion"
names(informalidad_junio)[4]<-"CI_inf"
names(informalidad_junio)[5]<-"CI_sup"
names(formalidad_junio)[3]<-"Proporcion"
names(formalidad_junio)[4]<-"CI_inf"
names(formalidad_junio)[5]<-"CI_sup"

sector_junio <- rbind(informalidad_junio,formalidad_junio)

sector_junio_grafica <- sector_junio %>%
  ggplot(aes(x=Sector,y=Proporcion))+
  facet_grid(~ninos)+
  geom_bar(stat="identity",aes(fill=Sector))+
  geom_errorbar(aes(x=Sector,ymin=CI_inf, ymax=CI_sup),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(Proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15))

# Perdida de empleo ----

prop.table(xtabs(facc_i_ocup~docup+menores,data=base_mayo),1)*100
prop.table(xtabs(fac_i_ocup~docup+ninos,data=base_junio),1)*100

# Perdida de empleo durante la cuarentena ----

prop.table(xtabs(facc_h_ocup~p32_economia+menores,data=base_mayo),1)*100

# Ingreso promedio---

porcentajes_mayo <- as.data.frame(prop.table(xtabs(facc_h_ocup~p29_economia+menores,data=base_mayo),2)*100)
porcentajes_junio <- as.data.frame(prop.table(xtabs(fac_h_ocup_norm~p27_economia+ninos,data=base_junio),2)*100)

#Mayo 

base_mayo$ing_menores <- ifelse(base_mayo$p29_economia ==-1,1,0)
base_mayo$ing_iguales <- ifelse(base_mayo$p29_economia ==0,1,0)
base_mayo$ing_mayores <- ifelse(base_mayo$p29_economia ==1,1,0)
base_mayo_e <- as_survey(base_mayo,id=id, weights = facc_h_ocup)

menores_ing_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(ing_menores = survey_mean(ing_menores, proportion = TRUE, vartype = "ci"),
              se = survey_var(ing_menores,na.rm=TRUE)))
menores_ing_mayo$menores <- ifelse(menores_ing_mayo$menores==1,"Con menores","Sin menores")
menores_ing_mayo[3:5]<-menores_ing_mayo[3:5]*100

iguales_ing_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(ing_iguales = survey_mean(ing_iguales, proportion = TRUE, vartype = "ci"),
              se = survey_var(ing_iguales,na.rm=TRUE)))
iguales_ing_mayo$menores <- ifelse(iguales_ing_mayo$menores==1,"Con menores","Sin menores")
iguales_ing_mayo[3:5]<-iguales_ing_mayo[3:5]*100

mayores_ing_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(ing_mayores = survey_mean(ing_mayores, proportion = TRUE, vartype = "ci"),
              se = survey_var(ing_mayores,na.rm=TRUE)))
mayores_ing_mayo$menores <- ifelse(mayores_ing_mayo$menores==1,"Con menores","Sin menores")
mayores_ing_mayo[3:5]<-mayores_ing_mayo[3:5]*100

menores_ing_mayo$Cambio <- "Menores"
mayores_ing_mayo$Cambio <- "Mayores"
iguales_ing_mayo$Cambio <- "Iguales"

names(menores_ing_mayo)[3]<-"Proporcion"
names(menores_ing_mayo)[4]<-"CI_inf"
names(menores_ing_mayo)[5]<-"CI_sup"
names(iguales_ing_mayo)[3]<-"Proporcion"
names(iguales_ing_mayo)[4]<-"CI_inf"
names(iguales_ing_mayo)[5]<-"CI_sup"
names(mayores_ing_mayo)[3]<-"Proporcion"
names(mayores_ing_mayo)[4]<-"CI_inf"
names(mayores_ing_mayo)[5]<-"CI_sup"

cambio_mayo <- rbind(menores_ing_mayo,iguales_ing_mayo,mayores_ing_mayo)

cambio_mayo$Cambio <- factor(cambio_mayo$Cambio,levels = c("Menores", "Iguales", "Mayores"))
cambio_mayo_grafica <- cambio_mayo %>%
  ggplot(aes(x=Cambio,y=Proporcion))+
  facet_grid(~menores)+
  geom_bar(stat="identity",aes(fill=Cambio))+
  geom_errorbar(aes(x=Cambio,ymin=CI_inf, ymax=CI_sup),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(Proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15))

# Junio

base_junio$ing_menores <- ifelse(base_junio$p27_economia ==-1,1,0)
base_junio$ing_iguales <- ifelse(base_junio$p27_economia ==0,1,0)
base_junio$ing_mayores <- ifelse(base_junio$p27_economia ==1,1,0)
base_junio_e <- as_survey(base_junio,id=id, weights = fac_h_ocup)

menores_ing_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(ing_menores = survey_mean(ing_menores, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(ing_menores),na.rm=TRUE)))
menores_ing_junio$ninos <- ifelse(menores_ing_junio$ninos==1,"Con menores","Sin menores")
menores_ing_junio[3:5]<-menores_ing_junio[3:5]*100

iguales_ing_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(ing_iguales = survey_mean(ing_iguales, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(ing_iguales),na.rm=TRUE)))
iguales_ing_junio$ninos <- ifelse(iguales_ing_junio$ninos==1,"Con menores","Sin menores")
iguales_ing_junio[3:5]<-iguales_ing_junio[3:5]*100

mayores_ing_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(ing_mayores = survey_mean(ing_mayores, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(ing_mayores),na.rm=TRUE)))
mayores_ing_junio$ninos <- ifelse(mayores_ing_junio$ninos==1,"Con menores","Sin menores")
mayores_ing_junio[3:5]<-mayores_ing_junio[3:5]*100

menores_ing_junio$Cambio <- "Menores"
mayores_ing_junio$Cambio <- "mayores"
iguales_ing_junio$Cambio <- "Iguales"

names(menores_ing_junio)[3]<-"Proporcion"
names(menores_ing_junio)[4]<-"CI_inf"
names(menores_ing_junio)[5]<-"CI_sup"
names(iguales_ing_junio)[3]<-"Proporcion"
names(iguales_ing_junio)[4]<-"CI_inf"
names(iguales_ing_junio)[5]<-"CI_sup"
names(mayores_ing_junio)[3]<-"Proporcion"
names(mayores_ing_junio)[4]<-"CI_inf"
names(mayores_ing_junio)[5]<-"CI_sup"

cambio_junio <- rbind(menores_ing_junio,iguales_ing_junio,mayores_ing_junio)

cambio_junio$Cambio <- factor(cambio_junio$Cambio,levels = c("Menores", "Iguales", "mayores"))
cambio_junio_grafica <- cambio_junio %>%
  ggplot(aes(x=Cambio,y=Proporcion))+
  facet_grid(~ninos)+
  geom_bar(stat="identity",aes(fill=Cambio))+
  geom_errorbar(aes(x=Cambio,ymin=CI_inf, ymax=CI_sup),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(Proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15))

# Ingresos promedio----

base_mayo %>%
  group_by(menores) %>%
  summarise(media = weighted.mean(p95_socio,facc_h_ocup,na.rm=TRUE))

base_junio %>%
  group_by(ninos) %>%
  summarise(media = weighted.mean(p100_socio,fac_h_ocup_norm,na.rm=TRUE))

# Seguridad alimentaria----

base_mayo$seg_alim_f <- NA
base_mayo$seg_alim_f[base_mayo$seg_alim == 0 |
                       base_mayo$seg_alim2 == 0] <- 0
base_mayo$seg_alim_f[base_mayo$seg_alim == 1 |
                       base_mayo$seg_alim2 == 1] <- 1
base_mayo$seg_alim_f[base_mayo$seg_alim == 2 |
                       base_mayo$seg_alim2 == 2] <- 2
base_mayo$seg_alim_f[base_mayo$seg_alim == 3 |
                       base_mayo$seg_alim2 == 3] <- 3

seguridad_mayo <- as.data.frame(prop.table(xtabs(facc_h_ocup~menores+seg_alim_f,data=base_mayo),1)*100)
seguridad_junio <- as.data.frame(prop.table(xtabs(fac_h_ocup_norm~ninos+seg_alim,data=base_junio),1)*100)

# Grafica mayo

base_mayo$seguridad_a <- ifelse(base_mayo$seg_alim_f ==0,1,0)
base_mayo$iseguridad_a_1 <- ifelse(base_mayo$seg_alim_f ==1,1,0)
base_mayo$iseguridad_a_2 <- ifelse(base_mayo$seg_alim_f ==2,1,0)
base_mayo$iseguridad_a_3 <- ifelse(base_mayo$seg_alim_f ==3,1,0)
base_mayo_e <- as_survey(base_mayo,id=id, weights = facc_h_ocup)

seguridad_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(seguridad_a, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(seguridad_a),na.rm=TRUE)))
seguridad_mayo[3:5]<-seguridad_mayo[3:5]*100

iseguridad_a_1 <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(iseguridad_a_1, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(iseguridad_a_1),na.rm=TRUE)))
iseguridad_a_1[3:5]<-iseguridad_a_1[3:5]*100

iseguridad_a_2 <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(iseguridad_a_2, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(iseguridad_a_2),na.rm=TRUE)))
iseguridad_a_2[3:5]<-iseguridad_a_2[3:5]*100

iseguridad_a_3 <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(iseguridad_a_3, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(iseguridad_a_3),na.rm=TRUE)))
iseguridad_a_3[3:5]<-iseguridad_a_3[3:5]*100

seguridad_mayo$ia <- "Seguridad"
iseguridad_a_1$ia <- "Inseguridad leve"
iseguridad_a_2$ia <- "Inseguridad moderada"
iseguridad_a_3$ia <- "Inseguridad severa"


ia_mayo <- rbind(seguridad_mayo,iseguridad_a_1,iseguridad_a_2,iseguridad_a_3)

ia_mayo$ia <- factor(ia_mayo$ia,levels = c("Inseguridad severa", "Inseguridad moderada", "Inseguridad leve","Seguridad"))
ia_mayo$menores <- ifelse(ia_mayo$menores==1,"Con menores","Sin menores")
ia_mayo_grafica <- ia_mayo %>%
  ggplot(aes(x=ia,y=proporcion))+
  facet_grid(~menores)+
  geom_bar(stat="identity",aes(fill=ia))+
  geom_errorbar(aes(x=ia,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  xlab("Grado de inseguridad alimentaria")+
  theme(text = element_text(size=15))+
  coord_flip()

# Grafica junio

base_junio$seguridad_a <- ifelse(base_junio$seg_alim ==0,1,0)
base_junio$iseguridad_a_1 <- ifelse(base_junio$seg_alim ==1,1,0)
base_junio$iseguridad_a_2 <- ifelse(base_junio$seg_alim ==2,1,0)
base_junio$iseguridad_a_3 <- ifelse(base_junio$seg_alim ==3,1,0)
base_junio_e <- as_survey(base_junio,id=id, weights = fac_h_ocup)

seguridad_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(seguridad_a, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(seguridad_a),na.rm=TRUE)))
seguridad_junio[3:5]<-seguridad_junio[3:5]*100

iseguridad_a_1 <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(iseguridad_a_1, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(iseguridad_a_1),na.rm=TRUE)))
iseguridad_a_1[3:5]<-iseguridad_a_1[3:5]*100

iseguridad_a_2 <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(iseguridad_a_2, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(iseguridad_a_2),na.rm=TRUE)))
iseguridad_a_2[3:5]<-iseguridad_a_2[3:5]*100

iseguridad_a_3 <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(iseguridad_a_3, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(iseguridad_a_3),na.rm=TRUE)))
iseguridad_a_3[3:5]<-iseguridad_a_3[3:5]*100

seguridad_junio$ia <- "Seguridad"
iseguridad_a_1$ia <- "Inseguridad leve"
iseguridad_a_2$ia <- "Inseguridad moderada"
iseguridad_a_3$ia <- "Inseguridad severa"


ia_junio <- rbind(seguridad_junio,iseguridad_a_1,iseguridad_a_2,iseguridad_a_3)

ia_junio$ia <- factor(ia_junio$ia,levels = c("Inseguridad severa", "Inseguridad moderada", "Inseguridad leve","Seguridad"))
ia_junio$ninos <- ifelse(ia_junio$ninos==1,"Con menores","Sin menores")
ia_junio_grafica <- ia_junio %>%
  ggplot(aes(x=ia,y=proporcion))+
  facet_grid(~ninos)+
  geom_bar(stat="identity",aes(fill=ia))+
  geom_errorbar(aes(x=ia,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  xlab("Grado de inseguridad alimentaria")+
  theme(text = element_text(size=15))+
  coord_flip()

# Seguridad alimentaria simplificada----

base_mayo$ins_simp <- ifelse(base_mayo$seg_alim_f<2,0,1)
base_junio$ins_simp <- ifelse(base_junio$seg_alim<2,0,1)

# Grafica mayo

base_mayo$seguridad_a_ag <- ifelse(base_mayo$seg_alim_f<2,1,0)
base_mayo_e <- as_survey(base_mayo,id=id, weights = facc_h_ocup)

ins_simp_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(ins_simp, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(ins_simp),na.rm=TRUE)))
ins_simp_mayo[3:5]<-ins_simp_mayo[3:5]*100

seg_simp_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(seguridad_a_ag, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(seguridad_a_ag),na.rm=TRUE)))
seg_simp_mayo[3:5]<-seg_simp_mayo[3:5]*100

ins_simp_mayo$ia <- "Inseguridad moderada o severa"
seg_simp_mayo$ia <- "Seguridad o Inseguridad leve"

ia_mayo <- rbind(ins_simp_mayo,seg_simp_mayo)

ia_mayo$ia <- factor(ia_mayo$ia,levels = c("Inseguridad moderada o severa", "Seguridad o Inseguridad leve"))
ia_mayo$menores <- ifelse(ia_mayo$menores==1,"Con menores","Sin menores")
ia_mayo_grafica_s <- ia_mayo %>%
  ggplot(aes(x=ia,y=proporcion))+
  facet_grid(~menores)+
  geom_bar(stat="identity",aes(fill=ia))+
  geom_errorbar(aes(x=ia,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  xlab("Grado de inseguridad alimentaria")+
  theme(text = element_text(size=15))+
  coord_flip()

# Grafica junio

base_junio$seguridad_a_ag <- ifelse(base_junio$seg_alim<2,1,0)
base_junio_e <- as_survey(base_junio,id=id, weights = fac_h_ocup)

ins_simp_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(ins_simp, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(ins_simp),na.rm=TRUE)))
ins_simp_junio[3:5]<-ins_simp_junio[3:5]*100

seg_simp_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(seguridad_a_ag, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(seguridad_a_ag),na.rm=TRUE)))
seg_simp_junio[3:5]<-seg_simp_junio[3:5]*100

ins_simp_junio$ia <- "Inseguridad moderada o severa"
seg_simp_junio$ia <- "Seguridad o Inseguridad leve"

ia_junio <- rbind(ins_simp_junio,seg_simp_junio)

ia_junio$ia <- factor(ia_junio$ia,levels = c("Inseguridad moderada o severa", "Seguridad o Inseguridad leve"))
ia_junio$ninos <- ifelse(ia_junio$ninos==1,"Con menores","Sin menores")
ia_junio_grafica_s <- ia_junio %>%
  ggplot(aes(x=ia,y=proporcion))+
  facet_grid(~ninos)+
  geom_bar(stat="identity",aes(fill=ia))+
  geom_errorbar(aes(x=ia,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(legend.position = "none")+
  ylab("Porcentaje (%)")+
  xlab("Grado de inseguridad alimentaria")+
  theme(text = element_text(size=15))+
  coord_flip()

# Porcentaje vistas aprende en casa por nse ----

# Mayo

base_mayo$nse_simp <- NA
base_mayo$nse_simp[base_mayo$nse<3] <- "Bajo (E/D)"
base_mayo$nse_simp[base_mayo$nse>=3 &
                     base_mayo$nse<=5] <- "Medio (D+/C-/C)"
base_mayo$nse_simp[base_mayo$nse>=6] <- "Alto (C+/B/A)"
base_mayo$no_ha_visto_a <- ifelse(base_mayo$p79_aprende==0,
                                  1,0)
base_mayo_e <- as_survey(base_mayo,id=id, weights = facc_h_ocup)

aprende_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(p79_aprende, proportion = TRUE, vartype = "ci"),
              ))
aprende_mayo[3:5]<-aprende_mayo[3:5]*100

aprende_no_visto_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(no_ha_visto_a, proportion = TRUE, vartype = "ci")))
aprende_no_visto_mayo[3:5]<-aprende_no_visto_mayo[3:5]*100

aprende_mayo$Visto <- "Sí"
aprende_no_visto_mayo$Visto <- "No"

aprende_mayo_f <- rbind(aprende_mayo,aprende_no_visto_mayo)

aprende_mayo_f$nse_simp <- factor(aprende_mayo_f$nse_simp,levels = c("Bajo (E/D)", "Medio (D+/C-/C)", "Alto (C+/B/A)"))
grafica_aprende_mayo <- aprende_mayo_f %>%
  ggplot(aes(x=nse_simp,y=proporcion,fill=Visto))+
  geom_bar(position = "dodge", stat="identity")+
  theme_minimal()+
  xlab("Nivel socioeconómico")+
  ylab("Porcentaje (%)")+
  geom_errorbar(aes(x=nse_simp,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))+
  coord_flip()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())
grafica_aprende_mayo

# Junio

base_junio$nse_simp <- NA
base_junio$nse_simp[base_junio$nse<3] <- "Bajo (E/D)"
base_junio$nse_simp[base_junio$nse>=3 &
                      base_junio$nse<=5] <- "Medio (D+/C-/C)"
base_junio$nse_simp[base_junio$nse>=6] <- "Alto (C+/B/A)"
base_junio$no_ha_visto_a <- ifelse(base_junio$p66_aprende==0,
                                   1,0)
base_junio_e <- as_survey(base_junio,id=id, weights = fac_h_ocup)

aprende_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(p66_aprende, proportion = TRUE, vartype = "ci")))
aprende_junio[3:5]<-aprende_junio[3:5]*100

aprende_no_visto_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(no_ha_visto_a, proportion = TRUE, vartype = "ci")))
aprende_no_visto_junio[3:5]<-aprende_no_visto_junio[3:5]*100

aprende_junio$Visto <- "Sí"
aprende_no_visto_junio$Visto <- "No"

aprende_junio_f <- rbind(aprende_junio,aprende_no_visto_junio)

aprende_junio_f$nse_simp <- factor(aprende_junio_f$nse_simp,levels = c("Bajo (E/D)", "Medio (D+/C-/C)", "Alto (C+/B/A)"))
grafica_aprende_junio <- aprende_junio_f %>%
  ggplot(aes(x=nse_simp,y=proporcion,fill=Visto))+
  geom_bar(position = "dodge", stat="identity")+
  theme_minimal()+
  xlab("Nivel socioeconómico")+
  ylab("Porcentaje (%)")+
  geom_errorbar(aes(x=nse_simp,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))+
  coord_flip()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())
grafica_aprende_junio

# Satisfacción aprende en casa----

# Mayo

base_mayo$satisfecho <- ifelse(base_mayo$p81_aprende<=2,
                               1,0)
base_mayo$insatisfecho <- ifelse(base_mayo$p81_aprende>=3,
                                 1,0)
base_mayo_e <- as_survey(base_mayo,id=id, weights = facc_h_ocup)

aprende_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(satisfecho, proportion = TRUE, vartype = "ci")))
aprende_mayo[3:5]<-aprende_mayo[3:5]*100

aprende_in_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(insatisfecho, proportion = TRUE, vartype = "ci")))
aprende_in_mayo[3:5]<-aprende_in_mayo[3:5]*100

aprende_mayo$satisfaccion<- "Satisfecho"
aprende_in_mayo$satisfaccion<- "Insatisfecho"

aprende_mayo_f <- rbind(aprende_mayo,aprende_in_mayo)

aprende_mayo_f$nse_simp <- factor(aprende_mayo_f$nse_simp,levels = c("Bajo (E/D)", "Medio (D+/C-/C)", "Alto (C+/B/A)"))
grafica_aprende_mayo <- aprende_mayo_f %>%
  ggplot(aes(x=nse_simp,y=proporcion,fill=satisfaccion))+
  geom_bar(position = "dodge", stat="identity")+
  theme_minimal()+
  xlab("Nivel socioeconómico")+
  ylab("Porcentaje (%)")+
  geom_errorbar(aes(x=nse_simp,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))+
  coord_flip()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())
grafica_aprende_mayo


# junio

base_junio$satisfecho <- ifelse(base_junio$p68_aprende<=2,
                                1,0)
base_junio$insatisfecho <- ifelse(base_junio$p68_aprende>=3,
                                  1,0)
base_junio_e <- as_survey(base_junio,id=id, weights = fac_h_ocup)

aprende_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(satisfecho, proportion = TRUE, vartype = "ci")))
aprende_junio[3:5]<-aprende_junio[3:5]*100

aprende_in_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(nse_simp) %>%
    summarize(proporcion = survey_mean(insatisfecho, proportion = TRUE, vartype = "ci")))
aprende_in_junio[3:5]<-aprende_in_junio[3:5]*100

aprende_junio$satisfaccion<- "Satisfecho"
aprende_in_junio$satisfaccion<- "Insatisfecho"

aprende_junio_f <- rbind(aprende_junio,aprende_in_junio)

aprende_junio_f$nse_simp <- factor(aprende_junio_f$nse_simp,levels = c("Bajo (E/D)", "Medio (D+/C-/C)", "Alto (C+/B/A)"))
grafica_aprende_junio <- aprende_junio_f %>%
  ggplot(aes(x=nse_simp,y=proporcion,fill=satisfaccion))+
  geom_bar(position = "dodge", stat="identity")+
  theme_minimal()+
  xlab("Nivel socioeconómico")+
  ylab("Porcentaje (%)")+
  geom_errorbar(aes(x=nse_simp,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))+
  coord_flip()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())
grafica_aprende_junio

# Problemas niños----

problemas_mayo <- base_mayo[c(2,166:173,267)]
problemas_mayo$levantamiento <- "Mayo"

problemas_junio <- base_junio[c(1,134:141,236)]
problemas_junio$levantamiento <- "Junio"

names(problemas_mayo)[2] <- "perdida_apetito"
names(problemas_mayo)[3] <- "dolor_cabeza"
names(problemas_mayo)[4] <- "mojar_cama"
names(problemas_mayo)[5] <- "pesadillas"
names(problemas_mayo)[6] <- "no_duerme"
names(problemas_mayo)[7] <- "miedo"
names(problemas_mayo)[8] <- "agresivo"
names(problemas_mayo)[9] <- "ninguno"
names(problemas_mayo)[10] <- "factor"

names(problemas_junio)[2] <- "perdida_apetito"
names(problemas_junio)[3] <- "dolor_cabeza"
names(problemas_junio)[4] <- "mojar_cama"
names(problemas_junio)[5] <- "pesadillas"
names(problemas_junio)[6] <- "no_duerme"
names(problemas_junio)[7] <- "miedo"
names(problemas_junio)[8] <- "agresivo"
names(problemas_junio)[9] <- "ninguno"
names(problemas_junio)[10] <- "factor"

problemas <- rbind(problemas_mayo,
                   problemas_junio)

problemas <- as_survey(problemas,id=id, weights = factor)

perdida_apetito <- as.data.frame(
  problemas %>% 
    group_by(levantamiento) %>%
    summarize(proporcion = survey_mean(perdida_apetito, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(perdida_apetito),na.rm=TRUE)))
perdida_apetito[3:5]<-perdida_apetito[3:5]*100

dolor_cabeza <- as.data.frame(
  problemas %>% 
    group_by(levantamiento) %>%
    summarize(proporcion = survey_mean(dolor_cabeza, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(dolor_cabeza),na.rm=TRUE)))
dolor_cabeza[3:5]<-dolor_cabeza[3:5]*100

mojar_cama <- as.data.frame(
  problemas %>% 
    group_by(levantamiento) %>%
    summarize(proporcion = survey_mean(mojar_cama, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(mojar_cama),na.rm=TRUE)))
mojar_cama[3:5]<-mojar_cama[3:5]*100

pesadillas <- as.data.frame(
  problemas %>% 
    group_by(levantamiento) %>%
    summarize(proporcion = survey_mean(pesadillas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(pesadillas),na.rm=TRUE)))
pesadillas[3:5]<-pesadillas[3:5]*100

no_duerme <- as.data.frame(
  problemas %>% 
    group_by(levantamiento) %>%
    summarize(proporcion = survey_mean(no_duerme, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(no_duerme),na.rm=TRUE)))
no_duerme[3:5]<-no_duerme[3:5]*100

miedo <- as.data.frame(
  problemas %>% 
    group_by(levantamiento) %>%
    summarize(proporcion = survey_mean(miedo, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(miedo),na.rm=TRUE)))
miedo[3:5]<-miedo[3:5]*100

agresivo <- as.data.frame(
  problemas %>% 
    group_by(levantamiento) %>%
    summarize(proporcion = survey_mean(agresivo, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(agresivo),na.rm=TRUE)))
agresivo[3:5]<-agresivo[3:5]*100

perdida_apetito$problema <- "Ha dejado de comer o perdido el apetito"
dolor_cabeza$problema <- "Ha tenido dolor de cabeza frecuente"
mojar_cama$problema <- "Ha empezado a mojar la cama o lo hace más seguido"
pesadillas$problema <- "Ha tenido pesadillas frecuentes"
no_duerme$problema <- "No ha dormido o se despierta por la noche"
miedo$problema <- "Tiene miedos nuevos o recurrentes"
agresivo$problema <- "Ha estado agresivo o terco"

problemas_final <- rbind(perdida_apetito,
                         dolor_cabeza,
                         mojar_cama,
                         pesadillas,
                         no_duerme,
                         miedo,
                         agresivo)

problemas_final$levantamiento <- factor(problemas_final$levantamiento,levels = c("Junio", "Mayo"))
grafico_problemas <- problemas_final %>%
  ggplot(aes(x=reorder(problema,proporcion),y=proporcion,fill=levantamiento))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  geom_errorbar(aes(x=problema,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=4)+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())+
  xlab("Problemas más frecuentes entre NNA")+
  ylab("Porcentaje (%)")
grafico_problemas

# Estrategias contra la crisis----

# Mayo

deudas_tarjetas <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p86a_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p86a_programas),na.rm=TRUE)))
deudas_tarjetas[3:5]<-deudas_tarjetas[3:5]*100

dejar_pagar_servicios <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p86b_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p86b_programas),na.rm=TRUE)))
dejar_pagar_servicios[3:5]<-dejar_pagar_servicios[3:5]*100

pedir_prestado <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p86c_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p86c_programas),na.rm=TRUE)))
pedir_prestado[3:5]<-pedir_prestado[3:5]*100

empenar_vender <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p86d_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p86d_programas),na.rm=TRUE)))
empenar_vender[3:5]<-empenar_vender[3:5]*100

tarjetas <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p86e_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p86e_programas),na.rm=TRUE)))
tarjetas[3:5]<-tarjetas[3:5]*100

vender <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p86f_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p86f_programas),na.rm=TRUE)))
vender[3:5]<-vender[3:5]*100

deudas_tarjetas$estrategia <- "Dejar de pagar deudas o tarjetas de crédito"
dejar_pagar_servicios$estrategia <- "Dejar de pagar la renta o servicios"
pedir_prestado$estrategia <- "Pedir prestado a familiares o amigos"
empenar_vender$estrategia <- "Empeñar o vender objetos de valor"
tarjetas$estrategia <- "Usar tarjetas de crédito o solicitar préstamos con bancos o prestamistas"
vender$estrategia <- "Vender o realizar otras actividades para conseguir dinero"

estrategias <- rbind(deudas_tarjetas,
                     dejar_pagar_servicios,
                     pedir_prestado,
                     empenar_vender,
                     tarjetas,
                     vender)

estrategias$menores <- ifelse(estrategias$menores == 1,"Con menores","Sin menores")

grafico_estrategias_mayo <- estrategias %>%
  ggplot(aes(x=reorder(estrategia,proporcion),y=proporcion,fill=menores))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=4)+
  xlab("Estrategias para afrontar la crisis")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())+
  geom_errorbar(aes(x=estrategia,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))
grafico_estrategias_mayo

# Junio

deudas_tarjetas <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(as.numeric(p75a_programas), proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p75a_programas),na.rm=TRUE)))
deudas_tarjetas[3:5]<-deudas_tarjetas[3:5]*100

dejar_pagar_servicios <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p75b_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p75b_programas),na.rm=TRUE)))
dejar_pagar_servicios[3:5]<-dejar_pagar_servicios[3:5]*100

pedir_prestado <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p75c_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p75c_programas),na.rm=TRUE)))
pedir_prestado[3:5]<-pedir_prestado[3:5]*100

empenar_vender <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p75d_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p75d_programas),na.rm=TRUE)))
empenar_vender[3:5]<-empenar_vender[3:5]*100

tarjetas <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p75e_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p75e_programas),na.rm=TRUE)))
tarjetas[3:5]<-tarjetas[3:5]*100

vender <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p75f_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p75f_programas),na.rm=TRUE)))
vender[3:5]<-vender[3:5]*100

deudas_tarjetas$estrategia <- "Dejar de pagar deudas o tarjetas de crédito"
dejar_pagar_servicios$estrategia <- "Dejar de pagar la renta o servicios"
pedir_prestado$estrategia <- "Pedir prestado a familiares o amigos"
empenar_vender$estrategia <- "Empeñar o vender objetos de valor"
tarjetas$estrategia <- "Usar tarjetas de crédito o solicitar préstamos con bancos o prestamistas"
vender$estrategia <- "Vender o realizar otras actividades para conseguir dinero"

estrategias <- rbind(deudas_tarjetas,
                     dejar_pagar_servicios,
                     pedir_prestado,
                     empenar_vender,
                     tarjetas,
                     vender)

estrategias$ninos <- ifelse(estrategias$ninos == 1,"Con ninos","Sin ninos")

grafico_estrategias_junio <- estrategias %>%
  ggplot(aes(x=reorder(estrategia,proporcion),y=proporcion,fill=ninos))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=4)+
  xlab("Estrategias para afrontar la crisis")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())+
  geom_errorbar(aes(x=estrategia,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))
grafico_estrategias_junio

# Subsidios----

# Mayo

base_mayo$recibe <- ifelse(base_mayo$p84i_programas==0,1,0)
base_mayo_e <- as_survey(base_mayo,id=id, weights = facc_h_ocup)

no_recibe_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84i_programas, proportion = TRUE, vartype = "ci")))
no_recibe_mayo[3:5]<-no_recibe_mayo[3:5]*100

recibe_mayo <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(recibe, proportion = TRUE, vartype = "ci")))
recibe_mayo[3:5]<-recibe_mayo[3:5]*100

no_recibe_mayo$subsidio <- "No recibe"
recibe_mayo$subsidio  <- "Sí recibe"

programas <- rbind(no_recibe_mayo,recibe_mayo)
programas$menores <- ifelse(programas$menores==1,"Con menores","Sin menores")

grafico_sunsidios_mayo <- programas %>%
  ggplot(aes(x=subsidio,y=proporcion,fill=menores))+
  geom_bar(stat="identity",position = "dodge")+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())+
  geom_errorbar(aes(x=subsidio,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))+
  xlab("")+
  ylab("Porcentaje (%)")
grafico_sunsidios_mayo  
  
# Junio

base_junio$recibe <- ifelse(base_junio$p75i_programas==0,1,0)
base_junio_e <- as_survey(base_junio,id=id, weights = fac_h_ocup)

no_recibe_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p75i_programas, proportion = TRUE, vartype = "ci")))
no_recibe_junio[3:5]<-no_recibe_junio[3:5]*100

recibe_junio <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(recibe, proportion = TRUE, vartype = "ci")))
recibe_junio[3:5]<-recibe_junio[3:5]*100

no_recibe_junio$subsidio <- "No recibe"
recibe_junio$subsidio  <- "Sí recibe"

programas_junio <- rbind(no_recibe_junio,recibe_junio)
programas_junio$ninos <- ifelse(programas_junio$ninos==1,"Con menores","Sin menores")

programas$levantamiento <- "Mayo"
programas_junio$levantamiento <- "Junio"
names(programas_junio)[1] <- "menores"
programas_final <- rbind(programas,
                         programas_junio)

programas_final$levantamiento <- factor(programas_final$levantamiento,levels = c("Mayo", "Junio"))
programas_final %>%
  ggplot(aes(x=subsidio,y=proporcion,fill=menores))+
  geom_bar(stat="identity",position="dodge")+
  facet_grid(~levantamiento)+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  geom_errorbar(aes(x=subsidio,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))+
  theme_minimal()+
  ylab("Porcentaje (%)")+
  xlab("")+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())
  
# Alimentación Junio----

dc_frutas_frescas <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p79a_alim, proportion = TRUE, vartype = "ci")))

dc_verduras <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p79b_alim, proportion = TRUE, vartype = "ci")))
  
dc_carne <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p79c_alim, proportion = TRUE, vartype = "ci")))

dc_leche <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p79d_alim, proportion = TRUE, vartype = "ci")))

dc_huevo <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p79e_alim, proportion = TRUE, vartype = "ci")))

dc_frijol <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(proporcion = survey_mean(p79f_alim, proportion = TRUE, vartype = "ci")))

dc_frutas_frescas$Alimento <- "Frutas frescas"
dc_verduras$Alimento <- "Verduras"
dc_carne$Alimento <- "Carnes"
dc_leche$Alimento <- "Leche, queso o yogurt"
dc_huevo$Alimento <- "Huevo"
dc_frijol$Alimento <- "Frijoles"

alimentos_dc <- rbind(dc_frutas_frescas,
                      dc_verduras,
                      dc_carne,
                      dc_leche,
                      dc_huevo,
                      dc_frijol)

alimentos_dc$ninos <- ifelse(alimentos_dc$ninos==1,"Con menores","Sin menores")

alimentos_dc$proporcion <- alimentos_dc$proporcion*100
alimentos_dc$proporcion_low <- alimentos_dc$proporcion_low*100
alimentos_dc$proporcion_upp <- alimentos_dc$proporcion_upp*100

alimentos_dc %>%
  ggplot(aes(x=reorder(Alimento,proporcion),y=proporcion,fill=ninos))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  theme_minimal()+
  xlab("")+
  ylab("Porcentaje (%)")+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=6)+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())+
  geom_errorbar(aes(x=Alimento,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))

# Alimentos diarios----

alimenots_diarios <- as.data.frame(
  base_junio_e %>% 
    group_by(ninos) %>%
    summarize(Refrescos = survey_mean(p80a_alim, vartype = "ci",na.rm=TRUE),
              Galletas = survey_mean(p80b_alim, vartype = "ci",na.rm=TRUE),
              Dulces = survey_mean(p80c_alim, vartype = "ci",na.rm=TRUE),
              Frituras = survey_mean(p80d_alim, vartype = "ci",na.rm=TRUE),
              Frutas = survey_mean(p80e_alim, vartype = "ci",na.rm=TRUE),
              Verduras = survey_mean(p80e_alim, vartype = "ci",na.rm=TRUE),
              Carnes = survey_mean(p80e_alim, vartype = "ci",na.rm=TRUE),
              Leche = survey_mean(p80f_alim, vartype = "ci",na.rm=TRUE)))

final_df <- as.data.frame(t(alimenots_diarios))
final_df

# Programas sociales desagregado ----

# Mayo
  
bienestar <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84a_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84a_programas),na.rm=TRUE)))
bienestar[3:5]<-bienestar[3:5]*100

sembrando_vida <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84b_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84b_programas),na.rm=TRUE)))
sembrando_vida[3:5]<-sembrando_vida[3:5]*100

benito_juarez <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84d_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84d_programas),na.rm=TRUE)))
benito_juarez[3:5]<-benito_juarez[3:5]*100

construyendo_futuro <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84e_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84e_programas),na.rm=TRUE)))
construyendo_futuro[3:5]<-construyendo_futuro[3:5]*100

tandas_bienestar <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84f_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84f_programas),na.rm=TRUE)))
tandas_bienestar[3:5]<-tandas_bienestar[3:5]*100

micro_bienestar <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84g_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84g_programas),na.rm=TRUE)))
micro_bienestar[3:5]<-micro_bienestar[3:5]*100

comida_escolar <- as.data.frame(
  base_mayo_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84m_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84m_programas),na.rm=TRUE)))
comida_escolar[3:5]<-comida_escolar[3:5]*100

bienestar$programa <- "Pensión para el Bienestar"
sembrando_vida$programa <- "Sembrando Vida"
benito_juarez$programa <- "Becas Benito Juárez."
construyendo_futuro$programa <- "Jóvenes Construyendo el Futuro."
tandas_bienestar$programa <- "Tandas para el Bienestar"
micro_bienestar$programa <- "Microcréditos para el Bienestar"
comida_escolar$programa <- "Desayunos o meriendas escolares"

programas_sociales <- rbind(bienestar,
                            sembrando_vida,
                            benito_juarez,
                            construyendo_futuro,
                            tandas_bienestar,
                            micro_bienestar,
                            comida_escolar)

programas_sociales$menores <- ifelse(programas_sociales$menores==1,"Con menores","Sin menores")
grafico_programas_mayo <- programas_sociales %>%
  ggplot(aes(x=reorder(programa,proporcion),y=proporcion,fill=as.factor(menores)))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=4)+
  xlab("Programas de gobierno")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())+
  geom_errorbar(aes(x=programa,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))
grafico_programas_mayo

# junio

bienestar <- as.data.frame(
  base_junio_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p75a_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p75a_programas),na.rm=TRUE)))
bienestar[3:5]<-bienestar[3:5]*100

sembrando_vida <- as.data.frame(
  base_junio_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84b_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84b_programas),na.rm=TRUE)))
sembrando_vida[3:5]<-sembrando_vida[3:5]*100

benito_juarez <- as.data.frame(
  base_junio_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84d_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84d_programas),na.rm=TRUE)))
benito_juarez[3:5]<-benito_juarez[3:5]*100

construyendo_futuro <- as.data.frame(
  base_junio_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84e_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84e_programas),na.rm=TRUE)))
construyendo_futuro[3:5]<-construyendo_futuro[3:5]*100

tandas_bienestar <- as.data.frame(
  base_junio_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84f_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84f_programas),na.rm=TRUE)))
tandas_bienestar[3:5]<-tandas_bienestar[3:5]*100

micro_bienestar <- as.data.frame(
  base_junio_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84g_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84g_programas),na.rm=TRUE)))
micro_bienestar[3:5]<-micro_bienestar[3:5]*100

comida_escolar <- as.data.frame(
  base_junio_e %>% 
    group_by(menores) %>%
    summarize(proporcion = survey_mean(p84m_programas, proportion = TRUE, vartype = "ci"),
              se = survey_var(as.numeric(p84m_programas),na.rm=TRUE)))
comida_escolar[3:5]<-comida_escolar[3:5]*100

bienestar$programa <- "Pensión para el Bienestar"
sembrando_vida$programa <- "Sembrando Vida"
benito_juarez$programa <- "Becas Benito Juárez."
construyendo_futuro$programa <- "Jóvenes Construyendo el Futuro."
tandas_bienestar$programa <- "Tandas para el Bienestar"
micro_bienestar$programa <- "Microcréditos para el Bienestar"
comida_escolar$programa <- "Desayunos o meriendas escolares"

programas_sociales <- rbind(bienestar,
                            sembrando_vida,
                            benito_juarez,
                            construyendo_futuro,
                            tandas_bienestar,
                            micro_bienestar,
                            comida_escolar)

programas_sociales$menores <- ifelse(programas_sociales$menores==1,"Con menores","Sin menores")
grafico_programas_junio <- programas_sociales %>%
  ggplot(aes(x=reorder(programa,proporcion),y=proporcion,fill=as.factor(menores)))+
  geom_bar(stat="identity",position="dodge")+
  coord_flip()+
  theme_minimal()+
  geom_text(aes(label=round(proporcion,digits=2)), position = position_dodge(0.9),size=4)+
  xlab("Programas de gobierno")+
  ylab("Porcentaje (%)")+
  theme(text = element_text(size=15),
        legend.position="top", 
        legend.title = element_blank())+
  geom_errorbar(aes(x=programa,ymin=proporcion_low, ymax=proporcion_upp),width = 0.5,position=position_dodge(.9))
grafico_programas_junio



