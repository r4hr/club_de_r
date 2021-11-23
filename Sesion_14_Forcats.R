## ----setup, include=FALSE---------------------------------------------------------------------
options(htmltools.dir.version = FALSE)


## ----message=FALSE, warning=FALSE, echo=FALSE-------------------------------------------------
library(funModeling)
library(gt)

gt(metadata_models)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------
library(googlesheets4)
library(gargle)
library(tidyverse)
library(scales)


## ----sysarmy, cache=TRUE, message=FALSE, warning=FALSE----------------------------------------

gs4_deauth()
options(scipen = 999) # Cambia la notación científica de los gráficos
encuesta_sysarmy <- sheets_read("1_db6zEAMvr-1GQjJb4hV-rSQfJ9w6GmezbqKJ2JJn7I", skip = 9)

sysarmy <- encuesta_sysarmy %>%
  select('Trabajo de','Me identifico', `Salario mensual BRUTO (en tu moneda local)`, 'Años de experiencia', "Cantidad de empleados", "Nivel de estudios alcanzado", "Carrera") %>%
  rename(Genero = 'Me identifico', 
         Puesto = 'Trabajo de',
         Sueldo_Bruto = `Salario mensual BRUTO (en tu moneda local)`,
         Experiencia = 'Años de experiencia',
         Empleados = 'Cantidad de empleados', 
         Estudios = "Nivel de estudios alcanzado") %>%
  filter(!is.na(Empleados),
         between(Sueldo_Bruto, 20000, 1000000),
         Genero != "Otros") %>%
  mutate(Experiencia = as.numeric(unlist(Experiencia)),
    Genero = factor(Genero),
    Puesto = factor(Puesto))


## ----bar-sys, fig.show='hide'-----------------------------------------------------------------
ggplot(sysarmy, aes(x=Puesto)) + 
  geom_bar() + 
  coord_flip()


## ----ref.label="bar-sys", echo=FALSE----------------------------------------------------------



## ----lump-infreq, fig.show='hide'-------------------------------------------------------------
ggplot(sysarmy, aes(x = fct_infreq(Estudios)))+
  geom_bar()


## ----ref.label="lump-infreq",echo=FALSE-------------------------------------------------------



## ----lump-cat, fig.show='hide'----------------------------------------------------------------
sysarmy %>% 
  select(Puesto) %>% 
  mutate(Puesto = fct_lump(Puesto, n = 10)) %>% 
  count(Puesto, sort = TRUE) %>% 
  ggplot(aes(x = Puesto, y = n)) + 
  geom_col() +
  coord_flip()


## ----ref.label="lump-cat", echo=FALSE, message=FALSE, warning=FALSE---------------------------



## ----lump-ordered,fig.show='hide'-------------------------------------------------------------
sysarmy %>% 
  select(Puesto) %>% 
  mutate(Puesto = fct_lump(Puesto, n = 10)) %>% 
  count(Puesto, sort = TRUE) %>% 
  ggplot(aes(x = fct_reorder(Puesto, n), y = n)) + #<<
  geom_col() +
  coord_flip()


## ----ref.label="lump-ordered", echo=FALSE-----------------------------------------------------



## ---------------------------------------------------------------------------------------------
sysarmy %>% 
  select(Puesto) %>% 
  mutate(Puesto = fct_lump(Puesto, 
                           prop = .05)) %>% # Selecciona las variables que superen el 5% de la muestra
  count(Puesto) 


## ---------------------------------------------------------------------------------------------
sysarmy %>% 
  select(Puesto) %>% 
  mutate(Puesto = fct_lump(Puesto, 
                           prop = .05, # Selecciona las variables que superen el 5% de la muestra
                           other_level="otros")) %>% 
  count(Puesto) 


## ---------------------------------------------------------------------------------------------
estudios <- sysarmy %>% 
  select(Estudios) %>% 
  mutate(Estudios = fct_relevel(Estudios, c("Primario", "Secundario", "Universitario","Terciario",
                                            "Posdoctorado","Posgrado", "Doctorado"))) %>% 
  count(Estudios)

estudios


## ----warning=FALSE, message=FALSE-------------------------------------------------------------
estudios <- estudios %>% 
  mutate(Estudios = fct_relevel(Estudios, "Posdoctorado", after = Inf)) %>% 
  count(Estudios)

estudios


## ----warning=FALSE, message=FALSE-------------------------------------------------------------
estudios <- estudios %>% 
  mutate(Estudios = fct_relevel(Estudios, "Universitario", after = 3)) %>% 
  count(Estudios)

estudios


## ---------------------------------------------------------------------------------------------
sysarmy %>% 
  select(Puesto) %>% 
  mutate(Puesto = fct_lump(Puesto, n = 10)) %>% 
  count(Puesto, sort = TRUE)


## ---------------------------------------------------------------------------------------------
sysarmy %>% 
  select(Puesto) %>% 
  mutate(Puesto = fct_lump(Puesto, n = 10),
         Puesto = fct_recode(Puesto, SysAdmin = "SysAdmin / DevOps / SRE",
                             QA = "QA / Tester",
                             Manager = "Manager / Director")) %>% 
  count(Puesto, sort = TRUE)

