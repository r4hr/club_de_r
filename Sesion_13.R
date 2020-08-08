library(readr)
library(gt)
library(tidyverse)
library(funModeling)
library(lubridate)



hr_data <- read_csv("HRDataset_v13.csv")

hr_data$Department <- str_trim(hr_data$Department, side = "both")

hr_data <- hr_data %>% 
  filter(!is.na(DateofHire)) %>% 
  mutate(DateofHire = mdy(DateofHire),
         Department = factor(Department)) 

summary(hr_data$Department)

max(hr_data$DateofHire)

hr_data$Department <- str_trim(hr$Department, side = "both")

status(hr_data)


tabla_1 <- hr_data %>% 
  select(Department) %>% 
  filter(!is.na(Department)) %>%  # Filtramos datos nulos
  group_by(Department) %>% 
  tally(name = "count") %>% 
  arrange(count)

glimpse(tabla_1)
tabla_1

gt(tabla_1) 

# Tabla con títulos y subtítulos
gt(tabla_1) %>% 
  tab_header(
    title = "Empleados por Departamento",
    subtitle = "Año Fiscal 2017")

# Tabla con títulos y subtítulos con formato markdown
gt(tabla_1) %>% 
  tab_header(
    title = md("**Empleados por Departamento**"),
    subtitle = md("Año Fiscal *2017*"))

gt(tabla_1) %>% 
  tab_header(
    title = md("**Empleados por Departamento**"),
    subtitle = md("Año Fiscal *2017*")) %>% 
  tab_source_note(source_note = md("*Club de R para RRHH*")) %>% 
  tab_source_note(source_note = md("Origen de Datos: [Kaggle](https://www.kaggle.com/rhuebner/human-resources-data-set/data)"))


gt(tabla_1) %>% 
  tab_header(
    title = md("**Empleados por Departamento**"),
    subtitle = md("Año Fiscal *2017*")) %>% 
  tab_source_note(source_note = md("*Club de R para RRHH*")) %>% 
  tab_source_note(source_note = md("Origen de Datos: [Kaggle](https://www.kaggle.com/rhuebner/human-resources-data-set/data)")) %>% 
  tab_footnote(footnote = "Incluyen fusión año 2019",
               locations = cells_body(
                 columns = vars(Department),
                 rows = c(3,6)
               ))



gt(tabla_1) %>% 
  tab_header(title = "Empleados por área") %>% 
  tab_row_group(
    group = "Dirección",
    rows = 1) %>% 
  tab_row_group(
    group = "Áreas de Staff",
    rows = c(2,3,5)) %>% 
  tab_row_group(
    group = "Operaciones",
    rows = c(4,6))


tabla_2 <- hr_data %>% 
  select(Department, Sex) %>% 
  filter(!is.na(Sex)) %>%             # Filtramos datos nulos
  mutate(Sex = factor(Sex, levels = c("F", "M"),
                      labels = c("Female","Male"))) %>% # Emprolijamos la variable Sex
  group_by(Department, Sex) %>% 
  tally(name = "count") %>%         # Contamos las cantidades
  pivot_wider(names_from = Sex, values_from = count) # Hacemos "ancha" la tabla

tabla_2


gt(tabla_1)
