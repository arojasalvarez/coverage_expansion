data <- read.csv("/Users/alfonso/Google Drive/UT/Fall 2023/Coverage Expansion US Mexico/Data work/conjunto_de_datos_enigh_ns_2020_csv/conjunto_de_datos_poblacion_enigh_2020_ns/conjunto_de_datos/conjunto_de_datos_poblacion_enigh_2020_ns.csv")
library(dplyr)

homes <- read.csv("/Users/alfonso/Google Drive/UT/Fall 2023/Coverage Expansion US Mexico/Data work/conjunto_de_datos_enigh_ns_2020_csv/conjunto_de_datos_viviendas_enigh_2020_ns/conjunto_de_datos/conjunto_de_datos_viviendas_enigh_2020_ns.csv")

homes <- homes %>%
  select(folioviv, ubica_geo) %>%
  mutate(entidad = as.character(ubica_geo)) %>%
  mutate(control = ifelse(nchar(entidad) == 4, 1, 0)) %>%
  mutate(entidad = ifelse(control == 1, substr(ubica_geo, 1, 1), substr(ubica_geo, 1, 2))) %>%
  select(folioviv, entidad) %>%
  mutate(entidad = as.numeric(entidad))

data2 <- left_join(data, homes, by = "folioviv")

#data2 <- data2 %>% 
#  filter(edad > 54) %>%
#  filter(edad < 65)

data2 <- data2 %>% select(sexo, etnia, pop_insabi, edad, entidad)

data2 <- data2 %>%
  mutate(descentralization1 = ifelse(entidad == 1 | entidad == 3 | entidad == 8 |  entidad == 5 
                                     |  entidad == 11 |  entidad == 14 |  entidad == 16 |  entidad == 28
                                     |  entidad == 10|  entidad == 19,  1, 0)) %>%
  mutate(descentralization2 = ifelse(entidad == 1 | entidad == 8 | entidad == 11 |  entidad == 14 |  entidad == 28
                                     |  entidad == 19,  1, 0)) %>%
  mutate(status = descentralization1 + descentralization2)

data2 <- data2 %>%
  mutate(insabi = ifelse(pop_insabi == 1, 1, 0)) %>%
  mutate(female = sexo -1)

table(data2$insabi)

first_table <- data2 %>%
  group_by(status) %>%
  summarize(insabi = mean(insabi, na.rm = T), mean(female), mean(edad), sample = n())

second_table <- data2 %>%
  group_by(entidad) %>%
  summarize(insabi = mean(insabi, na.rm = T), mean(female), mean(edad), sample = n(), status = mean(status))

write.csv(second_table, "second_table20202.csv")
