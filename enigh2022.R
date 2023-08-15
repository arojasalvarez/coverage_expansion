data <- read.csv("/Users/alfonso/Google Drive/UT/Fall 2023/Coverage Expansion US Mexico/Data work/conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_poblacion_enigh2022_ns/conjunto_de_datos/conjunto_de_datos_poblacion_enigh2022_ns.csv")
library(dplyr)

#data <- data %>% 
#  filter(edad > 54) %>%
#  filter(edad < 65)

data2 <- data %>% select(sexo, etnia, pop_insabi, edad, entidad)

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

write.csv(second_table, "second_table2.csv")
