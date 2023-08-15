us2021 <- read.csv("us2021acs.csv")
library(dplyr)

us2021 <- us2021 %>%
  filter(State != "Estimate") %>%
  mutate(total_medicaid = medicaid_m + medicaid_w) %>%
  mutate(total_pop = Men + Women) %>%
  mutate(medicaid_per = (total_medicaid / total_pop) * 100)

us2021c <- us2021 %>% select(State, medicaid_per)

write.csv(us2021c, "us2021c.csv")
