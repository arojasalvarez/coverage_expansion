us2014 <- read.csv("us2014acs.csv")
library(dplyr)

us2014 <- us2014 %>%
  rename(State = Label) %>%
  filter(State != "Estimate") %>%
  mutate(total_medicaid = medicaid_m + medicaid_w) %>%
  mutate(total_pop = Men + Women) %>%
  mutate(medicaid_per = (total_medicaid / total_pop) * 100)


us2014c <- us2014 %>% select(State, medicaid_per)

write.csv(us2014c, "us2014c.csv")
