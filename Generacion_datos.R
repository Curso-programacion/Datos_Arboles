library(tidyverse)
library(BIEN)
data(mtcars)

family_vector<-c("Pinaceae", 
  "Araucariaceae","Nothofagaceae"
  )


Data <- BIEN_trait_family(family_vector)
Data <- Data %>% dplyr::select(scrubbed_species_binomial, scrubbed_family, trait_name, trait_value, id, unit) %>% 
  dplyr::filter(trait_name %in% c("whole plant height", "diameter at breast height (1.3 m)")) %>% 
  mutate(Pair = paste(trait_name, unit)) %>% mutate(trait_value = as.numeric(trait_value)) %>% 
  arrange(id) %>% 
  dplyr::filter(!is.na(trait_value))

Data <- Data %>% 
  group_by(trait_name, scrubbed_species_binomial,scrubbed_family) %>% 
  summarise(value = mean(trait_value))  %>%  
  mutate(trait_name = ifelse(trait_name == "whole plant height", "Altura", "DAP")) %>% 
  pivot_wider(names_from = trait_name, values_from = value) 

Data <- Data %>% rename(Especie = scrubbed_species_binomial, Familia = scrubbed_family)

Data <- Data %>% dplyr::filter(DAP < 2000, !is.na(DAP), !is.na(Altura))

saveRDS(Data, "Data.rds")

ggplot(Data, aes(x = Altura, DAP)) + geom_point(aes(color = scrubbed_family)) + geom_smooth(method = "lm", aes(fill =scrubbed_family))
