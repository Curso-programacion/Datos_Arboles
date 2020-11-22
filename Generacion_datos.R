library(tidyverse)
library(BIEN)

family_vector<-c("Pinaceae","Cupressaceae")


Datas <- list()

for(i in 1:length(family_vector)){
  Data <- BIEN_trait_family(family_vector[i])
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
  
  Datas[[i]] <- Data %>% dplyr::filter(DAP < 2000, !is.na(DAP), !is.na(Altura))
  saveRDS(Datas[[i]], "Data.rds")
  gc()
  message(paste(i, "of", length(family_vector), "ready!"))
}



Data <- Datas %>% reduce(bind_rows)

saveRDS(Data, "Data.rds")

ggplot(Data, aes(x = Altura, DAP)) + geom_point(aes(color = Familia)) + geom_smooth(method = "lm", aes(fill =Familia))
