
# Load data ---------------------------------------------------------------

load(file = "health.RData")

library(tidyverse)
library(lubridate)

str(comp_death_state)
summary(comp_death_state)
unique(comp_death_state$Measure.Name)


# Do infections first

names(infections_hospital) <- tolower(names(infections_hospital))

# get infection ids

infections_hosp_sub <- infections_hospital %>% filter(measure.id == "HAI_1_SIR" | measure.id == "HAI_1_NUMERATOR" | measure.id == "HAI_2_SIR" | measure.id == "HAI_2_NUMERATOR" | 
                                       measure.id == "HAI_3_SIR"| measure.id == "HAI_3_NUMERATOR" | measure.id == "HAI_4_SIR"| 
                                       measure.id == "HAI_4_NUMERATOR" | measure.id == "HAI_5_SIR"| measure.id == "HAI_5_NUMERATOR" |
                                       measure.id == "HAI_6_SIR" | measure.id == "HAI_6_NUMERATOR")


## The data have two rows for the same infection. Should be one row with two columns: observed cases and score

infections_hosp_sub <- infections_hosp_sub %>% select(-compared.to.national, -measure.id) %>% spread(key = measure.name, value = score, fill = NA)

names(infections_hosp_sub)[12:23] <- c("c_diff_observed", "cauti_ratio", "cauti_observed", "clabsi_ratio", "clabsi_observed", 
                                       "c_diff_ratio", "mrsa_ratio", "mrsa_observed", "ssi_abdom_observed", "ssi_colon_observed",
                                       "ssi_abdom_ratio", "ssi_colon_ratio")

infections_hosp_tidy <- infections_hosp_sub %>% gather(`c_diff_observed`, `cauti_observed`, `clabsi_observed`, `mrsa_observed`, `ssi_abdom_observed`, `ssi_colon_observed`, key = "observed", value = "amount") %>% 
        gather(`c_diff_ratio`, `cauti_ratio`, `clabsi_ratio`, `mrsa_ratio`, `ssi_abdom_ratio`, `ssi_colon_ratio`, key = "ratio", value = "value")

infections_hosp_tidy$amount <- ifelse(infections_hosp_tidy$amount == "Not available", NA, infections_hosp_tidy$amount)

infections_hosp_tidy$value <- ifelse(infections_hosp_tidy$value == "Not available" | infections_hosp_tidy$value == "Not Available", NA, infections_hosp_tidy$value)

infections_hosp_tidy <- infections_hosp_tidy %>% na.omit()


str(infections_hosp_tidy)

infections_hosp_tidy <- infections_hosp_tidy %>% mutate(observed = as.factor(observed), amount = as.numeric(amount),
                                                        ratio = as.factor(ratio), value = as.numeric(value),
                                                        measure.start.date = as.Date(measure.start.date, format = "%m/%d/%Y"),
                                                        measure.end.date = as.Date(measure.end.date, format = "%m/%d/%Y"))

infections_hosp_tidy <- unique(infections_hosp_tidy)

test <- infections_hosp_tidy %>% group_by(state) %>% summarise(n = sum(amount)) %>% 
        
        ggplot(aes(reorder(state, n), n, fill = observed)) + geom_col() + coord_flip()

library(leaflet)
library(raster)

usa <- getData("GADM", country="USA", level = 1)


#create a color palette to fill the polygons
pal <- colorQuantile("Greens", NULL, n = 5)

#create a pop up (onClick)
polygon_popup <- paste0("<strong>Name: </strong>", usa$NAME_1, "<br>",
                        "<strong>Indicator: </strong>", test$n)

#create leaflet map
map1 <-  leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-98.35, 39.7,
                zoom = 4) %>% 
        addPolygons(data = usa, 
                    fillColor= ~pal(test$n),
                    fillOpacity = 0.4, 
                    weight = 2, 
                    color = "white",
                    popup = polygon_popup)
        
map1
