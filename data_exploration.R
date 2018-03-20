
# Load data ---------------------------------------------------------------

load(file = "health.RData")

library(tidyverse)
library(lubridate)

str(comp_death_state)
summary(comp_death_state)
unique(comp_death_state$Measure.Name)


# Start with complications with infections

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

infections_hosp_tidy <- infections_hosp_tidy %>% select(hospital.name, state, zip.code, observed, amount)

### Getting long/lat for hospitals from zipcode

library(zipcode)

infections_hosp_tidy$zip.code <- clean.zipcodes(infections_hosp_tidy$zip.code)

data("zipcode")

infections_hosp_tidy <- infections_hosp_tidy %>% left_join(zipcode[,c("zip", "latitude", "longitude")], by = c("zip.code" = "zip"))

test <- infections_hosp_tidy %>% group_by(state, observed) %>% summarise(n = sum(amount)) %>% 
        
        ggplot(aes(reorder(state, n), n, fill = observed)) + geom_col() + coord_flip()


test <- infections_hosp_tidy %>% filter(state == "AK") %>% group_by(hospital.name, observed, latitude, longitude) %>% 
        summarise(sum = sum(amount)) %>% filter(observed == "c_diff_observed")

infections_hosp_tidy %>% filter(state == "IN" | state == "AL") %>% mutate(state = as.factor(state)) %>% 
        filter(observed == "c_diff_observed") %>% group_by(hospital.name, state, observed) %>% 
        summarise(sum = sum(amount)) %>% ggplot(aes(hospital.name, sum - mean(sum))) + geom_col() + facet_grid(state~.) + 
        coord_flip() + ylab("test")

library(leaflet)

"http://www.clker.com/cliparts/I/X/g/L/q/2/yellow-neutral-face-th.png"

neutral <- makeIcon(
        iconUrl = "http://www.clker.com/cliparts/I/X/g/L/q/2/yellow-neutral-face-th.png",
        iconWidth = 38, iconHeight = 95,
        iconAnchorX = 22, iconAnchorY = 94

        )

#create a pop up (onClick)
popup <- paste0("<strong>Name: </strong>", test$hospital.name, "<br>",
                        "<strong>Incidences: </strong>", test$sum)

colors <- case_when(test$sum <= (mean(test$sum) - 2) ~ "green",
                             test$sum == mean(test$sum) - 1 | mean(test$sum) + 1 ~ "yellow",
                             test$sum >= (mean(test$sum) + 2) ~ "red")

#create leaflet map
map1 <- leaflet(test) %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-98.35, 39.7,
                zoom = 2) %>% 
        addCircleMarkers(radius = 6, color = colors, group = "observed", popup = popup)

        
map1
