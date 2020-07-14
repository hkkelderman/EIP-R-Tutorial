library(readxl)
library(tidyverse)
library(xlsx)
library(purrr)
library(ggplot2)

#reshaping data
book1 <- read_csv("Gorgas Data.csv")

book2 <- book1 %>%
  gather(contaminant, concentration, 4:24)

write.csv(book2, "fixed.csv", row.names = FALSE, na ="")

#combining data
state_files <- dir(pattern = "*.xlsx")

new_files <- state_files %>%
  map(read_excel) %>%
  reduce(rbind)

#cleaning techniques
unique(new_files$unit)
unique(new_files[which(new_files$unit == "mg/L"),1:2])
new_files$unit[new_files$unit == 'mg/L'] <- 'mg/l'

sort(unique(new_files$contaminant))
unique(new_files[which(new_files$contaminant == "Fluoride, total"),1:2])
new_files$contaminant[new_files$contaminant == "Fluoride, total"] <- 'Fluoride'

sort(unique(new_files$gradient))
unique(new_files[which(new_files$gradient == "downgradient"),1:2])
new_files$gradient[new_files$gradient == "downgradient"] <- 'Downgradient'

#do is.na first
sum(is.na(new_files$concentration))

#drop missing values (show how to do ?help)
cleaned_data <- new_files %>%
  drop_na(concentration)

#filter
df_1 <- cleaned_data %>%
  filter(state == 'AL')

#group and summarise
df_2 <- cleaned_data %>%
  group_by(site) %>%
  summarise(wells = n_distinct(well.id))

# or
df_3 <- cleaned_data %>%
  group_by(site, well.id, contaminant) %>%
  summarise(max_conc = max(concentration),
            min_conc = min(concentration)) %>%
  mutate(is_larger = ifelse(max_conc > min_conc, 1, 0)) %>%
  filter(is_larger != 1) %>%
  arrange(site, contaminant)

#plotting
#plot 1
plot1 <- ggplot(df_2, aes(x=site, y=wells)) +
  geom_col(fill='orange')

plot(plot1)


#plot 2
contams <- c('Chloride', 'Boron, total')

cleaned_data %>%
  filter(site == 'Plant Gadsden',
         contaminant %in% contams) %>%
  group_by(well.id, contaminant) %>%
  summarise(max_conc = max(concentration)) %>%
  ggplot(aes(x=well.id, y=max_conc)) +
  geom_point(aes(col=contaminant))



