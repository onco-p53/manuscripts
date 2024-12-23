# ================================
# Title: Pseudopithomyces clean code for manuscript
# Author: Bevan Weir
# Date: 20 Dec 2024
# Description: This script contains the R code for the manuscript:
# Global diversity analysis of plant-associated Pseudopithomyces fungi reveals
# a new species producing the toxin associated with facial eczema in livestock: 
# Pseudopithomyces toxicarius sp. nov.
# Source: https://github.com/onco-p53/manuscripts

# ================================

# load libraries ---------------------------------------------------------

library(tidyverse)
library(RColorBrewer)
library(sf)
library(ggspatial)
library(lubridate)
library(janitor)
library(readxl)
library(skimr)
library(ggnewscale)

#import files and select columns
AGR.df <- read_csv("aug23-cords.csv") |> 
  select(AccessionNumber, CurrentNamePart, DecimalLat, DecimalLong) |>
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pseudopithomyces", "Pse.")) |> 
  mutate(Collection = "AGR")

ICMP_NZ.df <- read_csv("pseudopithomyces_ICMP.csv") |> 
  filter(Country == "New Zealand") |> 
  filter(str_detect(CurrentNamePart, "^Pseudopitho")) |> 
  select(AccessionNumber, CurrentNamePart, DecimalLat, DecimalLong) |>
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pseudopithomyces", "Pse.")) |> 
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pse. sp. nov. NZ", "Pse. toxicarius")) |> 
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pse. sp. 'gladiolus NZ'", "Pse. xp. 'gladiolus NZ'")) |> 
  filter(str_detect(CurrentNamePart, "Pse. toxicarius|Pse. chartarum|Pse. palmicola|Pse. xp. 'gladiolus NZ'")) |>
  mutate(Collection = "ICMP")

ICMP_all.df <- read_csv("pseudopithomyces_ICMP.csv") |> 
  filter(str_detect(CurrentNamePart, "^Pseudopitho")) |> 
  select(AccessionNumber, CurrentNamePart, DecimalLat, DecimalLong) |>
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pseudopithomyces", "Pse.")) |> 
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pse. sp. nov. NZ", "Pse. toxicarius")) |> 
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pse. sp. 'gladiolus NZ'", "Pse. gp. 'gladiolus NZ'")) |>
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pse. sp. 'ziziphus AU'", "Pse. zp. 'ziziphus AU'")) |>
  mutate(CurrentNamePart = str_replace(CurrentNamePart, "Pse. pandanicola", "Pse. palmicola")) |>
  mutate(Collection = "ICMP")

# all images should be 18 cm wide

#colours

ten_colours <- c("#332288", "#66C2A5", "#DDCC77", "#E78AC3", "#999933", "#FC8D62", "#882255", "#AA4499", "#8DA0CB", "#117733")

#============ World map - Figure 1 ================

data_sf <- ICMP_all.df |> 
  filter(!is.na(DecimalLat)) |>
  st_as_sf(coords = c("DecimalLong", "DecimalLat")) |>
  st_set_crs(4326)

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

#Plotting - takes a second to execute
ggplot() +
  geom_sf(data = world, fill = 'white') +
  geom_sf(data = data_sf, aes(colour = CurrentNamePart),
          size = 1.5, alpha = 0.8, show.legend = TRUE) +
#  geom_sf(data = data_sf |> dplyr::filter(CurrentNamePart == "Pse. cynodontis"), aes(colour = CurrentNamePart),
#          size = 1.5, alpha = 0.8, shape = 2, show.legend = TRUE) +
  labs(colour = "Species") +
  scale_colour_manual(values=ten_colours) +
  geom_abline(slope = 0, intercept = -31.5, color = "#8DA0CB", linewidth = 0.5) + #NZ is -35.8
  geom_abline(slope = 0, intercept = -41.4, color = "#8DA0CB", linewidth = 0.5) +
  theme_minimal()
ggsave(file='./outputs2/FIG_1_world-map.png', width=9, bg="#FFFFFF") #ratio is 18:9
ggsave(file='./outputs2/FIG_1_world-map.svg', width=9)



#============ New Zealand map - Figure 2 ================

#merge that data
combined.df <- bind_rows(AGR.df, ICMP_NZ.df)

# Loading in data 
#Reading in a NZ specific map
nz.sf <- st_read(dsn = "./data/nz-coastlines-topo-150k/nz-coastlines-topo-150k.shp", quiet = FALSE) %>%
  st_transform(2193) #Setting map projection - NZGD2000

#Transforming to an SF object
rhizoNZ.sf <- combined.df %>%
  filter(!is.na(DecimalLat)) %>% #Removing missing obs as sf doesn't play with these
  st_as_sf(coords = c("DecimalLong", "DecimalLat")) %>% #Defining what the coord columns are
  st_set_crs(4326) %>% #Telling sf it is in WSG84 projection
  st_transform(2193) %>% #Changing it to NZGD2000 to match coastline polygon
  st_crop(st_bbox(nz.sf)) #Cropping out points that are outside the coastline polygons bounding box (e.g. not NZ)

#Plotting - takes a second to execute
ggplot() +
  geom_sf(data = nz.sf) +
  geom_sf(data = rhizoNZ.sf, aes(colour = CurrentNamePart),
          size = 1.5, alpha = 0.5, show.legend = TRUE) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  labs(colour = "Species") +
  scale_color_brewer(palette = "Set2") +
  geom_hline(yintercept = 5414907, color = "#8DA0CB", linewidth = 0.5) + #NZTM intercept
  theme_minimal()
#this is too slow so need to build in a delay
ggsave(file='./outputs2/FIG_2_NZ-map.png', units=c("cm"), width=18, bg="#FFFFFF")
ggsave(file='./outputs2/FIG_2_NZ-map.svg', units=c("cm"), width=18,)

#============Cardinal temp 3 species================

#load cardinal data

cardinal_path <- "Cardinal data Pseudopithomyces 2nd.xlsx"

cardinal2.df <- cardinal_path |>
  excel_sheets() |>
  set_names() |>
  map_dfr(read_excel, path = cardinal_path) |>
  clean_names() |>
  select(icmp, species, temperature_c, value_diameter_mm)

glimpse(cardinal2.df)



#quick check of points
ggplot(cardinal2.df, aes(temperature_c, value_diameter_mm)) +
  geom_point(aes(colour = species))

#create a new column that divides by six to get a per day value
cardinal2.df <- cardinal2.df |> 
  mutate(value6d = value_diameter_mm/6)

cardinal3.df <- cardinal2.df |> 
  mutate(species = str_replace(species, "P. pandanicola", "Pse. palmicola")) |> 
  mutate(species = str_replace(species, "P. sp.nov", "Pse. toxicarius")) |> 
  mutate(species = str_replace(species, "P. angolensis", "Pse. angolensis")) |>
  mutate(species = str_replace(species, "P. palmicola", "Pse. palmicola")) |>
  mutate(species = str_replace(species, "P. maydicus", "Pse. maydicus")) |>
  mutate(species = str_replace(species, "P. chartarum", "Pse. chartarum"))

cardinal4.df <- cardinal3.df |> 
  filter(species == "Pse. toxicarius" | species == "Pse. palmicola" | species == "Pse. chartarum" )

#cardinal temperature graph facet
ggplot() +
  geom_point(data = cardinal4.df, 
             mapping = aes(x = temperature_c, y = value6d, colour = species),
             size = 1,
             show.legend = FALSE) +
  geom_smooth(data = cardinal4.df,
              mapping = aes(x = temperature_c, y  = value6d, colour = species),
              method = "loess",
              span = 0.50,
              linewidth = 1,
              show.legend = FALSE) +
  geom_vline(xintercept=24, linetype=4, color="red") +
  geom_hline(yintercept=7.6, linetype=4, color="blue") +
  scale_x_continuous(breaks=seq(0,38,4)) + #edit this for axis ticks
  #scale_colour_manual(values=cbPalette4)+
  ylim(0,10) +
  labs(x = "Temperature (°C)", y = "Radial growth (mm/day)") +
  #facet_grid(rows = vars(species)) +
  facet_wrap(vars(species)) +
  scale_color_brewer(palette = "Set2") +
  theme_bw()
ggsave(file='./outputs2/FIG_5_cardinal-temp-facet.png', width=18, height=10.8, units="cm")
ggsave(file='./outputs2/FIG_5_cardinal-temp-facet.svg', width=18, height=10.8, units="cm")




#============Cardinal temp all data================

#load cardinal data

cardinal_path <- "Cardinal data Pseudopithomyces 2nd.xlsx"

cardinal2.df <- cardinal_path |>
  excel_sheets() |>
  set_names() |>
  map_dfr(read_excel, path = cardinal_path) |>
  clean_names() |>
  select(icmp, species, temperature_c, value_diameter_mm)

glimpse(cardinal2.df)

#load spore data

spore_path <- "Pithomyces spore measurement.xlsx"

spores.df <- spore_path |>
  excel_sheets() |>
  set_names() |>
  map_dfr(read_excel, path = spore_path) |>
  clean_names() |>
  rename(µm = mm) |>
  select(icmp, species, direction, µm) |>
  mutate(species = str_replace(species, "P.palmicola", "Pse. palmicola")) |>
  mutate(species = str_replace(species, "Pse. sp. nov.", "Pse. toxicarius")) |>
  mutate(species = str_replace(species, "P.maydicus", "Pse. maydicus")) |>
  mutate(species = str_replace(species, "P.chartarum", "Pse. chartarum"))

glimpse(spores.df)
skim(spores.df)

#quick check of points
ggplot(cardinal2.df, aes(temperature_c, value_diameter_mm)) +
  geom_point(aes(colour = species))

#create a new column that divides by six to get a per day value
cardinal2.df <- cardinal2.df |> 
  mutate(value6d = value_diameter_mm/6)

cardinal3.df <- cardinal2.df |> 
  mutate(species = str_replace(species, "P. pandanicola", "Pse. palmicola")) |> 
  mutate(species = str_replace(species, "P. sp.nov", "Pse. toxicarius")) |> 
  mutate(species = str_replace(species, "P. angolensis", "Pse. angolensis")) |>
  mutate(species = str_replace(species, "P. palmicola", "Pse. palmicola")) |>
  mutate(species = str_replace(species, "P. maydicus", "Pse. maydicus")) |>
  mutate(species = str_replace(species, "P. chartarum", "Pse. chartarum"))

#cardinal temperature graph facet
ggplot() +
  geom_point(data = cardinal3.df, 
             mapping = aes(x = temperature_c, y = value6d, colour = "gray"),
             size = 1,
             show.legend = FALSE) +
  geom_smooth(data = cardinal3.df,
              mapping = aes(x = temperature_c, y  = value6d, colour = "gray"),
              method = "loess",
              span = 0.50,
              linewidth = 1,
              show.legend = FALSE) +
  geom_vline(xintercept=24, linetype=4, color="red") +
  geom_hline(yintercept=7.6, linetype=4, color="blue") +
  scale_x_continuous(breaks=seq(0,38,4)) + #edit this for axis ticks
  #scale_colour_manual(values=cbPalette4)+
  ylim(0,10) +
  labs(x = "Temperature (°C)", y = "Radial growth (mm/day)") +
  #facet_grid(rows = vars(species)) +
  facet_wrap(vars(species)) +
  scale_color_brewer(palette = "Set2") +
  theme_bw()
ggsave(file='./outputs2/SUPP_FIG_5_cardinal-temp-facet.png', width=18, height=10.8, units="cm")
ggsave(file='./outputs2/SUPP_FIG_5_cardinal-temp-facet.svg', width=18, height=10.8, units="cm")


#============Spore sizes all================

#load spore data

spore_path <- "Pithomyces spore measurement.xlsx"

spores.df <- spore_path |>
  excel_sheets() |>
  set_names() |>
  map_dfr(read_excel, path = spore_path) |>
  clean_names() |>
  rename(µm = mm) |>
  select(icmp, species, direction, µm) |>
  mutate(species = str_replace(species, "P.palmicola", "Pse. palmicola")) |>
  mutate(species = str_replace(species, "Pse. sp. nov.", "Pse. toxicarius")) |>
  mutate(species = str_replace(species, "P.maydicus", "Pse. maydicus")) |>
  mutate(species = str_replace(species, "P.chartarum", "Pse. chartarum")) |> 
  mutate(species = str_replace(species, "Pse. ziziphus", "Pse. zp. 'ziziphus AU'")) |>
  mutate(species = str_replace(species, "Pse. gladiolus", "Pse. gp. 'gladiolus NZ'")) |>
  mutate(species = fct_rev(species)) #alphabetical order


glimpse(spores.df)
skim(spores.df)

#plot

ggplot(
  data = spores.df,
  mapping = aes(y = µm, x = species, fill = species)
) +
  geom_violin(show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "black", outlier.colour = NA) +
  stat_summary(fun = "median", geom = "point", fill = "white", shape = 21, size = 2.5) +
  # stat_summary(fun = "mean", colour = "red", size = 2.5, geom = "point") +
  facet_wrap(vars(direction), scales = "free") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()
ggsave(file = "./outputs2/SUPP_FIG_6_spore-violin-with bars.png", width = 18, height = 10.8, units = "cm")
ggsave(file = "./outputs2/SUPP_FIG_6_spore-violin-with bars.svg", width = 18, height = 10.8, units = "cm")

#============Spore sizes 3 species================

spores3.df <- spores.df |> 
  filter(species == "Pse. toxicarius" | species == "Pse. palmicola" | species == "Pse. chartarum" )

ggplot(
  data = spores3.df,
  mapping = aes(y = µm, x = species, fill = species)
) +
  geom_violin(show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "black", outlier.colour = NA) +
  stat_summary(fun = "median", geom = "point", fill = "white", shape = 21, size = 2.5) +
  # stat_summary(fun = "mean", colour = "red", size = 2.5, geom = "point") +
  facet_wrap(vars(direction), scales = "free") +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()
ggsave(file = "./outputs2/FIG_6_spore-violin-with bars.png", width = 18, height = 10.8, units = "cm")
ggsave(file = "./outputs2/FIG_6_spore-violin-with bars.svg", width = 18, height = 10.8, units = "cm")


#============Spore size stats================

# Calculate: min, lower quartile, upper quartile, max, average, and n
# manuscript format: (–16.4) 22.5–25.8 (–35.1) × (–10.4) 12.4–14.7 (–18) μm, av. 24.4 × 13.6 μm, n=177

spores.df |> 
  group_by(species, direction) |> 
  summarize(
    min_value = round(min(µm), 1),
    lower_quartile = round(quantile(µm, 0.25), 1),
    upper_quartile = round(quantile(µm, 0.75), 1),
    max_value = round(max(µm), 1),
    average = round(mean(µm), 1),
    n = n(),
    .groups = "drop"
  ) |> 
write_csv(file='./outputs2/spore-stats.csv')



