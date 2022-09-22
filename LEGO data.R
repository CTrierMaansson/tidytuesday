inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
elements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/elements.csv.gz')
inventory_minifigs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_minifigs.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
minifigs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/minifigs.csv.gz')
part_categories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/part_categories.csv.gz')
part_relationships <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/part_relationships.csv.gz')
parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')
themes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')

inventories
inventory_sets
inventory_parts
inventory_minifigs
sets
themes
colors
elements
part_categories
part_relationships
parts
minifigs

themes
sets

library(dplyr)
library(ggplot2)
library(tidyr)


th <- theme(panel.background = element_rect(fill = "#FFED00"),
            title = element_text(colour = "Blue", face = "bold"))

sets %>% 
    group_by(year) %>%
    summarise(Counts = n()) %>% 
    mutate(Cumulative = cumsum(Counts)) %>%
    filter(year != 2022) %>%
    pivot_longer(- year) %>%
    ggplot(aes(x = year, y = value))+
    geom_point(size = 3, color = "#E3000B")+
    geom_line(color = "#E3000B")+
    labs(title = "LEGO sets from 1949 to 2021",
         x = "Year", 
         y = "Sets")+
    theme_classic(17)+
    th+
    facet_wrap(~name, scales = "free")


parent_themes <- themes %>% 
    filter(id %in% unique(parent_id)) %>% 
    filter(is.na(parent_id))

parent_themes

themes

sets_themes <- left_join(sets, themes, by = c("theme_id" = "id"))

big_themes <- sets_themes %>%
    mutate(parent_id = ifelse(is.na(parent_id), theme_id, parent_id)) %>%
    group_by(parent_id) %>%
    summarise(count = n()) %>% 
    arrange(desc(count))

themes[themes$id %in% big_themes$parent_id[1:15],]

