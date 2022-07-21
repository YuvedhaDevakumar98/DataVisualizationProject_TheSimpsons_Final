library(tidyverse)
library(treemapify)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(wesanderson)

#Read Data
characters <- read_csv("simpsons_characters.csv")
locations <- read_csv("simpsons_locations.csv")
episodes <- read_csv("simpsons_episodes.csv")
dialogues <- read_csv("simpsons_script_lines.csv")

#Data Manipulation
location_lines <- data.frame(locations = c(unique(dialogues$raw_location_text)))
location_lines <- dialogues %>% group_by(raw_location_text) %>% summarize(counts = n()) 
location_lines <- location_lines %>% arrange(desc(counts))
colnames(location_lines)[colnames(location_lines)=="raw_location_text"] <- "Simpsons_Location"
View(location_lines)

top_20 <- location_lines[c(1:20),]
  
ggplot(top_20, aes(area = counts, fill = counts, label = Simpsons_Location)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)
