library(caret)
library(corrplot)
library(readr)
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
library(tm)
library(tidyr)
library(wordcloud)
library(ggplot2)
library(gridExtra)
library(grid)
library(leaflet)
library(randomForest)
library(plotly)
options(knitr.table.format = "html") 
options(knitr.table.format = "html") 

#Read data
dialogues = read_csv("simpsons_script_lines.csv")
episodes = read_csv("simpsons_episodes.csv")
locations = read_csv("simpsons_locations.csv")
characters = read_csv("simpsons_characters.csv")

#most lines for characters
script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
script_lines_character_df <- script_lines_character_df %>% filter(name != "") %>% group_by(name) %>%  
  summarise(nr = length(id)) %>% top_n(10,nr) %>% ungroup()%>%arrange(desc(nr))

#install.packages("plotly")
fig1 <- plot_ly(
  script_lines_character_df,
    x = ~nr, 
    y = ~name,
  name = "Most lines throughout the series",
  type = "bar",
  marker = list(color = c("darkred","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue"))
) %>%
  layout(
    title = "Most Lines thorughout the TV Series",
    yaxis = list(title = "Characters",
    categoryorder = "total ascending"),
    xaxis = list(title = "Lines")
  )
fig1

#lines per episode
main_characters <- c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
per_episode = script_lines_character_df$word_count = as.numeric(script_lines_character_df$word_count)
per_episode = script_lines_character_df %>% filter(name %in% main_characters) %>% group_by(name,episode_id) %>%  
  summarise(nr = length(id)) %>% ungroup() %>% arrange(desc(nr))

s <- split(per_episode, per_episode$name)
HomerSimpson <- s$`Homer Simpson`
MargeSimpson <- s$`Marge Simpson`
BartSimpson <- s$`Bart Simpson`
LisaSimpson <- s$`Lisa Simpson`

#Homer's Lines per episode
HS <- plot_ly(
  HomerSimpson,
  x = ~episode_id, 
  y = ~nr,
  color = ~name
)

HS <- HS %>% add_lines() %>%
  layout(title = "Main Character's Lines per Episode", legend=list(title=list(text='Characters')),
         plot_bgcolor='white', 
         xaxis = list( 
           title = "Episodes"), 
         yaxis = list( 
           title = "Number of Lines"
         ) )
HS

#Marge's Lines per episode
MS <- plot_ly(
  MargeSimpson,
  x = ~episode_id, 
  y = ~nr,
  color = ~name,
  colors ="darkolivegreen3"
)

MS <- MS %>% add_lines() %>%
  layout(title = "Marge's Lines per Episode", legend=list(title=list(text='Characters')),
         plot_bgcolor='white', 
         xaxis = list( 
           title = "Episodes"), 
         yaxis = list( 
           title = "Number of Lines"
         ) )
MS

#Bart's Lines per episode
BS <- plot_ly(
  BartSimpson,
  x = ~episode_id, 
  y = ~nr,
  color = ~name,
  colors ="cornflowerblue"
)

BS <- BS %>% add_lines() %>%
  layout(title = "Bart's Lines per Episode", legend=list(title=list(text='Characters')),
         plot_bgcolor='white', 
         xaxis = list( 
           title = "Episodes"), 
         yaxis = list( 
           title = "Number of Lines"
         ) )
BS

#Lisa's Lines per episode
LS <- plot_ly(
  LisaSimpson,
  x = ~episode_id, 
  y = ~nr,
  color = ~name,
  colors ="coral1"
)

LS <- LS %>% add_lines() %>%
  layout(title = "Lisa's Lines per Episode", legend=list(title=list(text='Characters')),
         plot_bgcolor='white', 
         xaxis = list( 
           title = "Episodes"), 
         yaxis = list( 
           title = "Number of Lines"
         ) )
LS

#lines per season
top10char <- script_lines_character_df %>% filter(name != "") %>% group_by(name) %>%  
  summarise(nr = length(id)) %>% top_n(10,nr) 
per_season <- merge(script_lines_character_df, episodes,  by.x = "episode_id",  by.y = "id")
per_season <- per_season %>% filter(name %in% top10char$name) %>% group_by(name,season) %>%  
  summarise(nr = length(id)) %>% ungroup()  
#plot
fig3 <- plot_ly(
  per_season,
  x = ~season, 
  y = ~nr,
  color = ~name
)

fig3 <- fig3 %>% add_lines() %>%
  layout(title = "Main Character's Lines per Season", 
         legend=list(title=list(text='Characters')),
         plot_bgcolor='white', 
         xaxis = list( 
           title = "Seasons"), 
         yaxis = list( 
           title = "Number of Lines"
         ) )
fig3




