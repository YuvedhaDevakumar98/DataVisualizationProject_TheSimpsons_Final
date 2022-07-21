library(tidyverse)
library(tidytext)
library(topicmodels)
library(scales)
library(readr)
library(kableExtra)
library(treemapify)
library(gridExtra)
library(ggpubr)
library(ggraph)
library(ggwordcloud)
library(ggcorrplot)
library(GGally)
library(udpipe)
library(plotly)
library(hrbrthemes)


characters <- read_csv("simpsons_characters.csv")
locations <- read_csv("simpsons_locations.csv")
episodes <- read_csv("simpsons_episodes.csv")
dialogues <- read_csv("simpsons_script_lines.csv")

#Histogram to show variety of views
fig4 <- plot_ly(x = ~episodes$id, y = ~episodes$us_viewers_in_millions, type = "bar",color = ~episodes$us_viewers_in_millions, colors = "YlGnBu")
fig4 <- fig4 %>% layout(title = "Populatity of Each Episode",xaxis = list(title = "Episodes"),bargap=10,
                        yaxis = list(title = "No of Views (in millions)"))
fig4

# US Views VS IMDB Ratings
episodes$views <- episodes$views / 1000
fig5 <- plot_ly(episodes, x = ~id, y = ~imdb_rating, name = 'IMDB Ratings', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = 'cornblueflower')
fig5 <- fig5 %>% add_trace(y = ~views, name = 'US Viewers', fillcolor = 'darkgoldenrod2')
fig5 <- fig5 %>% layout(title = 'Show Popularity',
                        xaxis = list(title = "Episodes",
                                     showgrid = FALSE),
                        yaxis = list(title = "Views and Ratings",
                                     showgrid = FALSE))
fig5

#No of Viewers and IMDB Votes
episodes$imdb_votes <- episodes$imdb_votes / 100
fig6 <- plot_ly(episodes, x = ~id, y = ~us_viewers_in_millions, name = 'US Viewers in Millions', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = 'darkgoldenrod1')
fig6 <- fig6 %>% add_trace(y = ~imdb_votes, name = 'IMDB Votes', fillcolor = 'coral')
fig6 <- fig6 %>% layout(title = 'Count Factors',
                        xaxis = list(title = "Episodes",
                                     showgrid = FALSE),
                        yaxis = list(title = "Views and Votes",
                                     showgrid = FALSE))
fig6
