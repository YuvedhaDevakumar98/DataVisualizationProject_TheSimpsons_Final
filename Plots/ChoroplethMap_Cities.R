library(tidyverse)
library(tidytext)
library(scales)
library(readr)
library(plotly)
library(hrbrthemes)
library(rjson)
library(dplyr)

characters <- read_csv("simpsons_characters.csv")
locations <- read_csv("simpsons_locations.csv")
episodes <- read_csv("simpsons_episodes.csv")
dialogues <- read_csv("simpsons_script_lines.csv")


us_states <- tolower(state.name)
states_df <- data.frame(tolower(state.name),state.abb)
colnames(states_df) <- c("us_states","Postalcode")
states_df$count <- 0
for(state in us_states){
  for (sentence in dialogues$normalized_text){
    if(grepl(state,sentence)){
      states_df[states_df$us_states==state,]$count <- states_df[states_df$us_states==state,]$count + 1
    }
  }
}

states_df <- states_df %>% arrange(desc(count,us_states))
states_df$us_states<-sub("(.)", "\\U\\1",us_states,perl=TRUE)
states_df$hover <- with(states_df, paste(states_df$us_states, '<br>', "Number of Mentions:", count))

View(states_df)

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig7 <- plot_geo(states_df, locationmode = 'USA-states')
fig7 <- fig7 %>% add_trace(
  z = ~count, text = ~hover, locations = ~Postalcode,
  color = ~count, colors = 'Purples'
)
fig7 <- fig7 %>% colorbar(title = "Number of Mentions")
fig7 <- fig7 %>% layout(
  title = 'Most Mentions of US States in The Simpsons Series',
  geo = g
)

fig7


































