library(readr)
library(dplyr)
library(tidyverse)

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

states_df$us_states<-sub("(.)", "\\U\\1",us_states,perl=TRUE)
states_df$hover <- with(states_df, paste(states_df$us_states, '<br>', "Number of Mentions:", count))

write_csv(states_df, "US_States_Map.csv")