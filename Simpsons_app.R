library(readr)
library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(treemapify)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
library(hrbrthemes)
library(rjson)
library(tidytext)
library(scales)
library(dashboardthemes)
library(shinyWidgets)
library(wordcloud)
library(tm)
library(rsconnect)

characters <- read_csv("simpsons_characters.csv")
locations <- read_csv("simpsons_locations.csv")
episodes <- read_csv("simpsons_episodes.csv")
dialogues <- read_csv("simpsons_script_lines.csv")
states <- read_csv("US_States_Map.csv")

words_hommer_df= read_csv("words_hommer_df.csv")
words_marge_dfm = read_csv("words_marge_dfm.csv")
words_bart_dfb = read_csv("words_bart_dfb.csv")
words_lisa_df = read_csv("words_lisa_df.csv")

l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "The Simpsons Show"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "page1", icon = icon("info")),
      menuItem("Show's Popularity", tabName = "page2", icon = icon("area-chart")),
      menuItem("Characters",icon = icon("line-chart"),
               menuSubItem("Most Number of Script Lines", tabName = "page3"), 
               menuSubItem("Main Characters", tabName = "page4"),
               menuSubItem("Top 10 Characters", tabName ="page5")),
      menuItem("Springfield", tabName = "page6", icon = icon("map-o")),
      menuItem("US States Mention", tabName = "page7", icon = icon("flag-usa")),
      menuItem("Frequent Words", tabName = "page8", icon = icon("cloud")),
      menuItem("Interactive Quiz", tabName = "page9", icon = icon("question")),
      menuItem("Database", tabName = "page10", icon = icon("database"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #b8e2f2;
      }
    '))),
    
    tabItems(
      tabItem(tabName = "page1",
              tags$img(src="simpsons.jpg",width="100%", height=180),
              br(),
              br(),
              box(title = strong("An Analysis on The Simpsons Show", style = 'font-size:22px;'), solidHeader = TRUE, 
                     status = "primary", collapsible = TRUE, width = 12, 
              ##Overview
              "With 26 seasons and 600 episodes, the Simpsons is one of the longest running and most popular shows in the United States. With so many seasons, there are hundreds of minor characters and recurring characters. We will break down who speaks the most and in what context.",style = 'font-size:12px;color:black;'),
              box(title= strong("Clip", style='font-size:22px;'), solidHeader= TRUE,
                  status="primary", collapsible= TRUE, width=12, align="center",
              HTML(
                '<iframe width="65%" height="300"
                  src="https://www.youtube.com/embed/jfVBrpIhH60"
                  frameborder="0" allowfullscreen></iframe>')),
              box(title = strong("7 Interesting Facts About The Simpsons", style = 'font-size:22px;'), solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 12, 
                  ##Overview
                  "1. 'Homer At The Bat' was the first episode to defeat The Cosby Show in its original airing.",
                  br(),
                  br(),
                  "2. On Conan O'Brien's first day as part of the writing staff, a bird flew through the window and killed itself.",
                  br(),
                  br(),
                  "3. Moe's telephone number is 764-84377, which is one digit more than a usual U.S number.",
                  br(),
                  br(),
                  "4. His number actually spells out SMITHERS.",
                  br(),
                  br(),
                  "5. In the French dubbed Simpsons, Homer's 'D'oh!' is translated to 'T'oh!'",
                  br(),
                  br(),
                  "6. In the Spanish version, Homer instead says 'Ouch!'",
                  br(),
                  br(),
                  "7. Homer's email, ChunkyLover53@aol.com, is genuinely registered and users will get an automated response from him when they try and contact it.",
      
                  style = 'font-size:12px;color:black;'),
              
            
              box(title=strong("Team",style='font-size:22px;'), solidHeader=TRUE, 
                  status="primary", collapsible= TRUE, width=12,
              tags$img(src="family.jpg", width="50%", height=180, align="left"),
              br(),
              "Yuvedha Devakumar",
              br(),
              "Stephanie Flores", 
              br(),
              "Lili Owen", 
              br(),
              "Lili Rowshan",
              br(),
              "Tarek Shihadeh", align="center", style = 'font-size:16px;color:black;'
              
      )),
                  
      
      tabItem(tabName = "page2",
              box(title= strong ("Simpson's Popularity", style='font-size:22px;'), solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 6.5, 
                  
                  "Simpsons aired for 600 episodes and 26 seasons meaning it is a very popular show until date. Here is a map depicting the most popular episodes through out the show. ", style='font-size:12px;',
                  br(),
                  "It shows that the show was extremely popular from pilot episode to 180th episode with 30 million views in the US  for the 14th episode.", style='font-size:12px;',
                  br(),
                  "Here are some of the demographics to explain their popularity based on US Viewers, IMDB Ratings and the Number of Votes for each of the 600 episodes.", style='font-size:12px;'),
              
                  
              box(title= strong ("Popularity of Each Episode", style='font-size:22px;'), solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 6.5, 
                
              "Zoom and hover over to see the popularity of each episode.", style='font-size:12px;',
                  br(),
                  br(),
                  plotlyOutput("famousepisodes", height = 350)),
              box(title=strong("Show's Popularity", style='font-size:22px;'),solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 6.5,
                  plotlyOutput("viewsratings", height = 300)),
              box(title=strong("Count Factors", style='font-size:22px;'),solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 6.5,
                  plotlyOutput("viewersvotes", height = 300))
      ),
      tabItem(tabName = "page3",
              box(title=strong("Most Lines Throughout The Series", style='font-size:22px;'),solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 6.5,
                  "Have you ever wondered how many lines the characters had in the show? Obviously its HOMER!!!! He has 30,000 lines, hover over the graph to check out what the others have to say.", style='font-size:14px;',
                  br(),
                  br(),
                  plotlyOutput("mostLines", height = 500))
      ),
      tabItem(tabName = "page4",
              box(title=strong("4 Main Characters' Lines Per Episode", style='font-size:22px;'),solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE,width=15,
                  
                tabsetPanel(
                  tabPanel("Homer Simpson",box(plotlyOutput("perepisode_homer", height = 500),width = 15)),
                  tabPanel("Marge Simpson",box(plotlyOutput("perepisode_marge", height = 500),width = 15)),
                  tabPanel("Bart Simpson",box(plotlyOutput("perepisode_bart", height = 500),width = 15)),
                  tabPanel("Lisa Simpson",box(plotlyOutput("perepisode_lisa", height = 500),width = 15))
      ))),
      tabItem(tabName = "page5",
              box(title=strong("10 Main Characters' Lines Per Episode", style='font-size:22px;'),solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE,width=15,
                  
                  box(plotlyOutput("perseason", height = 500),width = 15))
      ),
      tabItem(tabName = "page6",
              box(title= strong ("Where Do Most of the Conversations Take Place?", style='font-size:22px;'), solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 15, 
                  
                  "The Simpsons Series was set in Springfield, Oregon but have you ever wondered where most of the conversations have happened? It is in Simpson's Home followed by Springfield Elementary School, Moe's Tavern and Springfield Nuclear Power Plant.", style='font-size:12px;',
            
                  box(plotOutput("TreeMap"),width =15))
      ),
      tabItem(tabName = "page7",
              box(title= strong ("How Many Times Each State is Mentioned?", style='font-size:22px;'), solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 15,
                  "Since the Simpsons are the American simpleton, US States are a regular inclusion in the show. The choropleth map above shows the most mentioned US States in the show and New York wins with 86 mentions throughout the show.", style='font-size:12px;',
                  br(),
                  br(),
                  box(plotlyOutput("USStates"),width = 15)) 
      ),
      tabItem(tabName = "page8",
              box(title= strong ("Word Clouds", style='font-size:22px;'), solidHeader = TRUE, 
                  status = "primary", collapsible = TRUE, width = 15,
                  
                tabsetPanel(
                  tabPanel("Homer Simpson",box(plotOutput("homer_lines"),width = 15)),
                  tabPanel("Marge Simpson",box(plotOutput("marge_lines", height = 500),width = 15)),
                  tabPanel("Bart Simpson",box(plotOutput("bart_lines", height = 500),width = 15)),
                  tabPanel("Lisa Simpson",box(plotOutput("lisa_lines", height = 500),width = 15))))
      
      ),
      tabItem(tabName = "page9",
              box(title=strong("Character Personality Quiz", style='font-size:22px;'),solidHeader = TRUE, 
                status = "primary", collapsible = TRUE, width = 6.5, 
                selectInput( "UserInput",  
                           "What does your favorite character say about you?", 
                           choices = c("Maggie Simpson", "Bart Simpson", "Lisa Simpson","Homer Simpson")), 
                h4(textOutput("Result"), style='font-size:14px;')),
              br(),
              br(),
              br(),
            
              h5(tags$img(src="simpson.jpg",width="100%", height="100%")),
      
      
      ),
      tabItem(tabName = "page10",
              tabsetPanel(
                tabPanel("Characters",dataTableOutput("myTable1")),
                tabPanel("Locations",dataTableOutput("myTable2")),
                tabPanel("Episodes",dataTableOutput("myTable3")),
                tabPanel("ScriptLines",dataTableOutput("myTable4"))
               )
      )
  )
)
)



server <- function(input, output, session) {
  characters <- read_csv("simpsons_characters.csv")
  locations <- read_csv("simpsons_locations.csv")
  episodes <- read_csv("simpsons_episodes.csv")
  dialogues <- read_csv("simpsons_script_lines.csv")
  
  output$plot1 = renderPlot({
    
  })
  
  output$famousepisodes = renderPlotly({
    fig4 <- plot_ly(episodes, x = ~id, y = ~us_viewers_in_millions,type = "bar",color = ~us_viewers_in_millions,colors = "YlGnBu")
    fig4 <- fig4 %>% layout(xaxis = list(title = "Episodes"),bargap=10,
                            yaxis = list(title = "No of Views (in millions)"))
  })
  
  output$viewsratings = renderPlotly({
      episodes$views <- episodes$views / 1000
      fig5 <- plot_ly(episodes, x = ~id, y = ~imdb_rating, name = 'IMDB Ratings', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = 'cornblueflower')
      fig5 <- fig5 %>% add_trace(y = ~views, name = 'US Viewers', fillcolor = 'darkgoldenrod2')
      fig5 <- fig5 %>% layout(
                              xaxis = list(title = "Episodes",
                                           showgrid = FALSE),
                              yaxis = list(title = "Views and Ratings",
                                           showgrid = FALSE))
  })
  
  output$viewersvotes = renderPlotly({
     episodes$imdb_votes <- episodes$imdb_votes / 100
     fig6 <- plot_ly(episodes, x = ~id, y = ~us_viewers_in_millions, name = 'US Viewers in Millions', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = 'darkgoldenrod1')
     fig6 <- fig6 %>% add_trace(y = ~imdb_votes, name = 'IMDB Votes', fillcolor = 'coral')
     fig6 <- fig6 %>% layout(
                             xaxis = list(title = "Episodes",
                                          showgrid = FALSE),
                             yaxis = list(title = "Views and Votes",
                                          showgrid = FALSE))
  })
  
  output$mostLines = renderPlotly({
    #most lines for characters
    script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
    script_lines_character_df <- script_lines_character_df %>% filter(name != "") %>% group_by(name) %>%  
      summarise(nr = length(id)) %>% top_n(10,nr) %>% ungroup()%>%arrange(desc(nr))
    
    fig1 <- plot_ly(
      script_lines_character_df,
      x = ~nr, 
      y = ~name,
      name = "Most lines throughout the series",
      type = "bar",
      marker = list(color = c("darkred","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue","cornflowerblue"))
    ) %>%
      layout(
        
        yaxis = list(title = "Characters",
                     categoryorder = "total ascending"),
        xaxis = list(title = "Lines")
      )
  })
  
  output$perepisode_homer = renderPlotly({
      main_characters <- c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
      script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
      per_episode = script_lines_character_df$word_count = as.numeric(script_lines_character_df$word_count)
      per_episode = script_lines_character_df %>% filter(name %in% main_characters) %>% group_by(name,episode_id) %>%  
        summarise(nr = length(id)) %>% ungroup()%>%arrange(desc(nr))

      s <- split(per_episode, per_episode$name)
      HomerSimpson <- s$`Homer Simpson`
      
        HS <- plot_ly(
          HomerSimpson,
          x = ~episode_id, 
          y = ~nr,
          color = ~name,
          colors ="pink"
        )
        
        HS <- HS %>% add_lines() %>%
          layout(legend=list(title=list(text='Characters')),
                 plot_bgcolor='white', 
                 xaxis = list( 
                   title = "Episodes"), 
                 yaxis = list( 
                   title = "Number of Lines"
                 ) )
  })
  
  output$perepisode_marge = renderPlotly({
    main_characters <- c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
    script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
    per_episode = script_lines_character_df$word_count = as.numeric(script_lines_character_df$word_count)
    per_episode = script_lines_character_df %>% filter(name %in% main_characters) %>% group_by(name,episode_id) %>%  
      summarise(nr = length(id)) %>% ungroup()%>%arrange(desc(nr))
    
    s <- split(per_episode, per_episode$name)
    MargeSimpson <- s$`Marge Simpson`

    MS <- plot_ly(
      MargeSimpson,
      x = ~episode_id, 
      y = ~nr,
      color = ~name,
      colors ="darkolivegreen3"
    )
    
    MS <- MS %>% add_lines() %>%
      layout(legend=list(title=list(text='Characters')),
             plot_bgcolor='white', 
             xaxis = list( 
               title = "Episodes"), 
             yaxis = list( 
               title = "Number of Lines"
             ) )
  })
  
  output$perepisode_bart = renderPlotly({
    main_characters <- c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
    script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
    per_episode = script_lines_character_df$word_count = as.numeric(script_lines_character_df$word_count)
    per_episode = script_lines_character_df %>% filter(name %in% main_characters) %>% group_by(name,episode_id) %>%  
      summarise(nr = length(id)) %>% ungroup()%>%arrange(desc(nr))
    
    s <- split(per_episode, per_episode$name)
    BartSimpson <- s$`Bart Simpson`
    
    BS <- plot_ly(
      BartSimpson,
      x = ~episode_id, 
      y = ~nr,
      color = ~name,
      colors ="cornflowerblue"
    )
    
    BS <- BS %>% add_lines() %>%
      layout( legend=list(title=list(text='Characters')),
             plot_bgcolor='white', 
             xaxis = list( 
               title = "Episodes"), 
             yaxis = list( 
               title = "Number of Lines"
             ) )
  })
  
  output$perepisode_lisa = renderPlotly({
    main_characters <- c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
    script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
    per_episode = script_lines_character_df$word_count = as.numeric(script_lines_character_df$word_count)
    per_episode = script_lines_character_df %>% filter(name %in% main_characters) %>% group_by(name,episode_id) %>%  
      summarise(nr = length(id)) %>% ungroup()%>%arrange(desc(nr))
    
    s <- split(per_episode, per_episode$name)
    LisaSimpson <- s$`Lisa Simpson`
    
    LS <- plot_ly(
      LisaSimpson,
      x = ~episode_id, 
      y = ~nr,
      color = ~name,
      colors ="coral1"
    )
    
    LS <- LS %>% add_lines() %>%
      layout( legend=list(title=list(text='Characters')),
             plot_bgcolor='white', 
             xaxis = list( 
               title = "Episodes"), 
             yaxis = list( 
               title = "Number of Lines"
             ) )
  })
  
  
  output$perseason = renderPlotly({
      script_lines_character_df <- merge(dialogues, characters,  by.x = "character_id",  by.y = "id")
      top10char <- script_lines_character_df %>% filter(name != "") %>% group_by(name) %>%  
        summarise(nr = length(id)) %>% top_n(10,nr) 
      per_season <- merge(script_lines_character_df, episodes,  by.x = "episode_id",  by.y = "id")
      per_season <- per_season %>% filter(name %in% top10char$name) %>% group_by(name,season) %>%  
        summarise(nr = length(id)) %>% ungroup()  

      fig3 <- plot_ly(
        per_season,
        x = ~season, 
        y = ~nr,
        color = ~name
      )
      
      fig3 <- fig3 %>% add_lines() %>%
        layout( 
               legend=list(title=list(text='Characters')),
               plot_bgcolor='white', 
               xaxis = list( 
                 title = "Seasons"), 
               yaxis = list( 
                 title = "Number of Lines"
               ) )
  })
  
  output$TreeMap = renderPlot({
    location_lines <- data.frame(locations = c(unique(dialogues$raw_location_text)))
    location_lines <- dialogues %>% group_by(raw_location_text) %>% summarize(counts = n()) 
    location_lines <- location_lines %>% arrange(desc(counts))
    colnames(location_lines)[colnames(location_lines)=="raw_location_text"] <- "Simpsons_Location"
    
    top_20 <- location_lines[c(1:20),]
    
      ggplot(top_20, aes(area = counts, fill = counts, label = Simpsons_Location)) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15)
  })

  output$USStates = renderPlotly({
    fig7 <- plot_geo(states, locationmode = 'USA-states')
    fig7 <- fig7 %>% add_trace(
      z = ~count, text = ~hover, locations = ~Postalcode,
      color = ~count, colors = 'YlOrBr'
    )
    fig7 <- fig7 %>% colorbar(title = "Number of Mentions")
    fig7 <- fig7 %>% layout(
      title = 'Most Mentions of US States in The Simpsons Series',
      geo = g
    )
  })
  
  output$homer_lines = renderPlot({
    
    wordcloud(words=words_hommer_df$word, freq= words_hommer_df$freq, min.freq =1, max.words=150, random.order=FALSE, rot.per=0.35, colors=brewer.pal(7, "Spectral"), width = 480, height = 480, margin= 0)
  }) 
  
  output$marge_lines = renderPlot({
    
    wordcloud(words=words_marge_dfm$word, freq= words_marge_dfm$freq, min.freq =1, max.words=150, random.order=FALSE, rot.per=0.35, colors=brewer.pal(7, "Spectral"), width = 480, height = 480, margin= 0)
  })
  
  output$bart_lines = renderPlot({
    
    wordcloud(words=words_bart_dfb$word, freq= words_bart_dfb$freq, min.freq =1, max.words=150, random.order=FALSE, rot.per=0.35, colors=brewer.pal(7, "Spectral"), width = 480, height = 480, margin= 0)
  })
  
  output$lisa_lines = renderPlot({
    
    wordcloud(words=words_lisa_df$word, freq= words_lisa_df$freq, min.freq =1, max.words=150, random.order=FALSE, rot.per=0.35, colors=brewer.pal(7, "Spectral"), width = 480, height = 480, margin= 0)
  })
  
  
  
  output$Result = renderText({
      CharacterQuiz =function(q.c){ 
        if (q.c == "Maggie Simpson"){
          QuizResult="You are stubborn, acquisitive, and always seem to eventually get your own way. When you don't get what you want you become cholicky or throw tantrums, which cause your family to seek to immediately mollify you."
        } else if (q.c == "Bart Simpson") {
          QuizResult= "You are most prominent character traits are your mischievousness, rebelliousness and disrespect for authority."
        } else if (q.c == "Lisa Simpson"){
          QuizResult= "You are warm, gentle, and determined. Generally humble about your intelligence, except when you are actively proving it"
        } else if(q.c == "Homer Simpson"){
          QuizResult= "You can be lazy and ignorant from time to time. However, you show great caring, love, and even bravery to those you loveand care about"
        }else{
          QuizResult=""
        }
        return(QuizResult)
      }
      output$Result <- renderText({ 
        CharacterQuiz(input$UserInput) 
      })
  })
  
  
  output$myTable1 = renderDataTable({
    return(datatable(characters,rownames=FALSE))
  })
  
  output$myTable2 = renderDataTable({
    return(datatable(locations,rownames=FALSE))
  })
  
  output$myTable3 = renderDataTable({
    return(datatable(episodes,rownames=FALSE))
  })
  
  output$myTable4 = renderDataTable({
    return(datatable(dialogues,rownames=FALSE))
  })
  
  
}

shinyApp(ui = ui, server = server)