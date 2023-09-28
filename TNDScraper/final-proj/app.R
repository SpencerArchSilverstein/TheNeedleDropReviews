# load packages and data
library(shiny)
library(tidyverse)
library(DT)
library(plotly)

## Data Wrangling ----
# load my csv
tndd <- read.csv("data/TNDScraper.csv") %>% 
  mutate(genre = str_to_title(tolower(genre)))

# list of genres
list_of_gen <- c("Hip Hop", "Rap", "Trap", "Pop", "Rock", 
                 "R&B", "Metal", "Latin", "Soul", "Funk", 
                 "Punk", "Jazz", "Country", "Indie", "Alt", "Prog")

# list of corresponding score colors
score_colors <- c("0" = "black", "1" = "#3F0000", "2" = "#8B0000", "3" = "#ff0000",
                  "4" = "#8B4500", "5" = "#00468B", "6" = "#CFF800", "7" = "#008B45",
                  "8"  = "#746AB0", "9" = "#FF60A8", "10" = "gold")

# list of ratings
all_ratings <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

# Overview ----

asag <- tndd %>% separate_rows(genre, sep=", ")
ev_rev_ev_gen <- tndd %>% separate_rows(genre, sep = ", ") %>% select(genre) 
unique_gen <- ev_rev_ev_gen %>% count(genre, sort = TRUE)

# trap wrangling ----
trap_songs <- tndd[grepl("Trap", tndd$genre),]
remove_from_trap <- c("Pop Rap With Trap Vibes On The Instrumentals", "Bachelor's Degree In English Trap Raps", "Sorta Psychedelic Trap Crooner Stuff", "No Trap Beats")
unique_trap <- unique_gen[grepl("Trap", unique_gen$genre),] %>% filter(!genre %in% remove_from_trap)


# hip hop wrangling ----
hiphop_songs <- tndd[grepl("Hip Hop", tndd$genre),]
remove_from_hiphop <- c("Some Of The Wildest Hip Hop You'll Ever Hear", 
                        "Conscious Hip Hop But Hardly", "Sorta Conscious Hip Hop",
                        "Harcore Hip Hop")
unique_hiphop <- unique_gen[grepl("Hip Hop", unique_gen$genre),] %>% filter(!genre %in% remove_from_hiphop)


# rap wrangling ----
rap_songs <- tndd[grepl("Rap", tndd$genre),]
remove_from_rap <- c("Auto-Tuned Dream Rap Ballads That Feel Good", "Clou Rap (W/No Raps)",
                     "Bachelor's Degree In English Trap Raps", "Pop Rap With Trap Vibes On The Instrumentals",
                     "Soundcloud Raps", "Novelty Rap That Wishes It Was Something Else",
                     "Neo-Psychedelic-Soul-Grunge...Rap", "Jazz Rap A Couple Of Times")
unique_rap <- unique_gen[grepl("Rap", unique_gen$genre),] %>% filter(!genre %in% remove_from_rap)

# pop wrangling ----
pop_songs <- tndd[grepl("Pop", tndd$genre),]
remove_from_pop <- c("Pop Rap With Trap Vibes On The Instrumentals", "Island Music Pop Magic", "Trippy Pop Magic Future Stuff", 
                     "Dreary And Soft Electronic Pop Tunes", "Pop Folk Punk Grimyness", "Nutty Futuristic Pop")
unique_pop <- unique_gen[grepl("Pop", unique_gen$genre),] %>% filter(!genre %in% remove_from_pop)
# rock wrangling ----
rock_songs <- tndd[grepl("Rock", tndd$genre),]
remove_from_rock <- c("Vaguely Psychedelic Hard Rock", "80s Hard Rock Guitar Heroism Viewed Through A Lo-Fi Lens ", 
                      "Rockabilly...Sort Of...", "Post-Punky Beach Rock Vibes", "Blackened Hard Rock With A Little Hardcore Punk Mixed In?")
unique_rock <- unique_gen[grepl("Rock", unique_gen$genre),] %>% filter(!genre %in% remove_from_rock)
# r&b wrangling ----
r_n_b_songs <- tndd[grepl("R&B", tndd$genre),]
remove_from_r_n_b <- c("Uh......R&B....Sorta....","R&B-Ish", "Alternative R&B From Another Dimension")
unique_r_n_b <- unique_gen[grepl("R&B", unique_gen$genre),] %>% filter(!genre %in% remove_from_r_n_b)
# metal wrangling ----
metal_songs <- tndd[grepl("Metal", tndd$genre),]
remove_from_metal <- c("Noise Metal Harshness From Hell","Alt-Metal", "More Black Metal")
unique_metal <- unique_gen[grepl("Metal", unique_gen$genre),] %>% filter(!genre %in% remove_from_metal)
# latin wrangling ----
latin_songs <- tndd[grepl("Latin", tndd$genre),]
unique_latin <- unique_gen[grepl("Latin", unique_gen$genre),]

# soul wrangling ----
soul_songs <- tndd[grepl("Soul", tndd$genre),]
remove_from_soul <- c("Droney Shoegazey Darkness That Permeates My Soul",
                      "Neo-Psychedelic-Soul-Grunge...Rap", "Neo-Soul From The Future")
unique_soul <- unique_gen[grepl("Soul", unique_gen$genre),] %>% filter(!genre %in% remove_from_soul)

# funk wrangling ----
funk_songs <- tndd[grepl("Funk", tndd$genre),]
remove_from_funk <- c(" G Funk", "G Funk")
unique_funk <- unique_gen[grepl("Funk", unique_gen$genre),] %>% filter(!genre %in% remove_from_funk)

# punk wrangling ----
punk_songs <- tndd[grepl("Punk", tndd$genre),]
remove_from_punk <- c("Blackened Hard Rock With A Little Hardcore Punk Mixed In?", 
                      "Depressing Punk Jamz", "Pop Folk Punk Grimyness", 
                      "Post-Punky Beach Rock Vibes", "Zany Garage Pun", "Trying To Punk", "Zany Garage Punk")
unique_punk <- unique_gen[grepl("Punk", unique_gen$genre),] %>% filter(!genre %in% remove_from_punk)

# jazz wrangling ----
jazz_songs <- tndd[grepl("Jazz", tndd$genre),]
remove_from_jazz <- c("Jazz Rap A Couple Of Times", "Jazzed Up Bangers")
unique_jazz <- unique_gen[grepl("Jazz", unique_gen$genre),] %>% filter(!genre %in% remove_from_jazz)

# country wrangling ----
country_songs <- tndd[grepl("Country", tndd$genre),]
unique_country <- unique_gen[grepl("Country", unique_gen$genre),]

# indie wrangling ----
indie_songs <- tndd[grepl("Indie", tndd$genre),]
remove_from_indie <- c(" Indietronic", "Glistening Indie Pop", "Genre-Blending Indie Pop")
unique_indie <- unique_gen[grepl("Indie", unique_gen$genre),] %>% filter(!genre %in% remove_from_indie)

# alt wrangling ----
alt_songs <- tndd[grepl("Alt", tndd$genre),]
remove_from_alt <- c("Alternative R&B From Another Dimension", "Alt-Rock", "Alt-Metal", "Alt-Country", "Alt-Pop")
unique_alt <- unique_gen[grepl("Alt", unique_gen$genre),] %>% filter(!genre %in% remove_from_alt)

# prog wrangling ----
prog_songs <- tndd[grepl("Prog", tndd$genre),]
unique_prog <- unique_gen[grepl("Prog", unique_gen$genre),]

# misc wrangling ----
tndd1 <- anti_join(tndd, trap_songs)
tndd2 <- anti_join(tndd1, hiphop_songs)
tndd3 <- anti_join(tndd2, rap_songs)
tndd4 <- anti_join(tndd3, pop_songs)
tndd5 <- anti_join(tndd4, rock_songs)
tndd6 <- anti_join(tndd5, r_n_b_songs)
tndd7 <- anti_join(tndd6, metal_songs)
tndd8 <- anti_join(tndd7, latin_songs)
tndd9 <- anti_join(tndd8, soul_songs)
tndd10 <- anti_join(tndd9, funk_songs)
tndd11 <- anti_join(tndd10, punk_songs)
tndd12 <- anti_join(tndd11, jazz_songs)
tndd13 <- anti_join(tndd12, country_songs)
tndd14 <- anti_join(tndd13, indie_songs)
tndd15 <- anti_join(tndd14, alt_songs)

misc_songs <- anti_join(tndd15, prog_songs)
ev_misc <- misc_songs %>% separate_rows(genre, sep = ", ") %>% select(genre)
remove_misc <- c("Synthpop", "Electropop", "Hyperpop", "", 
                 "Abstract Electronic Music Meant To Mess With Your Head And Inspire Beautiful Mental Images",
                 "Etc.", "Everything A. G. Cook Does",
                 "Experimental Future Electronic Beat Music From Out Of This World",
                 "W But Still Ratio", "Washing Machine Sample Music That's Awesome",
                 "Look At The Album Cover", "Lots Of Gecs", "Riot Grrrl")
unique_misc <- ev_misc %>% count(genre, sort = TRUE) %>% filter(!genre %in% remove_misc)





# ui ----
ui <- fluidPage(
  # App title ----
  titlePanel("theneedledrop Review History"),
  
  tabsetPanel(
    type = "tabs",
    
    # Overview ----
    tabPanel(
      title = "Overview",
      br(),
      sidebarPanel(
        sliderInput(
          inputId = "rating_constraints_overview",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2("Overview:"),
        p("Hi everyone, Spencer Arch Silverstein here, the internet's busiest music nerd's biggest
          fan, and it's time for a review of this amazing YouTube Channel:", 
          tags$a(href = "https://www.youtube.com/@theneedledrop", em("theneedledrop")),
          "(for my STAT 302 final project)."),
        p("I watch a music reviewing channel on YouTube called", 
          em("theneedledrop"), "hosted by Anthony Fantano. I scraped the
          data of 1000 of his reviews to bring you his opinion of different 
          LPs (along with their names, genres, and the scores he gave to them) 
          spanning many musical genres. Have fun exploring his reviews and feel 
          free to use the widgets to help you!"),
        p(tags$a(href = "https://www.youtube.com/@theneedledrop", em("Data Citation: youtube.com/@theneedledrop")))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput(outputId = "overview_plot"),
      br(), br(), br(), br(), br(), 
      dataTableOutput("my_music_data")
    ),
    
    
    # Trap Tab ----
    tabPanel(
      title = "Trap",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "trap_sg",
          label = "Select Subgenre:",
          choices = c("All Trap", unique_trap$genre[unique_trap$genre != "Trap"]),
          selected = "All Trap"),
        sliderInput(
          inputId = "rating_constraints_trap",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      
      mainPanel(
        h2(strong("Trap:")), 
        p("A type of rap music, originating in the South, characterized by slow and heavy bass 
          drums with quick high-hat cymbals and gritty lyrics about drug culture."),
        tags$a(href = "https://www.dictionary.com/browse/trap-music", em("Source: dicionary.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("trap_plot")
    ),
    
    
    # Hip Hop Tab ----
    tabPanel(
      title = "Hip Hop",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "hiphop_sg",
          label = "Select Subgenre",
          choices = c("All Hip Hop", unique_hiphop$genre[unique_hiphop$genre != "Hip Hop"]),
          selected = "All Hip Hop"),
        sliderInput(
          inputId = "rating_constraints_hiphop",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Hip Hop:")), 
        p("Hip-hop, cultural movement that attained widespread popularity in the 1980s and ’90s and also the 
          backing music for rap, the musical style incorporating rhythmic and/or rhyming speech that became the 
          movement’s most lasting and influential art form."),
        tags$a(href = "https://www.britannica.com/art/hip-hop", em("Source: Britannica"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("hiphop_plot")
    ),
    
    
    # Rap Tab ----
    tabPanel(
      title = "Rap",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "rap_sg",
          label = "Select Subgenre",
          choices = c("All Rap", unique_rap$genre[unique_rap$genre != "Rap"]),
          selected = "All Rap"),
        sliderInput(
          inputId = "rating_constraints_rap",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Rap:")),
        p("A style of popular music, developed by disc jockeys and urban Black performers in the late 1970s, 
          in which an insistent, recurring beat pattern provides the background and counterpoint for rapid, 
          slangy, and often boastful rhyming patter intoned by a vocalist or vocalists."),
        tags$a(href = "https://www.dictionary.com/browse/rap--music", em("Source: dictionary.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("rap_plot")
    ),
    
    
    # Pop Tab ----
    tabPanel(
      title = "Pop",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "pop_sg",
          label = "Select Subgenre",
          choices = c("All Pop", unique_pop$genre[unique_pop$genre != "Pop"]),
          selected = "All Pop"),
        sliderInput(
          inputId = "rating_constraints_pop",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Pop:")),
        p("In general, pop music is made to appeal to a broad audience, rather than more nice music genres...
          There isn't one all-encompassing definition, as pop music is constantly evolving. But there are
          hundreds of examples from every era."),
        tags$a(href = "https://www.musicindustryhowto.com/what-is-pop-music/", em("Source: musicindustryhowto.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("pop_plot")
    ),
    
    
    
    # Rock Tab ----
    tabPanel(
      title = "Rock",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "rock_sg",
          label = "Select Subgenre",
          choices = c("All Rock", unique_rock$genre),
          selected = "All Rock"),
        sliderInput(
          inputId = "rating_constraints_rock",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Rock 🤘:")),
        p("Rock music, sometimes also known as", em("rock and roll,"), "is a style of music that became popular in 
          the 1950s in America and Europe. It is primarily based on older musical styles, such as the rhythm and 
          blues music originated by African American performers such as Chuck Berry and Little Richard, with a 
          heavy focus on guitar, drums, and powerful vocals."),
        tags$a(href = "https://www.musicalexpert.org/what-is-rock-music.htm", em("Source: www.musicalexpert.org"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("rock_plot")
    ),
    
    # R&B Tab ----
    tabPanel(
      title = "R&B",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "r_n_b_sg",
          label = "Select Subgenre",
          choices = c("All R&B", unique_r_n_b$genre[unique_r_n_b$genre != "R&B"]),
          selected = "All R&B"),
        sliderInput(
          inputId = "rating_constraints_r_n_b",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("R&B:")),
        p("R&B music is also known as rhythm and blues or, occasionally, R'n'B. R&B is a conglomerate of several early
          20th-century Black American music art forms. R&B first evolved in post-World War II Black communities and
          contained elements of soul, gospel, jazz, and of course, the blues."),
        tags$a(href = "https://www.musicindustryhowto.com/what-is-r-and-b-music/", em("Source: musicindustryhowto.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("r_n_b_plot")
    ),
    
    # Metal Tab ----
    tabPanel(
      title = "Metal",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "metal_sg",
          label = "Select Subgenre",
          choices = c("All Metal", unique_metal$genre), 
          selected = "All Metal"),
        sliderInput(
          inputId = "rating_constraints_metal",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Metal:")),
        p("Metal is a type of rock music that first appeared and became popular in the late 1960s and early 1970s 
          in the UK (we Brits claim responsibility for all the good stuff). Taking its origins from the blues rock 
          & psychedelic rock, the first bands to trial it used highly amplified guitar distortion, extended guitar
          solos, heavy drum beats, aggressive vocals, and they turned the volume up to eleven!"),
        tags$a(href = "https://vocal.media/beat/what-is-metal-music", em("Source: vocal.media"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("metal_plot")
    ),  
    
    # Latin Tab ----
    tabPanel(
      title = "Latin",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "latin_sg",
          label = "Select Subgenre",
          choices = c("All Latin", unique_latin$genre),
          selected = "All Latin"),
        sliderInput(
          inputId = "rating_constraints_latin",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Latin:")),
        p("Latin music is from Latin America, a cultural region consisting of countries
          that speak Latin-based languages in the Americas. It’s a mix of mostly 
          African rhythms and instruments, European song forms and instruments, 
          and indigenous influences."),
        tags$a(href = "https://rhythmnotes.net/what-is-latin-music/", em("Source: rhythmnotes.net"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("latin_plot")
    ),
    
    # Soul Tab ----
    tabPanel(
      title = "Soul",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "soul_sg",
          label = "Select Subgenre",
          choices = c("All Soul", unique_soul$genre[unique_soul$genre != "Soul"]),
          selected = "All Soul"),
        sliderInput(
          inputId = "rating_constraints_soul",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Soul:")),
        p("Soul is a departure from the religious lyrical themes of gospel music
          while maintaining some of its style elements. Where a gospel song describes 
          faith and joy, a soul song often paints a picture of desire, hardship, or 
          romance. The genre's music also puts a stronger emphasis on dancing and body movement."),
        tags$a(href = "https://www.musicindustryhowto.com/what-is-soul-music/", em("Source: musicindustryhowto.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("soul_plot")
    ), 
    
    # Funk Tab ----
    tabPanel(
      title = "Funk",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "funk_sg",
          label = "Select Subgenre",
          choices = c("All Funk", unique_funk$genre[unique_funk$genre != "Funk"]),
          selected = "All Funk"),
        sliderInput(
          inputId = "rating_constraints_funk",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Funk:")),
        p("Funk music is characterized by funky, syncopated bass lines and steady, 
          infectious drum grooves, which drove it to become one of the most
          popular genres in the 1970s and ’80s."),
        tags$a(href = "https://www.masterclass.com/articles/funk-music-guide", em("Source: masterclass.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("funk_plot")
    ), 
    
    # Punk Tab ----
    tabPanel(
      title = "Punk",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "punk_sg",
          label = "Select Subgenre",
          choices = c("All Punk", unique_punk$genre),
          selected = "All Punk"),
        sliderInput(
          inputId = "rating_constraints_punk",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Punk:")),
        p("Punk, also called punk rock, aggressive form of rock music that 
          coalesced into an international (though predominantly Anglo-American) 
          movement in 1975–80. Often politicized and full of vital energy beneath 
          a sarcastic, hostile facade, punk spread as an ideology and an aesthetic
          approach, becoming an archetype of teen rebellion and alienation."),
        tags$a(href = "https://www.britannica.com/art/punk", em("Source: britannica.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("punk_plot")
    ), 
    
    # Jazz Tab ----
    tabPanel(
      title = "Jazz",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "jazz_sg",
          label = "Select Subgenre",
          choices = c("All Jazz", unique_jazz$genre[unique_jazz$genre != "Jazz"]),
          selected = "All Jazz"),
        sliderInput(
          inputId = "rating_constraints_jazz",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Jazz:")),
        p("American music developed especially from ragtime and blues and 
          characterized by propulsive syncopated rhythms, polyphonic ensemble
          playing, varying degrees of improvisation, and often deliberate 
          distortions of pitch and timbre"),
        tags$a(href = "https://www.merriam-webster.com/dictionary/jazz", em("Source: merriam-webster.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("jazz_plot")
    ), 
    
    # Country Tab ----
    tabPanel(
      title = "Country",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "country_sg",
          label = "Select Subgenre",
          choices = c("All Country", unique_country$genre[unique_country$genre != "Country"]),
          selected = "All Country"),
        sliderInput(
          inputId = "rating_constraints_country",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Country:")),
        p("country music, also called country and western, style of American 
          popular music that originated in rural areas of the South and West in 
          the early 20th century. The term country and western music (later 
          shortened to country music) was adopted by the recording industry 
          in 1949 to replace the derogatory label hillbilly music."),
        tags$a(href = "https://www.britannica.com/art/country-music", em("Source: britannica.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("country_plot")
    ), 
    
    # Indie Tab ----
    tabPanel(
      title = "Indie",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "indie_sg",
          label = "Select Subgenre",
          choices = c("All Indie", unique_indie$genre),
          selected = "Indie"),
        sliderInput(
          inputId = "rating_constraints_indie",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Indie:")),
        p("Music made by independent artists, without the support of a record 
          label. Indie music is often characterized as having more grit and less
          of an eye towards commercial success."),
        tags$a(href = "https://www.musicindustryhowto.com/what-is-indie-music/", em("Source: musicindustryhowto.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("indie_plot")
    ), 
    
    # Alt Tab ----
    tabPanel(
      title = "Alt",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "alt_sg",
          label = "Select Subgenre",
          choices = c("All Alt", unique_alt$genre),
          selected = "All Alt"),
        sliderInput(
          inputId = "rating_constraints_alt",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Alt:")),
        p("Music outside of the mainstream realm. Alternative artists in the 
          past were seldom heard on the radio and grew their fan bases through 
          word of mouth or undergournd music clubs. Until the 2010s, alt music 
          relied on independent record labels for production and representation"),
        tags$a(href = "https://www.musicindustryhowto.com/what-is-alternative-music/", em("Source: musicindustryhowto.com"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("alt_plot")
    ), 
    
    # Prog Tab ----
    tabPanel(
      title = "Prog",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "prog_sg",
          label = "Select Subgenre",
          choices = c("All Prog", unique_prog$genre[unique_prog$genre != "Prog"]),
          selected = "All Prog"),
        sliderInput(
          inputId = "rating_constraints_prog",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Prog (Progressive):")),
        p("Progressive music is music that attempts to expand existing stylistic
          boundaries associated with specific genres of music. The word comes
          from the basic concept of 'progress', which refers to advancements 
          through accumulation, and is often deployed in the context of distinct
          genres, with progressive rock being the most notable example."),
        tags$a(href = "https://en.wikipedia.org/wiki/Progressive_music", em("Source: wikipedia.org"))
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("prog_plot")
    ),
    
    # Misc Tab ----
    tabPanel(
      title = "Misc",
      br(),
      sidebarPanel(
        selectInput(
          inputId = "misc_sg",
          label = "Select Subgenre",
          choices = c("All Misc", unique_misc$genre),
          selected = "All Misc"),
        sliderInput(
          inputId = "rating_constraints_misc",
          label = "Score: (/10)",
          min = 0, max = 10, value = c(0,10), step = 1)
      ),
      mainPanel(
        h2(strong("Miscellaneous:")),
        p("(Anything that didn't fit into another genre)")
      ),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      plotlyOutput("misc_plot")
    ),
    
    # more info tab ----
    tabPanel(
      title = "More Info",
      br(),
      mainPanel(
        h2(strong("More Info:")),
        p("Thank you so much for enjoying my project! I hope you were able to
          explore different music genres and subgenres effectively, and perhaps
          discover some new quality albums."),
        br(),
        h3(strong("Final Project Question:")),
        h4(strong("What is the core concept(s) or insight(s) into the data that 
                  you believe the visualization communicates? If you 
                  build an app then explain your choice of widgets and what 
                  about the data it help users understand.")),
        p("I set out on this venture with the goal of mapping Fantano music
          reviews, and I think that I accomplished that goal. I think this data
          does a great job of communicating the quality of different LPs
          throughout the 2010s according to a dedicated audiophile YouTuber. I
          used tabs to break up music by different genres. I used a dropdown menu
          to further break down the reviews. Finally, I used a slider to allow for
          the ability to filter through different albums by their score. I think
          all of these features helped the user understand the expansive nature
          of music both in genre type and in quality. I hope these widgets and
          the way I set up my data helped the user feel represented in their musical
          tastes and I hope they can find some of their favorite albums/genres
          represented here!"),
        p("In my memo, I set out to answer the following 3 questions:"),
        h4(em("1) Does Fantano have a bias/preferance for a specific genre?")),
        p("Over the course of 1000 reviews, I could not find compelling data
          that Fantano had any kind of bias. Had I had more time to work on this
          project I could've made more models to back up this claim. I plan on
          researching this question more, however, doing this project gave me a
          bigger appreciation of what Fantano does for a career. It takes serious
          dedication to consistently put out reviews independently for as long as
          he has, and painstakingly parsing this data has made my respect for him
          and his work grow infinitely."),
        h4(em("2) Was there a specific year of better or worse music 
              (according to Fantano's ratings)?")),
        p("In processing this data, I didn't come across a specific time frame
          underwhich there were long periods of low or high quality LPs. There's
          no outliers in the data of periods of notable quality. Overtime, the 
          reviews seem to have small peaks and valleys, but no period long 
          enough to label it as notable in my opinion."),
        h4(em("3) Did this project expose me to new and high quality LPs?")),
        p("Yes! Obviously the projects that were 10s all caught my attention. I
          didn't want to have to go manually through all of them. Below are all
          of the 10/10s I was able to scrape from his reviews:"),
        p(em("Lingua Ignota - Sinner Get Ready   |   Spellling - The Turning Wheel
                |   KIDS SEE GHOSTS - KIDS SEE GHOSTS   |   Kendrick Lamar - 
             To Pimp A Butterfly")),
        p("Additionally, for the sake of utilizing this dataset to its fullest
          potential, I feel obligated to subject myself to the albums that Fantano
          gave a 0/10 to as well. Below are those if you'd like to go through the
          torture with me:"),
        p(em("Ken Carson - X   |   Green Day - Father of All...   |   Chance the 
             Rapper - The Big Day   |   Kid Cudi - Speedin' Bullet 2 Heaven")),
        br(),
        p("Thank you so much for viewing my project. I hope you got something out
          of it. I had a blast making it! Go subscribe to Fantano's channel for 
          me, will you?", tags$a(href = "www.youtube.com/@theneedledrop", "www.youtube.com/@theneedledrop")),
        p("\"With all of this being said, yeah, I'm feeling a", strong("STRONG 10"), "on
          this project...yeah...this project rules.", em("TRAN--"), em("--SITION"),
          "have you given this project a look? Did you love it? Did you hate it?
          What would you rate it? You're the best, you're the best! What should I
          review next? Over here is another project you can check out:", 
          tags$a(href = "https://github.com/SpencerArchSilverstein",
          "github.com/SpencerArchSilverstein"), "Hit that up or the link to",
          tags$a(href = "www.youtube.com/@theneedledrop","subscribe to Fantano's channel."),
          "Spencer Arch Silverstein. Anthony Fantano Reviews.", strong("FOREVER!\"")),
        p("- Spencer Arch Silverstein, the internet's busiest music nerd's biggest fan")
          
        
        
      )
    )
  )
  
)




# server ----
#server <- function(input, output) {
server <- function(input, output) {
  
  # create plot ----
  create_plot <- function(my_data, score_colors, rating_constraints, x_val) {
    p <- ggplot(
      data = my_data,
      mapping = aes(
        x = x_val,
        y = score,
        fill = factor(score, levels = 0:10)
        )
    ) +
      geom_bar(
        stat = "identity", 
        mapping = aes(
          text = paste("LP Name:", lp_name, "<br>Artist Name:", artist_name, "<br>Score:", score))) +
      scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1)) +
      scale_fill_manual(values = score_colors) +
      labs(x = "Order of Reviews", y = "Score", fill = "Score") +
      theme_minimal() +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(axis.text.x = element_blank())
    
    plotly_plot <- ggplotly(p, tooltip = "text")
    plotly_plot

  }

  # overview plot ----
  output$overview_plot <- renderPlotly({
    tndd_altered <- tndd %>%
      filter(score >= input$rating_constraints_overview[1],
             score <= input$rating_constraints_overview[2])
    create_plot(tndd_altered, score_colors, rating_constraints_overview, tndd_altered$order_of_reviews)
  })
  
  # overview data table
  output$my_music_data <- renderDataTable({
    datatable(tndd, options = list(pageLength = 10))
  })
  
  # trap plot ----
  output$trap_plot <- renderPlotly({
    if(input$trap_sg == "All Trap"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_trap$genre,
               score >= input$rating_constraints_trap[1],
               score <= input$rating_constraints_trap[2])
      create_plot(sel_sg_data, score_colors, rating_constraints_trap, sel_sg_data$order_of_reviews)
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$trap_sg,             
               score >= input$rating_constraints_trap[1],
               score <= input$rating_constraints_trap[2])  %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_trap, sel_sg_data$row_num)
    }
  })
  
  # hiphop plot ----
  output$hiphop_plot <- renderPlotly({
    if(input$hiphop_sg == "All Hip Hop"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_hiphop$genre,
               score >= input$rating_constraints_hiphop[1],
               score <= input$rating_constraints_hiphop[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_hiphop, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$hiphop_sg,
               score >= input$rating_constraints_hiphop[1],
               score <= input$rating_constraints_hiphop[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_hiphop, sel_sg_data$row_num)
    }
  })
  
  # rap plot ----
  output$rap_plot <- renderPlotly({
    if(input$rap_sg == "All Rap"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_rap$genre,
               score >= input$rating_constraints_rap[1],
               score <= input$rating_constraints_rap[2]) 
      create_plot(sel_sg_data, score_colors, input$rating_constraints_rap, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$rap_sg,
               score >= input$rating_constraints_rap[1],
               score <= input$rating_constraints_rap[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_rap, sel_sg_data$row_num)
    }
  })
  
  # pop plot ----
  output$pop_plot <- renderPlotly({
    if(input$pop_sg == "All Pop"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_pop$genre,
               score >= input$rating_constraints_pop[1],
               score <= input$rating_constraints_pop[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_pop, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$pop_sg,
               score >= input$rating_constraints_pop[1],
               score <= input$rating_constraints_pop[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_pop, sel_sg_data$row_num) 
    }
  })
  
  # rock plot ----
  output$rock_plot <- renderPlotly({
    if(input$rock_sg == "All Rock"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_rock$genre,
               score >= input$rating_constraints_rock[1],
               score <= input$rating_constraints_rock[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_rock, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$rock_sg,
               score >= input$rating_constraints_rock[1],
               score <= input$rating_constraints_rock[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_rock, sel_sg_data$row_num) 
    }
  })
  
  # r_n_b plot ----
  output$r_n_b_plot <- renderPlotly({
    if(input$r_n_b_sg == "All R&B"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_r_n_b$genre,
               score >= input$rating_constraints_r_n_b[1],
               score <= input$rating_constraints_r_n_b[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_r_n_b, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$r_n_b_sg,
               score >= input$rating_constraints_r_n_b[1],
               score <= input$rating_constraints_r_n_b[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_r_n_b, sel_sg_data$row_num) 
    }
  })
  
  # metal plot ----
  output$metal_plot <- renderPlotly({
    if(input$metal_sg == "All Metal"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_metal$genre,
               score >= input$rating_constraints_metal[1],
               score <= input$rating_constraints_metal[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_metal, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$metal_sg,
               score >= input$rating_constraints_metal[1],
               score <= input$rating_constraints_metal[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_metal, sel_sg_data$row_num) 
    }
  })
  
  # latin plot ----
  output$latin_plot <- renderPlotly({
    if(input$latin_sg == "All Latin"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_latin$genre,
               score >= input$rating_constraints_latin[1],
               score <= input$rating_constraints_latin[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_latin, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$latin_sg,
               score >= input$rating_constraints_latin[1],
               score <= input$rating_constraints_latin[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_latin, sel_sg_data$row_num) 
    }})
  
  # soul plot ----
  output$soul_plot <- renderPlotly({
    if(input$soul_sg == "All Soul"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_soul$genre,
               score >= input$rating_constraints_soul[1],
               score <= input$rating_constraints_soul[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_soul, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$soul_sg,
               score >= input$rating_constraints_soul[1],
               score <= input$rating_constraints_soul[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_soul, sel_sg_data$row_num) 
    }})
  
  # funk plot ----
  output$funk_plot <- renderPlotly({
    if(input$funk_sg == "All Funk"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_funk$genre,
               score >= input$rating_constraints_funk[1],
               score <= input$rating_constraints_funk[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_funk, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$funk_sg,
               score >= input$rating_constraints_funk[1],
               score <= input$rating_constraints_funk[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_funk, sel_sg_data$row_num) 
    }})
  
  # punk plot ----
  output$punk_plot <- renderPlotly({
    if(input$punk_sg == "All Punk"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_punk$genre,
               score >= input$rating_constraints_punk[1],
               score <= input$rating_constraints_punk[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_punk, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$punk_sg,
               score >= input$rating_constraints_punk[1],
               score <= input$rating_constraints_punk[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_punk, sel_sg_data$row_num) 
    }})
  
  # jazz plot ----
  output$jazz_plot <- renderPlotly({
    if(input$jazz_sg == "All Jazz"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_jazz$genre,
               score >= input$rating_constraints_jazz[1],
               score <= input$rating_constraints_jazz[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_jazz, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$jazz_sg,
               score >= input$rating_constraints_jazz[1],
               score <= input$rating_constraints_jazz[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_jazz, sel_sg_data$row_num) 
    }})
  
  # country plot ----
  output$country_plot <- renderPlotly({
    if(input$country_sg == "All Country"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_country$genre,
               score >= input$rating_constraints_country[1],
               score <= input$rating_constraints_country[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_country, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$country_sg,
               score >= input$rating_constraints_country[1],
               score <= input$rating_constraints_country[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_country, sel_sg_data$row_num) 
    }})
  
  # indie plot ----
  output$indie_plot <- renderPlotly({
    if(input$indie_sg == "All Indie"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_indie$genre,
               score >= input$rating_constraints_indie[1],
               score <= input$rating_constraints_indie[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_indie, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$indie_sg,
               score >= input$rating_constraints_indie[1],
               score <= input$rating_constraints_indie[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_indie, sel_sg_data$row_num) 
    }})
  
  # alt plot ----
  output$alt_plot <- renderPlotly({
    if(input$alt_sg == "All Alt"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_alt$genre,
               score >= input$rating_constraints_alt[1],
               score <= input$rating_constraints_alt[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_alt, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$alt_sg,
               score >= input$rating_constraints_alt[1],
               score <= input$rating_constraints_alt[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_alt, sel_sg_data$row_num) 
    }})
  
  # prog plot ----
  output$prog_plot <- renderPlotly({
    if(input$prog_sg == "All Prog"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_prog$genre,
               score >= input$rating_constraints_prog[1],
               score <= input$rating_constraints_prog[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_prog, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$prog_sg,
               score >= input$rating_constraints_prog[1],
               score <= input$rating_constraints_prog[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_prog, sel_sg_data$row_num) 
    }})
  
  # misc plot ----
  output$misc_plot <- renderPlotly({
    if(input$misc_sg == "All Misc"){
      sel_sg_data <- asag %>%
        filter(genre %in% unique_misc$genre,
               score >= input$rating_constraints_misc[1],
               score <= input$rating_constraints_misc[2])
      create_plot(sel_sg_data, score_colors, input$rating_constraints_misc, sel_sg_data$order_of_reviews)
      
    } else{
      sel_sg_data <- asag %>% 
        filter(genre == input$misc_sg,
               score >= input$rating_constraints_misc[1],
               score <= input$rating_constraints_misc[2]) %>% 
        mutate(row_num = row_number())
      create_plot(sel_sg_data, score_colors, rating_constraints_misc, sel_sg_data$row_num) 
    }})
}

# Run the application 
shinyApp(ui = ui, server = server)
