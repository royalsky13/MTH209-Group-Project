################################
## Essential Packages
################################

library (tidyverse)
library (dplyr)
library (rvest)
library (stringr)


################################
## Data Preparation and Cleaning
################################


# Generating MyAnimeList's webpage URLs
links_generator <- function (limit)
{
  links <- NULL
  for ( i in seq(0, limit, by=50) ) 
  {
    print (i)
    links <- c( links, paste0("https://myanimelist.net/topanime.php?limit=",i) )
  }
  return (links)
}

# Setting the upper limit
ulimit = 9550 # (will change upper limit to 25350)
#anime_number <- 12
anime_number <- ulimit + 50
links <- links_generator (limit = ulimit)

# Creating anime title, score, recommended numbers, total reviews and anime pictures' URL column

# Initializing the column names
anime.names <- NULL; score.numbers <- NULL
anime.links <- NULL; picture.links <- NULL
recommended.numbers <- NULL
total.reviews <- NULL

# Feeding the values into it
for (i in 1:length(links) )
{
  print (i)
  html <- read_html (links[i])
  
  anime.name <- html %>% html_elements ( ".fl-l.fs14.fw-b.anime_ranking_h3 a" ) %>% html_text ()
  anime.link <- html %>% html_elements ( ".fl-l.fs14.fw-b.anime_ranking_h3 a" ) %>% html_attr ("href")
  picture.link <- html %>% html_elements (".lazyload") %>% html_attr ("data-src")
  score.number <- html %>% html_elements (".js-top-ranking-score-col.di-ib.al span") %>% html_text ()
  
  anime.names <- c( anime.names, anime.name )
  score.numbers <- c( score.numbers, as.numeric (score.number) )
  anime.links <- c( anime.links, anime.link )
  picture.links <- c( picture.links, picture.link )
}

##
anime.names <- anime.names [1:anime_number]
anime.links <- anime.links [1:anime_number] 
score.numbers <- score.numbers [1:anime_number]
picture.links <- picture.links [1:anime_number]
##

# Extracting the unique IDs from anime.links
uids <- as.numeric (sapply (1:anime_number, function (i) strsplit (substring (anime.links, 31), "/")[[i]][1] ) )

# Dummy Variables for more column names
information_vec <- NULL; type_cleaned <- NULL
episode_cleaned <- NULL; status_cleaned <- NULL
aired_uncleaned <- NULL; premiered_cleaned <- NULL
broadcast_uncleaned <- NULL; producers_cleaned <- NULL
licensors_cleaned <- NULL; studio_cleaned <- NULL
source_cleaned <- NULL; genre_cleaned <- NULL
theme_cleaned <- NULL; demographic_uncleaned <- NULL
duration_uncleaned <- NULL; rating_uncleaned <- NULL
ranked_cleaned <- 1:anime_number; popularity_cleaned <- NULL
members_cleaned <- NULL; favorites_cleaned <- NULL
synonyms_cleaned <- NULL; english_title <- NULL
scored_bys <- NULL

for (i in 1:length(anime.links) )
{
  html <- read_html (anime.links[i])
  print (i)
  
  # Getting the recommended no.s, scored by and total reviews column
  recommended.number <- html %>% html_elements (".recommended strong") %>% html_text ()
  total.review <- html %>% html_elements (".right strong") %>% html_text ()
  scored_by <- html %>% html_element (".fl-l.score") %>% html_attr ("data-user")
  scored_by <- gsub (",", "", scored_by)
  scored_by <- as.numeric (strsplit (scored_by, " ")[[1]][1] )
  
  scored_bys <- c( scored_bys, scored_by )
  recommended.numbers <- c( recommended.numbers, as.numeric (recommended.number) )
  total.reviews <- c( total.reviews, as.numeric (total.review) )
  
  # Removing multiple spaces from the information variable
  information <- html %>% html_elements (".spaceit_pad") %>% html_text ()
  information <- str_squish (information)
  len <- nchar (information)
  
  # Creating an information finder function
  information_finder <- function (words)
  {
    if (sum (grepl (words, information) == 1 ) )
    return (information [grep (words, information)] )
    else
      return (NA)
  }
  
  # Getting the other columns
  english_title <- c( english_title, substring (information_finder ("English:"), 10) )
  synonyms_cleaned <- c( synonyms_cleaned, substring (information_finder ("Synonyms: "), 11) )
  type_cleaned <- c( type_cleaned, substring (information_finder ("Type: "), 7) )
  episode_cleaned <- c( episode_cleaned, as.numeric (substring (information_finder ("Episodes: "), 11) ) )
  status_cleaned <- c( status_cleaned, substring (information_finder ("Status: "), 9) )
  aired_uncleaned <- c( aired_uncleaned, substring (information_finder ("Aired: "), 8) )
  premiered_cleaned <- c( premiered_cleaned, substring (information_finder ("Premiered:"), 12) )
  broadcast_uncleaned <- c( broadcast_uncleaned, substring (information_finder ("Broadcast: "), 12) )
  producers_cleaned <- c( producers_cleaned, substring (information_finder ("Producers:") , 12) )
  licensors_cleaned <- c( licensors_cleaned, substring (information_finder ("Licensors:"), 12) )
  studio_cleaned <- c( studio_cleaned, substring (information_finder ( "Studios: "), 10) )
  source_cleaned <- c( source_cleaned, substring (information_finder ("Source: "), 9) )
  genre_cleaned <- c( genre_cleaned, substring (information_finder ("Genres:"), 9) )
  theme_cleaned <- c( theme_cleaned, substring (information_finder ("Themes: "), 9) )
  demographic_uncleaned <- c( demographic_uncleaned, substring (information_finder ("Demographic: "), 14) )
  duration_uncleaned <- c( duration_uncleaned, substring (information_finder ("Duration: "), 11) )
  rating_uncleaned <- c( rating_uncleaned, substring (information_finder ("Rating: "), 9) )
  popularity_cleaned <- c( popularity_cleaned, as.numeric (substring (information_finder ("Popularity: "), 14)) )
  members_cleaned <- c( members_cleaned, as.numeric (gsub (",", "", substring (information_finder("Members: "), 10) ) ) )
  favorites_cleaned <- c( favorites_cleaned, as.numeric (gsub (",", "", substring (information_finder ("Favorites: "), 12) ) ) ) 
  ## (The Information Vector)
  information_vec <- c( information_vec, information )
}

# Getting season column and year of release from premiered_cleaned column
season <- sapply (1:anime_number, function (i) {strsplit (premiered_cleaned, " ")[[i]][1] } )
year_of_release <- sapply (1:anime_number, function (i) {strsplit (premiered_cleaned, " ")[[i]][2] } )


# Converting broadcast uncleaned column into broadcast day and broadcast JST time column
broadcast_day <- gsub ("s", "", sapply (1:anime_number, function (i) {strsplit (broadcast_uncleaned, " ")[[i]][1] } ) )
broadcast_jp_time <- sapply (1:anime_number, function (i) {strsplit (broadcast_uncleaned, " ")[[i]][3] } )

# Getting aired from and aired to column from aired_uncleaned column
aired_from <- sapply (1:anime_number, function (i) {strsplit (aired_uncleaned, " to ")[[i]][1] } )
aired_to <- sapply (1:anime_number, function (i) {strsplit (aired_uncleaned, " to ")[[i]][2] } )


# Replacing "?" in aired_to column which indicates ongoing time to current present time
aired_to [grep ( "\\?", aired_to) ] = format (Sys.Date(), "%b %d, %Y")

# Converting aired from and aired to column to DateTime object (yyyy-mm-dd format)
aired_from <- as.Date (aired_from, format = "%b %d, %Y")
aired_to <- as.Date (aired_to, format = "%b %d, %Y")

# Calculating the airing durations (from this we will find the longest running animes !~)
airing_duration <- sapply (1:anime_number, function (i) {difftime (aired_to[i], aired_from[i], units = "weeks") } )
airing_duration <- round (as.numeric (airing_duration), 2)

# Cleaning Duration column
duration_uncleaned <- gsub ("\\.", "", duration_uncleaned)
duration_uncleaned <- gsub (" per ep", "", duration_uncleaned)

# Cleaning the genre column further
genre_list <- sapply (1:anime_number, function (i) {trimws (strsplit (genre_cleaned, ",")[[i]] ) } )
genre_matcher <- function (incorrect_genre, correct_genre )
{
  for (i in 1:anime_number)
  {
    genre_list[[i]] = gsub (incorrect_genre, correct_genre, genre_list[[i]] )
  }
  return (genre_list)
}

genre_list <- genre_matcher ("ActionAction", "Action")
genre_list <- genre_matcher ("ComedyComedy", "Comedy")
genre_list <- genre_matcher ("HorrorHorror", "Horror")
genre_list <- genre_matcher ("SportsSports", "Sports")
genre_list <- genre_matcher ("AdventureAdventure", "Adventure")
genre_list <- genre_matcher ("DramaDrama", "Drama")
genre_list <- genre_matcher ("MysteryMystery", "Mystery")
genre_list <- genre_matcher ("SupernaturalSupernatural", "Supernatural")
genre_list <- genre_matcher ("Avant GardeAvant Garde", "Avant Garde")
genre_list <- genre_matcher ("FantasyFantasy", "Fantasy")
genre_list <- genre_matcher ("RomanceRomance", "Romance")
genre_list <- genre_matcher ("SuspenseSuspense", "Suspense")
genre_list <- genre_matcher ("Award WinningAward Winning", "Award Winning")
genre_list <- genre_matcher ("Girls LoveGirls Love", "Girls Love")
genre_list <- genre_matcher ("Sci-FiSci-Fi", "Sci-Fi")
genre_list <- genre_matcher ("Boys LoveBoys Love", "Boys Love")
genre_list <- genre_matcher ("GourmetGourmet", "Gourmet")
genre_list <- genre_matcher ("Slice of LifeSlice of Life", "Slice of Life")
genre_list <- genre_matcher ("EcchiEcchi", "Ecchi")

genre_list <- paste (genre_list, ",")
leng <- nchar(genre_list)
genre_list <- substring (genre_list, 4, leng-4)
genre_list <- gsub ("\"", "", genre_list)
genre_list[ c( grep ("^\\s*$", genre_list) ) ] = NA


# Cleaning theme column further
theme_list <- sapply (1:anime_number, function (i) {trimws (strsplit (theme_cleaned, "," )[[i]] ) } )
theme_matcher <- function (incorrect_theme, correct_theme)
{
  for (i in 1:anime_number)
  {
    theme_list[[i]] = gsub (incorrect_theme, correct_theme, theme_list[[i]] )
  }
  return (theme_list)
}

theme_list <- theme_matcher ("Adult CastAdult Cast", "Adult Cast")
theme_list <- theme_matcher ("CrossdressingCrossdressing", "Crossdressing")
theme_list <- theme_matcher ("GoreGore", "Gore")
theme_list <- theme_matcher ("Idols \\(Male\\)Idols \\(Male\\)", "Idols (Male)")
theme_list <- theme_matcher ("Mahou ShoujoMahou Shoujo", "Mahou Shoujo")
theme_list <- theme_matcher ("MusicMusic", "Music")
theme_list <- theme_matcher ("Performing ArtsPerforming Arts", "Performing Arts")
theme_list <- theme_matcher ("Reverse HaremReverse Harem", "Reverse Harem")
theme_list <- theme_matcher ("SpaceSpace", "Space")
theme_list <- theme_matcher ("Time TravelTime Travel", "Time Travel")
theme_list <- theme_matcher ("AnthropomorphicAnthropomorphic", "Anthropomorphic")
theme_list <- theme_matcher ("DelinquentsDelinquents", "Delinquents")
theme_list <- theme_matcher ("HaremHarem", "Harem")
theme_list <- theme_matcher ("IsekaiIsekai", "Isekai")
theme_list <- theme_matcher ("Martial ArtsMartial Arts", "Martial Arts")
theme_list <- theme_matcher ("MythologyMythology", "Mythology")
theme_list <- theme_matcher ("PetsPets", "Pets")
theme_list <- theme_matcher ("Romantic SubtextRomantic Subtext", "Romantic Subtext")
theme_list <- theme_matcher ("Strategy GameStrategy Game", "Strategy Game")
theme_list <- theme_matcher ("VampireVampire", "Vampire")
theme_list <- theme_matcher ("CGDCTCGDCT", "CGDCT")
theme_list <- theme_matcher ("DetectiveDetective", "Detective")
theme_list <- theme_matcher ("High Stakes GameHigh Stakes Game", "High Stakes Game")
theme_list <- theme_matcher ("IyashikeiIyashikei", "Iyashikei")
theme_list <- theme_matcher ("MechaMecha", "Mecha")
theme_list <- theme_matcher ("Organized CrimeOrganized Crime", "Organized Crime")
theme_list <- theme_matcher ("PsychologicalPsychological", "Psychological")
theme_list <- theme_matcher ("SamuraiSamurai", "Samurai")
theme_list <- theme_matcher ("Super PowerSuper Power", "Super Power")
theme_list <- theme_matcher ("Video GameVideo Game", "Video Game")
theme_list <- theme_matcher ("ChildcareChildcare", "Childcare")
theme_list <- theme_matcher ("EducationalEducational", "Educational")
theme_list <- theme_matcher ("HistoricalHistorical", "Historical")
theme_list <- theme_matcher ("Love PolygonLove Polygon", "Love Polygon")
theme_list <- theme_matcher ("MedicalMedical", "Medical")
theme_list <- theme_matcher ("Otaku CultureOtaku Culture", "Otaku Culture")
theme_list <- theme_matcher ("RacingRacing", "Racing")
theme_list <- theme_matcher ("SchoolSchool", "School")
theme_list <- theme_matcher ("SurvivalSurvival", "Survival")
theme_list <- theme_matcher ("Visual ArtsVisual Arts", "Visual Arts")
theme_list <- theme_matcher ("SurvivalSurvival", "Survival")
theme_list <- theme_matcher ("Combat SportsCombat Sports", "Combat Sports")
theme_list <- theme_matcher ("Gag HumorGag Humor", "Gag Humor")
theme_list <- theme_matcher ("Idols \\(Female\\)Idols \\(Female\\)", "Idols (Female)")
theme_list <- theme_matcher ("Magical Sex ShiftMagical Sex Shift", "Magical Sex Shift")
theme_list <- theme_matcher ("MilitaryMilitary", "Military")
theme_list <- theme_matcher ("ParodyParody", "Parody")
theme_list <- theme_matcher ("ReincarnationReincarnation", "Reincarnation")
theme_list <- theme_matcher ("ShowbizShowbiz", "Showbiz")
theme_list <- theme_matcher ("Team SportsTeam Sports", "Team Sports")
theme_list <- theme_matcher ("WorkplaceWorkplace", "Workplace")

theme_list <- paste (theme_list, ",")
leng <- nchar (theme_list)
theme_list <- substring (theme_list, 4, leng-4)
theme_list <- gsub ("\"", "", theme_list)
theme_list[ c( grep ("^\\s*$", theme_list) ) ] = NA


# Cleaning demographic column further
demographic_list <- sapply (1:anime_number, function (i) {trimws (strsplit (demographic_uncleaned, " ")[[i]] ) } )
demographic_matcher <- function (incorrect_demographic, correct_demographic )
{
  for (i in 1:anime_number)
  {
    demographic_list[[i]] = gsub (incorrect_demographic, correct_demographic, demographic_list[[i]] )
  }
  return (demographic_list)
}

demographic_list <- demographic_matcher ("JoseiJosei", "Josei")
demographic_list <- demographic_matcher ("KidsKids", "Kids")
demographic_list <- demographic_matcher ("SeinenSeinen", "Seinen")
demographic_list <- demographic_matcher ("ShoujoShoujo", "Shoujo")
demographic_list <- demographic_matcher ("ShounenShounen", "Shounen")

# Changing "None found, add some" to NA in Licensor column !!!
licensors_cleaned <- gsub ("None found, add some", NA, licensors_cleaned )


################################
## Creating our Anime Dataframe
################################

anime_df <- data.frame("Unique IDs" = uids, "Rank" = ranked_cleaned, 
                        "Anime Title" = anime.names, 
                       "English Title" = english_title, "Synonyms" = synonyms_cleaned,
                       "Score" = score.numbers, "Scored by" = scored_bys,
                       "Type" = type_cleaned, "Source" = source_cleaned,
                       "Episode Count" = episode_cleaned,
                       "Status" = status_cleaned, 
                       "Aired from" = aired_from, "Aired to" = aired_to, "Airing Duration (in weeks)" =airing_duration,
                       "Season" = season, "Release Year" = year_of_release, 
                       "Broadcast Day" = broadcast_day, "Broadcast JST" = broadcast_jp_time,
                       "Producers" = producers_cleaned, "Licensors" = licensors_cleaned,
                       "Studio" = studio_cleaned, "Genre" = genre_list,
                       "Theme" = theme_list, "Demographic" = demographic_list,
                       "Duration per Episode" = duration_uncleaned,"Rating" = rating_uncleaned,
                       "Popularity Rank" = popularity_cleaned,
                       "Members" = members_cleaned, "Favorites" = favorites_cleaned,
                       "Recommended Votes" = recommended.numbers, "Reviews received" = total.reviews,
                       "Anime URLs" = anime.links, "Image URLs" = picture.links,
                       stringsAsFactors = FALSE)
View (anime_df)


################################
## Saving the df as a csv file
################################

write.csv (anime_df, paste0 (getwd(), "/", "Anime Dataframe.csv"), row.names = FALSE)
print ('CSV file written Successfully :)')