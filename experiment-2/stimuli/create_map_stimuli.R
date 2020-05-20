library(dplyr)
library(readr)
library(maps)
library(fst)

save.files <- TRUE
png.width  <- 1000 # px
png.height <- 1000 # px

# for cities that are not highlighted:
passive.pch <- 19
passive.cex <- 3
passive.col <- "darkgrey"

# for hightlighted city:
active.pch <- 19
active.cex <- 5
active.col <- "red"

# other:
outline.col <- "grey"
names.cex <- 1.7
names.offset <- 1 


# USA ---------------------------------------------------------------------

us_cities <- read_csv("stimuli/us/uscitiesv1.4.csv")


# Big cities (population >= 100,000)
big_cities <- us_cities %>%
  filter(!grepl("\\s", city)) %>% # City name is a single word
  filter(nchar(city) <= 8) %>% # City name is no longer than 8 characters
  filter(city == city_ascii) %>% # City name does not contain non-ascii characters
  filter(population >= 100000)

if(save.files) png("stimuli/big_cities_all.png", png.width*1.5, png.height)

map("state", col="grey")
title("All big cities")

points(big_cities$lng, big_cities$lat, 
       pch=passive.pch, cex=passive.cex, col=passive.col)

text(big_cities$lng, big_cities$lat+0.5, labels=paste0(big_cities$city, ",", big_cities$state_id), cex = 1.5)

if(save.files) dev.off()


# From the set of all big cities, select 30 candidates by hand
big_candidates <- c("Seattle", 
                    "Portland",
                    "Boise", 
                    "Billings", 
                    "Provo", 
                    "Reno",
                    "Fresno", 
                    "Glendale",
                    "Phoenix",
                    "Denver",
                    "Fargo",
                    "Omaha",
                    "Wichita",
                    "Tulsa",
                    "Dallas",
                    "Houston",
                    "Mobile",
                    "Jackson",
                    "Memphis",
                    "Madison",
                    "Chicago",
                    "Detroit",
                    "Buffalo",
                    "Atlanta",
                    "Tampa",
                    "Miami",
                    "Boston",
                    "Newark",
                    "Richmond",
                    "Raleigh")

big_cities_selection <- filter(big_cities, city %in% big_candidates) %>%
  # Take care of recurring city names
  filter(!(city == "Portland" & state_id == "ME"),
         !(city == "Richmond" & state_id == "CA"),
         !(city == "Glendale" & state_id == "AZ"))

if(save.files) png("stimuli/big_cities_selection.png", png.width*1.5, png.height)

map("state", col="grey")
title("Selected big cities")

points(big_cities_selection$lng, big_cities_selection$lat, 
       pch=passive.pch, cex=passive.cex, col=passive.col)

text(big_cities_selection$lng, big_cities_selection$lat+0.5, labels=paste0(big_cities_selection$city, ",", big_cities_selection$state_id), cex = 1.5)

if(save.files) dev.off()


# Small cities (population <= 5000)
small_cities <- us_cities %>%
  filter(!grepl("\\s", city)) %>% # City name is a single word
  filter(nchar(city) <= 8) %>% # City name is no longer than 8 characters
  filter(city == city_ascii) %>% # City name does not contain non-ascii characters
  filter(population <= 5000) %>%
  group_by(city) %>% 
  filter(n() >= 5) %>% # There are at least 5 cities with this name
  ungroup()

# Make sure that we have not used these cities before
load("stimuli/us/set1.rda")
load("stimuli/us/set2.rda")

small_cities <- filter(small_cities, !city %in% c(set1$city, set2$city))

# Make sure that the names do not match any of the big-city names
small_cities <- filter(small_cities, !city %in% big_cities$city)

# Sample at most 10 candidates per state
small_cities <- small_cities %>%
  group_by(state_id) %>%
  slice(1:10)


if(save.files) png("stimuli/small_cities_all.png", png.width*1.5, png.height)

map("state", col="grey")
title("All small cities")


points(small_cities$lng, small_cities$lat, 
       pch=passive.pch, cex=passive.cex, col=passive.col)

text(small_cities$lng, small_cities$lat+0.5, labels=paste0(small_cities$city, ",", small_cities$state_id), cex = 0.75)

if(save.files) dev.off()


# From the set of all small cities, select 30 candidates by hand
small_candidates <- c(
  "Albion, WA",
  "Elkton, OR",
  "Seneca, OR",
  "Plymouth, CA",
  "Benson, AZ",
  "Chester, MT",
  "Hope, NM",
  "Morton, TX",
  "Naples, UT",
  "Lyman, UT",
  "Norwood, CO",
  "Wilton, MN",
  "Ridgeway, IA",
  "Verona, MO",
  "Danville, AR",
  "Goshen, AL",
  "Jasper, FL",
  "Fairfax, SC",
  "Argyle, NY",
  "Spencer, WV",
  "Hanover, IN",
  "Winfield, TN",
  "Dawson, ND",
  "Yale, SD",
  "Lamar, NE",
  "Cuba, NM",
  "Afton, WY",
  "Wayland, MI",
  "Fairview, PA",
  "Thayer, IL"
)

small_cities$city_state <- paste(small_cities$city, small_cities$state_id, sep = ", ")
small_cities_selection <- filter(small_cities, city_state %in% small_candidates)


if(save.files) png("stimuli/small_cities_selection.png", png.width*1.5, png.height)

map("state", col="grey")
title("Selected small cities")

points(small_cities_selection$lng, small_cities_selection$lat, 
       pch=passive.pch, cex=passive.cex, col=passive.col)

text(small_cities_selection$lng, small_cities_selection$lat+0.5, labels=paste0(small_cities_selection$city, ",", small_cities_selection$state_id), cex = 1.5)

if(save.files) dev.off()


# Plot all 60 selected stimuli
if(save.files) png("stimuli/all_selected_cities.png", png.width*1.5, png.height)

map("state", col="grey")
title("Selected small (lightblue) and big (blue) cities")

points(small_cities_selection$lng, small_cities_selection$lat, 
       pch=passive.pch, cex=passive.cex, col="lightblue")

text(small_cities_selection$lng, small_cities_selection$lat+0.5, labels=paste0(small_cities_selection$city, ",", small_cities_selection$state_id), cex = 1.5)


points(big_cities_selection$lng, big_cities_selection$lat, 
       pch=passive.pch, cex=passive.cex, col="blue")

text(big_cities_selection$lng, big_cities_selection$lat+0.5, labels=paste0(big_cities_selection$city, ",", big_cities_selection$state_id), cex = 1.5)


if(save.files) dev.off()


big_cities_selection$city_type <- "big"
small_cities_selection$city_type <- "small"

setC <- bind_rows(big_cities_selection, small_cities_selection)
write_fst(setC, "stimuli/setC.fst")

# Loop through and create the individual stimuli used in the experiment
for(i in 1:nrow(setC)) {
  if(save.files) 
    png(paste("stimuli/us/setC/", as.character(setC$city[i]), ".png", sep=""), png.width, png.height)
  
  map("state", col=outline.col)     # draw the outline
  
  points(setC$lng[-i], setC$lat[-i],   # all but the highlighted city
         pch=passive.pch, cex=passive.cex, col=passive.col)  # in passive colors
  
  points(setC$lng[i], setC$lat[i],   # only the highlighted city
         pch=active.pch, cex=active.cex, col=active.col)  # in active colors
  
  if(save.files) dev.off()
}

# Create materials csv
setC_formatted <- setC %>%
  arrange(city) %>%
  transmute(id = row_number(),
            image = paste0("./res/img/setC/setC_", formatC(id, width = 2, flag = "0"), ".png"),
            text = "",
            answer = tolower(city))

write_csv(setC_formatted, "stimuli/setC.csv")
            