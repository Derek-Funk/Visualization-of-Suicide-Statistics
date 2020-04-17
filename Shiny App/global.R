library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(maps)
library(sf)
library(rgdal)
library(geojsonio)
library(dplyr)
library(shinyjs)
library(shiny.semantic)
library(plotly)
library(shinyjqui)
library(dygraphs)
library(d3heatmap)

finiteIncrementer <- function(value, limit) {
  x <- if(value>=limit) {
    1
  } else {
    value + 1
  }
  return(x)
}

finiteDecrementer <- function(value, limit) {
  x <- if(value<=1) {
    limit
  } else {
    value - 1
  }
  return(x)
}

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#process US data
usa <- read.csv("www/rawData/cdc_national.csv")
usa <- usa[c("Sex","Race","Age.Group","Year","Deaths","Population")]
usa <- usa[-which(usa$Age.Group %in% c("Unknown yrs","All Ages")),]
usa <- usa[-nrow(usa),]
usa$Population <- as.integer(as.character(usa$Population))
usa$Age.Group2 <- 
  ifelse(usa$Age.Group %in% c("00-04 yrs","05-09 yrs","10-14 yrs"),"<15",
    ifelse(usa$Age.Group %in% c("15-19 yrs","20-24 yrs"), "15-24",
      ifelse(usa$Age.Group %in% c("25-29 yrs","30-34 yrs"), "25-34",
        ifelse(usa$Age.Group %in% c("35-39 yrs","40-44 yrs"), "35-44",
          ifelse(usa$Age.Group %in% c("45-49 yrs","50-54 yrs"), "45-54",
            ifelse(usa$Age.Group %in% c("55-59 yrs","60-64 yrs"), "55-64",
              ifelse(usa$Age.Group %in% c("65-69 yrs","70-74 yrs"), "65-74",
                "75+")))))))
levels(usa$Race)[levels(usa$Race)=="Am Indian/AK Native"] <- "Native American"
levels(usa$Race)[levels(usa$Race)=="Asian/Pac Islander"] <- "Asian"
usa2017 <- usa %>% filter(Year==2017)

#process state data
states <- read.csv(file = "www/rawData/cdc_states.csv")
states <- states[c("State", "Year", "Deaths", "Crude.Rate")]
dim(states)
#returns 989 = 51 states (DC included) * 19 years + 19 year subtotals + 1 grand total
#remove the year subtotals and 1 grand total to get to 51*19=969 rows
states <- states[states$State!="",]
states2017 <- states %>%
  filter(Year == 2017) %>%
  select(Crude.Rate)
statesVector <- as.character(states$State[1:51])
statesUrl <- "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
statePolygons <- geojson_read(x = statesUrl , what = "sp")
for(i in 1999:2017) {
  statePolygons[[as.character(i)]] <- c(filter(states, Year==i)[,"Crude.Rate"], NA)
}
statePolygons$clicked <- rep(FALSE,52)
suicideRateBins <- c(0, 5, 10, 15, 20, 25, 30)
mapboxToken <- "pk.eyJ1IjoiZGVyZWstZnVuayIsImEiOiJjanRzd3g5am4wb2t4M3lxdXM5bzM0ZzBrIn0.FF7p0w7uLe3HeVFMNuxQaw"

#process global data
nations <- read.csv(
  file = "www/rawData/who_InternationalSuicideRates_5yrIntervals.csv",
  skip = 1
)[,c(1,2,6,9)]
names(nations)[3] <- "Crude.Rate"
badNationsIndex <- which(nations$Country.Code %in% c("ATG","BRB","COM","CPV","CRI","GRD","KIR","LCA","MDV","SGP","STP","SYC","VCT","WSM"))
nations <- nations[-badNationsIndex,]

nationsUrl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
nationPolygons <- geojson_read(x = nationsUrl, what = "sp")
badNationsIndex2 <- which(nationPolygons$id %in% c("-99","ATA","ATF","CRI","CS-KM","ESH","GRL","GUF","PRI","PSE"))
nationPolygons <- nationPolygons[-badNationsIndex2,]
nationsVector <- as.character(unique(nationPolygons$name))
for(i in c(2000,2005,2010,2015,2016)) {
  nationPolygons[[as.character(i)]] <- filter(nations, Year==i)[,"Crude.Rate"]
}
nationPolygons$clicked <- rep(FALSE,169)
suicideRateBinsNations <- c(0, 10, 20, 30, 40, 50, 60)
