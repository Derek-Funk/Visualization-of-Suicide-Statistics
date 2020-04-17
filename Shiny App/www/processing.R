#process state data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
states <- read.csv(file = "rawData/cdc_states.csv")
states <- states[c("State", "Year", "Deaths", "Crude.Rate")]
dim(states)
#returns 989 = 51 states (DC included) * 19 years + 19 year subtotals + 1 grand total
#remove the year subtotals and 1 grand total to get to 51*19=969 rows
states <- states[states$State!="",]
dim(states)

statesVector <- as.character(states$State[1:51])
