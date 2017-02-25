# Clean 2016 precinct-level general elections data from Monroe county
# Exported csvs (headerless) from original PDFs using Tabula
# Formatting to match https://github.com/openelections/openelections-data-ny/blob/master/2016/20161108__ny__general__putnam__precinct.csv

library(dplyr)
library(tidyr)

# Format comma-formatted numbers as numeric
asCommaless <- function(x) {
	x <- as.numeric(gsub(",", "", x))	
}

# Data formatting common for all races
formatData <- function(dt, race) {
	# Format numeric columns
	dt[, -c(1:2)] <- sapply(dt[, -c(1:2)], asCommaless)
	
	# District labels are listed in the rows above precinct data
	# Need to propogate down to all rows below, until next row with non-null district
	dt <- dt %>% fill(district) %>% 
		# Remove district header rows and total rows - aka rows with missing precincts
		filter(!is.na(precinct))  %>%
	# Convert data to long for Elex format
		gather(party, votes, -c(1:2)) %>%
		# Add & format identifying columns
		mutate(county = "Monroe",
					 office = race,
					 party = toupper(party))
	
}

#########################################################
# President
#########################################################
prez_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results president.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(prez_raw) <- c("district", "precinct", "total", "dem", "rep", "con", "gre", "wor", "ind", "wep", "lbt", "scatter", "blankvoid")
# Format
prez <- formatData(prez_raw, "President")

# Candidate names
table(prez$party)
prez <- prez %>% mutate(candidate = ifelse(
	party %in% c("DEM", "WOR", "WEP"), "Hillary Clinton",
	ifelse(party %in% c("REP", "CON"), "Donald J. Trump",
				 ifelse(party %in% c("IND", "LBT"), "Gary Johnson",
				 			 ifelse(party == "GRE", "Jill Stein",
				 			 			 ifelse(party == "TOTAL", "Total votes",
				 			 			 			 ifelse(party == "SCATTER", "Scatter",
				 			 			 			 			 ifelse(party == "BLANKVOID", "Blank and void",
				 			 			 NA)))))))) %>%
# NA party for the non-party rows
	mutate(party = ifelse(party %in% c("TOTAL", "SCATTER", "BLANKVOID"), NA,
												party))

# Match order of example csv
prez <- prez %>% select(county, precinct, office, district, party, candidate, votes)

write.csv(prez, "data-final/monroe_president_2016.csv", row.names = F, na="")

#########################################################
# Senator
#########################################################
senate_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results ussenator.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(senate_raw) <- c("district", "precinct", "total", "dem", "rep", "con", "gre", "wor", "ind", "wep", "ref", "lbt", "scatter", "blankvoid")
senate <- formatData(senate_raw, "U.S. Senate")

# Candidate names
table(senate$party)
senate <- senate %>% mutate(candidate = ifelse(
	party %in% c("DEM", "WOR", "IND", "WEP"), "Charles E. Schumer",
	ifelse(party %in% c("REP", "CON", "REF"), "Wendy Long",
				 ifelse(party == "LBT", "Alex Merced",
				 			 ifelse(party == "GRE", "Robin Laverne Wilson",
				 			 			 ifelse(party == "TOTAL", "Total votes",
				 			 			 			 ifelse(party == "SCATTER", "Scatter",
				 			 			 			 			 ifelse(party == "BLANKVOID", "Blank and void",
				 			 			 			 			 			 NA)))))))) %>%
	# NA party for the non-party rows
	mutate(party = ifelse(party %in% c("TOTAL", "SCATTER", "BLANKVOID"), NA,
												party))

# Match order of example csv
senate <- senate %>% select(county, precinct, office, district, party, candidate, votes)
write.csv(senate, "data-final/monroe_ussenator_2016.csv", row.names = F, na="")


#########################################################
# All joined, saved with OpenElections naming scheme
#########################################################
monroe <- rbind(prez, senate)
write.csv(monroe, "data-final/20161108__ny__general__monroe__precinct.csv
.csv", row.names = F, na="")
