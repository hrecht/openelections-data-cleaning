# Clean 2016 precinct-level general elections data from Monroe county
# Exported csvs (headerless) from original PDFs using Tabula
# Formatting to match https://github.com/openelections/openelections-data-ny/blob/master/2016/20161108__ny__general__putnam__precinct.csv

library(dplyr)
library(tidyr)

# Format comma-formatted numbers as numeric
asCommaless <- function(x) {
	x <- as.numeric(gsub(",", "", x))	
}

#########################################################
# President
#########################################################
prez_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results president.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(prez_raw) <- c("district", "precinct", "total", "dem", "rep", "con", "gre", "wor", "ind", "wep", "lbt", "scatter", "blankvoid")
# Format numeric
prez_raw[, -c(1:2)] <- sapply(prez_raw[, -c(1:2)], asCommaless)

# District labels are listed in the rows above precinct data
# Need to propogate down to all rows below, until next row with non-null district
prez <- prez_raw %>% fill(district)

# Remove district header rows and total rows - aka rows with missing precincts
prez <- prez %>% filter(!is.na(precinct))

# Convert data to long for Elex format
# Format identifying columns
prez_long <- prez %>% gather(party, votes, -c(1:2)) %>%
	mutate(county = "Monroe",
				 office = "President",
				 party = toupper(party))

# Candidate names
table(prez_long$party)
prez_long <- prez_long %>% mutate(candidate = ifelse(
	party %in% c("DEM", "WOR", "WEP"), "Hillary Clinton",
	ifelse(party %in% c("REP", "CON"), "Donald J. Trump",
				 ifelse(party %in% c("IND", "LBT"), "Gary Johnson",
				 			 ifelse(party == "Green", "Jill Stein",
				 			 			 ifelse(party == "TOTAL", "Total votes",
				 			 			 			 ifelse(party == "SCATTER", "Scatter",
				 			 			 			 			 ifelse(party == "BLANKVOID", "Blank and void",
				 			 			 NA)))))))) %>%
# NA party for the non-party rows
	mutate(party = ifelse(party %in% c("TOTAL", "SCATTER", "BLANKVOID"), NA,
												party))

# Match order of example csv
prez_long <- prez_long %>% select(county, precinct, office, district, party, candidate, votes)

write.csv(prez_long, "data-final/monroe_president.csv")
