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
formatData <- function(dt, race, districtnum) {
	# Format numeric columns
	dt[, -c(1:2)] <- sapply(dt[, -c(1:2)], asCommaless)
	
	# District labels are listed in the rows above precinct data
	# Need to propogate down to all rows below, until next row with non-null district
	dt <- dt %>% fill(town) %>% 
		# Remove district header rows and total rows - aka rows with missing precincts
		filter(!is.na(precinct))  %>%
		# Convert data to long for Elex format
		gather(party, votes, -c(1:2)) %>%
		# Add & format identifying columns
		mutate(county = "Monroe",
					 office = race,
					 party = toupper(party),
					 district = districtnum) %>%
		# Format precinct as town + precinct number
		mutate(precinct = paste(town, precinct, sep = " ")) %>%
		# Format non-party rows
		mutate(candidate = ifelse(party == "TOTAL", "Total votes",
															ifelse(party == "SCATTER", "Scatter",
																		 ifelse(party == "BLANKVOID", "Blank and void",
																		 			 NA)))) %>%
		# NA party for the non-party rows
		mutate(party = ifelse(party %in% c("TOTAL", "SCATTER", "BLANKVOID"), NA,
													party)) %>%
		# Match order of example csv
		select(county, precinct, office, district, party, candidate, votes)
	
}

#########################################################
# President
#########################################################
prez_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results president.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(prez_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "gre", "wor", "ind", "wep", "lbt", "scatter", "blankvoid")
# Format
prez <- formatData(prez_raw, "President", NA)

# Candidate names
prez <- prez %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party %in% c("DEM", "WOR", "WEP"), "Hillary Clinton",
		ifelse(party %in% c("REP", "CON"), "Donald J. Trump",
			ifelse(party %in% c("IND", "LBT"), "Gary Johnson",
				ifelse(party == "GRE", "Jill Stein",
					NA))))))
table(prez$candidate, prez$party, useNA = "always")

write.csv(prez, "data-final/monroe_president_2016.csv", row.names = F, na="")

#########################################################
# Senator
#########################################################
senate_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results ussenator.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(senate_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "gre", "wor", "ind", "wep", "ref", "lbt", "scatter", "blankvoid")
senate <- formatData(senate_raw, "U.S. Senate", NA)

# Candidate names
senate <- senate %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party %in% c("DEM", "WOR", "IND", "WEP"), "Charles E. Schumer",
		ifelse(party %in% c("REP", "CON", "REF"), "Wendy Long",
				 ifelse(party == "LBT", "Alex Merced",
				 		ifelse(party == "GRE", "Robin Laverne Wilson",
				 			NA))))))
table(senate$candidate, senate$party, useNA = "always")

write.csv(senate, "data-final/monroe_ussenator_2016.csv", row.names = F, na="")

#########################################################
# U.S. Representative, 25th and 27th districts
#########################################################
# 25TH DISTRICT
rep25_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results rep25.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(rep25_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "wor", "ind", "wep", "ref", "scatter", "blankvoid")
rep25 <- formatData(rep25_raw, "U.S. House", 25)

# Candidate names
rep25 <- rep25 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party %in% c("DEM", "WOR", "WEP"), "Louise M. Slaughter",
		ifelse(party %in% c("REP", "CON", "IND", "REF"), "Mark W. Assini",
			NA))))

table(rep25$candidate, rep25$party, useNA = "always")

write.csv(rep25, "data-final/monroe_ushouse25_2016.csv", row.names = F, na="")

# 27TH DISTRICT
rep27_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results rep27.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(rep27_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "ind", "ref", "scatter", "blankvoid")
rep27 <- formatData(rep27_raw, "U.S. House", 27)

# Candidate names
rep27 <- rep27 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party == "DEM", "Diana K. Kastenbaum",
		ifelse(party %in% c("REP", "CON", "IND", "REF"), "Chris Collins",
			NA))))
table(rep27$candidate, rep27$party, useNA = "always")

write.csv(rep27, "data-final/monroe_ushouse27_2016.csv", row.names = F, na="")

#########################################################
# State Senate
#########################################################
nys54_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results nysenate54.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(nys54_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "ind", "ref", "scatter", "blankvoid")
nys54 <- formatData(nys54_raw, "State Senate", 54)

# Candidate names
nys54 <- nys54 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party == "DEM", "Kenan S. Baldridge",
		ifelse(party %in% c("REP", "CON", "IND"), "Pamela A. Helming",
			ifelse(party == "REF", "Floyd Rayburn",
			NA)))))
table(nys54$candidate, nys54$party, useNA = "always")

write.csv(nys54, "data-final/monroe_nysenate54_2016.csv", row.names = F, na="")


#########################################################
# All joined, saved with OpenElections naming scheme
#########################################################
monroe <- rbind(prez, senate, rep25, rep27)
write.csv(monroe, "data-final/20161108__ny__general__monroe__precinct.csv", row.names = F, na="")
