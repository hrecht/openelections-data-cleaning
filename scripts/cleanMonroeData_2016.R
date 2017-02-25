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
rm(prez_raw)

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
rm(senate_raw)

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

reps <- rbind(rep25, rep27)
rm(rep25, rep25_raw, rep27, rep27_raw)

#########################################################
# State Senate
#########################################################
# 54TH DISTRICT
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

# 55TH DISTRICT
nys55_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results nysenate55.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(nys55_raw) <- c("town", "precinct", "total", "rep", "con", "ind", "ref", "scatter", "blankvoid")
nys55 <- formatData(nys55_raw, "State Senate", 55)

# Candidate names
nys55 <- nys55 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party %in% c("REP", "CON", "IND", "REF"), "Rich Funke",
		NA)))
table(nys55$candidate, nys55$party, useNA = "always")

write.csv(nys55, "data-final/monroe_nysenate55_2016.csv", row.names = F, na="")

# 56TH DISTRICT
nys56_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results nysenate56.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(nys56_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "wor", "ind", "ref", "scatter", "blankvoid")
nys56 <- formatData(nys56_raw, "State Senate", 56)

# Candidate names
nys56 <- nys56 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party %in% c("DEM", "WOR"), "Ann C. Lewis",
		ifelse(party %in% c("REP", "CON", "IND", "REF"), "Joseph E. Robach",
																						 			 NA))))
table(nys56$candidate, nys56$party, useNA = "always")

write.csv(nys56, "data-final/monroe_nysenate56_2016.csv", row.names = F, na="")

# 59TH DISTRICT
nys59_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results nysenate59.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(nys59_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "ind", "ref", "scatter", "blankvoid")
nys59 <- formatData(nys59_raw, "State Senate", 59)

# Candidate names
nys59 <- nys59 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party == "DEM", "Tom Casey",
		ifelse(party %in% c("REP", "CON", "IND", "REF"), "Patrick M. Gallivan",
			NA))))
table(nys59$candidate, nys59$party, useNA = "always")

write.csv(nys59, "data-final/monroe_nysenate59_2016.csv", row.names = F, na="")

# 61ST DISTRICT
nys61_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results nysenate61.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(nys61_raw) <- c("town", "precinct", "total", "dem", "rep", "con", "gre", "wor", "ind", "wep", "ref", "scatter", "blankvoid")
nys61 <- formatData(nys61_raw, "State Senate", 61)

# Candidate names
nys61 <- nys61 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party %in% c("DEM", "WOR", "WEP"), "Thomas A. Loughran",
		ifelse(party %in% c("REP", "CON", "IND", "REF"), "Michael H. Ranzenhoffer",
			ifelse(party == "GRE", "Ruben Cartagena, Jr.",
				NA)))))
table(nys61$candidate, nys61$party, useNA = "always")

write.csv(nys61, "data-final/monroe_nysenate61_2016.csv", row.names = F, na="")

# 62ND DISTRICT
nys62_raw <- read.csv("data-export/tabula-2016 Monroe, NY precinct-level election results nysenate62.csv", stringsAsFactors = F, header = F, na.strings = "")

# Column names
colnames(nys62_raw) <- c("town", "precinct", "total", "rep", "con", "ind", "ref", "scatter", "blankvoid")
nys62 <- formatData(nys62_raw, "State Senate", 62)

# Candidate names
nys62 <- nys62 %>% mutate(candidate = ifelse(is.na(party), candidate,
	ifelse(party %in% c("REP", "CON", "IND", "REF"), "Robert G. Ortt",
		NA)))
table(nys62$candidate, nys62$party, useNA = "always")

write.csv(nys62, "data-final/monroe_nysenate62_2016.csv", row.names = F, na="")

nys <- rbind(nys54, nys55, nys56, nys59, nys61, nys62)
rm(nys54_raw, nys55_raw, nys56_raw, nys59_raw, nys61_raw, nys62_raw, nys54, nys55, nys56, nys59, nys61, nys62)

#########################################################
# NY Assembly
# Saved district #, party, and candidate names in a CSV for much more efficiency
# In the future, do it this way always
#########################################################
nya_candidates <- read.csv("data-export/monroe-candidates.csv", stringsAsFactors = F)
nya_candidates$party <- toupper(nya_candidates$party)

formatAssembly <- function(race, districtnum) {
	# Read csv
	inpath <- paste("data-export/tabula-2016 Monroe, NY precinct-level election results nyassembly", districtnum, ".csv", sep = "")
	dt <- read.csv(inpath, stringsAsFactors = F, header = F, na.strings = "")
	
	# Get colnames from CSV - parties that ran
	colnya <- nya_candidates[which(nya_candidates$district==districtnum),]$party
	colnames(dt) <- c("town", "precinct", "total", colnya, "scatter", "blankvoid")
	
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
		mutate(precinct = paste(town, precinct, sep = " "))
	
	# Join candidate names
	dt <- left_join(dt, nya_candidates, by = c("party", "district", "office"))
	dt <- dt %>%
		# Format non-party rows
		mutate(candidate = ifelse(!is.na(candidate), candidate,
					 	ifelse(party == "TOTAL", "Total votes",
															ifelse(party == "SCATTER", "Scatter",
																		 ifelse(party == "BLANKVOID", "Blank and void",
																		 			 NA))))) %>%
		# NA party for the non-party rows
		mutate(party = ifelse(party %in% c("TOTAL", "SCATTER", "BLANKVOID"), NA,
													party)) %>%
		# Match order of example csv
		select(county, precinct, office, district, party, candidate, votes)
	print(table(dt$candidate, dt$party, useNA = "always"))
	
	outpath <- paste("data-final/monroe_nyassembly", districtnum, "_2016.csv", sep="")
	write.csv(dt, outpath, row.names = F, na="")
	return(dt)
}

# Read, format, write NY Assembly 
nya133 <- formatAssembly("State Assembly", 133)
nya134 <- formatAssembly("State Assembly", 134)
nya135 <- formatAssembly("State Assembly", 135)
nya136 <- formatAssembly("State Assembly", 136)
nya137 <- formatAssembly("State Assembly", 137)
nya138 <- formatAssembly("State Assembly", 138)
nya139 <- formatAssembly("State Assembly", 139)

# Join
nya <- rbind(nya133, nya134, nya135, nya136, nya137, nya138, nya139)
rm(nya133, nya134, nya135, nya136, nya137, nya138, nya139)

#########################################################
# All joined, saved with OpenElections file naming scheme
#########################################################
monroe <- rbind(prez, senate, reps, nys, nya)
write.csv(monroe, "data-final/20161108__ny__general__monroe__precinct.csv", row.names = F, na="")
