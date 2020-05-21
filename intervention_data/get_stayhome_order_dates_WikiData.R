#!/usr/bin/env Rscript

require(data.table)
require(SPARQL)
require(usmap)

# -----------------------------------------------
#  Code to retrieve county and state-level stay-home
#  order dates from WikiData
# -----------------------------------------------

# contact: qcorbin@hsph.harvard.edu

# Notes:
#	References for all orders and dates can be found
#	from WikiData pages for each code listed in d$item.

# function for converting timestamps to R date format
ts2date <- function(x) as.Date(gsub("T.*", "", x), format = "%Y-%m-%d")

# return NA if [all/any] values missing, max non-missing otherwise
max.nm <- function(x, check_na_fun = all ){
	max(x, na.rm = !check_na_fun(is.na(x)) )
}

# return NA for [all/any] values missing, min non-missing otherwise
min.nm <- function(x, check_na_fun = all ){
	min(x, na.rm = !check_na_fun(is.na(x)) )
}

# -----------------------------------------------
#  Get US-wide county-level FIPS from usmap data
# -----------------------------------------------

fips_state <- as.data.table(utils::read.csv(system.file("extdata", "state_fips.csv",package = "usmap")))
setnames(fips_state, c('state', 'fips_state', 'stateName'))

fips_county <- as.data.table(utils::read.csv(system.file("extdata", "county_fips.csv", package = "usmap")))
setnames(fips_county, c('stateName','state', 'county', 'fips'))
#http://www.r-bloggers.com/sparql-with-r-in-less-than-5-minutes/

fips_dt <- merge(fips_state, fips_county, by = c('state', 'stateName'), all = TRUE)


# -----------------------------------------------
#  Retreive stay-home orders from WikiData
# -----------------------------------------------

query <- '
#COVID-19 Lockdown Orders
SELECT DISTINCT ?item ?itemLabel ?idLabel ?locLabel ?typeLabel ?residLabel ?stateLabel ?fips ?startLabel ?endLabel 
WHERE
{
	?item wdt:P31 wd:Q88509703 .
	OPTIONAL { ?item wdt:P1001 ?id . }
	OPTIONAL { ?id wdt:P131 ?resid . }
	OPTIONAL { ?id wdt:P131 ?state . ?state wdt:P31 wd:Q35657} 
	OPTIONAL {?id wdt:P373 ?loc} 
	OPTIONAL { ?id wdt:P882 ?fips. }
	OPTIONAL { ?id wdt:P31 ?type. }
	OPTIONAL { ?id wdt:P1705 ?name. }
	OPTIONAL { ?item wdt:P580 ?start. }
	OPTIONAL { ?item wdt:P582 ?end. }
	SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
'

endpoint <- "https://query.wikidata.org/sparql"
useragent <- paste("WDQS-Example", R.version.string)

qd <- SPARQL(endpoint,query,curl_args=list(useragent=useragent))
df <- qd$results

# remove language tags from fields
for(cl in grep("Label", colnames(df), value = TRUE)){
	df[[cl]] <- gsub('@en$', '', gsub('"', '', df[[cl]]))
}
d <- as.data.table(df)
rm(df)

# -----------------------------------------------
#  Extract state-level stay-home orders
# -----------------------------------------------

# extract state orders and collapse (if needed)
d_state <- subset(d, 
	typeLabel == "state of the United States" & 
	idLabel %in% fips_state$stateName
)[,list(
	stateName = idLabel, 
	start_state = ts2date(startLabel), 
	end_state = ts2date(endLabel)
),][,list(
	start_state = min.nm(start_state, all),
	end_state = max.nm(end_state, any)
),by=stateName]

# Manually add in missing dates for 2 states
d_state_extra <- data.table(
	stateName = c("Massachusetts", "Kentucky"),
	start_state = ts2date(c("2020-03-31", "2020-03-26")),
	end_state  = ts2date(c("2020-05-18", "2020-05-22"))
)
# Massachusetts reference: 
#    https://www.mass.gov/info-details/covid-19-state-of-emergency
# Kentucky reference: 
#    https://governor.ky.gov/attachments/20200325_Executive-Order_2020-257_Healthy-at-Home.pdf

# collapse a 2nd time in case MA or KY has been updated
d_state <- as.data.table(rbind(d_state_extra, d_state))[,list(
	start_state = min.nm(start_state, all),
	end_state = max.nm(end_state, any)
),by=stateName]


# -----------------------------------------------
#     Process county-level orders
# ----------------------------------------------- 

# fill in missing state names from location field
d$stateLabel[is.na(d$stateLabel)] <- gsub(".*, ", "",
	d$locLabel[is.na(d$stateLabel)]
)

# fill in missing state names for county names
# that only exist in a single state. 
uniq_fips_county <- fips_county
uniq_fips_county[,m := .N, by = county]
uniq_fips_county <- subset(uniq_fips_county, m == 1)

d$stateLabel[is.na(d$stateLabel)] <- uniq_fips_county$stateName[
	match(d$residLabel[is.na(d$stateLabel)], uniq_fips_county$county)
]

# extracte and collapse county and city-level orders
dss <- unique(subset(d, !(idLabel %in% fips_state$stateName))[,list(
	item = item,
	label = gsub(".*2020[ ,]*", "", itemLabel),
	place = idLabel,
	type = typeLabel,
	part_of = residLabel,
	stateName = stateLabel,
	fips = fips,
	start = ts2date(startLabel),
	end = ts2date(endLabel)
),])

# Two Indian reservations map to multiple states. 
# Remove these. 
dss[,
	no_states_in := length(unique(part_of[part_of %in% fips_state$stateName]))
,by = list(item, place)]

dss <- subset(dss, no_states_in <= 1)

# collapse orders by county, keeping the minimum start date and
# maximum end date for each county
dss <- dss[,list(start = min(start), end = max(end)),by = list(label, part_of, place, stateName, fips)]

# convert FIPS code to integer (consistent w usmap)
dss$fips <- as.integer(dss$fips)

# make a unique "County-Name, State-Name" key to fill
# in any missing FIPS codes
dss[,key := paste0(part_of, ', ', stateName),]

fips_dt[,key := paste0(county, ', ', stateName),]

# Only keep entries with either (a) known FIPS code, 
# or (b) county-state key present in usmap fips_dt
dss <- subset(dss, 
	!is.na(fips) | key %in% fips_dt$key
)

# Fill in missing FIPS codes
dss$fips[is.na(dss$fips)] <- fips_dt$fips[
	match(dss$key[is.na(dss$fips)], fips_dt$key)
]

# Remove any entries that we couldn't map
dss <- subset(dss, !is.na(fips))

# Extract order type/name
dss[,type := gsub(place, "", label),by=place]
dss[,type := gsub("^[,;. ]*", "", type),]
dss[,type := gsub("[,;. ]*$", "", type),]

dss[,type := gsub(".*stay", "stay", type),]
dss[,type := gsub(".*safe", "safe", type),]
dss[,type := gsub(".*shelter", "shelter", type),]

# Create final county-level order data.table
d_county <- dss[,list(
	start_county = min.nm(start, all),
	end_county = max.nm(end, any)
),by = fips]

# -----------------------------------------------
#  Merge county and state orders
# -----------------------------------------------

out <- merge(
	x = fips_dt, 
	y = d_state, 
	by = 'stateName',
	all.x = TRUE, all.y = FALSE
)
out <- merge(
	x = out, 
	y = d_county, 
	by = 'fips',
	all.x = TRUE, all.y = FALSE
)

# Use min(state-start, county-start) as final stay-home
# start date within each county
out[,`:=`(
	start = min.nm(c(start_state, start_county), all),
	end = if( is.na(start_county) ){ end_state }else if( is.na(start_state) ){ end_county }else{ max.nm(c(end_county, end_state), any) }
),by=fips]

# stay-home order from Yu data set

# dc <- fread("intervention_dates.csv")[,c(1:4)]
# setnames(dc, c('fips', 'state', 'county', 'stayhome'))
# dc[,stayhome := ts2date(stayhome),]

# out <- merge(y = out, x = dc[,list(fips = as.integer(fips), stayhome),], by = 'fips', all.y = TRUE, all.x = FALSE)

# output file with both county and state order dates
fwrite(out, paste0("intervention_dates_all_WikiData.csv"))

# collapsed output file
fwrite(out[,list(fips, stateName, county, stayhome_start = start, stayhome_end = end),], "intervention_dates_simple_WikiData.csv")



