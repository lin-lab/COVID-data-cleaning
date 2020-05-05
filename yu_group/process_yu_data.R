#!/usr/bin/Rscript 

# contact: qcorbin@hsph.harvard.edu

require(reticulate)
require(data.table)

DATE = commandArgs(TRUE)[1]

# where to write output
out_dir = "cleaned_data/"

# raw pkl file to read
yu_pkl_file = paste0("raw_data/yu_data.",DATE,".pkl")

if( !file.exists(yu_pkl_file) ){
	stop(paste(yu_pkl_file, "does not exist."))
}

# county-level and state-elev output file names
county_out_file = paste0(out_dir, "/Yu_Group_County.",DATE,".csv")
state_out_file = paste0(out_dir, "/Yu_Group_State.",DATE,".csv")

# python verison to use
py_bin = "python3.6"

reticulate::use_python(py_bin, required = T)

# use reticulate to read 
# python::pandas pickle file in R

# make a temporary python script file
system("echo 'import pandas

def read_pkl(file):
    out = pandas.read_pickle(file)
    return out
' > tmp.py")
# read in the python script 
reticulate::source_python("tmp.py")
# remove the python script 
system("rm -f tmp.py")

# read in the Yu data pickle file, convert to data.table
dt_wide <- as.data.table(read_pkl(yu_pkl_file))

# format State column as character type
dt_wide[,State:=as.character(State),]

# Fix state names that were not correctly parsed
nan_states <- c("VA" = "Virginia", "HI" = "Hawaii", "AK" = "Alaska")

dt_wide[,
  State := (function(state,abrv){
    if(abrv %in% names(nan_states)){
      nan_states[abrv]
    }else{
      state
    }
  })(State,StateName),
by = list(State,StateName)]

# save as RDS file 
saveRDS(dt_wide, gsub(".pkl$", ".rds", yu_pkl_file))

# identifier columns
# id_cols = c('StateName','StateNameAbbreviation','CountyName','CountyName/StateAbbrev','FIPSStateCode','FIPSCountyCode','CensusRegionCode','CensusRegionName','CensusDivisionCode','CensusDivisionName','FederalRegionCode')
id_cols = c('countyFIPS','STATEFP','COUNTYFP','CountyName','StateName','State','lat','lon','POP_LATITUDE','POP_LONGITUDE','CensusRegionName','CensusDivisionName')

convert_to_char <- intersect(names(which(sapply(dt_wide, class) == "list")), id_cols)

for(cn in convert_to_char){
	dt_wide[[cn]] <- as.character(dt_wide[[cn]])
}
dt_wide <- as.data.table(dt_wide)


# function to convert 'wide' to 'long' format
# for no. deaths or for no. cases
get_long_dt <- function(prefix){

	prefix = paste0("^", gsub("_$", "", prefix), "_")
	name = gsub("[#-_]", "", tolower(prefix))
	var_cols = grep(prefix,names(dt_wide),value=TRUE)

	out = data.table::melt(
		dt_wide[,c(id_cols,var_cols),with=F], 
		id.vars = id_cols, measure.vars = var_cols,
		variable.name = "date", value.name = name
	)
	out[,date:= gsub(prefix,"",date),]

	out
}

# dt_nc = dt_wide[,grep("^#Cases", grep("^#Deaths", names(dt_wide), invert = T, val = T), invert = T, val = T),with=FALSE]

# combined long-format file
dt = merge(get_long_dt("#Deaths"), get_long_dt("#Cases"), by = c(id_cols, "date"))

# format date column
dt[,date := as.Date(date, format = "%m-%d-%Y"),]

# order by county, state, date
data.table::setorder(dt, STATEFP, COUNTYFP, date)

# calculate new cases and deaths within each county and state
dt[,`:=`(
	new_deaths = deaths - c(0, deaths[-.N]),
	new_cases = cases - c(0, cases[-.N])
),by = list(STATEFP,COUNTYFP)]

# write county-level file
data.table::fwrite(
	dt[,list(FIPS=countyFIPS, county=CountyName, stateName = State, date, positiveIncrease=new_cases,deathIncrease=new_deaths,positive=cases,death=deaths),][order(date,stateName,county)],
	file = county_out_file,
	sep = ',', quote = FALSE, row.names = FALSE, col.names = TRUE
)

# aggregate across counties withi state
dt_state = dt[,list(deaths = sum(deaths), cases = sum(cases)),by = list(date, State, StateName, stateFIPS = STATEFP)][order(StateName,date)]

dt_state[,`:=`(
        new_deaths = deaths - c(0, deaths[-.N]),
        new_cases = cases - c(0, cases[-.N])
),by = list(stateFIPS, State)]

# write state-level file
data.table::fwrite(
	dt_state[,list(date, stateFIPS, stateName = State, positive=cases,death=deaths, positiveIncrease=new_cases,deathIncrease=new_deaths),][order(date,stateName)],
	file = state_out_file,
	sep = ',', quote = FALSE, row.names = FALSE, col.names = TRUE
)

# done


