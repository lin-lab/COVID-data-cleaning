#/usr/bin/Rscript

require(reticulate)
require(data.table)

# contact: qcorbin@hsph.harvard.edu

# Stay-home order dates used in Yu group data-set and elsewhere
f_url = "https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/interventions.csv"

# python version to use
py_bin = "/n/sw/helmod/apps/centos7/Core/Anaconda3/5.0.1-fasrc01/x/bin/python3.6"
reticulate::use_python(py_bin, required = T)

# import datetime python module 
datetime <- reticulate::import("datetime")

# function to convert dates from ordinal integer format to character date
ordinalToDate <- function(v){sapply(v, function(x){
	if(is.na(x)){
		""
	}else{
		as.character(datetime$datetime$fromordinal(as.integer(x))[[1]])
	}
})}

d <- fread(f_url)

for(cn in names(d)[-c(1:3)]){
	d[[cn]] <- (ordinalToDate(d[[cn]]))
}

fwrite(d, paste0("intervention_dates_JieYingWu.csv"), quote = TRUE, row.n=F, col.n=T)



