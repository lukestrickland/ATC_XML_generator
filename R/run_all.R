source("R/1-callsign_generator.R")
source("R/2-maps_aircraft_generator.R")
source("R/3-trials_generator.R")
source("R/4-xml_combiner.R")
library(reticulate)
py_config()
#need to use python 3
source_python("python/file_creator.py")
