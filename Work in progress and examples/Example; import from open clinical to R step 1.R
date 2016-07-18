library(peter.utilities)
setwd("I:/Data/MSG Support/projects/EuroSarc/LINES/Data Management/Data/09Dec2014")


path="H:/Homemade-programs/Programs/Raw"
new.folder="H:/Homemade-programs/Programs/RawR/"
trial.stub="ln_aw"
db.name="LINES_database_date"

import.open.clinical.batch(path=path,trial.stub,db.name,new.folder=new.folder)
