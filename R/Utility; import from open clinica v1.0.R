

# This function will run import.from.open.clinica on a series of
# text files and create a directory new.folder to put the end result in
import.open.clinical.batch=function(path,trial.stub,db.name,new.folder,del.stub=""){

  # find all files of interest
  files=list.files(paste0(path))
  # must contain trial.stub
  files=files[grep(trial.stub,files)]
  # must be text files
  files=files[grep(".txt",files)]

  print(files)
  # create folder for resulting Rdta file
  dir.create(new.folder)

  # run over each file
  trial.data=list()
    for(file in files){
      fname=gsub(".txt","",file)
      print(fname)
      if(del.stub!=""){
        fname=gsub(del.stub,"",fname)
      }

      trial.data[[fname]]=import.from.open.clinica(file,path,new.folder,dates=NULL)
      assign(fname,import.from.open.clinica(file,path,new.folder,dates=NULL))
      #print(trial.data[[fname]])
    }

  if(substr(new.folder,nchar(new.folder),nchar(new.folder))!="/"){
    new.folder=paste0(new.folder,"/")
  }

  fnames=gsub(".txt","",files)
  if(del.stub!=""){
    fnames=gsub(del.stub,"",fnames)
  }

  db.list = c(fnames,"db.list")

  save(list=db.list,file=paste0(new.folder,db.name,".RData"))

}



# This function is the first step to importing trial.data from open clinica to R.
# It imports the comma delimited text file, formats dates, deals with labels
# and unneeded variables returning a trial.data.frame
import.from.open.clinica=function(txt,path,new.folder,dates=NULL){


  # read trial.data from comma delimited text file
  if(substr(path,nchar(path),nchar(path))!="/"){
    path=paste0(path,"/")
  }
  trial.data=read.csv(paste0(path,txt),stringsAsFactors=FALSE)

  # Save all column names
  trial.data.names=names(trial.data)

  # Search for dates with the string "date" contained in their name
  dates2=trial.data.names[grep("date",names(trial.data))]
  if(!is.null(dates)){
    dates=unique(dates2,dates)
  } else {
    dates=dates2
  }

  # convert dates into posixct date-times (r date format)
  if(!is.null(dates)){
    for(i in dates){
      trial.data[i]=as.POSIXct(as.vector(t(trial.data[i])), tz = "", format="%d/%m/%Y %H:%M:%S",origin="1970-01-01")
    }
  }

  # Discard label values since R cannot cope with factors in an interilligent way
  # Transfer labels from name_label to name
  labels=trial.data.names[grep("label",names(trial.data))]
  for(label in labels){
    slab=sub("_label","",label)
    trial.data[slab]=trial.data[label]
    trial.data[label]=NULL
  }

  if("ssid" %in% trial.data.names){
    # rename ssid patid and bring to front of table
    trial.data$patid=trial.data$ssid
    trial.data$ssid=NULL
    trial.data=trial.data[,c("patid",setdiff(names(trial.data),"patid"))]
  }


  # Conservatively remove unwanted variable
  trial.data$item_trial.data_id=NULL
  trial.data$ssoid=NULL
  trial.data$oc_event_start_date=NULL
  trial.data$oc_event_end_date=NULL
  trial.data$event_status=NULL
  trial.data$event_location=NULL
  trial.data$subject_age_at_event=NULL
  trial.data$crf_status=NULL
  trial.data$interviewername=NULL
  trial.data$interviewdate=NULL
  trial.data$warehouse_insert_created_timestamp=NULL

  return(trial.data)
}


