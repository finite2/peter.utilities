
#' import Open Clinica data
#'
#' Takes text files generated from Open Clinica and does some initial formatting saving them in an R format.
#'
#' @param txt Filename
#' @param path Filepath
#' @param dates A vector of column names to convert to dates
#' @param trialStub A string to filter unwanted files from the target folder
#' @param dbName The file name for the .Rdata file will all the data contained within it
#' @param delStub A string to remove off the data.frames if the names contain lots of unwanted duplication
#'
#' @details
#'
#' \code{import.openClinica} creates a data.frame from a comma delimited file. Any column with the word date and any column named in the date variable in it is formatted to a \code{\link{POSIXct}} date
#'
#' \code{}
#'
#' (path,trialStub,dbName,newFolder,delStub="")
#'
#'
#' @export importOpenClinica
importOpenClinica=function(txt,path,dates=NULL){


  # read trialData from comma delimited text file
  if(substr(path,nchar(path),nchar(path))!="/"){
    path=paste0(path,"/")
  }
  trialData=read.csv(paste0(path,txt),stringsAsFactors=FALSE)

  # Save all column names
  trialData_names=names(trialData)

  # Search for dates with the string "date" contained in their name
  dates2=trialData_names[grep("date",names(trialData))]
  if(!is.null(dates)){
    dates=unique(dates2,dates)
  } else {
    dates=dates2
  }

  # convert dates into posixct date-times (r date format)
  if(!is.null(dates)){
    for(i in dates){
      trialData[i]=as.POSIXct(as.vector(t(trialData[i])), tz = "", format="%d/%m/%Y %H:%M:%S",origin="1970-01-01")
    }
  }

  # Discard label values since R cannot cope with factors in an interilligent way
  # Transfer labels from name_label to name
  labels=trialData_names[grep("label",names(trialData))]
  for(label in labels){
    slab=sub("_label","",label)
    trialData[slab]=trialData[label]
    trialData[label]=NULL
  }

  if("ssid" %in% trialData_names){
    # rename ssid patid and bring to front of table
    trialData$patid=trialData$ssid
    trialData$ssid=NULL
    trialData=trialData[,c("patid",setdiff(names(trialData),"patid"))]
  }


  # Conservatively remove unwanted variable
  trialData$item_trialData_id=NULL
  trialData$ssoid=NULL
  trialData$oc_event_start_date=NULL
  trialData$oc_event_end_date=NULL
  trialData$event_status=NULL
  trialData$event_location=NULL
  trialData$subject_age_at_event=NULL
  trialData$crf_status=NULL
  trialData$interviewername=NULL
  trialData$interviewdate=NULL
  trialData$warehouse_insert_created_timestamp=NULL

  return(trialData)
}

#' @describeIn importOpenClinica Runs importOpenClinica on a folder and saves to an .Rdata file.
#' @export importOpenClinica_batch
importOpenClinica_batch = function(path,trialStub,dbName,newFolder,delStub=""){

  # find all files of interest
  files=list.files(paste0(path))
  # must contain trialStub
  files=files[grep(trialStub,files)]
  # must be text files
  files=files[grep(".txt",files)]

  # print(files)
  # create folder for resulting Rdta file
  dir.create(newFolder)

  # run over each file
  fnames = c()
    for(file in files){
      fname=gsub(".txt","",file)
      print(fname)
      if(delStub!=""){
        fname=gsub(delStub,"",fname)
      }

      # trialData[[fname]]=import.from.open.clinica(file,path,newFolder,dates=NULL)
      assign(fname,importOpenClinica(file,path,dates=NULL))
      fnames = c(fnames, fname)
      #print(trialData[[fname]])
    }

  if(substr(newFolder,nchar(newFolder),nchar(newFolder))!="/"){
    newFolder=paste0(newFolder,"/")
  }

  dbList = c(fnames,"dbList")

  save(list=dbList,file=paste0(newFolder,dbName,".RData"))

}




