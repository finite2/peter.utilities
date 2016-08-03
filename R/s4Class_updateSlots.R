#' @export updateSlots
updateSlots=function(rowID,simsToRun,s4object){

  for(txtVar in names(simsToRun)[2:((dim(simsToRun)[2]-2))]){
    if(grepl("_",txtVar)){
      i2=str_split(txtVar,"_")[[1]]
      slot(slot(s4object,i2[1]),i2[2])=simsToRun[rowID,txtVar]
    } else {
      slot(s4object,txtVar)=simsToRun[rowID,txtVar]
    }
  }

  return(s4object)
}
