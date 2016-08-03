#' @export getValue
getValue=function(txtVar,s4object){
  if(grepl("_",txtVar)){
    i2=str_split(txtVar,"_")[[1]]
    return((slot(slot(s4object,i2[1]),i2[2])))
  } else {
    return(slot(s4object,txtVar))
  }
}
