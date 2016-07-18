addrow=function(s4object,simsToRun){
  dm=dim(simsToRun)
  nm=names(simsToRun)[2:((dim(simsToRun)[2]-2))]
  row=c(dm[1]+1,lapply(nm,getValue,s4object=s4object),FALSE)
  simsToRun[dm[1]+1,]=row
  return(simsToRun)
}
