

#' Merge databases
#'
#' Provides a function simular to merge in Stata to merge data from two data.frames.
#'
#' @param merge.param A vector of column names to use to merge data
#' @param db1,db2 The data.frames to merge
#' @param entries1,entries2 1 or "m" to tell it whether to do 1:1, 1:m or m:1 matching between databases
#' @export ut.merge
ut.merge=function(merge.param,db1,db2,entries1,entries2){


  if(entries1==1){
    if(sum(duplicated(db1[merge.param]))>0){
      stop("First database entries are no uniquely identified")
    }
  }
  if(entries2==1){
    if(sum(duplicated(db2[merge.param]))>0){
      stop("Second database entries are no uniquely identified")
    }
  }


  if(entries1=="m" | entries2==1){
    db=db1
    db.add=db2
    db.switch=1
  } else if(entries1==1 & entries2=="m") {
    db=db2
    db.add=db1
    db.switch=2
  } else {
    stop("Matching entries must be 1:1, 1:m or m:1")
  }

  col.names=unique(c(colnames(db),colnames(db.add)))
  col.names.2=colnames(db.add)
  col.names.2.minus.intersect=col.names.2[!col.names.2 %in% intersect(colnames(db),col.names.2)]


  classes=lapply(db.add, class)
  db$merge=db.switch
  # add new columns as empty columns
  for(column in col.names[!col.names %in% colnames(db)]){
    db[column]=NA
    class(db[[column]])=classes[[column]]


  }

  db.length=dim(db)[1]
  # merge db2 to db1
  for(i in 1:dim(db.add)[1]){
    match=NULL
    for(j in 1:dim(db)[1]){
      if(all(db.add[i,merge.param]==db[j,merge.param])){
        match=c(match,j)
      }
    }
    #print(duplicated(rbind(db.add[merge.param][i,],db1[merge.param])))
    #print(rbind(db1[merge.param],db.add[merge.param][i,]))
    #print(match)
    if(length(match)==0){
      # no match found
      db[db.length+1,]=NA
      db$merge[db.length+1]=3-db.switch

      db[db.length+1,col.names.2]=db.add[i,]
      db.length=db.length+1
    } else {
      # match found
      db[match,col.names.2.minus.intersect]=db.add[i,col.names.2.minus.intersect]
      db$merge[match]=3
    }
  }




  return(db)
}
