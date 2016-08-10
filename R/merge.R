

#' Merge databases
#'
#' Provides a function simular to merge in Stata to merge data from two data.frames.
#'
#' @param mergeParam A vector of column names to use to merge data
#' @param db1,db2 The data.frames to merge
#' @param entries1,entries2 1 or "m" to tell it whether to do 1:1, 1:m or m:1 matching between databases
#' @export ut.merge
#'
#' @details
#' Merges 1:1, 1:M or M:1. M:M merging is not supported because this makes unfounded assumptions about the ordering of varaibles. It is better to specify an additional mergeParam column to merge with to explicitely asign M:M matches.
#'
#' Returns a column named merged denoting 1 for db1, 2 for db2 and 3 for row found in both. Note this will overwrite previous merge columns if they exist.
#'
ut.merge=function(mergeParam,db1,db2,entries1,entries2){


  if(entries1==1){
    if(sum(duplicated(db1[mergeParam]))>0){
      stop("First database entries are no uniquely identified")
    }
  }
  if(entries2==1){
    if(sum(duplicated(db2[mergeParam]))>0){
      stop("Second database entries are no uniquely identified")
    }
  }


  if(entries1=="m" | entries2==1){
    db=db1
    dbAdd=db2
    dbSwitch=1
  } else if(entries1==1 & entries2=="m") {
    db=db2
    dbAdd=db1
    dbSwitch=2
  } else {
    stop("Matching entries must be 1:1, 1:m or m:1")
  }

  colNames=unique(c(colnames(db),colnames(dbAdd)))
  colNames2=colnames(dbAdd)
  colNames2MinusIntersect=colNames2[!colNames2 %in% intersect(colnames(db),colNames2)]


  classes=lapply(dbAdd, class)
  db$merge=dbSwitch
  # add new columns as empty columns
  for(column in colNames[!colNames %in% colnames(db)]){
    db[column]=NA
    class(db[[column]])=classes[[column]]


  }

  dbLen=dim(db)[1]
  # merge dbAdd to db
  if(dim(dbAdd)[1] > 0){
    for(i in 1:dim(dbAdd)[1]){
      match=NULL
      for(j in 1:dim(db)[1]){
        if(all(dbAdd[i,mergeParam]==db[j,mergeParam])){
          match=c(match,j)
        }
      }
      #print(duplicated(rbind(dbAdd[mergeParam][i,],db1[mergeParam])))
      #print(rbind(db1[mergeParam],dbAdd[mergeParam][i,]))
      #print(match)
      if(length(match)==0){
        # no match found
        db[dbLen+1,]=NA
        db$merge[dbLen+1]=3-dbSwitch

        db[dbLen+1,colNames2]=dbAdd[i,]
        dbLen=dbLen+1
      } else {
        # match found
        db[match,colNames2MinusIntersect]=dbAdd[i,colNames2MinusIntersect]
        db$merge[match]=3
      }
    }
  }




  return(db)
}
