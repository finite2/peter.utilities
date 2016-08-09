

#' query
#'
#' A function for testing queries on some data. This function uses the gmp package to match data queries which were asked previously.
#'
#' @param q An object of reference class dataQueries
#' @param data The database to check is valid
#' @param validation a quote object to test with the data
#' @param CRF The CRF name (string)
#' @param mess The message to return if any data fails the validation
#' @param parameters a vector of column names to append to the message (string)
#' @param patid Name for the main identifier column. Repeat line can take a second. Any others should be added to parameters.
#' @param repeatLine1,repeatLine2 Name of a line reference column when data is in long format with respect to patid
#' @param reject Logical should the validation be TRUE or FALSE to report the query?
#' @param prnt Logical should the number of failed queries be returned as a message?
#'
#' @details
#'
#' \code{queryQ()} is a shorthand form which means the parameters do not need to be passed to the function. Instead they must be correctly named in the current namespace. This is currently only tested in the global environment and may not work in other environments such as creating .Rmd files.
#'
#' @example examples/query_example.R
#' @import gmp
#' @importFrom gmp nextprime
#' @importFrom gmp as.bigz
#' @importFrom gmp %*%
#' @export query
query = function(q, data, validation, CRF, mess, parameters = NULL, patid = "patid", repeatLine1 = NULL, repeatLine2 = NULL, reject = TRUE, prnt = TRUE){
  mod = as.bigz("900000000000000046043660025881") # nextprime(10^30 - 10^29)
  nme = names(data)

  ev = if(reject){
    sapply(with(data,eval(validation)), function(x) isTRUE(x))
  } else {
    sapply(with(data,!eval(validation)), function(x) isTRUE(x))
  }
  dsub = data[ev,]

  dma = dim(dsub)
  counter = c(0,0)
  if(dma[1] > 0){
    for(i in 1:dma[1]){
      a = 0
      text = paste0(dsub[i,patid], CRF, repeatLine1, repeatLine2, mess, paste0(validation,collapse = ","))
      code = .encode(text)
      dm = dim(q$q)

      if(dm[1] > 0){
        if(code %in% q$q$identifier){
          a = 1
          counter[2] = counter[2] + 1
          lineNumber = which(code == q$q$identifier)

          if(q$q$queryRun[lineNumber] != q$queryRun){
            q$q$STATSresolved[lineNumber] = "No"
            q$q$firstQuery[lineNumber] = "No"

          }
          q$q$queryRun[lineNumber] = q$queryRun
        }
      }
      if(a == 0){
        counter[1] = counter[1] + 1
        thisMessage = mess
        if(!is.null(parameters)){
          thisMessage = paste(thisMessage,"(")
          for(p in 1:length(parameters)) {
            thisMessage = paste0(thisMessage, parameters[p]," = ", dsub[i,parameters[p]],ifelse(p == length(parameters),"",", "))
          }
          thisMessage = paste0(thisMessage,")")
        }

        q$q[dm[1]+1,] <- c(dm[1]+1, code, q$queryRun, dsub[i,patid], CRF, ifelse(is.null(repeatLine1),"1",dsub[i,repeatLine1]), ifelse(is.null(repeatLine2),"1",dsub[i,repeatLine2]), "Yes", thisMessage, "", "","No")
      }
    }
    if(prnt){
      message("New queries: ", counter[1], " Unresolved queries: ", counter[2])
    }
  }
}

#' @describeIn query A short hand version which will find the parameters from the global environment
#' @export queryQ
queryQ = function(){

  if(!exists("patid", mode = "any")){
    patid = "patid"
  }
  if(!exists("repeatLine")){
    repeatLine = NA
  }
  if(!exists("prnt")){
    prnt = TRUE
  }
  if(!exists("parameters")){
    parameters = NULL
  }
  if(!exists("reject")){
    reject = TRUE
  }

  mod = as.bigz("900000000000000046043660025881") # nextprime(10^30 - 10^29)
  nme = names(data)

  ev = if(reject){
    sapply(with(data,eval(validation)), function(x) isTRUE(x))
  } else {
    sapply(with(data,!eval(validation)), function(x) isTRUE(x))
  }
  dsub = data[ev,]

  dma = dim(dsub)
  # print(dsub)
  counter = c(0,0)
  if(dma[1] > 0){
    for(i in 1:dma[1]){
      a = 0
      text = paste0(dsub[i,patid], CRF, repeatLine1, repeatLine2, mess, paste0(validation,collapse = ","))
      code = .encode(text)
      dm = dim(q$q)

      if(dm[1] > 0){
        if(code %in% q$q$identifier){
          a = 1
          counter[2] = counter[2] + 1
          lineNumber = which(code == q$q$identifier)

          if(q$q$queryRun[lineNumber] != q$queryRun){
            q$q$STATSresolved[lineNumber] = "No"
            q$q$firstQuery[lineNumber] = "No"

          }
          q$q$queryRun[lineNumber] = q$queryRun
        }
      }
      if(a == 0){
        counter[1] = counter[1] + 1
        thisMessage = mess
        if(!is.null(parameters)){
          thisMessage = paste(thisMessage,"(")
          for(p in 1:length(parameters)) {
            thisMessage = paste0(thisMessage, parameters[p]," = ", dsub[i,parameters[p]],ifelse(p == length(parameters),"",", "))
          }
          thisMessage = paste0(thisMessage,")")
        }

        q$q[dm[1]+1,] <- c(dm[1]+1, code, q$queryRun, dsub[i,patid], CRF, ifelse(is.null(repeatLine1),"1",dsub[i,repeatLine1]), ifelse(is.null(repeatLine2),"1",dsub[i,repeatLine2]), "Yes", thisMessage, "", "","No")
      }
    }
    if(prnt){
      message("New queries: ", counter[1], " Unresolved queries: ", counter[2])
    }
  }
}

.hexNumber = function(text){
  paste0(sapply(as.character(utf8ToInt(text)), function(x) ifelse(nchar(x) < 3, paste0(rep("0",3-length(x)),x),x)),collapse = "")
}

.encode = function(text){
  num = .hexNumber(text)
  big = as.bigz(paste0(1,num), mod = "900000000000000046043660025881") # mod = nextprime(10^30 - 10^29)
  return(as.character(numerator(big)))
}


