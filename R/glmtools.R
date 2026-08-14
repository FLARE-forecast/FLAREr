#All functions are from GLMtools package
#We moved them here because GLMtools package
#was not keeping up with R updates


#' Read a GLM/ELCOM namelist file into an R list
#'
#' @param nml_file Path to a \code{.nml} file.
#' @return A named list of class \code{"nml"} with one element per namelist block.
#' @export
read_nml  <-	function(nml_file = 'template'){

  if (!ascii_only(nml_file)){
    stop('non-ASCII characters found in nml file on line ', what_ascii(nml_file))
  }
  # skip all commented lines, return all variables and associated values
  # requires NO return line variables (all variables must be completely defined on a single line)
  c <- file(nml_file,"r")
  fileLines <- readLines(c)
  close(c)
  lineStart	<-	substr(fileLines,1,1)
  # ignore comment lines or empty lines
  ignoreLn	<-	lineStart=='!' | fileLines==""
  lineStart	<-	lineStart[!ignoreLn]
  fileLines	<-	fileLines[!ignoreLn]
  # find all lines which start with "&" * requires FIRST char to be value

  lineIdx		<- seq(1,length(lineStart))
  blckOpen	<-	lineIdx[lineStart=="&"]
  blckClse	<-	lineIdx[lineStart=="/"]

  nml <- list()
  for (i in seq_len(length(blckOpen))){
    blckName   <-	substr(fileLines[blckOpen[i]],
                         2, nchar(fileLines[blckOpen[i]]))
    blckName   <- gsub("\\s", "", blckName)
    oldNms	   <-	names(nml)
    nml[[i]]   <-	list()
    names(nml) <-	c(oldNms,blckName)

    carryover <- ''

    for (j in (blckOpen[i]+1):(blckClse[i]-1)){

      textLine	<-	paste(carryover,
                        gsub("\t", "", gsub(" ", "", fileLines[j])), sep = '')

      if(substr(textLine, 1, 1) != '!'){
        # Add a check here, sometimes, if there is a hanging comma,
        #and only sometimes that means add next row
        if(substr(textLine, nchar(textLine), nchar(textLine)) == ',' &&
           j+1 <= length(fileLines) &&
           !any(grep("=", fileLines[j + 1])) &&
           !any(grep("/", fileLines[j + 1]))){

          carryover = textLine
          next
        }else{
          carryover = ''
        }
        # else, line is commented out
        lineVal	  <-	buildVal(textLine, lineNum = j, blckName)

        if(names(lineVal) %in% c("start","stop")){
          lineVal[1] <- paste0(substring(lineVal[1],1,10)," ",substring(lineVal[1],11,15))
        }
        nml[[i]]	<-	c(nml[[i]], lineVal)
      }
    }
  }
  nml <- .nml(nml)
  return(nml)
}
# private function
buildVal	<-	function(textLine, lineNum, blckName){
  #-----function appends nml list with new values-----
  # remove all text after comment string
  textLine	<-	strsplit(textLine,'!')[[1]][1]

  if (!any(grep("=", textLine))){
    stop(c("no hanging lines allowed in .nml, used ",textLine,'.\nSee line number:',lineNum,' in "&',blckName,'" section.'))
  }
  params	<-	strsplit(textLine,"=") # break text at "="
  parNm	  <-	params[[1]][1]
  parVl	  <-	params[[1]][2]
  # figure out what parval is...if string, remove quotes and keep as string
  # ***for boolean text, use "indentical" so that 0!= FALSE
  # can be: string, number, comma-sep-numbers, or boolean

  # special case for date:
  if (is.na(parVl)){
    stop('Empty values after "', textLine, '" on line ', lineNum,
         '. \nPerhaps the values are on the next line?', call. = FALSE)
  }
  if (nchar(parVl>17) & substr(parVl,14,14)==':' & substr(parVl,17,17)==':'){
    parVl<-paste(c(substr(parVl,1,11),' ',substr(parVl,12,nchar(parVl))),collapse='')
  }
  if (any(grep("'",parVl))){

    parVl	<-	gsub("'","",parVl)
  }else if (any(grep("\"",parVl))){
    parVl  <-	gsub("\"","",parVl)
  }else if (isTRUE(grepl(".true.",parVl) || grepl(".false.",parVl))){
    logicals <- unlist(strsplit(parVl,","))
    parVl <- from.glm_boolean(logicals)
  }else if (any(grep(",",parVl))){	# comma-sep-nums
    parVl	<-	c(as.numeric(unlist(strsplit(parVl,","))))
  }else {	# test for number
    parVl	<-	as.numeric(parVl)
  }
  lineVal	<-	list(parVl)
  names(lineVal)	<-	parNm
  return(lineVal)
}

#' go from glm2.nml logical vectors to R logicals
#'
#' @param values a vector of strings containing either .false. or .true.
#' @return a logical vector
#' @keywords internal
#' @noRd
from.glm_boolean <- function(values){

  logicals <- sapply(values, FUN = function(x){
    if (!isTRUE(grepl(".true.", x) || grepl(".false.", x))){
      stop(x, ' is not a .true. or .false.; conversion to TRUE or FALSE failed.',
           call. = FALSE)
    }
    return(ifelse(isTRUE(grepl(".true.", x)), TRUE, FALSE))
  })
  return(as.logical(logicals))
}

to.glm_boolean <- function(values){
  val.logical <- values
  values[val.logical] <- '.true.'
  values[!val.logical] <- '.false.'
  return(values)
}

#' @importFrom utils capture.output
what_ascii <- function(file){
  response <- capture.output(tools::showNonASCIIfile(file))
  return(response)
}

ascii_only <- function(file){
  response <- what_ascii(file)


  if (length(response) > 0){
    return(FALSE)
  } else {
    return(TRUE)
  }

}


.nml <- function(list_obj){
 nml <- list_obj
class(nml) <- "nml"
invisible(nml)
}

#' Write an nml list to a namelist file
#'
#' @param glm_nml A named list of class \code{"nml"} (from \code{read_nml}).
#' @param file Path of the output \code{.nml} file.
#' @return Invisibly, \code{NULL}.
#' @export
write_nml  <-	function(glm_nml,file){
  sink(file)

  print(glm_nml)
  sink()
}

#' Print NML
#'
#' @param x An object of class \code{"nml"}.
#' @param ... Ignored.
#' @return Invisibly, \code{x}.
#' @method print nml
#' @export
print.nml <- function(x, ...){
  glm_nml <- x
  for (i in seq_len(length(names(glm_nml)))){ # these are the blocks
    blckNm  <-	names(glm_nml)[i]
    cat("&")
    cat(blckNm)
    cat('\n')
    blckList	<-	glm_nml[[i]]
    for (j in seq_len(length(names(blckList)))){
      cat('   ')
      cat(names(blckList)[j])
      cat(' = ')
      if (length(blckList[[j]])>1){
        if (is.logical(blckList[[j]])){
          charText <- to.glm_boolean(blckList[[j]])
        } else {
          charText <- c(blckList[[j]])
        }
        writer	<-	paste(charText,collapse=', ')
      } else if (is.character(blckList[[j]])) {
        charText <- strsplit(blckList[[j]],',')
        writer <- paste(c("'",paste(c(charText[[1]]),collapse="','"),"'"),collapse='')
      } else if (is.logical(blckList[[j]])){
        writer <- to.glm_boolean(blckList[[j]])
      } else {
        writer <- blckList[[j]]
      }
      cat(writer)
      cat('\n')
    }
    cat('/\n')
  }
}
#' @method summary nml
#' @export
summary.nml <- function(object,...){
  print(object,...)
}

