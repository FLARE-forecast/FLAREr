#All functions are from GLMtools package
#We moved them here because GLMtools package 
#was not keeping up with R updates


#'@title read in a GLM simulation *.nml file
#'@description 
#'read in a GLM simulation *.nml file and create a list.  \cr
#'
#'
#'@param nml_file a string with the path to the GLM glm2.nml file, or 
#'\code{'template'} for loading the GLM template nml file with GLMr (default)
#'@return glm_nml a nml (a list) for GLM config
#'@keywords methods
#'@author
#'Jordan S. Read
#'@seealso \link{get_nml_value}
#'@examples 
#'glm_nml <- read_nml()
#'print(glm_nml)
#'@export
read_nml  <-	function(nml_file = 'template'){
  
  #nml_file <- nml_path_norm(nml_file)
  
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
# private function
findBlck	<-	function(nml,argName){
  
  # test for argName being a string
  if (!is.character(argName)){stop(c("parameter name must be a string"))}
  fau <- " "
  fault.string <- rep(fau,1000) # names fault matrix, only returned when empty match
  blockNames	<-	names(nml)
  blckI	<-	c()
  for (i in seq_len(length(blockNames))){
    if (any(argName %in% names(nml[[i]]))){
      blckI	<- c(blckI,i)
    } else {
      one.i <- which(fault.string==fau)[1]
      fault.string[one.i:(one.i+length(names(nml[[i]]))-1)]=names(nml[[i]])
    }
    
  }
  fault.string <- fault.string[!fault.string==fau] # is empty if found
  # test to see if a block match was made
  if (is.null(blckI)){stop(c("parameter name ",argName," not found in nml. Possible names:",paste(fault.string,collapse=', ')))}
  return(blckI)
}

# private function
setnmlList <- function(glm_nml,arg_list){
  if (!is.list(arg_list)){stop("arg_list must be a list")}
  
  if (any(nchar(names(arg_list)) == 0) | length(names(arg_list)) == 0){
    stop('arg_list must be a named list')
  }
  
  arg_names  <-	names(arg_list)
  
  for (i in seq_len(length(arg_names))){
    glm_nml <- set_nml(glm_nml,arg_name=arg_names[i],arg_val=arg_list[[i]])
  }
  
  return(glm_nml)
}

# private function
#' @importFrom utils tail
is_nml_file <- function(nml_file){
  
  is_nml <- FALSE
  fl_ext <- tail(strsplit(nml_file, "\\.")[[1]],1)
  
  if (fl_ext == 'nml'){
    is_nml <- TRUE
  }
  return(is_nml)
}

#' @importFrom utils capture.output
what_ascii <- function(file){
  response <- capture.output(showNonASCIIfile(file))
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


get_block <- function(glm_nml, arg_name, warn=TRUE){
  arg_split = strsplit(arg_name,'::')[[1]]
  if (length(arg_split) > 1){
    blck = arg_split[1]
    arg_name = get_arg_name(arg_name)
  } else{
    blck	<-	findBlck(glm_nml,arg_name)
  }
  if (length(blck) > 1){
    if (warn)
      warning(arg_name, " found in ", paste(names(glm_nml[blck]), collapse=' & '), ", returning the first. Try ",names(glm_nml[blck])[1],"::",arg_name, " for explicit match")
    blck = blck[1]
  }
  
  return(blck)
}

get_arg_name <- function(arg_name){
  arg_split = strsplit(arg_name,'::')[[1]]
  
  if (length(arg_split) > 1){
    blck = arg_split[1]
    arg_name = arg_split[2]
  }
  return(arg_name)
}

.nml <- function(list_obj){
 nml <- list_obj
class(nml) <- "nml"
invisible(nml)
}

write_nml  <-	function(glm_nml,file){
  sink(file)
  
  print(glm_nml)
  sink()
}

nml_path_norm <- function(nml_file){
  if (nml_file == "template"){
    nml_file <- GLMr::nml_template_path()
  }
  if (!is_nml_file(nml_file)){
    stop(nml_file, ' is not of file type *.nml')
  }
  
  return(nml_file)
}

nml_template_path <- function(){
  return(system.file('sim_files/glm3.nml'))
}

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
#'@export
summary.nml <- function(object,...){
  print(object,...)
}

#'@title get surface height from GLM simulation
#'@description 
#'Creates a data.frame with DateTime and surface_height.  \cr
#'
#'
#'@param file a string with the path to the netcdf output from GLM
#'@param ice.rm a boolean for including ice thickness in surface height
#'@param snow.rm a boolean for including snow depth thickness in surface height
#'@param ... additional arguments passed to \code{\link{resample_sim}}
#'@return a data.frame with DateTime and surface_height (in meters)
#'@keywords methods
#'@author
#'Jordan S. Read, Luke A. Winslow
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'surface <- get_surface_height(file = nc_file)
#'surface_w_ice <- get_surface_height(file = nc_file, ice.rm = FALSE, snow.rm = FALSE)
#'@importFrom ncdf4 ncvar_get
#'@export
get_surface_height  <-	function(file = 'output.nc', ice.rm = TRUE, snow.rm = TRUE, ...){
  glm_nc <- get_glm_nc(file)
  NS	<- 	ncvar_get(glm_nc, "NS")
  elev <- ncvar_get(glm_nc, "z")
  time <- get_time(glm_nc)
  close_glm_nc(glm_nc)
  
  surface_height <- vector(mode = "numeric",length = length(NS))
  for (j in seq_len(length(NS))){
    surface_height[j] <- elev[NS[j],j]
  }
  if (!ice.rm){
    surface_height <- surface_height + get_ice(file, snow.rm = TRUE)[, 2]
  }
  if (!snow.rm){
    snow <- get_ice(file, snow.rm = TRUE)[, 2] - get_ice(file, snow.rm = TRUE)[, 2]
    surface_height <- surface_height + snow
  }
  
  glm_surface <- data.frame('DateTime'=time, 'surface_height'=surface_height)
  
  glm_surface <- resample_sim(df = glm_surface, ...)
  
  return(glm_surface)
}

get_glm_nc  <-  function(file){
  if(length(file) < 1 || is.na(file)){
    stop('glm_nc file must be supplied string or proper file handle')
  }
  glm_nc	<- 	nc_open(file, readunlim=TRUE)
  return(glm_nc)
}
#' @importFrom ncdf4 nc_close
close_glm_nc <- function(glm_nc){
  nc_close(glm_nc)
}

# Summary: Returns the converted time vector in R format
#' @importFrom ncdf4 ncvar_get
get_time  <-  function(glm_nc){
  hours_since  <- ncvar_get(glm_nc, "time")
  time_info <- get_time_info(glm_nc)
  
  time <- time_info$startDate + time_info$time_unit * hours_since * 60*60*24
  
  return(time)
  
}

#' @importFrom ncdf4 ncatt_get ncvar_get
get_time_info <- function(glm_nc, file = NULL){
  
  day_secs = 86400
  time_unit <- 3600/day_secs
  close_nc <- FALSE #flag if we should close nc in this function
  
  #The units attribute on the time variable has basically the info we need
  if (missing(glm_nc)){
    glm_nc <- get_glm_nc(file)
    close_nc <- TRUE
  }
  time_units <- ncatt_get(glm_nc,'time','units')$value
  
  
  #It is written in prose instead of machine-readable format. Check to makes sure
  # it says "hours since ", then we know the timestep is hours. As far as I know, 
  # this never changes
  tiCheck <- regexpr('(hours since) (.*)' ,time_units, perl=TRUE)
  
  #make sure the unit string is as expected. I think 
  # the timestep is always in hours
  if(attr(tiCheck,'capture.start')[1] < 0 || attr(tiCheck,'capture.start')[2] < 0){
    stop('Unexpected time unit in NetCDF file')
  }
  
  # Get the epoch from the unit string
  epoch <- substr(time_units, attr(tiCheck,'capture.start')[2], attr(tiCheck,'capture.start')[2] + attr(tiCheck,'capture.length')[2])
  
  #get the length of the time data, will use this later
  tLen <- glm_nc$dim[["time"]][["len"]]
  
  time_info  <-  data.frame("time_unit"=time_unit)
  start_date <- coerce_date(epoch)
  time_info  <-  cbind(time_info,"startDate"=start_date)
  
  #End date/time 
  endT <- time_info$startDate + ncvar_get(glm_nc, 'time', start=tLen, count=1) * time_unit * day_secs
  
  time_info  <-  cbind(time_info,"stopDate"=endT[1])
  if (close_nc){
    close_glm_nc(glm_nc)
  }
  return(time_info)
}

coerce_date <- function(dates){
  # for non-POSIX dates
  if (!"POSIXct" %in% class(dates) || attr(dates,'tzone') == ""){
    # strip off POSIXct zone and replace w/ GMT offset
    dates <- as.POSIXct(as.character(dates), tz = get_UTM_offset())
  }
  
  return(dates)
}

get_UTM_offset <- function(){
  # local date comparison for daylight savings. Uses this to find UTM offset, which will be used as tz for POSIXct
  summer <- data.frame(NH = as.POSIXct("2011-06-01 12:00:00"), SH = as.POSIXct("2011-12-01 12:00:00"))
  dst <- c(NA, FALSE, TRUE)[as.POSIXlt(c(summer[,1], summer[,2]))$isdst + 2]
  
  use_i <- which(!dst)[1]
  UTM <- data.frame(NH = as.POSIXct("2011-06-01 12:00:00",tz = "GMT"), SH = as.POSIXct("2011-12-01 12:00:00", tz = "GMT"))
  
  if (length(use_i) == 0 | is.na(use_i)){ return("")}
  UTM_dif <- as.numeric(summer[,use_i] - UTM[,use_i]) # in hours
  sym <- ifelse(UTM_dif < 0, '-','+')
  tz <- paste0("Etc/GMT",sym, as.character(UTM_dif))
  return(tz)
}

#'@title get subset of time from a generic timeseries data.frame
#'@description 
#'resamples the input data.frame to only have rows corresponding to matches between 
#'df$DateTime and t_out. Both df$DateTime and t_out are of type POSIXct, and the 
#'precision of the match is passed in through the \code{precision} argument. 
#'\emph{The order of t_out}, not df$DateTime is retained.
#'
#'@param df a data.frame with DateTime and potentially other columns
#'@param t_out a vector of POSIXct dates (or character array that can be coerced into POSIXct) 
#'for matching to df$DateTime
#'@param method 'match' for exact match or 'interp' for temporal interpolation
#'@param precision matching precision (must be 'secs', 'mins','hours', 'days', or 'exact'). 
#'@return a data.frame with DateTime other original columns, resampled according to t_out
#'@keywords methods
#'@seealso \link{get_temp}, \link{get_wind}, \link{get_surface_height}, \link{get_evaporation}, \link{get_ice}
#'@author
#'Jordan S. Read
#'@examples 
#'sim_folder <- run_example_sim(verbose = FALSE)
#'nc_file <- file.path(sim_folder, 'output.nc')
#'temp_surf <- get_temp(nc_file, reference = 'surface', z_out = c(0,1,2))
#'t_out <- as.POSIXct(c("2011-04-01", "2011-06-14", "2011-04-05", "2011-07-28"))
#'temp_out <- resample_sim(df = temp_surf, t_out = t_out)
#'
#'t_out <- c("2011-04-01 10:00", "2011-04-05 08:15", 
#'       "2011-06-14 10:30", "2011-04-05 10:21", 
#'       "2011-07-28 10:00")
#'temp_out <- resample_sim(df = temp_surf, t_out = t_out, precision = 'days')
#'
#'temp_out <- resample_sim(df = temp_surf, t_out = t_out, method = 'interp', precision = 'hours')
#'@export
resample_sim <- function(df, t_out, method = 'match', precision = 'days'){
  
  if (missing(t_out)){
    t_out = NULL
  }
  
  if (is.null(t_out)){
    return(df)
  } 
  
  if (length(unique(t_out)) != length(t_out)){stop('t_out values must be unique')}
  
  
  t_out <- coerce_date(t_out)
  
  if (!(method %in% c("match", "interp"))){
    stop(paste0('method ', method, ' not currently supported'))
  }
  
  # wish this could be vectorized, but we need to retain the order of *t_out*, not df
  if (precision != 'exact'){
    time <- time_precision(t_out, precision)
  } else {
    time <- t_out
  }
  
  
  if (method == 'interp'){
    df <- df_interp(df, time)
    time_compr <- df$DateTime
  } else {
    time_compr <- time_precision(df$DateTime, precision)
  }
  
  idx_out <- vector(length = length(time))
  for (j in seq_len(length(time))){
    m_i <- which(time[j] - time_compr == 0) #funny, match doesn't work (lt vs ct types)
    idx_out[j] = ifelse(length(m_i)==0,NA,m_i)
  }
  
  idx_out <- idx_out[!is.na(idx_out)]
  
  df_out <- df[idx_out, ]
  
  if (nrow(df_out) == 0){
    add_msg = ''
    if (method == 'match'){
      add_msg = ". Try method = 'interp'"
    }
    warning(paste0("no matches found using method = '",method,"' at ",precision,' precision',add_msg))
  }
  
  return(df_out)
  
}