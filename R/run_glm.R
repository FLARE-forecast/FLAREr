run_glm <- function(dir, verbose = FALSE){

  os       <- Sys.info()[["sysname"]]
  glm_path <- Sys.getenv("GLM_PATH")

  if(glm_path == ""){
    stop("Sys.getenv('GLM_PATH') is not set.  Use Sys.setenv('GLM_PATH'='XXXX') to set the path to the GLM executable.\n Setting the GLM_PATH to GLM3r uses the binaries in GLM3r package.")
  }

  # Resolve executable path and dylib directory before changing the working
  # directory so that system.file() lookups run in the original environment.
  if(os == "Darwin"){
    if(glm_path == "GLM3r"){
      dylib_path <- system.file("exec", package = "GLM3r")
      glm_path   <- system.file("exec/macglm3", package = "GLM3r")
    }else{
      dylib_path <- ""
    }
  }else if(os == "Windows"){
    if(glm_path == "GLM3r"){
      glm_path <- system.file("extbin/glm-3.0.5_x64/glm.exe", package = "GLM3r")
    }
    dylib_path <- ""
  }else{
    if(glm_path == "GLM3r"){
      dylib_path <- system.file("exec", package = "GLM3r")
      glm_path   <- system.file("exec/nixglm", package = "GLM3r")
    }else{
      dylib_path <- ""
    }
  }

  # Change into the member directory once; on.exit restores it unconditionally
  # so the working directory is always left clean regardless of errors or early
  # returns — no need for finally blocks in each tryCatch below.
  origin <- getwd()
  setwd(dir)
  on.exit(setwd(origin), add = TRUE)

  tryCatch({
    if(os == "Darwin"){
      if(verbose){
        out <- system2(glm_path, wait = TRUE, stdout = "", stderr = "", args = "")
      }else{
        out <- system2(glm_path, wait = TRUE, stdout = NULL, stderr = NULL, args = "")
      }
    }else if(os == "Windows"){
      if(verbose){
        out <- system2(glm_path, wait = TRUE, stdout = "", stderr = "", args = "--no-gui")
      }else{
        out <- system2(glm_path, wait = TRUE, stdout = NULL, stderr = NULL, args = "--no-gui")
      }
    }else{
      if(verbose){
        out <- system2(glm_path, wait = TRUE, stdout = "",   stderr = "",   args = "",
                       env = paste0("DYLD_LIBRARY_PATH=", dylib_path))
      }else{
        out <- system2(glm_path, wait = TRUE, stdout = NULL, stderr = NULL, args = "",
                       env = paste0("DYLD_LIBRARY_PATH=", dylib_path))
      }
    }
    out
  }, error = function(err){
    print(paste("GLM_ERROR:  ", err))
  })
}
