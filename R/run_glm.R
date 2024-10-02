run_glm <- function(dir, verbose = FALSE){

  os <- Sys.info()[['sysname']]

  origin <- getwd()
  setwd(dir)

  glm_path <- Sys.getenv("GLM_PATH")

  if(glm_path == ""){
    stop("Sys.getenv('GLM_PATH') is not set.  Use Sys.setenv('GLM_PATH'='XXXX') to set the path to the GLM executable.\n Setting the GLM_PATH to GLM3r uses the binaries in GLM3r package.")
  }

    ### macOS ###
    if(os == "Darwin") {

      if(glm_path == "GLM3r"){
        dylib_path <- system.file("exec", package = "GLM3r")
        glm_path <- glm_path <- system.file("exec/macglm3", package = "GLM3r")
      }else{
        dylib_path <- ""
      }

      tryCatch({
        if (verbose){
         # out <- system2(glm_path, wait = TRUE, stdout = "",
        #                 stderr = "", args = "--no-gui", env = paste0("DYLD_LIBRARY_PATH=", dylib_path))
          out <- system2(glm_path, wait = TRUE, stdout = "", stderr = "", args = "--no-gui")
        }else{
          #out <- system2(glm_path, wait = TRUE, stdout = NULL,
          #               stderr = NULL, args = "--no-gui", env = paste0("DYLD_LIBRARY_PATH=", dylib_path))
          out <- system2(glm_path, wait = TRUE, stdout = NULL, stderr = NULL, args = "--no-gui")
        }
      }, error = function(err) {
        print(paste("GLM_ERROR:  ",err))
      }, finally = {
        setwd(origin)
        return(out)
      })
    } else if(os == "Windows"){
      tryCatch({

        if(glm_path == "GLM3r"){
          glm_path <- system.file("extbin/glm-3.0.5_x64/glm.exe", package = "GLM3r")
        }

        if (verbose){
          out <- system2(glm_path, wait = TRUE, stdout = "", stderr = "", args = "--no-gui")
        }else{
          out <- system2(glm_path, wait = TRUE, stdout = NULL, stderr = NULL , args = "--no-gui")
        }
      }, error = function(err) {
        print(paste("GLM_ERROR:  ",err))
      }, finally = {
        setwd(origin)
        return(out)
      })
    }else { #LINUX

      if(glm_path == "GLM3r"){
        dylib_path <- system.file("exec", package = "GLM3r")
        glm_path <- glm_path <- system.file("exec/nixglm", package = "GLM3r")
      }else{
        dylib_path <- ""
      }

      tryCatch({
        if (verbose){
          out <- system2(glm_path, wait = TRUE, stdout = "",
                         stderr = "", args = "--no-gui", env = paste0("DYLD_LIBRARY_PATH=", dylib_path))
        }else{
          out <- system2(glm_path, wait = TRUE, stdout = NULL,
                         stderr = NULL, args = "--no-gui", env = paste0("DYLD_LIBRARY_PATH=", dylib_path))
        }
      }, error = function(err) {
        print(paste("GLM_ERROR:  ",err))
      }, finally = {
        setwd(origin)
        return(out)
      })
    }
  }
