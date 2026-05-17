run_glm <- function(dir, verbose = FALSE){

  glm_path <- Sys.getenv("GLM_PATH")

  if(glm_path == ""){
    stop("Sys.getenv('GLM_PATH') is not set.  Use Sys.setenv('GLM_PATH'='XXXX') to set the path to the GLM executable.\n Setting the GLM_PATH to GLMAEDr uses the binaries in the GLMAEDr package.")
  }

  if(glm_path == "GLMAEDr"){
    return(GLMAEDr::run_glm(sim_folder = dir, verbose = verbose))
  }

  if(glm_path == "GLM3r"){
    return(GLM3r::run_glm(sim_folder = dir, verbose = verbose))
  }

  result <- processx::run(
    command = glm_path, wd = dir, echo = verbose, error_on_status = FALSE
  )
  if (result$status != 0L) {
    stop("GLM exited with status ", result$status, ".\n",
         if (nzchar(trimws(result$stderr))) result$stderr else result$stdout)
  }
  invisible(result)
}
