
#' Sets up a List object with file paths for pre-and-postprocessing
#'
#' \code{jc_get_path_setup} Reads path information from a setup file, then does
#' various checks and creates the necessary folders for pre-processing, 
#' calibration and  running a Juno Cassandra model, and post-processing results.
#'
#' @param paths_setup_file Excel file with path setup information for jcass. It
#' is assumed this file has a sheet named 'paths' and that this sheet is in the
#' proper template format
#' @export
jc_get_path_setup <- function(paths_setup_file) {
  
  paths_setup_sheet <- "paths"
  path_info <- as.data.frame(read_xlsx(paths_setup_file, paths_setup_sheet))
  row.names(path_info) <- path_info$parameter
  
  # Get Path & File info from path data (replace backslash with front-slash for R)
  root_folder <- gsub("\\\\", "/", path_info[["root_folder", "value"]])
  scripts_folder <- gsub("\\\\", "/", path_info[["r_scripts_folder", "value"]])
  project_folder <- gsub("\\\\", "/", path_info[["project_folder", "value"]])
  jcass_exe <- gsub("\\\\", "/", path_info[["jcass_executable", "value"]])
  customiser_dll <- gsub("\\\\", "/", path_info[["customiser_dll", "value"]])
  
  #----------     Run checks to ensure specified files and folder exist     ------
  #
  # NOTE: this checking requires absolute paths
  if (!dir.exists(root_folder)) { stop("Specified Root Folder not found.")}
  check_dir <- paste0(root_folder, "/", scripts_folder)
  if (!dir.exists(check_dir)) { stop("Specified R-Scripts folder not found.")}
  check_dir <- paste0(root_folder, "/", project_folder)
  if (!dir.exists(check_dir)) { stop("Specified JCass Project folder not found.")}
  
  if (!file.exists(jcass_exe)) {stop("JCass Runner executable was not found")}
  if (!file.exists(customiser_dll)) {stop("Custom DLL file was not found")}
  
  #-------------  Set Working Folder and Load Utility Functions       ------------
  #
  setwd(root_folder)  # Now set the working folder
  utils_file <- paste0(scripts_folder,"/utils/jcass_utilities.R") 
  source(utils_file) # load utilities
  
  #-------  Set up path to JCass meta setup file for current project   -----------
  #
  meta_file <- paste0(project_folder, "/jcass_meta.xlsx")
  check_file <- paste0(root_folder, "/", meta_file)
  if (!file.exists(check_file)) {stop("JCass Meta setup file was not found")}
  
  #--------  Update the location of the custom .DLL in the meta setup file -------
  #          This is needed because on a shared drive the location differs 
  #          depending on the user
  jc_update_setup_parameter(meta_file, "meta", "customiser_dll", customiser_dll)
  
  if (!endsWith(root_folder, '/')) {
    root_folder <- paste0(root_folder, "/")
  }
  
  if (!endsWith(scripts_folder, '/')) {
    scripts_folder <- paste0(scripts_folder, "/")
  }
  
  if (!endsWith(project_folder, '/')) {
    project_folder <- paste0(project_folder, "/")
  }
  
  out_folder <- paste0(project_folder, "outputs/")
  debug_folder <- paste0(project_folder, "debug/")
  preproc_folder <- paste0(project_folder, "preproc/")
  calib_folder <- paste0(project_folder, "calibration/")
  
  jc_folders <- c(out_folder, debug_folder, preproc_folder, calib_folder)
  .check_folders(root_folder, jc_folders)
  
  result = list(root = root_folder, scripts = scripts_folder,
                project = project_folder, executable = jcass_exe,
                customiser = customiser_dll, meta_file = meta_file,
                out_dir = out_folder, debug_dir = debug_folder, 
                preproc_dir = preproc_folder, calib_dir = calib_folder)
  return(result)
  
}


#-----------------------     Read Path Information     -------------------------
#
# read path information from path setup file specified at the top

.check_folders <- function(root_folder, jc_folders) {
  for (sub_foldr in jc_folders) {
    check_dir <- paste0(root_folder, "/", sub_foldr)  
    if (!dir.exists(check_dir)) { 
      print(paste0("Creating sub-folder '", sub_foldr, "' in project folder"))
      dir.create(file.path(root_folder, sub_foldr), showWarnings = FALSE)
    }
  }
}