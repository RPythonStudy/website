my_read_excel<- function(directory, file_name, col_types = NULL) {
  
  # project_root <- rprojroot::find_root(rprojroot::is_rstudio_project)를 통해 미리 정의되어 있어야 함.
  file_path <- file.path(project_root, directory, file_name)
  data <- data.table::as.data.table(readxl::read_xlsx(file_path, col_types = col_types))
  
  return(data)
}