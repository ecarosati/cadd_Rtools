# QUESTA SCRIPT LEGGE I FILE CONTENUTI NELLA DIRECTORY IR E CREA UN FILE .RData
# CHE POI VERRA' LETTO DALLA APP

library(OpenSpecy)
library(glue)
library(magrittr)
library(dplyr)

code <- "IR"

setwd("C:\\Users\\Emanuele\\Documents\\GithubRepositories\\cadd_Rtools\\app_IR")

dir_spectra <- file.path(getwd(), "spectra")
output_file <- file.path(dir_spectra, "dataIR.R")

# nist <- FALSE
# dir_spectra <- file.path(dir_spectra, "LAB")
# print(dir_spectra)

read_spectra_data <- function(mydir, nist) {
  all_files <- list.files(mydir)

  f_counter <- 0
  print(all_files)
  
  for (f_file in all_files) {
    f_name <- stringr::str_replace(string=f_file, pattern=".dx|.jdx", replacement = "")
    cat("\n---------------\n")
    print(f_name)
    
    f_data <- stringr::str_split(string=f_name, pattern="_", simplify = F)[[1]] 
    
    # print(f_data)
    nr_data_fields <- length(f_data)
    # print(nr_data_fields)
    
    
    if (f_data[1] == code) {
      f_code <- code
      cat("CODE", f_code, "\n")
      
      if (nist) {
        f_method <- f_data[nr_data_fields]
        f_mol <- f_data[nr_data_fields-1]
        f_date <- NA
        f_substance <- NA
        f_source <- "NIST"
        
      } else {
        f_method <- f_data[nr_data_fields]
        f_mol_tmp <- f_data[nr_data_fields-1]
        f_date <- f_data[nr_data_fields-2]
        f_source <- "LAB"
        
        if (stringr::str_detect(string = f_mol_tmp, pattern = "^BKG")) {
          f_mol <- stringr::str_remove(string = f_mol_tmp, pattern = "^BKG")
          f_substance <- "BKG"
        } else if (stringr::str_detect( string = f_mol_tmp, pattern = "^STD")) {
          f_mol <- stringr::str_remove(string = f_mol_tmp, pattern = "^STD")
          f_substance <- "STD"
        } else if (stringr::str_detect( string = f_mol_tmp, pattern = "^INC")) {
          f_mol <- stringr::str_remove(string = f_mol_tmp, pattern = "^INC")
          f_substance <- "INC"
        } else {
          cat("WRONG FORMAT FOR FILE", f_name, "\n")
        }
        
      }
      
      spectra_data <- OpenSpecy::read_jdx(file=file.path(mydir, f_file))
      
      if ('wavenumber' %in% colnames(spectra_data) &&
          'intensity' %in% colnames(spectra_data)) {
        
        spectra_data <- spectra_data #%>% 
          # dplyr::mutate(Tperc = 100 * intensity)
        
        
        # I dati dovrebbero essere OK e possiamo inserirli nel dataframe
        f_counter <- f_counter + 1
        if (f_counter == 1) {
          full_data <- spectra_data %>%
            dplyr::mutate(code=f_code) %>%
            dplyr::mutate(source=f_source) %>% 
            dplyr::mutate(date=f_date) %>%
            dplyr::mutate(mol=f_mol) %>%
            dplyr::mutate(substance=f_substance) %>%
            dplyr::mutate(method=f_method) %>% 
            dplyr::mutate(Tperc = case_when(substance == "BKG" ~ intensity, 
                                            substance != "BKG" ~ 100 * intensity,
                                            is.na(substance) ~ 100 * intensity))
          
        } else {
          single_data <- spectra_data %>%
            dplyr::mutate(code=f_code) %>%
            dplyr::mutate(source=f_source) %>% 
            dplyr::mutate(date=f_date) %>%
            dplyr::mutate(mol=f_mol) %>%
            dplyr::mutate(substance=f_substance) %>%
            dplyr::mutate(method=f_method) %>% 
            dplyr::mutate(Tperc = case_when(substance == "BKG" ~ intensity, 
                                            substance != "BKG" ~ 100 * intensity,
                                            is.na(substance) ~ 100 * intensity))
          
          # dplyr::mutate(pippo = if_else((!is.na(substance) && substance == "BKG"), intensity, Tperc))
          
          full_data <- rbind(full_data, single_data)

        }
      }

    } else {
      cat("WRONG CODE FOR FILE:", f_name, "\n")
    }
    
  }
  
  final_data <- full_data %>%
    dplyr::select(code, source, date, method, substance, mol, wavenumber, intensity, Tperc) %>%
    dplyr::mutate(molname = glue::glue("{mol} ({source}/{substance}/{method}/{date})"))

  return(final_data)
  
}


all_data_IR <- rbind(
  read_spectra_data(mydir= file.path(dir_spectra, "LAB"), nist=FALSE),
  read_spectra_data(mydir= file.path(dir_spectra, "NIST"), nist=TRUE)
)

saveRDS(object = all_data_IR, file = output_file)
