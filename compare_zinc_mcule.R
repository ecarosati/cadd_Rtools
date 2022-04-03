library(dplyr)
library(magrittr)

# Questa script legge tutti i file .txt scaricati da ZINC/Enamine (13M di molecole)
# e tutti i dati scaricati per MCUL (5M di molecole) per i quali era stata 
# ottenuta la inchikey usando rdkit
# Fa il join dei due dataset (left join su ZINC), elaborando un file TXT per volta.
# Alla file si avranno tanti file TXT con una colonna aggiuntiva che sarà il codice
# della stessa nolecola disponibile nel dataset MCULE


db_dir <- "C:\\Users\\Emanuele\\Documents\\Trieste\\ProgettiRicerca\\2022_CACHE\\DATABASE_ZINC_MCULE"
mcule_dir <- paste0(db_dir, "\\MCULE_RDKIT_INCHIKEY")

# Read MCULE data
mcule_files <- list.files(path = mcule_dir)

for (mcule_idx in 1:5) {
  mcule_file <- mcule_files[mcule_idx]
  mcule_tmpdata <- read.delim2(file = paste0(mcule_dir, "\\", mcule_file), 
                            header = FALSE, sep = ",")
  if (mcule_idx == 1) {
    mcule_data <- mcule_tmpdata
  } else {
    mcule_data <- rbind(mcule_data, mcule_tmpdata)
  }
}
colnames(mcule_data) <- c("mcule_id", "inchikey")


# Read ZINC data (single file)
zinc_dir <- paste0(db_dir, "\\ZINC_ORIGDATA")

zinc_files <- list.files(path = zinc_dir)

# Facciamo blocchi da 100 files
for (zinc_idx in 1:length(zinc_files)) {
  zinc_file <- zinc_files[zinc_idx]
  zinc_data <- read.delim2(file = paste0(zinc_dir, "\\", zinc_file), 
                           header = TRUE, sep = "\t")
  
  joned_file <- gsub(pattern=".txt", replacement="_joined.txt", x=zinc_file)
  
  # Join ZINC data with MCULE data
  joined_data <- zinc_data %>% 
    dplyr::left_join(mcule_data, by="inchikey")
  joined_dir <- paste0(db_dir, "\\JOINED_BY_INCHIKEY")
  
  write.csv2(x = joined_data, 
             file = paste0(joined_dir, "\\", joned_file),
             quote = FALSE,
             row.names = FALSE)

}



