library(dplyr)
library(magrittr)


db_dir <- "C:\\Users\\Emanuele\\Documents\\Trieste\\ProgettiRicerca\\2022_CACHE\\DATABASE_ZINC_MCULE"

joined_dir <- paste0(db_dir, "\\JOINED_BY_INCHIKEY")

joined_files <- list.files(path = joined_dir)

for (joined_idx in 1:length(joined_files)) {
#for (joined_idx in 1:10) {
  cat("WORK ON IDX", joined_idx, "\n")
  joined_file <- joined_files[joined_idx]
  joined_tmpdata <- read.delim2(file = paste0(joined_dir, "\\", joined_file), 
                           header = TRUE, sep = ";")
  if (joined_idx == 1) {
    joined_data <- joined_tmpdata
  } else {
    joined_data <- rbind(joined_data, joined_tmpdata)
  }
  
}

full_joned_file <- "Full_joined_zinc_mcule.csv"
write.csv2(x = joined_data, 
           file = paste0(joined_dir, "\\", full_joned_file),
           quote = FALSE,
           row.names = FALSE)


print(colnames(joined_data))

zinc_data_mcule <- joined_data %>% 
  dplyr::filter(!is.na(mcule_id))

print(dim(joined_data))
print(dim(zinc_data_mcule))


# 3 datasets: 
# C. Molecole in comune tra ZINC e MCULE (primo dataset da lavorare) => 
#  qualunque scelta verrà fatta da CACHE i calcoli andranno bene.
# E. Molecole selettive ZINC/ENAMINE
# M. Molecole selettive MCULE


# C. Estrazione dal dataframe (dopo join) delle molecole in comune
dataset_C_file <- "Dataset_C.csv"
write.csv2(x = zinc_data_mcule, 
           file = paste0(joined_dir, "\\", dataset_C_file),
           quote = FALSE,
           row.names = FALSE)

zinc_data_selective <- joined_data %>% 
  dplyr::filter(is.na(mcule_id))


# E. Estrazione dal dataframe (dopo join) delle molecole selettive ZINC/ENAMINE
dataset_E_file <- "Dataset_E.csv"
write.csv2(x = zinc_data_selective, 
           file = paste0(joined_dir, "\\", dataset_E_file),
           quote = FALSE,
           row.names = FALSE)

# M. Lavoro su MCULE per ottenere dei file .smi 
# da processare, che non abbiano molecole in comune con ZINC

db_dir <- "C:\\Users\\Emanuele\\Documents\\Trieste\\ProgettiRicerca\\2022_CACHE\\DATABASE_ZINC_MCULE"
mcule_dir <- paste0(db_dir, "\\MCULE_ORIGDATA")

# Read MCULE data
mcule_files <- list.files(path = mcule_dir)

for (mcule_idx in 1:5) {
  mcule_file <- mcule_files[mcule_idx]
  mcule_tmpdata <- read.delim2(file = paste0(mcule_dir, "\\", mcule_file), 
                               header = TRUE, sep = "\t")
  if (mcule_idx == 1) {
    mcule_smi_data <- mcule_tmpdata
  } else {
    mcule_smi_data <- rbind(mcule_smi_data, mcule_tmpdata)
  }
}

mcule_data_selective <- mcule_smi_data %>% 
  dplyr::anti_join(joined_data, by="mcule_id")

dataset_M_file <- "Dataset_M.csv"
write.csv2(x = mcule_data_selective, 
           file = paste0(joined_dir, "\\", dataset_M_file),
           quote = FALSE,
           row.names = FALSE)

