library(readxl)
library(dplyr)
library(stringr)
library(writexl) # untuk menyimpan hasil

extract_value <- function(value) {
  value <- as.character(value)
  value <- str_trim(value)
  
  # Jika NA atau kosong
  if (is.na(value) || value == "") return(NA)
  
  # 1. x ± y (termasuk koma)
  if (str_detect(value, "^\\d{1,3}(,\\d{3})*(\\.\\d+)?\\s*±\\s*\\d+(\\.\\d+)?")) {
    num <- str_replace_all(str_extract(value, "^\\d{1,3}(,\\d{3})*(\\.\\d+)?"), ",", "")
    return(as.numeric(num))
  }
  
  # 2. x±y tanpa koma
  if (str_detect(value, "^\\d+(\\.\\d+)?\\s*±\\s*\\d+(\\.\\d+)?")) {
    return(as.numeric(str_extract(value, "^\\d+(\\.\\d+)?")))
  }
  
  # 3. x+y
  if (str_detect(value, "^\\d+(\\.\\d+)?\\+\\d+(\\.\\d+)?")) {
    return(as.numeric(str_extract(value, "^\\d+(\\.\\d+)?")))
  }
  
  # 4. x/y (z%) → ambil x
  if (str_detect(value, "^\\d+/\\d+\\s*\\(\\d+\\.?\\d*%\\)")) {
    return(as.numeric(str_extract(value, "^\\d+")))
  }
  
  # 5. z% (x/y) → ambil x
  if (str_detect(value, "^\\d+\\.?\\d*%\\s*\\(\\d+/\\d+\\)")) {
    return(as.numeric(str_match(value, "\\((\\d+)/")[,2]))
  }
  
  # 6. x (y%) → ambil x
  if (str_detect(value, "^\\d+(\\.\\d+)?\\s*\\(\\d+\\.?\\d*%\\)")) {
    return(as.numeric(str_extract(value, "^\\d+(\\.\\d+)?")))
  }
  
  # 7. x% (n: y) → ambil y
  if (str_detect(value, "^\\d+\\.?\\d*%\\s*\\(n:\\s*\\d+\\)")) {
    return(as.numeric(str_match(value, "n:\\s*(\\d+)")[,2]))
  }
  
  # 8. x (y.z) atau x (a–b) → ambil x
  if (str_detect(value, "^\\d+(\\.\\d+)?\\s*\\(.*\\)")) {
    return(as.numeric(str_extract(value, "^\\d+(\\.\\d+)?")))
  }
  
  # 9. Kalimat panjang dengan angka diawali "1. x/y ..." → ambil x
  if (str_detect(value, "^1\\.\\s*\\d+/\\d+")) {
    return(as.numeric(str_match(value, "^1\\.\\s*(\\d+)/")[,2]))
  }
  
  # 10. x (y%) = text → ambil x
  if (str_detect(value, "^\\d+\\s*\\(\\d+\\.?\\d*%\\)\\s*=\\s*.*")) {
    return(as.numeric(str_extract(value, "^\\d+")))
  }
  
  # 11. Angka dengan koma ribuan: 1,809.7 atau 3,396.8 ± ...
  if (str_detect(value, "^\\d{1,3}(,\\d{3})*(\\.\\d+)?")) {
    num <- str_extract(value, "^\\d{1,3}(,\\d{3})*(\\.\\d+)?")
    num_clean <- str_replace_all(num, ",", "")
    return(as.numeric(num_clean))
  }
  
  # 12. Angka desimal tunggal
  if (str_detect(value, "^\\d+(\\.\\d+)?$")) {
    return(as.numeric(value))
  }
  
  # Jika tidak cocok dengan aturan apa pun
  return(NA)
}


# Baca semua nama sheet
file_path <- "Dataset.xlsx"
sheet_names <- excel_sheets(file_path)

# List untuk simpan hasil bersih
cleaned_sheets <- list()

# Loop semua sheet
for (sheet in sheet_names) {
  df <- read_excel(file_path, sheet = sheet)
  colnames(df) <- c("STUDY ID","Treatment 1","Treatment 2",	"Treatment Sample Size 1",
                    "Treatment Sample Size 2","Treatment 1 Outcome (%)","Treatment 2 Outcome (%)",
                    "Dose Treatment 1",	"Dose Treatment 2","Follow up (Weeks)",	"Design")
  
  df <- df %>%
    mutate(
      raw1 = str_replace_all(`Treatment 1 Outcome (%)`, "[\r\n]", " "),
      raw1 = str_squish(raw1),
      raw2 = str_replace_all(`Treatment 2 Outcome (%)`, "[\r\n]", " "),
      raw2 = str_squish(raw2),
      sample1 = as.numeric(`Treatment Sample Size 1`),
      sample2 = as.numeric(`Treatment Sample Size 2`),
      treat1 = `Treatment 1`,
      treat2 = `Treatment 2`,
      event1 = round(case_when(
        str_detect(raw1, "^0\\.\\d+$") ~ as.numeric(raw1) * sample1,
        str_detect(raw1, "^\\d+\\.?\\d*%$") ~ as.numeric(str_remove(raw1, "%")) / 100 * sample1,
        TRUE ~ sapply(raw1, function(x) {
          val <- extract_value(x)
          if (is.numeric(val)) return(val) else return(NA)
        })
      ), 0),
      event2 = round(case_when(
        str_detect(raw2, "^0\\.\\d+$") ~ as.numeric(raw2) * sample2,
        str_detect(raw2, "^\\d+\\.?\\d*%$") ~ as.numeric(str_remove(raw2, "%")) / 100 * sample2,
        TRUE ~ sapply(raw2, function(x) {
          val <- extract_value(x)
          if (is.numeric(val)) return(val) else return(NA)
        })
      ), 0)
      )
 
  df <- df %>% distinct(treat1, treat2, .keep_all = TRUE)
  df <- df %>% filter(treat1 != treat2)
  
  # Hapus baris jika event > sample
  df <- df %>% filter(event1 <= sample1 & event2 <= sample2)
  
  if (nrow(df) >=3){
    cleaned_sheets[[sheet]] <- df 
  }
}

# Optional: Simpan ke file baru
write_xlsx(cleaned_sheets, path = "Cleaned_Dataset.xlsx")

library(meta)

# List untuk simpan hasil pairwise per sheet
pairwise_results <- list()

# Loop setiap sheet
for (sheet_name in names(cleaned_sheets)) {
  
  df <- cleaned_sheets[[sheet_name]]
  
  df <- data.frame(df)
  
  # Pilih hanya kolom yang diperlukan
  df <- df %>% select(
    STUDY.ID, sample1, sample2, treat1, treat2, event1, event2
  )
  
  # Hapus baris dengan NA
  df <- df %>% filter(
    !is.na(treat1) & !is.na(treat2) &
      !is.na(event1) & !is.na(event2) &
      !is.na(sample1) & !is.na(sample2) &
      !is.na(STUDY.ID)
  )
  
  # Jalankan pairwise (pastikan semua kolom ada dan jumlah baris konsisten)
  pw <- pairwise(
    list(df$treat1, df$treat2),
    list(df$event1, df$event2),
    list(df$sample1, df$sample2),
    studlab = df$STUDY.ID
  )
  
  # Hapus baris dengan NA
  pw <- pw %>% filter(
    !is.na(TE) & !is.na(seTE)
  )
  
  pairwise_results[[sheet_name]] <- pw
  
  # Tampilkan nama sheet yang berhasil diproses
  cat("Sheet berhasil diproses:", sheet_name, "\n")
}

# Optional: Simpan ke file baru
write_xlsx(pairwise_results, path = "Pairwise Dataset.xlsx")
