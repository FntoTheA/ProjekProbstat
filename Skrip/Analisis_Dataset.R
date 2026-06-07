# =====================================
# PROJECT AKHIR PROBSTAT
# Analisis Data Pembangunan Wilayah
# =====================================

# Load library
library(ggplot2)
library(dplyr)
library(corrplot)

# ==========================
# 1. MEMBACA DATASET test
# ==========================
data <- read.csv("Dataset/pembangunan_wilayah_missing_outlier.csv")
data$tahun_factor <- as.factor(data$tahun)
# Struktur data
str(data)

# Jumlah observasi dan variabel
dim(data)

# Ringkasan data
summary(data)

head(data)

# ==========================
# 2. STATISTIKA DESKRIPTIF
# ==========================

numeric_data <- data %>%
  select(where(is.numeric), -tahun)

deskriptif <- data.frame(
  Mean = sapply(numeric_data, mean, na.rm = TRUE),
  Median = sapply(numeric_data, median, na.rm = TRUE),
  SD = sapply(numeric_data, sd, na.rm = TRUE),
  Variance = sapply(numeric_data, var, na.rm = TRUE),
  Minimum = sapply(numeric_data, min, na.rm = TRUE),
  Maximum = sapply(numeric_data, max, na.rm = TRUE)
)

print(deskriptif)

# Kuartil
sapply(numeric_data, quantile, na.rm = TRUE)

# ==========================
# 3. ANALISIS MISSING VALUE
# ==========================

missing_count <- colSums(is.na(data))
print(missing_count)

# Median Imputation
data_clean <- data

for(col in names(data_clean)){
  if(is.numeric(data_clean[[col]])){
    data_clean[[col]][is.na(data_clean[[col]])] <-
      median(data_clean[[col]], na.rm = TRUE)
  }
}

# Cek ulang
colSums(is.na(data_clean))

# ==========================
# 4. ANALISIS OUTLIER (IQR)
# ==========================

detect_outlier <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  sum(x < lower | x > upper, na.rm = TRUE)
}

outlier_count <- sapply(data_clean[,sapply(data_clean,is.numeric)],
                        detect_outlier)

print(outlier_count)

# ==========================
# 5. VISUALISASI DATA
# ==========================
folder_gambar <- "Gambar"
if (!dir.exists(folder_gambar)) {
  dir.create(folder_gambar)
  cat("\n[SISTEM] Folder '", folder_gambar, "' berhasil dibuat.\n", sep="")
}

kontinu_cols <- c("pdrb_perkapita", "kemiskinan", "pengangguran", "ipm", 
                  "harapan_hidup", "rata_lama_sekolah", "akses_internet", 
                  "jalan_baik", "air_bersih")

# --- 1. Histogram IPM ---
ggplot(data_clean, aes(x = ipm)) +
  geom_histogram(bins = 20, fill = "#3498db", color = "white", alpha = 0.8) +
  labs(title = "Kurva Distribusi Empiris IPM Wilayah", x = "Skor IPM", y = "Frekuensi") +
  theme_minimal()
ggsave(filename = paste0(folder_gambar, "/1_distribusi_ipm.png"), width = 7, height = 4.5, dpi = 300)

# --- 2. Boxplot Kemiskinan ---
ggplot(data_clean, aes(y = kemiskinan)) +
  geom_boxplot(fill = "#e74c3c", color = "#c0392b", alpha = 0.7) +
  labs(title = "Boxplot Evaluasi Outlier Tingkat Kemiskinan", y = "Persentase (%)") +
  theme_minimal()
ggsave(filename = paste0(folder_gambar, "/2_boxplot_kemiskinan.png"), width = 5, height = 4.5, dpi = 300)

# --- 3. Scatter Plot Hubungan Kausalitas ---
ggplot(data_clean, aes(x = ipm, y = kemiskinan)) +
  geom_point(alpha = 0.4, color = "#2c3e50") +
  geom_smooth(method = "lm", color = "#27ae60", se = TRUE, linetype = "dashed") +
  labs(title = "Scatter Plot Hubungan Linear: IPM vs Kemiskinan", x = "IPM", y = "Kemiskinan (%)") +
  theme_minimal()
ggsave(filename = paste0(folder_gambar, "/3_scatter_ipm_kemiskinan.png"), width = 7, height = 4.5, dpi = 300)

# --- 4. Bar Chart Frekuensi Provinsi ---
ggplot(data_clean, aes(x = provinsi)) +
  geom_bar(fill = "#27ae60", alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Jumlah Observasi Sampel per Provinsi", x = "Provinsi", y = "Jumlah Baris")
ggsave(filename = paste0(folder_gambar, "/4_bar_provinsi.png"), width = 7.5, height = 5, dpi = 300)

# --- 5. Histogram Pengangguran ---
ggplot(data_clean, aes(x = pengangguran)) +
  geom_histogram(bins = 20, fill = "#f39c12", color = "white", alpha = 0.8) +
  labs(title = "Kurva Distribusi Tingkat Pengangguran", x = "Persentase (%)", y = "Frekuensi") +
  theme_minimal()
ggsave(filename = paste0(folder_gambar, "/5_distribusi_pengangguran.png"), width = 7, height = 4.5, dpi = 300)

# --- 6. Heatmap Korelasi Kontinu (Menggunakan Base R Plot Device) ---
png(filename = paste0(folder_gambar, "/6_heatmap_korelasi.png"), width = 800, height = 750, res = 120)
matriks_korelasi <- cor(data_clean[kontinu_cols])
corrplot(matriks_korelasi, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)
dev.off()

# ==========================
# 6. DISTRIBUSI & PROBABILITAS
# ==========================

# Uji normalitas
shapiro.test(sample(data_clean$ipm, 500))

# Probabilitas kemiskinan > rata-rata
mean_kemiskinan <- mean(data_clean$kemiskinan)

prob_kemiskinan <- mean(
  data_clean$kemiskinan > mean_kemiskinan
)

prob_kemiskinan

# Probabilitas IPM > 75
prob_ipm <- mean(data_clean$ipm > 75)
prob_ipm

# Probabilitas pengangguran > 10%
prob_pengangguran <- mean(
  data_clean$pengangguran > 10
)

prob_pengangguran

# ==========================
# 7. ANALISIS KORELASI
# ==========================

corr_matrix <- cor(
  data_clean[,sapply(data_clean,is.numeric)]
)

print(corr_matrix)

corrplot(corr_matrix,
         method = "color",
         tl.cex = 0.8)

# Korelasi IPM dan Kemiskinan
cor.test(data_clean$ipm,
         data_clean$kemiskinan)

# Korelasi IPM dan Internet
cor.test(data_clean$ipm,
         data_clean$akses_internet)

# Korelasi PDRB dan Kemiskinan
cor.test(data_clean$pdrb_perkapita,
         data_clean$kemiskinan)

# ==========================
# SELESAI
# ==========================