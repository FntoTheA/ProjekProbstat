# =====================================
# PROJECT AKHIR PROBSTAT
# Analisis Data Pembangunan Wilayah
# =====================================

# Load library
library(ggplot2)
library(dplyr)
library(corrplot)

# ==========================
# 1. MEMBACA DATASET
# ==========================
data <- read.csv("Dataset/pembangunan_wilayah_missing_outlier.csv")

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
  select(where(is.numeric))

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

# Histogram IPM
ggplot(data_clean, aes(ipm)) +
  geom_histogram(bins = 20) +
  ggtitle("Distribusi IPM")

# Boxplot Kemiskinan
ggplot(data_clean, aes(y = kemiskinan)) +
  geom_boxplot() +
  ggtitle("Boxplot Tingkat Kemiskinan")

# Scatter Plot IPM vs Kemiskinan
ggplot(data_clean,
       aes(x = ipm, y = kemiskinan)) +
  geom_point(alpha = 0.5) +
  ggtitle("Hubungan IPM dan Kemiskinan")

# Bar Chart Provinsi
ggplot(data_clean,
       aes(x = provinsi)) +
  geom_bar() +
  theme(axis.text.x =
          element_text(angle = 45, hjust = 1)) +
  ggtitle("Jumlah Data per Provinsi")

# Histogram Pengangguran
ggplot(data_clean,
       aes(pengangguran)) +
  geom_histogram(bins = 20) +
  ggtitle("Distribusi Pengangguran")

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