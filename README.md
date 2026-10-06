# ⚡ EnergiLasso — Seleksi Variabel Desain Bangunan dengan Regresi Lasso

Proyek mata kuliah **Analisis Regresi (ANREG) S2**: menerapkan **Regresi Lasso** untuk memprediksi kebutuhan energi bangunan, yaitu **Heating Load (Y1)** dan **Cooling Load (Y2)**, dengan **Random Forest** sebagai pembanding akurasi. Hasil analisis juga disajikan dalam **dashboard R Shiny** yang interaktif.

**Penulis:** Ricardo Filemon Renaldy Saragih

---

## ❓ Pertanyaan Penelitian

> Variabel desain bangunan mana (X1–X8) yang tetap **dipilih oleh Lasso** (koefisien ≠ 0) saat memprediksi Heating Load (Y1) dan Cooling Load (Y2) secara terpisah?

---

## 📊 Data

**Energy Efficiency Dataset (ENB2012)** dari UCI Machine Learning Repository: 768 konfigurasi bangunan hasil simulasi Ecotect.

| Variabel | Keterangan | Jenis |
|---|---|---|
| X1 | Relative Compactness | Numerik |
| X2 | Surface Area | Numerik |
| X3 | Wall Area | Numerik |
| X4 | Roof Area | Numerik |
| X5 | Overall Height | Numerik |
| X6 | Orientation | Kategorik |
| X7 | Glazing Area | Numerik |
| X8 | Glazing Area Distribution | Kategorik |
| **Y1** | **Heating Load** | Respons |
| **Y2** | **Cooling Load** | Respons |

Sumber: Tsanas, A. & Xifara, A. (2012). *Accurate quantitative estimation of energy performance of residential buildings using statistical machine learning tools.* Energy and Buildings, 49, 560–567.

---

## ⚙️ Alur Analisis

| Langkah | Isi |
|---|---|
| 2 | Rumusan pertanyaan |
| 3 | EDA: kualitas data, distribusi Y, korelasi Spearman, LOWESS, cek hubungan X2 = X3 + 2·X4 |
| 4 | Split train/test 80:20, fit OLS (baseline) dan Lasso (λ dipilih dengan 5-fold CV) |
| 5 | Diagnostik OLS: rank matriks, VIF, Breusch-Pagan, Jarque-Bera, Cook's Distance |
| 6 | Evaluasi test set + repeated CV (10-fold × 5 ulangan) + stabilitas seleksi Lasso |
| 7 | Benchmark Random Forest (tuning `mtry` & `min.node.size` dengan OOB), perbandingan per fold, permutation importance |

**Catatan penting:**
- Y1 dan Y2 dimodelkan **terpisah**.
- X6 dan X8 diperlakukan sebagai **kategorik** (one-hot encoding).
- Koefisien Lasso ≠ 0 berarti **"dipilih model"**, bukan "signifikan secara p-value".

---

## 📁 Isi Repositori

```
├── README.md
├── .gitignore
├── analisis_anreg_s2.R   # Script analisis lengkap (Langkah 2–7)
└── app.R                 # Dashboard R Shiny (3 tab)
```

---

## 🚀 Cara Menjalankan

### 1. Install package (sekali saja)

```r
install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "readxl",
                   "glmnet", "lmtest", "ranger", "plotly", "patchwork"))
```

### 2. Siapkan data

Unduh `ENB2012_data.xlsx` dari [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/242/energy+efficiency), lalu simpan di folder yang sama dengan file `.R`.

> Dashboard bisa mengunduh data secara otomatis jika file tidak ditemukan (butuh internet).

### 3a. Menjalankan script analisis

Buka `analisis_anreg_s2.R` di RStudio, lalu klik **Source**.

### 3b. Menjalankan dashboard

Buka `app.R` di RStudio, lalu klik **Run App**.

> Pembukaan pertama ± 5 menit (repeated CV Lasso & Random Forest). Hasil disimpan di folder `cache_hasil/`, sehingga pembukaan berikutnya langsung tampil.

---

## 🖥️ Isi Dashboard

| Tab | Isi |
|---|---|
| **1. Data & EDA** | Filter bangunan, distribusi target, scatter + LOWESS, heatmap korelasi |
| **2. Model + Diagnostik** | Slider λ untuk melihat koefisien menyusut, persamaan Lasso, stabilitas seleksi, VIF, residual, Q-Q, Cook's Distance |
| **3. Prediksi + Benchmark** | Rancang bangunan dan lihat prediksi OLS / Lasso / RF, perbandingan RMSE per fold, importance RF vs seleksi Lasso |

---

## 📄 Lisensi

Dibuat untuk keperluan akademik. Dataset ENB2012 berlisensi CC BY 4.0 dari UCI Machine Learning Repository.
