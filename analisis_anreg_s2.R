# =============================================================================
# Energy Efficiency - Lasso Regression untuk Heating Load (Y1) dan Cooling Load (Y2)
# Konversi dari notebook Python ke R (untuk RStudio)
# Cakupan: Langkah 2 sampai Langkah 6
#
# Prinsip analisis
# - Unit data: konfigurasi desain bangunan hasil simulasi Ecotect (768 observasi).
# - Y1 (Heating Load) dan Y2 (Cooling Load) dimodelkan SECARA TERPISAH.
# - Kandidat prediktor: X1 sampai X8. Y1 tidak dipakai memprediksi Y2 (dan sebaliknya).
# - X6 (orientation) dan X8 (glazing area distribution) diperlakukan sebagai kategorik.
# - Hubungan X2 = X3 + 2*X4 dipertahankan sebagai masalah multikolinearitas,
#   bukan alasan menghapus X2 sebelum Lasso.
# - Koefisien Lasso != 0 berarti "terpilih oleh model", bukan "signifikan (p-value)".
# =============================================================================


# Langkah 2 - Rumusan Pertanyaan ----------------------------------------------
# Variabel desain arsitektural manakah, di antara X1 sampai X8, yang tetap
# terpilih oleh Lasso (koefisien tidak sama dengan nol) saat memprediksi
# Heating Load (Y1) dan Cooling Load (Y2) secara terpisah?


# Persiapan Library dan Data --------------------------------------------------

# Jalankan SEKALI saja jika paket belum terpasang:
# install.packages(c("tidyverse", "readxl", "glmnet", "lmtest", "patchwork"))

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, ggplot2, purrr, tibble
  library(readxl)      # membaca file .xlsx
  library(glmnet)      # Lasso (pengganti LassoCV sklearn)
  library(lmtest)      # uji Breusch-Pagan
  library(patchwork)   # menggabungkan beberapa grafik ggplot
})

theme_set(theme_bw(base_size = 12))
options(width = 160, tibble.width = Inf, tibble.print_max = Inf)

# Gunakan garis miring "/" (atau "\\") pada path Windows di R
FILE_PATH <- "ENB2012_data.xlsx"   # simpan file data di folder yang sama dengan script ini
df <- read_excel(FILE_PATH, sheet = 1)

cat("Shape:", nrow(df), "x", ncol(df), "\n")
print(head(df))


# Definisi Variabel -----------------------------------------------------------

FEATURES             <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")
TARGETS              <- c("Y1", "Y2")
NUMERIC_FEATURES     <- c("X1", "X2", "X3", "X4", "X5", "X7")
CATEGORICAL_FEATURES <- c("X6", "X8")

variable_info <- tibble(
  Variable = c(FEATURES, TARGETS),
  Description = c(
    "Relative Compactness", "Surface Area", "Wall Area", "Roof Area",
    "Overall Height", "Orientation", "Glazing Area", "Glazing Area Distribution",
    "Heating Load", "Cooling Load"
  ),
  Role = c(rep("Predictor", 8), rep("Response", 2))
)
print(variable_info)


# Langkah 3 - EDA Terarah -----------------------------------------------------
# Fokus: kualitas data, distribusi Y1 dan Y2, hubungan prediktor-response,
# indikasi non-linearitas, dan multikolinearitas struktural.

## Kualitas data ----
vars <- c(FEATURES, TARGETS)

quality <- tibble(
  Variable = vars,
  dtype    = vapply(df[vars], function(x) class(x)[1], character(1), USE.NAMES = FALSE),
  missing  = vapply(df[vars], function(x) sum(is.na(x)), numeric(1), USE.NAMES = FALSE),
  n_unique = vapply(df[vars], function(x) n_distinct(x), numeric(1), USE.NAMES = FALSE)
)
print(quality)

cat("Duplicate rows:", sum(duplicated(df)), "\n")

# Ringkasan statistik (setara df.describe().T di pandas)
describe_tbl <- function(data) {
  q <- function(p) vapply(data, function(x) quantile(x, p, na.rm = TRUE, names = FALSE), numeric(1), USE.NAMES = FALSE)
  tibble(
    Variable = names(data),
    count    = vapply(data, function(x) sum(!is.na(x)), numeric(1), USE.NAMES = FALSE),
    mean     = vapply(data, mean, numeric(1), na.rm = TRUE, USE.NAMES = FALSE),
    std      = vapply(data, sd,   numeric(1), na.rm = TRUE, USE.NAMES = FALSE),
    min      = vapply(data, min,  numeric(1), na.rm = TRUE, USE.NAMES = FALSE),
    `25%`    = q(0.25),
    `50%`    = q(0.50),
    `75%`    = q(0.75),
    max      = vapply(data, max,  numeric(1), na.rm = TRUE, USE.NAMES = FALSE)
  )
}
print(describe_tbl(df[vars]))

## Hubungan struktural X2, X3, dan X4 ----
# X2 = X3 + 2*X4 -> multikolinearitas sempurna pada regresi linear tanpa penalti.
dependency <- df$X2 - (df$X3 + 2 * df$X4)
cat("Maximum absolute difference X2 - (X3 + 2*X4):", max(abs(dependency)), "\n")

## Distribusi Y1 dan Y2 ----
plot_dist <- function(target) {
  p1 <- ggplot(df, aes(x = .data[[target]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "steelblue", alpha = 0.6, color = "white") +
    geom_density(linewidth = 1) +
    labs(title = paste("Distribution of", target), x = target, y = "Density")
  
  p2 <- ggplot(df, aes(x = .data[[target]])) +
    geom_boxplot(fill = "steelblue", alpha = 0.6) +
    labs(title = paste("Boxplot of", target), x = target) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  p1 | p2
}
print(plot_dist("Y1") / plot_dist("Y2"))
print(describe_tbl(df[TARGETS]))

## Korelasi rank Spearman ----
# Korelasi kecil tidak berarti prediktor tidak penting jika hubungannya non-linear.
spearman_cols <- c("X1", "X2", "X3", "X4", "X5", "X7", "Y1", "Y2")
spearman_corr <- cor(df[spearman_cols], method = "spearman")

spearman_long <- as.data.frame(as.table(spearman_corr)) %>%
  setNames(c("Var1", "Var2", "rho"))

p_heat <- ggplot(spearman_long, aes(x = Var1, y = Var2, fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", rho)), size = 3.5) +
  scale_fill_gradient2(low = "#3B4CC0", mid = "white", high = "#B40426",
                       midpoint = 0, limits = c(-1, 1)) +
  scale_x_discrete(limits = spearman_cols) +
  scale_y_discrete(limits = rev(spearman_cols)) +
  coord_fixed() +
  labs(title = "Spearman Rank Correlation", x = NULL, y = NULL)
print(p_heat)

print(round(spearman_corr[c("X1", "X2", "X3", "X4", "X5", "X7"), c("Y1", "Y2")], 4))

## Fokus X7 (Glazing Area) dan potensi non-linearitas ----
# Kurva LOWESS (setara seaborn regplot lowess=True).
plot_lowess <- function(target) {
  lw <- lowess(df$X7, df[[target]], f = 2/3, iter = 3, delta = 0)
  ggplot(df, aes(x = X7, y = .data[[target]])) +
    geom_point(alpha = 0.35) +
    geom_line(data = tibble(X7 = lw$x, fit = lw$y), aes(x = X7, y = fit),
              inherit.aes = FALSE, color = "firebrick", linewidth = 1) +
    labs(title = paste("LOWESS: X7 vs", target))
}
print(plot_lowess("Y1") | plot_lowess("Y2"))

## Prediktor kategorik terhadap Y1 dan Y2 ----
box_cat <- function(x, y, title) {
  ggplot(df, aes(x = factor(.data[[x]]), y = .data[[y]])) +
    geom_boxplot(fill = "steelblue", alpha = 0.6) +
    labs(title = title, x = x, y = y)
}
print(
  (box_cat("X6", "Y1", "Y1 by Orientation (X6)") | box_cat("X8", "Y1", "Y1 by Glazing Distribution (X8)")) /
    (box_cat("X6", "Y2", "Y2 by Orientation (X6)") | box_cat("X8", "Y2", "Y2 by Glazing Distribution (X8)"))
)


# Langkah 4 - Fit Lasso dan Baseline ------------------------------------------
# Dua model per response:
#   1. Multiple Linear Regression (baseline)
#   2. Lasso, alpha (lambda di glmnet) dipilih dengan cross-validation
# Preprocessing:
#   - X1, X2, X3, X4, X5, X7 distandardisasi (mean 0, sd populasi 1)
#   - X6 dan X8 di-one-hot encode (kategori pertama sebagai referensi)
#
# Catatan: fungsi Lasso glmnet dengan standardize = FALSE memiliki fungsi objektif
# yang sama dengan sklearn Lasso, yaitu (1/(2n))*RSS + alpha*|beta|_1, sehingga
# "lambda" di glmnet setara dengan "alpha" di sklearn.

## Split train/test (sekali, agar Y1 dan Y2 memakai observasi yang sama) ----
set.seed(42)
n_obs    <- nrow(df)
n_test   <- ceiling(0.20 * n_obs)          # test_size = 0.20
test_idx <- sample(n_obs, n_test)

X_train <- df[-test_idx, FEATURES];  X_test <- df[test_idx, FEATURES]
Y_train <- df[-test_idx, TARGETS];   Y_test <- df[test_idx, TARGETS]

cat("X_train:", nrow(X_train), "x", ncol(X_train), "\n")
cat("X_test :", nrow(X_test),  "x", ncol(X_test),  "\n")

## Preprocessing (dipelajari HANYA dari data training) ----
fit_prep <- function(X) {
  sc <- vapply(X[NUMERIC_FEATURES], function(v) sqrt(mean((v - mean(v))^2)), numeric(1))
  sc[sc == 0] <- 1
  list(
    center = vapply(X[NUMERIC_FEATURES], mean, numeric(1)),
    scale  = sc,
    levels = lapply(X[CATEGORICAL_FEATURES], function(v) sort(unique(v)))
  )
}

apply_prep <- function(prep, X) {
  # numerik: standardisasi
  num <- do.call(cbind, lapply(NUMERIC_FEATURES, function(v) {
    (X[[v]] - prep$center[[v]]) / prep$scale[[v]]
  }))
  colnames(num) <- NUMERIC_FEATURES
  
  # kategorik: one-hot, drop = "first"; level tak dikenal -> semua dummy 0
  cat_list <- lapply(CATEGORICAL_FEATURES, function(v) {
    lv <- prep$levels[[v]][-1]
    m  <- matrix(vapply(lv, function(l) as.numeric(X[[v]] == l), numeric(nrow(X))),
                 nrow = nrow(X), ncol = length(lv))
    colnames(m) <- paste0(v, "_", lv)
    m
  })
  
  cbind(num, do.call(cbind, cat_list))
}

## Fungsi fit model (baseline OLS + LassoCV) untuk SATU response ----
LAMBDA_GRID <- 10^seq(2, -4, length.out = 200)   # setara np.logspace(-4, 2, 200), urut menurun
INNER_FOLDS <- 5

fit_target_models <- function(X_tr, y_tr, seed = 42) {
  prep <- fit_prep(X_tr)
  Z    <- apply_prep(prep, X_tr)
  
  # Baseline: regresi linear (pada rank-deficient, lm memberi NA pada koefisien yang teralias)
  baseline <- lm(y ~ ., data = data.frame(y = y_tr, Z, check.names = FALSE))
  
  # Lasso dengan inner 5-fold CV; alpha optimal = lambda dengan MSE CV minimum
  set.seed(seed)
  foldid <- sample(rep(seq_len(INNER_FOLDS), length.out = nrow(Z)))
  lasso <- cv.glmnet(
    x = Z, y = y_tr, alpha = 1, lambda = LAMBDA_GRID, foldid = foldid,
    standardize = FALSE, thresh = 1e-10, maxit = 1e6
  )
  
  list(prep = prep, baseline = baseline, lasso = lasso)
}

MODEL_NAMES <- c("Linear Regression", "Lasso")

predict_model <- function(fit, model_name, X_new) {
  Z <- apply_prep(fit$prep, X_new)
  if (model_name == "Linear Regression") {
    # peringatan "rank-deficient fit" diharapkan (X2 = X3 + 2*X4); prediksi tetap valid
    suppressWarnings(as.numeric(predict(fit$baseline, newdata = data.frame(Z, check.names = FALSE))))
  } else {
    as.numeric(predict(fit$lasso, newx = Z, s = "lambda.min"))
  }
}

## Fit model untuk Y1 dan Y2 secara terpisah ----
fitted_models <- setNames(
  lapply(TARGETS, function(t) fit_target_models(X_train, Y_train[[t]])),
  TARGETS
)

for (t in TARGETS) {
  cat(t, "| Optimal alpha:", fitted_models[[t]]$lasso$lambda.min, "\n")
}

## Koefisien Lasso pada level fitur hasil encoding ----
lasso_coef_table <- function(fit) {
  b <- as.matrix(coef(fit$lasso, s = "lambda.min"))
  b <- b[rownames(b) != "(Intercept)", 1]
  tibble(
    Feature     = names(b),
    Coefficient = unname(b),
    Selected    = abs(unname(b)) > 1e-10
  )
}

lasso_feature_tables <- list()
for (t in TARGETS) {
  lasso_feature_tables[[t]] <- lasso_coef_table(fitted_models[[t]]) %>%
    arrange(desc(abs(Coefficient)))
  cat("\n", t, "\n", sep = "")
  print(lasso_feature_tables[[t]])
}

## Seleksi dikembalikan ke level variabel asli X1-X8 ----
# Variabel kategorik dianggap terpilih jika minimal satu dummy-nya bukan nol.
original_variable <- function(feature_name) sub("_.*$", "", feature_name)   # "X6_3" -> "X6"

summarise_selection <- function(coef_table) {
  coef_table %>%
    mutate(Variable = original_variable(Feature)) %>%
    group_by(Variable) %>%
    summarise(
      Nonzero_Encoded_Terms = sum(Selected),
      Max_Abs_Coefficient   = max(abs(Coefficient)),
      .groups = "drop"
    ) %>%
    mutate(Selected = Nonzero_Encoded_Terms > 0) %>%
    arrange(match(Variable, FEATURES)) %>%
    select(Variable, Selected, Max_Abs_Coefficient, Nonzero_Encoded_Terms)
}

selection_tables <- lapply(lasso_feature_tables, summarise_selection)

selection_summary <- inner_join(
  selection_tables[["Y1"]], selection_tables[["Y2"]],
  by = "Variable", suffix = c("_Y1", "_Y2")
)
print(selection_summary)


# Langkah 5 - Diagnostic Model ------------------------------------------------
# Diagnostic dilakukan pada baseline OLS karena residual dan Cook's Distance
# memiliki interpretasi klasik pada OLS. Inferensi koefisien OLS harus dibaca
# hati-hati karena ada multikolinearitas sempurna di antara X2, X3, dan X4.

## Design matrix OLS + cek rank ----
prep_for_ols   <- fit_prep(X_train)
X_train_design <- apply_prep(prep_for_ols, X_train)
X_train_const  <- cbind(const = 1, X_train_design)

sv               <- svd(X_train_const)$d
matrix_rank      <- sum(sv > max(dim(X_train_const)) * max(sv) * .Machine$double.eps)
n_columns        <- ncol(X_train_const)
condition_number <- max(sv) / min(sv)

cat("Jumlah kolom design matrix :", n_columns, "\n")
cat("Rank design matrix         :", matrix_rank, "\n")
cat("Rank deficient?            :", matrix_rank < n_columns, "\n")
cat("Condition number           :", condition_number, "\n")

## VIF ----
# VIF sangat besar / Inf adalah konsekuensi dependensi linear sempurna.
# Dihitung manual: VIF_j = 1 / (1 - R2_j), R2_j dari regresi kolom j pada kolom lain.
vif_one <- function(j, M) {
  y   <- M[, j]
  fit <- lm(y ~ M[, -j, drop = FALSE])
  r2  <- 1 - sum(residuals(fit)^2) / sum((y - mean(y))^2)
  1 / (1 - r2)
}

vif_table <- tibble(
  Variable = colnames(X_train_design),
  VIF      = vapply(seq_len(ncol(X_train_design)), vif_one, numeric(1), M = X_train_design)
) %>%
  arrange(desc(VIF))
print(vif_table)

## Residual dan influential observations untuk Y1 dan Y2 ----
jarque_bera_p <- function(x) {
  n  <- length(x)
  m  <- x - mean(x)
  m2 <- mean(m^2); m3 <- mean(m^3); m4 <- mean(m^4)
  S  <- m3 / m2^1.5
  K  <- m4 / m2^2
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
  pchisq(JB, df = 2, lower.tail = FALSE)
}

ols_models      <- list()
diagnostic_rows <- list()
cook_threshold  <- 4 / nrow(X_train)

for (t in TARGETS) {
  m <- lm(y ~ ., data = data.frame(y = Y_train[[t]], X_train_design, check.names = FALSE))
  ols_models[[t]] <- m
  
  res     <- residuals(m)
  bp      <- bptest(m, studentize = TRUE)      # Breusch-Pagan (Koenker)
  cooks_d <- cooks.distance(m)
  
  diagnostic_rows[[t]] <- tibble(
    Target          = t,
    R2_OLS          = summary(m)$r.squared,
    Breusch_Pagan_p = unname(bp$p.value),
    Jarque_Bera_p   = jarque_bera_p(res),
    Cook_Threshold  = cook_threshold,
    N_Influential   = sum(cooks_d > cook_threshold)
  )
}
diagnostic_summary <- bind_rows(diagnostic_rows)
print(diagnostic_summary)

# Residual vs Fitted dan Q-Q plot
plot_resid <- function(m, t) {
  ggplot(tibble(fitted = fitted(m), resid = residuals(m)), aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residual vs Fitted -", t), x = "Fitted values", y = "Residuals")
}

plot_qq <- function(m, t) {
  r <- residuals(m)
  ggplot(tibble(z = (r - mean(r)) / sd(r)), aes(sample = z)) +
    stat_qq() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = paste("Q-Q Plot -", t), x = "Theoretical Quantiles", y = "Sample Quantiles")
}

print(
  (plot_resid(ols_models$Y1, "Y1") | plot_qq(ols_models$Y1, "Y1")) /
    (plot_resid(ols_models$Y2, "Y2") | plot_qq(ols_models$Y2, "Y2"))
)

# Cook's Distance
plot_cook <- function(m, t) {
  cd <- as.numeric(cooks.distance(m))
  ggplot(tibble(obs = seq_along(cd) - 1, cooks = cd), aes(x = obs, y = cooks)) +
    geom_segment(aes(xend = obs, yend = 0)) +
    geom_point(size = 0.6) +
    geom_hline(yintercept = cook_threshold, linetype = "dashed", color = "red") +
    annotate("text", x = Inf, y = cook_threshold, label = "4/n",
             hjust = 1.1, vjust = -0.6, color = "red") +
    labs(title = paste("Cook's Distance -", t),
         x = "Training Observation", y = "Cook's Distance")
}
print(plot_cook(ols_models$Y1, "Y1") / plot_cook(ols_models$Y2, "Y2"))


# Langkah 6 - Train/Test, Cross-Validation, Inferensi vs Prediksi -------------
# Prediksi: RMSE, MAE, R2 pada test set dan repeated cross-validation.
# Inferensi: p-value OLS TIDAK dipakai untuk menyebut variabel "benar-benar penting"
# karena rank deficiency. Untuk Lasso: koefisien nonzero, konsistensi seleksi pada
# repeated CV, dan performa out-of-sample.

metric_rmse <- function(y, p) sqrt(mean((y - p)^2))
metric_mae  <- function(y, p) mean(abs(y - p))
metric_r2   <- function(y, p) 1 - sum((y - p)^2) / sum((y - mean(y))^2)

## Evaluasi test set ----
test_results <- map_dfr(TARGETS, function(t) {
  map_dfr(MODEL_NAMES, function(mn) {
    pred <- predict_model(fitted_models[[t]], mn, X_test)
    tibble(
      Target = t, Model = mn,
      RMSE = metric_rmse(Y_test[[t]], pred),
      MAE  = metric_mae(Y_test[[t]], pred),
      R2   = metric_r2(Y_test[[t]], pred)
    )
  })
})
print(test_results)

## Actual vs Predicted ----
plot_avp <- function(t, mn) {
  d <- tibble(Actual = Y_test[[t]], Predicted = predict_model(fitted_models[[t]], mn, X_test))
  ggplot(d, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.65) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = paste(t, "-", mn))
}
print(
  (plot_avp("Y1", "Linear Regression") | plot_avp("Y1", "Lasso")) /
    (plot_avp("Y2", "Linear Regression") | plot_avp("Y2", "Lasso"))
)

## Repeated cross-validation ----
# 10-fold CV x 5 repeats (outer). Di setiap outer training fold, lambda Lasso
# kembali dipilih dengan inner 5-fold CV, sehingga validation fold tidak dipakai
# untuk memilih alpha. Sekaligus mencatat variabel yang terpilih di tiap fold.
N_SPLITS  <- 10
N_REPEATS <- 5

cv_rows           <- list()
selection_records <- list()

for (rep_id in seq_len(N_REPEATS)) {
  set.seed(42 + rep_id)
  fold_assign <- sample(rep(seq_len(N_SPLITS), length.out = n_obs))
  
  for (k in seq_len(N_SPLITS)) {
    tr      <- which(fold_assign != k)
    te      <- which(fold_assign == k)
    X_tr    <- df[tr, FEATURES]
    X_te    <- df[te, FEATURES]
    fold_no <- (rep_id - 1) * N_SPLITS + k
    
    for (t in TARGETS) {
      y_tr <- df[[t]][tr]
      y_te <- df[[t]][te]
      fit  <- fit_target_models(X_tr, y_tr, seed = 1000 * rep_id + k)
      
      for (mn in MODEL_NAMES) {
        pred <- predict_model(fit, mn, X_te)
        cv_rows[[length(cv_rows) + 1]] <- tibble(
          Target = t, Model = mn, Repeat = rep_id, Fold = fold_no,
          RMSE = metric_rmse(y_te, pred),
          MAE  = metric_mae(y_te, pred),
          R2   = metric_r2(y_te, pred)
        )
      }
      
      selection_records[[length(selection_records) + 1]] <-
        lasso_coef_table(fit) %>%
        summarise_selection() %>%
        mutate(Target = t, Fold = fold_no)
    }
  }
  cat("Repeated CV: repeat", rep_id, "dari", N_REPEATS, "selesai\n")
}

cv_results <- bind_rows(cv_rows)
print(head(cv_results))

## Ringkasan ketidakpastian performa ----
# Q025-Q975 = 95% empirical interval dari hasil resampling (bukan CI inferensial klasik).
cv_summary <- cv_results %>%
  group_by(Target, Model) %>%
  summarise(
    RMSE_Mean = mean(RMSE),
    RMSE_SD   = sd(RMSE),
    RMSE_Q025 = quantile(RMSE, 0.025, names = FALSE),
    RMSE_Q975 = quantile(RMSE, 0.975, names = FALSE),
    MAE_Mean  = mean(MAE),
    MAE_SD    = sd(MAE),
    R2_Mean   = mean(R2),
    R2_SD     = sd(R2),
    R2_Q025   = quantile(R2, 0.025, names = FALSE),
    R2_Q975   = quantile(R2, 0.975, names = FALSE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(cv_summary)

p_cv <- cv_results %>%
  pivot_longer(c(RMSE, MAE, R2), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("RMSE", "MAE", "R2"))) %>%
  ggplot(aes(x = Target, y = Value, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Repeated CV (10-fold x 5 repeats)", y = NULL)
print(p_cv)

## Stabilitas seleksi Lasso pada 50 outer folds ----
# Selection frequency: 100% = selalu terpilih di seluruh outer folds;
# nilai lebih rendah = seleksi lebih sensitif terhadap sampel training.
selection_cv <- bind_rows(selection_records)

selection_stability <- selection_cv %>%
  group_by(Target, Variable) %>%
  summarise(
    Selection_Frequency        = mean(Selected) * 100,
    Median_Max_Abs_Coefficient = median(Max_Abs_Coefficient),
    .groups = "drop"
  ) %>%
  arrange(Target, desc(Selection_Frequency))

print(selection_stability %>% mutate(across(where(is.numeric), ~ round(.x, 4))))

## Tabel jawaban utama ----
final_answer <- selection_summary %>%
  select(Variable, Selected_Y1, Max_Abs_Coefficient_Y1, Selected_Y2, Max_Abs_Coefficient_Y2) %>%
  left_join(
    selection_stability %>%
      filter(Target == "Y1") %>%
      select(Variable, CV_Selection_Frequency_Y1 = Selection_Frequency),
    by = "Variable"
  ) %>%
  left_join(
    selection_stability %>%
      filter(Target == "Y2") %>%
      select(Variable, CV_Selection_Frequency_Y2 = Selection_Frequency),
    by = "Variable"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(final_answer)

# =============================================================================
# Persiapan Library -----------------------------------------------------------
# Jalankan SEKALI jika paket belum terpasang:
# install.packages("ranger")

suppressPackageStartupMessages({
  library(ranger)      # implementasi Random Forest yang cepat
})


# Langkah 7 - Random Forest Benchmark -----------------------------------------

## Pengaturan RF ----
RF_NUM_TREES <- 500
RF_GRID <- expand_grid(
  mtry          = c(2, 4, 6, 8),       # jumlah prediktor acak di tiap split (maks 8)
  min.node.size = c(1, 5, 10)          # ukuran minimum node daun
)
print(RF_GRID)

# Level kategori X6 dan X8 ditetapkan dari seluruh data (hanya level, tanpa Y),
# agar faktor konsisten di semua split/fold.
RF_LEVELS <- lapply(df[CATEGORICAL_FEATURES], function(v) sort(unique(v)))

rf_frame <- function(X) {
  X <- as.data.frame(X)
  for (v in CATEGORICAL_FEATURES) X[[v]] <- factor(X[[v]], levels = RF_LEVELS[[v]])
  X
}

## Fungsi tuning + fit RF untuk SATU response ----
# Setiap kombinasi grid di-fit pada data training, lalu dipilih yang OOB RMSE-nya
# minimum. Model terbaik kemudian di-fit ulang dengan permutation importance.
fit_rf_model <- function(X_tr, y_tr, seed = 42, importance = "none") {
  dat <- data.frame(rf_frame(X_tr), y = y_tr)
  
  grid_res <- RF_GRID %>%
    mutate(OOB_RMSE = map2_dbl(mtry, min.node.size, function(m, nd) {
      fit <- ranger(
        y ~ ., data = dat, num.trees = RF_NUM_TREES, mtry = m,
        min.node.size = nd, respect.unordered.factors = "order",
        seed = seed, num.threads = 1
      )
      sqrt(fit$prediction.error)          # prediction.error = OOB MSE
    }))
  
  best <- grid_res %>% slice_min(OOB_RMSE, n = 1, with_ties = FALSE)
  
  model <- ranger(
    y ~ ., data = dat, num.trees = RF_NUM_TREES, mtry = best$mtry,
    min.node.size = best$min.node.size, respect.unordered.factors = "order",
    importance = importance, seed = seed, num.threads = 1
  )
  
  list(model = model, grid = grid_res, best = best)
}

predict_rf <- function(rf_fit, X_new) {
  as.numeric(predict(rf_fit$model, data = rf_frame(X_new))$predictions)
}


## 7.1 Tuning RF pada data training (split yang sama dengan Lasso) ----
rf_models <- setNames(
  lapply(TARGETS, function(t) fit_rf_model(X_train, Y_train[[t]], seed = 42,
                                           importance = "permutation")),
  TARGETS
)

rf_best_params <- map_dfr(TARGETS, function(t) {
  rf_models[[t]]$best %>% mutate(Target = t, .before = 1)
})
print(rf_best_params)

# Visual hasil grid tuning (OOB RMSE)
plot_rf_grid <- function(t) {
  ggplot(rf_models[[t]]$grid,
         aes(x = factor(mtry), y = factor(min.node.size), fill = OOB_RMSE)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.3f", OOB_RMSE)), size = 3.5) +
    scale_fill_gradient(low = "#2C7FB8", high = "#EDF8B1") +
    labs(title = paste("RF Tuning (OOB RMSE) -", t),
         x = "mtry", y = "min.node.size", fill = "OOB RMSE")
}
print(plot_rf_grid("Y1") | plot_rf_grid("Y2"))


## 7.2 Evaluasi test set: OLS vs Lasso vs Random Forest ----
rf_test_results <- map_dfr(TARGETS, function(t) {
  pred <- predict_rf(rf_models[[t]], X_test)
  tibble(
    Target = t, Model = "Random Forest",
    RMSE = metric_rmse(Y_test[[t]], pred),
    MAE  = metric_mae(Y_test[[t]], pred),
    R2   = metric_r2(Y_test[[t]], pred)
  )
})

test_results_all <- bind_rows(test_results, rf_test_results) %>%
  mutate(Model = factor(Model, levels = c("Linear Regression", "Lasso", "Random Forest"))) %>%
  arrange(Target, Model)
print(test_results_all)

# Actual vs Predicted (format sama dengan plot_avp Langkah 6).
# Fungsi dibuat mandiri agar tidak bergantung pada plot_avp yang mungkin
# tertimpa oleh objek lain di environment.
plot_avp_l7 <- function(t, model_name) {
  pred <- if (model_name == "Random Forest") {
    predict_rf(rf_models[[t]], X_test)
  } else {
    predict_model(fitted_models[[t]], model_name, X_test)
  }
  d <- tibble(Actual = Y_test[[t]], Predicted = pred)
  ggplot(d, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.65) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = paste(t, "-", model_name))
}
print(
  (plot_avp_l7("Y1", "Lasso") | plot_avp_l7("Y1", "Random Forest")) /
    (plot_avp_l7("Y2", "Lasso") | plot_avp_l7("Y2", "Random Forest"))
)


## 7.3 Repeated CV RF dengan fold yang SAMA dengan Lasso ----
# fold_assign dibangkitkan ulang dengan seed yang sama (42 + rep_id), sehingga
# partisi fold identik dengan Langkah 6. Di setiap outer training fold, RF
# di-tuning ulang dengan OOB (validation fold tidak dipakai untuk tuning).
rf_cv_rows <- list()

for (rep_id in seq_len(N_REPEATS)) {
  set.seed(42 + rep_id)
  fold_assign <- sample(rep(seq_len(N_SPLITS), length.out = n_obs))
  
  for (k in seq_len(N_SPLITS)) {
    tr      <- which(fold_assign != k)
    te      <- which(fold_assign == k)
    X_tr    <- df[tr, FEATURES]
    X_te    <- df[te, FEATURES]
    fold_no <- (rep_id - 1) * N_SPLITS + k
    
    for (t in TARGETS) {
      y_tr <- df[[t]][tr]
      y_te <- df[[t]][te]
      fit  <- fit_rf_model(X_tr, y_tr, seed = 1000 * rep_id + k)
      pred <- predict_rf(fit, X_te)
      
      rf_cv_rows[[length(rf_cv_rows) + 1]] <- tibble(
        Target = t, Model = "Random Forest", Repeat = rep_id, Fold = fold_no,
        RMSE = metric_rmse(y_te, pred),
        MAE  = metric_mae(y_te, pred),
        R2   = metric_r2(y_te, pred),
        mtry = fit$best$mtry, min.node.size = fit$best$min.node.size
      )
    }
  }
  cat("Repeated CV RF: repeat", rep_id, "dari", N_REPEATS, "selesai\n")
}

rf_cv_results <- bind_rows(rf_cv_rows)

# Hyperparameter terpilih di 50 outer folds (stabilitas tuning)
rf_tuning_stability <- rf_cv_results %>%
  count(Target, mtry, min.node.size, name = "N_Folds") %>%
  group_by(Target) %>%
  mutate(Percent = N_Folds / sum(N_Folds) * 100) %>%
  ungroup() %>%
  arrange(Target, desc(N_Folds))
print(rf_tuning_stability)

# Gabungkan dengan hasil CV OLS dan Lasso
cv_results_all <- bind_rows(
  cv_results,
  rf_cv_results %>% select(Target, Model, Repeat, Fold, RMSE, MAE, R2)
) %>%
  mutate(Model = factor(Model, levels = c("Linear Regression", "Lasso", "Random Forest")))


## 7.4 Ringkasan ketidakpastian performa (3 model) ----
cv_summary_all <- cv_results_all %>%
  group_by(Target, Model) %>%
  summarise(
    RMSE_Mean = mean(RMSE),
    RMSE_SD   = sd(RMSE),
    RMSE_Q025 = quantile(RMSE, 0.025, names = FALSE),
    RMSE_Q975 = quantile(RMSE, 0.975, names = FALSE),
    MAE_Mean  = mean(MAE),
    MAE_SD    = sd(MAE),
    R2_Mean   = mean(R2),
    R2_SD     = sd(R2),
    R2_Q025   = quantile(R2, 0.025, names = FALSE),
    R2_Q975   = quantile(R2, 0.975, names = FALSE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(cv_summary_all)

p_cv_all <- cv_results_all %>%
  pivot_longer(c(RMSE, MAE, R2), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("RMSE", "MAE", "R2"))) %>%
  ggplot(aes(x = Target, y = Value, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Repeated CV (10-fold x 5 repeats): OLS vs Lasso vs Random Forest", y = NULL)
print(p_cv_all)


## 7.5 Perbandingan paired Lasso vs Random Forest per fold ----
# Selisih = RMSE_Lasso - RMSE_RF pada fold yang sama.
# Selisih > 0 berarti RF lebih akurat pada fold tersebut.
# Q025-Q975 = 95% empirical interval dari 50 fold (bukan uji hipotesis formal,
# karena fold-fold repeated CV saling bergantung).
paired_diff <- cv_results_all %>%
  filter(Model %in% c("Lasso", "Random Forest")) %>%
  select(Target, Fold, Model, RMSE) %>%
  pivot_wider(names_from = Model, values_from = RMSE) %>%
  mutate(Diff_RMSE = Lasso - `Random Forest`)

paired_summary <- paired_diff %>%
  group_by(Target) %>%
  summarise(
    Mean_Diff_RMSE   = mean(Diff_RMSE),
    SD_Diff_RMSE     = sd(Diff_RMSE),
    Q025_Diff_RMSE   = quantile(Diff_RMSE, 0.025, names = FALSE),
    Q975_Diff_RMSE   = quantile(Diff_RMSE, 0.975, names = FALSE),
    Pct_Folds_RF_Better = mean(Diff_RMSE > 0) * 100,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(paired_summary)

p_paired <- ggplot(paired_diff, aes(x = Target, y = Diff_RMSE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.2) +
  labs(title = "Paired Difference per Fold: RMSE Lasso - RMSE Random Forest",
       subtitle = "> 0 : Random Forest lebih akurat pada fold tersebut",
       x = "Target", y = "Selisih RMSE")
print(p_paired)


## 7.6 Permutation importance RF vs seleksi Lasso ----
# Importance = kenaikan MSE (OOB) ketika nilai satu variabel diacak.
# Ini ukuran kontribusi PREDIKTIF, bukan bukti signifikansi/kausal.
rf_importance <- map_dfr(TARGETS, function(t) {
  imp <- rf_models[[t]]$model$variable.importance
  tibble(Target = t, Variable = names(imp), Permutation_Importance = unname(imp))
}) %>%
  group_by(Target) %>%
  mutate(
    Importance_Pct = Permutation_Importance / sum(pmax(Permutation_Importance, 0)) * 100,
    RF_Rank        = rank(-Permutation_Importance, ties.method = "min")
  ) %>%
  ungroup() %>%
  arrange(Target, RF_Rank)
print(rf_importance %>% mutate(across(where(is.numeric), ~ round(.x, 4))))

plot_rf_imp <- function(t) {
  rf_importance %>%
    filter(Target == t) %>%
    ggplot(aes(x = reorder(Variable, Permutation_Importance), y = Permutation_Importance)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(title = paste("RF Permutation Importance -", t),
         x = NULL, y = "Kenaikan MSE (OOB)")
}
print(plot_rf_imp("Y1") | plot_rf_imp("Y2"))

# Tabel pembanding: seleksi Lasso vs peringkat importance RF (level X1-X8)
lasso_vs_rf <- final_answer %>%
  select(Variable, Selected_Y1, CV_Selection_Frequency_Y1,
         Selected_Y2, CV_Selection_Frequency_Y2) %>%
  left_join(
    rf_importance %>% filter(Target == "Y1") %>%
      select(Variable, RF_Importance_Pct_Y1 = Importance_Pct, RF_Rank_Y1 = RF_Rank),
    by = "Variable"
  ) %>%
  left_join(
    rf_importance %>% filter(Target == "Y2") %>%
      select(Variable, RF_Importance_Pct_Y2 = Importance_Pct, RF_Rank_Y2 = RF_Rank),
    by = "Variable"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(lasso_vs_rf)


## 7.7 Tabel ringkas perbandingan model (jawaban benchmark) ----
model_comparison <- cv_summary_all %>%
  select(Target, Model, RMSE_Mean, RMSE_SD, MAE_Mean, R2_Mean) %>%
  left_join(
    test_results_all %>%
      select(Target, Model, Test_RMSE = RMSE, Test_MAE = MAE, Test_R2 = R2),
    by = c("Target", "Model")
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  arrange(Target, Model)
print(model_comparison)

# Opsional: simpan hasil ke CSV (hapus tanda # jika diperlukan)
# write_csv(test_results_all, "test_results_all.csv")
# write_csv(cv_summary_all,   "cv_summary_all.csv")
# write_csv(paired_summary,   "paired_lasso_vs_rf.csv")
# write_csv(lasso_vs_rf,      "lasso_vs_rf_importance.csv")
# write_csv(model_comparison, "model_comparison.csv")


# Panduan Membaca Hasil Langkah 7 ---------------------------------------------
# Inferensi statistik vs predictive accuracy:
#   - Lasso  : model linear terpenalti -> koefisien bertanda dan sparse, dapat
#              dijelaskan ("X7 naik 1 SD -> Y1 naik ... unit, ceteris paribus").
#              Seleksi dibaca dari koefisien nonzero + CV selection frequency.
#   - RF     : model non-parametrik -> menangkap non-linearitas dan interaksi
#              (mis. pola X7 pada LOWESS, efek X5 yang berupa dua kelompok),
#              tetapi tidak menghasilkan koefisien, arah efek, maupun p-value.
#              Importance hanya menunjukkan "seberapa dipakai untuk prediksi".
#
# Cara menulis temuan:
#   BENAR  : "RF menghasilkan RMSE CV rata-rata ... vs Lasso ...; RF lebih akurat
#             pada ...% fold, sehingga terdapat struktur non-linear yang tidak
#             ditangkap model linear."
#   BENAR  : "Lasso tetap dipakai untuk interpretasi karena memberikan model
#             sparse dengan arah efek yang jelas."
#   HINDARI: "X5 paling signifikan karena importance RF tertinggi."
#   HINDARI: "RF lebih baik secara keseluruhan" hanya dari satu split test set;
#            gunakan repeated CV dan paired difference.
#
# Catatan korelasi prediktor: pada RF, importance X2, X3, X4 (serta X1, X5) dapat
# "terbagi" karena saling berkorelasi kuat; importance rendah tidak berarti
# variabel tidak berpengaruh.