# =============================================================================
# DASHBOARD R SHINY  -  EnergiLasso v3
# Lasso Regression untuk Heating Load (Y1) dan Cooling Load (Y2)
# dengan Random Forest sebagai Benchmark Prediksi
# Energy Efficiency Dataset (ENB2012)
#
# Alur analisis = script "Hasil Analisis ANREG S2.R" (Langkah 2 - 7), tanpa
# perubahan metode, seed, grid, fold, maupun preprocessing.
#
# Susunan dashboard (3 tab):
#   1. Data & EDA
#   2. Model + Diagnostik
#   3. Prediksi Interaktif + Benchmark
#
# Cara menjalankan (RStudio):
#   1. Simpan file ini sebagai app.R di satu folder khusus.
#   2. Jalankan SEKALI jika paket belum terpasang:
#      install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "readxl",
#                         "glmnet", "lmtest", "ranger", "plotly"))
#   3. Buka app.R lalu klik "Run App".
#
# Data dimuat otomatis (tanpa unggah), dengan urutan pencarian:
#   a. ENB2012_data.xlsx / ENB2012_data.csv di folder yang sama dengan app.R
#   b. DEFAULT_PATH (path yang sama dengan script analisis)
#   c. Unduh otomatis dari UCI Machine Learning Repository, lalu disimpan di
#      folder app.R agar tidak perlu diunduh lagi.
# Hasil analisis di-cache di folder "cache_hasil": pembukaan pertama +/- 5 menit
# (repeated CV Lasso & RF), pembukaan berikutnya langsung tampil.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(tidyverse)
  library(readxl)
  library(glmnet)
  library(lmtest)
  library(ranger)
  library(plotly)
})

APP_VERSION  <- "3.0"
APP_DIR      <- getwd()
CACHE_DIR    <- file.path(APP_DIR, "cache_hasil")
DEFAULT_PATH <- "C:/Users/Ricar/Downloads/energy+efficiency/ENB2012_data.xlsx"
UCI_ZIP_URL  <- "https://archive.ics.uci.edu/static/public/242/energy+efficiency.zip"
MIRROR_CSV   <- "https://raw.githubusercontent.com/Yrzxiong/Energy-Efficiency-Analysis/master/ENB2012_data.csv"
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. DEFINISI VARIABEL (sama dengan script)
# =============================================================================
FEATURES             <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")
TARGETS              <- c("Y1", "Y2")
NUMERIC_FEATURES     <- c("X1", "X2", "X3", "X4", "X5", "X7")
CATEGORICAL_FEATURES <- c("X6", "X8")
VARS                 <- c(FEATURES, TARGETS)

VAR_DESC <- c(
  X1 = "Relative Compactness", X2 = "Surface Area", X3 = "Wall Area", X4 = "Roof Area",
  X5 = "Overall Height", X6 = "Orientation", X7 = "Glazing Area",
  X8 = "Glazing Area Distribution", Y1 = "Heating Load", Y2 = "Cooling Load"
)
variable_info <- tibble(
  Variable = c(FEATURES, TARGETS),
  Description = unname(VAR_DESC[c(FEATURES, TARGETS)]),
  Role = c(rep("Predictor", 8), rep("Response", 2))
)

# Label kategori (ENB2012): orientasi 2-5 = Utara, Timur, Selatan, Barat
X6_LABEL <- c("2" = "Utara", "3" = "Timur", "4" = "Selatan", "5" = "Barat")
X8_LABEL <- c("0" = "Tanpa kaca", "1" = "Merata", "2" = "Dominan Utara",
              "3" = "Dominan Timur", "4" = "Dominan Selatan", "5" = "Dominan Barat")


# =============================================================================
# 2. MEMBACA DATA OTOMATIS (tanpa unggah)
# =============================================================================
NAME_ALIASES <- c(
  RELATIVECOMPACTNESS = "X1", SURFACEAREA = "X2", WALLAREA = "X3",
  ROOFAREA = "X4", OVERALLHEIGHT = "X5", ORIENTATION = "X6",
  GLAZINGAREA = "X7", GLAZINGAREADISTRIBUTION = "X8",
  HEATINGLOAD = "Y1", COOLINGLOAD = "Y2"
)

to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "-")] <- NA
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

read_csv_smart <- function(path) {
  first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  n_semi  <- lengths(regmatches(first, gregexpr(";", first)))
  n_comma <- lengths(regmatches(first, gregexpr(",", first)))
  n_tab   <- lengths(regmatches(first, gregexpr("\t", first)))
  sep <- c(";", ",", "\t")[which.max(c(n_semi, n_comma, n_tab))]
  utils::read.table(path, sep = sep, header = TRUE, quote = "\"",
                    stringsAsFactors = FALSE, check.names = FALSE,
                    na.strings = c("", "NA"), fill = TRUE,
                    colClasses = "character", fileEncoding = "UTF-8-BOM")
}

standardise_columns <- function(raw) {
  raw <- as.data.frame(raw, check.names = FALSE, stringsAsFactors = FALSE)
  keep <- vapply(raw, function(x) !all(is.na(x) | trimws(as.character(x)) == ""), logical(1))
  raw  <- raw[, keep, drop = FALSE]
  key <- toupper(gsub("[^A-Za-z0-9]", "", names(raw)))
  key <- ifelse(key %in% names(NAME_ALIASES), NAME_ALIASES[key], key)
  names(raw) <- key
  if (!all(VARS %in% names(raw))) {
    if (ncol(raw) == length(VARS)) names(raw) <- VARS
    else return(list(ok = FALSE, msg = paste0("Kolom tidak ditemukan: ",
                                              paste(setdiff(VARS, names(raw)), collapse = ", "))))
  }
  list(ok = TRUE, data = raw)
}

read_energy_data <- function(path) {
  ext <- tolower(tools::file_ext(path))
  out <- tryCatch({
    if (ext %in% c("xlsx", "xls")) standardise_columns(read_excel(path, sheet = 1))
    else standardise_columns(read_csv_smart(path))
  }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
  if (!isTRUE(out$ok)) return(out)
  d <- out$data[, VARS, drop = FALSE]
  d[] <- lapply(d, to_num)
  d <- as_tibble(d[rowSums(!is.na(d)) > 0, , drop = FALSE])
  if (any(is.na(d))) return(list(ok = FALSE, msg = "Terdapat nilai kosong / non-numerik."))
  list(ok = TRUE, data = d)
}

# Cari file data; jika tidak ada, unduh sekali lalu simpan di folder app
locate_data <- function() {
  cand <- c(file.path(APP_DIR, "ENB2012_data.xlsx"), file.path(APP_DIR, "ENB2012_data.csv"),
            DEFAULT_PATH)
  hit <- cand[file.exists(cand)]
  if (length(hit)) return(hit[1])

  zip_tmp <- tempfile(fileext = ".zip")
  ok <- tryCatch({
    utils::download.file(UCI_ZIP_URL, zip_tmp, mode = "wb", quiet = TRUE)
    files <- utils::unzip(zip_tmp, list = TRUE)$Name
    xl <- files[grepl("ENB2012_data\\.xlsx$", files)][1]
    utils::unzip(zip_tmp, files = xl, exdir = tempdir(), junkpaths = TRUE)
    file.copy(file.path(tempdir(), basename(xl)), cand[1], overwrite = TRUE)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (isTRUE(ok) && file.exists(cand[1])) return(cand[1])

  ok <- tryCatch({
    utils::download.file(MIRROR_CSV, cand[2], mode = "wb", quiet = TRUE); TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (isTRUE(ok) && file.exists(cand[2])) return(cand[2])
  NULL
}


# =============================================================================
# 3. FUNGSI ANALISIS (identik dengan script)
# =============================================================================
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
  num <- do.call(cbind, lapply(NUMERIC_FEATURES, function(v) {
    (X[[v]] - prep$center[[v]]) / prep$scale[[v]]
  }))
  colnames(num) <- NUMERIC_FEATURES

  cat_list <- lapply(CATEGORICAL_FEATURES, function(v) {
    lv <- prep$levels[[v]][-1]
    m  <- matrix(vapply(lv, function(l) as.numeric(X[[v]] == l), numeric(nrow(X))),
                 nrow = nrow(X), ncol = length(lv))
    colnames(m) <- paste0(v, "_", lv)
    m
  })

  cbind(num, do.call(cbind, cat_list))
}

LAMBDA_GRID <- 10^seq(2, -4, length.out = 200)
INNER_FOLDS <- 5

fit_target_models <- function(X_tr, y_tr, seed = 42) {
  prep <- fit_prep(X_tr)
  Z    <- apply_prep(prep, X_tr)

  baseline <- lm(y ~ ., data = data.frame(y = y_tr, Z, check.names = FALSE))

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
    suppressWarnings(as.numeric(predict(fit$baseline, newdata = data.frame(Z, check.names = FALSE))))
  } else {
    as.numeric(predict(fit$lasso, newx = Z, s = "lambda.min"))
  }
}

lasso_coef_table <- function(fit) {
  b <- as.matrix(coef(fit$lasso, s = "lambda.min"))
  b <- b[rownames(b) != "(Intercept)", 1]
  tibble(
    Feature     = names(b),
    Coefficient = unname(b),
    Selected    = abs(unname(b)) > 1e-10
  )
}

original_variable <- function(feature_name) sub("_.*$", "", feature_name)

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

vif_one <- function(j, M) {
  y   <- M[, j]
  fit <- lm(y ~ M[, -j, drop = FALSE])
  r2  <- 1 - sum(residuals(fit)^2) / sum((y - mean(y))^2)
  1 / (1 - r2)
}

jarque_bera_p <- function(x) {
  n  <- length(x)
  m  <- x - mean(x)
  m2 <- mean(m^2); m3 <- mean(m^3); m4 <- mean(m^4)
  S  <- m3 / m2^1.5
  K  <- m4 / m2^2
  JB <- n / 6 * (S^2 + (K - 3)^2 / 4)
  pchisq(JB, df = 2, lower.tail = FALSE)
}

metric_rmse <- function(y, p) sqrt(mean((y - p)^2))
metric_mae  <- function(y, p) mean(abs(y - p))
metric_r2   <- function(y, p) 1 - sum((y - p)^2) / sum((y - mean(y))^2)

N_SPLITS  <- 10
N_REPEATS <- 5


# =============================================================================
# 3b. FUNGSI RANDOM FOREST (identik dengan Langkah 7)
# =============================================================================
RF_NUM_TREES <- 500
RF_GRID <- expand_grid(
  mtry          = c(2, 4, 6, 8),
  min.node.size = c(1, 5, 10)
)

rf_frame <- function(X, rf_levels) {
  X <- as.data.frame(X)
  for (v in CATEGORICAL_FEATURES) X[[v]] <- factor(X[[v]], levels = rf_levels[[v]])
  X
}

fit_rf_model <- function(X_tr, y_tr, rf_levels, seed = 42, importance = "none") {
  dat <- data.frame(rf_frame(X_tr, rf_levels), y = y_tr)

  grid_res <- RF_GRID %>%
    mutate(OOB_RMSE = map2_dbl(mtry, min.node.size, function(m, nd) {
      fit <- ranger(
        y ~ ., data = dat, num.trees = RF_NUM_TREES, mtry = m,
        min.node.size = nd, respect.unordered.factors = "order",
        seed = seed, num.threads = 1
      )
      sqrt(fit$prediction.error)
    }))

  best <- grid_res %>% slice_min(OOB_RMSE, n = 1, with_ties = FALSE)

  model <- ranger(
    y ~ ., data = dat, num.trees = RF_NUM_TREES, mtry = best$mtry,
    min.node.size = best$min.node.size, respect.unordered.factors = "order",
    importance = importance, seed = seed, num.threads = 1
  )

  list(model = model, grid = grid_res, best = best)
}

predict_rf <- function(rf_fit, X_new, rf_levels) {
  as.numeric(predict(rf_fit$model, data = rf_frame(X_new, rf_levels))$predictions)
}

MODEL_LEVELS <- c("Linear Regression", "Lasso", "Random Forest")


# =============================================================================
# 4. MENJALANKAN SELURUH ALUR ANALISIS (Langkah 3 - 7)
# =============================================================================
run_analysis <- function(df, progress = function(value, detail) NULL) {
  R <- list()
  R$df <- df

  # ---- Langkah 3: EDA ----
  progress(0.02, "EDA")
  vars <- VARS
  R$quality <- tibble(
    Variable = vars,
    dtype    = vapply(df[vars], function(x) class(x)[1], character(1), USE.NAMES = FALSE),
    missing  = vapply(df[vars], function(x) sum(is.na(x)), numeric(1), USE.NAMES = FALSE),
    n_unique = vapply(df[vars], function(x) n_distinct(x), numeric(1), USE.NAMES = FALSE)
  )
  R$n_duplicate  <- sum(duplicated(df))
  R$describe_all <- describe_tbl(df[vars])
  R$describe_y   <- describe_tbl(df[TARGETS])
  R$dependency_max <- max(abs(df$X2 - (df$X3 + 2 * df$X4)))

  spearman_cols   <- c("X1", "X2", "X3", "X4", "X5", "X7", "Y1", "Y2")
  R$spearman_cols <- spearman_cols
  R$spearman_corr <- cor(df[spearman_cols], method = "spearman")

  # ---- Langkah 4: Split, fit OLS + Lasso ----
  progress(0.06, "Split data & fit Lasso")
  set.seed(42)
  n_obs    <- nrow(df)
  n_test   <- ceiling(0.20 * n_obs)
  test_idx <- sample(n_obs, n_test)

  X_train <- df[-test_idx, FEATURES];  X_test <- df[test_idx, FEATURES]
  Y_train <- df[-test_idx, TARGETS];   Y_test <- df[test_idx, TARGETS]
  R$X_test <- X_test; R$Y_test <- Y_test
  R$dims <- c(train_n = nrow(X_train), train_p = ncol(X_train),
              test_n = nrow(X_test), test_p = ncol(X_test))

  fitted_models <- setNames(
    lapply(TARGETS, function(t) fit_target_models(X_train, Y_train[[t]])),
    TARGETS
  )
  R$fitted_models <- fitted_models
  R$lambda_min <- vapply(TARGETS, function(t) fitted_models[[t]]$lasso$lambda.min, numeric(1))

  lasso_feature_tables <- list()
  for (t in TARGETS) {
    lasso_feature_tables[[t]] <- lasso_coef_table(fitted_models[[t]]) %>%
      arrange(desc(abs(Coefficient)))
  }
  R$lasso_feature_tables <- lasso_feature_tables

  selection_tables <- lapply(lasso_feature_tables, summarise_selection)
  selection_summary <- inner_join(
    selection_tables[["Y1"]], selection_tables[["Y2"]],
    by = "Variable", suffix = c("_Y1", "_Y2")
  )
  R$selection_summary <- selection_summary

  # ---- Langkah 5: Diagnostic ----
  progress(0.10, "Diagnostic model")
  prep_for_ols   <- fit_prep(X_train)
  X_train_design <- apply_prep(prep_for_ols, X_train)
  X_train_const  <- cbind(const = 1, X_train_design)

  sv               <- svd(X_train_const)$d
  matrix_rank      <- sum(sv > max(dim(X_train_const)) * max(sv) * .Machine$double.eps)
  n_columns        <- ncol(X_train_const)
  condition_number <- max(sv) / min(sv)
  R$rank_info <- tibble(
    Keterangan = c("Jumlah kolom design matrix", "Rank design matrix",
                   "Rank deficient?", "Condition number"),
    Nilai = c(as.character(n_columns), as.character(matrix_rank),
              as.character(matrix_rank < n_columns), format(condition_number, digits = 6))
  )

  R$vif_table <- tibble(
    Variable = colnames(X_train_design),
    VIF      = vapply(seq_len(ncol(X_train_design)), vif_one, numeric(1), M = X_train_design)
  ) %>%
    arrange(desc(VIF))

  ols_models      <- list()
  diagnostic_rows <- list()
  cook_threshold  <- 4 / nrow(X_train)

  for (t in TARGETS) {
    m <- lm(y ~ ., data = data.frame(y = Y_train[[t]], X_train_design, check.names = FALSE))
    ols_models[[t]] <- m
    res     <- residuals(m)
    bp      <- bptest(m, studentize = TRUE)
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
  R$ols_models <- ols_models
  R$cook_threshold <- cook_threshold
  R$diagnostic_summary <- bind_rows(diagnostic_rows)

  # ---- Langkah 6: Test set ----
  progress(0.13, "Evaluasi test set")
  R$test_results <- map_dfr(TARGETS, function(t) {
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

  # ---- Langkah 6: Repeated CV ----
  cv_rows           <- list()
  selection_records <- list()
  total <- N_REPEATS * N_SPLITS

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
      progress(0.15 + 0.25 * fold_no / total,
               sprintf("Repeated CV Lasso: repeat %d/%d, fold %d/%d", rep_id, N_REPEATS, k, N_SPLITS))
    }
  }

  cv_results <- bind_rows(cv_rows)
  R$cv_results <- cv_results

  R$cv_summary <- cv_results %>%
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

  selection_cv <- bind_rows(selection_records)
  selection_stability <- selection_cv %>%
    group_by(Target, Variable) %>%
    summarise(
      Selection_Frequency        = mean(Selected) * 100,
      Median_Max_Abs_Coefficient = median(Max_Abs_Coefficient),
      .groups = "drop"
    ) %>%
    arrange(Target, desc(Selection_Frequency))
  R$selection_stability <- selection_stability

  R$final_answer <- selection_summary %>%
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

  # ===========================================================================
  # Langkah 7 - Random Forest Benchmark
  # ===========================================================================
  progress(0.41, "Tuning Random Forest")
  rf_levels <- lapply(df[CATEGORICAL_FEATURES], function(v) sort(unique(v)))

  rf_models <- setNames(
    lapply(TARGETS, function(t) fit_rf_model(X_train, Y_train[[t]], rf_levels, seed = 42,
                                             importance = "permutation")),
    TARGETS
  )
  # disimpan untuk fitur Prediksi Interaktif (tidak mengubah hasil analisis)
  R$rf_models <- rf_models
  R$rf_levels <- rf_levels

  R$rf_best_params <- map_dfr(TARGETS, function(t) rf_models[[t]]$best %>% mutate(Target = t, .before = 1))
  R$rf_grid <- map_dfr(TARGETS, function(t) rf_models[[t]]$grid %>% mutate(Target = t, .before = 1))

  rf_test_results <- map_dfr(TARGETS, function(t) {
    pred <- predict_rf(rf_models[[t]], X_test, rf_levels)
    tibble(
      Target = t, Model = "Random Forest",
      RMSE = metric_rmse(Y_test[[t]], pred),
      MAE  = metric_mae(Y_test[[t]], pred),
      R2   = metric_r2(Y_test[[t]], pred)
    )
  })
  R$test_results_all <- bind_rows(R$test_results, rf_test_results) %>%
    mutate(Model = factor(Model, levels = MODEL_LEVELS)) %>%
    arrange(Target, Model)

  R$avp <- map_dfr(TARGETS, function(t) {
    bind_rows(
      map_dfr(MODEL_NAMES, function(mn) tibble(Target = t, Model = mn, Actual = Y_test[[t]],
                                               Predicted = predict_model(fitted_models[[t]], mn, X_test))),
      tibble(Target = t, Model = "Random Forest", Actual = Y_test[[t]],
             Predicted = predict_rf(rf_models[[t]], X_test, rf_levels))
    )
  })

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
        fit  <- fit_rf_model(X_tr, y_tr, rf_levels, seed = 1000 * rep_id + k)
        pred <- predict_rf(fit, X_te, rf_levels)

        rf_cv_rows[[length(rf_cv_rows) + 1]] <- tibble(
          Target = t, Model = "Random Forest", Repeat = rep_id, Fold = fold_no,
          RMSE = metric_rmse(y_te, pred),
          MAE  = metric_mae(y_te, pred),
          R2   = metric_r2(y_te, pred),
          mtry = fit$best$mtry, min.node.size = fit$best$min.node.size
        )
      }
      progress(0.42 + 0.56 * fold_no / total,
               sprintf("Repeated CV Random Forest: repeat %d/%d, fold %d/%d", rep_id, N_REPEATS, k, N_SPLITS))
    }
  }
  rf_cv_results <- bind_rows(rf_cv_rows)

  R$rf_tuning_stability <- rf_cv_results %>%
    count(Target, mtry, min.node.size, name = "N_Folds") %>%
    group_by(Target) %>%
    mutate(Percent = N_Folds / sum(N_Folds) * 100) %>%
    ungroup() %>%
    arrange(Target, desc(N_Folds))

  cv_results_all <- bind_rows(
    cv_results,
    rf_cv_results %>% select(Target, Model, Repeat, Fold, RMSE, MAE, R2)
  ) %>%
    mutate(Model = factor(Model, levels = MODEL_LEVELS))
  R$cv_results_all <- cv_results_all

  R$cv_summary_all <- cv_results_all %>%
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

  paired_diff <- cv_results_all %>%
    filter(Model %in% c("Lasso", "Random Forest")) %>%
    mutate(Model = as.character(Model)) %>%
    select(Target, Fold, Model, RMSE) %>%
    pivot_wider(names_from = Model, values_from = RMSE) %>%
    mutate(Diff_RMSE = Lasso - `Random Forest`)
  R$paired_diff <- paired_diff

  R$paired_summary <- paired_diff %>%
    group_by(Target) %>%
    summarise(
      Mean_Diff_RMSE      = mean(Diff_RMSE),
      SD_Diff_RMSE        = sd(Diff_RMSE),
      Q025_Diff_RMSE      = quantile(Diff_RMSE, 0.025, names = FALSE),
      Q975_Diff_RMSE      = quantile(Diff_RMSE, 0.975, names = FALSE),
      Pct_Folds_RF_Better = mean(Diff_RMSE > 0) * 100,
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))

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
  R$rf_importance <- rf_importance

  R$lasso_vs_rf <- R$final_answer %>%
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

  R$model_comparison <- R$cv_summary_all %>%
    select(Target, Model, RMSE_Mean, RMSE_SD, MAE_Mean, R2_Mean) %>%
    left_join(
      R$test_results_all %>%
        select(Target, Model, Test_RMSE = RMSE, Test_MAE = MAE, Test_R2 = R2),
      by = c("Target", "Model")
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    arrange(Target, Model)

  progress(1, "Selesai")
  R
}

cache_path <- function(data_file) {
  key <- unname(tools::md5sum(data_file))
  file.path(CACHE_DIR, paste0("hasil_", key, "_v", APP_VERSION, ".rds"))
}


# =============================================================================
# 5. PENJELASAN SINGKAT (angka diambil langsung dari hasil analisis)
# =============================================================================
fp <- function(p) if (p < 0.001) "< 0,001" else sub("\\.", ",", formatC(p, format = "f", digits = 3))
fn <- function(x, d = 2) sub("\\.", ",", formatC(x, format = "f", digits = d))
lbl <- c(Y1 = "Heating Load (Y1)", Y2 = "Cooling Load (Y2)")
join_id <- function(v) {
  if (length(v) == 0) return("tidak ada")
  if (length(v) == 1) return(v)
  paste(paste(v[-length(v)], collapse = ", "), "dan", v[length(v)])
}

# Format penjelasan: satu kalimat cara membaca + satu/dua kalimat temuan
expl <- function(read, seen) {
  tagList(
    tags$p(class = "ex-read", icon("book-open"), " ", HTML(read)),
    lapply(seen, function(s) tags$p(class = "ex-seen", icon("eye"), " ", HTML(s)))
  )
}

n_peaks <- function(x) {
  d  <- density(x)
  pk <- which(diff(sign(diff(d$y))) == -2) + 1
  sum(d$y[pk] > 0.10 * max(d$y))
}

# ---- Tab 1 ----
ex_dist <- function(df_all, df_f, t) {
  x <- df_all[[t]]
  s <- sprintf("%s berkisar %s–%s (median %s)%s",
               lbl[t], fn(min(x)), fn(max(x)), fn(median(x)),
               if (n_peaks(x) >= 2) ", dengan dua kelompok: bangunan beban rendah dan beban tinggi." else ".")
  if (nrow(df_f) > 0 && nrow(df_f) < nrow(df_all))
    s <- paste0(s, sprintf(" Hasil filter: <b>%d bangunan</b>, median %s.", nrow(df_f), fn(median(df_f[[t]]))))
  expl("Abu-abu = seluruh data, berwarna = bangunan hasil filter.", list(s))
}

ex_rel <- function(df, t, v, span) {
  if (v %in% CATEGORICAL_FEATURES) {
    md <- df %>% group_by(k = .data[[v]]) %>% summarise(m = median(.data[[t]]), .groups = "drop")
    rng <- max(md$m) - min(md$m)
    return(expl("Bandingkan garis median antar kotak: makin sejajar, makin kecil pengaruh kategori.",
                list(sprintf("Selisih median antar kategori %s = %s, jadi %s.", v, fn(rng),
                             if (rng < 0.1 * sd(df[[t]])) "praktis tidak berpengaruh" else "cukup berpengaruh"))))
  }
  lw <- lowess(df[[v]], df[[t]], f = span, iter = 3, delta = 0)
  u  <- tibble(x = lw$x, y = lw$y) %>% distinct(x, .keep_all = TRUE) %>% arrange(x)
  sl <- if (nrow(u) > 2) diff(u$y) / diff(u$x) else 1
  curved <- length(sl) > 1 && (any(sign(sl) != sign(sl[1])) || max(abs(sl)) / max(min(abs(sl)), 1e-9) > 1.5)
  expl("Garis tebal = tren LOWESS. Span kecil = garis lebih lentur, span besar = lebih mulus.",
       list(sprintf("ρ Spearman %s dengan %s = %s. Trennya %s.", v, t,
                    fn(suppressWarnings(cor(df[[v]], df[[t]], method = "spearman"))),
                    if (curved) "<b>melengkung</b>, sehingga model linear hanya menangkap kemiringan rata-ratanya" else "hampir lurus")))
}

ex_corr <- function(R, t, th) {
  sc <- R$spearman_corr; preds <- c("X1", "X2", "X3", "X4", "X5", "X7")
  v <- sc[preds, t]; o <- order(-abs(v))
  m <- sc[preds, preds]; m[upper.tri(m, diag = TRUE)] <- NA
  ij <- which(abs(m) == max(abs(m), na.rm = TRUE), arr.ind = TRUE)[1, ]
  expl("Merah = naik bersama, biru = berlawanan arah. Kotak di bawah ambang dipudarkan.",
       list(sprintf("Terkuat terhadap %s: %s (ρ = %s) dan %s (ρ = %s). Antar-prediktor, %s–%s sangat erat (ρ = %s): sumber multikolinearitas.",
                    t, preds[o[1]], fn(v[o[1]]), preds[o[2]], fn(v[o[2]]),
                    rownames(m)[ij[1]], colnames(m)[ij[2]], fn(m[ij[1], ij[2]]))))
}

# ---- Tab 2 ----
ex_lambda <- function(R, t, s) {
  expl("Geser λ ke kanan: penalti membesar, koefisien menyusut, dan yang abu-abu sudah dibuang Lasso.",
       list(sprintf("Pada λ.min (hasil resmi), Lasso untuk %s mempertahankan %d dari 14 fitur. Pada λ yang Anda pilih: <b>%d fitur</b>, RMSE test %s.",
                    t, sum(R$lasso_feature_tables[[t]]$Selected), s$nz, fn(s$te, 3))))
}

ex_stab <- function(R, t) {
  d <- R$selection_stability %>% filter(Target == t)
  expl("Persentase dari 50 fold di mana variabel dipilih Lasso. Hijau = selalu terpilih (100%).",
       list(sprintf("Selalu terpilih: <b>%s</b>. Tidak selalu: %s.",
                    join_id(d$Variable[d$Selection_Frequency == 100]),
                    join_id(sprintf("%s (%s%%)", d$Variable[d$Selection_Frequency < 100],
                                    fn(d$Selection_Frequency[d$Selection_Frequency < 100], 0))))))
}

ex_diag <- function(R, t) {
  r  <- R$diagnostic_summary %>% filter(Target == t)
  ri <- R$rank_info$Nilai
  v  <- R$vif_table
  expl("VIF > 10 (merah) = multikolinearitas. Residual idealnya acak di sekitar nol; Q-Q idealnya menempel garis.",
       list(sprintf("Rank design matrix %s dari %s kolom (X2 = X3 + 2·X4), VIF ≈∞ pada %s. Karena itu p-value OLS tidak dipakai.",
                    ri[2], ri[1], join_id(v$Variable[!is.finite(v$VIF) | v$VIF > 1e6])),
            sprintf("OLS %s: R² = %s, Breusch-Pagan p = %s (%s), Jarque-Bera p = %s (%s). Ada %d observasi di atas batas Cook 4/n; tidak dihapus karena data simulasi.",
                    t, fn(r$R2_OLS, 3), fp(r$Breusch_Pagan_p),
                    if (r$Breusch_Pagan_p < 0.05) "heteroskedastis" else "varians konstan",
                    fp(r$Jarque_Bera_p), if (r$Jarque_Bera_p < 0.05) "tidak normal" else "normal",
                    r$N_Influential)))
}

# ---- Tab 3 ----
ex_bench <- function(R, t) {
  d  <- R$cv_summary_all %>% filter(Target == t) %>% arrange(RMSE_Mean)
  ps <- R$paired_summary %>% filter(Target == t)
  expl("RMSE makin kecil makin akurat. Titik di atas garis nol = fold di mana RF lebih akurat dari Lasso.",
       list(sprintf("Urutan RMSE CV: %s. RF lebih akurat dari Lasso pada <b>%s%%</b> dari 50 fold.",
                    paste(sprintf("%s (%s)", d$Model, fn(d$RMSE_Mean, 3)), collapse = " < "),
                    fn(ps$Pct_Folds_RF_Better, 0))))
}

ex_imp <- function(R, t) {
  d <- R$rf_importance %>% filter(Target == t) %>% arrange(RF_Rank)
  fa <- R$final_answer
  expl("Panjang batang = seberapa diandalkan RF. Warna = dipilih atau dibuang Lasso.",
       list(sprintf("Tiga teratas RF: %s. Lasso memilih: %s.", join_id(head(d$Variable, 3)),
                    join_id(fa$Variable[fa[[paste0("Selected_", t)]]]))))
}

ex_final <- function(R) {
  fa <- R$final_answer; cs <- R$cv_summary_all
  tagList(lapply(TARGETS, function(t) {
    sel  <- fa$Variable[fa[[paste0("Selected_", t)]]]
    best <- cs %>% filter(Target == t) %>% slice_min(RMSE_Mean, n = 1, with_ties = FALSE)
    las  <- cs %>% filter(Target == t, Model == "Lasso")
    tags$p(HTML(sprintf("<b>%s</b>: Lasso memilih %s (RMSE CV %s). Model paling akurat: %s (RMSE %s).",
                        lbl[t], join_id(sel), fn(las$RMSE_Mean, 3), best$Model, fn(best$RMSE_Mean, 3))))
  }))
}

# =============================================================================
# 6. GAYA TAMPILAN, ANIMASI, DAN ILUSTRASI BANGUNAN
# =============================================================================
PRIMARY   <- "#0B2447"   # navy
SECONDARY <- "#19376D"   # biru tua
ACCENT    <- "#F39C12"   # oranye (heating)
TEAL      <- "#0F9D9A"   # teal (cooling)
TCOL      <- c(Y1 = ACCENT, Y2 = TEAL)
MCOL      <- c("Linear Regression" = "#94A3B8", "Lasso" = SECONDARY, "Random Forest" = "#2E9E5B")
VCOL      <- c(X1 = "#0B2447", X2 = "#576CBC", X3 = "#A5B4FC", X4 = "#38BDF8", X5 = "#F39C12",
               X6 = "#E76F51", X7 = "#0F9D9A", X8 = "#8E7DBE")

css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;500;600;700&display=swap');
:root { --navy:#0B2447; --blue:#19376D; --orange:#F39C12; --teal:#0F9D9A; --tc:#F39C12; --tc-soft:#FEF3E2; }
body.tgt-Y2 { --tc:#0F9D9A; --tc-soft:#E1F5F4; }
body, h1, h2, h3, h4, h5, h6, .main-header .logo, .sidebar-menu, .box-title, table, .irs {
  font-family: 'Poppins', 'Segoe UI', Arial, sans-serif !important; }
body { color:#1f2937; }
.content-wrapper, .right-side { background:#F7F9FC !important; padding-bottom:50px; }

/* Header */
.skin-blue .main-header .logo { background:var(--navy) !important; color:#fff; font-weight:600; font-size:17px; }
.skin-blue .main-header .navbar { background:linear-gradient(90deg, var(--navy) 0%, var(--blue) 55%, var(--teal) 100%) !important; }
.skin-blue .main-header .navbar .sidebar-toggle { color:#fff; font-size:20px; }
.header-title { display:block; color:#fff; font-size:15px; font-weight:500; line-height:50px; padding:0 22px 0 12px; white-space:nowrap; }
@media (max-width:767px) { .header-title { display:none; } }
.logo-bolt { color:var(--orange); animation: flicker 2.4s infinite; display:inline-block; }

/* Sidebar */
.skin-blue .main-sidebar, .skin-blue .left-side { background:var(--navy) !important; }
.skin-blue .sidebar-menu > li > a { color:#dbe4f3; font-size:14px; border-left:4px solid transparent; padding:14px 15px; }
.skin-blue .sidebar-menu > li > a > svg, .skin-blue .sidebar-menu > li > a > .fa { width:22px; color:var(--orange); }
.skin-blue .sidebar-menu > li:hover > a, .skin-blue .sidebar-menu > li.active > a {
  background:var(--blue) !important; color:#fff; border-left-color:var(--orange); }
.side-block { margin:14px 14px 0 14px; padding:12px; border-radius:12px; background:rgba(255,255,255,.06); color:#dbe4f3; font-size:12.5px; }
.side-block h5 { color:#fff; font-weight:600; margin:0 0 8px 0; font-size:13px; }
.side-block ol { padding-left:16px; margin:0; line-height:1.7; }
.side-house { text-align:center; margin:6px 0 2px 0; }
.tgt-toggle .radio { margin:0; }
.tgt-toggle .shiny-options-group { display:flex; gap:6px; }
.tgt-toggle .radio label { flex:1; padding:0; }
.tgt-toggle .radio input { display:none; }
.tgt-toggle .radio label span { display:block; text-align:center; padding:8px 4px; border-radius:9px; cursor:pointer;
  background:rgba(255,255,255,.08); color:#dbe4f3; font-weight:500; transition:all .3s; font-size:12.5px; }
.tgt-toggle .radio:first-child input:checked + span { background:var(--orange); color:#fff; box-shadow:0 4px 14px rgba(243,156,18,.45); }
.tgt-toggle .radio:last-child  input:checked + span { background:var(--teal); color:#fff; box-shadow:0 4px 14px rgba(15,157,154,.45); }
.tgt-toggle { margin-top:4px; }
.tgt-toggle .control-label { display:none; }

/* Box */
.box { border-radius:14px; box-shadow:0 2px 12px rgba(11,36,71,.07); border-top:3px solid var(--blue);
       animation: fadeUp .6s ease both; background:#fff; }
.box.box-tc { border-top-color:var(--tc); transition:border-color .5s; }
.box-header .box-title { font-weight:600; font-size:15px; color:var(--navy); }
.box-header .box-title svg { color:var(--tc); transition:color .5s; }
.box.box-explain { border-top:3px solid var(--teal); background:#FAFCFF; }
.ex-read { font-size:13px; color:#6b7280; margin:0 0 6px 0; line-height:1.6; }
.ex-seen { font-size:14px; color:#1f2937; margin:0 0 4px 0; line-height:1.7; }
.ex-read svg, .ex-seen svg { color:var(--orange); }
.ex-sec { margin-bottom:10px; }
.ex-sec > b { display:block; color:var(--blue); font-size:13.5px; margin-bottom:3px; }
.ex-sec > b svg { color:var(--orange); }
.ex-sec p { font-size:14px; line-height:1.75; color:#374151; margin:0 0 6px 0; text-align:justify; }
.ex-sec p b { color:#111827; }

/* Panduan interaktif */
.howto { background:linear-gradient(90deg, var(--tc-soft), #fff 70%); border:1px dashed var(--tc); border-radius:12px;
         padding:10px 14px; margin-bottom:12px; font-size:13px; color:#374151; transition:all .5s; }
.howto .ht-title { font-weight:600; color:var(--navy); margin-bottom:4px; }
.howto .ht-title svg { color:var(--tc); animation: tap 1.6s infinite; }
.howto ul { margin:0; padding-left:18px; line-height:1.7; }
.howto kbd { background:var(--navy); color:#fff; border-radius:5px; font-size:11.5px; padding:1px 6px; box-shadow:none; }

/* Judul section */
.sec-head { display:flex; align-items:center; gap:12px; margin:26px 0 12px 0; animation: fadeUp .6s ease both; }
.sec-num { width:38px; height:38px; border-radius:11px; background:var(--navy); color:#fff; font-weight:700;
           display:flex; align-items:center; justify-content:center; font-size:16px; flex-shrink:0;
           box-shadow:0 6px 16px rgba(11,36,71,.25); }
.sec-head h3 { margin:0; font-weight:700; font-size:20px; color:var(--navy); }
.sec-head .sub { color:#6b7280; font-size:13px; }
.sec-head h3 svg { color:var(--tc); margin-right:6px; transition:color .5s; }

/* Hero / banner tab */
.hero { display:flex; align-items:center; gap:24px; flex-wrap:wrap; background:linear-gradient(120deg, var(--navy), var(--blue) 60%, #1f5f8b);
        border-radius:18px; padding:22px 26px; color:#fff; box-shadow:0 12px 30px rgba(11,36,71,.22); animation: fadeUp .7s ease both;
        position:relative; overflow:hidden; }
.hero .h-text { flex:1 1 380px; min-width:280px; z-index:1; }
.hero .h-art { flex:0 1 460px; min-width:260px; z-index:1; }
.hero h1 { font-weight:700; font-size:28px; line-height:1.25; margin:4px 0 8px 0; }
.hero p { color:#dbe4f3; font-size:14px; line-height:1.7; margin:0 0 10px 0; }
.hero .kicker { text-transform:uppercase; letter-spacing:2px; font-size:11.5px; color:var(--orange); font-weight:600; }
.chip { display:inline-block; padding:5px 13px; border-radius:20px; margin:3px 4px 3px 0; font-size:12.5px; font-weight:500; color:#fff; }
.steps { display:flex; gap:8px; flex-wrap:wrap; margin-top:10px; }
.step { background:rgba(255,255,255,.1); border:1px solid rgba(255,255,255,.18); border-radius:10px; padding:7px 11px; font-size:12.5px; }
.step b { color:var(--orange); margin-right:4px; }

/* KPI */
.kpi-row { display:flex; gap:14px; flex-wrap:wrap; margin:16px 0 4px 0; }
.kpi { flex:1 1 150px; background:#fff; border-radius:14px; padding:14px 16px; box-shadow:0 2px 12px rgba(11,36,71,.07);
       display:flex; gap:12px; align-items:center; animation: fadeUp .6s ease both; border-bottom:3px solid var(--tc); transition:border-color .5s; }
.kpi .ic { width:44px; height:44px; border-radius:12px; display:flex; align-items:center; justify-content:center; font-size:20px; color:#fff; flex-shrink:0; }
.kpi .v { font-size:24px; font-weight:700; color:var(--navy); line-height:1.1; }
.kpi .l { font-size:12px; color:#6b7280; }
.kpi:hover { transform:translateY(-3px); transition:transform .25s; }

/* Kartu prediksi */
.pred-card { border-radius:14px; padding:14px 16px; margin-bottom:12px; color:#fff; position:relative; overflow:hidden; }
.pred-card.Y1 { background:linear-gradient(135deg, #F39C12, #e67e22); }
.pred-card.Y2 { background:linear-gradient(135deg, #0F9D9A, #117a8b); }
.pred-card h4 { margin:0 0 8px 0; font-weight:600; font-size:15px; }
.pred-row { display:flex; justify-content:space-between; align-items:baseline; padding:4px 0; border-top:1px solid rgba(255,255,255,.22); font-size:13px; }
.pred-row .pm-val { font-size:22px; font-weight:700; }
.pred-row.best .pm-name::after { content:' ★'; color:#FFE8A3; }
.pred-card .unit { font-size:11px; opacity:.85; }
.pred-card .bg-ic { display:none; }
.pred-card.Y1 .bg-ic { animation: flicker 2.2s infinite; }
.pred-card.Y2 .bg-ic { animation: spin 12s linear infinite; }
.rating { border-radius:12px; padding:10px 14px; font-weight:600; text-align:center; font-size:14px; transition:all .5s; }
.rating.r1 { background:#DDF4EA; color:#0B7A4B; } .rating.r2 { background:#FEF3E2; color:#B45309; } .rating.r3 { background:#FDE2E1; color:#B91C1C; }
.badge-warn { display:inline-block; background:#FEF3E2; color:#B45309; border-radius:8px; padding:4px 10px; font-size:12px; margin-top:6px; }
.badge-ok { display:inline-block; background:#DDF4EA; color:#0B7A4B; border-radius:8px; padding:4px 10px; font-size:12px; margin-top:6px; }
.shape-tbl td { padding:2px 10px 2px 0; font-size:12.5px; } .shape-tbl td:first-child { color:#6b7280; }

/* Input */
.irs--shiny .irs-bar { background:var(--tc); border-color:var(--tc); transition:background .5s; }
.irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background:var(--navy); }
.irs--shiny .irs-handle { border-color:var(--tc); box-shadow:0 0 0 4px rgba(0,0,0,.04); }
.irs--shiny .irs-handle:hover { transform:scale(1.15); }
.slider-animate-button { color:var(--tc) !important; font-size:18px !important; opacity:1 !important; }
.btn-soft { background:#EEF3FA; color:var(--navy); border:none; border-radius:9px; font-weight:500; margin:2px 4px 2px 0; }
.btn-soft:hover { background:var(--tc); color:#fff; }
.btn-soft svg { margin-right:4px; }
.form-group { margin-bottom:10px; }
.control-label { font-weight:500; font-size:13px; color:var(--navy); }

/* Tabel */
table.dataTable thead th { background:#EEF3FA; color:var(--navy); font-weight:600; font-size:13px; border-bottom:2px solid var(--teal) !important; white-space:nowrap; }
table.dataTable tbody td { font-size:13px; padding:7px 10px !important; border-top:1px solid #EEF1F5; }
table.dataTable tbody tr:hover { background:#F2F7FF !important; }
.dataTables_wrapper .dataTables_paginate .paginate_button.current { background:var(--blue) !important; color:#fff !important; border:none !important; border-radius:6px; }
.pill { display:inline-block; padding:1px 10px; border-radius:10px; font-size:12px; font-weight:500; }
.pill-yes { background:#DDF4EA; color:#0B7A4B; } .pill-no { background:#F1F3F5; color:#6B7280; }

/* Ilustrasi & animasi */
@keyframes fadeUp { from { opacity:0; transform:translateY(14px); } to { opacity:1; transform:none; } }
@keyframes flicker { 0%,100% { transform:scale(1) rotate(-2deg); opacity:1; } 50% { transform:scale(1.12) rotate(3deg); opacity:.8; } }
@keyframes spin { to { transform:rotate(360deg); } }
@keyframes tap { 0%,100% { transform:translateY(0); } 50% { transform:translateY(-3px); } }
@keyframes twinkle { 0%,100% { opacity:.25; } 50% { opacity:1; } }
@keyframes fall { 0% { transform:translateY(-20px); opacity:0; } 15% { opacity:1; } 100% { transform:translateY(230px); opacity:0; } }
@keyframes rise { 0% { transform:translateY(8px); opacity:0; } 40% { opacity:.9; } 100% { transform:translateY(-26px); opacity:0; } }
@keyframes float { 0%,100% { transform:translateY(0); } 50% { transform:translateY(-6px); } }
@keyframes glare { 0% { transform:translateX(-30px); } 60%,100% { transform:translateX(40px); } }
.svg-anim * { transform-box:fill-box; }
.tw { animation: twinkle 3s infinite; }
.sun-rays { transform-origin:center; animation: spin 18s linear infinite; }
.flake { animation: fall 6s linear infinite; }
.wave { animation: rise 2.6s ease-in-out infinite; }
.floaty { animation: float 4s ease-in-out infinite; }
.bld-anim #bld-body { transform-origin:50% 100%; transition: transform .9s cubic-bezier(.34,1.56,.64,1); }
.bld-anim #floor2 { transform-origin:50% 100%; transition: transform .9s cubic-bezier(.34,1.56,.64,1), opacity .6s; }
.bld-anim #roof { transition: transform .9s cubic-bezier(.34,1.56,.64,1); }
.bld-anim .win { transform-origin:50% 50%; transition: transform .7s cubic-bezier(.34,1.56,.64,1), fill .5s; }
.bld-anim #plan-rot { transform-origin:50% 50%; transition: transform 1s cubic-bezier(.34,1.56,.64,1); }
.bld-anim .side { transition: stroke .5s, stroke-width .5s; }
.bld-anim #sun-g, .bld-anim #snow-g, .bld-anim #heat-g { transition: opacity .8s; }
.bld-anim .th-fill { transform-origin:50% 100%; transition: transform 1s cubic-bezier(.34,1.3,.64,1); }
.side-house .fire-ic { color:var(--orange); animation: flicker 1.8s infinite; display:none; font-size:22px; }
.side-house .snow-ic { color:#7fe3df; animation: spin 8s linear infinite; display:none; font-size:22px; }
body.tgt-Y1 .side-house .fire-ic { display:inline-block; } body.tgt-Y2 .side-house .snow-ic { display:inline-block; }

/* Overlay pemuatan */
#loading-overlay { position:fixed; inset:0; z-index:5000; background:linear-gradient(135deg, #0B2447, #19376D 60%, #0F9D9A);
  display:flex; flex-direction:column; align-items:center; justify-content:center; color:#fff; transition:opacity .6s; }
#loading-overlay.hide { opacity:0; pointer-events:none; }
#loading-overlay h2 { font-weight:700; margin:18px 0 6px 0; }
#loading-overlay .lo-sub { color:#dbe4f3; font-size:14px; }
.lo-bar { width:320px; max-width:80vw; height:8px; background:rgba(255,255,255,.18); border-radius:8px; margin-top:16px; overflow:hidden; }
.lo-bar > div { height:100%; width:3%; background:var(--orange); border-radius:8px; transition:width .4s; }
.shiny-notification { z-index:6000; }

/* Footer */
.app-footer { position:fixed; bottom:0; left:0; right:0; height:36px; z-index:1000; background:linear-gradient(90deg, var(--navy), var(--blue));
              color:#dbe4f3; font-size:12px; line-height:36px; text-align:center; }
.app-footer b { color:var(--orange); }
"

js <- "
function fmtID(v, d){ return v.toFixed(d).replace('.', ','); }
function countTo(el, to, d){
  var from = parseFloat(el.getAttribute('data-v') || '0'); var t0 = null; var dur = 900;
  el.setAttribute('data-v', to);
  function step(ts){ if(!t0) t0 = ts; var p = Math.min((ts - t0)/dur, 1); var e = 1 - Math.pow(1 - p, 3);
    el.textContent = fmtID(from + (to - from) * e, d); if(p < 1) requestAnimationFrame(step); }
  requestAnimationFrame(step);
}
$(document).on('shiny:connected', function(){
  Shiny.addCustomMessageHandler('countup', function(m){
    for (var id in m.v) { var el = document.getElementById(id); if (el) countTo(el, m.v[id], m.d[id] === undefined ? 2 : m.d[id]); }
  });
  Shiny.addCustomMessageHandler('target', function(t){
    document.body.classList.remove('tgt-Y1', 'tgt-Y2'); document.body.classList.add('tgt-' + t);
  });
  Shiny.addCustomMessageHandler('prog', function(m){
    var b = document.getElementById('lo-fill'); if (b) b.style.width = Math.round(m.v * 100) + '%';
    var s = document.getElementById('lo-detail'); if (s) s.textContent = m.d;
  });
  Shiny.addCustomMessageHandler('ready', function(m){
    var o = document.getElementById('loading-overlay'); if (o) { o.classList.add('hide'); setTimeout(function(){ o.style.display = 'none'; }, 700); }
    setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 400);
  });
  Shiny.addCustomMessageHandler('best', function(m){
    ['Y1','Y2'].forEach(function(t){ ['ols','las','rf'].forEach(function(k){
      var r = document.getElementById('row_' + t + '_' + k); if (r) r.classList.toggle('best', m[t] === k); }); });
  });
  Shiny.addCustomMessageHandler('bld', function(m){
    var q = function(id){ return document.getElementById(id); };
    if (!q('bld-body')) return;
    q('bld-body').style.transform = 'scaleX(' + m.w + ')';
    q('floor2').style.transform = m.floors == 2 ? 'scaleY(1)' : 'scaleY(0)';
    q('floor2').style.opacity = m.floors == 2 ? 1 : 0;
    q('roof').style.transform = 'translateY(' + (m.floors == 2 ? 0 : 70) + 'px)';
    document.querySelectorAll('.win').forEach(function(el){ el.style.transform = 'scale(' + m.g + ')'; });
    q('sun-g').style.opacity = m.hot; q('snow-g').style.opacity = m.cold; q('heat-g').style.opacity = m.cold;
    q('th1').style.transform = 'scaleY(' + m.t1 + ')'; q('th2').style.transform = 'scaleY(' + m.t2 + ')';
  });
});
"

# ---- Ilustrasi SVG ----------------------------------------------------------
svg_tower <- function(x, w, h, cols, rows, fill, seed, base = 232) {
  set.seed(seed)
  y <- base - h
  cw <- (w - 12) / cols; rh <- min(18, (h - 20) / rows)
  wins <- unlist(lapply(seq_len(rows), function(r) lapply(seq_len(cols), function(cc) {
    sprintf('<rect class="tw" x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="1.5" fill="#FFE8A3" style="animation-delay:%.1fs"/>',
            x + 6 + (cc - 1) * cw + cw * 0.18, y + 12 + (r - 1) * rh, cw * 0.64, rh * 0.55, runif(1, 0, 3))
  })))
  paste0(sprintf('<rect x="%d" y="%d" width="%d" height="%d" rx="3" fill="%s"/>', x, y, w, h, fill),
         paste(wins, collapse = ""))
}

hero_svg <- function() {
  flakes <- paste(vapply(1:9, function(i) sprintf(
    '<text class="flake" x="%d" y="10" font-size="%d" fill="#E0F7FA" style="animation-delay:%.1fs;animation-duration:%.1fs">&#10052;</text>',
    c(18, 52, 86, 34, 120, 70, 150, 100, 8)[i], c(12, 9, 14, 10, 9, 13, 10, 8, 11)[i], i * 0.65, 5 + (i %% 3)), character(1)), collapse = "")
  waves <- paste(vapply(1:3, function(i) sprintf(
    '<path class="wave" d="M%d 118 q6 -8 0 -16 q-6 -8 0 -16" stroke="#F39C12" stroke-width="3" fill="none" stroke-linecap="round" style="animation-delay:%.1fs"/>',
    c(214, 232, 250)[i], i * 0.7), character(1)), collapse = "")
  HTML(paste0(
    '<svg class="svg-anim" viewBox="0 0 460 250" width="100%" role="img" aria-label="Ilustrasi bangunan dan energi">',
    '<defs><linearGradient id="hsky" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#23497f"/><stop offset="1" stop-color="#0F9D9A"/></linearGradient></defs>',
    '<rect width="460" height="250" rx="16" fill="url(#hsky)"/>',
    '<g transform="translate(392 52)"><g class="sun-rays">',
    paste(vapply(0:11, function(i) sprintf('<line x1="0" y1="-30" x2="0" y2="-40" stroke="#F39C12" stroke-width="3" stroke-linecap="round" transform="rotate(%d)"/>', i * 30), character(1)), collapse = ""),
    '</g><circle r="22" fill="#F39C12"/><circle r="16" fill="#FFC857"/></g>',
    '<g>', flakes, '</g>',
    svg_tower(20, 70, 120, 3, 5, "#0B2447", 1),
    svg_tower(98, 56, 170, 2, 8, "#162f5c", 2),
    svg_tower(162, 104, 110, 4, 4, "#0B2447", 3),
    waves,
    svg_tower(276, 60, 150, 2, 7, "#162f5c", 4),
    svg_tower(344, 92, 96, 4, 4, "#0B2447", 5),
    '<g class="floaty"><rect x="162" y="104" width="104" height="18" rx="4" fill="#F39C12"/>',
    '<text x="214" y="117" font-size="10" font-family="Poppins" fill="#fff" text-anchor="middle" font-weight="600">Y1 · Y2</text></g>',
    '<rect x="0" y="232" width="460" height="18" fill="#0B2447"/>',
    '<rect x="0" y="232" width="460" height="3" fill="#0F9D9A" opacity=".7"/>',
    '</svg>'))
}

# Bangunan parametrik (Tab 3) - diperbarui lewat JavaScript agar transisinya halus
building_svg <- function() {
  win <- function(x, y) sprintf('<rect class="win" x="%d" y="%d" width="30" height="34" rx="3" fill="#A5D8FF" stroke="#fff" stroke-width="2"/>', x, y)
  f1 <- paste(vapply(c(150, 205, 290, 345), function(x) win(x, 222), character(1)), collapse = "")
  f2 <- paste(vapply(c(150, 205, 290, 345), function(x) win(x, 152), character(1)), collapse = "")
  flakes <- paste(vapply(1:8, function(i) sprintf(
    '<text class="flake" x="%d" y="30" font-size="%d" fill="#ffffff" style="animation-delay:%.1fs">&#10052;</text>',
    c(30, 70, 110, 440, 480, 90, 460, 20)[i], c(14, 10, 12, 13, 10, 9, 11, 12)[i], i * 0.7), character(1)), collapse = "")
  waves <- paste(vapply(1:4, function(i) sprintf(
    '<path class="wave" d="M%d 118 q7 -9 0 -18 q-7 -9 0 -18" stroke="#F39C12" stroke-width="3.5" fill="none" stroke-linecap="round" style="animation-delay:%.1fs"/>',
    c(215, 245, 275, 305)[i], i * 0.55), character(1)), collapse = "")
  HTML(paste0(
    '<svg class="svg-anim bld-anim" viewBox="0 0 540 330" width="100%" role="img" aria-label="Bangunan hasil rancangan">',
    '<defs><linearGradient id="bsky" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#1b3f73"/><stop offset="1" stop-color="#8fd3e8"/></linearGradient>',
    '<linearGradient id="wall" x1="0" y1="0" x2="1" y2="0"><stop offset="0" stop-color="#F4F1EA"/><stop offset="1" stop-color="#E3DED2"/></linearGradient></defs>',
    '<rect width="540" height="330" rx="16" fill="url(#bsky)"/>',
    '<g id="sun-g" style="opacity:.6"><g transform="translate(468 58)"><g class="sun-rays">',
    paste(vapply(0:11, function(i) sprintf('<line x1="0" y1="-30" x2="0" y2="-42" stroke="#FFC857" stroke-width="3" stroke-linecap="round" transform="rotate(%d)"/>', i * 30), character(1)), collapse = ""),
    '</g><circle r="22" fill="#F39C12"/><circle r="15" fill="#FFD166"/></g></g>',
    '<g id="snow-g" style="opacity:.6">', flakes, '</g>',
    '<rect x="0" y="292" width="540" height="38" fill="#2f6b3a"/><rect x="0" y="292" width="540" height="5" fill="#4c9a58"/>',
    '<g id="bld-body">',
    '<rect x="130" y="210" width="280" height="82" fill="url(#wall)" stroke="#0B2447" stroke-width="2"/>', f1,
    '<rect x="252" y="246" width="36" height="46" rx="3" fill="#19376D"/>',
    '<g id="floor2"><rect x="130" y="140" width="280" height="70" fill="url(#wall)" stroke="#0B2447" stroke-width="2"/>', f2, '</g>',
    '<g id="roof"><polygon points="118,142 270,92 422,142" fill="#19376D"/><rect x="118" y="138" width="304" height="8" fill="#0B2447"/>',
    '<g id="heat-g" style="opacity:.6">', waves, '</g></g>',
    '</g>',
    '<g transform="translate(34 120)"><rect x="0" y="0" width="16" height="150" rx="8" fill="#ffffff" opacity=".85"/>',
    '<rect id="th1" class="th-fill" x="3" y="3" width="10" height="144" rx="5" fill="#F39C12" style="transform:scaleY(.5)"/>',
    '<circle cx="8" cy="156" r="12" fill="#F39C12"/><text x="8" y="182" font-size="12" fill="#fff" text-anchor="middle" font-family="Poppins" font-weight="600">Y1</text></g>',
    '<g transform="translate(70 120)"><rect x="0" y="0" width="16" height="150" rx="8" fill="#ffffff" opacity=".85"/>',
    '<rect id="th2" class="th-fill" x="3" y="3" width="10" height="144" rx="5" fill="#0F9D9A" style="transform:scaleY(.5)"/>',
    '<circle cx="8" cy="156" r="12" fill="#0F9D9A"/><text x="8" y="182" font-size="12" fill="#fff" text-anchor="middle" font-family="Poppins" font-weight="600">Y2</text></g>',
    '</svg>'))
}

plan_svg <- function() {
  HTML(paste0(
    '<svg class="svg-anim bld-anim" viewBox="0 0 200 200" width="100%" style="max-width:210px" role="img" aria-label="Denah dan orientasi">',
    '<circle cx="100" cy="100" r="92" fill="#F3F7FD" stroke="#dbe4f3"/>',
    '<text x="100" y="20" text-anchor="middle" font-size="13" font-weight="700" fill="#0B2447" font-family="Poppins">U</text>',
    '<text x="186" y="104" text-anchor="middle" font-size="11" fill="#6b7280" font-family="Poppins">T</text>',
    '<text x="100" y="192" text-anchor="middle" font-size="11" fill="#6b7280" font-family="Poppins">S</text>',
    '<text x="14" y="104" text-anchor="middle" font-size="11" fill="#6b7280" font-family="Poppins">B</text>',
    '<g id="plan-rot"><rect x="55" y="55" width="90" height="90" fill="#fff"/>',
    '<line id="sideN" class="side" x1="55" y1="55" x2="145" y2="55" stroke="#cbd5e1" stroke-width="3"/>',
    '<line id="sideE" class="side" x1="145" y1="55" x2="145" y2="145" stroke="#cbd5e1" stroke-width="3"/>',
    '<line id="sideS" class="side" x1="55" y1="145" x2="145" y2="145" stroke="#cbd5e1" stroke-width="3"/>',
    '<line id="sideW" class="side" x1="55" y1="55" x2="55" y2="145" stroke="#cbd5e1" stroke-width="3"/>',
    '<polygon points="100,70 110,95 90,95" fill="#F39C12"/></g>',
    '</svg>'))
}

mini_art <- function(kind) {
  if (kind == "model") {
    HTML(paste0('<svg class="svg-anim" viewBox="0 0 300 170" width="100%" role="img" aria-label="Ilustrasi model">',
      '<rect width="300" height="170" rx="14" fill="#10305f"/>',
      paste(vapply(0:9, function(i) sprintf('<line x1="%d" y1="0" x2="%d" y2="170" stroke="#1d4580" stroke-width="1"/>', i * 30, i * 30), character(1)), collapse = ""),
      paste(vapply(0:5, function(i) sprintf('<line x1="0" y1="%d" x2="300" y2="%d" stroke="#1d4580" stroke-width="1"/>', i * 30, i * 30), character(1)), collapse = ""),
      '<g class="floaty"><rect x="40" y="70" width="110" height="70" fill="none" stroke="#A5D8FF" stroke-width="2.5"/>',
      '<polygon points="30,72 95,35 160,72" fill="none" stroke="#A5D8FF" stroke-width="2.5"/>',
      '<rect x="58" y="88" width="20" height="20" fill="none" stroke="#FFC857" stroke-width="2"/><rect x="112" y="88" width="20" height="20" fill="none" stroke="#FFC857" stroke-width="2"/>',
      '<line x1="40" y1="152" x2="150" y2="152" stroke="#F39C12" stroke-width="2"/><text x="95" y="165" fill="#F39C12" font-size="10" text-anchor="middle" font-family="Poppins">X2 = X3 + 2·X4</text></g>',
      '<g transform="translate(185 30)">',
      paste(vapply(1:6, function(i) sprintf('<rect x="%d" y="%d" width="14" height="%d" rx="3" fill="%s" class="tw" style="animation-delay:%.1fs"/>',
                                            (i - 1) * 18, 110 - c(95, 70, 0, 50, 0, 30)[i], max(c(95, 70, 0, 50, 0, 30)[i], 3),
                                            if (c(95, 70, 0, 50, 0, 30)[i] == 0) "#475569" else "#0F9D9A", i * 0.4), character(1)), collapse = ""),
      '<line x1="-4" y1="110" x2="108" y2="110" stroke="#dbe4f3"/><text x="52" y="128" fill="#dbe4f3" font-size="10" text-anchor="middle" font-family="Poppins">koefisien Lasso</text></g>',
      '</svg>'))
  } else {
    HTML(paste0('<svg class="svg-anim" viewBox="0 0 300 170" width="100%" role="img" aria-label="Ilustrasi prediksi">',
      '<rect width="300" height="170" rx="14" fill="#10305f"/>',
      '<g transform="translate(250 38)"><g class="sun-rays">',
      paste(vapply(0:7, function(i) sprintf('<line x1="0" y1="-18" x2="0" y2="-26" stroke="#FFC857" stroke-width="3" stroke-linecap="round" transform="rotate(%d)"/>', i * 45), character(1)), collapse = ""),
      '</g><circle r="14" fill="#F39C12"/></g>',
      svg_tower(30, 60, 100, 2, 5, "#1d4580", 7, base = 150),
      svg_tower(100, 80, 70, 3, 3, "#19376D", 8, base = 150),
      '<g class="floaty"><polyline points="190,140 212,118 232,126 256,96 280,80" fill="none" stroke="#2E9E5B" stroke-width="3"/>',
      '<polyline points="190,140 212,124 232,120 256,104 280,92" fill="none" stroke="#A5D8FF" stroke-width="3" stroke-dasharray="5 4"/></g>',
      '<rect x="0" y="150" width="300" height="20" rx="0" fill="#0B2447"/>',
      '</svg>'))
  }
}

# ---- Komponen UI -------------------------------------------------------------
sec_head <- function(num, ic, title, sub = NULL) {
  div(class = "sec-head", div(class = "sec-num", num),
      div(h3(icon(ic), title), if (!is.null(sub)) div(class = "sub", sub)))
}
howto <- function(...) {
  div(class = "howto", div(class = "ht-title", icon("hand-pointer"), " Coba fitur interaktif ini"),
      tags$ul(lapply(list(...), function(x) tags$li(HTML(x)))))
}
tbox <- function(title, ic, ..., width = 12, collapsible = FALSE, collapsed = FALSE) {
  box(width = width, title = tagList(icon(ic), " ", title), class = "box-tc",
      collapsible = collapsible, collapsed = collapsed, ...)
}
expl_box <- function(id, width = 12) {
  box(width = width, class = "box-explain", title = tagList(icon("lightbulb"), " Penjelasan"), uiOutput(id))
}
kpi <- function(id, label, ic, bg, value = "0") {
  div(class = "kpi", div(class = "ic", style = paste0("background:", bg), icon(ic)),
      div(div(class = "v", id = id, value), div(class = "l", label)))
}
ly <- function(id, h = 380) plotlyOutput(id, height = paste0(h, "px"))

# =============================================================================
# 7. TAMPILAN (UI) - 3 TAB
# =============================================================================
ui <- dashboardPage(
  skin = "blue",
  title = "EnergiLasso - Energy Efficiency Dashboard",

  dashboardHeader(
    title = tagList(span(class = "logo-bolt", icon("bolt")), " EnergiLasso"),
    titleWidth = 260,
    tags$li(class = "dropdown", tags$span(class = "header-title",
                                          "Lasso Regression · Heating & Cooling Load · Random Forest Benchmark"))
  ),

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "menu",
      menuItem("Data & EDA",                   tabName = "t_eda",   icon = icon("magnifying-glass-chart")),
      menuItem("Model + Diagnostik",           tabName = "t_model", icon = icon("sliders")),
      menuItem("Prediksi Interaktif + Benchmark", tabName = "t_pred", icon = icon("wand-magic-sparkles"))
    ),
    div(class = "side-block",
        h5(icon("bullseye"), " Target yang dianalisis"),
        div(class = "side-house", span(class = "fire-ic", icon("fire")), span(class = "snow-ic", icon("snowflake"))),
        div(class = "tgt-toggle",
            radioButtons("target", NULL, inline = FALSE,
                         choiceNames = list("Heating (Y1)", "Cooling (Y2)"),
                         choiceValues = list("Y1", "Y2"), selected = "Y1")),
        div(style = "margin-top:8px; font-size:11.5px; color:#b8c6e0;",
            "Pilihan ini berlaku untuk semua grafik di ketiga tab.")),
    div(class = "side-block",
        h5(icon("compass"), " Cara pakai"),
        "Pilih target di atas, lalu buka tab 1 → 2 → 3. Kontrol yang bisa digeser ditandai kotak bergaris putus-putus.")
  ),

  dashboardBody(
    tags$head(tags$style(HTML(css)), tags$script(HTML(js))),
    tags$script(HTML("document.body.classList.add('tgt-Y1');")),

    div(id = "loading-overlay",
        div(style = "width:300px; max-width:80vw;", class = "floaty", hero_svg()),
        h2("Menyiapkan analisis…"),
        div(class = "lo-sub", id = "lo-detail", "Memuat data ENB2012"),
        div(class = "lo-bar", div(id = "lo-fill")),
        div(class = "lo-sub", style = "margin-top:14px; font-size:12px; opacity:.8;",
            "Pembukaan pertama menjalankan repeated CV (± 5 menit). Setelah itu hasil disimpan dan langsung tampil.")),

    tabItems(
      # ===================================================================
      # TAB 1 - DATA & EDA
      # ===================================================================
      tabItem("t_eda",
        div(class = "hero",
            div(class = "h-text",
                div(class = "kicker", "Energy Efficiency · ENB2012"),
                h1("Seleksi Variabel Desain Bangunan dengan Regresi Lasso"),
                p("Setiap baris data adalah satu konfigurasi bangunan hasil simulasi Ecotect. ",
                  "Pertanyaannya: variabel desain mana (X1–X8) yang tetap dipilih Lasso saat memprediksi ",
                  tags$b("Heating Load (Y1)"), " dan ", tags$b("Cooling Load (Y2)"), " secara terpisah?"),
                div(span(class = "chip", style = paste0("background:", ACCENT), icon("fire"), " Heating Load"),
                    span(class = "chip", style = paste0("background:", TEAL), icon("snowflake"), " Cooling Load"),
                    span(class = "chip", style = "background:#2E9E5B", icon("tree"), " Random Forest"),
                    span(class = "chip", style = "background:rgba(255,255,255,.18)", icon("database"), " 768 bangunan")),
                div(class = "steps",
                    div(class = "step", tags$b("1"), "Kenali data"),
                    div(class = "step", tags$b("2"), "Pahami model"),
                    div(class = "step", tags$b("3"), "Rancang & bandingkan"))),
            div(class = "h-art floaty", hero_svg())),

        div(class = "kpi-row",
            kpi("k_n", "Konfigurasi bangunan", "building", SECONDARY),
            kpi("k_p", "Variabel desain (X1–X8)", "ruler-combined", ACCENT),
            kpi("k_shape", "Bentuk bangunan unik", "shapes", TEAL),
            kpi("k_miss", "Nilai kosong", "circle-check", "#2E9E5B"),
            kpi("k_mean", "Rata-rata target (kWh/m²)", "gauge-high", PRIMARY)),

        # ---- 1. Distribusi & filter ----
        sec_head("1", "filter", "Saring bangunan & lihat distribusi target",
                 "Gunakan filter untuk membandingkan kelompok bangunan tertentu dengan seluruh data"),
        fluidRow(
          tbox("Filter desain bangunan", "sliders", width = 4,
               howto("Atur filter di bawah; grafik di kanan ikut berubah."),
               sliderInput("f_x1", "X1 · Relative Compactness", min = 0.62, max = 0.98, value = c(0.62, 0.98), step = 0.01),
               checkboxGroupInput("f_x5", "X5 · Overall Height", choices = c("3,5 m (1 lantai)" = 3.5, "7 m (2 lantai)" = 7),
                                  selected = c(3.5, 7), inline = TRUE),
               sliderInput("f_x7", "X7 · Glazing Area (rasio kaca)", min = 0, max = 0.4, value = c(0, 0.4), step = 0.05),
               checkboxGroupInput("f_x6", "X6 · Orientation", choices = setNames(names(X6_LABEL), X6_LABEL),
                                  selected = names(X6_LABEL), inline = TRUE),
               checkboxGroupInput("f_x8", "X8 · Glazing Distribution", choices = setNames(names(X8_LABEL), X8_LABEL),
                                  selected = names(X8_LABEL)),
               actionButton("f_reset", tagList(icon("rotate-left"), "Reset filter"), class = "btn-soft")),
          column(8,
            fluidRow(
              tbox("Distribusi target", "chart-column", width = 12,
                   fluidRow(column(6, sliderInput("bins", "Jumlah bin histogram", 10, 60, 30, step = 2, width = "100%")),
                            column(6, div(style = "padding-top:26px;", uiOutput("f_summary")))),
                   ly("p_hist", 300), ly("p_box", 150))),
            fluidRow(expl_box("ex_dist")))
        ),

        # ---- 2. Hubungan prediktor ----
        sec_head("2", "chart-line", "Hubungan prediktor dengan target",
                 "Pilih variabel dan atur kelenturan kurva LOWESS untuk melihat pola non-linear"),
        fluidRow(
          tbox("Pengaturan", "gear", width = 4,
               howto("Pilih variabel, lalu geser <b>span</b> atau tekan <kbd>▶</kbd>."),
               selectInput("rel_x", "Variabel X", choices = setNames(FEATURES, paste(FEATURES, "·", VAR_DESC[FEATURES])),
                           selected = "X7"),
               sliderInput("span", "Span LOWESS (f)", min = 0.1, max = 1, value = 0.67, step = 0.03,
                           animate = animationOptions(interval = 450, loop = FALSE)),
               radioButtons("rel_col", "Warna titik", choices = c("Satu warna" = "none", "Tinggi (X5)" = "X5",
                                                                  "Distribusi kaca (X8)" = "X8"), inline = TRUE),
               checkboxInput("rel_jit", "Jitter titik (pisahkan titik yang bertumpuk)", TRUE)),
          tbox("Scatter + LOWESS / Boxplot kategori", "chart-line", width = 8, ly("p_rel", 400))
        ),
        fluidRow(expl_box("ex_rel")),

        # ---- 3. Korelasi ----
        sec_head("3", "table-cells", "Korelasi rank Spearman",
                 "Temukan pasangan variabel yang bergerak bersama (sumber multikolinearitas)"),
        fluidRow(
          tbox("Heatmap korelasi", "table-cells", width = 7,
               howto("Geser <b>ambang |ρ|</b> untuk menyorot korelasi yang kuat."),
               sliderInput("rho_th", "Ambang |ρ| yang disorot", 0, 1, 0, step = 0.05,
                           animate = animationOptions(interval = 500, loop = FALSE)),
               ly("p_heat", 470)),
          column(5, expl_box("ex_corr", width = 12),
                 tbox("Kualitas data", "clipboard-check", width = 12,
                      uiOutput("dep_txt"), DTOutput("tbl_quality")))
        ),
        fluidRow(tbox("Tabel data (hasil filter di bagian 1)", "table", width = 12, collapsible = TRUE, collapsed = TRUE,
                      DTOutput("tbl_data")))
      ),

      # ===================================================================
      # TAB 2 - MODEL + DIAGNOSTIK
      # ===================================================================
      tabItem("t_model",
        div(class = "hero",
            div(class = "h-text",
                div(class = "kicker", "Langkah 4 – 6"),
                h1("Bagaimana Lasso memilih variabel?"),
                p("Lasso memberi penalti λ pada koefisien. Makin besar λ, makin banyak koefisien jadi nol (variabel dibuang).")),
            div(class = "h-art", style = "flex-basis:300px;", mini_art("model"))),

        sec_head("A", "sliders", "Pengaruh λ terhadap koefisien Lasso"),
        fluidRow(
          tbox("Koefisien pada λ terpilih", "chart-bar", width = 7,
               howto("Geser slider atau tekan <kbd>▶</kbd>. Tombol <b>λ.min</b> kembali ke λ hasil resmi."),
               fluidRow(column(8, sliderInput("loglam", "log₁₀(λ)", min = -4, max = 1, value = -2, step = 0.05, width = "100%",
                                              animate = animationOptions(interval = 220, loop = FALSE))),
                        column(4, div(style = "padding-top:26px;",
                                      actionButton("go_min", tagList(icon("bullseye"), "λ.min"), class = "btn-soft")))),
               ly("p_coefbar", 400)),
          column(5,
                 div(class = "kpi-row", style = "margin-top:0;",
                     kpi("k_nz", "Fitur aktif (dari 14)", "filter", ACCENT),
                     kpi("k_te", "RMSE test set", "vial", PRIMARY)),
                 tbox("Kesalahan 5-fold CV", "chart-area", width = 12, ly("p_cvcurve", 250)),
                 expl_box("ex_lambda", width = 12))
        ),

        fluidRow(tbox("Persamaan model Lasso (mengikuti slider λ)", "square-root-variable", width = 12, uiOutput("eq_lasso"))),

        sec_head("B", "scale-balanced", "Stabilitas seleksi (50 fold)"),
        fluidRow(
          tbox("Frekuensi terpilih Lasso", "scale-balanced", width = 7, ly("p_stab", 300)),
          expl_box("ex_stab", width = 5)
        ),

        sec_head("C", "stethoscope", "Diagnostik model (OLS)"),
        fluidRow(
          tbox("VIF (multikolinearitas)", "layer-group", width = 4, ly("p_vif", 330)),
          tbox("Residual vs Fitted", "wave-square", width = 4, ly("p_resid", 330)),
          tbox("Q-Q plot", "chart-line", width = 4, ly("p_qq", 330))
        ),
        fluidRow(
          tbox("Cook's Distance (batas 4/n)", "circle-exclamation", width = 7, ly("p_cook", 280)),
          expl_box("ex_diag", width = 5)
        )
      ),

      # ===================================================================
      # TAB 3 - PREDIKSI INTERAKTIF + BENCHMARK
      # ===================================================================
      tabItem("t_pred",
        div(class = "hero",
            div(class = "h-text",
                div(class = "kicker", "Langkah 7"),
                h1("Rancang bangunan, lalu bandingkan model"),
                p("Atur desain bangunan dan lihat prediksi OLS, Lasso, dan Random Forest. Di bawahnya, perbandingan akurasi ketiga model.")),
            div(class = "h-art", style = "flex-basis:300px;", mini_art("pred"))),

        sec_head("1", "compass-drafting", "Perancang bangunan"),
        fluidRow(
          tbox("Rancangan", "ruler-combined", width = 3,
               howto("Geser kontrol di bawah; rumah & prediksi ikut berubah. <b>Acak</b> = ambil bangunan nyata dari data."),
               sliderInput("d_shape", "Bentuk bangunan · mengatur X1–X5 sekaligus", min = 1, max = 12, value = 6, step = 1),
               uiOutput("shape_info"),
               radioButtons("d_x6", "Orientasi (X6)", choices = setNames(names(X6_LABEL), X6_LABEL), selected = "4", inline = TRUE),
               sliderInput("d_x7", "Luas kaca (X7)", min = 0, max = 0.4, value = 0.25, step = 0.05,
                           animate = animationOptions(interval = 700, loop = FALSE)),
               selectInput("d_x8", "Distribusi kaca (X8)", choices = setNames(names(X8_LABEL), X8_LABEL), selected = "1"),
               actionButton("d_rand", tagList(icon("shuffle"), "Acak"), class = "btn-soft"),
               actionButton("d_reset", tagList(icon("rotate-left"), "Reset"), class = "btn-soft")),
          tbox("Visual bangunan", "building", width = 5, building_svg(),
               div(style = "font-size:12px; color:#6b7280; margin-top:6px;",
                   "Lantai & lebar mengikuti bentuk, jendela mengikuti luas kaca, termometer mengikuti prediksi RF.")),
          column(4,
                 div(class = "pred-card Y1", span(class = "bg-ic", icon("fire")),
                     h4(icon("fire"), " Heating Load (Y1)"),
                     div(class = "pred-row", id = "row_Y1_ols", span(class = "pm-name", "OLS"), span(span(class = "pm-val", id = "pv_Y1_ols", "0"), span(class = "unit", " kWh/m²"))),
                     div(class = "pred-row", id = "row_Y1_las", span(class = "pm-name", "Lasso"), span(span(class = "pm-val", id = "pv_Y1_las", "0"), span(class = "unit", " kWh/m²"))),
                     div(class = "pred-row", id = "row_Y1_rf", span(class = "pm-name", "Random Forest"), span(span(class = "pm-val", id = "pv_Y1_rf", "0"), span(class = "unit", " kWh/m²")))),
                 div(class = "pred-card Y2", span(class = "bg-ic", icon("snowflake")),
                     h4(icon("snowflake"), " Cooling Load (Y2)"),
                     div(class = "pred-row", id = "row_Y2_ols", span(class = "pm-name", "OLS"), span(span(class = "pm-val", id = "pv_Y2_ols", "0"), span(class = "unit", " kWh/m²"))),
                     div(class = "pred-row", id = "row_Y2_las", span(class = "pm-name", "Lasso"), span(span(class = "pm-val", id = "pv_Y2_las", "0"), span(class = "unit", " kWh/m²"))),
                     div(class = "pred-row", id = "row_Y2_rf", span(class = "pm-name", "Random Forest"), span(span(class = "pm-val", id = "pv_Y2_rf", "0"), span(class = "unit", " kWh/m²")))),
                 uiOutput("actual_box"))
        ),

        sec_head("2", "trophy", "Benchmark: OLS vs Lasso vs Random Forest"),
        fluidRow(tbox("Ringkasan performa", "table-list", width = 12, DTOutput("tbl_comp"))),
        fluidRow(
          tbox("RMSE di 50 fold (repeated CV)", "box-archive", width = 6, ly("p_cvbox", 330)),
          tbox("Selisih RMSE per fold: Lasso − RF", "code-compare", width = 6, ly("p_paired", 330))
        ),
        fluidRow(
          tbox("Importance RF vs seleksi Lasso", "ranking-star", width = 7, ly("p_imp", 320)),
          column(5, expl_box("ex_bench", width = 12), expl_box("ex_imp", width = 12))
        ),

        sec_head("3", "flag-checkered", "Kesimpulan"),
        fluidRow(
          box(width = 12, class = "box-explain", title = tagList(icon("lightbulb"), " Jawaban utama"),
              uiOutput("ex_final"),
              DTOutput("tbl_final"),
              div(style = "font-size:12px; color:#6b7280; margin:10px 0;",
                  "Lasso dipakai untuk interpretasi (model ringkas), RF sebagai pembanding akurasi. Koefisien nonzero bukan p-value."),
              downloadButton("dl_final", "final_answer.csv", class = "btn-soft"),
              downloadButton("dl_comp", "model_comparison.csv", class = "btn-soft"))
        )
      )
    ),

    div(class = "app-footer",
        icon("bolt"), " EnergiLasso · Lasso Regression untuk ", tags$b("Heating"), " & ",
        tags$b("Cooling"), " Load · Random Forest Benchmark · ENB2012")
  )
)

# =============================================================================
# 8. SERVER
# =============================================================================
dt_table <- function(d, page = 10, digits = 4, filter = "none") {
  d <- as.data.frame(d, check.names = FALSE)
  lgl_cols <- names(d)[vapply(d, is.logical, logical(1))]
  for (cc in lgl_cols) d[[cc]] <- ifelse(d[[cc]], '<span class="pill pill-yes">Ya</span>',
                                         '<span class="pill pill-no">Tidak</span>')
  num_cols <- names(d)[vapply(d, is.numeric, logical(1))]
  inf_cols <- num_cols[vapply(d[num_cols], function(x) any(is.infinite(x)), logical(1))]
  for (cc in inf_cols) d[[cc]] <- ifelse(is.infinite(d[[cc]]), "Inf", formatC(d[[cc]], format = "g", digits = 6))
  num_cols   <- setdiff(num_cols, inf_cols)
  int_cols   <- num_cols[vapply(d[num_cols], function(x) all(x == round(x)), logical(1))]
  small_cols <- num_cols[vapply(d[num_cols], function(x) any(x != 0 & abs(x) < 1e-3), logical(1))]
  round_cols <- setdiff(num_cols, c(int_cols, small_cols))
  bar_cols   <- num_cols[grepl("Frequency|Importance_Pct|Percent", num_cols)]

  out <- datatable(
    d, rownames = FALSE, class = "compact stripe hover", filter = filter,
    escape = which(!names(d) %in% lgl_cols),
    options = list(pageLength = page, scrollX = TRUE, autoWidth = FALSE,
                   dom = if (nrow(d) <= page) "t" else "ftip",
                   columnDefs = list(list(className = "dt-center", targets = which(names(d) %in% lgl_cols) - 1)),
                   language = list(search = "Cari:", info = "_START_–_END_ dari _TOTAL_",
                                   zeroRecords = "Tidak ada data",
                                   paginate = list(previous = "‹", `next` = "›")))
  )
  if (length(round_cols)) out <- formatRound(out, round_cols, digits)
  if (length(small_cols)) out <- formatSignif(out, small_cols, digits)
  for (cc in bar_cols) {
    out <- formatStyle(out, cc, background = styleColorBar(c(0, 100), "#CDEFEA"),
                       backgroundSize = "96% 70%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
  }
  out
}

# Tata letak dasar plotly (seragam di semua grafik)
pl <- function(p, xlab = NULL, ylab = NULL, legend = TRUE) {
  p %>%
    layout(
      font = list(family = "Poppins, Segoe UI, Arial", size = 12, color = "#374151"),
      xaxis = list(title = xlab, gridcolor = "#EEF1F5", zeroline = FALSE),
      yaxis = list(title = ylab, gridcolor = "#EEF1F5", zeroline = FALSE),
      plot_bgcolor = "#FFFFFF", paper_bgcolor = "rgba(0,0,0,0)", showlegend = legend,
      legend = list(orientation = "h", x = 0, y = 1.12),
      margin = list(l = 60, r = 20, t = 30, b = 50),
      hoverlabel = list(bgcolor = "white", font = list(size = 12))
    ) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "hoverCompareCartesian", "toggleSpikelines"),
           toImageButtonOptions = list(format = "png", scale = 2))
}
vline <- function(x, col, dash = "solid", width = 2.5) {
  list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper", line = list(color = col, dash = dash, width = width))
}
hline <- function(y, col = "#DC2626", dash = "dash") {
  list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y, line = list(color = col, dash = dash, width = 2))
}
empty_plot <- function(msg) {
  plot_ly() %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                       annotations = list(list(text = msg, showarrow = FALSE, font = list(size = 14, color = "#6b7280")))) %>%
    config(displayModeBar = FALSE)
}
jitter_x <- function(x, amount) { set.seed(7); x + runif(length(x), -amount, amount) }

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Memuat data + analysis (otomatis, tanpa unggah; memakai cache jika ada)
  # ---------------------------------------------------------------------------
  RV <- reactiveVal(NULL)
  observeEvent(TRUE, once = TRUE, {
    prog <- function(v, d) session$sendCustomMessage("prog", list(v = v, d = d))
    prog(0.02, "Mencari file data ENB2012")
    f <- locate_data()
    if (is.null(f)) {
      prog(0, "File data tidak ditemukan")
      showModal(modalDialog(title = "Data ENB2012 tidak ditemukan", easyClose = FALSE,
        "Simpan file ENB2012_data.xlsx di folder yang sama dengan app.R, lalu jalankan ulang aplikasi. ",
        "Aplikasi juga sudah mencoba mengunduh otomatis dari UCI, tetapi gagal (periksa koneksi internet)."))
      return()
    }
    rd <- read_energy_data(f)
    if (!rd$ok) {
      showModal(modalDialog(title = "Data tidak valid", rd$msg)); return()
    }
    cp  <- cache_path(f)
    res <- if (file.exists(cp)) tryCatch(readRDS(cp), error = function(e) NULL) else NULL
    if (is.null(res)) {
      res <- withProgress(message = "Menjalankan analisis", value = 0, {
        run_analysis(rd$data, progress = function(v, d) { setProgress(value = v, detail = d); prog(v, d) })
      })
      try(saveRDS(res, cp), silent = TRUE)
      old <- setdiff(list.files(CACHE_DIR, pattern = "^hasil_.*\\.rds$", full.names = TRUE), cp)
      unlink(old)
    }
    prog(1, "Selesai")
    RV(res)
    session$sendCustomMessage("ready", 1)
  })

  R  <- reactive({ req(RV()); RV() })
  tg <- reactive(input$target)
  observeEvent(tg(), session$sendCustomMessage("target", tg()))

  # ===========================================================================
  # TAB 1 - DATA & EDA
  # ===========================================================================
  observe({
    df <- R()$df
    session$sendCustomMessage("countup", list(
      v = list(k_n = nrow(df), k_p = length(FEATURES), k_shape = nrow(distinct(df, X1, X2, X3, X4, X5)),
               k_miss = sum(is.na(df)), k_mean = mean(df[[tg()]])),
      d = list(k_n = 0, k_p = 0, k_shape = 0, k_miss = 0, k_mean = 2)))
  })

  fdf <- reactive({
    R()$df %>% filter(
      X1 >= input$f_x1[1] - 1e-9, X1 <= input$f_x1[2] + 1e-9,
      X5 %in% as.numeric(input$f_x5), X6 %in% as.numeric(input$f_x6),
      X7 >= input$f_x7[1] - 1e-9, X7 <= input$f_x7[2] + 1e-9,
      X8 %in% as.numeric(input$f_x8))
  })

  observeEvent(input$f_reset, {
    updateSliderInput(session, "f_x1", value = c(0.62, 0.98))
    updateSliderInput(session, "f_x7", value = c(0, 0.4))
    updateCheckboxGroupInput(session, "f_x5", selected = c(3.5, 7))
    updateCheckboxGroupInput(session, "f_x6", selected = names(X6_LABEL))
    updateCheckboxGroupInput(session, "f_x8", selected = names(X8_LABEL))
  })

  output$f_summary <- renderUI({
    d <- fdf(); t <- tg()
    if (!nrow(d)) return(span(class = "badge-warn", icon("triangle-exclamation"), " Tidak ada bangunan yang cocok dengan filter."))
    tagList(
      span(class = "badge-ok", icon("building"), sprintf(" %d dari %d bangunan", nrow(d), nrow(R()$df))), " ",
      span(class = "badge-ok", style = "background:var(--tc-soft); color:#1f2937;",
           sprintf("median %s = %s", t, fn(median(d[[t]])))))
  })

  output$p_hist <- renderPlotly({
    t <- tg(); all <- R()$df[[t]]; d <- fdf()[[t]]
    size <- diff(range(all)) / input$bins
    xb <- list(start = min(all), end = max(all) + size, size = size)
    dens <- density(all)
    p <- plot_ly() %>%
      add_histogram(x = all, name = "Seluruh data", xbins = xb, histnorm = "probability density",
                    marker = list(color = "#CBD5E1", line = list(color = "#fff", width = 1))) %>%
      add_lines(x = dens$x, y = dens$y, name = "Kepadatan (semua)", line = list(color = "#64748B", width = 2))
    if (length(d) > 0 && length(d) < length(all)) {
      p <- p %>% add_histogram(x = d, name = "Hasil filter", xbins = xb, histnorm = "probability density",
                               marker = list(color = TCOL[[t]], opacity = 0.75, line = list(color = "#fff", width = 1)))
      if (length(unique(d)) > 2) {
        df_ <- density(d)
        p <- p %>% add_lines(x = df_$x, y = df_$y, name = "Kepadatan (filter)", line = list(color = TCOL[[t]], width = 3))
      }
    }
    p %>% pl(paste(t, "·", VAR_DESC[t], "(kWh/m²)"), "Kepadatan") %>% layout(barmode = "overlay")
  })

  output$p_box <- renderPlotly({
    t <- tg(); all <- R()$df[[t]]; d <- fdf()[[t]]
    p <- plot_ly() %>% add_boxplot(x = all, name = "Semua", marker = list(color = "#94A3B8"),
                                   line = list(color = "#64748B"), fillcolor = "#E2E8F0")
    if (length(d)) p <- p %>% add_boxplot(x = d, name = "Filter", marker = list(color = TCOL[[t]]),
                                          line = list(color = TCOL[[t]]), fillcolor = paste0(TCOL[[t]], "55"))
    p %>% pl(NULL, NULL, legend = FALSE) %>% layout(margin = list(t = 5, b = 30))
  })

  output$ex_dist <- renderUI(ex_dist(R()$df, fdf(), tg()))

  output$p_rel <- renderPlotly({
    t <- tg(); v <- input$rel_x; df <- R()$df; colby <- input$rel_col
    grp <- if (colby == "none") rep("Bangunan", nrow(df)) else
      if (colby == "X5") ifelse(df$X5 >= 7, "X5 = 7 m (2 lantai)", "X5 = 3,5 m (1 lantai)") else paste0("X8 = ", df$X8)
    pal <- if (colby == "none") setNames(PRIMARY, "Bangunan") else
      if (colby == "X5") setNames(c(SECONDARY, ACCENT), c("X5 = 3,5 m (1 lantai)", "X5 = 7 m (2 lantai)")) else
        setNames(c("#94A3B8", unname(VCOL[c("X1", "X2", "X7", "X5", "X6")])), paste0("X8 = ", 0:5))

    if (v %in% CATEGORICAL_FEATURES) {
      labs_ <- if (v == "X6") X6_LABEL else X8_LABEL
      xcat <- factor(paste0(df[[v]], " · ", labs_[as.character(df[[v]])]),
                     levels = paste0(names(labs_), " · ", labs_))
      p <- plot_ly()
      for (g in unique(grp)) {
        ix <- grp == g
        p <- p %>% add_boxplot(x = xcat[ix], y = df[[t]][ix], name = g, boxpoints = "outliers",
                               marker = list(color = pal[[g]]), line = list(color = pal[[g]]),
                               fillcolor = paste0(if (colby == "none") TCOL[[t]] else pal[[g]], "66"))
      }
      return(p %>% pl(paste(v, "·", VAR_DESC[v]), paste(t, "(kWh/m²)"), legend = colby != "none") %>%
               layout(boxmode = "group"))
    }

    x  <- df[[v]]
    xj <- if (isTRUE(input$rel_jit)) jitter_x(x, 0.015 * diff(range(x))) else x
    lw <- lowess(x, df[[t]], f = input$span, iter = 3, delta = 0)
    p <- plot_ly()
    for (g in unique(grp)) {
      ix <- grp == g
      p <- p %>% add_markers(x = xj[ix], y = df[[t]][ix], name = g,
                             marker = list(color = pal[[g]], opacity = 0.45, size = 7),
                             hovertemplate = paste0(v, " = %{x:.3f}<br>", t, " = %{y:.2f}<extra>", g, "</extra>"))
    }
    p %>% add_lines(x = lw$x, y = lw$y, name = sprintf("LOWESS (span %s)", fn(input$span)),
                    line = list(color = TCOL[[t]], width = 4, shape = "spline")) %>%
      pl(paste(v, "·", VAR_DESC[v]), paste(t, "(kWh/m²)"))
  })
  output$ex_rel <- renderUI(ex_rel(R()$df, tg(), input$rel_x, input$span))

  output$p_heat <- renderPlotly({
    sc <- R()$spearman_corr; cols <- R()$spearman_cols; th <- input$rho_th; t <- tg()
    ycols <- rev(cols)
    z  <- sc[ycols, cols]
    zz <- ifelse(abs(z) >= th, z, 0)
    txt <- matrix(sprintf("%.2f", z), nrow(z))
    ann <- list()
    for (i in seq_along(ycols)) for (j in seq_along(cols)) {
      val <- z[i, j]
      ann[[length(ann) + 1]] <- list(x = cols[j], y = ycols[i], text = sprintf("%.2f", val), showarrow = FALSE,
                                     font = list(size = 11, color = if (abs(val) < th) "#CBD5E1" else if (abs(val) > 0.6) "#fff" else "#111827"))
    }
    ti <- match(t, cols) - 1; yi <- match(t, ycols) - 1
    plot_ly(x = cols, y = ycols, z = zz, type = "heatmap", text = txt, xgap = 2, ygap = 2,
            colorscale = list(c(0, "#3B4CC0"), c(0.5, "#FFFFFF"), c(1, "#B40426")), zmin = -1, zmax = 1,
            hovertemplate = "%{y} – %{x}<br>ρ = %{text}<extra></extra>",
            colorbar = list(title = "ρ", len = 0.8)) %>%
      pl(NULL, NULL, legend = FALSE) %>%
      layout(annotations = ann,
             xaxis = list(side = "bottom", showgrid = FALSE), yaxis = list(showgrid = FALSE, scaleanchor = "x"),
             shapes = list(
               list(type = "rect", x0 = ti - 0.5, x1 = ti + 0.5, y0 = -0.5, y1 = length(cols) - 0.5,
                    line = list(color = TCOL[[t]], width = 3)),
               list(type = "rect", y0 = yi - 0.5, y1 = yi + 0.5, x0 = -0.5, x1 = length(cols) - 0.5,
                    line = list(color = TCOL[[t]], width = 3))))
  })
  output$ex_corr <- renderUI(ex_corr(R(), tg(), input$rho_th))

  output$dep_txt <- renderUI({
    tags$p(style = "font-size:13.5px;",
           icon("link"), " Hubungan struktural ", tags$b("X2 = X3 + 2·X4"), ": selisih absolut maksimum ",
           tags$b(format(R()$dependency_max, digits = 6)), ". Duplikat baris: ", tags$b(R()$n_duplicate),
           ". Hubungan ini dipertahankan sebagai masalah multikolinearitas (lihat Tab 2).")
  })
  output$tbl_quality <- renderDT(dt_table(R()$quality, page = 10))
  output$tbl_data    <- renderDT(dt_table(fdf(), page = 10, filter = "top"))

  # ===========================================================================
  # TAB 2 - MODEL + DIAGNOSTIK
  # ===========================================================================
  lp <- reactive({
    fit <- R()$fitted_models[[tg()]]
    g   <- fit$lasso$glmnet.fit
    Zte <- apply_prep(fit$prep, R()$X_test)
    P   <- predict(g, newx = Zte, s = g$lambda)
    list(lam = g$lambda, B = as.matrix(g$beta), a0 = as.numeric(g$a0), cvm = fit$lasso$cvm, cvsd = fit$lasso$cvsd,
         lmin = fit$lasso$lambda.min,
         test_rmse = sqrt(colMeans((R()$Y_test[[tg()]] - P)^2)))
  })
  observeEvent(lp(), updateSliderInput(session, "loglam", value = round(log10(lp()$lmin), 2)))
  observeEvent(input$go_min, updateSliderInput(session, "loglam", value = round(log10(lp()$lmin), 2)))

  sel_lam <- reactive({
    L <- lp(); ll <- log10(L$lam); x <- input$loglam
    near <- which.min(abs(ll - x))
    ip <- function(yv) approx(ll, yv, xout = x, rule = 2, ties = mean)$y
    b <- apply(L$B, 1, ip); b[abs(L$B[, near]) <= 1e-10] <- 0
    list(b = b, a = ip(L$a0), lam = 10^x, nz = sum(abs(L$B[, near]) > 1e-10), te = ip(L$test_rmse))
  })
  observe({
    s <- sel_lam()
    session$sendCustomMessage("countup", list(v = list(k_nz = s$nz, k_te = s$te), d = list(k_nz = 0, k_te = 3)))
  })

  output$p_coefbar <- renderPlotly({
    s <- sel_lam(); L <- lp(); t <- tg()
    f <- rownames(L$B); b <- s$b[f]; lim <- max(abs(L$B)) * 1.15
    plot_ly(x = b, y = factor(f, levels = rev(f)), type = "bar", orientation = "h",
            marker = list(color = ifelse(abs(b) < 1e-10, "#CBD5E1", ifelse(b > 0, TCOL[[t]], SECONDARY))),
            text = ifelse(abs(b) < 1e-10, "0 (dibuang)", sprintf("%+.2f", b)), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{y}: β = %{x:.4f}<extra></extra>") %>%
      pl("Koefisien", NULL, legend = FALSE) %>%
      layout(xaxis = list(range = c(-lim, lim), zeroline = TRUE, zerolinecolor = "#94A3B8"), margin = list(l = 70, r = 30))
  })

  output$p_cvcurve <- renderPlotly({
    L <- lp(); t <- tg(); x <- log10(L$lam)
    plot_ly() %>%
      add_ribbons(x = x, ymin = L$cvm - L$cvsd, ymax = L$cvm + L$cvsd, fillcolor = paste0(TCOL[[t]], "33"),
                  line = list(color = "transparent"), hoverinfo = "skip", showlegend = FALSE) %>%
      add_lines(x = x, y = L$cvm, line = list(color = TCOL[[t]], width = 3), showlegend = FALSE,
                hovertemplate = "log₁₀λ = %{x:.2f}<br>MSE = %{y:.3f}<extra></extra>") %>%
      pl("log₁₀(λ)", "MSE CV", legend = FALSE) %>%
      layout(shapes = list(vline(isolate(input$loglam), PRIMARY)), yaxis = list(type = "log"),
             xaxis = list(range = c(-4, 1)), margin = list(t = 10))
  })
  observeEvent(input$loglam, {
    req(input$menu == "t_model")
    plotlyProxy("p_cvcurve", session) %>% plotlyProxyInvoke("relayout", list(shapes = list(vline(input$loglam, PRIMARY))))
  }, ignoreInit = TRUE)
  output$ex_lambda <- renderUI(ex_lambda(R(), tg(), sel_lam()))

  output$eq_lasso <- renderUI({
    t <- tg(); fit <- R()$fitted_models[[t]]; s <- sel_lam(); L <- lp()
    b <- s$b; a <- s$a
    is_min <- abs(log10(s$lam) - log10(L$lmin)) < 0.03
    term <- function(f) {
      v <- b[[f]]; nm <- if (f %in% NUMERIC_FEATURES) sprintf("z<sub>%s</sub>", f) else sub("_(\\d)$", "<sub>=\\1</sub>", f)
      sprintf("%s %s·%s", if (v < 0) "−" else "+", fn(abs(v)), nm)
    }
    keep <- names(b)[abs(b) > 1e-10]; drop <- names(b)[abs(b) <= 1e-10]
    pr <- fit$prep
    tagList(
      div(style = "margin-bottom:8px;",
          if (is_min) span(class = "badge-ok", icon("circle-check"), sprintf(" λ = %s (λ.min · model resmi)", formatC(s$lam, format = "g", digits = 3)))
          else span(class = "badge-warn", icon("sliders"), sprintf(" λ = %s (hasil geser slider, bukan model resmi) · tekan tombol λ.min untuk kembali", formatC(s$lam, format = "g", digits = 3)))),
      div(style = "font-size:17px; line-height:2; color:#0B2447; background:#F3F7FD; border-radius:10px; padding:10px 16px; overflow-x:auto;",
          HTML(sprintf("<b>Ŷ<sub>%s</sub></b> = %s %s", sub("Y", "", t), fn(a), if (length(keep)) paste(vapply(keep, term, ""), collapse = " ") else "(semua koefisien nol)"))),
      tags$p(style = "font-size:13px; color:#374151; margin:10px 0 0 0; line-height:1.8;",
             HTML(sprintf("Dibuang Lasso (β = 0): <b>%s</b>.<br>z<sub>X</sub> = (X − rata-rata) / SD data training: %s.<br>X6<sub>=k</sub>, X8<sub>=k</sub> bernilai 1 jika kategori = k (referensi: X6 = 2 Utara, X8 = 0 tanpa kaca). Satuan Ŷ: kWh/m².",
                          join_id(drop),
                          paste(sprintf("%s (%s; %s)", NUMERIC_FEATURES, fn(pr$center, 3), fn(pr$scale, 3)), collapse = ", "))))
    )
  })

  output$p_stab <- renderPlotly({
    t <- tg()
    d <- R()$selection_stability %>% filter(Target == t) %>% mutate(Variable = factor(Variable, levels = FEATURES)) %>% arrange(Variable)
    plot_ly(x = d$Variable, y = d$Selection_Frequency, type = "bar",
            marker = list(color = ifelse(d$Selection_Frequency >= 100, "#2E9E5B", "#CBD5E1")),
            text = paste0(fn(d$Selection_Frequency, 0), "%"), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{x}: %{y:.0f}%<extra></extra>") %>%
      pl(NULL, "Terpilih (%)", legend = FALSE) %>% layout(yaxis = list(range = c(0, 115)))
  })
  output$ex_stab <- renderUI(ex_stab(R(), tg()))

  output$p_vif <- renderPlotly({
    v <- R()$vif_table; capped <- !is.finite(v$VIF) | v$VIF > 1e6
    val <- ifelse(capped, 1e6, v$VIF)
    plot_ly(x = val, y = factor(v$Variable, levels = rev(v$Variable)), type = "bar", orientation = "h",
            marker = list(color = ifelse(val > 10, "#DC2626", TEAL)),
            text = ifelse(capped, "≈∞", fn(v$VIF, 1)), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{y}: VIF %{text}<extra></extra>") %>%
      pl("VIF (log)", NULL, legend = FALSE) %>%
      layout(xaxis = list(type = "log", range = c(0, 7)), margin = list(l = 60, r = 30),
             shapes = list(list(type = "line", x0 = 10, x1 = 10, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = ACCENT, dash = "dash", width = 2))))
  })

  output$p_resid <- renderPlotly({
    t <- tg(); m <- R()$ols_models[[t]]
    plot_ly(x = fitted(m), y = residuals(m), type = "scatter", mode = "markers",
            marker = list(color = TCOL[[t]], opacity = 0.5, size = 6),
            hovertemplate = "fitted %{x:.2f}<br>resid %{y:.2f}<extra></extra>") %>%
      pl("Fitted", "Residual", legend = FALSE) %>% layout(shapes = list(hline(0, "#111827")))
  })

  output$p_qq <- renderPlotly({
    t <- tg(); r <- residuals(R()$ols_models[[t]]); z <- sort((r - mean(r)) / sd(r))
    th <- qnorm(ppoints(length(z))); lim <- range(c(th, z))
    plot_ly() %>%
      add_lines(x = lim, y = lim, line = list(color = "#111827", dash = "dash"), hoverinfo = "skip") %>%
      add_markers(x = th, y = z, marker = list(color = TCOL[[t]], opacity = 0.55, size = 6)) %>%
      pl("Kuantil teoretis", "Kuantil sampel", legend = FALSE)
  })

  output$p_cook <- renderPlotly({
    t <- tg(); cd <- as.numeric(cooks.distance(R()$ols_models[[t]])); thr <- R()$cook_threshold
    obs <- seq_along(cd) - 1; big <- cd > thr
    plot_ly() %>%
      add_segments(x = obs, xend = obs, y = 0, yend = cd, line = list(color = "#94A3B8", width = 1.2), hoverinfo = "skip") %>%
      add_markers(x = obs, y = cd, marker = list(size = 4, color = ifelse(big, TCOL[[t]], "#64748B")),
                  hovertemplate = "obs %{x}<br>Cook's D = %{y:.4f}<extra></extra>") %>%
      pl("Observasi training", "Cook's Distance", legend = FALSE) %>% layout(shapes = list(hline(thr)))
  })
  output$ex_diag <- renderUI(ex_diag(R(), tg()))

  # ===========================================================================
  # TAB 3 - PREDIKSI INTERAKTIF + BENCHMARK
  # ===========================================================================
  shp <- reactive(R()$df %>% distinct(X1, X2, X3, X4, X5) %>% arrange(X1))
  observeEvent(shp(), updateSliderInput(session, "d_shape", max = nrow(shp()), value = min(6, nrow(shp()))))

  dX <- reactive({
    s <- shp()[min(input$d_shape, nrow(shp())), ]
    tibble(X1 = s$X1, X2 = s$X2, X3 = s$X3, X4 = s$X4, X5 = s$X5,
           X6 = as.numeric(input$d_x6), X7 = input$d_x7, X8 = as.numeric(input$d_x8))
  })
  preds <- reactive(setNames(lapply(TARGETS, function(t) {
    X <- dX()
    tibble(ols = predict_model(R()$fitted_models[[t]], "Linear Regression", X),
           las = predict_model(R()$fitted_models[[t]], "Lasso", X),
           rf  = predict_rf(R()$rf_models[[t]], X, R()$rf_levels))
  }), TARGETS))

  observe({
    p <- preds(); X <- dX(); df <- R()$df
    v <- list(); for (t in TARGETS) for (k in c("ols", "las", "rf")) v[[paste0("pv_", t, "_", k)]] <- p[[t]][[k]]
    session$sendCustomMessage("countup", list(v = v, d = lapply(v, function(x) 2)))
    session$sendCustomMessage("best", lapply(setNames(TARGETS, TARGETS), function(t) {
      b <- R()$cv_summary_all %>% filter(Target == t) %>% slice_min(RMSE_Mean, n = 1, with_ties = FALSE)
      c("Linear Regression" = "ols", "Lasso" = "las", "Random Forest" = "rf")[[as.character(b$Model)]]
    }))
    sc <- function(val, y) max(0.06, min(1, (val - min(y)) / (max(y) - min(y))))
    t1 <- sc(p$Y1$rf, df$Y1); t2 <- sc(p$Y2$rf, df$Y2); r4 <- range(sqrt(df$X4))
    session$sendCustomMessage("bld", list(
      w = 0.72 + 0.28 * (sqrt(X$X4) - r4[1]) / max(diff(r4), 1e-9),
      floors = if (X$X5 >= mean(range(df$X5))) 2 else 1,
      g = if (X$X7 <= 0) 0 else 0.35 + 0.65 * X$X7 / max(df$X7),
      hot = 0.45 + 0.55 * t2, cold = 0.3 + 0.7 * t1, t1 = t1, t2 = t2))
  })

  output$shape_info <- renderUI({
    X <- dX()
    tags$table(class = "shape-tbl", style = "margin:-6px 0 10px 0;",
               tags$tr(tags$td("X1 Relative Compactness"), tags$td(tags$b(fn(X$X1)))),
               tags$tr(tags$td("X2 Surface Area"), tags$td(tags$b(fn(X$X2, 1)), " m²")),
               tags$tr(tags$td("X3 Wall Area"), tags$td(tags$b(fn(X$X3, 1)), " m²")),
               tags$tr(tags$td("X4 Roof Area"), tags$td(tags$b(fn(X$X4, 2)), " m²")),
               tags$tr(tags$td("X5 Overall Height"), tags$td(tags$b(fn(X$X5, 1)), paste0(" m (", if (X$X5 >= 7) "2 lantai" else "1 lantai", ")"))))
  })

  output$actual_box <- renderUI({
    X <- dX(); df <- R()$df
    hit <- df %>% filter(abs(X1 - X$X1) < 1e-9, X6 == X$X6, abs(X7 - X$X7) < 1e-9, X8 == X$X8)
    if (nrow(hit)) {
      div(class = "badge-ok", style = "display:block; font-size:13px; padding:8px 12px;", icon("circle-check"),
          HTML(sprintf(" Ada di data. Nilai aktual: <b>Y1 = %s</b>, <b>Y2 = %s</b>", fn(mean(hit$Y1)), fn(mean(hit$Y2)))))
    } else {
      div(class = "badge-warn", style = "display:block; font-size:13px; padding:8px 12px;", icon("triangle-exclamation"),
          " Kombinasi ini tidak ada di data simulasi, prediksi hanya perkiraan.")
    }
  })

  observeEvent(input$d_rand, {
    row <- R()$df[sample(nrow(R()$df), 1), ]
    updateSliderInput(session, "d_shape", value = match(row$X1, shp()$X1))
    updateRadioButtons(session, "d_x6", selected = as.character(row$X6))
    updateSliderInput(session, "d_x7", value = row$X7)
    updateSelectInput(session, "d_x8", selected = as.character(row$X8))
  })
  observeEvent(input$d_reset, {
    updateSliderInput(session, "d_shape", value = 6)
    updateRadioButtons(session, "d_x6", selected = "4")
    updateSliderInput(session, "d_x7", value = 0.25)
    updateSelectInput(session, "d_x8", selected = "1")
  })

  # ---- Benchmark ----
  output$tbl_comp <- renderDT({
    t <- tg()
    d <- R()$model_comparison %>% filter(Target == t) %>% mutate(Model = as.character(Model)) %>%
      select(Model, RMSE_CV = RMSE_Mean, MAE_CV = MAE_Mean, R2_CV = R2_Mean, Test_RMSE, Test_R2)
    best <- d$Model[which.min(d$RMSE_CV)]
    dt_table(d) %>% formatStyle("Model", target = "row",
                                fontWeight = styleEqual(best, "bold"),
                                backgroundColor = styleEqual(best, if (t == "Y1") "#FEF3E2" else "#E1F5F4"))
  })

  output$p_cvbox <- renderPlotly({
    t <- tg(); d <- R()$cv_results_all %>% filter(Target == t)
    p <- plot_ly()
    for (mn in MODEL_LEVELS) {
      dd <- d %>% filter(Model == mn)
      p <- p %>% add_boxplot(y = dd$RMSE, name = mn, boxpoints = "all", jitter = 0.45, pointpos = 0,
                             marker = list(color = MCOL[[mn]], size = 4, opacity = 0.6),
                             line = list(color = MCOL[[mn]]), fillcolor = paste0(MCOL[[mn]], "40"))
    }
    p %>% pl(NULL, "RMSE per fold", legend = FALSE)
  })

  output$p_paired <- renderPlotly({
    t <- tg(); d <- R()$paired_diff %>% filter(Target == t) %>% arrange(Fold)
    plot_ly(x = d$Fold, y = d$Diff_RMSE, type = "scatter", mode = "markers",
            marker = list(size = 9, color = ifelse(d$Diff_RMSE > 0, "#2E9E5B", SECONDARY)),
            hovertemplate = "Fold %{x}<br>Selisih %{y:.3f}<extra></extra>") %>%
      pl("Fold", "RMSE Lasso − RMSE RF", legend = FALSE) %>%
      layout(shapes = list(hline(0, "#DC2626")),
             annotations = list(list(x = 1, xref = "paper", y = 1, yref = "paper", text = "▲ di atas 0 = RF lebih akurat",
                                     showarrow = FALSE, xanchor = "right", font = list(color = "#2E9E5B"))))
  })
  output$ex_bench <- renderUI(ex_bench(R(), tg()))

  output$p_imp <- renderPlotly({
    t <- tg(); d <- R()$rf_importance %>% filter(Target == t) %>% arrange(Permutation_Importance)
    fa <- R()$final_answer; sel <- fa[[paste0("Selected_", t)]][match(d$Variable, fa$Variable)]
    p <- plot_ly()
    for (s in c(TRUE, FALSE)) {
      ix <- sel == s
      if (!any(ix)) next
      p <- p %>% add_bars(x = d$Importance_Pct[ix], y = factor(d$Variable[ix], levels = d$Variable), orientation = "h",
                          name = if (s) "Dipilih Lasso" else "Dibuang Lasso", marker = list(color = if (s) TCOL[[t]] else "#CBD5E1"),
                          text = paste0(fn(d$Importance_Pct[ix], 1), "%"), textposition = "outside", cliponaxis = FALSE,
                          hovertemplate = "%{y}: %{x:.1f}%<extra></extra>")
    }
    p %>% pl("Importance RF (%)", NULL) %>% layout(margin = list(l = 50, r = 40))
  })
  output$ex_imp <- renderUI(ex_imp(R(), tg()))

  output$ex_final  <- renderUI(ex_final(R()))
  output$tbl_final <- renderDT(dt_table(R()$final_answer %>%
                                          select(Variable, Selected_Y1, CV_Selection_Frequency_Y1, Selected_Y2, CV_Selection_Frequency_Y2)))
  output$dl_final  <- downloadHandler("final_answer.csv", function(f) write_csv(R()$final_answer, f))
  output$dl_comp   <- downloadHandler("model_comparison.csv", function(f) write_csv(R()$model_comparison, f))
}

shinyApp(ui, server)
