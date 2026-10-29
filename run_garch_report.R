# ============================================================
# run_garch_report.R
# 目的：在 RStudio 手動跑一次 GARCH(1,1)，產生可貼進論文的表格與圖
#
# 用法：
#   1. 把這個檔案和 garch_model.R 放在專案根目錄（與 app.R 同一層）
#   2. Session > Set Working Directory > Choose Directory 選到專案根目錄
#   3. 全選（Ctrl+A）後執行（Ctrl+Enter）
#
# 產出：garch_out/ 資料夾
#   garch_參數表.csv       ← 貼進論文或 Excel
#   garch_執行摘要.txt     ← 含執行時間、R 版本，可證明是你跑的
#   fig_vol_xxx.png        ← 指定股票的條件波動圖
#   fig_ratio.png          ← 全部股票 預測/實現 比值
#   fig_alpha_beta.png     ← α 與 β 的散布
# ============================================================

# ------------------------------------------------------------
# 0. 可調參數
# ------------------------------------------------------------

WINDOW   <- 1000        # 每檔用最近幾個交易日估計（500 / 1000 / 1500）
HORIZON  <- 63          # 往前預測幾個交易日（63 ≈ 一季）
TARGET   <- "0050"      # 要單獨畫圖的股票
ONLY     <- NULL        # 只跑特定幾檔，例：c("0050","2330","1303")；NULL = 全部
OUT_DIR  <- "garch_out"

# ------------------------------------------------------------
# 1. 檢查環境
# ------------------------------------------------------------

cat("\n=== GARCH(1,1) 手動執行 ===\n")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("找不到 ggplot2，請先執行 install.packages('ggplot2')")
}

DATA_FILE <- "data/stock_data.rds"
if (!file.exists(DATA_FILE)) {
  stop(paste0(
    "找不到 ", DATA_FILE, "\n",
    "請確認：Session > Set Working Directory > Choose Directory 已選到專案根目錄。\n",
    "目前工作目錄是：", getwd()
  ))
}

if (!file.exists("garch_model.R")) {
  stop("找不到 garch_model.R，請把它放在同一個資料夾。")
}
source("garch_model.R", encoding = "UTF-8")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 2. 讀資料
# ------------------------------------------------------------

raw <- readRDS(DATA_FILE)
d <- as.data.frame(raw)

need <- c("symbol", "date", "adjusted")
miss <- setdiff(need, names(d))
if (length(miss) > 0) {
  stop(paste0("資料缺少欄位：", paste(miss, collapse = ", "),
              "\n實際欄位：", paste(names(d), collapse = ", ")))
}

d$symbol <- as.character(d$symbol)
d$date   <- as.Date(d$date)
d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
d <- d[order(d$symbol, d$date), , drop = FALSE]

cat("資料筆數：", nrow(d), "\n")
cat("股票檔數：", length(unique(d$symbol)), "\n")
cat("日期範圍：", format(min(d$date)), " ~ ", format(max(d$date)), "\n\n")

# ------------------------------------------------------------
# 3. 股票名稱對照（不依賴 app.R）
# ------------------------------------------------------------

CODE <- c("0050","2330","2454","2308","2317","3711","2303","2383","3037","2881",
          "2891","1303","3017","2882","2887","2345","2382","2327","2360","2885",
          "2884","2059","2408","6669","2357","2883","2886","3008","2890","3443",
          "3231","2301","2344","2412","3653","2880","2892","3665","1216","4958",
          "6446","7769","2368","2449","2395","5880","8046","2603","4904","3045",
          "6505")

NAME <- c("元大台灣50","台積電","聯發科","台達電","鴻海","日月光投控","聯電","台光電","欣興","富邦金",
          "中信金","南亞","奇鋐","國泰金","台新新光金","智邦","廣達","國巨","致茂","元大金",
          "玉山金","川湖","南亞科","緯穎","華碩","凱基金","兆豐金","大立光","永豐金","創意",
          "緯創","光寶科","華邦電","中華電","健策","華南金","第一金","貿聯-KY","統一","臻鼎-KY",
          "藥華藥","鴻勁","金像電","京元電子","研華","合庫金","南電","長榮","遠傳","台灣大",
          "台塑化")

name_of <- function(x) {
  i <- match(as.character(x), CODE)
  ifelse(is.na(i), as.character(x), NAME[i])
}

# ------------------------------------------------------------
# 4. 逐檔估計
# ------------------------------------------------------------

syms <- sort(unique(d$symbol))
if (!is.null(ONLY)) syms <- intersect(syms, ONLY)

sp <- split(d$adjusted, d$symbol)
res <- list()
fail <- character(0)

pb <- utils::txtProgressBar(min = 0, max = length(syms), style = 3)

for (k in seq_along(syms)) {
  s <- syms[k]
  utils::setTxtProgressBar(pb, k)

  x <- sp[[s]]
  if (is.null(x) || length(x) < 260) {
    fail <- c(fail, paste0(s, "：資料不足"))
    next
  }

  r <- 100 * diff(log(as.numeric(x)))
  r <- r[is.finite(r)]
  if (length(r) > WINDOW) r <- utils::tail(r, WINDOW)
  if (length(r) < 250) {
    fail <- c(fail, paste0(s, "：有效報酬少於 250"))
    next
  }

  fit <- try(garch11_fit(r), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit)) {
    fail <- c(fail, paste0(s, "：估計失敗"))
    next
  }

  sm <- garch11_summary(fit, HORIZON)

  tail_n <- min(252, length(r))
  rv <- sqrt(mean(utils::tail(r, tail_n)^2) * 252) / 100
  ratio <- if (rv > 0) sm$forecast_vol / rv else NA_real_

  verdict <- if (!isTRUE(fit$converged)) {
    "未收斂"
  } else if (is.finite(fit$lb_p) && fit$lb_p < 0.05) {
    "配適不足"
  } else if (fit$persistence >= 0.99) {
    "近整合"
  } else {
    "通過"
  }

  res[[length(res) + 1]] <- data.frame(
    股票代號 = s,
    股票名稱 = name_of(s),
    樣本數 = fit$n,
    omega = fit$omega,
    alpha = fit$alpha,
    beta = fit$beta,
    持續性 = fit$persistence,
    半衰期 = fit$half_life,
    alpha_t = fit$t_alpha,
    beta_t = fit$t_beta,
    預測年化波動 = sm$forecast_vol,
    近一年實現波動 = rv,
    預測實現比 = ratio,
    VaR95_1日 = sm$var95,
    CVaR95_1日 = sm$cvar95,
    對數概似 = fit$loglik,
    AIC = fit$aic,
    BIC = fit$bic,
    LB_p值 = fit$lb_p,
    收斂 = fit$converged,
    判定 = verdict,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

close(pb)

if (!length(res)) stop("沒有任何一檔估計成功，請檢查資料。")

tab <- do.call(rbind, res)
rownames(tab) <- NULL

# ------------------------------------------------------------
# 5. 主控台輸出
# ------------------------------------------------------------

cat("\n\n=== 參數估計結果 ===\n")

show <- data.frame(
  代號 = tab$股票代號,
  名稱 = tab$股票名稱,
  omega = sprintf("%.4f", tab$omega),
  alpha = sprintf("%.4f", tab$alpha),
  beta = sprintf("%.4f", tab$beta),
  `a+b` = sprintf("%.3f", tab$持續性),
  半衰期 = ifelse(is.finite(tab$半衰期), sprintf("%.1f", tab$半衰期), "Inf"),
  預測波動 = sprintf("%.1f%%", tab$預測年化波動 * 100),
  實現波動 = sprintf("%.1f%%", tab$近一年實現波動 * 100),
  比值 = sprintf("%.2f", tab$預測實現比),
  LB_p = sprintf("%.3f", tab$LB_p值),
  判定 = tab$判定,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

print(show, row.names = FALSE)

cat("\n=== 全體統計 ===\n")
cat("成功：", nrow(tab), " 檔；失敗：", length(fail), " 檔\n")
cat("平均持續性 (a+b)：", sprintf("%.4f", mean(tab$持續性, na.rm = TRUE)), "\n")
cat("alpha 顯著 (t >= 1.96)：",
    sum(tab$alpha_t >= 1.96, na.rm = TRUE), " 檔\n")
cat("判定通過：", sum(tab$判定 == "通過"), " 檔\n")
if (length(fail)) cat("失敗原因：", paste(fail, collapse = "；"), "\n")

# ------------------------------------------------------------
# 6. 輸出 CSV
# ------------------------------------------------------------

save_csv <- function(df, path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_excel_csv(df, path)
  } else {
    utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  }
}

csv_path <- file.path(OUT_DIR, "garch_參數表.csv")
save_csv(tab, csv_path)

# ------------------------------------------------------------
# 7. 輸出執行摘要（可證明是你跑的）
# ------------------------------------------------------------

si <- utils::sessionInfo()
lines <- c(
  "GARCH(1,1) 估計執行摘要",
  "========================================",
  paste0("執行時間：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("R 版本：", R.version.string),
  paste0("平台：", R.version$platform),
  paste0("資料檔：", normalizePath(DATA_FILE)),
  paste0("資料筆數：", nrow(d)),
  paste0("資料範圍：", format(min(d$date)), " ~ ", format(max(d$date))),
  paste0("估計視窗：最近 ", WINDOW, " 個交易日"),
  paste0("預測期：", HORIZON, " 個交易日"),
  paste0("成功估計：", nrow(tab), " 檔"),
  paste0("失敗：", length(fail), " 檔"),
  "",
  "參數定義：sigma2_t = omega + alpha * r2_(t-1) + beta * sigma2_(t-1)",
  "持續性 = alpha + beta；半衰期 = ln(0.5)/ln(alpha+beta)",
  "標準誤以 observed information 計算，t 值 = 估計值 / 標準誤",
  "",
  "套件版本：",
  paste0("  ", names(si$otherPkgs), " ",
         vapply(si$otherPkgs, function(p) as.character(p$Version), character(1)),
         collapse = "\n")
)

writeLines(lines, file.path(OUT_DIR, "garch_執行摘要.txt"), useBytes = TRUE)

# ------------------------------------------------------------
# 8. 圖：指定股票的條件波動與預測
# ------------------------------------------------------------

if (TARGET %in% tab$股票代號) {
  x <- sp[[TARGET]]
  r <- 100 * diff(log(as.numeric(x)))
  r <- r[is.finite(r)]
  if (length(r) > WINDOW) r <- utils::tail(r, WINDOW)

  fit <- garch11_fit(r)
  if (!is.null(fit)) {
    n <- length(r)
    idx <- max(1, n - 249):n
    gvol <- sqrt(fit$sigma2[idx] * 252) / 100

    # 21 日實現波動
    real <- rep(NA_real_, n)
    for (i in 21:n) real[i] <- sqrt(mean(r[(i - 20):i]^2) * 252) / 100

    df <- data.frame(t = seq_along(idx), garch = gvol, real = real[idx])

    p1 <- ggplot2::ggplot(df, ggplot2::aes(x = t)) +
      ggplot2::geom_line(ggplot2::aes(y = real), colour = "grey65", linewidth = 0.7) +
      ggplot2::geom_line(ggplot2::aes(y = garch), colour = "#1976d2", linewidth = 0.9) +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = paste0(TARGET, " GARCH(1,1) conditional volatility"),
                    subtitle = "blue = GARCH, grey = 21-day realised",
                    x = "last 250 trading days", y = "annualised volatility") +
      ggplot2::theme_minimal()

    ggplot2::ggsave(file.path(OUT_DIR, paste0("fig_vol_", TARGET, ".png")),
                    p1, width = 10, height = 5, dpi = 150)

    fv <- garch11_forecast(fit, HORIZON)
    d2 <- data.frame(h = seq_along(fv), vol = sqrt(fv * 252) / 100)

    p2 <- ggplot2::ggplot(d2, ggplot2::aes(x = h, y = vol)) +
      ggplot2::geom_line(colour = "#2e7d32", linewidth = 1) +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = paste0(TARGET, " volatility forecast (", HORIZON, " days)"),
                    x = "forecast horizon (trading days)", y = "annualised volatility") +
      ggplot2::theme_minimal()

    ggplot2::ggsave(file.path(OUT_DIR, paste0("fig_forecast_", TARGET, ".png")),
                    p2, width = 9, height = 4.5, dpi = 150)
  }
}

# ------------------------------------------------------------
# 9. 圖：全部股票的 預測 / 實現 比值
# ------------------------------------------------------------

tt <- tab[is.finite(tab$預測實現比), , drop = FALSE]
if (nrow(tt) > 0) {
  tt <- tt[order(tt$預測實現比), , drop = FALSE]
  tt$lab <- paste0(tt$股票名稱, "(", tt$股票代號, ")")
  tt$lab <- factor(tt$lab, levels = tt$lab)
  tt$ratio <- tt$預測實現比

  p3 <- ggplot2::ggplot(tt, ggplot2::aes(x = lab, y = ratio, fill = ratio)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_gradient2(midpoint = 1, low = "#1565c0", mid = "white", high = "#c62828") +
    ggplot2::labs(title = "Forecast / Realised volatility ratio",
                  x = NULL, y = "ratio") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.y = ggplot2::element_text(size = 7))

  ggplot2::ggsave(file.path(OUT_DIR, "fig_ratio.png"), p3,
                  width = 8, height = 11, dpi = 150)
}

# ------------------------------------------------------------
# 10. 圖：alpha 與 beta 的散布
# ------------------------------------------------------------

tab$status <- factor(tab$判定)

p4 <- ggplot2::ggplot(tab, ggplot2::aes(x = alpha, y = beta)) +
  ggplot2::geom_point(ggplot2::aes(colour = status), size = 2.6) +
  ggplot2::geom_hline(yintercept = 0.9, linetype = 3, colour = "grey50") +
  ggplot2::labs(title = "GARCH(1,1) parameter estimates",
                subtitle = "each point = one stock",
                x = "alpha (ARCH effect)", y = "beta (persistence)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(file.path(OUT_DIR, "fig_alpha_beta.png"), p4,
                width = 9, height = 6, dpi = 150)

# ------------------------------------------------------------
# 11. 完成
# ------------------------------------------------------------

cat("\n=== 輸出檔案 ===\n")
cat("資料表：", normalizePath(csv_path), "\n")
cat("執行摘要：", normalizePath(file.path(OUT_DIR, "garch_執行摘要.txt")), "\n")
cat("圖檔：", normalizePath(OUT_DIR), "\n\n")
cat("完成。把 garch_out/ 整個資料夾留著，口試用得到。\n")
