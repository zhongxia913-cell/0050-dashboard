# ============================================================
# build_backtest.R：歷史訊號回測（每週手動或排程執行一次）
# 規則固定後不再更動（避免迎合歷史資料）
# ============================================================
library(dplyr)
source("indicators_technical.R")

fmt_pct <- function(x, digits = 2, show_sign = TRUE) {
  if (show_sign) {
    ifelse(is.finite(x), sprintf(paste0("%+.", digits, "f%%"), x * 100), "NA")
  } else {
    ifelse(is.finite(x), sprintf(paste0("%.", digits, "f%%"), x * 100), "NA")
  }
}

HORIZONS <- c(5, 20, 60)
raw <- readRDS("data/stock_data.rds")

# 向量化訊號標籤（快、可重現）
label_signals <- function(d) {
  d <- calc_technical_one(d)
  d$signal <- case_when(
    d$close > d$MA20 & d$MA20 > d$MA60 & d$DIF > d$DEA ~ "偏多",
    d$close < d$MA20 & d$MA20 < d$MA60 & d$DIF < d$DEA ~ "偏空",
    TRUE ~ "中立"
  )
  d
}

results <- lapply(unique(raw$symbol[raw$symbol != "0050"]), function(code) {
  d <- raw %>% filter(symbol == code) %>% arrange(date)
  if (nrow(d) < 300) return(NULL)          # 樣本太短（鴻勁、藥華藥）剔除
  d <- label_signals(d)
  n <- nrow(d)
  out <- lapply(HORIZONS, function(h) {
    idx <- seq_len(n - h)
    data.frame(symbol = code, date = d$date[idx],
               signal = d$signal[idx],
               horizon = h,
               fwd_ret = d$adjusted[idx + h] / d$adjusted[idx] - 1)
  })
  bind_rows(out)
})
bt_signals <- bind_rows(results)

# ---------- 訊號統計表 ----------
bt_summary <- bt_signals %>%
  group_by(signal, horizon) %>%
  summarise(次數 = n(),
            平均報酬 = mean(fwd_ret, na.rm = TRUE),
            正報酬比例 = mean(fwd_ret > 0, na.rm = TRUE),
            .groups = "drop")

# ---------- 策略 vs Benchmark ----------
# 策略：每日等權持有當天「偏多」的股票（每日再平衡）
daily_strat <- bt_signals %>%
  filter(horizon == 5, signal == "偏多") %>%
  group_by(date) %>%
  summarise(strat_ret = mean(fwd_ret, na.rm = TRUE) / 5, .groups = "drop") %>%
  arrange(date) %>%
  mutate(strat_cum = cumprod(1 + strat_ret))

bench <- raw %>% filter(symbol == "0050") %>% arrange(date) %>%
  mutate(bench_cum = adjusted / first(adjusted))

metrics <- function(ret_vec, cum_vec) {
  ann_vol <- sd(ret_vec, na.rm = TRUE) * sqrt(252)
  list(cum = tail(cum_vec, 1) - 1,
       sharpe = ifelse(ann_vol > 0,
                       mean(ret_vec, na.rm = TRUE) * 252 / ann_vol, NA),
       mdd = min(cum_vec / cummax(cum_vec) - 1, na.rm = TRUE))
}
bench_ret <- c(NA, diff(log(bench$adjusted)))
bt_benchmark <- data.frame(
  方法 = c("系統策略（持有偏多股）", "0050 Buy & Hold"),
  累積報酬 = c(fmt_pct(metrics(daily_strat$strat_ret, daily_strat$strat_cum)$cum),
           fmt_pct(metrics(bench_ret, bench$bench_cum)$cum)),
  Sharpe = c(round(metrics(daily_strat$strat_ret, daily_strat$strat_cum)$sharpe, 2),
             round(metrics(bench_ret, bench$bench_cum)$sharpe, 2)),
  最大回撤 = c(fmt_pct(metrics(daily_strat$strat_ret, daily_strat$strat_cum)$mdd),
           fmt_pct(metrics(bench_ret, bench$bench_cum)$mdd))
)

saveRDS(list(signals = bt_signals, summary = bt_summary,
             benchmark = bt_benchmark, strat_curve = daily_strat,
             bench_curve = bench %>% select(date, bench_cum),
             built_at = format(Sys.time(), tz = "Asia/Taipei")),
        "data/backtest_signals.rds")
cat("回測完成：", nrow(bt_signals), "筆訊號紀錄\n")