# ============================================================
# score_model.R：投資建議評分模型
# ============================================================
cross_up <- function(a, b) {
  x <- a > b
  lag_x <- c(NA, x[-length(x)])
  !is.na(lag_x) & x & !lag_x
}

tech_signals <- function(df) {
  df <- df[order(df$date), ]
  n <- nrow(df)
  sig <- data.frame(indicator = character(), type = character(), text = character(), stringsAsFactors = FALSE)
  add <- function(ind, type, text) sig <<- rbind(sig, data.frame(indicator = ind, type = type, text = text))
  win <- max(1, n - 4):n   
  
  if (any(cross_up(df$MA5, df$MA20)[win], na.rm = TRUE)) add("MA", "buy", "黃金交叉（MA5 上穿 MA20）")
  if (any(cross_up(df$MA20, df$MA5)[win], na.rm = TRUE)) add("MA", "sell", "死亡交叉（MA5 下穿 MA20）")
  if (any(cross_up(df$K, df$D)[win] & df$K[win] < 20, na.rm = TRUE)) add("KD", "buy", "20 以下 K 上穿 D")
  if (any(cross_up(df$D, df$K)[win] & df$K[win] > 80, na.rm = TRUE)) add("KD", "sell", "80 以上 K 下穿 D")
  if (length(df$RSI14) >= n && !is.na(df$RSI14[n]) && df$RSI14[n] < 30) add("RSI", "buy", "RSI 低於 30（超賣）")
  if (length(df$RSI14) >= n && !is.na(df$RSI14[n]) && df$RSI14[n] > 70) add("RSI", "sell", "RSI 高於 70（超買）")
  if (any(cross_up(df$DIF, df$DEA)[win], na.rm = TRUE)) add("MACD", "buy", "DIF 上穿 DEA")
  if (any(cross_up(df$DEA, df$DIF)[win], na.rm = TRUE)) add("MACD", "sell", "DIF 下穿 DEA")
  sig
}

score_technical <- function(df) {
  df <- df[order(df$date), ]
  n <- nrow(df)
  pts <- 0; notes <- character()
  last <- df[n, ]
  ma60_slope <- last$MA60 - df[max(1, n - 20), ]$MA60
  obv_slope <- last$OBV - df[max(1, n - 20), ]$OBV
  vol_ma20 <- mean(tail(df$volume, 20), na.rm = TRUE)
  
  if (!is.na(last$close) && !is.na(last$MA20) && last$close > last$MA20) { pts <- pts + 10; notes <- c(notes, "股價站上 MA20") }
  if (!is.na(last$MA20) && !is.na(last$MA60) && last$MA20 > last$MA60) { pts <- pts + 10; notes <- c(notes, "MA20 高於 MA60") }
  if (!is.na(ma60_slope) && ma60_slope > 0) { pts <- pts + 10; notes <- c(notes, "MA60 向上") }
  if (!is.na(last$DIF) && !is.na(last$DEA) && last$DIF > last$DEA) { pts <- pts + 10; notes <- c(notes, "MACD 多頭排列") }
  if (!is.na(last$K) && !is.na(last$D) && last$K > last$D) { pts <- pts + 5; notes <- c(notes, "KD 金叉狀態") }
  if (!is.na(last$RSI14) && last$RSI14 >= 30 && last$RSI14 <= 70) pts <- pts + 5
  if (!is.na(obv_slope) && obv_slope > 0) { pts <- pts + 5; notes <- c(notes, "OBV 向上") }
  if (!is.na(last$volume) && last$volume > 1.2 * vol_ma20 && last$close > last$open) { pts <- pts + 5; notes <- c(notes, "量價齊揚") }
  if (!is.na(last$RSI14) && last$RSI14 > 80) { pts <- pts - 5; notes <- c(notes, "RSI 嚴重超買") }
  list(score = max(0, min(100, pts / 60 * 100)), notes = notes)
}

score_fundamental <- function(l, symbol = NULL) {
  r <- compute_fin_ratios(l, symbol)
  pts <- 0; notes <- character()
  cr <- r["current_ratio"]; dr <- r["debt_ratio"]
  if (!is.na(cr)) { if (cr >= 150) { pts <- pts + 10; notes <- c(notes, "流動比率優良") } else if (cr >= 100) pts <- pts + 6 }
  if (!is.na(dr)) { if (dr < 50) { pts <- pts + 10; notes <- c(notes, "負債比率低") } else if (dr <= 70) pts <- pts + 6 }
  roe <- r["roe"]
  if (!is.na(roe)) { if (roe > 15) { pts <- pts + 15; notes <- c(notes, "ROE 優秀") } else if (roe >= 8) pts <- pts + 10 else if (roe >= 0) pts <- pts + 5 }
  gm <- r["gross_margin"]
  if (!is.na(gm)) { if (gm > 30) pts <- pts + 10 else if (gm >= 15) pts <- pts + 6 else pts <- pts + 2 }
  nm <- r["net_margin"]
  if (!is.na(nm)) { if (nm > 10) pts <- pts + 10 else if (nm >= 5) pts <- pts + 6 else if (nm > 0) pts <- pts + 3 }
  pe <- r["pe"]
  if (!is.na(pe)) { if (pe > 0 && pe <= 15) { pts <- pts + 10; notes <- c(notes, "本益比偏低") } else if (pe <= 25) pts <- pts + 6 else if (pe <= 40) pts <- pts + 3 }
  pb <- r["pb"]
  if (!is.na(pb)) { if (pb < 2) pts <- pts + 8 else if (pb <= 4) pts <- pts + 5 else pts <- pts + 2 }
  dy <- r["div_yield"]
  if (!is.na(dy)) { if (dy > 4) pts <- pts + 7 else if (dy >= 2) pts <- pts + 5 else if (dy > 0) pts <- pts + 2 }
  list(score = max(0, min(100, pts / 95 * 100)), notes = notes)
}

rating_label <- function(score) {
  ifelse(score >= 70, "買進",
         ifelse(score >= 55, "加碼",
                ifelse(score >= 45, "持有",
                       ifelse(score >= 30, "減碼", "觀望迴避"))))
}