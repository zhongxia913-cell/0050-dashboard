# ============================================================
# indicators_technical.R：技術指標計算（最終完整版）
# ============================================================
library(TTR)
library(tidyr) # 用於填補市場廣度指標的 NA

calc_technical_one <- function(df) {
  df <- df[order(df$date), ]
  df <- df[!is.na(df$open) & !is.na(df$high) & !is.na(df$low) &
             !is.na(df$close) & is.finite(df$close), ]
  n <- nrow(df)
  if (n == 0) return(df)
  
  close <- df$close; high <- df$high; low <- df$low
  vol <- df$volume; vol[is.na(vol)] <- 0
  chg <- c(NA_real_, diff(close))
  hlc <- cbind(high = high, low = low, close = close)
  
  ## 移動平均線 MA
  df$MA5   <- SMA(close, 5)
  df$MA10  <- SMA(close, 10)
  df$MA20  <- SMA(close, 20)
  df$MA60  <- SMA(close, 60)
  df$MA120 <- SMA(close, 120)
  df$MA240 <- SMA(close, 240)
  
  ## 乖離率 BIAS
  df$BIAS6  <- (close / SMA(close, 6)  - 1) * 100
  df$BIAS12 <- (close / SMA(close, 12) - 1) * 100
  df$BIAS24 <- (close / SMA(close, 24) - 1) * 100
  
  ## OBV
  df$OBV <- OBV(close, vol)
  
  ## RSI
  df$RSI14 <- RSI(close, 14)
  
  ## KD（手動台股遞迴定義，避開 stoch 函數）
  ll <- runMin(low, 9); hh <- runMax(high, 9)
  rsv <- (close - ll) / (hh - ll) * 100
  rsv[!is.finite(rsv)] <- 50
  K <- numeric(n); D <- numeric(n)
  K[1] <- 50; D[1] <- 50
  if (n > 1) {
    for (i in 2:n) {
      K[i] <- (K[i - 1] * 2 + rsv[i]) / 3
      D[i] <- (D[i - 1] * 2 + K[i]) / 3
    }
  }
  df$K <- K; df$D <- D
  
  ## MACD（使用矩陣索引取值）
  mc <- MACD(close, 12, 26, 9)
  df$DIF <- as.numeric(mc[, "macd"])
  df$DEA <- as.numeric(mc[, "signal"])
  df$OSC <- (df$DIF - df$DEA) * 2
  
  ## DMI（手動 Wilder 實作）
  prev_close <- c(NA_real_, close[-n])
  up_move    <- c(NA_real_, diff(high))
  down_move  <- c(NA_real_, -diff(low))
  plusDM  <- ifelse(up_move > down_move & up_move > 0, up_move, 0)
  minusDM <- ifelse(down_move > up_move & down_move > 0, down_move, 0)
  plusDM[1]  <- 0; minusDM[1] <- 0
  tr <- pmax(high - low, abs(high - prev_close), abs(low  - prev_close))
  tr[1] <- NA_real_
  wilder <- function(x) EMA(x, n = 14, wilder = TRUE)
  tr_s   <- wilder(tr)
  plusDI  <- 100 * wilder(plusDM)  / tr_s
  minusDI <- 100 * wilder(minusDM) / tr_s
  plusDI  <- ifelse(is.finite(plusDI),  plusDI,  NA_real_)
  minusDI <- ifelse(is.finite(minusDI), minusDI, NA_real_)
  dx <- 100 * abs(plusDI - minusDI) / (plusDI + minusDI)
  dx <- ifelse(is.finite(dx), dx, NA_real_)
  df$PLUS_DI  <- plusDI
  df$MINUS_DI <- minusDI
  df$ADX      <- EMA(dx, n = 14, wilder = TRUE)
  
  ## WMS%R（手動計算）
  hh14 <- runMax(high, 14)
  ll14 <- runMin(low, 14)
  denom <- hh14 - ll14
  df$WMSR <- ifelse(denom > 0, -100 * (hh14 - close) / denom, NA_real_)
  
  ## PSY
  upday <- ifelse(chg > 0, 1, 0)
  upday[1] <- 0
  df$PSY12 <- runSum(upday, 12) / 12 * 100
  
  ## VR
  vA <- ifelse(chg > 0, vol, 0); vB <- ifelse(chg < 0, vol, 0)
  vC <- ifelse(chg == 0, vol, 0)
  vA[1] <- 0; vB[1] <- 0; vC[1] <- 0
  df$VR <- (runSum(vA, 26) + 0.5 * runSum(vC, 26)) /
    (runSum(vB, 26) + 0.5 * runSum(vC, 26)) * 100
  
  ## 寶塔線
  prev <- c(NA_real_, close[-n])
  df$tower_low  <- pmin(close, prev)
  df$tower_high <- pmax(close, prev)
  df$tower_up   <- close >= prev
  lag_up <- c(NA, df$tower_up[-n])
  df$tower_flip <- !is.na(lag_up) & (df$tower_up != lag_up)
  df
}

# ------------------------------------------------------------
# 市場廣度指標（修正 NA 問題）
# ------------------------------------------------------------
calc_market_breadth <- function(data) {
  daily <- data %>%
    filter(symbol != "0050") %>%
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(chg = close - lag(close)) %>%
    ungroup() %>%
    filter(!is.na(chg)) %>%
    group_by(date) %>%
    summarise(adv = sum(chg > 0), dec = sum(chg < 0), .groups = "drop") %>%
    arrange(date)
  
  daily$ADR <- ifelse(daily$dec > 0, daily$adv / daily$dec * 100, NA_real_)
  
  # 關鍵修正：將中間的 NA 向下填補，避免 runMean 報錯
  daily <- tidyr::fill(daily, ADR, .direction = "down")
  
  daily$ADR10  <- runMean(daily$ADR, 10)
  daily$ADL    <- cumsum(daily$adv - daily$dec)
  daily$OBOS10 <- runSum(daily$adv - daily$dec, 10)
  daily
}

# ------------------------------------------------------------
# K 線主圖與副圖函數庫 (保持原樣)
# ------------------------------------------------------------
plot_candlestick <- function(df, ma_cols = c("MA5", "MA20", "MA60")) {
  ma_palette <- c(MA5 = "#1976d2", MA10 = "#f57c00", MA20 = "#7b1fa2",
                  MA60 = "#00796b", MA120 = "#5d4037", MA240 = "#455a64")
  ma_cols <- intersect(ma_cols, names(df))
  candle_col <- ifelse(df$close >= df$open, UP_COLOR, DOWN_COLOR)
  g <- ggplot(df, aes(x = date)) +
    geom_segment(aes(x = date, xend = date, y = low, yend = high),
                 color = candle_col, linewidth = 0.4) +
    geom_rect(aes(xmin = date - 0.4, xmax = date + 0.4,
                  ymin = pmin(open, close), ymax = pmax(open, close)),
              fill = candle_col, color = "gray35", linewidth = 0.2)
  if (length(ma_cols) > 0) {
    g <- g + lapply(ma_cols, function(m) {
      geom_line(aes(y = .data[[m]]), color = ma_palette[[m]], linewidth = 0.7)
    })
  }
  g + labs(title = "K 線與移動平均線", x = "日期", y = "價格") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

tech_subchart_funs <- list(
  "成交量 VOL" = function(df) {
    ggplot(df, aes(x = date, y = volume)) +
      geom_col(fill = ifelse(df$close >= df$open, UP_COLOR, DOWN_COLOR)) +
      labs(title = "成交量", y = "成交量") + theme_minimal()
  },
  "MACD (DIF/DEA/OSC)" = function(df) {
    ggplot(df, aes(x = date)) +
      geom_col(aes(y = OSC), fill = ifelse(df$OSC >= 0, UP_COLOR, DOWN_COLOR), width = 0.8) +
      geom_line(aes(y = DIF, color = "DIF"), linewidth = 0.7) +
      geom_line(aes(y = DEA, color = "DEA"), linewidth = 0.7) +
      scale_color_manual(values = c(DIF = "#1976d2", DEA = "#f57c00")) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "MACD", color = NULL) + theme_minimal()
  },
  "相對強弱 RSI" = function(df) {
    ggplot(df, aes(x = date, y = RSI14)) + geom_line(color = "#7b1fa2", linewidth = 0.8) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "gray50") +
      labs(title = "RSI(14)") + theme_minimal()
  },
  "隨機指標 KD" = function(df) {
    ggplot(df, aes(x = date)) +
      geom_line(aes(y = K, color = "K"), linewidth = 0.8) +
      geom_line(aes(y = D, color = "D"), linewidth = 0.8) +
      scale_color_manual(values = c(K = "#1976d2", D = "#f57c00")) +
      geom_hline(yintercept = c(20, 80), linetype = "dashed", color = "gray50") +
      labs(title = "KD 值", color = NULL) + theme_minimal()
  },
  "乖離率 BIAS" = function(df) {
    ggplot(df, aes(x = date)) +
      geom_line(aes(y = BIAS6, color = "BIAS6"), linewidth = 0.7) +
      geom_line(aes(y = BIAS12, color = "BIAS12"), linewidth = 0.7) +
      geom_line(aes(y = BIAS24, color = "BIAS24"), linewidth = 0.7) +
      scale_color_manual(values = c(BIAS6 = "#1976d2", BIAS12 = "#f57c00", BIAS24 = "#7b1fa2")) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "乖離率 (%)", color = NULL) + theme_minimal()
  },
  "OBV 線" = function(df) {
    ggplot(df, aes(x = date, y = OBV)) + geom_line(color = "#00796b", linewidth = 0.8) +
      labs(title = "OBV") + theme_minimal()
  },
  "量強弱 VR" = function(df) {
    ggplot(df, aes(x = date, y = VR)) + geom_line(color = "#5d4037", linewidth = 0.8) +
      geom_hline(yintercept = c(70, 250), linetype = "dashed", color = "gray50") +
      labs(title = "VR(26)") + theme_minimal()
  },
  "趨向指標 DMI" = function(df) {
    ggplot(df, aes(x = date)) +
      geom_line(aes(y = PLUS_DI, color = "+DI"), linewidth = 0.7) +
      geom_line(aes(y = MINUS_DI, color = "-DI"), linewidth = 0.7) +
      geom_line(aes(y = ADX, color = "ADX"), linewidth = 0.7) +
      scale_color_manual(values = c("+DI" = "#c23b33", "-DI" = "#1976d2", ADX = "#455a64")) +
      labs(title = "DMI(14)", color = NULL) + theme_minimal()
  },
  "威廉指標 WMS%R" = function(df) {
    ggplot(df, aes(x = date, y = WMSR)) + geom_line(color = "#455a64", linewidth = 0.8) +
      geom_hline(yintercept = c(-20, -80), linetype = "dashed", color = "gray50") +
      labs(title = "WMS%R(14)") + theme_minimal()
  },
  "心理線 PSY" = function(df) {
    ggplot(df, aes(x = date, y = PSY12)) + geom_line(color = "#f57c00", linewidth = 0.8) +
      geom_hline(yintercept = c(25, 75), linetype = "dashed", color = "gray50") +
      labs(title = "PSY(12)") + theme_minimal()
  },
  "寶塔線" = function(df) {
    ggplot(df, aes(x = date)) +
      geom_rect(aes(xmin = date - 0.4, xmax = date + 0.4,
                    ymin = tower_low, ymax = tower_high),
                fill = ifelse(df$tower_up, UP_COLOR, "#222222"),
                color = "gray35", linewidth = 0.2) +
      labs(title = "寶塔線（紅＝多、黑＝空）") + theme_minimal()
  }
)

breadth_chart_funs <- list(
  "ADR（10 日均線）" = function(b) {
    ggplot(b, aes(x = date, y = ADR10)) + geom_line(color = "#1976d2", linewidth = 0.8) +
      geom_hline(yintercept = 100, linetype = "dashed") +
      labs(title = "ADR 漲跌比率（10 日均線，>100 市場偏多）") + theme_minimal()
  },
  "ADL 騰落線" = function(b) {
    ggplot(b, aes(x = date, y = ADL)) + geom_line(color = "#7b1fa2", linewidth = 0.8) +
      labs(title = "ADL 騰落指標（累計漲跌家數差）") + theme_minimal()
  },
  "OBOS 超買超賣" = function(b) {
    ggplot(b, aes(x = date, y = OBOS10)) + geom_line(color = "#00796b", linewidth = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "OBOS（10 日漲跌家數差）") + theme_minimal()
  }
)