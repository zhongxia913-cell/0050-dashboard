# ============================================================
# summary_model.R：懶人總覽標籤、理由產生、比較基準、市場關聯
# ============================================================

# ---------- 1. 四面向標籤 ----------
lab_fin  <- function(s) ifelse(s >= 60, "良好", ifelse(s >= 40, "普通", "偏弱"))
lab_tech <- function(s) ifelse(s >= 60, "偏多", ifelse(s >= 40, "中立", "偏空"))
lab_val <- function(pe, pb) {
  pe <- pe[1]
  pb <- pb[1]
  if (!is.finite(pe) || pe <= 0) return("無資料")
  if (!is.finite(pb)) pb <- 99
  ifelse(pe <= 15 & pb < 2, "偏低",
         ifelse(pe >= 30 | pb > 4, "偏高", "合理"))
}
lab_risk <- function(vol, all_vol) {          # vol=該股年化波動, all_vol=全市場
  q <- mean(all_vol <= vol, na.rm = TRUE)     # 百分位（越大越風險）
  list(label = ifelse(q >= 2/3, "高", ifelse(q >= 1/3, "中", "低")), pct = q)
}

# ---------- 2. 一句話結論（規則模板，不接 AI） ----------
one_liner <- function(fin_l, tech_l, risk_l) {
  if (fin_l == "良好" & tech_l == "偏多" & risk_l == "低")
    return("基本面與技術面表現良好，目前風險相對較低。")
  if (fin_l == "良好" & tech_l == "偏多")
    return("基本面與技術趨勢偏正向，但近期波動較高，應注意短期回檔風險。")
  if (tech_l == "偏空" & fin_l == "良好")
    return("基本面穩健，但技術面轉弱，可等待趨勢止穩後再評估。")
  if (fin_l == "偏弱" & tech_l == "偏空")
    return("基本面與技術面皆偏弱，建議觀望。")
  paste0("基本面", fin_l, "、技術面", tech_l, "、風險", risk_l,
         "，建議搭配詳細圖表進一步確認。")
}

# ---------- 3. 支持原因 ✓ 與注意事項 △ ----------
make_reasons <- function(tech_last, ratios, peer_avg) {
  sup <- character(); cau <- character()
  add <- function(ok, txt_sup, txt_cau = NULL) {
    ok <- ok[1]
    if (isTRUE(ok)) sup <<- c(sup, txt_sup)
    else if (!is.null(txt_cau) && !is.na(ok)) cau <<- c(cau, txt_cau)
  }
  add(ratios["roe"] > peer_avg$roe, "ROE 高於 0050 成分股平均", "ROE 低於成分股平均")
  add(ratios["revenue_growth"] > 0, "營收維持正成長", "營收成長轉弱")
  add(tech_last$close > tech_last$MA20, "股價位於中期均線（MA20）之上", "股價跌破 MA20")
  add(tech_last$DIF > tech_last$DEA, "MACD 為正（DIF 高於 DEA）", "MACD 轉弱")
  if (is.finite(tech_last$RSI14)) {
    if (tech_last$RSI14 > 70) cau <- c(cau, "RSI 接近過熱區")
    if (tech_last$RSI14 < 30) sup <- c(sup, "RSI 處於超賣區，留意反彈")
  }
  add(tech_last$ann_vol < peer_avg$ann_vol, "波動度低於成分股平均",
      "近期波動度高於平均")
  list(support = sup, caution = cau)
}

# ---------- 4. 全市場橫截面統計（比較基準的核心） ----------
cross_section_stats <- function(data, start_date, end_date) {
  data %>%
    filter(symbol != "0050", date >= start_date, date <= end_date) %>%
    group_by(symbol) %>%
    arrange(date, .by_group = TRUE) %>%
    summarise(
      n_days   = n(),
      ann_ret  = annualized_return(first(adjusted), last(adjusted), n()),
      ann_vol  = sd(log_return, na.rm = TRUE) * sqrt(TRADING_DAYS_PER_YEAR),
      sharpe   = ifelse(ann_vol > 0,
                        mean(log_return, na.rm = TRUE) *
                          TRADING_DAYS_PER_YEAR / ann_vol, NA_real_),
      max_dd   = min(drawdown_value(adjusted), na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    filter(n_days >= 60)            # 藥華藥等短樣本股票不列入比較
}

# 排名與百分位文字：higher_better 控制方向
rank_text <- function(value, all_values, higher_better = TRUE) {
  all_values <- all_values[is.finite(all_values)]
  if (!is.finite(value) || length(all_values) < 5) return("無資料")
  r <- if (higher_better) mean(all_values >= value) else mean(all_values <= value)
  paste0("前 ", max(1, round(r * 100)), "%（", length(all_values), " 檔中）")
}

# 評價文字（較佳/較高/較低）
eval_text <- function(value, avg, higher_better = TRUE) {
  if (!is.finite(value) || !is.finite(avg)) return("—")
  if (higher_better) ifelse(value >= avg, "較佳", "較差")
  else               ifelse(value <= avg, "較低", "較高")
}

# ---------- 5. 市場關聯 ----------
cor_label <- function(r) {
  ifelse(r >= 0.7, "高度正相關",
         ifelse(r >= 0.4, "中度正相關",
                ifelse(r >= 0, "低度相關", "負相關")))
}

# 該股 vs 全市場的相關係數（回傳與0050、最高3、最低3）
market_correlations <- function(data, symbol, start_date, end_date) {
  wide <- data %>%
    filter(date >= start_date, date <= end_date) %>%
    select(date, symbol, log_return) %>%
    tidyr::pivot_wider(names_from = symbol, values_from = log_return)
  target <- wide[[symbol]]
  others <- setdiff(names(wide), c("date", symbol))
  cors <- vapply(others, function(s)
    cor(target, wide[[s]], use = "pairwise.complete.obs"), numeric(1))
  cors <- cors[is.finite(cors)]
  list(
    cor_0050 = unname(cors["0050"]),
    top = sort(cors[names(cors) != "0050"], decreasing = TRUE)[1:3],
    bottom = sort(cors[names(cors) != "0050"], decreasing = FALSE)[1:3]
  )
}

# ---------- 6. 投資組合分散效果 ----------
portfolio_cor_stats <- function(cor_mat) {
  off <- cor_mat[upper.tri(cor_mat)]
  avg <- mean(off, na.rm = TRUE)
  idx <- which(upper.tri(cor_mat), arr.ind = TRUE)
  pairs <- data.frame(
    s1 = rownames(cor_mat)[idx[, 1]],
    s2 = colnames(cor_mat)[idx[, 2]],
    cor = off
  )
  list(
    avg = avg,
    label = ifelse(avg < 0.4, "良好", ifelse(avg <= 0.6, "普通", "差")),
    max_pair = pairs[which.max(pairs$cor), ],
    min_pair = pairs[which.min(pairs$cor), ]
  )
}