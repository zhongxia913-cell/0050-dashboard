# ============================================================
# indicators_financial.R：財務比率計算與中繼資料
# ============================================================

safe_div <- function(a, b) {
  if (length(a) == 0 || length(b) == 0) return(NA_real_)
  a <- as.numeric(a); b <- as.numeric(b)
  # 【徹底修正】使用 any() 確保 || 兩側為長度 1
  if (any(!is.finite(a)) || any(!is.finite(b)) || any(b == 0)) {
    return(NA_real_)
  }
  a[1] / b[1]
}

compute_fin_ratios <- function(l, symbol = NULL) {
  pick <- function(...) {
    vals <- c(...)
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0) vals[1] else NA_real_
  }
  num1 <- function(x) {
    x <- as.numeric(x)
    if (length(x) == 0) NA_real_ else x[1]
  }
  
  ## 由負債權益比反推負債比率 / 權益比率
  de <- num1(l$fd_debt_equity)
  debt_fb <- if (is.finite(de)) (de / 100) / (1 + de / 100) * 100 else NA_real_
  equity_fb <- if (is.finite(debt_fb)) 100 - debt_fb else NA_real_
  
  ## 由 P/B 與 ROE 反推本益比：PE = PB / ROE
  pb_v <- num1(l$priceToBook)
  roe_v <- num1(l$fd_roe)
  pe_fb <- if (is.finite(pb_v) && is.finite(roe_v) && roe_v > 0) {
    pb_v / (roe_v / 100)
  } else NA_real_
  
  out <- c(
    current_ratio  = pick(l$fd_current_ratio,
                          safe_div(l$totalCurrentAssets, l$totalCurrentLiabilities) * 100),
    quick_ratio    = pick(l$fd_quick_ratio,
                          safe_div(l$totalCurrentAssets - l$inventory, l$totalCurrentLiabilities) * 100),
    debt_ratio     = pick(safe_div(l$totalLiab, l$totalAssets) * 100, debt_fb),
    equity_ratio   = pick(safe_div(l$totalEquity, l$totalAssets) * 100, equity_fb),
    roe            = pick(safe_div(l$netIncome, l$totalEquity) * 100, l$fd_roe),
    roa            = pick(safe_div(l$netIncome, l$totalAssets) * 100, l$fd_roa),
    gross_margin   = pick(l$fd_gross_margin,
                          safe_div(l$grossProfit, l$totalRevenue) * 100),
    oper_margin    = pick(l$fd_oper_margin,
                          safe_div(l$operatingIncome, l$totalRevenue) * 100),
    net_margin     = pick(safe_div(l$netIncome, l$totalRevenue) * 100, l$fd_net_margin),
    recv_days      = pick(365 / safe_div(l$totalRevenue, l$netReceivables)),
    inv_days       = pick(365 / safe_div(l$costOfRevenue, l$inventory)),
    oper_cycle     = pick(365 / safe_div(l$totalRevenue, l$netReceivables) +
                            365 / safe_div(l$costOfRevenue, l$inventory)),
    equity_turn    = pick(safe_div(l$totalRevenue, l$totalEquity)),
    cf_to_cl       = pick(safe_div(l$operatingCashflow, l$totalCurrentLiabilities) * 100),
    pe             = pick(l$trailingPE, l$pe_sd, pe_fb),
    pb             = pick(l$priceToBook, l$pb_sd),
    div_yield      = pick(l$dividendYield),
    equity_growth  = pick(l$equity_growth, l$fd_earnings_growth),
    revenue_growth = pick(l$revenue_growth, l$fd_revenue_growth)
  )
  
  # 金融保險業：流動比率、速動比率、毛利率不適用（產業慣例）
  if (!is.null(symbol) &&
      identical(get_stock_industry_one(symbol), "金融保險")) {
    out["current_ratio"] <- NA_real_
    out["quick_ratio"]   <- NA_real_
    out["gross_margin"]  <- NA_real_
  }
  out
}

ratio_meta <- data.frame(
  key = c("current_ratio","quick_ratio","debt_ratio","equity_ratio",
          "roe","roa","gross_margin","oper_margin","net_margin","pe","pb","div_yield"),
  name = c("流動比率","速動比率","負債比率","權益比率",
           "權益報酬率","總資產報酬率","毛利率","營業利益率","淨利率","本益比","股價淨值比","股利收益率"),
  unit = c("%","%","%","%",
           "%","%","%","%","%","倍","倍","%"),
  group = c("償債能力","償債能力","財務槓桿","財務槓桿",
            "獲利能力","獲利能力","獲利能力","獲利能力","獲利能力","市場評價","市場評價","市場評價"),
  stringsAsFactors = FALSE
)