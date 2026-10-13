# ============================================================
# update_financial.R：抓取 Yahoo Finance 財務資料 (強化防呆版)
# ============================================================
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

COOKIE_JAR <- tempfile()   # 全脚本共用的 Cookie 保存檔
UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"

PROJECT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
DATA_DIR <- file.path(PROJECT_DIR, "data")
FIN_FILE <- file.path(DATA_DIR, "financial_data.rds")

stock_codes <- c(
  "2330","2454","2308","2317","3711","2303","2383","3037","2881","2891",
  "1303","3017","2882","2887","2345","2382","2327","2360","2885","2884",
  "2059","2408","6669","2357","2883","2886","3008","2890","3443","3231",
  "2301","2344","2412","3653","2880","2892","3665","1216","4958","6446",
  "7769","2368","2449","2395","5880","8046","2603","4904","3045","6505"
)

get_yahoo_crumb <- function() {
  for (dom in c("https://fc.yahoo.com", "https://finance.yahoo.com")) {
    request(dom) %>%
      req_user_agent(UA) %>%
      req_cookie_preserve(COOKIE_JAR) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
  }
  for (url in c("https://query1.finance.yahoo.com/v1/test/getcrumb",
                "https://query2.finance.yahoo.com/v1/test/getcrumb")) {
    resp <- request(url) %>%
      req_user_agent(UA) %>%
      req_cookie_preserve(COOKIE_JAR) %>%
      req_timeout(30) %>%
      req_error(is_error = function(resp) FALSE) %>%
      req_perform()
    if (resp_status(resp) == 200) {
      crumb <- resp_body_string(resp)
      if (nchar(crumb) > 0 && nchar(crumb) < 20) return(crumb)
    }
  }
  ""
}

safe_num <- function(x) {
  # Yahoo 數值欄位為 list(raw = ..., fmt = ...)，需取 raw
  if (is.list(x)) x <- x$raw
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || any(!is.finite(x))) NA_real_ else x[1]
}

fetch_one_stock <- function(code, crumb) {
  sym <- paste0(code, ".TW")
  url <- paste0(
    "https://query1.finance.yahoo.com/v10/finance/quoteSummary/", sym,
    "?modules=defaultKeyStatistics,summaryDetail,",
    "balanceSheetHistory,balanceSheetHistoryQuarterly,",
    "incomeStatementHistory,cashflowStatementHistory",
    "&crumb=", utils::URLencode(crumb, reserved = TRUE)
  )
  j <- request(url) %>%
    req_user_agent(UA) %>%
    req_cookie_preserve(COOKIE_JAR) %>%
    req_retry(max_tries = 3, max_seconds = 30) %>%
    req_timeout(30) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = FALSE)
  
  q   <- j$quoteSummary$result[[1]]
  bs  <- q$balanceSheetHistory$balanceSheetStatements[[1]]
  # 年度資產負債表缺失時（金融業常見），改用最近一季資產負債表
  if (is.null(bs) || is.null(bs$totalAssets)) {
    bs <- q$balanceSheetHistoryQuarterly$balanceSheetStatements[[1]]
  }
  ic  <- q$incomeStatementHistory$incomeStatementHistory[[1]]
  cf  <- q$cashflowStatementHistory$cashflowStatements[[1]]
  ks  <- q$defaultKeyStatistics
  sd  <- q$summaryDetail
  fdm <- q$financialData          # 新增：Yahoo 統計頁來源
  
  out <- list(
    ## 原始報表欄位（台股可能為空）
    totalCurrentAssets    = safe_num(bs$totalCurrentAssets),
    totalCurrentLiabilities = safe_num(bs$totalCurrentLiabilities),
    inventory             = safe_num(bs$inventory),
    netReceivables        = safe_num(bs$netReceivables),
    totalLiab             = safe_num(bs$totalLiab),
    totalEquity           = safe_num(bs$totalStockholderEquity),
    longTermDebt          = safe_num(bs$longTermDebt),
    totalAssets           = safe_num(bs$totalAssets),
    totalRevenue          = safe_num(ic$totalRevenue),
    costOfRevenue         = safe_num(ic$costOfRevenue),
    grossProfit           = safe_num(ic$grossProfit),
    operatingIncome       = safe_num(ic$operatingIncome),
    netIncome             = safe_num(ic$netIncome),
    operatingCashflow     = safe_num(cf$operatingCashflow),
    trailingPE            = safe_num(ks$trailingPE),
    priceToBook           = safe_num(ks$priceToBook),
    dividendYield         = safe_num(sd$dividendYield) * 100,
    pe_sd                 = safe_num(sd$trailingPE),
    pb_sd                 = safe_num(sd$priceToBook),
    equity_growth         = NA_real_,
    revenue_growth        = NA_real_,
    ## financialData 備援比率（注意：比率類為小數，需 ×100；debtToEquity 本身為 %）
    fd_current_ratio   = safe_num(fdm$currentRatio) * 100,
    fd_quick_ratio     = safe_num(fdm$quickRatio) * 100,
    fd_debt_equity     = safe_num(fdm$debtToEquity),
    fd_roe             = safe_num(fdm$returnOnEquity) * 100,
    fd_roa             = safe_num(fdm$returnOnAssets) * 100,
    fd_gross_margin    = safe_num(fdm$grossMargins) * 100,
    fd_oper_margin     = safe_num(fdm$operatingMargins) * 100,
    fd_net_margin      = safe_num(fdm$profitMargins) * 100,
    fd_revenue_growth  = safe_num(fdm$revenueGrowth) * 100,
    fd_earnings_growth = safe_num(fdm$earningsGrowth) * 100
  )
  core <- c(out$totalRevenue, out$netIncome, out$fd_roe)
  if (all(is.na(core))) stop("Yahoo 回傳空資料：", code)
  out
}

cat("開始抓取財務資料...\n")
crumb <- tryCatch(get_yahoo_crumb(), error = function(e) "")

if (crumb == "") {
  warning("⚠️ 無法取得 Yahoo Crumb。這通常是因為 Yahoo 封鎖了雲端環境（如 Posit Cloud）的 IP。")
  cat("系統將產生『骨架版』財務資料，讓 Dashboard 可以正常啟動，但財務數值將顯示為 NA。\n")
  cat("💡 解決方案：請在您的【本地電腦】RStudio 執行此腳本，成功後再將 data/financial_data.rds 上傳或 Commit。\n")
  
  empty_fin <- list()
  for (code in stock_codes) {
    empty_fin[[code]] <- list(
      totalCurrentAssets = NA_real_, totalCurrentLiabilities = NA_real_, inventory = NA_real_,
      netReceivables = NA_real_, totalLiab = NA_real_, totalEquity = NA_real_,
      longTermDebt = NA_real_, totalAssets = NA_real_, totalRevenue = NA_real_,
      costOfRevenue = NA_real_, grossProfit = NA_real_, operatingIncome = NA_real_,
      ebit = NA_real_, interestExpense = NA_real_, netIncome = NA_real_,
      operatingCashflow = NA_real_, trailingPE = NA_real_, priceToBook = NA_real_,
      dividendYield = NA_real_, equity_growth = NA_real_, revenue_growth = NA_real_
    )
  }
  fin_data <- list(
    updated_at = format(Sys.time(), tz = "Asia/Taipei", format = "%Y-%m-%d %H:%M:%S"),
    stocks = empty_fin,
    failures = stock_codes
  )
  saveRDS(fin_data, FIN_FILE)
  cat("骨架版財務資料已儲存。Dashboard 可正常運行。\n")
  
} else {
  results <- list()
  failures <- character()
  for (code in stock_codes) {
    cat("抓取：", code, "\n")
    res <- tryCatch(fetch_one_stock(code, crumb), error = function(e) NULL)
    if (is.null(res)) failures <- c(failures, code) else results[[code]] <- res
    Sys.sleep(1.5) 
  }
  
  fin_data <- list(
    updated_at = format(Sys.time(), tz = "Asia/Taipei", format = "%Y-%m-%d %H:%M:%S"),
    stocks = results,
    failures = failures
  )
  saveRDS(fin_data, FIN_FILE)
  cat("財務資料更新完成：成功 ", length(results), " 檔，失敗 ", length(failures), " 檔\n")
}