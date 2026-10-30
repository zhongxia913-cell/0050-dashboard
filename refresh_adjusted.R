# ============================================================
# refresh_adjusted.R
# 用途：全歷史重抓 adjusted，修正「除權息造成 Yahoo 回溯調整」的落差
#
# 為什麼需要：
#   update_data.R 只抓最近 10 天，且去重時保留舊資料。
#   但 Yahoo 的 adjusted 是「以今天為基準」回溯計算的，
#   一旦某檔除權息，整段歷史 adjusted 都會被改寫。
#   只做增量更新時，舊資料停留在舊基準、新資料是新基準，
#   兩段接縫會出現一個「假跳空」，直接污染報酬率與所有分數。
#
# 這支腳本只改 adjusted 欄位，不動 OHLCV、不新增/刪除任何列。
# ============================================================

suppressPackageStartupMessages({
  library(tidyquant)
  library(dplyr)
  library(purrr)
})

FILE <- "data/stock_data.rds"
if (!file.exists(FILE)) stop("找不到 ", FILE)

old <- readRDS(FILE)
old$date <- as.Date(old$date)

syms  <- sort(unique(old$symbol))
start <- min(old$date, na.rm = TRUE)
cat(sprintf("共 %d 檔，起點 %s\n", length(syms), format(start)))

get_one <- function(s) {
  x <- try(
    tidyquant::tq_get(paste0(s, ".TW"), get = "stock.prices",
                      from = start - 10, to = Sys.Date() + 1),
    silent = TRUE
  )
  if (inherits(x, "try-error") || is.null(x) || nrow(x) == 0) {
    message("跳過（無資料）：", s)
    return(NULL)
  }
  data.frame(
    symbol   = s,
    date     = as.Date(x$date),
    adjusted = as.numeric(x$adjusted),
    stringsAsFactors = FALSE
  )
}

new <- purrr::map_dfr(syms, get_one)
if (nrow(new) == 0) stop("沒有取得任何資料，中止（原檔未修改）")

new <- new[!is.na(new$adjusted) & new$adjusted > 0, ]

key_old <- paste(old$symbol, format(old$date))
key_new <- paste(new$symbol, format(new$date))

idx <- match(key_old, key_new)

before <- old$adjusted
old$adjusted <- ifelse(is.na(idx), before, new$adjusted[idx])

chg <- abs(old$adjusted / before - 1)
n_chg <- sum(is.finite(chg) & chg > 1e-6)
pct   <- n_chg / nrow(old)

cat(sprintf("比對 %d 筆，其中 %d 筆 adjusted 有變動（%.2f%%）\n",
            nrow(old), n_chg, 100 * pct))

# ---- 安全閥 ----
if (any(!is.finite(old$adjusted))) stop("adjusted 出現非有限值，中止（原檔未修改）")
if (nrow(old) != length(before))    stop("列數改變，中止（原檔未修改）")
if (pct > 0.90) stop("變動比例超過 90%，疑似資料異常，中止（原檔未修改）")

# ---- 印出變動最大的 5 筆，方便人工檢查 ----
top <- order(-ifelse(is.finite(chg), chg, 0))[1:min(5, length(chg))]
cat("\n變動最大的 5 筆：\n")
print(data.frame(
  symbol = old$symbol[top],
  date   = format(old$date[top]),
  before = round(before[top], 4),
  after  = round(old$adjusted[top], 4),
  pct    = sprintf("%+.2f%%", 100 * chg[top])
), row.names = FALSE)

saveRDS(old, FILE)
cat("\n已寫回 ", FILE, "\n")
