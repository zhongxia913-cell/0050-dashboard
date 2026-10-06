# ============================================================
# 0050 Investment Research Dashboard
# 第 1 段：資料、共用函數、UI
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
source("indicators_technical.R", local = TRUE)
source("indicators_financial.R", local = TRUE)
source("score_model.R", local = TRUE)          

options(scipen = 999)
# ============================================================
# 1. 基本設定
# ============================================================

DATA_FILE <- "data/stock_data.rds"

RESEARCH_START_DATE <- as.Date("2016-01-04")
RESEARCH_END_DATE   <- as.Date("2025-12-31")

TRADING_DAYS_PER_YEAR <- 252
MAX_SELECTED_STOCKS <- 5
TRADE_UNIT <- 1


# ============================================================
# 2. 0050 成分股資料
# ============================================================

stock_info <- data.frame(
  
  symbol = c(
    "2330","2454","2308","2317","3711",
    "2303","2383","3037","2881","2891",
    "1303","3017","2882","2887","2345",
    "2382","2327","2360","2885","2884",
    "2059","2408","6669","2357","2883",
    "2886","3008","2890","3443","3231",
    "2301","2344","2412","3653","2880",
    "2892","3665","1216","4958","6446",
    "7769","2368","2449","2395","5880",
    "8046","2603","4904","3045","6505"
  ),
  
  name = c(
    "台積電","聯發科","台達電","鴻海","日月光投控",
    "聯電","台光電","欣興","富邦金","中信金",
    "南亞","奇鋐","國泰金","台新新光金","智邦",
    "廣達","國巨","致茂","元大金","玉山金",
    "川湖","南亞科","緯穎","華碩","凱基金",
    "兆豐金","大立光","永豐金","創意","緯創",
    "光寶科","華邦電","中華電","健策","華南金",
    "第一金","貿聯-KY","統一","臻鼎-KY","藥華藥",
    "鴻勁","金像電","京元電子","研華","合庫金",
    "南電","長榮","遠傳","台灣大","台塑化"
  ),
  
  industry = c(
    "半導體業","半導體業","電子零組件業","其他電子業","半導體業",
    "半導體業","電子零組件業","電子零組件業","金融保險","金融保險",
    "塑膠工業","電腦及週邊設備業","金融保險","金融保險","通信網路業",
    "電腦及週邊設備業","電子零組件業","其他電子業","金融保險","金融保險",
    "電子零組件業","半導體業","電腦及週邊設備業","電腦及週邊設備業","金融保險",
    "金融保險","其他電子業","金融保險","半導體業","電腦及週邊設備業",
    "電腦及週邊設備業","半導體業","通信網路業","電子零組件業","金融保險",
    "金融保險","其他電子業","食品工業","電子零組件業","生技醫療業",
    "半導體業","電子零組件業","半導體業","電腦及週邊設備業","金融保險",
    "電子零組件業","航運業","通信網路業","通信網路業","油電燃氣業"
  ),
  
  stringsAsFactors = FALSE
)

benchmark_info <- data.frame(
  symbol = "0050",
  name = "元大台灣50",
  industry = "Benchmark",
  stringsAsFactors = FALSE
)
# ============================================================
# 產業分類：少於 4 檔的產業合併為「其他」
# ============================================================

industry_count <- table(
  stock_info$industry
)

major_industries <- names(
  industry_count[
    industry_count >= 4
  ]
)

industry_group <- function(x) {
  
  ifelse(
    x %in% major_industries,
    x,
    "其他"
  )
  
}

# ============================================================
# 3. 共用函數
# ============================================================

# ============================================================
# 股票名稱 / 產業 / 顯示標籤
# 支援單一股票，也支援整個向量
# ============================================================

get_stock_name_one <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  if (
    length(symbol) == 0 ||
    is.na(symbol)
  ) {
    return(NA_character_)
  }
  
  if (symbol == "0050") {
    return("元大台灣50")
  }
  
  result <- stock_info$name[
    match(
      symbol,
      stock_info$symbol
    )
  ]
  
  if (
    length(result) == 0 ||
    is.na(result)
  ) {
    return(symbol)
  }
  
  result
}


get_stock_name <- function(symbol) {
  
  vapply(
    as.character(symbol),
    get_stock_name_one,
    character(1)
  )
  
}


get_stock_industry_one <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  if (
    length(symbol) == 0 ||
    is.na(symbol)
  ) {
    return(NA_character_)
  }
  
  if (symbol == "0050") {
    return("Benchmark")
  }
  
  result <- stock_info$industry[
    match(
      symbol,
      stock_info$symbol
    )
  ]
  
  if (
    length(result) == 0 ||
    is.na(result)
  ) {
    return("其他")
  }
  
  result
}


get_stock_industry <- function(symbol) {
  
  vapply(
    as.character(symbol),
    get_stock_industry_one,
    character(1)
  )
  
}


get_stock_label <- function(symbol) {
  
  symbol <- as.character(symbol)
  
  paste0(
    get_stock_name(symbol),
    " (",
    symbol,
    ")"
  )
  
}


convert_to_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    
    finite_x <- x[is.finite(x)]
    
    if (
      length(finite_x) > 0 &&
      min(finite_x) >= 10000 &&
      max(finite_x) <= 60000
    ) {
      
      return(
        as.Date(
          x,
          origin = "1899-12-30"
        )
      )
    }
  }
  
  as.Date(x)
}


safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}


fmt_price <- function(x) {
  ifelse(
    is.finite(x),
    sprintf("%.2f", x),
    "NA"
  )
}


fmt_num <- function(x, digits = 0) {
  
  ifelse(
    is.finite(x),
    formatC(
      x,
      format = "f",
      digits = digits,
      big.mark = ","
    ),
    "NA"
  )
}

fmt_pct <- function(
    x,
    digits = 2,
    show_sign = TRUE
) {
  
  if (show_sign) {
    
    ifelse(
      is.finite(x),
      sprintf(
        paste0("%+.", digits, "f%%"),
        x * 100
      ),
      "NA"
    )
    
  } else {
    
    ifelse(
      is.finite(x),
      sprintf(
        paste0("%.", digits, "f%%"),
        x * 100
      ),
      "NA"
    )
  }
}


annualized_return <- function(
    start_value,
    end_value,
    trading_days
) {
  
  if (
    !is.finite(start_value) ||
    !is.finite(end_value) ||
    start_value <= 0 ||
    end_value <= 0 ||
    trading_days <= 1
  ) {
    
    return(NA_real_)
  }
  
  (
    end_value / start_value
  ) ^ (
    TRADING_DAYS_PER_YEAR /
      trading_days
  ) - 1
}


drawdown_value <- function(x) {
  
  x <- as.numeric(x)
  
  high <- cummax(x)
  
  x / high - 1
}


nearest_date <- function(
    dates,
    target
) {
  
  dates <- sort(unique(as.Date(dates)))
  target <- as.Date(target)
  
  if (
    length(dates) == 0 ||
    is.na(target)
  ) {
    return(NA)
  }
  
  dates[
    which.min(
      abs(
        as.numeric(dates - target)
      )
    )
  ]
}


# ============================================================
# 4. 資料整理函數
# ============================================================

prepare_stock_data <- function(raw_data) {
  
  if (!is.data.frame(raw_data)) {
    stop("stock_data.rds 不是 data.frame。")
  }
  
  required <- c(
    "date",
    "symbol"
  )
  
  missing_cols <-
    setdiff(
      required,
      names(raw_data)
    )
  
  if (length(missing_cols) > 0) {
    
    stop(
      paste0(
        "資料缺少必要欄位：",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
  }
  
  x <- raw_data
  
  
  # ----------------------------------------------------------
  # 日期與股票代號
  # ----------------------------------------------------------
  
  x$date <- convert_to_date(
    x$date
  )
  
  if (is.numeric(x$symbol)) {
    
    x$symbol <- sprintf(
      "%04d",
      as.integer(x$symbol)
    )
    
  } else {
    
    x$symbol <- as.character(
      x$symbol
    )
  }
  
  
  # ----------------------------------------------------------
  # 調整後價格
  # ----------------------------------------------------------
  
  if ("adjusted" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$adjusted
    )
    
  } else if ("price.adjusted" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$price.adjusted
    )
    
  } else if ("close" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$close
    )
    
  } else if ("price.close" %in% names(x)) {
    
    x$adjusted <- safe_numeric(
      x$price.close
    )
    
  } else {
    
    stop(
      "找不到 adjusted / price.adjusted / close / price.close。"
    )
  }
  
  
  # ----------------------------------------------------------
  # 普通收盤價
  # ----------------------------------------------------------
  
  if ("close" %in% names(x)) {
    
    x$close <- safe_numeric(
      x$close
    )
    
  } else if ("price.close" %in% names(x)) {
    
    x$close <- safe_numeric(
      x$price.close
    )
    
  } else {
    
    x$close <- x$adjusted
  }
  
  
  # ----------------------------------------------------------
  # 成交量
  # ----------------------------------------------------------
  
  if ("volume" %in% names(x)) {
    
    x$volume <- safe_numeric(
      x$volume
    )
    
  } else if ("price.volume" %in% names(x)) {
    
    x$volume <- safe_numeric(
      x$price.volume
    )
    
  } else {
    
    x$volume <- NA_real_
  }
  
  
  # ----------------------------------------------------------
  # 股票名稱與產業
  # ----------------------------------------------------------
  
  x$name <- vapply(
    x$symbol,
    get_stock_name,
    character(1)
  )
  
  x$industry <- vapply(
    x$symbol,
    get_stock_industry,
    character(1)
  )
  
  x$industry_group <-
    ifelse(
      x$industry %in%
        major_industries,
      x$industry,
      "其他"
    )
  
  
  # ----------------------------------------------------------
  # 基本清理
  # ----------------------------------------------------------
  
  x <- x %>%
    filter(
      !is.na(date),
      !is.na(symbol)
    ) %>%
    arrange(
      symbol,
      date
    ) %>%
    distinct(
      symbol,
      date,
      .keep_all = TRUE
    ) %>%
    mutate(
      adjusted = ifelse(
        is.finite(adjusted) &
          adjusted > 0,
        adjusted,
        NA_real_
      ),
      close = ifelse(
        is.finite(close) &
          close > 0,
        close,
        NA_real_
      )
    )
  
  
  # ----------------------------------------------------------
  # 日報酬
  # ----------------------------------------------------------
  
  x <- x %>%
    group_by(symbol) %>%
    arrange(
      date,
      .by_group = TRUE
    ) %>%
    mutate(
      
      daily_return =
        adjusted /
        lag(adjusted) - 1,
      
      log_return =
        log(
          adjusted /
            lag(adjusted)
        )
      
    ) %>%
    ungroup() %>%
    arrange(
      symbol,
      date
    )
  
  
  if (!"0050" %in% x$symbol) {
    stop("資料中找不到 0050。")
  }
  
  x
}


# ============================================================
# 5. 載入初始資料
# ============================================================

if (!file.exists(DATA_FILE)) {
  
  stop(
    paste0(
      "找不到資料檔：",
      DATA_FILE,
      "\n請確認 data/stock_data.rds 存在。"
    )
  )
}

initial_data <- prepare_stock_data(
  readRDS(DATA_FILE)
)

data_min_date <- min(
  initial_data$date,
  na.rm = TRUE
)

data_max_date <- max(
  initial_data$date,
  na.rm = TRUE
)

available_symbols <- sort(
  unique(
    initial_data$symbol
  )
)

dashboard_symbols <- c(
  "0050",
  stock_info$symbol
)

dashboard_symbols <-
  dashboard_symbols[
    dashboard_symbols %in%
      available_symbols
  ]

dashboard_stock_choices <-
  setNames(
    dashboard_symbols,
    vapply(
      dashboard_symbols,
      get_stock_label,
      character(1)
    )
  )
portfolio_default_end_date <- data_max_date

portfolio_default_start_date <- max(
  data_min_date,
  data_max_date - 365
)

# ============================================================
# 共用顏色
# ============================================================

UP_COLOR <- "#C23B33"
DOWN_COLOR <- "#39FF14"
NEUTRAL_COLOR <- "#777777"

# ============================================================
# 代表色系統
# ============================================================

# 0050 固定使用紅色，其餘股票依固定順序產生代表色。
# 因此只要股票代號相同，同一檔股票在所有圖形都會維持相同顏色。

other_stock_symbols <-
  setdiff(
    dashboard_symbols,
    "0050"
  )

stock_colors <- c(
  "0050" = "red"
)

if (
  length(other_stock_symbols) > 0
) {
  
  stock_colors[
    other_stock_symbols
  ] <-
    hcl.colors(
      length(other_stock_symbols),
      palette = "Dark 3"
    )
  
}


# ------------------------------------------------------------
# 產業分類
#
# 少於 4 檔成分股的產業全部合併為「其他」。
# ------------------------------------------------------------

stock_info$industry_group <- industry_group(
  stock_info$industry
)

available_industries <- sort(
  unique(
    stock_info$industry_group
  )
)

# ============================================================
# 產業重新分組
# 少於 4 檔的產業全部合併為「其他」
# ============================================================

industry_count <-
  table(
    stock_info$industry
  )

major_industries <-
  names(
    industry_count[
      industry_count >= 4
    ]
  )

stock_info$industry_group <-
  ifelse(
    stock_info$industry %in%
      major_industries,
    stock_info$industry,
    "其他"
  )

available_industries <-
  sort(
    unique(
      stock_info$industry_group
    )
  )


# ------------------------------------------------------------
# 產業代表色
# ------------------------------------------------------------

industry_colors <-
  setNames(
    hcl.colors(
      length(available_industries),
      palette = "Dark 3"
    ),
    available_industries
  )

if (
  "其他" %in% names(industry_colors)
) {
  
  industry_colors[
    "其他"
  ] <- "gray45"
  
}


# ============================================================
# 6. 共用產業
# ============================================================

available_industries <- sort(
  unique(
    stock_info$industry_group
  )
)

# ============================================================
# 7. UI
# ============================================================

ui <- fluidPage(
  
  lang = "zh-Hant",
  
  tags$head(
    
    tags$style(
      
      HTML(
        
        "
        body {
          background:#f5f7fa;
          color:#263238;
          font-family:
            -apple-system,
            BlinkMacSystemFont,
            'Segoe UI',
            'Microsoft JhengHei',
            sans-serif;
        }

        .container-fluid {
          padding:24px 30px 35px 30px;
        }

        .dashboard-title {
          font-size:30px;
          font-weight:700;
          margin-bottom:4px;
        }

        .dashboard-subtitle {
          color:#607d8b;
          margin-bottom:20px;
        }

        .nav-tabs {
          margin-bottom:20px;
        }

        .nav-tabs > li > a {
          font-weight:600;
          color:#455a64;
        }

        .section-box {
          background:#ffffff;
          border:1px solid #e0e6eb;
          border-radius:12px;
          padding:24px;
          margin-bottom:20px;
        }

        .page-description {
          color:#546e7a;
          line-height:1.7;
        }

        .reading-guide {
          background:#f1f7ff;
          border-left:5px solid #1976d2;
          padding:14px 18px;
          border-radius:6px;
          margin:15px 0 20px 0;
          line-height:1.7;
        }

        .plot-explanation {
          background:#fafafa;
          border:1px solid #e3e7eb;
          border-radius:8px;
          padding:13px 16px;
          margin:10px 0 20px 0;
          line-height:1.7;
          color:#455a64;
        }

        .info-card {
          background:#ffffff;
          border:1px solid #e0e6eb;
          border-radius:10px;
          padding:15px;
          min-height:105px;
          margin-bottom:15px;
        }

        .info-card-title {
          color:#607d8b;
          font-size:14px;
          margin-bottom:8px;
        }

        .info-card-value {
          font-size:24px;
          font-weight:700;
        }

        .data-status {
          background:#f8fafc;
          border:1px solid #dde5eb;
          border-radius:8px;
          padding:15px;
          line-height:1.8;
        }

        .warning-note {
          background:#fff8e1;
          border-left:5px solid #f9a825;
          padding:13px 16px;
          border-radius:6px;
          margin:10px 0;
          line-height:1.7;
        }

        .quick-button-row {
          margin:10px 0 15px 0;
        }

        .quick-button-row .btn {
          margin-right:5px;
          margin-bottom:6px;
        }

        .portfolio-trade-box {
          background:#f8fafc;
          border:1px solid #e1e7eb;
          border-radius:10px;
          padding:18px;
          margin-bottom:15px;
        }

        .dashboard-footer {
          text-align:center;
          color:#90a4ae;
          font-size:13px;
          padding-top:20px;
        }
        "
        
      )
    )
  ),
  
  
  div(
    class = "dashboard-title",
    "0050 投資組合研究 Dashboard"
  ),
  
  div(
    class = "dashboard-subtitle",
    "市場監控、個股分析、產業比較、相關性分析與投資組合模擬"
  ),
  
  
  # ==========================================================
  # 主分頁
  # ==========================================================
  
  tabsetPanel(
    
    id = "main_tabs",
    
    
    # ========================================================
    # 1. 市場總覽
    # ========================================================
    
    tabPanel(
      
      "市場總覽",
      
      div(
        class = "section-box",
        
        h2("市場總覽"),
        
        p(
          class = "page-description",
          "快速掌握目前資料狀態、0050 市場表現，以及 50 檔成分股近期漲跌情況。"
        ),
        
        div(
          class = "reading-guide",
          strong("怎麼看？ "),
          "先看資料日期，再看 0050 的價格與最近一日報酬；下方排名則用來觀察成分股近期相對強弱。"
        ),
        
        h3("資料狀態"),
        
        div(
          class = "data-status",
          
          p(
            strong("目前最新日期："),
            textOutput(
              "global_latest_date",
              inline = TRUE
            )
          ),
          
          p(
            strong("最早日期："),
            textOutput(
              "global_earliest_date",
              inline = TRUE
            )
          ),
          
          p(
            strong("股票數量："),
            textOutput(
              "global_stock_count",
              inline = TRUE
            )
          ),
          
          p(
            strong("資料筆數："),
            textOutput(
              "global_observation_count",
              inline = TRUE
            )
          ),
          
          p(
            strong("檔案更新時間："),
            textOutput(
              "global_file_modified",
              inline = TRUE
            )
          )
        ),
        
        h3("市場觀察日期"),
        
        dateInput(
          "market_observation_date",
          "請選擇日期：",
          value = data_max_date,
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          language = "zh-TW",
          weekstart = 1
        ),
        
        h3("0050 與市場概況"),
        
        fluidRow(
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "0050 調整後價格"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_0050_price",
                  inline = TRUE
                )
              )
            )
          ),
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "最近一日報酬"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_0050_daily_return",
                  inline = TRUE
                )
              )
            )
          ),
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "年初至今報酬"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_ytd_return",
                  inline = TRUE
                )
              )
            )
          ),
          
          column(
            3,
            div(
              class = "info-card",
              div(
                class = "info-card-title",
                "50 檔漲跌家數"
              ),
              h3(
                class = "info-card-value",
                textOutput(
                  "market_breadth",
                  inline = TRUE
                )
              )
            )
          )
        ),
        
        hr(),
        
        h3("0050 調整後收盤價"),
        
        checkboxInput(
          "market_price_color_by_change",
          "啟用漲跌著色",
          value = FALSE
        ),
        
        dateRangeInput(
          "market_chart_date_range",
          "圖表時間範圍：",
          start = data_min_date,
          end = data_max_date,
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          separator = " 至 "
        ),
        
        plotOutput(
          "market_0050_price_chart",
          height = "550px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "主要觀察 0050 長期價格趨勢。曲線上升代表調整後價格增加，短期斜率越大代表該段價格變動越明顯。"
        ),
        
        hr(),
        
        h3("0050 近一年每日報酬"),
        
        checkboxInput(
          "market_return_color_by_change",
          "啟用漲跌幅著色",
          value = FALSE
        ),
        
        plotOutput(
          "market_0050_daily_return_timeseries",
          height = "500px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "高於 0% 代表上漲，低於 0% 代表下跌；離 0% 越遠代表當日價格變動幅度越大。"
        ),
        
        hr(),
        
        h3("近 30 個交易日成分股漲跌幅"),
        
        plotOutput(
          "market_30d_return_ranking",
          height = "700px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "往右代表近 30 個交易日累積漲幅較大，往左代表跌幅較大。此圖描述過去期間，不代表未來表現。"
        ),
        
        hr(),
        
        h3("最新行情"),
        
        tableOutput(
          "realtime_monitor_table"
        )
      )
    ),
    
    
    # ========================================================
    # 2. 51 檔行情監控
    # ========================================================
    
    tabPanel(
      
      "51 檔行情監控",
      
      div(
        class = "section-box",
        
        h2("51 檔行情監控"),
        
        p(
          class = "page-description",
          "查看 0050 與 50 檔成分股在指定日期的價格、日報酬與成交量資訊。"
        ),
        
        dateInput(
          "monitor_date",
          "行情觀察日期：",
          value = data_max_date,
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          language = "zh-TW",
          weekstart = 1
        ),
        
        tableOutput(
          "monitor_table"
        )
      )
    ),
    
    
    # ========================================================
    # 3. 個股分析 (含子分頁)
    # ========================================================
    
    tabPanel(
      "個股分析",
      
      # 使用 tabsetPanel 建立子分頁
      tabsetPanel(
        id = "stock_subtabs",
        
        # === 子分頁 1：原本的報酬與風險分析 ===
        tabPanel(
          "報酬與風險分析",
          div(
            class = "section-box",
            h2("個股分析：報酬與風險"),
            p(class = "page-description", "最多選擇 5 檔股票，以指定期間比較價格、累積報酬、日對數報酬與風險。"),
            div(class = "reading-guide", strong("分析原則："), "研究分析使用調整後價格；日對數報酬率為 ln(Pt / Pt-1)。"),
            
            selectizeInput("selected_stock", "請選擇股票（最多 5 檔）：",
                           choices = dashboard_stock_choices, selected = "0050", multiple = TRUE,
                           options = list(maxItems = MAX_SELECTED_STOCKS, plugins = list("remove_button"))),
            
            div(class = "quick-button-row", strong("快速期間："),
                actionButton("period_research", "研究期間"), actionButton("period_all", "全部"),
                actionButton("period_1m", "1 個月"), actionButton("period_1q", "1 季"),
                actionButton("period_6m", "6 個月"), actionButton("period_1y", "1 年"),
                actionButton("period_ytd", "今年以來")),
            
            dateRangeInput("date_range", "分析期間：",
                           start = RESEARCH_START_DATE, end = min(RESEARCH_END_DATE, data_max_date),
                           min = data_min_date, max = data_max_date, format = "yyyy-mm-dd", separator = " 至 "),
            
            textOutput("analysis_period_text"), hr(),
            h3("所選股票實際資料期間"), tableOutput("selected_stock_ranges"), uiOutput("stock_warning"), hr(),
            
            h3("調整後收盤價"), plotOutput("price_plot", height = "500px"),
            div(class = "plot-explanation", strong("怎麼看？ "), "價格圖主要看趨勢。不同股票價格尺度不同，因此比較績效時應搭配累積報酬率。"),
            uiOutput("cumulative_return_display"), hr(),
            
            h3("日對數報酬描述統計"), tableOutput("return_summary"), hr(),
            h3("日對數報酬分布"), uiOutput("return_distribution_ui"),
            div(class = "plot-explanation", strong("怎麼看？ "), "直方圖呈現實際日對數報酬；深色曲線為依該股票樣本平均數與標準差估計的常態機率密度函數。若實際分布與曲線差異較大，表示資料未必符合常態分布。"), hr(),
            
            h3("日對數報酬箱型圖"), uiOutput("return_boxplot_ui"),
            div(class = "plot-explanation", strong("怎麼看？ "), "箱體代表中間 50% 的報酬分布。箱體越高通常表示報酬波動範圍越大。"), hr(),
            
            h3("日對數報酬時間序列"), uiOutput("log_return_series_ui"),
            div(class = "plot-explanation", strong("怎麼看？ "), "每檔股票各自一張圖，可以直接觀察高波動與極端報酬發生在哪些日期。"), hr(),
            
            h3("期間績效"), tableOutput("period_performance"), hr(),
            h3("價格描述統計"), tableOutput("price_summary"), hr(),
            h3("風險摘要"), tableOutput("risk_summary"), hr(),
            
            h3("回撤"), uiOutput("drawdown_charts_ui"),
            div(class = "plot-explanation", strong("怎麼看？ "), "回撤表示價格從之前高點下跌多少。越接近 0% 表示越接近高點，負值越深表示回落幅度越大。"), hr(),
            
            h3("報酬與風險關係"), plotOutput("return_risk_chart", height = "550px"),
            div(class = "plot-explanation", strong("怎麼看？ "), "越往右代表年化波動度越高；越往上代表年化報酬率越高。右上方代表報酬與波動都較高，左下方則代表報酬與波動都較低。這張圖主要用來觀察報酬與風險的分布關係。")
          )
        ),
        
        # === 子分頁 2：新增的技術指標 K 線 ===
        tabPanel(
          "技術指標 K 線",
          div(
            class = "section-box",
            h2("技術指標 K 線"),
            p(class = "page-description",
              "K 線主圖疊加移動平均線；副圖可自由勾選技術指標。ADR、ADL、OBOS 為市場廣度指標，以 0050 成分股代理計算。"),
            
            selectInput("tech_stock", "請選擇單一股票查看 K 線：",
                        choices = dashboard_stock_choices, selected = "0050"),
            
            sliderInput("tech_days", "顯示最近幾個交易日：",
                        min = 60, max = 300, value = 120, step = 10),
            
            checkboxGroupInput("tech_mas", "主圖均線：",
                               choices = c("MA5", "MA10", "MA20", "MA60", "MA120", "MA240"),
                               selected = c("MA5", "MA20", "MA60"), inline = TRUE),
            
            plotOutput("tech_main_chart", height = "550px"),
            
            hr(),
            h3("技術指標副圖"),
            checkboxGroupInput("tech_subs", "選擇副圖（可複選）：",
                               choices = c("成交量 VOL", "MACD (DIF/DEA/OSC)", "相對強弱 RSI",
                                           "隨機指標 KD", "乖離率 BIAS", "OBV 線",
                                           "量強弱 VR", "趨向指標 DMI", "威廉指標 WMS%R",
                                           "心理線 PSY", "寶塔線"),
                               selected = c("成交量 VOL", "MACD (DIF/DEA/OSC)",
                                            "相對強弱 RSI", "隨機指標 KD"), inline = TRUE),
            
            uiOutput("tech_sub_ui"),
            
            hr(),
            h3("市場廣度指標（0050 成分股代理）"),
            p(class = "page-description", "以下指標反映整體市場漲跌家數的動能，非單一股票指標。"),
            uiOutput("breadth_ui")
          )
        )
      )
    ),
    
    # ========================================================
    # 4. 產業比較
    # ========================================================
    
    tabPanel(
      
      "產業比較",
      
      div(
        class = "section-box",
        
        h2("產業比較"),
        
        p(
          class = "page-description",
          "產業比較中，原始產業分類若少於 4 檔成分股，統一合併為「其他」。"
        ),
        
        div(
          class = "reading-guide",
          strong("怎麼看？ "),
          "先在上方選擇一個產業查看其個別結果，再往下觀察所有產業的綜合比較。每個產業都有固定代表色。"
        ),
        
        # ========================================================
        # 單一產業
        # ========================================================
        
        h3("單一產業分析"),
        
        selectInput(
          "selected_industry",
          "請選擇產業：",
          choices = available_industries,
          selected = available_industries[1]
        ),
        
        h3("產業成分股"),
        
        tableOutput(
          "industry_stock_table"
        ),
        
        h3("產業報酬摘要"),
        
        tableOutput(
          "industry_summary"
        ),
        
        h3("產業累積報酬"),
        
        plotOutput(
          "industry_cumulative_chart",
          height = "550px"
        ),
        
        h3("產業風險與報酬"),
        
        plotOutput(
          "industry_risk_return_chart",
          height = "500px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "越往右代表波動度越高；越往上代表年化報酬率越高。可用來觀察該產業內各成分股的報酬與風險分布。"
        ),
        
        hr(),
        
        # ========================================================
        # 所有產業
        # ========================================================
        
        h3("所有產業綜合比較"),
        
        h4("產業累積報酬比較"),
        
        plotOutput(
          "industry_all_cumulative_chart",
          height = "600px"
        ),
        
        h4("產業日對數報酬分布"),
        
        uiOutput(
          "industry_all_distribution_ui"
        ),
        
        h4("產業報酬與風險"),
        
        plotOutput(
          "industry_all_risk_return_chart",
          height = "550px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "所有產業圖形使用各自代表色。累積報酬圖比較不同產業期間變化，分布圖比較日報酬型態，風險報酬圖則比較產業的年化報酬率與年化波動度。"
        )
        
      )
      
    ),
    
    # ========================================================
    # 財務分析
    # ========================================================
    tabPanel(
      "財務分析",
      div(
        class = "section-box",
        h2("財務分析"),
        p(class = "page-description", "資料來源為 Yahoo Finance 財務模組。最多選 5 檔進行比較。（0050 為 ETF，無財報數據）"),
        div(class = "data-status", p(strong("財務資料更新時間："), textOutput("fin_updated_at", inline = TRUE))),
        selectizeInput("fin_stock", "請選擇股票（最多 5 檔）：",
                       choices = dashboard_stock_choices, selected = c("2330", "2454"),
                       multiple = TRUE, options = list(maxItems = MAX_SELECTED_STOCKS, plugins = list("remove_button"))),
        h3("財務比率比較表"),
        tableOutput("fin_compare_table"),
        hr(),
        h3("單一比率長條圖比較"),
        selectInput("fin_bar_ratio", "選擇比率：",
                    choices = setNames(ratio_meta$key, ratio_meta$name), selected = "roe"),
        plotOutput("fin_bar_chart", height = "400px")
      )
    ),
    
    # ========================================================
    # 投資建議
    # ========================================================
    tabPanel(
      "投資建議",
      div(
        class = "section-box",
        h2("投資建議評分"),
        p(class = "page-description", "綜合技術面與基本面評分，產生透明可解釋的參考評級。"),
        div(class = "warning-note", strong("重要聲明："), "本模組為教學與研究用途，輸出僅為量化規則之機械化結果，不構成任何投資建議。"),
        sliderInput("adv_w_tech", "技術面權重（%）：", min = 0, max = 100, value = 50, step = 5),
        selectizeInput("adv_stock", "請選擇股票（最多 5 檔）：",
                       choices = dashboard_stock_choices, selected = c("2330", "2454"),
                       multiple = TRUE, options = list(maxItems = MAX_SELECTED_STOCKS, plugins = list("remove_button"))),
        actionButton("adv_scan_all", "掃描全部成分股（耗時較長）", class = "btn-primary"),
        hr(),
        h3("自選股評分結果"),
        tableOutput("adv_table"),
        plotOutput("adv_bar_chart", height = "400px"),
        h3("觸發訊號明細"),
        uiOutput("adv_signals_ui"),
        hr(),
        h3("全成分股掃描結果"),
        tableOutput("adv_scan_table")
      )
    ),
        
    # ========================================================
    # 5. 相關性分析
    # ========================================================
    
    tabPanel(
      
      "相關性分析",
      
      div(
        class = "section-box",
        
        h2("相關性分析"),
        
        p(
          class = "page-description",
          "使用指定分析期間的日對數報酬率計算股票間的相關係數。"
        ),
        
        selectizeInput(
          "cor_stock",
          "請選擇股票（最多 5 檔）：",
          choices = dashboard_stock_choices,
          selected = "0050",
          multiple = TRUE,
          options = list(
            maxItems = MAX_SELECTED_STOCKS,
            plugins = list("remove_button")
          )
        ),
        
        h3("相關係數矩陣"),
        
        tableOutput(
          "correlation_table"
        ),
        
        h3("相關係數熱圖"),
        
        plotOutput(
          "correlation_heatmap",
          height = "600px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "相關係數越接近 1 表示日報酬變化越同步；越接近 -1 表示反向變化較明顯；接近 0 表示線性關係較弱。"
        )
      )
    ),
    
    
    # ========================================================
    # 6. 投資組合
    # ========================================================
    
    tabPanel(
      
      "投資組合",
      
      div(
        class = "section-box",
        
        h2("投資組合模擬"),
        
        p(
          class = "page-description",
          "最多選擇 5 檔股票，設定買入、賣出日期與投入方式，模擬交易成本與投資績效。"
        ),
        
        div(
          class = "reading-guide",
          strong("怎麼使用？ "),
          "先選股票，再設定每檔股票的交易日期與投入方式。系統會依調整後價格計算交易與每日資產價值。"
        ),
        
        selectizeInput(
          "portfolio_stock",
          "投資組合股票（最多 5 檔）：",
          choices = dashboard_stock_choices,
          selected = "0050",
          multiple = TRUE,
          options = list(
            maxItems = MAX_SELECTED_STOCKS,
            plugins = list("remove_button")
          )
        ),
        
        selectInput(
          "portfolio_trade_mode",
          "交易設定方式：",
          choices = c(
            "以投入金額設定",
            "以持有股數設定"
          ),
          selected = "以投入金額設定"
        ),
        
        uiOutput(
          "portfolio_trade_inputs"
        ),
        
        hr(),
        
        h3("交易成本設定"),
        
        fluidRow(
          
          column(
            3,
            numericInput(
              "brokerage_rate",
              "手續費率（%）：",
              value = 0.1425,
              min = 0,
              step = 0.01
            )
          ),
          
          column(
            3,
            numericInput(
              "minimum_fee",
              "最低手續費（元）：",
              value = 20,
              min = 0,
              step = 1
            )
          ),
          
          column(
            3,
            numericInput(
              "stock_tax_rate",
              "股票證交稅（%）：",
              value = 0.30,
              min = 0,
              step = 0.01
            )
          ),
          
          column(
            3,
            numericInput(
              "etf_tax_rate",
              "ETF 證交稅（%）：",
              value = 0.10,
              min = 0,
              step = 0.01
            )
          )
        ),
        
        p(
          class = "control-description",
          "手續費於買進與賣出時計算；證交稅僅於賣出時計算。0050 使用 ETF 證交稅設定。"
        ),
        
        hr(),
        
        h3("投資組合交易明細"),
        
        tableOutput(
          "portfolio_trade_table"
        ),
        
        hr(),
        
        h3("投資組合每日資產價值"),
        
        dateRangeInput(
          "portfolio_value_date_range",
          "圖表時間範圍：",
          start = portfolio_default_start_date,
          end = portfolio_default_end_date,
          min = data_min_date,
          max = data_max_date,
          format = "yyyy-mm-dd",
          separator = " 至 "
        ),
        
        plotOutput(
          "portfolio_value_chart",
          height = "600px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "曲線上升表示投資組合資產價值增加；資產價值包含現金與尚未賣出的股票持倉。"
        ),
        
        h3("投資組合績效摘要"),
        
        tableOutput(
          "portfolio_summary"
        ),
        
        h3("投資組合績效與風險指標"),
        
        tableOutput(
          "portfolio_performance_metrics"
        ),
        
        h3("投資組合與 0050 績效比較"),
        
        tableOutput(
          "portfolio_benchmark_summary"
        ),
        
        plotOutput(
          "portfolio_benchmark_chart",
          height = "550px"
        ),
        
        div(
          class = "plot-explanation",
          strong("怎麼看？ "),
          "紅色虛線為 0050，另一條線為投資組合；兩者由相同基準點開始比較。"
        )
      )
    ),
    
    
    # ========================================================
    # 7. 資料明細
    # ========================================================
    
    tabPanel(
      
      "資料明細",
      
      div(
        class = "section-box",
        
        h2("資料明細"),
        
        p(
          class = "page-description",
          "查看目前 Dashboard 使用的股票清單、產業分類與資料範圍。"
        ),
        
        tableOutput(
          "stock_table"
        )
      )
    )
  ),
  
  
  div(
    class = "dashboard-footer",
    "0050 投資組合研究 Dashboard"
  )
)


# ============================================================
# 第 1 段結束
# ============================================================

# ============================================================
# 第 2 段：Server
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # 1. 自動讀取資料
  # ==========================================================
  #
  # 每 60 秒檢查一次 stock_data.rds。
  # 如果 update_data.R 更新了 RDS，
  # Dashboard 會重新讀取資料。
  #
  # 不需要重新啟動網站。
  # ==========================================================
  
  raw_data <- reactiveFileReader(
    intervalMillis = 60000,
    session = session,
    filePath = DATA_FILE,
    readFunc = readRDS
  )
  
  data <- reactive({
    prepare_stock_data(
      raw_data()
    )
  })
  
  trading_dates <- reactive({
    sort(
      unique(
        data()$date
      )
    )
  })
  
  latest_date <- reactive({
    max(
      data()$date,
      na.rm = TRUE
    )
  })
  
  earliest_date <- reactive({
    min(
      data()$date,
      na.rm = TRUE
    )
  })
  
  
  # ==========================================================
  # 2. 共用日期工具
  # ==========================================================
  
  selected_market_date <- reactive({
    
    req(
      input$market_observation_date
    )
    
    nearest_date(
      trading_dates(),
      input$market_observation_date
    )
  })
  
  selected_monitor_date <- reactive({
    
    req(
      input$monitor_date
    )
    
    nearest_date(
      trading_dates(),
      input$monitor_date
    )
  })
  
  
  # ==========================================================
  # 3. 市場總覽
  # ==========================================================
  
  output$global_latest_date <- renderText({
    format(
      latest_date(),
      "%Y-%m-%d"
    )
  })
  
  output$global_earliest_date <- renderText({
    format(
      earliest_date(),
      "%Y-%m-%d"
    )
  })
  
  output$global_stock_count <- renderText({
    n_distinct(
      data()$symbol
    )
  })
  
  output$global_observation_count <- renderText({
    fmt_num(
      nrow(data())
    )
  })
  
  output$global_file_modified <- renderText({
    
    info <- file.info(
      DATA_FILE
    )
    
    if (
      nrow(info) == 0 ||
      is.na(info$mtime)
    ) {
      
      return("無法取得")
      
    }
    
    format(
      info$mtime,
      "%Y-%m-%d %H:%M:%S"
    )
  })
  
  
  # ----------------------------------------------------------
  # 市場觀察日期自動同步
  # ----------------------------------------------------------
  
  observe({
    
    req(
      latest_date()
    )
    
    updateDateInput(
      session,
      "market_observation_date",
      value = latest_date(),
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  observe({
    
    req(
      latest_date()
    )
    
    updateDateInput(
      session,
      "monitor_date",
      value = latest_date(),
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 0050 最新價格
  # ----------------------------------------------------------
  
  output$market_0050_price <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        is.finite(adjusted)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    req(
      nrow(x) > 0
    )
    
    fmt_price(
      x$adjusted[1]
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 最近一日普通報酬
  # ----------------------------------------------------------
  
  output$market_0050_daily_return <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        is.finite(log_return)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    req(
      nrow(x) > 0
    )
    
    fmt_pct(
      x$log_return[1]
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 YTD
  # ----------------------------------------------------------
  
  output$market_ytd_return <- renderText({
    
    d <- selected_market_date()
    
    year_now <-
      format(
        d,
        "%Y"
      )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date <= d,
        format(date, "%Y") == year_now,
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) >= 2
    )
    
    ytd <-
      tail(x$adjusted, 1) /
      head(x$adjusted, 1) - 1
    
    fmt_pct(
      ytd
    )
  })
  
  
  # ----------------------------------------------------------
  # 50 檔成分股漲跌家數
  # ----------------------------------------------------------
  
  output$market_breadth <- renderText({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol %in% stock_info$symbol,
        date <= d,
        is.finite(daily_return)
      ) %>%
      group_by(symbol) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(n = 1) %>%
      ungroup()
    
    up <- sum(
      x$daily_return > 0,
      na.rm = TRUE
    )
    
    down <- sum(
      x$daily_return < 0,
      na.rm = TRUE
    )
    
    flat <- sum(
      x$daily_return == 0,
      na.rm = TRUE
    )
    
    paste0(
      up,
      " ↑ / ",
      down,
      " ↓ / ",
      flat,
      " –"
    )
  })
  
  
  # ----------------------------------------------------------
  # 0050 歷史價格
  # ----------------------------------------------------------
  
  output$market_0050_price_chart <- renderPlot({
    
    req(
      input$market_chart_date_range
    )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date >= as.Date(
          input$market_chart_date_range[1]
        ),
        date <= as.Date(
          input$market_chart_date_range[2]
        ),
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) > 1
    )
    
    # ----------------------------------------------------------
    # 未勾選：
    # 整條線維持黑色
    # ----------------------------------------------------------
    
    if (
      !isTRUE(
        input$market_price_color_by_change
      )
    ) {
      
      return(
        
        ggplot(
          x,
          aes(
            x = date,
            y = adjusted
          )
        ) +
          
          geom_line(
            color = "black",
            linewidth = 0.85
          ) +
          
          labs(
            title = "0050 調整後收盤價",
            x = "日期",
            y = "調整後價格"
          ) +
          
          theme_minimal() +
          
          theme(
            plot.title =
              element_text(
                hjust = 0.5,
                face = "bold"
              ),
            
            axis.text.x =
              element_text(
                angle = 45,
                hjust = 1
              )
          )
        
      )
    }
    
    
    # ----------------------------------------------------------
    # 勾選：
    # 依當日漲跌把線段染色
    #
    # 紅 = 上漲
    # 綠 = 下跌
    # 灰 = 持平
    # ----------------------------------------------------------
    
    x <- x %>%
      mutate(
        next_date = lead(date),
        next_price = lead(adjusted),
        
        direction = case_when(
          lead(adjusted) > adjusted ~ "上漲",
          lead(adjusted) < adjusted ~ "下跌",
          TRUE ~ "持平"
        )
      ) %>%
      filter(
        is.finite(next_price)
      )
    
    
    ggplot() +
      
      geom_segment(
        data = x,
        aes(
          x = date,
          y = adjusted,
          xend = next_date,
          yend = next_price,
          color = direction
        ),
        linewidth = 0.85
      ) +
      
      scale_color_manual(
        values = c(
          "上漲" = UP_COLOR,
          "下跌" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      ) +
      
      labs(
        title = "0050 調整後收盤價",
        x = "日期",
        y = "調整後價格",
        color = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom",
        
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 0050 近一年每日對數報酬
  # ----------------------------------------------------------
  
  output$market_0050_daily_return_timeseries <- renderPlot({
    
    d <- selected_market_date()
    
    dates <- trading_dates()
    
    end_index <- max(
      which(
        dates <= d
      )
    )
    
    start_index <- max(
      1,
      end_index - 251
    )
    
    x <- data() %>%
      filter(
        symbol == "0050",
        date >= dates[start_index],
        date <= d,
        is.finite(log_return)
      ) %>%
      arrange(date)
    
    req(
      nrow(x) > 1
    )
    
    # --------------------------------------------------------
    # 不啟用著色：黑色
    # --------------------------------------------------------
    
    if (
      !isTRUE(
        input$market_return_color_by_change
      )
    ) {
      
      return(
        
        ggplot(
          x,
          aes(
            x = date,
            y = log_return
          )
        ) +
          
          geom_hline(
            yintercept = 0,
            linetype = "dashed"
          ) +
          
          geom_line(
            color = "black",
            linewidth = 0.7
          ) +
          
          labs(
            title = "0050 近一年每日對數報酬率",
            x = "日期",
            y = "日對數報酬率"
          ) +
          
          scale_y_continuous(
            labels = function(z) {
              paste0(
                round(
                  z * 100,
                  2
                ),
                "%"
              )
            }
          ) +
          
          theme_minimal() +
          
          theme(
            plot.title =
              element_text(
                hjust = 0.5,
                face = "bold"
              )
          )
        
      )
    }
    
    # --------------------------------------------------------
    # 啟用著色
    # --------------------------------------------------------
    
    x <- x %>%
      mutate(
        next_date =
          lead(date),
        
        next_return =
          lead(log_return),
        
        direction =
          case_when(
            next_return > log_return ~ "上升",
            next_return < log_return ~ "下降",
            TRUE ~ "持平"
          )
      ) %>%
      filter(
        is.finite(next_return)
      )
    
    ggplot(
      x
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_segment(
        aes(
          x = date,
          y = log_return,
          xend = next_date,
          yend = next_return,
          color = direction
        ),
        linewidth = 0.8
      ) +
      
      scale_color_manual(
        values = c(
          "上升" = UP_COLOR,
          "下降" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      ) +
      
      labs(
        title = "0050 近一年每日對數報酬率",
        x = "日期",
        y = "日對數報酬率",
        color = NULL
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              2
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom"
      )
    
  })
  
  # ----------------------------------------------------------
  # 近 30 個交易日排名
  # ----------------------------------------------------------
  
  output$market_30d_return_ranking <- renderPlot({
    
    d <- selected_market_date()
    
    x <- data() %>%
      filter(
        symbol %in% stock_info$symbol,
        date <= d,
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      slice_tail(n = 30) %>%
      summarise(
        n = n(),
        return_30d =
          ifelse(
            n >= 2,
            last(adjusted) /
              first(adjusted) - 1,
            NA_real_
          ),
        .groups = "drop"
      ) %>%
      filter(
        is.finite(return_30d)
      )
    
    req(
      nrow(x) > 0
    )
    
    x <- x %>%
      arrange(return_30d) %>%
      mutate(
        label = paste0(
          symbol,
          " ",
          get_stock_name(symbol)
        )
      )
    
    x <- x %>%
      mutate(
        direction = case_when(
          return_30d > 0 ~ "上漲",
          return_30d < 0 ~ "下跌",
          TRUE ~ "持平"
        )
      )
    
    ggplot(
      x,
      aes(
        x = return_30d,
        y = reorder(label, return_30d),
        fill = direction
      )
    ) +
      
      geom_col(
        width = 0.7
      ) +
      
      scale_fill_manual(
        values = c(
          "上漲" = UP_COLOR,
          "下跌" = DOWN_COLOR,
          "持平" = NEUTRAL_COLOR
        )
      )  +
      geom_vline(
        xintercept = 0,
        linetype = "dashed"
      ) +
      labs(
        title = "50 檔成分股近 30 個交易日累積漲跌幅",
        x = "累積漲跌幅",
        y = NULL
      ) +
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(z * 100, 1),
            "%"
          )
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 最新行情表
  # ----------------------------------------------------------
  
  latest_market_table <- reactive({
    
    d <- selected_market_date()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            date <= d
          ) %>%
          arrange(
            desc(date)
          ) %>%
          slice_head(n = 1)
        
        if (nrow(x) == 0) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          日期 = x$date[1],
          調整後價格 = x$adjusted[1],
          日報酬 = x$daily_return[1],
          成交量 = x$volume[1],
          stringsAsFactors = FALSE
        )
      }
    )
    
    bind_rows(result)
    
  })
  
  
  output$realtime_monitor_table <- renderTable({
    
    latest_market_table() %>%
      mutate(
        日期 = format(
          日期,
          "%Y-%m-%d"
        ),
        調整後價格 = fmt_price(
          調整後價格
        ),
        日報酬 = fmt_pct(
          日報酬
        ),
        成交量 = fmt_num(
          成交量
        )
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
 
  # ==========================================================
  # 4. 51 檔行情監控
  # ==========================================================
  
  monitor_data <- reactive({
    
    d <- selected_monitor_date()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            date <= d
          ) %>%
          arrange(
            desc(date)
          ) %>%
          slice_head(
            n = 1
          )
        
        if (
          nrow(x) == 0
        ) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          日期 = x$date[1],
          收盤價 = x$close[1],
          調整後價格 = x$adjusted[1],
          日報酬 = x$daily_return[1],
          成交量 = x$volume[1],
          stringsAsFactors = FALSE
        )
        
      }
    )
    
    bind_rows(
      result
    )
    
  })
  
  
  output$monitor_table <- renderTable({
    
    monitor_data() %>%
      mutate(
        日期 = format(
          日期,
          "%Y-%m-%d"
        ),
        收盤價 = fmt_price(
          收盤價
        ),
        調整後價格 = fmt_price(
          調整後價格
        ),
        日報酬 = fmt_pct(
          日報酬
        ),
        成交量 = fmt_num(
          成交量
        )
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 5. 個股分析
  # ==========================================================
  
  analysis_range <- reactive({
    
    req(
      input$date_range
    )
    
    as.Date(
      input$date_range
    )
    
  })
  
  
  analysis_symbols <- reactive({
    
    s <- input$selected_stock
    
    if (
      is.null(s) ||
      length(s) == 0
    ) {
      return("0050")
    }
    
    head(
      unique(s),
      MAX_SELECTED_STOCKS
    )
    
  })
  
  
  analysis_data <- reactive({
    
    r <- analysis_range()
    
    data() %>%
      filter(
        symbol %in% analysis_symbols(),
        date >= r[1],
        date <= r[2]
      )
    
  })
  
  
  output$analysis_period_text <- renderText({
    
    r <- analysis_range()
    
    paste0(
      "目前分析期間：",
      format(r[1], "%Y-%m-%d"),
      " 至 ",
      format(r[2], "%Y-%m-%d")
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 快速期間
  # ----------------------------------------------------------
  
  update_analysis_range <- function(
    start_date,
    end_date
  ) {
    
    start_date <- nearest_date(
      trading_dates(),
      start_date
    )
    
    end_date <- nearest_date(
      trading_dates(),
      end_date
    )
    
    if (
      start_date > end_date
    ) {
      tmp <- start_date
      start_date <- end_date
      end_date <- tmp
    }
    
    updateDateRangeInput(
      session,
      "date_range",
      start = start_date,
      end = end_date
    )
  }
  
  
  observeEvent(
    input$period_research,
    {
      update_analysis_range(
        RESEARCH_START_DATE,
        min(
          RESEARCH_END_DATE,
          latest_date()
        )
      )
    }
  )
  
  
  observeEvent(
    input$period_all,
    {
      update_analysis_range(
        earliest_date(),
        latest_date()
      )
    }
  )
  
  
  observeEvent(
    input$period_1m,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 21) {
        update_analysis_range(
          dates[n - 20],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_1q,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 63) {
        update_analysis_range(
          dates[n - 62],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_6m,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 126) {
        update_analysis_range(
          dates[n - 125],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_1y,
    {
      dates <- trading_dates()
      n <- length(dates)
      
      if (n >= 252) {
        update_analysis_range(
          dates[n - 251],
          dates[n]
        )
      }
    }
  )
  
  
  observeEvent(
    input$period_ytd,
    {
      d <- latest_date()
      
      y <- format(
        d,
        "%Y"
      )
      
      y_dates <- trading_dates()[
        format(
          trading_dates(),
          "%Y"
        ) == y &
          trading_dates() <= d
      ]
      
      if (length(y_dates) >= 2) {
        
        update_analysis_range(
          min(y_dates),
          max(y_dates)
        )
      }
    }
  )
  
  
  # ----------------------------------------------------------
  # 個股資料期間
  # ----------------------------------------------------------
  
  output$selected_stock_ranges <- renderTable({
    
    result <- lapply(
      analysis_symbols(),
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (nrow(x) == 0) {
          return(NULL)
        }
        
        data.frame(
          股票代號 = s,
          股票名稱 = get_stock_name(s),
          最早日期 = format(
            min(x$date),
            "%Y-%m-%d"
          ),
          最新日期 = format(
            max(x$date),
            "%Y-%m-%d"
          ),
          有效觀測數 = nrow(x),
          stringsAsFactors = FALSE
        )
      }
    )
    
    bind_rows(result)
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 資料期間警告
  # ----------------------------------------------------------
  
  output$stock_warning <- renderUI({
    
    r <- analysis_range()
    
    selected <- analysis_symbols()
    
    warnings <- lapply(
      selected,
      function(s) {
        
        x <- data() %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (nrow(x) == 0) {
          return(
            div(
              class = "warning-note",
              paste0(
                get_stock_label(s),
                " 沒有有效價格資料。"
              )
            )
          )
        }
        
        actual_start <- min(x$date)
        actual_end <- max(x$date)
        
        if (
          actual_start > r[1] ||
          actual_end < r[2]
        ) {
          
          div(
            class = "warning-note",
            paste0(
              get_stock_label(s),
              " 的實際資料範圍為 ",
              format(actual_start, "%Y-%m-%d"),
              " 至 ",
              format(actual_end, "%Y-%m-%d"),
              "，與目前分析期間不完全重疊。"
            )
          )
          
        } else {
          
          NULL
        }
      }
    )
    
    do.call(
      tagList,
      warnings
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 個股調整後收盤價
  # 所選股票全部合併在同一張圖
  # ----------------------------------------------------------
  
  output$price_plot <- renderPlot({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) >= 1
    )
    
    x <- analysis_data() %>%
      filter(
        symbol %in% symbols,
        is.finite(adjusted)
      ) %>%
      arrange(
        date
      )
    
    req(
      nrow(x) > 0
    )
    
    # --------------------------------------------------------
    # 建立目前選擇股票的代表色
    # --------------------------------------------------------
    
    plot_colors <- stock_colors[
      symbols
    ]
    
    # --------------------------------------------------------
    # 建立股票名稱
    # --------------------------------------------------------
    
    plot_labels <- get_stock_label(
      symbols
    )
    
    names(plot_labels) <- symbols
    
    # --------------------------------------------------------
    # 繪圖
    # --------------------------------------------------------
    
    ggplot(
      x,
      aes(
        x = date,
        y = adjusted,
        color = symbol,
        group = symbol
      )
    ) +
      
      geom_line(
        linewidth = 0.9,
        na.rm = TRUE
      ) +
      
      scale_color_manual(
        values = plot_colors,
        breaks = symbols,
        labels = plot_labels,
        drop = FALSE
      ) +
      
      labs(
        title = "個股調整後收盤價",
        x = "日期",
        y = "調整後價格",
        color = NULL
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        
        legend.position =
          "bottom",
        
        legend.text =
          element_text(
            size = 10
          ),
        
        axis.text.x =
          element_text(
            angle = 45,
            hjust = 1
          )
      )
    
  })
  
  # ----------------------------------------------------------
  # 日對數報酬描述統計
  # ----------------------------------------------------------
  
  output$return_summary <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return)
      ) %>%
      group_by(symbol) %>%
      summarise(
        有效交易日 = n(),
        平均數 = mean(
          log_return,
          na.rm = TRUE
        ),
        標準差 = sd(
          log_return,
          na.rm = TRUE
        ),
        最大值 = max(
          log_return,
          na.rm = TRUE
        ),
        最小值 = min(
          log_return,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    req(
      nrow(x) > 0
    )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        平均數 = fmt_pct(平均數, 3),
        標準差 = fmt_pct(
          標準差,
          3,
          show_sign = FALSE
        ),
        最大值 = fmt_pct(最大值, 3),
        最小值 = fmt_pct(最小值, 3)
      ) %>%
      select(
        股票代號,
        股票名稱,
        有效交易日,
        平均數,
        標準差,
        最大值,
        最小值
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 日對數報酬分布
  # ----------------------------------------------------------
  
  output$return_distribution_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) > 0
    )
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 3
      ),
      
      function(i) {
        
        ids <-
          symbols[
            i:min(
              i + 2,
              length(symbols)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "return_distribution_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 10
                  )
                  
                  base_col <-
                    stock_colors[s]
                  
                  light_col <-
                    adjustcolor(
                      base_col,
                      alpha.f = 0.50
                    )
                  
                  hist_col <-
                    adjustcolor(
                      base_col,
                      alpha.f = 0.35
                    )
                  
                  mu <-
                    mean(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  sig <-
                    sd(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  p <- ggplot(
                    
                    x,
                    
                    aes(
                      x = log_return
                    )
                    
                  ) +
                    
                    geom_histogram(
                      
                      aes(
                        y =
                          after_stat(
                            density
                          )
                      ),
                      
                      bins = 40,
                      
                      fill =
                        light_col,
                      
                      color =
                        hist_col,
                      
                      linewidth = 0.3
                      
                    ) +
                    
                    labs(
                      
                      title =
                        get_stock_label(s),
                      
                      x =
                        "日對數報酬率",
                      
                      y =
                        "密度"
                      
                    ) +
                    
                    scale_x_continuous(
                      
                      labels =
                        function(z) {
                          
                          paste0(
                            round(
                              z * 100,
                              1
                            ),
                            "%"
                          )
                          
                        }
                      
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        )
                      
                    )
                  
                  if (
                    is.finite(sig) &&
                    sig > 0
                  ) {
                    
                    p <- p +
                      
                      stat_function(
                        
                        fun = dnorm,
                        
                        args = list(
                          mean = mu,
                          sd = sig
                        ),
                        
                        color =
                          base_col,
                        
                        linewidth =
                          1.2
                        
                      )
                    
                  }
                  
                  p
                  
                })
                
                column(
                  
                  width = 4,
                  
                  plotOutput(
                    plot_id,
                    height = "350px"
                  )
                  
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 日對數報酬箱型圖
  # 固定 1 × 5 排版
  # 即使只選 1 檔，也保留 5 個固定位置
  # ----------------------------------------------------------
  
  output$return_boxplot_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    req(
      length(symbols) > 0
    )
    
    div(
      style = "
      display:flex;
      width:100%;
      align-items:flex-start;
    ",
      
      lapply(
        seq_len(5),
        function(i) {
          
          if (i <= length(symbols)) {
            
            local({
              
              s <- symbols[i]
              
              plot_id <- paste0(
                "return_boxplot_",
                s
              )
              
              output[[plot_id]] <- renderPlot({
                
                x <- analysis_data() %>%
                  filter(
                    symbol == s,
                    is.finite(log_return)
                  )
                
                req(
                  nrow(x) > 0
                )
                
                current_color <-
                  stock_colors[s]
                
                ggplot(
                  x,
                  aes(
                    x = "",
                    y = log_return
                  )
                ) +
                  
                  geom_hline(
                    yintercept = 0,
                    linetype = "dashed"
                  ) +
                  
                  geom_boxplot(
                    width = 0.55,
                    fill = current_color,
                    color = current_color,
                    outlier.alpha = 0.35
                  ) +
                  
                  labs(
                    title = get_stock_label(s),
                    x = NULL,
                    y = "日對數報酬率"
                  ) +
                  
                  scale_y_continuous(
                    labels = function(z) {
                      paste0(
                        round(
                          z * 100,
                          2
                        ),
                        "%"
                      )
                    }
                  ) +
                  
                  theme_minimal() +
                  
                  theme(
                    plot.title =
                      element_text(
                        hjust = 0.5,
                        face = "bold",
                        size = 12
                      ),
                    
                    axis.text.x =
                      element_blank(),
                    
                    axis.ticks.x =
                      element_blank(),
                    
                    plot.margin =
                      margin(
                        10,
                        5,
                        10,
                        5
                      )
                  )
                
              })
              
              div(
                style = "
                flex:0 0 20%;
                width:20%;
                padding:0 5px;
              ",
                
                plotOutput(
                  plot_id,
                  height = "400px"
                )
              )
              
            })
            
          } else {
            
            div(
              style = "
              flex:0 0 20%;
              width:20%;
              padding:0 5px;
              height:400px;
            "
            )
            
          }
          
        }
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # 每檔股票獨立日對數報酬時間序列
  # ----------------------------------------------------------
  
  output$log_return_series_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 2
      ),
      
      function(i) {
        
        ids <-
          symbols[
            i:min(
              i + 1,
              length(symbols)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "log_return_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 1
                  )
                  
                  ggplot(
                    
                    x,
                    
                    aes(
                      x = date,
                      y = log_return
                    )
                    
                  ) +
                    
                    geom_hline(
                      yintercept = 0,
                      linetype = "dashed"
                    ) +
                    
                    geom_line(
                      color =
                        stock_colors[s],
                      linewidth = 0.7
                    ) +
                    
                    labs(
                      title =
                        get_stock_label(s),
                      x = "日期",
                      y = "日對數報酬率"
                    ) +
                    
                    scale_y_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            2
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        ),
                      axis.text.x =
                        element_text(
                          angle = 45,
                          hjust = 1
                        )
                    )
                  
                })
                
                column(
                  
                  width = 6,
                  
                  plotOutput(
                    plot_id,
                    height = "270px"
                  )
                  
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 期間績效
  # ----------------------------------------------------------
  
  output$period_performance <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(date, .by_group = TRUE) %>%
      summarise(
        起始日期 = first(date),
        結束日期 = last(date),
        起始價格 = first(adjusted),
        結束價格 = last(adjusted),
        有效交易日 = n(),
        .groups = "drop"
      ) %>%
      mutate(
        累積報酬率 =
          結束價格 /
          起始價格 - 1,
        年化報酬率 =
          mapply(
            annualized_return,
            起始價格,
            結束價格,
            有效交易日
          )
      )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        起始日期 = format(
          起始日期,
          "%Y-%m-%d"
        ),
        結束日期 = format(
          結束日期,
          "%Y-%m-%d"
        ),
        起始價格 = fmt_price(
          起始價格
        ),
        結束價格 = fmt_price(
          結束價格
        ),
        累積報酬率 = fmt_pct(
          累積報酬率
        ),
        年化報酬率 = fmt_pct(
          年化報酬率
        )
      ) %>%
      select(
        股票代號,
        股票名稱,
        起始日期,
        起始價格,
        結束日期,
        結束價格,
        有效交易日,
        累積報酬率,
        年化報酬率
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 價格摘要
  # ----------------------------------------------------------
  
  output$price_summary <- renderTable({
    
    x <- analysis_data() %>%
      filter(
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      summarise(
        平均價格 = mean(
          adjusted,
          na.rm = TRUE
        ),
        價格標準差 = sd(
          adjusted,
          na.rm = TRUE
        ),
        期間最低價 = min(
          adjusted,
          na.rm = TRUE
        ),
        期間最高價 = max(
          adjusted,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        平均價格 = fmt_price(
          平均價格
        ),
        價格標準差 = fmt_price(
          價格標準差
        ),
        期間最低價 = fmt_price(
          期間最低價
        ),
        期間最高價 = fmt_price(
          期間最高價
        )
      ) %>%
      select(
        股票代號,
        股票名稱,
        平均價格,
        價格標準差,
        期間最低價,
        期間最高價
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 風險摘要
  # ----------------------------------------------------------
  
  risk_data <- reactive({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        max_drawdown =
          min(
            drawdown_value(adjusted),
            na.rm = TRUE
          ),
        
        mean_daily =
          mean(
            log_return,
            na.rm = TRUE
          ),
        
        .groups = "drop"
      ) %>%
      mutate(
        
        sharpe =
          ifelse(
            annual_volatility > 0,
            mean_daily *
              TRADING_DAYS_PER_YEAR /
              annual_volatility,
            NA_real_
          )
        
      )
    
    x
    
  })
  
  
  output$risk_summary <- renderTable({
    
    x <- risk_data()
    
    x %>%
      mutate(
        股票代號 = symbol,
        股票名稱 = vapply(
          symbol,
          get_stock_name,
          character(1)
        ),
        年化波動度 = fmt_pct(
          annual_volatility,
          2,
          FALSE
        ),
        最大回撤 = fmt_pct(
          max_drawdown
        ),
        Sharpe_Ratio =
          ifelse(
            is.finite(sharpe),
            sprintf(
              "%.3f",
              sharpe
            ),
            "NA"
          )
      ) %>%
      select(
        股票代號,
        股票名稱,
        年化波動度,
        最大回撤,
        Sharpe_Ratio
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 回撤圖
  # ----------------------------------------------------------
  
  output$drawdown_charts_ui <- renderUI({
    
    symbols <- analysis_symbols()
    
    lapply(
      
      seq(
        1,
        length(symbols),
        by = 2
      ),
      
      function(i) {
        
        ids <- symbols[
          i:min(
            i + 1,
            length(symbols)
          )
        ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(symbol) {
              
              local({
                
                s <- symbol
                
                plot_id <-
                  paste0(
                    "drawdown_plot_",
                    s
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- analysis_data() %>%
                    filter(
                      symbol == s,
                      is.finite(adjusted)
                    ) %>%
                    arrange(date)
                  
                  req(
                    nrow(x) > 1
                  )
                  
                  x$drawdown <-
                    drawdown_value(
                      x$adjusted
                    )
                  
                  ggplot(
                    x,
                    aes(
                      x = date,
                      y = drawdown
                    )
                  ) +
                    
                    geom_hline(
                      yintercept = 0,
                      linetype = "dashed"
                    ) +
                    
                    geom_line(
                      color =
                        stock_colors[s],
                      linewidth = 0.9
                    ) +
                    
                    labs(
                      title =
                        get_stock_label(s),
                      x = "日期",
                      y = "回撤"
                    ) +
                    
                    scale_y_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            1
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        ),
                      
                      axis.text.x =
                        element_text(
                          angle = 45,
                          hjust = 1
                        )
                    )
                  
                })
                
                column(
                  6,
                  plotOutput(
                    plot_id,
                    height = "320px"
                  )
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 報酬與風險圖
  # ----------------------------------------------------------
  
  output$return_risk_chart <- renderPlot({
    
    x <- analysis_data() %>%
      filter(
        is.finite(log_return),
        is.finite(adjusted)
      ) %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_return =
          annualized_return(
            first(adjusted),
            last(adjusted),
            n()
          ),
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      
      x,
      
      aes(
        x = annual_volatility,
        y = annual_return,
        color = symbol,
        label = symbol
      )
      
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_point(
        size = 3
      ) +
      
      geom_text(
        vjust = -0.8
      ) +
      
      scale_color_manual(
        values = stock_colors,
        guide = "none"
      ) +
      
      labs(
        title = "報酬與風險關係",
        x = "年化波動度",
        y = "年化報酬率"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  # ==========================================================
  # 5.5 技術指標 K 線 (Server)
  # ==========================================================
  tech_full <- reactive({
    d <- data() %>% filter(symbol == input$tech_stock)
    req(nrow(d) > 60)
    calc_technical_one(d)
  })
  
  tech_view <- reactive({
    slice_tail(tech_full(), n = input$tech_days)
  })
  
  output$tech_main_chart <- renderPlot({
    plot_candlestick(tech_view(), input$tech_mas)
  })
  
  output$tech_sub_ui <- renderUI({
    sel <- input$tech_subs
    req(length(sel) > 0)
    df <- tech_view()
    lapply(seq(1, length(sel), by = 2), function(i) {
      ids <- sel[i:min(i + 1, length(sel))]
      fluidRow(lapply(ids, function(nm) {
        pid <- paste0("tech_sub_", make.names(nm))
        output[[pid]] <- renderPlot(tech_subchart_funs[[nm]](df))
        column(width = 6, plotOutput(pid, height = "300px"))
      }))
    })
  })
  
  output$breadth_ui <- renderUI({
    b <- calc_market_breadth(data()) %>% slice_tail(n = 250)
    lapply(names(breadth_chart_funs), function(nm) {
      pid <- paste0("breadth_", make.names(nm))
      output[[pid]] <- renderPlot(breadth_chart_funs[[nm]](b))
      fluidRow(column(width = 12, plotOutput(pid, height = "300px")))
    })
  })
  
  # ==========================================================
  # 6. 產業比較
  # ==========================================================
  
  selected_industry_stocks <- reactive({
    
    stock_info %>%
      filter(
        industry_group ==
          input$selected_industry
      ) %>%
      pull(symbol)
    
  })
  
  
  selected_industry_data <- reactive({
    
    data() %>%
      filter(
        symbol %in%
          selected_industry_stocks(),
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      )
    
  })
  
  
  industry_daily <- reactive({
    
    selected_industry_data() %>%
      group_by(date) %>%
      summarise(
        log_return =
          mean(
            log_return,
            na.rm = TRUE
          ),
        .groups = "drop"
      ) %>%
      arrange(date)
    
  })
  
  
  output$industry_stock_table <- renderTable({
    
    stock_info %>%
      filter(
        industry_group ==
          input$selected_industry
      ) %>%
      transmute(
        股票代號 = symbol,
        股票名稱 = name,
        產業 = industry_group
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$industry_summary <- renderTable({
    
    x <- industry_daily()
    
    req(
      nrow(x) > 1
    )
    
    data.frame(
      
      產業 =
        input$selected_industry,
      
      成分股數 =
        length(
          selected_industry_stocks()
        ),
      
      平均日對數報酬 =
        fmt_pct(
          mean(
            x$log_return,
            na.rm = TRUE
          ),
          3
        ),
      
      日對數報酬標準差 =
        fmt_pct(
          sd(
            x$log_return,
            na.rm = TRUE
          ),
          3,
          FALSE
        ),
      
      累積報酬率 =
        fmt_pct(
          exp(
            sum(
              x$log_return,
              na.rm = TRUE
            )
          ) - 1
        ),
      
      有效交易日 =
        nrow(x),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 單一產業累積報酬
  # ----------------------------------------------------------
  
  output$industry_cumulative_chart <- renderPlot({
    
    industry_x <- industry_daily() %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1
      )
    
    benchmark <- data() %>%
      filter(
        symbol == "0050",
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      arrange(date) %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1,
        series = "0050"
      )
    
    req(
      nrow(industry_x) > 1,
      nrow(benchmark) > 1
    )
    
    ggplot() +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        data = industry_x,
        aes(
          x = date,
          y = cumulative
        ),
        color =
          industry_colors[
            input$selected_industry
          ],
        linewidth = 1
      ) +
      
      geom_line(
        data = benchmark,
        aes(
          x = date,
          y = cumulative
        ),
        color = "red",
        linewidth = 1,
        linetype = "dashed"
      ) +
      
      labs(
        title =
          paste0(
            input$selected_industry,
            " 與 0050 累積報酬比較"
          ),
        x = "日期",
        y = "累積報酬率"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 單一產業風險報酬
  # ----------------------------------------------------------
  
  output$industry_risk_return_chart <- renderPlot({
    
    x <- selected_industry_data() %>%
      group_by(symbol) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      summarise(
        
        annual_return =
          exp(
            mean(
              log_return,
              na.rm = TRUE
            ) *
              TRADING_DAYS_PER_YEAR
          ) - 1,
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = annual_volatility,
        y = annual_return,
        label = symbol
      )
    ) +
      
      geom_point(
        size = 3,
        color =
          industry_colors[
            input$selected_industry
          ]
      ) +
      
      geom_text(
        vjust = -0.8,
        color =
          industry_colors[
            input$selected_industry
          ]
      ) +
      
      labs(
        title =
          paste0(
            input$selected_industry,
            " 成分股風險與報酬"
          ),
        x = "年化波動度",
        y = "年化報酬率"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          )
      )
    
  })
  
  
  # ==========================================================
  # 所有產業綜合比較
  # ==========================================================
  
  all_industry_daily <- reactive({
    
    data() %>%
      filter(
        symbol %in%
          stock_info$symbol,
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      group_by(
        industry_group,
        date
      ) %>%
      summarise(
        log_return =
          mean(
            log_return,
            na.rm = TRUE
          ),
        .groups = "drop"
      ) %>%
      arrange(
        industry_group,
        date
      )
    
  })
  
  
  output$industry_all_cumulative_chart <- renderPlot({
    
    x <- all_industry_daily() %>%
      group_by(
        industry_group
      ) %>%
      arrange(
        date,
        .by_group = TRUE
      ) %>%
      mutate(
        cumulative =
          exp(
            cumsum(
              log_return
            )
          ) - 1
      ) %>%
      ungroup()
    
    ggplot(
      x,
      aes(
        x = date,
        y = cumulative,
        color = industry_group
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        linewidth = 0.9
      ) +
      
      scale_color_manual(
        values =
          industry_colors
      ) +
      
      labs(
        title =
          "所有產業累積報酬比較",
        x = "日期",
        y = "累積報酬率",
        color = "產業"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        legend.position =
          "bottom"
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 所有產業報酬分布
  # 3 × N
  # ----------------------------------------------------------
  
  output$industry_all_distribution_ui <- renderUI({
    
    industries <-
      sort(
        unique(
          all_industry_daily()$industry_group
        )
      )
    
    lapply(
      
      seq(
        1,
        length(industries),
        by = 3
      ),
      
      function(i) {
        
        ids <-
          industries[
            i:min(
              i + 2,
              length(industries)
            )
          ]
        
        fluidRow(
          
          lapply(
            
            ids,
            
            function(industry) {
              
              local({
                
                ind <- industry
                
                plot_id <-
                  paste0(
                    "industry_distribution_",
                    make.names(ind)
                  )
                
                output[[plot_id]] <- renderPlot({
                  
                  x <- all_industry_daily() %>%
                    filter(
                      industry_group == ind,
                      is.finite(log_return)
                    )
                  
                  req(
                    nrow(x) > 10
                  )
                  
                  col <-
                    industry_colors[ind]
                  
                  light <-
                    adjustcolor(
                      col,
                      alpha.f = 0.50
                    )
                  
                  edge <-
                    adjustcolor(
                      col,
                      alpha.f = 0.35
                    )
                  
                  mu <-
                    mean(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  sig <-
                    sd(
                      x$log_return,
                      na.rm = TRUE
                    )
                  
                  p <- ggplot(
                    x,
                    aes(
                      x = log_return
                    )
                  ) +
                    
                    geom_histogram(
                      aes(
                        y =
                          after_stat(
                            density
                          )
                      ),
                      bins = 40,
                      fill = light,
                      color = edge
                    ) +
                    
                    labs(
                      title = ind,
                      x = "平均日對數報酬率",
                      y = "密度"
                    ) +
                    
                    scale_x_continuous(
                      labels = function(z) {
                        paste0(
                          round(
                            z * 100,
                            1
                          ),
                          "%"
                        )
                      }
                    ) +
                    
                    theme_minimal() +
                    
                    theme(
                      plot.title =
                        element_text(
                          hjust = 0.5,
                          face = "bold"
                        )
                    )
                  
                  if (
                    is.finite(sig) &&
                    sig > 0
                  ) {
                    
                    p <- p +
                      stat_function(
                        fun = dnorm,
                        args = list(
                          mean = mu,
                          sd = sig
                        ),
                        color = col,
                        linewidth = 1.1
                      )
                    
                  }
                  
                  p
                  
                })
                
                column(
                  width = 4,
                  plotOutput(
                    plot_id,
                    height = "340px"
                  )
                )
                
              })
              
            }
            
          )
          
        )
        
      }
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 所有產業風險報酬
  # ----------------------------------------------------------
  
  output$industry_all_risk_return_chart <- renderPlot({
    
    x <- all_industry_daily() %>%
      group_by(
        industry_group
      ) %>%
      summarise(
        
        annual_return =
          exp(
            mean(
              log_return,
              na.rm = TRUE
            ) *
              TRADING_DAYS_PER_YEAR
          ) - 1,
        
        annual_volatility =
          sd(
            log_return,
            na.rm = TRUE
          ) *
          sqrt(
            TRADING_DAYS_PER_YEAR
          ),
        
        .groups = "drop"
        
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      
      x,
      
      aes(
        x = annual_volatility,
        y = annual_return,
        color = industry_group,
        label = industry_group
      )
      
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_point(
        size = 4
      ) +
      
      geom_text(
        vjust = -0.8
      ) +
      
      scale_color_manual(
        values =
          industry_colors
      ) +
      
      labs(
        title =
          "所有產業報酬與風險比較",
        x = "年化波動度",
        y = "年化報酬率",
        color = "產業"
      ) +
      
      scale_x_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(
              z * 100,
              1
            ),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title =
          element_text(
            hjust = 0.5,
            face = "bold"
          ),
        legend.position =
          "bottom"
      )
    
  })

  # ==========================================================
  # 6.5 財務分析
  # ==========================================================
  
  fin_raw <- reactivePoll(
    intervalMillis = 600000, session = session,
    checkFunc = function() {
      if (file.exists("data/financial_data.rds")) file.info("data/financial_data.rds")$mtime else NULL
    },
    valueFunc = function() readRDS("data/financial_data.rds")
  )
  
  output$fin_updated_at <- renderText({
    req(file.exists("data/financial_data.rds"))
    fin_raw()$updated_at
  })
  
  fin_symbols <- reactive({
    s <- setdiff(input$fin_stock, "0050")
    req(length(s) > 0)
    head(s, MAX_SELECTED_STOCKS)
  })
  
  fin_ratio_matrix <- reactive({
    fd <- fin_raw()
    req(!is.null(fd$stocks))
    res <- lapply(fin_symbols(), function(code) {
      st <- fd$stocks[[code]]
      if (is.null(st)) return(NULL)
      r <- compute_fin_ratios(st)
      data.frame(key = names(r), value = as.numeric(r), symbol = code, stringsAsFactors = FALSE)
    })
    out <- bind_rows(res)
    # 防護機制：若無資料，回傳帶有正確欄位名稱的空資料框，避免 filter 報錯
    if (nrow(out) == 0) {
      return(data.frame(key = character(), value = numeric(), symbol = character(), stringsAsFactors = FALSE))
    }
    out
  })
  
  output$fin_compare_table <- renderTable({
    m <- fin_ratio_matrix()
    req(nrow(m) > 0)
    wide <- m %>%
      mutate(label = paste0(get_stock_name(symbol), " (", symbol, ")")) %>%
      select(key, label, value) %>%
      tidyr::pivot_wider(names_from = label, values_from = value)
    meta <- ratio_meta %>% select(key, name, unit, group)
    out <- meta %>% dplyr::left_join(wide, by = "key")
    num_cols <- setdiff(names(out), c("key", "name", "unit", "group"))
    out %>% mutate(across(all_of(num_cols), ~ ifelse(is.finite(.), sprintf("%.1f", .), "NA"))) %>%
      select(group, name, unit, all_of(num_cols))
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$fin_bar_chart <- renderPlot({
    m <- fin_ratio_matrix() %>% filter(key == input$fin_bar_ratio)
    req(nrow(m) > 0)
    m <- m %>% mutate(label = paste0(symbol, " ", get_stock_name(symbol)))
    ggplot(m, aes(x = reorder(label, value), y = value, fill = label)) +
      geom_col(width = 0.6) + scale_fill_discrete(guide = "none") +
      labs(title = paste0(ratio_meta$name[match(input$fin_bar_ratio, ratio_meta$key)], " 比較"),
           x = NULL, y = ratio_meta$unit[match(input$fin_bar_ratio, ratio_meta$key)]) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # ==========================================================
  # 6.6 投資建議
  # ==========================================================
  
  adv_scores <- reactive({
    req(input$adv_stock)
    fd <- fin_raw()
    req(!is.null(fd$stocks))
    w <- input$adv_w_tech / 100
    
    res <- lapply(setdiff(input$adv_stock, "0050"), function(code) {
      d <- data() %>% filter(symbol == code)
      st <- fd$stocks[[code]]
      if (nrow(d) < 60 || is.null(st)) return(NULL)
      t <- score_technical(calc_technical_one(d))
      f <- score_fundamental(st)
      total <- w * t$score + (1 - w) * f$score
      data.frame(股票代號 = code, 股票名稱 = get_stock_name(code),
                 技術分 = round(t$score, 1), 財務分 = round(f$score, 1),
                 綜合分 = round(total, 1), 評級 = rating_label(total), stringsAsFactors = FALSE)
    })
    
    out <- bind_rows(res)
    if (nrow(out) == 0) return(NULL)
    out
  })
  
  output$adv_table <- renderTable({ 
    req(adv_scores())
    adv_scores() 
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$adv_bar_chart <- renderPlot({
    x <- adv_scores()
    req(!is.null(x) && nrow(x) > 0)
    ggplot(x, aes(x = reorder(股票名稱, 綜合分), y = 綜合分, fill = 評級)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = c("買進" = "#c23b33", "加碼" = "#e57373", "持有" = "#90a4ae", "減碼" = "#66bb6a", "觀望迴避" = "#2e7d32")) +
      geom_hline(yintercept = c(30, 45, 55, 70), linetype = "dotted", color = "gray60") +
      labs(title = "綜合評分", x = NULL, y = "分數") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$adv_signals_ui <- renderUI({
    req(input$adv_stock)
    lapply(setdiff(input$adv_stock, "0050"), function(code) {
      d <- data() %>% filter(symbol == code)
      if (nrow(d) < 60) return(NULL)
      sig <- tech_signals(calc_technical_one(d))
      
      tags$div(
        tags$h4(get_stock_label(code)),
        if (nrow(sig) == 0) {
          tags$p("近期無技術訊號觸發。")
        } else {
          tags$ul(lapply(seq_len(nrow(sig)), function(i) {
            tags$li(tags$span(
              tags$strong(ifelse(sig$type[i] == "buy", "【買方訊號】", "【賣方訊號】")),
              paste0(" ", sig$indicator[i], "：", sig$text[i])
            ))
          }))
        }
      )
    })
  })
  
  adv_scan_result <- eventReactive(input$adv_scan_all, {
    fd <- fin_raw()
    req(!is.null(fd$stocks))
    w <- input$adv_w_tech / 100
    res <- lapply(names(fd$stocks), function(code) {
      d <- data() %>% filter(symbol == code)
      if (nrow(d) < 60) return(NULL)
      t <- score_technical(calc_technical_one(d))
      f <- score_fundamental(fd$stocks[[code]])
      total <- w * t$score + (1 - w) * f$score
      data.frame(股票代號 = code, 股票名稱 = get_stock_name(code),
                 技術分 = round(t$score, 1), 財務分 = round(f$score, 1),
                 綜合分 = round(total, 1), 評級 = rating_label(total), stringsAsFactors = FALSE)
    })
    out <- bind_rows(res)
    if (nrow(out) == 0) return(NULL)
    out %>% arrange(desc(綜合分))
  })
  
  output$adv_scan_table <- renderTable({ 
    req(adv_scan_result())
    adv_scan_result() 
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  # ==========================================================
  # 7. 相關性分析
  # ==========================================================
  
  correlation_data <- reactive({
    
    req(
      input$cor_stock
    )
    
    data() %>%
      filter(
        symbol %in% input$cor_stock,
        date >= analysis_range()[1],
        date <= analysis_range()[2],
        is.finite(log_return)
      ) %>%
      select(
        date,
        symbol,
        log_return
      ) %>%
      pivot_wider(
        names_from = symbol,
        values_from = log_return
      )
    
  })
  
  
  correlation_matrix <- reactive({
    
    x <- correlation_data()
    
    req(
      nrow(x) >= 2,
      ncol(x) >= 3
    )
    
    r <- cor(
      x %>%
        select(-date),
      use = "pairwise.complete.obs"
    )
    
    r
    
  })
  
  
  output$correlation_table <- renderTable({
    
    r <- correlation_matrix()
    
    result <- as.data.frame(
      round(
        r,
        3
      )
    )
    
    result$股票 <- rownames(
      result
    )
    
    result <- result %>%
      select(
        股票,
        everything()
      )
    
    names(result)[-1] <-
      vapply(
        names(result)[-1],
        function(s) {
          get_stock_label(s)
        },
        character(1)
      )
    
    result
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$correlation_heatmap <- renderPlot({
    
    r <- correlation_matrix()
    
    long <- as.data.frame(
      as.table(r)
    )
    
    names(long) <- c(
      "Stock1",
      "Stock2",
      "Correlation"
    )
    
    ggplot(
      long,
      aes(
        x = Stock1,
        y = Stock2,
        fill = Correlation
      )
    ) +
      geom_tile(
        color = "white"
      ) +
      geom_text(
        aes(
          label =
            sprintf(
              "%.2f",
              Correlation
            )
        ),
        size = 5
      ) +
      scale_fill_gradient2(
        limits = c(-1, 1),
        midpoint = 0
      ) +
      scale_x_discrete(
        labels = function(z) {
          vapply(
            z,
            get_stock_name,
            character(1)
          )
        }
      ) +
      scale_y_discrete(
        labels = function(z) {
          vapply(
            z,
            get_stock_name,
            character(1)
          )
        }
      ) +
      labs(
        title = "日對數報酬相關係數熱圖",
        x = NULL,
        y = NULL,
        fill = "相關係數"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # 8. 投資組合
  # ==========================================================
  
  # ----------------------------------------------------------
  # 動態交易設定
  # ----------------------------------------------------------
  
  output$portfolio_trade_inputs <- renderUI({
    
    req(
      input$portfolio_stock
    )
    
    tagList(
      
      lapply(
        
        input$portfolio_stock,
        
        function(symbol) {
          
          x <- data() %>%
            filter(
              symbol == !!symbol,
              is.finite(adjusted)
            ) %>%
            arrange(date)
          
          if (nrow(x) == 0) {
            return(
              div(
                class = "warning-note",
                paste0(
                  get_stock_label(symbol),
                  " 沒有可用資料。"
                )
              )
            )
          }
          
          dates <- unique(
            x$date
          )
          buy_default <- nearest_date(
            dates,
            max(
              min(dates),
              max(dates) - 365
            )
          )
          
          sell_default <- nearest_date(
            dates,
            max(dates)
          )
          
          if (
            sell_default < buy_default
          ) {
            sell_default <- max(dates)
          }
          
          div(
            
            class = "portfolio-trade-box",
            
            h4(
              get_stock_label(symbol)
            ),
            
            fluidRow(
              
              column(
                
                4,
                
                dateInput(
                  paste0(
                    "portfolio_buy_",
                    symbol
                  ),
                  "買入日期：",
                  value = buy_default,
                  min = min(dates),
                  max = max(dates),
                  format = "yyyy-mm-dd",
                  language = "zh-TW"
                )
                
              ),
              
              column(
                
                4,
                
                dateInput(
                  paste0(
                    "portfolio_sell_",
                    symbol
                  ),
                  "賣出日期：",
                  value = sell_default,
                  min = min(dates),
                  max = max(dates),
                  format = "yyyy-mm-dd",
                  language = "zh-TW"
                )
                
              ),
              
              column(
                
                4,
                
                if (
                  input$portfolio_trade_mode ==
                  "以投入金額設定"
                ) {
                  
                  numericInput(
                    paste0(
                      "portfolio_amount_",
                      symbol
                    ),
                    "配置資金（元）：",
                    value = 100000,
                    min = 0,
                    step = 10000
                  )
                  
                } else {
                  
                  numericInput(
                    paste0(
                      "portfolio_shares_",
                      symbol
                    ),
                    "持有股數：",
                    value = 1000,
                    min = 0,
                    step = TRADE_UNIT
                  )
                }
                
              )
            ),
            
            p(
              class = "control-description",
              "若輸入日期不是交易日，計算時會自動使用該股票最近的有效交易日。"
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 手續費函數
  # ----------------------------------------------------------
  
  brokerage_fee <- function(
    value
  ) {
    
    rate <- max(
      0,
      input$brokerage_rate
    ) / 100
    
    minimum <- max(
      0,
      input$minimum_fee
    )
    
    if (
      !is.finite(value) ||
      value <= 0
    ) {
      return(0)
    }
    
    max(
      value * rate,
      minimum
    )
    
  }
  
  
  # ----------------------------------------------------------
  # 證交稅
  # ----------------------------------------------------------
  
  sell_tax <- function(
    symbol,
    value
  ) {
    
    if (
      !is.finite(value) ||
      value <= 0
    ) {
      return(0)
    }
    
    rate <- if (
      symbol == "0050"
    ) {
      input$etf_tax_rate
    } else {
      input$stock_tax_rate
    }
    
    value * max(
      0,
      rate
    ) / 100
    
  }
  
  
  # ----------------------------------------------------------
  # 股票最近有效價格
  # ----------------------------------------------------------
  
  price_on_or_before <- function(
    symbol,
    d
  ) {
    
    x <- data() %>%
      filter(
        symbol == !!symbol,
        date <= d,
        is.finite(adjusted)
      ) %>%
      arrange(
        desc(date)
      ) %>%
      slice_head(
        n = 1
      )
    
    if (
      nrow(x) == 0
    ) {
      return(NA_real_)
    }
    
    x$adjusted[1]
  }
  
  
  # ----------------------------------------------------------
  # 投資組合交易資料
  # ----------------------------------------------------------
  
  portfolio_trades <- reactive({
    
    req(
      input$portfolio_stock
    )
    
    result <- lapply(
      
      input$portfolio_stock,
      
      function(symbol) {
        
        x <- data() %>%
          filter(
            symbol == !!symbol,
            is.finite(adjusted)
          ) %>%
          arrange(date)
        
        if (
          nrow(x) == 0
        ) {
          return(NULL)
        }
        
        dates <- unique(
          x$date
        )
        
        buy_input <- input[[
          paste0(
            "portfolio_buy_",
            symbol
          )
        ]]
        
        sell_input <- input[[
          paste0(
            "portfolio_sell_",
            symbol
          )
        ]]
        
        if (
          is.null(buy_input) ||
          is.null(sell_input)
        ) {
          return(NULL)
        }
        
        buy_date <- nearest_date(
          dates,
          buy_input
        )
        
        sell_date <- nearest_date(
          dates,
          sell_input
        )
        
        if (
          is.na(buy_date) ||
          is.na(sell_date) ||
          sell_date < buy_date
        ) {
          
          return(
            data.frame(
              股票代號 = symbol,
              股票名稱 = get_stock_name(symbol),
              狀態 = "日期設定無效",
              stringsAsFactors = FALSE
            )
          )
        }
        
        buy_price <- price_on_or_before(
          symbol,
          buy_date
        )
        
        sell_price <- price_on_or_before(
          symbol,
          sell_date
        )
        
        if (
          !is.finite(buy_price) ||
          !is.finite(sell_price)
        ) {
          return(NULL)
        }
        
        if (
          input$portfolio_trade_mode ==
          "以投入金額設定"
        ) {
          
          amount_input <- input[[
            paste0(
              "portfolio_amount_",
              symbol
            )
          ]]
          
          amount_input <- gsub(
            ",",
            "",
            amount_input
          )
          
          amount <- suppressWarnings(
            as.numeric(amount_input)
          )
          
          if (
            !is.finite(amount)
          ) {
            amount <- 0
          }
          
          amount <- max(
            0,
            amount
          )
          
          shares <- floor(
            amount /
              (
                buy_price *
                  TRADE_UNIT
              )
          ) *
            TRADE_UNIT
          
          while (
            shares > 0 &&
            (
              shares *
              buy_price +
              brokerage_fee(
                shares *
                buy_price
              )
            ) > amount
          ) {
            
            shares <-
              shares -
              TRADE_UNIT
            
          }
          
          allocation <- amount
          
        } else {
          
          shares <- max(
            0,
            floor(
              input[[
                paste0(
                  "portfolio_shares_",
                  symbol
                )
              ]] /
                TRADE_UNIT
            ) *
              TRADE_UNIT
          )
          
          allocation <-
            shares *
            buy_price +
            brokerage_fee(
              shares *
                buy_price
            )
        }
        
        buy_value <-
          shares *
          buy_price
        
        buy_fee <-
          brokerage_fee(
            buy_value
          )
        
        sell_value <-
          shares *
          sell_price
        
        sell_fee <-
          brokerage_fee(
            sell_value
          )
        
        tax <-
          sell_tax(
            symbol,
            sell_value
          )
        
        net_sell <-
          sell_value -
          sell_fee -
          tax
        
        data.frame(
          
          股票代號 = symbol,
          
          股票名稱 =
            get_stock_name(symbol),
          
          買入日期 =
            buy_date,
          
          買入價格 =
            buy_price,
          
          賣出日期 =
            sell_date,
          
          賣出價格 =
            sell_price,
          
          股數 =
            shares,
          
          配置資金 =
            allocation,
          
          買入成交金額 =
            buy_value,
          
          買入手續費 =
            buy_fee,
          
          實際投入資金 =
            buy_value +
            buy_fee,
          
          賣出成交金額 =
            sell_value,
          
          賣出手續費 =
            sell_fee,
          
          證交稅 =
            tax,
          
          賣出後所得 =
            net_sell,
          
          狀態 = ifelse(
            shares > 0,
            "有效",
            "股數為 0"
          ),
          
          stringsAsFactors =
            FALSE
          
        )
      }
    )
    
    trades <- bind_rows(
      result
    )
    
    # --------------------------------------------------------
    # 確保交易資料具有固定欄位
    # --------------------------------------------------------
    
    required_cols <- c(
      "股票代號",
      "股票名稱",
      "買入日期",
      "買入價格",
      "賣出日期",
      "賣出價格",
      "股數",
      "配置資金",
      "買入成交金額",
      "買入手續費",
      "實際投入資金",
      "賣出成交金額",
      "賣出手續費",
      "證交稅",
      "賣出後所得",
      "狀態"
    )
    
    date_cols <- c(
      "買入日期",
      "賣出日期"
    )
    
    character_cols <- c(
      "股票代號",
      "股票名稱",
      "狀態"
    )
    
    for (
      nm in setdiff(
        required_cols,
        names(trades)
      )
    ) {
      
      if (
        nm %in% date_cols
      ) {
        
        trades[[nm]] <- as.Date(
          NA
        )
        
      } else if (
        nm %in% character_cols
      ) {
        
        trades[[nm]] <- NA_character_
        
      } else {
        
        trades[[nm]] <- NA_real_
        
      }
    }
    
    trades <- trades[
      ,
      required_cols,
      drop = FALSE
    ]
    
    trades
    
  })
  
  
  output$portfolio_trade_table <- renderTable({
    
    x <- portfolio_trades()
    
    req(
      nrow(x) > 0
    )
    
    x %>%
      mutate(
        買入日期 =
          ifelse(
            is.na(買入日期),
            "NA",
            format(
              as.Date(買入日期),
              "%Y-%m-%d"
            )
          ),
        賣出日期 =
          ifelse(
            is.na(賣出日期),
            "NA",
            format(
              as.Date(賣出日期),
              "%Y-%m-%d"
            )
          ),
        買入價格 =
          fmt_price(買入價格),
        賣出價格 =
          fmt_price(賣出價格),
        股數 =
          fmt_num(股數),
        配置資金 =
          fmt_num(配置資金, 2),
        買入成交金額 =
          fmt_num(買入成交金額, 2),
        買入手續費 =
          fmt_num(買入手續費, 2),
        實際投入資金 =
          fmt_num(實際投入資金, 2),
        賣出成交金額 =
          fmt_num(賣出成交金額, 2),
        賣出手續費 =
          fmt_num(賣出手續費, 2),
        證交稅 =
          fmt_num(證交稅, 2),
        賣出後所得 =
          fmt_num(賣出後所得, 2)
      )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 投資組合每日資產價值
  # ----------------------------------------------------------
  
  portfolio_daily <- reactive({
    
    trades <- portfolio_trades() %>%
      filter(
        狀態 == "有效",
        股數 > 0,
        is.finite(買入價格),
        is.finite(賣出價格)
      )
    
    req(
      nrow(trades) > 0
    )
    
    # --------------------------------------------------------
    # 初始資金
    # --------------------------------------------------------
    
    initial_capital <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
      
    } else {
      
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
    }
    
    req(
      is.finite(initial_capital),
      initial_capital > 0
    )
    
    
    # --------------------------------------------------------
    # 投資組合期間
    # 第一筆買入 → 最後一筆賣出
    # --------------------------------------------------------
    
    start_date <- min(
      trades$買入日期,
      na.rm = TRUE
    )
    
    end_date <- max(
      trades$賣出日期,
      na.rm = TRUE
    )
    
    req(
      is.finite(start_date),
      is.finite(end_date),
      start_date <= end_date
    )
    
    
    # 只使用投資組合期間內的交易日
    dates <- trading_dates()[
      trading_dates() >= start_date &
        trading_dates() <= end_date
    ]
    
    req(
      length(dates) > 0
    )
    
    
    # --------------------------------------------------------
    # 每一個交易日計算
    # --------------------------------------------------------
    
    result <- lapply(
      
      dates,
      
      function(d) {
        
        # ======================================================
        # 1. 從初始資金開始
        # ======================================================
        
        cash <- initial_capital
        
        
        # ======================================================
        # 2. 扣除截至今天已經完成的買入
        # ======================================================
        
        bought <- trades[
          trades$買入日期 <= d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(bought) > 0
        ) {
          
          cash <-
            cash -
            sum(
              bought$實際投入資金,
              na.rm = TRUE
            )
          
        }
        
        
        # ======================================================
        # 3. 加回截至今天已經完成的賣出
        # ======================================================
        
        sold <- trades[
          trades$賣出日期 <= d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(sold) > 0
        ) {
          
          cash <-
            cash +
            sum(
              sold$賣出後所得,
              na.rm = TRUE
            )
          
        }
        
        
        # ======================================================
        # 4. 計算目前仍然持有的股票市值
        # ======================================================
        
        holdings <- 0
        
        active_trades <- trades[
          trades$買入日期 <= d &
            trades$賣出日期 > d,
          ,
          drop = FALSE
        ]
        
        if (
          nrow(active_trades) > 0
        ) {
          
          for (
            i in seq_len(
              nrow(active_trades)
            )
          ) {
            
            tr <- active_trades[i, ]
            
            current_price <- price_on_or_before(
              tr$股票代號,
              d
            )
            
            if (
              is.finite(current_price)
            ) {
              
              holdings <-
                holdings +
                tr$股數 *
                current_price
              
            }
          }
        }
        
        
        # ======================================================
        # 5. 投資組合總資產
        # ======================================================
        
        portfolio_value <-
          cash +
          holdings
        
        
        data.frame(
          date = d,
          cash = cash,
          holdings = holdings,
          portfolio_value = portfolio_value,
          stringsAsFactors = FALSE
        )
        
      }
      
    )
    
    
    result <- bind_rows(
      result
    )
    
    
    # --------------------------------------------------------
    # 數值安全檢查
    # --------------------------------------------------------
    
    result <- result %>%
      mutate(
        cash = ifelse(
          is.finite(cash),
          cash,
          NA_real_
        ),
        holdings = ifelse(
          is.finite(holdings),
          holdings,
          NA_real_
        ),
        portfolio_value = ifelse(
          is.finite(portfolio_value),
          portfolio_value,
          NA_real_
        )
      )
    
    
    result
    
  })
  
  
  output$portfolio_value_chart <- renderPlot({
    
    x <- portfolio_daily()
    
    req(
      input$portfolio_value_date_range
    )
    
    x <- x %>%
      filter(
        date >= as.Date(
          input$portfolio_value_date_range[1]
        ),
        date <= as.Date(
          input$portfolio_value_date_range[2]
        )
      )
    
    req(
      nrow(x) > 0
    )
    
    ggplot(
      x,
      aes(
        x = date,
        y = portfolio_value
      )
    ) +
      geom_line(
        linewidth = 0.9
      ) +
      labs(
        title = "投資組合每日資產價值",
        x = "日期",
        y = "資產價值（元）"
      ) +
      scale_y_continuous(
        labels = function(z) {
          format(
            round(z),
            big.mark = ","
          )
        }
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ----------------------------------------------------------
  # 投資組合績效摘要
  # ----------------------------------------------------------
  
  portfolio_metrics <- reactive({
    
    x <- portfolio_daily()
    trades <- portfolio_trades() %>%
      filter(
        狀態 == "有效",
        股數 > 0
      )
    
    initial <- if (
      input$portfolio_trade_mode ==
      "以投入金額設定"
    ) {
      sum(
        trades$配置資金,
        na.rm = TRUE
      )
    } else {
      sum(
        trades$實際投入資金,
        na.rm = TRUE
      )
    }
    
    final <- tail(
      x$portfolio_value,
      1
    )
    
    daily_ret <-
      x$portfolio_value /
      lag(x$portfolio_value) - 1
    
    daily_ret <-
      daily_ret[
        is.finite(daily_ret)
      ]
    
    total_return <-
      final /
      initial - 1
    
    ann_return <-
      annualized_return(
        initial,
        final,
        nrow(x)
      )
    
    ann_vol <-
      sd(
        daily_ret,
        na.rm = TRUE
      ) *
      sqrt(
        TRADING_DAYS_PER_YEAR
      )
    
    sharpe <-
      if (
        is.finite(ann_vol) &&
        ann_vol > 0
      ) {
        mean(
          daily_ret,
          na.rm = TRUE
        ) *
          TRADING_DAYS_PER_YEAR /
          ann_vol
      } else {
        NA_real_
      }
    
    dd <- drawdown_value(
      x$portfolio_value
    )
    
    max_dd <- min(
      dd,
      na.rm = TRUE
    )
    
    list(
      initial = initial,
      final = final,
      total_return = total_return,
      annual_return = ann_return,
      annual_volatility = ann_vol,
      sharpe = sharpe,
      max_drawdown = max_dd,
      days = nrow(x)
    )
    
  })
  
  
  output$portfolio_summary <- renderTable({
    
    m <- portfolio_metrics()
    
    data.frame(
      
      指標 = c(
        "初始配置資金",
        "期末資產價值",
        "累積報酬率",
        "年化報酬率",
        "有效交易日數"
      ),
      
      數值 = c(
        
        fmt_num(
          m$initial,
          0
        ),
        
        fmt_num(
          m$final,
          2
        ),
        
        fmt_pct(
          m$total_return
        ),
        
        fmt_pct(
          m$annual_return
        ),
        
        fmt_num(
          m$days
        )
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$portfolio_performance_metrics <- renderTable({
    
    m <- portfolio_metrics()
    
    data.frame(
      
      指標 = c(
        "年化報酬率",
        "年化波動度",
        "最大回撤",
        "Sharpe Ratio"
      ),
      
      數值 = c(
        
        fmt_pct(
          m$annual_return
        ),
        
        fmt_pct(
          m$annual_volatility,
          2,
          FALSE
        ),
        
        fmt_pct(
          m$max_drawdown
        ),
        
        ifelse(
          is.finite(m$sharpe),
          sprintf(
            "%.3f",
            m$sharpe
          ),
          "NA"
        )
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ----------------------------------------------------------
  # 投資組合與 0050 比較
  # ----------------------------------------------------------
  
  portfolio_benchmark <- reactive({
    
    p <- portfolio_daily()
    
    req(
      nrow(p) > 1
    )
    
    start <- min(
      p$date
    )
    
    end <- max(
      p$date
    )
    
    b <- data() %>%
      filter(
        symbol == "0050",
        date >= start,
        date <= end,
        is.finite(adjusted)
      ) %>%
      arrange(date)
    
    req(
      nrow(b) > 1
    )
    
    p_return <-
      p$portfolio_value /
      first(p$portfolio_value) - 1
    
    b_return <-
      b$adjusted /
      first(b$adjusted) - 1
    
    p2 <- data.frame(
      date = p$date,
      portfolio_return = p_return
    )
    
    b2 <- data.frame(
      date = b$date,
      benchmark_return = b_return
    )
    
    list(
      portfolio = p2,
      benchmark = b2
    )
    
  })
  
  
  output$portfolio_benchmark_summary <- renderTable({
    
    x <- portfolio_benchmark()
    
    p <- tail(
      x$portfolio$portfolio_return,
      1
    )
    
    b <- tail(
      x$benchmark$benchmark_return,
      1
    )
    
    data.frame(
      
      指標 = c(
        "投資組合",
        "0050",
        "超額報酬"
      ),
      
      累積報酬率 = c(
        fmt_pct(p),
        fmt_pct(b),
        fmt_pct(p - b)
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  output$portfolio_benchmark_chart <- renderPlot({
    
    x <- portfolio_benchmark()
    
    p <- x$portfolio
    b <- x$benchmark
    
    merged <- full_join(
      p,
      b,
      by = "date"
    ) %>%
      arrange(date)
    
    ggplot(
      merged,
      aes(
        x = date
      )
    ) +
      
      geom_hline(
        yintercept = 0,
        linetype = "dashed"
      ) +
      
      geom_line(
        aes(
          y = portfolio_return,
          color = "投資組合"
        ),
        linewidth = 1
      ) +
      
      geom_line(
        aes(
          y = benchmark_return,
          color = "0050"
        ),
        linewidth = 1,
        linetype = "dashed"
      ) +
      
      scale_color_manual(
        values = c(
          "投資組合" = "steelblue",
          "0050" = "red"
        )
      ) +
      
      labs(
        title = "投資組合與 0050 累積報酬比較",
        x = "日期",
        y = "累積報酬率",
        color = "比較標的"
      ) +
      
      scale_y_continuous(
        labels = function(z) {
          paste0(
            round(z * 100, 1),
            "%"
          )
        }
      ) +
      
      theme_minimal() +
      
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        legend.position = "bottom",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
  })
  
  
  # ==========================================================
  # 9. 資料明細
  # ==========================================================
  
  output$stock_table <- renderTable({
    
    x <- data()
    
    symbols <- c(
      "0050",
      stock_info$symbol
    )
    
    result <- lapply(
      symbols,
      function(s) {
        
        z <- x %>%
          filter(
            symbol == s,
            is.finite(adjusted)
          )
        
        if (
          nrow(z) == 0
        ) {
          return(NULL)
        }
        
        data.frame(
          
          股票代號 = s,
          
          股票名稱 =
            get_stock_name(s),
          
          產業 =
            get_stock_industry(s),
          
          最早日期 =
            format(
              min(z$date),
              "%Y-%m-%d"
            ),
          
          最新日期 =
            format(
              max(z$date),
              "%Y-%m-%d"
            ),
          
          有效觀測數 =
            nrow(z),
          
          stringsAsFactors =
            FALSE
          
        )
      }
    )
    
    bind_rows(
      result
    )
    
  },
  striped = TRUE,
  bordered = TRUE,
  hover = TRUE
  )
  
  
  # ==========================================================
  # 10. 日期同步
  # ==========================================================
  
  # ----------------------------------------------------------
  # 市場價格圖：資料更新後同步可選日期範圍
  # ----------------------------------------------------------
  
  observe({
    
    req(
      latest_date(),
      earliest_date()
    )
    
    updateDateRangeInput(
      session,
      "market_chart_date_range",
      min = earliest_date(),
      max = latest_date()
    )
    
  })
  
  
  # ----------------------------------------------------------
  # 投資組合：跟著實際交易期間同步
  # ----------------------------------------------------------
  
  observeEvent(
    portfolio_trades(),
    {
      
      trades <- portfolio_trades() %>%
        filter(
          狀態 == "有效",
          股數 > 0,
          !is.na(買入日期),
          !is.na(賣出日期)
        )
      
      if (
        nrow(trades) == 0
      ) {
        return()
      }
      
      portfolio_start <- min(
        trades$買入日期,
        na.rm = TRUE
      )
      
      portfolio_end <- max(
        trades$賣出日期,
        na.rm = TRUE
      )
      
      if (
        !is.finite(portfolio_start) ||
        !is.finite(portfolio_end) ||
        portfolio_start > portfolio_end
      ) {
        return()
      }
      
      updateDateRangeInput(
        session,
        "portfolio_value_date_range",
        start = portfolio_start,
        end = portfolio_end,
        min = earliest_date(),
        max = latest_date()
      )
      
    },
    ignoreInit = FALSE
  )
  
  
  # ==========================================================
  # 11. 結束 Server
  # ==========================================================
  
}


# ============================================================
# 12. 啟動 Shiny
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

