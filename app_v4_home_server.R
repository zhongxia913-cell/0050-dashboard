  # ============================================================
  # v4 首頁：0010 總覽
  # 貼在 server 內（位置任意，建議最後）
  # 依賴：v4_score.R、v4_drawdown.R
  # ============================================================

  v4_cap_amount <- 1000000   # 首頁試算金額

  # ---- 價格矩陣 ----
  v4_pm <- shiny::reactive({
    d <- as.data.frame(data())
    d <- d[d$symbol %in% c("0050", stock_info$symbol), , drop = FALSE]
    d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]

    w <- as.data.frame(
      tidyr::pivot_wider(d[, c("date", "symbol", "adjusted")],
                         names_from = symbol, values_from = adjusted)
    )
    dates <- as.Date(w$date)
    m <- as.matrix(w[, -1, drop = FALSE])
    rownames(m) <- as.character(dates)
    storage.mode(m) <- "double"
    m
  })

  # ---- 分數表（若 GARCH 表已存在就一併納入）----
  v4_tbl <- shiny::reactive({
    g <- tryCatch(get("g10_store")(), error = function(e) NULL)
    v4_score_table(as.data.frame(data()), garch_tbl = g)
  })

  v4_sel <- shiny::reactive({
    t <- v4_tbl()
    shiny::req(!is.null(t), nrow(t) >= 10)
    v4_pick(t, 10)
  })

  v4_w <- shiny::reactive({
    s <- v4_sel()
    shiny::req(nrow(s) > 0)
    v4_weights(s, scheme = "inv_vol", cap = 0.20)
  })

  # ---- 上一次再平衡日（最近一個季末）----
  v4_reb <- shiny::reactive({
    m <- v4_pm()
    d <- as.Date(rownames(m))
    key <- paste0(format(d, "%Y"), "Q", ceiling(as.integer(format(d, "%m")) / 3))
    last_idx <- tapply(seq_along(d), key, max)
    i <- max(as.integer(last_idx))
    list(idx = i, date = d[i])
  })

  # ---- 當期持有（買進持有，不算再平衡）----
  v4_hold <- shiny::reactive({
    v4_buyhold(v4_pm(), v4_w(), v4_reb()$idx)
  })

  v4_bench <- shiny::reactive({
    m <- v4_pm()
    i <- v4_reb()$idx
    x <- m[i:nrow(m), "0050"]
    data.frame(date = as.Date(rownames(m)[i:nrow(m)]),
               value = x / x[1])
  })

  v4_hold_ret <- shiny::reactive({
    h <- v4_hold()
    shiny::req(!is.null(h))
    h$value[nrow(h)] - 1
  })

  v4_bench_ret <- shiny::reactive({
    b <- v4_bench()
    shiny::req(!is.null(b))
    b$value[nrow(b)] - 1
  })

  output$v4_k_ret <- shiny::renderText({
    v <- v4_hold_ret()
    ifelse(is.finite(v), sprintf("%+.2f%%", v * 100), "—")
  })

  output$v4_k_bench <- shiny::renderText({
    v <- v4_bench_ret()
    ifelse(is.finite(v), sprintf("%+.2f%%", v * 100), "—")
  })

  output$v4_k_excess <- shiny::renderText({
    v <- v4_hold_ret() - v4_bench_ret()
    ifelse(is.finite(v), sprintf("%+.2f%%", v * 100), "—")
  })

  output$v4_k_mdd <- shiny::renderText({
    h <- v4_hold()
    shiny::req(!is.null(h))
    s <- v4_dd_series(h$value, h$date)
    st <- v4_dd_stats(s)
    if (is.null(st)) "—" else sprintf("%.2f%%", st$max_dd * 100)
  })

  # ---- 名單 ----
  output$v4_home_table <- shiny::renderTable({
    s <- v4_sel()
    w <- v4_w()
    data.frame(
      名次 = seq_len(nrow(s)),
      股票 = paste0(s$name, "（", s$symbol, "）"),
      產業 = s$industry,
      動能分 = sprintf("%.1f", s$mom_score),
      風險分 = sprintf("%.1f", s$risk_score),
      `0010 分` = sprintf("%.1f", s$p10_smooth),
      權重 = sprintf("%.1f%%", as.numeric(w[s$symbol]) * 100),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ---- 回撤圖 ----
  output$v4_home_dd <- shiny::renderPlot({
    h <- v4_hold()
    b <- v4_bench()
    shiny::req(!is.null(h), !is.null(b))

    sh <- v4_dd_series(h$value, h$date)
    sb <- v4_dd_series(b$value, b$date)

    df <- rbind(
      data.frame(date = sh$date, dd = sh$dd, series = "0010"),
      data.frame(date = sb$date, dd = sb$dd, series = "0050")
    )

    ggplot2::ggplot(df, ggplot2::aes(x = date, y = dd, colour = series)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = "本期回撤（自上次再平衡起）", x = NULL, y = "回撤", colour = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$v4_home_dd_text <- shiny::renderText({
    h <- v4_hold()
    shiny::req(!is.null(h))
    st <- v4_dd_stats(v4_dd_series(h$value, h$date))
    if (is.null(st)) return("資料不足")
    paste0(
      "最大回撤 ", sprintf("%.2f%%", st$max_dd * 100),
      "（", as.character(st$trough_date), "）",
      "；水下時間 ", sprintf("%.0f%%", st$underwater_pct * 100),
      "；最長連續水下 ", st$longest_underwater, " 個交易日。"
    )
  })

  # ---- 權重與金額 ----
  output$v4_home_weight <- shiny::renderTable({
    s <- v4_sel()
    w <- v4_w()
    amt <- as.numeric(w[s$symbol]) * v4_cap_amount
    fee <- pmax(20, amt * 0.001425)

    data.frame(
      股票 = paste0(s$name, "（", s$symbol, "）"),
      權重 = sprintf("%.1f%%", as.numeric(w[s$symbol]) * 100),
      金額 = formatC(round(amt), format = "d", big.mark = ","),
      買進手續費 = formatC(round(fee), format = "d", big.mark = ","),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$v4_home_note <- shiny::renderText({
    d <- as.Date(rownames(v4_pm()))
    last <- max(d, na.rm = TRUE)
    y <- as.integer(format(last, "%Y")); mo <- as.integer(format(last, "%m"))
    q <- (mo - 1) %/% 3 + 1
    if (q == 4) { y <- y + 1; mo <- 1 } else { mo <- q * 3 + 1 }
    nd <- as.Date(sprintf("%04d-%02d-01", y, mo))
    while (as.integer(format(nd, "%u")) > 5) nd <- nd + 1

    paste0("評分日：", as.character(v4_reb()$date),
           "｜下次再平衡：", as.character(nd),
           "｜投入金額試算：", formatC(v4_cap_amount, format = "d", big.mark = ","), " 元")
  })

  # ---- 50 檔完整表 ----
  output$v4_all_table <- shiny::renderTable({
    t <- v4_tbl()
    shiny::req(!is.null(t))
    data.frame(
      排名 = seq_len(nrow(t)),
      股票 = paste0(t$name, "（", t$symbol, "）"),
      產業 = t$industry,
      動能 = sprintf("%.1f", t$mom_score),
      風險 = sprintf("%.1f", t$risk_score),
      `0010` = sprintf("%.1f", t$p10_smooth),
      波動 = sprintf("%.1f%%", t$vol * 100),
      回撤 = sprintf("%.1f%%", t$mdd * 100),
      CVaR = sprintf("%.2f%%", t$cvar * 100),
      篩選 = t$reason,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
