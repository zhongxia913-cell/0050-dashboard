  # ============================================================
  # 第 3 版：GARCH 波動預測與 0010 組合
  # 整段貼在 server 內（位置任意，建議貼在最後）
  # 所有名稱都加 g10_ 前綴
  # ============================================================

  g10_pct <- function(x, d = 2, sign = TRUE) {
    ifelse(is.finite(x), sprintf(if (sign) paste0("%+.", d, "f%%") else paste0("%.", d, "f%%"), x * 100), "—")
  }
  g10_num <- function(x, d = 2) {
    ifelse(is.finite(x), sprintf(paste0("%.", d, "f"), x), "—")
  }

  # ----------------------------------------------------------
  # 1. 單檔 GARCH
  # ----------------------------------------------------------

  g10_one <- shiny::reactive({
    s <- input$g10_stock
    shiny::req(!is.null(s), nzchar(s))
    d <- as.data.frame(data())
    x <- d[d$symbol == s & !is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    x <- x[order(x$date), , drop = FALSE]
    shiny::req(nrow(x) > 300)
    garch_analyse_one(x$adjusted, window = as.integer(input$g10_window), horizon = 63)
  })

  output$g10_kpi_fvol <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    g10_pct(a$summary$forecast_vol, 2, FALSE)
  })

  output$g10_kpi_rvol <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    g10_pct(a$real_vol, 2, FALSE)
  })

  output$g10_kpi_ratio <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    g10_num(a$ratio, 3)
  })

  output$g10_kpi_regime <- shiny::renderText({
    a <- g10_one(); shiny::req(!is.null(a))
    a$regime
  })

  output$g10_param_table <- shiny::renderTable({
    a <- g10_one(); shiny::req(!is.null(a))
    f <- a$fit
    data.frame(
      參數 = c("ω（常數項）", "α（ARCH 衝擊）", "β（GARCH 延續）", "α + β（持續性）", "半衰期（交易日）"),
      估計值 = c(g10_num(f$omega, 4), g10_num(f$alpha, 4), g10_num(f$beta, 4),
                 g10_num(f$persistence, 4), g10_num(f$half_life, 1)),
      標準誤 = c(g10_num(f$se_omega, 4), g10_num(f$se_alpha, 4), g10_num(f$se_beta, 4), "—", "—"),
      t值 = c(g10_num(f$t_omega, 2), g10_num(f$t_alpha, 2), g10_num(f$t_beta, 2), "—", "—"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$g10_vol_path <- shiny::renderPlot({
    a <- g10_one(); shiny::req(!is.null(a))
    f <- a$fit
    v <- f$sigma2
    n <- length(v)
    idx <- max(1, n - 249):n
    gvol <- sqrt(v[idx] * GARCH_TD) / 100

    r <- a$returns
    k <- length(r)
    real <- rep(NA_real_, k)
    w <- 21
    for (i in seq(w, k)) real[i] <- sqrt(mean(r[(i - w + 1):i]^2) * GARCH_TD) / 100

    df <- data.frame(t = seq_along(idx), garch = gvol, real = real[idx])
    ggplot2::ggplot(df, ggplot2::aes(x = t)) +
      ggplot2::geom_line(ggplot2::aes(y = real), colour = "grey65", linewidth = 0.7) +
      ggplot2::geom_line(ggplot2::aes(y = garch), colour = "#1976d2", linewidth = 0.9) +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = "GARCH 條件波動（藍）vs 21 日實現波動（灰）",
                    x = "最近 250 個交易日", y = "年化波動") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$g10_fan <- shiny::renderPlot({
    a <- g10_one(); shiny::req(!is.null(a))
    fv <- garch11_forecast(a$fit, 63)
    df <- data.frame(h = seq_along(fv), vol = sqrt(fv * GARCH_TD) / 100)
    ggplot2::ggplot(df, ggplot2::aes(x = h, y = vol)) +
      ggplot2::geom_line(colour = "#2e7d32", linewidth = 1) +
      ggplot2::geom_hline(yintercept = a$real_vol, linetype = 2, colour = "#e53935") +
      ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
      ggplot2::labs(title = "未來 63 日年化波動預測（紅虛線 = 近一年實現波動）",
                    x = "預測期（交易日）", y = "年化波動") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$g10_diag <- shiny::renderTable({
    a <- g10_one(); shiny::req(!is.null(a))
    f <- a$fit
    data.frame(
      項目 = c("樣本數", "對數概似", "AIC", "BIC", "Ljung-Box Q(10)", "Q 檢定 p 值", "收斂", "無條件年化波動", "1 日 VaR 95%", "1 日 CVaR 95%"),
      數值 = c(as.character(f$n), g10_num(f$loglik, 1), g10_num(f$aic, 1), g10_num(f$bic, 1),
              g10_num(f$lb_stat, 2), g10_num(f$lb_p, 4),
              if (isTRUE(f$converged)) "是" else "否",
              g10_pct(f$uncond_vol, 2, FALSE),
              g10_pct(a$summary$var95, 2, FALSE),
              g10_pct(a$summary$cvar95, 2, FALSE)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  # ----------------------------------------------------------
  # 2. 全成分股 GARCH 表（按鈕觸發 + 快取）
  # ----------------------------------------------------------

  g10_store <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$g10_run_all, {
    d <- as.data.frame(data())
    syms <- stock_info$symbol
    withProgress(message = "估計 GARCH(1,1)…", value = 0, {
      tbl <- p10_garch_table(
        d, syms,
        window = as.integer(input$g10_window),
        horizon = 63,
        progress = function(k, n, s) {
          setProgress(k / n, detail = paste0("第 ", k, " / ", n, " 檔：", s))
        }
      )
    })
    g10_store(tbl)
  })

  output$g10_all_table <- shiny::renderTable({
    g <- g10_store()
    if (is.null(g)) return(data.frame(提示 = "請先按上方按鈕估計"))
    out <- data.frame(
      股票 = paste0(get_stock_name(g$symbol), "（", g$symbol, "）"),
      `ω` = g10_num(g$omega, 4),
      `α` = g10_num(g$alpha, 4),
      `β` = g10_num(g$beta, 4),
      `α+β` = g10_num(g$persistence, 4),
      半衰期 = g10_num(g$half_life, 1),
      預測波動 = g10_pct(g$forecast_vol, 1, FALSE),
      實現波動 = g10_pct(g$real_vol, 1, FALSE),
      比值 = g10_num(g$ratio, 2),
      狀態 = g$regime,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out[order(g$ratio), , drop = FALSE]
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$g10_ratio_bar <- shiny::renderPlot({
    g <- g10_store()
    shiny::req(!is.null(g))
    x <- g[is.finite(g$ratio), , drop = FALSE]
    x <- x[order(x$ratio), , drop = FALSE]
    labs <- paste0(get_stock_name(x$symbol), "(", x$symbol, ")")
    x$label <- factor(labs, levels = labs)
    ggplot2::ggplot(x, ggplot2::aes(x = label, y = ratio, fill = ratio)) +
      ggplot2::geom_col(width = 0.75) +
      ggplot2::geom_hline(yintercept = 1, linetype = 2) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient2(midpoint = 1, low = "#1565c0", mid = "#ffffff", high = "#c62828") +
      ggplot2::labs(title = "預測波動 ÷ 近一年實現波動", x = NULL, y = "比值") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     axis.text.y = ggplot2::element_text(size = 8),
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  # ----------------------------------------------------------
  # 3. 0010 組合
  # ----------------------------------------------------------

  # 共用的寬表（報酬、價格、EWMA 波動），只建一次
  g10_mats <- shiny::reactive({
    d <- as.data.frame(data())
    d <- d[d$symbol %in% c("0050", stock_info$symbol), , drop = FALSE]
    d <- d[!is.na(d$adjusted) & d$adjusted > 0, , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]

    wide <- as.data.frame(tidyr::pivot_wider(d[, c("date", "symbol", "log_return")],
                                             names_from = symbol, values_from = log_return))
    widep <- as.data.frame(tidyr::pivot_wider(d[, c("date", "symbol", "adjusted")],
                                              names_from = symbol, values_from = adjusted))
    shiny::req(nrow(wide) > 300, nrow(wide) == nrow(widep))

    dates <- as.Date(wide$date)
    m <- as.matrix(wide[, -1, drop = FALSE]); rownames(m) <- as.character(dates)
    pm <- as.matrix(widep[, -1, drop = FALSE]); rownames(pm) <- as.character(dates)
    storage.mode(m) <- "double"; storage.mode(pm) <- "double"

    list(dates = dates, m = m, pm = pm, vol = p10_vol_matrix(m))
  })

  g10_scores <- shiny::reactive({
    mt <- g10_mats()
    vol <- mt$vol

    # 若已估計 GARCH，用最新一期的 GARCH 預測波動取代最後一列
    g <- g10_store()
    if (!is.null(g) && nrow(g) > 0) {
      idx <- match(colnames(mt$m), g$symbol)
      fv <- g$forecast_vol[idx]
      ok <- is.finite(fv)
      if (any(ok)) vol[nrow(vol), ok] <- fv[ok]
    }

    sc <- p10_matrix_scores(mt$m, vol, preset = input$g10_preset)
    shiny::req(!is.null(sc), nrow(sc) > 0)
    sc <- sc[sc$symbol != "0050", , drop = FALSE]
    sc[order(-sc$p10_score), , drop = FALSE]
  })

  g10_pick <- shiny::reactive({
    t <- g10_scores()
    sel <- p10_select(t, 10)
    w <- p10_weights(sel, scheme = input$g10_scheme, cap = input$g10_cap)
    list(sel = sel, w = w)
  })

  output$g10_weights <- shiny::renderTable({
    p <- g10_pick()
    sel <- p$sel
    w <- p$w
    cap <- as.numeric(input$g10_capital)
    amt <- as.numeric(w) * cap
    data.frame(
      排名 = seq_len(nrow(sel)),
      股票 = paste0(sel$name, "（", sel$symbol, "）"),
      `0010 分數` = g10_num(sel$p10_score, 1),
      Growth = g10_num(sel$growth_score, 1),
      Risk = g10_num(sel$risk_score, 1),
      Stability = g10_num(sel$stability_score, 1),
      GARCH = g10_num(sel$garch_score, 1),
      預測波動 = g10_pct(sel$forecast_vol, 1, FALSE),
      權重 = paste0(round(as.numeric(w) * 100, 1), "%"),
      金額 = formatC(round(amt), format = "d", big.mark = ","),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$g10_cost <- shiny::renderTable({
    p <- g10_pick()
    cap <- as.numeric(input$g10_capital)
    amt <- as.numeric(p$w) * cap

    buy_fee <- sum(pmax(P10_COST$min_fee, amt * P10_COST$fee))
    buy_slip <- sum(amt * P10_COST$slip)
    sell_fee <- sum(pmax(P10_COST$min_fee, amt * P10_COST$fee))
    sell_tax <- sum(amt * P10_COST$tax)
    sell_slip <- sum(amt * P10_COST$slip)

    out <- data.frame(
      項目 = c("買進手續費", "買進滑價", "賣出手續費", "證交稅（賣出）", "賣出滑價",
              "建倉成本合計", "假設全數賣出之總成本", "佔投入金額（%）"),
      金額 = c(buy_fee, buy_slip, sell_fee, sell_tax, sell_slip,
              buy_fee + buy_slip,
              buy_fee + buy_slip + sell_fee + sell_tax + sell_slip,
              (buy_fee + buy_slip + sell_fee + sell_tax + sell_slip) / cap * 100),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    out$金額 <- ifelse(out$項目 == "佔投入金額（%）",
                       paste0(sprintf("%.2f", out$金額), "%"),
                       formatC(round(out$金額), format = "d", big.mark = ","))
    out
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$g10_next_date <- shiny::renderUI({
    d <- as.data.frame(data())
    nd <- p10_next_rebalance(max(d$date, na.rm = TRUE))
    div(class = "time-info",
        strong("下一次再平衡日："), format(nd, "%Y-%m-%d"),
        "（每季一次；評分使用當日以前資料，持有 63 個交易日）")
  })

  # ----------------------------------------------------------
  # 3-1. 持有追蹤（以上一期季末名單為基準）
  # ----------------------------------------------------------

  g10_track_data <- shiny::reactive({
    tryCatch({
      mt <- g10_mats()
      dates <- mt$dates
      m <- mt$m
      pm <- mt$pm

      # 最近一個「已結束」的季末：必須排除資料最後一天所屬的本季，
      # 否則買進日 = 今天，每檔報酬永遠是 0，而且會跟現在的名單完全一樣。
      key <- paste0(format(dates, "%Y"), "Q", ceiling(as.integer(format(dates, "%m")) / 3))
      lastq <- tapply(seq_along(dates), key, max)
      idx_all <- sort(as.integer(lastq))
      idx_all <- idx_all[idx_all < nrow(m)]
      if (!length(idx_all)) stop("資料裡找不到已結束的季度")

      i <- max(idx_all)
      if (i < 300) stop("最近季末之前的交易日不足 300 天")

      sub <- m[1:i, , drop = FALSE]
      vs <- mt$vol[1:i, , drop = FALSE]

      sc <- p10_matrix_scores(sub, vs, preset = input$g10_preset)
      if (is.null(sc) || !nrow(sc)) stop("該季無法計算分數（樣本不足）")
      sc <- sc[sc$symbol != "0050", , drop = FALSE]
      sc <- sc[is.finite(sc$p10_score), , drop = FALSE]
      if (nrow(sc) < 3) stop("有效分數不足 3 檔")

      sel <- utils::head(sc[order(-sc$p10_score), , drop = FALSE], 10)
      ww <- p10_weights(sel, scheme = input$g10_scheme, cap = input$g10_cap)

      buy <- unname(pm[i, sel$symbol]); names(buy) <- sel$symbol
      now <- unname(pm[nrow(pm), sel$symbol]); names(now) <- sel$symbol
      for (k in seq_along(now)) {
        if (!is.finite(now[k])) {
          col <- pm[, sel$symbol[k]]
          jj <- suppressWarnings(max(which(is.finite(col))))
          if (is.finite(jj)) now[k] <- col[jj]
        }
      }
      ret <- now / buy - 1

      list(ok = TRUE, date = dates[i], sel = sel, w = ww,
           buy = buy, now = now, ret = ret,
           i = i, nrow_m = nrow(m), n_scored = nrow(sc))
    }, error = function(e) {
      list(ok = FALSE, msg = conditionMessage(e))
    })
  })

  g10_cmp <- shiny::reactive({
    t <- g10_track_data()
    if (!isTRUE(t$ok)) return(NULL)
    p <- tryCatch(g10_pick(), error = function(e) NULL)
    if (is.null(p) || is.null(p$w) || !length(p$w)) return(NULL)

    prev <- names(t$w)
    cur <- names(p$w)

    drift <- t$w * (1 + t$ret[names(t$w)])
    drift[!is.finite(drift)] <- 0
    if (sum(drift) > 0) drift <- drift / sum(drift)

    all_sym <- union(prev, cur)
    a <- ifelse(all_sym %in% names(drift), drift[all_sym], 0)
    b <- ifelse(all_sym %in% names(p$w), p$w[all_sym], 0)

    list(new = setdiff(cur, prev), hold = intersect(cur, prev), drop = setdiff(prev, cur),
         turnover = sum(abs(b - a), na.rm = TRUE) / 2)
  })

  output$g10_n_new <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(length(c$new), " 檔")
  })

  output$g10_n_hold <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(length(c$hold), " 檔")
  })

  output$g10_n_drop <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(length(c$drop), " 檔")
  })

  output$g10_turnover_est <- shiny::renderText({
    c <- g10_cmp(); if (is.null(c)) return("—"); paste0(round(c$turnover * 100, 1), "%")
  })

  output$g10_track <- shiny::renderTable({
    t <- g10_track_data()
    if (!isTRUE(t$ok)) return(data.frame(說明 = t$msg, check.names = FALSE))

    g <- g10_store()
    fv <- NULL
    if (!is.null(g)) fv <- g$forecast_vol[match(t$sel$symbol, g$symbol)]
    if (is.null(fv) || all(!is.finite(fv))) fv <- t$sel$forecast_vol
    bad <- !is.finite(fv)
    fv[bad] <- t$sel$forecast_vol[bad]

    h <- 21
    s <- fv / sqrt(252) * sqrt(h)
    stop_px <- t$buy * (1 - input$g10_k_stop * s)
    take_px <- t$buy * (1 + input$g10_k_take * s)

    status <- ifelse(t$now <= stop_px, "已觸停損",
                     ifelse(t$now >= take_px, "已觸停利", "持有中"))
    status[is.na(status)] <- "—"

    data.frame(
      股票 = paste0(get_stock_name(t$sel$symbol), "（", t$sel$symbol, "）"),
      權重 = paste0(round(as.numeric(t$w) * 100, 1), "%"),
      買進價 = ifelse(is.finite(t$buy), sprintf("%.2f", t$buy), "—"),
      現價 = ifelse(is.finite(t$now), sprintf("%.2f", t$now), "—"),
      報酬 = g10_pct(t$ret, 2),
      停損價 = ifelse(is.finite(stop_px), sprintf("%.2f", stop_px), "—"),
      停利價 = ifelse(is.finite(take_px), sprintf("%.2f", take_px), "—"),
      狀態 = status,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$g10_track_note <- shiny::renderUI({
    t <- g10_track_data()
    if (!isTRUE(t$ok)) {
      return(div(class = "warning-note",
                 strong("無法計算持有追蹤："), t$msg))
    }
    g <- g10_store()
    src <- if (is.null(g)) "EWMA（尚未估計 GARCH）" else "GARCH（最新一期）"
    div(class = "time-info",
        strong("基準日："), format(t$date, "%Y-%m-%d"),
        "｜波動估計：", src,
        "｜資料截止：", format(max(g10_mats()$dates), "%Y-%m-%d"),
        "｜季末索引 ", t$i, " / ", t$nrow_m, " 日，有效評分 ", t$n_scored, " 檔")
  })

  # ----------------------------------------------------------
  # 4. 季度再平衡回測
  # ----------------------------------------------------------

  g10_bt <- shiny::eventReactive(input$g10_run_bt, {
    d <- as.data.frame(data())
    d <- d[d$symbol %in% c("0050", stock_info$symbol) & is.finite(d$log_return), , drop = FALSE]
    d <- d[order(d$date), , drop = FALSE]

    wide <- as.data.frame(
      tidyr::pivot_wider(d[, c("date", "symbol", "log_return")],
                         names_from = symbol, values_from = log_return)
    )
    shiny::req(ncol(wide) >= 20)

    dates <- as.Date(wide$date)
    m <- as.matrix(wide[, -1, drop = FALSE])
    rownames(m) <- as.character(dates)
    storage.mode(m) <- "double"

    vol <- p10_vol_matrix(m)

    key <- paste0(format(dates, "%Y"), "Q", ceiling(as.integer(format(dates, "%m")) / 3))
    last_idx <- tapply(seq_along(dates), key, max)
    idx <- sort(as.integer(last_idx))
    idx <- idx[idx > 800 & idx < (length(dates) - 63)]
    shiny::req(length(idx) >= 4)

    withProgress(message = "季度回測中…", value = 0, {
      p10_backtest(
        m, vol, idx,
        hold = 63, n = 10,
        preset = input$g10_preset,
        scheme = input$g10_scheme,
        cap = input$g10_cap,
        progress = function(k, n, s) {
          setProgress(k / n, detail = paste0("第 ", k, " / ", n, " 季"))
        }
      )
    })
  })

  output$g10_bt_stats <- shiny::renderTable({
    r <- g10_bt()
    shiny::req(!is.null(r))
    s <- r$stats
    data.frame(
      指標 = c("回測季數", "累積報酬（扣成本）", "年化報酬", "年化波動", "Sharpe", "最大回撤",
              "季勝率（vs 0050）", "平均換手率", "累計成本", "0050 同期累積報酬"),
      數值 = c(as.character(s$n_periods),
              g10_pct(s$cum_ret), g10_pct(s$ann_ret), g10_pct(s$ann_vol, 2, FALSE),
              g10_num(s$sharpe, 2), g10_pct(s$mdd),
              paste0(round(s$win_vs_bench * 100, 1), "%"),
              paste0(round(s$avg_turnover * 100, 1), "%"),
              g10_pct(s$total_cost, 2, FALSE),
              g10_pct(s$bench_cum)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)

  output$g10_bt_curve <- shiny::renderPlot({
    r <- g10_bt()
    shiny::req(!is.null(r))
    q <- r$quarters
    df <- rbind(
      data.frame(date = q$date, value = q$equity, series = "0010（扣成本）"),
      data.frame(date = q$date, value = q$equity_0050, series = "0050")
    )
    ggplot2::ggplot(df, ggplot2::aes(x = date, y = value, colour = series)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::labs(title = "季度再平衡淨值曲線", x = NULL, y = "淨值（起始 = 1）", colour = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom",
                     plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  output$g10_bt_quarters <- shiny::renderTable({
    r <- g10_bt()
    shiny::req(!is.null(r))
    q <- r$quarters
    data.frame(
      評分日 = format(q$date, "%Y-%m-%d"),
      換手率 = paste0(round(q$turnover * 100, 1), "%"),
      成本 = g10_pct(q$cost_rate, 2, FALSE),
      毛報酬 = g10_pct(q$gross, 2),
      淨報酬 = g10_pct(q$net, 2),
      `0050` = g10_pct(q$bench, 2),
      持股 = q$top,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
