    # ========================================================
    # 9. GARCH 預測與 0010 組合（新增頁籤）
    #    插入位置：放在「研究方法與說明」tabPanel 之前
    # ========================================================

    tabPanel(
      "GARCH 與 0010",

      div(
        class = "section-box",
        h2("GARCH 波動預測與 0010 組合"),
        p(class = "page-description",
          "這一頁用 GARCH(1,1) 預測每檔成分股「下一季的波動」，再把報酬、風險、穩定性與波動預測四個面向合成 0010 分數，選出 10 檔並給出持股權重與交易成本。"),

        div(class = "warning-note",
            strong("先講清楚："),
            "GARCH 預測的是「波動（變異數）」，不是「報酬」。它不會告訴你哪檔會漲，只會告訴你哪檔接下來可能比較晃。所以它在 0010 裡的角色是風險調整，不是 alpha。"),

        tabsetPanel(
          id = "g10_tabs",
          type = "pills",

          # --------------------------------------------------
          # 1. 單檔 GARCH 模型
          # --------------------------------------------------
          tabPanel(
            "GARCH 單檔模型",
            br(),
            fluidRow(
              column(4, selectInput("g10_stock", "選擇股票：",
                                    choices = setNames(stock_info$symbol,
                                                       paste0(stock_info$name, " (", stock_info$symbol, ")")),
                                    selected = "2330")),
              column(4, selectInput("g10_window", "估計樣本（交易日）：",
                                    choices = c("近 2 年" = 500, "近 4 年" = 1000, "近 6 年" = 1500),
                                    selected = 1000)),
              column(4, div(class = "time-info",
                            strong("預測期："), "未來 63 個交易日（約一季）"))
            ),

            h3("預測結果"),
            fluidRow(
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "預測年化波動（下一季）"),
                            h3(class = "info-card-value", textOutput("g10_kpi_fvol", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "近一年實現波動"),
                            h3(class = "info-card-value", textOutput("g10_kpi_rvol", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "預測／實現"),
                            h3(class = "info-card-value", textOutput("g10_kpi_ratio", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "波動狀態"),
                            h3(class = "info-card-value", textOutput("g10_kpi_regime", inline = TRUE))))
            ),

            h3("參數估計（最大概似法）"),
            tableOutput("g10_param_table"),
            div(class = "plot-explanation", strong("怎麼看？"),
                "α 是「今天的 shock 對明天波動的影響」，β 是「波動本身的記憶」。α+β 越接近 1，代表波動一旦被拉高就越難退回去；半衰期就是把衝擊消掉一半所需的天數。"),

            h3("條件波動（模型配適）"),
            plotOutput("g10_vol_path", height = "380px"),

            h3("未來 63 日波動預測"),
            plotOutput("g10_fan", height = "340px"),

            h3("模型診斷"),
            tableOutput("g10_diag"),
            div(class = "plot-explanation", strong("怎麼看？"),
                "Ljung-Box 的 p 值大於 0.05，代表標準化殘差已經沒有明顯的自我相關，模型大致夠用；若 p 值很小，代表還有結構沒抓到，該檔的預測請保守看待。")
          ),

          # --------------------------------------------------
          # 2. 波動預測總表
          # --------------------------------------------------
          tabPanel(
            "波動預測總表",
            br(),
            actionButton("g10_run_all", "估計全部成分股 GARCH（約 5–15 秒）", class = "btn-primary"),
            div(class = "method-note",
                "第一次使用請先按這個按鈕。結果會快取，切換頁籤不會重算。"),
            h3("50 檔成分股波動預測"),
            tableOutput("g10_all_table"),
            h3("預測／實現波動比"),
            plotOutput("g10_ratio_bar", height = "700px"),
            div(class = "plot-explanation", strong("怎麼看？"),
                "比值 < 1 代表模型預期未來波動比過去一年平緩；> 1 代表預期波動升溫。這是一個「方向」訊號，不是「幅度」預測。")
          ),

          # --------------------------------------------------
          # 3. 0010 組合（本期）
          # --------------------------------------------------
          tabPanel(
            "0010 組合",
            br(),
            div(class = "reading-guide", strong("0010 是什麼？"),
                "從 50 檔成分股中，用四因子綜合分數選出前 10 檔，並依波動度反比給權重（單檔上限 20%）。每季重新評分一次。"),

            fluidRow(
              column(3, selectInput("g10_preset", "權重組合：",
                                    choices = stats::setNames(names(P10_PRESET_LABEL), unname(P10_PRESET_LABEL)),
                                    selected = "A")),
              column(3, selectInput("g10_scheme", "持股權重方式：",
                                    choices = c("波動度反比（GARCH）" = "inv_vol",
                                                "等權重" = "equal",
                                                "分數加權" = "score"),
                                    selected = "inv_vol")),
              column(3, sliderInput("g10_cap", "單檔權重上限：", min = 0.10, max = 0.30, value = 0.20, step = 0.01)),
              column(3, numericInput("g10_capital", "模擬投入金額（元）：", value = 1000000, min = 100000, step = 100000))
            ),

            h3("本期執行摘要"),
            fluidRow(
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "本季新買"),
                            h3(class = "info-card-value", textOutput("g10_n_new", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "續抱"),
                            h3(class = "info-card-value", textOutput("g10_n_hold", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "下季剔除"),
                            h3(class = "info-card-value", textOutput("g10_n_drop", inline = TRUE)))),
              column(3, div(class = "info-card",
                            div(class = "info-card-title", "預估換手率"),
                            h3(class = "info-card-value", textOutput("g10_turnover_est", inline = TRUE))))
            ),
            div(class = "plot-explanation", strong("怎麼看？"),
                "「新買」是本季名單新出現的股票；「續抱」是上季已在名單內；「剔除」是上季持有但本季不再入選，下次再平衡時賣出。預估換手率已考慮持股自然漂移。"),

            h3("本期 0010 名單與權重"),
            tableOutput("g10_weights"),

            h3("交易成本試算"),
            tableOutput("g10_cost"),
            div(class = "plot-explanation", strong("怎麼算？"),
                "手續費 0.1425%（每筆最低 20 元，買賣各收一次）、證交稅 0.3%（僅賣出）、滑價 0.1%（可調）。建倉成本 = 買進手續費 + 滑價；換股成本另計換手率。"),

            h3("下一次再平衡"),
            uiOutput("g10_next_date"),

            h3("持有追蹤與風控（以上一期季末名單為基準）"),
            p(class = "page-description",
              "停損與停利距離不是固定的 5%，而是用 GARCH 的預測波動決定：距離 = k × 預測 21 日波動。波動大的股票停損自然拉寬，避免被正常震盪洗出場；波動小的股票停損收緊，資金效率較高。"),
            fluidRow(
              column(6, sliderInput("g10_k_stop", "停損倍數 k", min = 1, max = 3, value = 2, step = 0.1)),
              column(6, sliderInput("g10_k_take", "停利倍數 k", min = 2, max = 5, value = 3, step = 0.1))
            ),
            tableOutput("g10_track"),
            uiOutput("g10_track_note"),
            div(class = "plot-explanation", strong("怎麼看？"),
                "停損價與停利價都以「買進價」為基準計算，不隨價格上移（不是移動停損）。狀態顯示是否已觸及。真實交易仍需考慮流動性與跳空，這裡只做統計上的參考距離。")
          ),

          # --------------------------------------------------
          # 4. 季度再平衡回測
          # --------------------------------------------------
          tabPanel(
            "季度再平衡回測",
            br(),
            div(class = "reading-guide", strong("這頁在驗證什麼？"),
                "每一季只用當時已知的資料重新評分、選股、給權重，持有 63 個交易日後再換股，並扣掉手續費、證交稅與滑價。看的是「扣完成本之後還剩多少」。"),
            actionButton("g10_run_bt", "執行季度回測（約 20–60 秒）", class = "btn-primary"),
            div(class = "method-note",
                "歷史回測的波動估計採 EWMA（GARCH 家族的 λ=0.94 特例），因為它不需要每季重新估計參數，也完全不使用未來資料；最新一期的預測才用完整 GARCH(1,1) MLE。"),
            h3("績效摘要"),
            tableOutput("g10_bt_stats"),
            h3("淨值曲線（已扣成本）"),
            plotOutput("g10_bt_curve", height = "420px"),
            h3("各季明細"),
            tableOutput("g10_bt_quarters")
          )
        )
      )
    ),
