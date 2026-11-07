      # ======================================================
      # v4 新增：本期 0010 —— 貼在 0050 總覽 的內容之前
      # ======================================================

      div(
        class = "section-box",
        h2("本期 0010"),
        p(class = "page-description",
          "從 50 檔成分股中，以動能 70% ＋ 風險穩定 30% 選出 10 檔，每季更新一次。"),

        fluidRow(
          column(3, div(class = "info-card",
                        div(class = "info-card-title", "本期報酬"),
                        h3(class = "info-card-value", textOutput("v4_k_ret", inline = TRUE)))),
          column(3, div(class = "info-card",
                        div(class = "info-card-title", "0050 同期"),
                        h3(class = "info-card-value", textOutput("v4_k_bench", inline = TRUE)))),
          column(3, div(class = "info-card",
                        div(class = "info-card-title", "超額報酬"),
                        h3(class = "info-card-value", textOutput("v4_k_excess", inline = TRUE)))),
          column(3, div(class = "info-card",
                        div(class = "info-card-title", "本期最大回撤"),
                        h3(class = "info-card-value", textOutput("v4_k_mdd", inline = TRUE))))
        ),

        h3("名單"),
        tableOutput("v4_home_table"),

        h3("本期回撤"),
        plotOutput("v4_home_dd", height = "360px"),
        div(class = "plot-explanation", textOutput("v4_home_dd_text", inline = TRUE)),

        h3("10 檔權重與金額"),
        tableOutput("v4_home_weight"),
        p(class = "page-description", textOutput("v4_home_note", inline = TRUE))
      ),

      div(
        class = "section-box",
        tags$details(
          tags$summary("查看 50 檔完整分數與篩選結果"),
          br(),
          tableOutput("v4_all_table")
        )
      ),
