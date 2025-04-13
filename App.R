rm(list = ls())

# install.packages("shinycssloaders")

source("modelo.R")

library(dplyr)
library(ggplot2)
library(ppcor)
library(avalitools)
library(readxl)
library(corrgram)
library(conflicted)
library(knitr)
library(shiny)
library(bslib)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

#==============================================

ui <- bootstrapPage(page_navbar(
  nav_panel(
    "Laudo",
    layout_columns(
      # set a toggle
      fileInput('file', label = NULL),
      actionButton('reset', 'Reset Input'),
      actionButton('gerarLaudo', 'Gerar laudo!', class = "btn-success"),
      col_widths = c(8, 1, 3)
    ),

    layout_column_wrap(
      card(
        verbatimTextOutput("summary"),
        # ggplot2Output
      ),
      card(
        h5("Gráfico de Aderência Observado x Estimado"),
        shinycssloaders::withSpinner(
          plotOutput("outlierPlot")
        ),
        p(
          "Este gráfico descreve a relação entre o que estimamos de uma variável e de outra, (...)",
          style = "color: gray"
        ),
      ),
      card(
        h5("Gráfico de Aderência Observado x Estimado"),
        shinycssloaders::withSpinner(
          plotOutput("aderenciaPlot")
        ),
        p(
          "Este gráfico descreve a relação entre o que estimamos de uma variável e de outra, (...)",
          style = "color: gray"
        ),
      ),
      col_widths = c(2, 4, 6)
    ),
  ),
  nav_panel("B", "Page B content"),
  nav_panel("C", "Page C content"),
  title = "QUALITOOLS",
  id = "page",
))

server <- shinyServer(function(input, output, session) {
  values <- reactiveValues(
    upload_state = NULL
  )

  observeEvent(input$gerarLaudo, {
    print("Gerando Laudo")
    if (!is.null(input$file)) {
      laudoData = gerarLaudo(input$file$datapath)
      # get data
      # descDf = laudoData$descDf
      # avalSummary_df = laudoData$avalSummary_df
      # outliersDf = laudoData$outliersDf
      # anova = laudoData$anova
      # elast_list = laudoData$elast_list

      tabela_observado_estimado = laudoData$tabela_observado_estimado
      tabela_estimado_residuo = laudoData$tabela_estimado_residuo
      coeficientes = laudoData$coeficientes
      df_info_complementares = laudoData$df_info_complementares
      df_dados = laudoData$df_dados
      modelo_aov = laudoData$modelo_aov
      transformacoes = laudoData$transformacoes

      # ====
      df_estat_descr <- descr_avalitools(df_dados)
      cat_df(
        df_estat_descr,
        colnames = colnames(df_estat_descr),
        rownames = vari_uti
      )

      df_sumario_avaliacao <- summary_avalitools(modelo_reg)
      cat_df(df_sumario_avaliacao, colnames = colnames(df_sumario_avaliacao))

      tabela_anova <- as_avaliation_anova(modelo_aov)
      cat_df(tabela_anova, colnames = colnames(tabela_anova)) # !

      lista_elasticidade <- elasticity_plots(
        modelo = modelo_reg,
        df = df_dados,
        trans = transformacoes,
        vari_uti
      )
      # ====

      print("gerando tabela observado/estimado")

      output$aderenciaPlot = renderPlot({
        ggplot(data = tabela_observado_estimado) +
          geom_point(aes(x = v1, y = v2), color = "blue", fill = "white") +
          geom_abline(col = "yellow") +
          geom_abline(
            aes(slope = coeficientes[2], intercept = coeficientes[1]),
            col = "red"
          ) +
          labs(x = "Observado", y = "Estimado") +
          scale_y_continuous(
            labels = scales::number_format(big.mark = ".", accuracy = 1)
          ) +
          scale_x_continuous(
            labels = scales::number_format(big.mark = ".", accuracy = 1)
          ) +
          theme_bw()
      })

      output$outlierPlot = renderPlot({
        ggplot(tabela_estimado_residuo) +
          geom_point(
            aes(x = est, y = res, fill = out),
            col = "blue4",
            stroke = 0.84,
            shape = 21,
            show.legend = F
          ) +
          geom_abline(aes(intercept = 0, slope = 0), col = "blue") +
          geom_abline(aes(intercept = 2, slope = 0), col = "red") +
          geom_abline(aes(intercept = -2, slope = 0), col = "red") +
          labs(y = "Residuos/DP", x = "Valores Estimados") +
          scale_y_continuous(limits = c(-3.5, 3.5), n.breaks = 12) +
          scale_x_continuous(
            labels = scales::number_format(big.mark = ".", accuracy = 1)
          ) +
          scale_fill_manual(values = c("blue", "red")) +
          theme_bw()
      })
    } else {
      showNotification("Nenhum arquivo enviado!", type = "warning")
    }
  })

  observeEvent(input$file, {
    values$upload_state <- 'uploaded'
  })

  observeEvent(input$reset, {
    values$upload_state <- 'reset'
    print("reset")
  })

  file_input <- reactive({
    print("file_input")
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$file)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })

  output$summary <- renderText({
    print("summary")
    return(paste("Uploaded file:", file_input()$name))
  })

  output$path <- renderText({
    print("render")
    return(paste("Path:", input$file$datapath))
  })
})

shinyApp(ui = ui, server = server)
