if(!require("pacman", quietly = T)){
  install.packages("pacman")
}

pacman::p_load (shiny,avalitools,ggcorrplot,rlang,quarto)




shinyUI(navbarPage("avalitools",

  ## Página 1: Carregar dados ----
  tabPanel("📁 Carregar dados",
    fluidPage(
      titlePanel("Carregar dados da avaliação"),
      fluidRow(
        column(6,
          fileInput('file', label = "Selecione o arquivo de avaliação")
        )
      ),
      br(),
      hr(),
      h4("Visualização dos dados carregados"),
      dataTableOutput("summary")  
    )
  ),

  ## Pagina 2 Escolher variável ----

  tabPanel("⚙️ Escolher Transformaçoes",

      fluidPage(
        dataTableOutput("transfTable")
      )


  ),


  ## Página 2: Verificar Pressupostos ----
  tabPanel("📊 Verificar pressupostos",
    fluidPage(
      titlePanel("Verificação dos Pressupostos da Regressão"),
      tabsetPanel(type = "pills",

        ## Subaba: Geral
        tabPanel("📌 Geral",
          fluidRow(
            # Coluna lateral com seleção
            column(3,
              h4("Selecionar tabela"),
              radioButtons("geralSel", label = NULL,
                choices = c(
                  "Informações Básicas" = "basicInfo",
                  "Informações Complementares" = "complementaryInfo",
                  "Estatísticas do Modelo" = "basicStats"
                ),
                selected = "basicInfo"
              )
            ),

            # Coluna principal com tabela exibida
            column(9,
              h4("Resultado"),
              dataTableOutput("geralOutput"),
              br()
            )
          )
        ),  # <-- Aqui a vírgula foi adicionada para separar as abas

        ## Subabas futuras
        tabPanel("📈 Resíduos", 
          fluidRow(
            # Coluna lateral com seleção
            column(3,
              h4("Selecionar gráfico"),
              radioButtons("normSel", label = NULL,
                choices = c(
                  "Histograma Resíduos" = "histRes",
                  "QQplot Resíduos" = "qqplot",
                  "Resíduos/DP vs. Est." = "homoRes"
                ),
                selected = "histRes"
              )
            ),

            # Coluna principal com tabela exibida
            column(9,
              h4("Resultado"),
              plotOutput("normOutput"),
              br()
            )
          )
        ),
        
        tabPanel("🔁 Auto-Correlação", 
          fluidRow(
            # Coluna lateral com seleção
            column(3,
              h4("Selecionar tabela"),
              radioButtons("corrSel", label = NULL,
                choices = c(
                  "Correlações Parciais" = "corr",
                  "Correlações com Influência" = "corrInf"
                ),
                selected = "corr"
              )
            ),

            # Coluna principal com tabela exibida
            column(9,
              h4("Resultado"),
              plotOutput("corrOutput"),
              br()
            )
          )
        ),
        tabPanel("📊 Variáveis", 
          fluidPage(
            titlePanel("Gráfico Interativo com Seleção de Variáveis"),
          
            sidebarLayout(
              sidebarPanel(
                selectInput("var_x", "Escolha a variável X:", 
                            choices = NULL),
          
                selectInput("var_y", "Escolha a variável Y:", 
                            choices = NULL)
              ),
          
              mainPanel(
                plotOutput("varsPlot")
              )
            )
          )
        ),
        tabPanel("📌 Projeção",
          fluidRow(
            # Coluna lateral com seleção
            column(3,
              h4("Selecionar tabela"),
              radioButtons("projSel", label = NULL,
                choices = c(
                  "Dados do Imóvel" = "imoInfo",
                  "Projeção" = "proj"
                ),
                selected = "imoInfo"
              )
            ),

            # Coluna principal com tabela exibida
            column(9,
              h4("Resultado"),
              dataTableOutput("projOutput"),
              br()
            )
          )
        )
      )
    )
  ),

  ## Página 3: Relatório ----
  tabPanel("📝 Gerar relatório",
    fluidPage(
      titlePanel("Gerar Relatório"),
      p("Aqui você pode exportar os resultados para um arquivo .docx."),
      downloadButton("download", "Gere o Relatorio!")

      # Conteúdo futuro aqui
    )
  )
))
