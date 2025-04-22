


shinyUI(navbarPage("avalitools",

  ## page 1: Carregar dados ----
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

  ## page 2 Escolher variável ----

  tabPanel("⚙️ Escolher Transformaçoes",

      fluidPage(
        dataTableOutput("transfTable")
      )


  ),


  ## page 3: Verificar Pressupostos ----
  tabPanel("📊 Verificar pressupostos",
    fluidPage(
      titlePanel("Verificação dos Pressupostos da Regressão"),
      tabsetPanel(type = "pills",

        ## tab: Geral
        tabPanel("📌 Geral",
          fluidRow(
            # lateral column
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

            # main column
            column(9,
              h4("Resultado"),
              dataTableOutput("geralOutput"),
              br()
            )
          )
        ),  

        ## tab: Resíduos
        tabPanel("📈 Resíduos", 
          fluidRow(
            # lateral column
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

            # main column
            column(9,
              h4("Resultado"),
              plotOutput("normOutput"),
              br()
            )
          )
        ),
        
        ## tab: Auto - Correlacao
        tabPanel("🔁 Auto-Correlação", 
          fluidRow(
            # lateral column
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

            # main column
            column(9,
              h4("Resultado"),
              plotOutput("corrOutput"),
              br()
            )
          )
        ),

        ## tab : Variáveis
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

        ## tab: Projeçao
        tabPanel("📌 Projeção",
          fluidRow(
            # lateral column
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

            # main column
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

  ## page 3: Relatório ----
  tabPanel("📝 Gerar relatório",
    fluidPage(
      titlePanel("Gerar Relatório"),
      p("Aqui você pode exportar os resultados para um arquivo .docx."),
      downloadButton("download", "Gere o Relatorio!")

    )
  )
))
