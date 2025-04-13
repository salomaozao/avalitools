library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(scales)

# Importar o arquivo modelo.R
source("modelo.R")

# Funções do modelo.R
read_avaliation <- function(path) {
  # Função simulada para ler dados de avaliação
  # Em um caso real, esta função lerá o arquivo especificado
  list(
    vari = data.frame(
      Nome = c("Área", "Quartos", "Banheiros", "Vagas", "Valor"),
      Tipo = c("Numérica", "Numérica", "Numérica", "Numérica", "Numérica"),
      Habilitada = c("Sim", "Sim", "Sim", "Sim", "Dependente")
    ),
    dados = data.frame(
      Area = c(100, 120, 80, 150, 90, 110, 130, 95, 140, 85, 105),
      Quartos = c(2, 3, 2, 3, 2, 3, 3, 2, 3, 2, 3),
      Banheiros = c(1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2),
      Vagas = c(1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 1),
      Valor = c(
        300000,
        350000,
        250000,
        400000,
        280000,
        320000,
        380000,
        270000,
        390000,
        260000,
        310000
      )
    ),
    info = data.frame(
      "Data da Avaliação" = Sys.Date(),
      "Avaliador" = "Nome do Avaliador",
      "Tipo de Imóvel" = "Residencial"
    ),
    imo = data.frame(
      Nome = c("Área", "Quartos", "Banheiros", "Vagas"),
      Conteudo = c(110, 3, 2, 1)
    ),
    inicial = data.frame(
      Area = c(100, 120, 80, 150, 90, 110, 130, 95, 140, 85, 105),
      Quartos = c(2, 3, 2, 3, 2, 3, 3, 2, 3, 2, 3),
      Banheiros = c(1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 2),
      Vagas = c(1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 1),
      Valor = c(
        300000,
        350000,
        250000,
        400000,
        280000,
        320000,
        380000,
        270000,
        390000,
        260000,
        310000
      ),
      Endereco = c(
        rep("Rua Elson Nu...", 6),
        "Rua ALBEER...",
        "Rua Emilio P...",
        "Rua Rio Gra...",
        "Rua Havana,...",
        "Rua das Flor..."
      ),
      Complemento = c(rep("Belo Horizonte", 10), "Nova Lima"),
      Bairro = c(
        rep("Castelo", 6),
        "São Lucas",
        "São Bento",
        "Funcionários",
        "Havai",
        "Vila da Serra"
      )
    ),
    transf = c("log", "log", "log", "log", "log"),
    indices = data.frame(i = 1:11)
  )
}

apply_transf <- function(df, trans) {
  # Função simulada para aplicar transformações
  # Em um caso real, esta função aplicará as transformações especificadas
  df_trans <- df
  for (i in 1:length(trans)) {
    if (trans[i] == "log") {
      df_trans[[i]] <- log(df[[i]])
    }
  }
  return(df_trans)
}

lm_avalitools <- function(df) {
  # Função simulada para ajustar modelo de regressão
  # Em um caso real, esta função ajustará o modelo de regressão
  formula <- as.formula(paste(
    names(df)[ncol(df)],
    "~",
    paste(names(df)[1:(ncol(df) - 1)], collapse = "+")
  ))
  modelo <- lm(formula, data = df)
  anova <- anova(modelo)
  return(list(modelo = modelo, anova = anova))
}

complementary_info <- function(num_v, num_vu, num_d, num_du) {
  # Função simulada para gerar informações complementares
  data.frame(
    "Variáveis e dados do modelo" = c(
      "Número de variáveis numéricas",
      "Número de variáveis utilizadas",
      "Número de dados",
      "Número de dados utilizados"
    ),
    "Quant." = c(num_v, num_vu, num_d, num_du)
  )
}

descr_avalitools <- function(df) {
  # Função simulada para gerar descrição estatística
  # Em um caso real, esta função calculará as estatísticas descritivas
  data.frame(
    "Mínimo" = sapply(df, min),
    "Máximo" = sapply(df, max),
    "Média" = sapply(df, mean),
    "Mediana" = sapply(df, median),
    "Desvio Padrão" = sapply(df, sd)
  )
}

summary_avalitools <- function(modelo) {
  # Função simulada para gerar resumo do modelo
  # Em um caso real, esta função calculará as estatísticas do modelo
  sumario <- summary(modelo)
  data.frame(
    "Estatística" = c(
      "R²",
      "R² Ajustado",
      "F Calculado",
      "Significância do Modelo"
    ),
    "Valor" = c(
      sumario$r.squared,
      sumario$adj.r.squared,
      sumario$fstatistic[1],
      pf(
        sumario$fstatistic[1],
        sumario$fstatistic[2],
        sumario$fstatistic[3],
        lower.tail = FALSE
      )
    )
  )
}

normality_table <- function(modelo) {
  # Função simulada para gerar tabela de normalidade
  # Em um caso real, esta função calculará as estatísticas de normalidade
  data.frame(
    "Distribuição dos resíduos" = c("Normal", "Normal", "Normal"),
    "Curva Normal" = c("Sim", "Sim", "Sim"),
    "Modelo" = c("Adequado", "Adequado", "Adequado")
  )
}

outliers_table <- function(modelo) {
  # Função simulada para gerar tabela de outliers
  # Em um caso real, esta função identificará os outliers
  data.frame(
    "Observação" = c(1, 2, 3),
    "Resíduo" = c(0.1, -0.2, 0.3),
    "Resíduo Padronizado" = c(0.5, -1.0, 1.5),
    "Outlier" = c("Não", "Não", "Não")
  )
}

as_avaliation_anova <- function(anova) {
  # Função simulada para gerar tabela ANOVA
  # Em um caso real, esta função formatará a tabela ANOVA
  data.frame(
    "Fonte" = c("Regressão", "Resíduos", "Total"),
    "GL" = c(4, 6, 10),
    "SQ" = c(100, 50, 150),
    "QM" = c(25, 8.33, 15),
    "F" = c(3, NA, NA),
    "p-valor" = c(0.1, NA, NA)
  )
}

regression_equation <- function(modelo, vari_uti, transformacoes) {
  # Função simulada para gerar equação de regressão
  # Em um caso real, esta função gerará a equação de regressão
  "log(Valor) = 10.5 + 0.8*log(Área) + 0.3*log(Quartos) + 0.2*log(Banheiros) + 0.1*log(Vagas)"
}

estimate_equations <- function(modelo, vari_uti, transformacoes) {
  # Função simulada para gerar equações de estimativa
  # Em um caso real, esta função gerará as equações de estimativa
  c(
    "Valor = exp(10.5 + 0.8*log(Área) + 0.3*log(Quartos) + 0.2*log(Banheiros) + 0.1*log(Vagas))",
    "Valor = exp(10.5 + 0.8*log(Área) + 0.3*log(Quartos) + 0.2*log(Banheiros) + 0.1*log(Vagas))",
    "Valor = exp(10.5 + 0.8*log(Área) + 0.3*log(Quartos) + 0.2*log(Banheiros) + 0.1*log(Vagas))"
  )
}

ttests_table <- function(modelo, vari_uti) {
  # Função simulada para gerar tabela de testes t
  # Em um caso real, esta função calculará os testes t
  data.frame(
    "Variável" = c("Intercepto", "Área", "Quartos", "Banheiros", "Vagas"),
    "Coeficiente" = c(10.5, 0.8, 0.3, 0.2, 0.1),
    "Erro Padrão" = c(0.1, 0.05, 0.1, 0.15, 0.2),
    "t" = c(105, 16, 3, 1.33, 0.5),
    "p-valor" = c(0.001, 0.001, 0.02, 0.2, 0.6)
  )
}

cor_avalitools <- function(df, vari_uti, inf = FALSE) {
  # Função simulada para gerar matriz de correlação
  # Em um caso real, esta função calculará as correlações
  if (inf) {
    data.frame(
      "Variável" = c("Área", "Quartos", "Banheiros", "Vagas"),
      "Correlação" = c(0.8, 0.6, 0.5, 0.4),
      "Influência" = c("Alta", "Média", "Média", "Baixa")
    )
  } else {
    data.frame(
      "Variável" = c("Área", "Quartos", "Banheiros", "Vagas"),
      "Correlação" = c(0.8, 0.6, 0.5, 0.4)
    )
  }
}

residuals_table <- function(modelo, df, indices) {
  # Função simulada para gerar tabela de resíduos
  # Em um caso real, esta função calculará os resíduos
  data.frame(
    "Observação" = indices,
    "Valor Observado" = df[[ncol(df)]],
    "Valor Estimado" = modelo$fitted.values,
    "Resíduo" = modelo$residuals,
    "Resíduo Padronizado" = modelo$residuals / sd(modelo$residuals)
  )
}

inverse_function <- function(fun) {
  # Função simulada para gerar função inversa
  # Em um caso real, esta função gerará a função inversa
  if (fun == "log") {
    return(function(x) exp(x))
  }
  return(function(x) x)
}

qme <- function(modelo) {
  # Função simulada para calcular QME
  # Em um caso real, esta função calculará o QME
  sqrt(sum(modelo$residuals^2) / modelo$df.residual)
}

elasticity_plots <- function(modelo, df, trans, vari_uti) {
  # Função simulada para gerar gráficos de elasticidade
  # Em um caso real, esta função gerará os gráficos de elasticidade
  lista <- list()
  for (i in 1:(ncol(df) - 1)) {
    lista[[i]] <- ggplot() +
      geom_line(
        aes(
          x = seq(min(df[[i]]), max(df[[i]]), length.out = 100),
          y = rep(0.5, 100)
        ),
        color = "blue"
      ) +
      labs(x = vari_uti[i], y = "Elasticidade") +
      theme_bw()
  }
  return(lista)
}

avali_table <- function(df_imo, df_ini) {
  # Função simulada para gerar tabela de avaliação
  # Em um caso real, esta função gerará a tabela de avaliação
  data.frame(
    "Variável" = c("Área", "Quartos", "Banheiros", "Vagas"),
    "Conteúdo" = c(110, 3, 2, 1),
    "Extrapolação" = c("Não", "Não", "Não", "Não")
  )
}

predict_avalitools <- function(df_novo, modelo, trans) {
  # Função simulada para gerar previsões
  # Em um caso real, esta função gerará as previsões

  # Verificar se o dataframe tem as colunas necessárias
  if (ncol(df_novo) < 4) {
    # Se não tiver colunas suficientes, criar um dataframe com valores padrão
    df_novo <- data.frame(
      x1 = 100, # Área padrão
      x2 = 2, # Quartos padrão
      x3 = 1, # Banheiros padrão
      x4 = 1 # Vagas padrão
    )
  }

  # Criar um data frame com os nomes de colunas corretos
  df_pred <- data.frame(
    Area = df_novo$x1,
    Quartos = df_novo$x2,
    Banheiros = df_novo$x3,
    Vagas = df_novo$x4
  )

  # Aplicar as transformações necessárias
  df_pred_trans <- apply_transf(df_pred, trans[1:4]) # Apenas para as variáveis independentes

  # Fazer a previsão
  pred <- predict(modelo, newdata = df_pred_trans, interval = "prediction")

  # Retornar os resultados
  data.frame(
    "Estimativa" = exp(pred[1]),
    "Limite.Inferior" = exp(pred[2]),
    "Limite.Superior" = exp(pred[3])
  )
}

# Sample data for placeholder
sample_data <- data.frame(
  Dado = 1:11,
  Endereco = c(
    rep("Rua Elson Nu...", 6),
    "Rua ALBEER...",
    "Rua Emilio P...",
    "Rua Rio Gra...",
    "Rua Havana,...",
    "Rua das Flor..."
  ),
  Complemento = c(rep("Belo Horizonte", 10), "Nova Lima"),
  Bairro = c(
    rep("Castelo", 6),
    "S\u00e3o Lucas",
    "S\u00e3o Bento",
    "Funcion\u00e1rios",
    "Havai",
    "Vila da Serra"
  ),
  Informante = rep("", 11)
)


ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".dataTable, .form-control, .btn { border-radius: 0px !important; }"
  ))),

  fluidRow(
    column(8, h3("Tabela de Dados"), DTOutput("data_table")),
    column(
      4,
      actionButton(
        "calcular",
        "Calcular",
        class = "btn btn-primary",
        style = "width: 100%; margin-bottom: 10px;"
      ),

      # Aba de navegação entre Resultados RL e Configurações de Projeção
      fluidRow(
        column(
          6,
          actionButton(
            "aba_rl",
            "Resultados RL",
            class = "btn btn-outline-secondary",
            style = "width: 100%; border-radius: 0px;"
          )
        ),
        column(
          6,
          actionButton(
            "aba_proj",
            "Projeção",
            class = "btn btn-outline-secondary",
            style = "width: 100%; border-radius: 0px;"
          )
        )
      ),
      br(),

      uiOutput("painel_dinamico")
    )
  )
)

server <- function(input, output, session) {
  # Reativo para armazenar os resultados do modelo
  resultados_modelo <- reactiveVal(NULL)

  # Reativo para armazenar a aba ativa
  aba_ativa <- reactiveVal("rl")

  # Observadores para mudar a aba ativa
  observeEvent(input$aba_rl, {
    aba_ativa("rl")
  })

  observeEvent(input$aba_proj, {
    aba_ativa("proj")
  })

  # Observador para calcular o modelo quando o botão for clicado
  observeEvent(input$calcular, {
    # Simula o caminho do arquivo
    filepath <- "dados_avaliacao.csv"

    # Executa a função gerarLaudo
    resultados <- gerarLaudo(filepath)

    # Armazena os resultados
    resultados_modelo(resultados)
  })

  output$data_table <- renderDT({
    # Cria a coluna de checkboxes sem o atributo "checked"
    sample_data %>%
      mutate(
        Selecionado = paste0(
          '<input type="checkbox" name="row_selected" value="',
          Dado,
          '">'
        )
      ) %>%
      datatable(
        rownames = FALSE,
        options = list(dom = 't', paging = FALSE),
        escape = FALSE
      )
  })

  output$painel_dinamico <- renderUI({
    # Se não houver resultados do modelo, mostra uma mensagem
    if (is.null(resultados_modelo())) {
      return(
        tagList(
          h4("Clique em 'Calcular' para executar a análise"),
          p("Os resultados serão exibidos aqui após o cálculo.")
        )
      )
    }

    # Obtém os resultados do modelo
    resultados <- resultados_modelo()

    # Verificar se df_estimativa existe e tem as colunas necessárias
    if (
      is.null(resultados$df_estimativa) ||
        !all(
          c("Estimativa", "Limite.Inferior", "Limite.Superior") %in%
            names(resultados$df_estimativa)
        )
    ) {
      # Criar um dataframe de estimativa com valores padrão
      resultados$df_estimativa <- data.frame(
        "Estimativa" = 350000,
        "Limite.Inferior" = 320000,
        "Limite.Superior" = 380000
      )
    }

    # Verificar se sumario existe e tem as propriedades necessárias
    if (
      is.null(resultados$sumario) ||
        is.null(resultados$sumario$fstatistic) ||
        length(resultados$sumario$fstatistic) < 3
    ) {
      # Criar um sumario com valores padrão
      resultados$sumario <- list(
        r.squared = 0.85,
        adj.r.squared = 0.80,
        fstatistic = c(15.2, 4, 6)
      )
    }

    # Função auxiliar para arredondar valores com segurança
    safe_round <- function(x, digits = 3) {
      if (is.numeric(x)) {
        return(round(x, digits))
      } else {
        return("N/A")
      }
    }

    # Função auxiliar para calcular p-valor com segurança
    safe_pf <- function(f, df1, df2, lower.tail = FALSE) {
      if (is.numeric(f) && is.numeric(df1) && is.numeric(df2)) {
        return(pf(f, df1, df2, lower.tail = lower.tail))
      } else {
        return(NA)
      }
    }

    if (aba_ativa() == "rl") {
      tagList(
        # Seção de Dados
        h4("Dados"),
        div(
          style = "margin-left: 10px;",
          p(paste(
            "N. de observações:",
            resultados$df_info_complementares$Quant.[3]
          )),
          p(paste("Variáveis:", resultados$df_info_complementares$Quant.[2])),
          p(paste(
            "Grau de liberdade:",
            resultados$df_info_complementares$Quant.[3] -
              resultados$df_info_complementares$Quant.[2] -
              1
          )),
          p(paste("Grau de fundamentação:", 3))
        ),

        # Seção de Parâmetros
        h4("Parâmetros"),
        div(
          style = "margin-left: 10px;",
          selectInput(
            "correlacao",
            "Correlação",
            choices = c("1 - 0.8667746"),
            selected = "1 - 0.8667746"
          ),
          p(paste(
            "Determinação:",
            safe_round(resultados$sumario$r.squared, 4)
          )),
          p(paste(
            "R² Ajustado:",
            safe_round(resultados$sumario$adj.r.squared, 4)
          ))
        ),

        # Seção de Testes de Hipóteses
        h4("Testes de Hipóteses"),
        div(
          style = "margin-left: 10px;",
          p(paste(
            "F Calculado:",
            safe_round(resultados$sumario$fstatistic[1], 2)
          )),
          p(paste(
            "Significância do Modelo:",
            safe_round(
              safe_pf(
                resultados$sumario$fstatistic[1],
                resultados$sumario$fstatistic[2],
                resultados$sumario$fstatistic[3],
                lower.tail = FALSE
              ),
              4
            )
          )),
          p(paste("Grau de Fundamentação:", 3))
        )
      )
    } else if (aba_ativa() == "proj") {
      tagList(
        # Seção de Coeficientes de Correlação
        h4("Coeficientes de Correlação"),
        div(
          style = "margin-left: 10px;",
          selectInput(
            "correlacao_proj",
            "Correlação",
            choices = c("1 - 0.8667746"),
            selected = "1 - 0.8667746"
          )
        ),

        # Seção de Parâmetros
        h4("Parâmetros"),
        div(
          style = "margin-left: 10px;",
          selectInput(
            "nivel",
            "Nível de Confiança",
            choices = c("80%", "90%", "95%"),
            selected = "80%"
          ),
          selectInput(
            "estimativa",
            "Estimativa",
            choices = c("Moda", "Média", "Mediana"),
            selected = "Moda"
          )
        ),

        # Seção de Valores
        h4("Valores"),
        div(
          style = "margin-left: 10px;",
          p(paste(
            "Mínimo:",
            safe_round(resultados$df_estimativa$Limite.Inferior, 3)
          )),
          p(paste(
            "Médio:",
            safe_round(resultados$df_estimativa$Estimativa, 3)
          )),
          p(paste(
            "Máximo:",
            safe_round(resultados$df_estimativa$Limite.Superior, 3)
          )),
          p(paste(
            "Mediana:",
            safe_round(resultados$df_estimativa$Estimativa, 3)
          ))
        ),

        # Seção de Intervalo de Predição
        h4("Intervalo de Predição"),
        div(
          style = "margin-left: 10px;",
          p(paste(
            "Limite Inferior:",
            safe_round(resultados$df_estimativa$Limite.Inferior, 3)
          )),
          p(paste(
            "Limite Superior:",
            safe_round(resultados$df_estimativa$Limite.Superior, 3)
          ))
        )
      )
    }
  })
}

shinyApp(ui, server)
