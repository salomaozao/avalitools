gerarLaudo = function(filepath) {
  pesquisa <- read_avaliation(path = filepath)

  # Cria dataframes que representam a pesquisa
  df_vari <- pesquisa$vari
  df_dados <- pesquisa$dados
  df_info <- pesquisa$info
  df_imo <- pesquisa$imo
  df_dados_ini <- pesquisa$inicial

  # Pega um vetor de caracteres que representam as transformações
  transformacoes <- pesquisa$transf
  indices <- pesquisa$indices
  indices <- indices$i

  # Aplica as transformações nos dados utilizados no calculo
  df_dados_trans <- apply_transf(df = df_dados, trans = transformacoes)

  # Calcula modelo de regressão e tabela anova
  modelo <- lm_avalitools(df_dados_trans)

  # Atribui a dois objetos distintos os modelos
  modelo_reg <- modelo$modelo
  modelo_aov <- modelo$anova

  # cria um resumo para o modelo de regressão linear
  sumario <- summary(modelo_reg) # !

  df_info_basicas <- cbind(colnames(df_info), unlist(df_info))

  cat_df(df_info_basicas) # !
  #Numero de dados
  num_dados <- nrow(df_dados_ini)
  #Numero de dados utilizados
  num_ativo <- nrow(df_dados)
  #Numero de variaveis utilizadas

  vari_num <- df_vari %>%
    filter(Tipo == "Numérica") %>%
    select(Nome)

  vari_uti <- df_vari %>%
    filter(Tipo == "Numérica" & Habilitada == "Sim") %>%
    select(Nome)

  vari_uti <- vari_uti$Nome

  #Numero de variáveis utilizadas
  num_vari_uti <- length(vari_uti)
  #Numero de variaveis numericas
  num_vari_num <- nrow(vari_num)

  # Rotulos para as informações complementares

  df_info_complementares <- complementary_info(
    # !
    num_v = num_vari_num,
    num_vu = num_vari_uti,
    num_d = num_dados,
    num_du = num_ativo
  )

  cat_df(
    df_info_complementares,
    colnames = c("Variáveis e dados do modelo", "Quant.")
  )

  ## Descrição das Variáveis
  cat_df(df_vari, colnames(df_vari))

  # Usa o pacote summarytools para criar uma descrição com minimo maximo e media dos dados
  df_estat_descr <- descr_avalitools(df_dados)

  cat_df(
    df_estat_descr,
    colnames = colnames(df_estat_descr),
    rownames = vari_uti
  )

  df_sumario_avaliacao <- summary_avalitools(modelo_reg)
  cat_df(df_sumario_avaliacao, colnames = colnames(df_sumario_avaliacao))

  df_normalidade <- normality_table(modelo_reg)
  cat_df(
    df_normalidade,
    c("Distribuição dos resíduos", "Curva Normal", "Modelo")
  )

  ## Outliers do Modelo de Regressão:
  df_outliers <- outliers_table(modelo_reg)
  cat_df(df_outliers)

  ## Análise de Variância:
  tabela_anova <- as_avaliation_anova(modelo_aov)
  cat_df(tabela_anova, colnames = colnames(tabela_anova))

  ## Equação de Regressão

  df_equacao_regrecao <- data.frame(
    x = regression_equation(modelo_reg, vari_uti, transformacoes)
  )
  cat_df(df_equacao_regrecao)

  ## Funções Estimativa
  vetor <- estimate_equations(modelo_reg, vari_uti, transformacoes)
  if (length(vetor) != 3) {
    vetor <- rep(vetor, 3)
  }

  ### Função Estimativa (Moda)
  cat_df(data.frame(vetor[1]))
  cat_df(data.frame(vetor[2]))
  cat_df(data.frame(vetor[3]))

  ## Teste de Hipóteses (significância dos regressores)
  df_teste_sign <- ttests_table(modelo_reg, vari_uti)
  cat_df(df_teste_sign, colnames = colnames(df_teste_sign))

  ## Correlações Parciais Isoladas
  matriz_correlacao <- cor_avalitools(df_dados_trans, vari_uti)
  cat_df(matriz_correlacao, colnames = colnames(matriz_correlacao))

  ## Correlações Parciais Influência
  matriz_correlacao_influencia <- cor_avalitools(
    df_dados_trans,
    vari_uti,
    inf = TRUE
  )
  cat_df(
    matriz_correlacao_influencia,
    colnames = colnames(matriz_correlacao_influencia)
  )

  ## Tabela de Residuos
  tabela_residuos <- residuals_table(modelo_reg, df_dados_trans, indices)
  cat_df(tabela_residuos, colnames = colnames(tabela_residuos))

  ## Gráficos de Aderência e de resíduos da regressão:
  ### Aderência Observado x Estimado - Regressão Linear na forma direta
  inv_fun_response <- inverse_function(
    fun = transformacoes[length(transformacoes)]
  )

  tabela_observado_estimado <- data.frame(
    v1 = df_dados[[num_vari_uti]],
    v2 = inv_fun_response(modelo_reg$fitted.values)
  )

  segundo_modelo_reg <- lm(v2 ~ v1, data = tabela_observado_estimado)

  coeficientes <- segundo_modelo_reg$coefficients

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

  ### Residuos da regressão linear
  qme_ <- qme(modelo_reg)

  tabela_estimado_residuo <- data.frame(
    est = modelo_reg$fitted.values,
    res = modelo_reg$residuals / qme_
  ) %>%
    mutate(out = ifelse(res >= 2 | res <= -2, "Outlier", "Não outlier"))

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

  ## Graficos de elasticidade da função no ponto médio
  lista_elasticidade <- elasticity_plots(
    modelo = modelo_reg,
    df = df_dados,
    trans = transformacoes,
    vari_uti
  )

  for (i in 1:(ncol(df_dados) - 1)) {
    print(lista_elasticidade[[i]])
  }

  ## Tabela de Dados - Amostra:

  ### Variáveis Texto

  nomes_vari_texto <- df_vari %>%
    filter(Tipo == "Texto")
  nomes_vari_texto <- nomes_vari_texto$Nome

  df_texto <- df_dados_ini %>% select(all_of(nomes_vari_texto))

  cat_df(df_texto, colnames(df_texto))

  ### Variáveis Numéricas

  nomes_vari_numerica <- df_vari %>%
    filter(Tipo == "Numérica")
  nomes_vari_numerica <- nomes_vari_numerica$Nome

  df_numerica <- df_dados_ini %>% select(all_of(nomes_vari_numerica))

  cat_df(df_numerica, colnames(df_numerica))

  ## Estimativa de Valores
  vetor_novo <- df_imo %>%
    filter(!is.na(Conteudo) | !is.numeric(Conteudo))

  vetor_novo <- vetor_novo$Conteudo

  novos <- matrix(vetor_novo, nrow = 1, ncol = length(vetor_novo))

  novos <- as.data.frame(novos)

  colnames(novos) <- paste("x", 1:ncol(novos), sep = "")

  df_estimativa <- predict_avalitools(
    df_novo = novos,
    modelo = modelo_reg,
    trans = transformacoes
  )

  cat_df(df_estimativa, colnames = colnames(df_estimativa))

  ## Dados do imóvel avaliando:
  nomes_vari_uti <- df_vari %>%
    filter(df_vari[[3]] != "Dependente")
  nomes_vari_uti <- nomes_vari_uti$Nome

  df_ini <- df_dados_ini %>% select(all_of(nomes_vari_uti))

  imo_table <- avali_table(df_imo, df_ini)

  cat_df(imo_table, colnames = c("Variável", "Conteúdo", "Extrapolação"))
  print("FEITO!")

  return(list(
    tabela_observado_estimado = tabela_observado_estimado,
    tabela_estimado_residuo = tabela_estimado_residuo,
    coeficientes = coeficientes,
    df_info_complementares = df_info_complementares,
    df_dados = df_dados,
    modelo_aov = modelo_aov,
    transformacoes = transformacoes
  ))
}
