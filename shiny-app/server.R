library(conflicted)
library(dplyr)
library(avalitools)
library(ggcorrplot)
library(shiny)
library(readxl)
library(quarto)
library(rlang)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# server logic
shinyServer(function(input, output, session) {

  #---- Loading Data
  research <- reactive({
    req(input$file)  # ensures the research file was sent
    path <- input$file$datapath
    read_avaliation(path = path)
  })


  # Assign the search objects to data.frames
  vars_df <- reactive({ req(research()); research()$vari })
  observedData_df <- reactive({ req(research()); research()$dados })
  researchInfos_df <- reactive({ req(research()); research()$info })
  state_df <- reactive({ req(research()); research()$imo })
  rawObservedData_df <- reactive({ req(research()); research()$inicial })
  varsTransf <- reactive({ research()$transf })
  index <- reactive({ research()$indices$i })


  # tranform variables to the assigned transformations
  transformedData_df <- reactive({ 
    
    req(observedData_df())
    apply_transf(df = observedData_df(), trans = varsTransf()) 
  
  })


  # fit the linear regression model to the transformedData_df 
  model <- reactive({
    req(transformedData_df())
    lm_avalitools(transformedData_df()) 
  })


  numericEnableVars_names <- reactive({


    numericEnableVars_names <- vars_df() |>
      filter(Tipo == "Numérica", Habilitada == "Sim") |>
      pull(Nome)

  })

  # assingn the objects of the model object to diferent variables
  modelReg <- reactive({ req(model()); model()$modelo })
  modelAnova <- reactive({ req(model()); model()$anova })



  observe({
    updateSelectInput(session, "var_x",
      choices = numericEnableVars_names(),
      selected = numericEnableVars_names()[1])
    
    updateSelectInput(session, "var_y",
      choices = numericEnableVars_names(),
      selected = numericEnableVars_names()[2])
  })

  # ---- Carregar Dados
  output$summary <- renderDataTable({
    req(research())
    rawObservedData_df()
  })


  # ---- Escolha Transformaçao


  output$transfTable <- renderDataTable({



    transf  <- c("lnx","x", "1/x")

    transf_matriz <- expand.grid(rep(list(transf), ncol(observedData_df()))) 


    for (i in 1:nrow(transf_matriz)) {
      
      it_transf <- transf_matriz[i,]
      
      df_dados_transf <- apply_transf(observedData_df(), it_transf)
      
      modelo <- lm_avalitools(df_dados_transf)
      
      sumario <- summary(modelo$modelo)
      coef_det_ajust <- sumario[[9]]
      
      coef_det_ajust <- as.numeric(coef_det_ajust)
      sigma <- qme(modelo$modelo)

      if (i == 1) {
        matriz <- matrix(c(coef_det_ajust, sigma, it_transf), nrow = 1, ncol = (length(it_transf) + 2) )
      }
      else {
        matriz <- rbind(matriz, c(coef_det_ajust, sigma, it_transf))
      }
      

    }

    matriz <- as.data.frame(matriz)

    colnames(matriz) <- c("Determinação", "Sigma", paste0("v", 1:(ncol(matriz) - 2) ) )

    matriz[[1]] <- as.numeric(matriz[[1]])
    matriz[[2]] <- as.numeric(matriz[[2]])

    matriz
  })






  # ---- Pressupostos
    # ---- Geral
  output$geralOutput <- renderDataTable({
    req(input$geralSel)
  
    switch(input$geralSel,
      "basicInfo" = researchInfos_df(),
      "complementaryInfo" = {
        dataObs_num <- nrow(rawObservedData_df())
        dataActive_num <- nrow(observedData_df())
  
        numericVars_names <- vars_df() |>
          filter(Tipo == "Numérica") |>
          pull(Nome)
  
        
        numericVars_num <- length(numericVars_names)
        numericEnableVars_num <- length(numericEnableVars_names())
  
        df <- complementary_info(
          num_v = numericVars_num,
          num_vu = numericEnableVars_num,
          num_d = dataObs_num,
          num_du = dataActive_num
        )
  
        colnames(df) <- c("Estatísticas", "Quantidade")
        df
      },
      "basicStats" = {
        summary_avalitools(modelReg())
      }
    )
  }, options = list(pageLength = 10, scrollX = TRUE))
  

    # ---- Normalidade
  output$normOutput <- renderPlot({
    req(input$normSel)
  
    switch(input$normSel,
      "histRes" = {
  
        
        ggplot(data.frame(residuals = modelReg()$residuals), aes(x = residuals)) +
        geom_histogram(bins = 12, fill = "skyblue", color = "black") +
        labs(
          title = "Histograma dos resíduos da regressão",
          x = "Resíduos",
          y = "Frequência"
        ) +
        theme_minimal()





      },
      "qqplot" = {
        

        ggplot(data.frame(residuals = modelReg()$residuals), aes(sample = residuals)) +
        stat_qq() +
        stat_qq_line(color = "red") +
        labs(
          title = "QQ Plot dos Resíduos",
          x = "Quantis Teóricos",
          y = "Quantis Amostrais"
        ) +
        theme_minimal()



      },
      "homoRes" = {
        sigmaEstimate <- qme(modelReg())

        estimateAndResiduals_df <- data.frame(
          est = modelReg()$fitted.values,
          res = modelReg()$residuals / sigmaEstimate
        ) |>
          mutate(out = ifelse(res >= 2 | res <= -2, "Outlier", "Não outlier"))
        
        
        ggplot(estimateAndResiduals_df) +
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


      }
    )
  })

    # ---- Auto Correlacao

  output$corrOutput <- renderPlot({
    req(input$corrSel)

    
    transformedData_it <- transformedData_df()
    
    switch(input$corrSel,
      "corr" = {
  
        colnames(transformedData_it) <- numericEnableVars_names()
        corr_matrix <- cor(transformedData_it)
        opplot(corr_matrix, 
          method = "circle", 
          type = "lower", 
          lab = TRUE, 
          lab_size = 3, 
          colors = c("#6D9EC1", "white", "#E46726"),
          title = "Matriz de Correlação",
          ggtheme = theme_minimal())




      },
      "corrInf" = {
        

        colnames(transformedData_it) <- numericEnableVars_names()
        corrInf_matrix <- ppcor::pcor(transformedData_it)$estimate
        ggcorrplot(corrInf_matrix, 
          method = "circle", 
          type = "lower", 
          lab = TRUE, 
          lab_size = 3, 
          colors = c("#6D9EC1", "white", "#E46726"),
          title = "Matriz de Correlação com Influência",
          ggtheme = theme_minimal())



      }
      
    )
  })


    # ---- Variáveis

  output$varsPlot <- renderPlot({
    req(input$var_x, input$var_y)
    observedData_it <- observedData_df()


    colnames(observedData_it) <- numericEnableVars_names()
    
    ggplot(data = observedData_it, aes(x = !!sym(input$var_x), y = !!sym(input$var_y)))+
      geom_point(color = "blue")+
      theme_minimal()


  })



    # ---- Projeção 


  output$projOutput <- renderDataTable({
    req(input$projSel)
  
    switch(input$projSel,
      "imoInfo" = {
        vars_it <- vars_df()
        independentVars <- vars_it |>
          filter(vars_it[[3]] != "Dependente")
        independentVars <- independentVars$Nome

        stateObs_df <- avali_table( state_df(), rawObservedData_df()[,independentVars] ) 

        stateObs_df
      },
      "proj" = {
        
        stateData_df <- state_df() |>
          filter(!is.na(Conteudo) & is.numeric(Conteudo))

        stateData<- stateData_df$Conteudo

        stateData <- matrix(stateData, nrow = 1, ncol = length(stateData))

        stateData <- as.data.frame(stateData)

        colnames(stateData) <- paste("x", 1:ncol(stateData), sep = "")


        estimate_df <- predict_avalitools(
          df_novo = stateData,
          modelo = modelReg(),
          trans = varsTransf()
        )

        estimate_df

      }
      
    )
  }, options = list(pageLength = 10, scrollX = TRUE))


  # ---- Gerar o Relatorio


  output$download <- downloadHandler(
    filename = function() {
      paste0("relatorio_", Sys.Date(), ".docx")  # download file name
    },
    content = function(file) {
      # create a temporary dir to allocate the files between the render
      temp_dir <- tempdir()
  
      # temp paths
      report_path <- file.path(temp_dir, "relatorio.qmd")
      reference_doc_path <- file.path(temp_dir, "custom-reference-doc.docx")
      output_path <- "relatorio.docx"  # Nome simples para o arquivo de saída, sem caminho
  
      # copy needed files to the temp paths
      file.copy("relatorio.qmd", report_path, overwrite = TRUE)
      file.copy("custom-reference-doc.docx", reference_doc_path, overwrite = TRUE)
  
      # render "relatorio" in the temp dir
      quarto::quarto_render(
        input = report_path,
        output_file = output_path,  # Apenas o nome do arquivo
        output_format = "docx",
        execute_params = list(path = input$file$datapath)
      )
  
      #copy "relatorio" generated to the file that can be download by the user
      file.copy(output_path, file, overwrite = TRUE)
    }
  )

})