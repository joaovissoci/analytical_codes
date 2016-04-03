library(dplyr)

mandatoryFields <- c(#"nome", "cod_cnes_unidade", "numero_cartao_sus",
  "endereco_logradouro", "endereco_bairro", "endereco_municipio", "endereco_cep", "endereco_estado")

admMandatoryFields <- c("admin_username", "admin_password")

icons <- leaflet::iconList(
  green = leaflet::makeIcon("icons/marker-icon-green.png", "icons/marker-icon-2x-green.png", 25, 41),
  red = leaflet::makeIcon("icons/marker-icon-red.png", "icons/marker-icon-2x-red.png", 25, 41)
)

server <- function(input, output, session) {

  observeEvent(input$sexo, {
    if (input$sexo == "Feminino") {
      shinyjs::show("gestante", anim = TRUE)
    } else {
      shinyjs::hide("gestante", anim = TRUE)
      shinyjs::reset("gestante")
    }
  })

  observeEvent(input$crianca_0_9, {
    if (input$crianca_0_9) {
      shinyjs::show("crianca_0_9_cuidador", anim = TRUE)
    } else {
      shinyjs::hide("crianca_0_9_cuidador", anim = TRUE)
      shinyjs::reset("crianca_0_9_cuidador")
    }
  })

  observeEvent(input$informar_orientacao, {
    if (input$informar_orientacao) {
      shinyjs::show("orientacao_sexual", anim = TRUE)
    } else {
      shinyjs::hide("orientacao_sexual", anim = TRUE)
      shinyjs::reset("orientacao_sexual")
    }
  })

  #con <- DBI::dbConnect(RMySQL::MySQL(), dbname = "automapsus", 
  #  username = "root", password = "automap", 
  #  host = "localhost", port = 3306)

  #session$onSessionEnded(function() {
  #  DBI::dbDisconnect(con)
  #})

  values <- shiny::reactiveValues(data = NULL)
  values$confirmed <- FALSE
  values$address <- NULL
  values$point <- NULL
  values$error <- FALSE

  shiny::observe({
    mandatoryFilled <-
      vapply(mandatoryFields,
        function(x) {
          !is.null(input[[x]]) && input[[x]] != ""
        },
        logical(1)
      )
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  shiny::observe({
    admMandatoryFilled <-
      vapply(admMandatoryFields,
        function(x) {
          !is.null(input[[x]]) && input[[x]] != ""
        },
      logical(1)
    )
    admMandatoryFilled <- all(admMandatoryFilled)
    shinyjs::toggleState(id = "admin_submit", condition = admMandatoryFilled)
  })

  shiny::observeEvent(input$submit, {

    addressInputs <- c("logradouro", "bairro", "municipio", "estado", "cep")
    vars <- c()
    for (addressInput in addressInputs) {
      var <- paste("endereco_parser", addressInput, sep = "_")
      vars <- c(vars, input[[var]])
    }

    if (values$confirmed && all(vars == "")) {

      shinyjs::reset(id = "personal")
      shinyjs::reset(id = "house")
      shinyjs::reset(id = "family")
      shinyjs::reset(id = "address")
      shinyjs::reset(id = "addressError")

      shinyjs::hide(id = "form", anim = TRUE)
      shinyjs::hide(id = "formAddress", anim = TRUE)
      shinyjs::hide(id = "formAddressError", anim = TRUE)
      shinyjs::hide(id = "formSubmit", anim = TRUE)

      variables <- c("nome", "cod_cnes_unidade", "microarea", "numero_cartao_sus", "data_nascimento", "idade", 
        "sexo", "gestante", "raca_cor", "nome_completo_mae", "nacionalidade", "estado_civil", 
        "grau_escolaridade", "ocupacao", "crianca", "crianca_cuidador", "cuidador", 
        "plano_de_saude", "informar_orientacao", "orientacao_sexual", "deficiencia", "peso", 
        "peso_kg", "altura_cm", "imc", "ca_cm", "pa_mmhg", "glicemia", "fumante", "alcool", 
        "drogas", "hipertensao", "remedio_hipertensao", "diabetes", "remedio_diabetes", 
        "colesterol_alto", "remedio_colesterol", "avc_derrame", "infarto", "doenca_cardiaca", 
        "doenca_renal", "doenca_respiratoria", "hanseniase", "tuberculose", "cancer", 
        "internacao", "saude_mental", "acamado", "plantas_medicinais", "moradia", 
        "localizacao_urbana", "domicilio", "acesso_domicilio", "construcao_domicilio", 
        "abastecimento_agua", "tratamento_agua", "esgoto", "lixo", "animais", "doenca_procura", 
        "comunicacao", "participacao_grupos", "transporte", "bolsa_familia", "cad_unico", "familia_acamado", 
        "deficiencia_fisica", "deficiencia_mental", "saneamento", "desnutricao_grave", "drogadicao", 
        "desemprego", "analfabetismo", "menor_6_anos", "maior_70_anos", "has", "dm", "numero_moradores", "numero_comodos"
      )

      data <- lapply(variables, function(var) input[[var]])
      names(data) <- variables
      data <- as.data.frame(data)

      if (values$error) {
        data$lat <- input$endereco_error_latitude
        data$lon <- input$endereco_error_longitude
      } else {
        data$endereco <- values$address
        data$lat <- values$point$lat
        data$lon <- values$point$lon
      }
      
      db <- RMySQL::dbConnect(RMySQL::MySQL(), user = "root", host = "localhost", password = "automap", dbname = "automapsus")
      RMySQL::dbWriteTable(db, "observations", data, append = TRUE, row.names = FALSE)
      RMySQL::dbDisconnect(db) 

      shinyjs::show(id = "formSubmitted", anim = TRUE)

    } else {

      addressInputs <- c("logradouro", "bairro", "municipio", "estado", "cep")
      addressForms <- c("endereco_error", "endereco_parser", "endereco")
      for (addressForm in addressForms) {
        vars <- c()
        for (addressInput in addressInputs) {
          var <- paste(addressForm, addressInput, sep = "_")
          vars <- c(vars, input[[var]])
        }
        if ( any(vars != "") ) {
          address <- paste(vars, collapse = " ")
          break
        }
      }

      point = tryCatch( ggmap::geocode( address ),
        warning = function(w) w,
        error = function(e) e 
      )

      if ( is( point,"warning" ) || is( point,"error" ) ) {

        values$address <- NULL
        values$point <- NULL
        values$confirmed <- TRUE
        values$error <- TRUE
        output$map <- leaflet::renderLeaflet({
          leaflet::leaflet() %>%
            leaflet::addProviderTiles("Stamen.TonerLite",
              options = leaflet::providerTileOptions(noWrap = TRUE))
        })

        shinyjs::reset(id = "address")
        shinyjs::reset(id = "addressError")

        shinyjs::hide(id = "form", anim = TRUE)
        shinyjs::hide(id = "formAddress", anim = TRUE)

        shinyjs::show(id = "formAddressError", anim = TRUE)

      } else {

        values$address <- address
        values$point <- point
        values$confirmed <- TRUE

        output$map <- leaflet::renderLeaflet({
          leaflet::leaflet() %>%
            leaflet::addProviderTiles("Stamen.TonerLite",
              options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
            leaflet::addMarkers(data = point)
        })

        shinyjs::reset(id = "address")
        shinyjs::reset(id = "addressError")

        shinyjs::hide(id = "form", anim = TRUE)
        shinyjs::hide(id = "formAddressError", anim = TRUE)

        shinyjs::show(id = "formAddress", anim = TRUE)
        
      }

    }

  })

  shiny::observeEvent(input$admin_login, {

    db <- dplyr::src_mysql("automapsus", host = "localhost", port = 3306, user = "root", password = "automap")
    query <- paste("SELECT id FROM admin WHERE username = '", input$admin_username,"' AND password = '", input$admin_password, "' ; ", sep="")
    response <- RMySQL::dbSendQuery(db$con, query)
    login <- RMySQL::fetch(response)

    if ( nrow(login) > 0 ) {

      data <- dplyr::collect(dplyr::select(dplyr::tbl(db, "observations"), everything()))

      data$raca_cor <- factor(data$raca_cor, 
        levels <- intersect(c("Branca", "Negra", "Parda"), unique(data$raca_cor[which(data$raca_cor != "NA")])), exclude = c("NA"))
      data$estado_civil <- factor(data$estado_civil, 
        levels <- intersect(c("Casado", "Solteiro", "Separado", "Viuvo", "UniaoEstavel"), unique(data$estado_civil[which(data$estado_civil != "NA")])), exclude = c("NA"))
      data$grau_escolaridade <- factor(data$grau_escolaridade, 
        levels <- intersect(c("Alfabetizado", "FundamentalIncompleto", "FundamentalCompleto", 
        "MedioIncompleto", "MedioCompleto", "SuperiorIncompleto", "SuperiorCompleto", "PosGraduacao"), unique(data$grau_escolaridade[which(data$grau_escolaridade != "NA")])), exclude = c("NA"))
      data$peso <- factor(data$peso, 
        levels <- intersect(c("AcimaPeso", "PesoAdequado", "AbaixoPeso"), unique(data$peso[which(data$peso != "NA")])), exclude = c("NA"))
      data$imc <- factor(data$imc, 
        levels <- intersect(c("Maior30", "Entre25e29", "Entre19e24", "Menor18"), unique(data$imc[which(data$imc != "NA")])), exclude = c("NA"))
      data$doenca_cardiaca <- factor(data$doenca_cardiaca, 
        levels <- intersect(c("InsuficienciaCardiaca", "Outro", "NaoSabe"), unique(data$doenca_cardiaca[which(data$doenca_cardiaca != "NA")])), exclude = c("NA"))
      data$doenca_renal <- factor(data$doenca_renal, 
        levels <- intersect(c("InsuficienciaRenal", "Outro", "NaoSabe"), unique(data$doenca_renal[which(data$doenca_renal != "NA")])), exclude = c("NA"))
      data$doenca_respiratoria <- factor(data$doenca_respiratoria, 
        levels <- intersect(c("Asma", "Enfisema", "Outro", "NaoSabe"), unique(data$doenca_respiratoria[which(data$doenca_respiratoria != "NA")])), exclude = c("NA"))
      data$moradia <- factor(data$moradia, 
        levels <- intersect(c("Proprio", "Financiado", "Arrendado", "Cedido", "Ocupacao", "SituacaoRua", "Outra"), unique(data$moradia[which(data$moradia != "NA")])), exclude = c("NA"))
      data$domicilio <- factor(data$domicilio, 
        levels <- intersect(c("Casa", "Apartamento", "Comodo", "Outro"), unique(data$domicilio[which(data$domicilio != "NA")])), exclude = c("NA"))
      data$acesso_domicilio <- factor(data$acesso_domicilio, 
        levels <- intersect(c("Pavimento", "ChaoBatido", "Fluvial", "Outro"), unique(data$acesso_domicilio[which(data$acesso_domicilio != "NA")])), exclude = c("NA"))
      data$construcao_domicilio <- factor(data$construcao_domicilio, 
        levels <- intersect(c("AlvenariaComRevestimento", "AlvenariaSemRevestimento", 
        "TaipaComRevestimento", "TaipaSemRevestimento", "Madeira", "MaterialAproveitado", "Palha", "Outro"), unique(data$construcao_domicilio[which(data$construcao_domicilio != "NA")])), exclude = c("NA"))
      data$abastecimento_agua <- factor(data$abastecimento_agua, 
        levels <- intersect(c("RedeEncanada", "CarroPipa", "Poco","Cisterna", "Outro"), unique(data$abastecimento_agua[which(data$abastecimento_agua != "NA")])), exclude = c("NA"))
      data$tratamento_agua <- factor(data$tratamento_agua, 
        levels <- intersect(c("Filtracao", "Fervura", "Cloração", "SemTratamento"), unique(data$tratamento_agua[which(data$tratamento_agua != "NA")])), exclude = c("NA"))
      data$esgoto <- factor(data$esgoto, 
        levels <- intersect(c("Esgoto", "Rio", "Fossa", "CeuAberto", "FossaRudimentar", "Outra"), unique(data$esgoto[which(data$esgoto != "NA")])), exclude = c("NA"))
      data$lixo <- factor(data$lixo, 
        levels <- intersect(c("Coletado", "Queimado", "Enterrado", "CeuAberto", "Outro"), unique(data$lixo[which(data$lixo != "NA")])), exclude = c("NA"))
      data$animais <- factor(data$animais, 
        levels <- intersect(c("Gato", "Cachorro", "Passaro", "Porco", "Galinha", "Outro"), unique(data$animais[which(data$animais != "NA")])), exclude = c("NA"))
      data$doenca_procura <- factor(data$doenca_procura, 
        levels <- intersect(c("Hospital", "UnidadeSaude", "Benzedeira", "Farmacia", "Outro"), unique(data$doenca_procura[which(data$doenca_procura != "NA")])), exclude = c("NA"))
      data$comunicacao <- factor(data$comunicacao, 
        levels <- intersect(c("Televisao", "Radio", "Internet", "Outro"), unique(data$comunicacao[which(data$comunicacao != "NA")])), exclude = c("NA"))
      data$participacao_grupos <- factor(data$participacao_grupos, 
        levels <- intersect(c("Cooperativa", "GrupoReligioso", "Associacoes", "Outro"), unique(data$participacao_grupos[which(data$participacao_grupos != "NA")])), exclude = c("NA"))
      data$transporte <- factor(data$transporte, 
        levels <- intersect(c("Carro", "Moto", "Onibus", "Caminhao", "Carroca", "Outro"), unique(data$transporte[which(data$transporte != "NA")])), exclude = c("NA"))

      output$downloadData <- downloadHandler(
        filename = function() {"data.csv"},
        content = function(file) {write.csv(data, file, row.names = FALSE)}
      )
        
      output$admin_map <- leaflet::renderLeaflet({
        points <- data[, c("nome", "pa_mmhg", "glicemia", "lon", "lat")]
        points$lon <- as.numeric(points$lon)
        points$lat <- as.numeric(points$lat)
        points$glicemia[is.na(points$glicemia)] <- 0
        points$popup <- paste("<b>", points$nome, "</b><br>Pressão arterial: ", points$pa_mmhg, "<br>Glicemia: ", points$glicemia, sep = "")
        points$type <- factor(ifelse(points$glicemia < 126, "green", "red"), c("green", "red"))
        leaflet::leaflet() %>%
        leaflet::addProviderTiles("Stamen.TonerLite", options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
        leaflet::addMarkers(data = points[, c("lon", "lat")], popup = points$popup, icon = ~icons[points$type])
      })

      variables <- c("idade", 
        "sexo", "gestante", "raca_cor",  "estado_civil", 
        "grau_escolaridade", "crianca", "cuidador", 
        "plano_de_saude", "informar_orientacao", "orientacao_sexual", "deficiencia", "peso", 
        "peso_kg", "altura_cm", "imc", "ca_cm", "pa_mmhg", "glicemia", "fumante", "alcool", 
        "drogas", "hipertensao", "remedio_hipertensao", "diabetes", "remedio_diabetes", 
        "colesterol_alto", "remedio_colesterol", "avc_derrame", "infarto", "doenca_cardiaca", 
        "doenca_renal", "doenca_respiratoria", "hanseniase", "tuberculose", "cancer", 
        "internacao", "saude_mental", "acamado", "plantas_medicinais", "moradia", 
        "localizacao_urbana", "domicilio", "acesso_domicilio", "construcao_domicilio", 
        "abastecimento_agua", "tratamento_agua", "esgoto", "lixo", "animais", "doenca_procura", 
        "comunicacao", "participacao_grupos", "transporte", "bolsa_familia", "cad_unico", "familia_acamado", 
        "deficiencia_fisica", "deficiencia_mental", "saneamento", "desnutricao_grave", "drogadicao", 
        "desemprego", "analfabetismo", "menor_6_anos", "maior_70_anos", "has", "dm", "numero_moradores", "numero_comodos"
      )
      for (var in variables) {
        if (is.character(data[[var]])) {
          data[[var]] <- as.factor(data[[var]])
        }
      }
      univariate <- reactiveValues()
      univariate$vars <- c()
      output$uniAnalysis <- shiny::renderUI({
        outputList <- lapply(variables, function(var) {
          if (is.factor(data[[var]])) {
            plot <- shiny::renderUI(
              shiny::fluidRow(
                shinydashboard::box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = paste("Variável: ", var),
                  plotly::plotlyOutput(paste(var, "Plot", sep = ""))
                )
              )
            )
          } else if (is.numeric(data[[var]])) {
            plot <- shiny::renderUI(
              shiny::fluidRow(
                shinydashboard::box(
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = paste("Variável: ", var),
                  plotly::plotlyOutput(paste(var, "Histogram", sep = "")),
                  plotly::plotlyOutput(paste(var, "BoxPlot", sep = ""))
                )
              )
            )
          }
          shiny::isolate({ univariate$vars <- c(univariate$vars, var) })
          plot
        })
        do.call(shiny::tagList, outputList)
      })
      shiny::observe({
        for (var in univariate$vars) {
          local({
            local_var <- var
            if (is.factor(data[[local_var]])) {
              summ <- plyr::count(data[local_var])
              output[[paste(local_var, "Plot", sep="")]] <- plotly::renderPlotly({ 
                if (any(is.na(data[[local_var]]))) {
                  gg <- plotly::layout(
                    plotly::plot_ly(x = summ$x, y = summ$freq, type = "bar"), 
                    xaxis = list(
                      title = local_var, 
                      tickmode = "array", 
                      tickvals = 0:(length(levels(data[[local_var]]))+1), 
                      ticktext = c(levels(data[[local_var]]), "NA")
                    ),
                    yaxis = list(title = "")
                  )
                } else { 
                  gg <- plotly::layout(
                    plotly::plot_ly(x = summ$x, y = summ$freq, type = "bar"), 
                    xaxis = list(
                      title = local_var, 
                      tickmode = "array", 
                      tickvals = 0:length(levels(data[[local_var]])), 
                      ticktext = levels(data[[local_var]])
                    ),
                    yaxis = list(title = "")
                  )
                }
                gg
              })
            } else if (is.numeric(data[[local_var]])) {
              ldata <- na.omit(data[local_var])
              output[[paste(local_var, "Histogram", sep="")]] <- plotly::renderPlotly({ 
                gg <- plotly::layout(
                  plotly::plot_ly(x = ldata[[local_var]], type = "histogram"), 
                  xaxis = list(title = local_var, showticklabels = TRUE),
                  yaxis = list(title = "")
                )
                gg
              })
              output[[paste(local_var, "BoxPlot", sep="")]] <- plotly::renderPlotly({ 
                gg <- plotly::layout(
                  plotly::plot_ly(y = ldata[[local_var]], type = "box"), 
                  xaxis = list(title = local_var, showticklabels = FALSE),
                  yaxis = list(title = "")
                )
                gg
              })
            }
          })
        }
      })

      shinyjs::hide(id = "admin_login", anim = TRUE)
      shinyjs::hide(id = "admin_submit", anim = TRUE)
      shinyjs::show(id = "admin_logged", anim = TRUE)

    }

  })

}