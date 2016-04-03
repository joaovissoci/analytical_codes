
ui <- shinydashboard::dashboardPage(
  title = "AUTOMAP-SUS",
  shinydashboard::dashboardHeader(
    title = "AUTOMAP-SUS"
  ),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Início", tabName = "home", icon = icon("home")),
      shinydashboard::menuItem("Formulário", tabName = "input", icon = icon("th")),
      shinydashboard::menuItem("Administração", tabName = "admin", icon = icon("cog"))
    )
  ),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),

    shinydashboard::tabItems(

      ## Home page
      shinydashboard::tabItem(
        tabName = "home",
        shiny::fluidRow(
          shiny::column(2, NULL),
          shiny::column(8,
            shinydashboard::box(
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              title = "AUTOMAP-SUS",
              shiny::p("AUTOMAP-SUS é um projeto realizado em parceria pela Faculdade Ingá e Hospital Joao de Freitas, no Paraná. Com colaboração da Duke University, Instituto de Cardiologia do Rio Grande do Sul e Universidade Estadual de Maringá, este projeto visa utilizar o registro e coleta de dados continuo preconizado pela Estrategia de Saúde da Família (ESF) e associar a uma inovação que mistura analise geoespacial e visualizacão interativa e dinâmica de dados. Portanto, inovando no conceito de Mapas Inteligente, nosso objetivo é criar um sistema de coleta e visualizacão de dados automatizado e dinâmica para utilização em gestão e planejamento de atenção em saúde pública.")
            )
          ),
          shiny::column(2, NULL)
        ),
        shiny::fluidRow(
          shiny::column(2, NULL),
          shiny::column(8,
            shinydashboard::box(
              width = 12,
              shiny::fluidRow(
                shiny::img(src = "images/dukeInstitute.png", width = "49%"),
                shiny::img(src = "images/dukeMedicine.png", width = "49%")
              ),
              shiny::fluidRow(
                shiny::img(src = "images/inscardiologia.png", width = "24%"),
                shiny::img(src = "images/araucaria.png", width = "24%"),
                shiny::img(src = "images/inga.png", width = "24%"),
                shiny::img(src = "images/uem.png", width = "24%")
              )
            )
          ),
          shiny::column(2, NULL)
        )
      ),


      ## Form page
      shinydashboard::tabItem(
        tabName = "input",
        shiny::fluidRow(
          shiny::column(2, NULL),

          shiny::column(8,

            shiny::div(
              id = "form",

              shinydashboard::box(
                id = "personal",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                title = "Informações Pessoais",
                textInput("nome", "Nome Completo:"),
                textInput("cod_cnes_unidade", "Código CNES da Unidade:"),
                textInput("microarea", "Microárea:"),
                textInput("numero_cartao_sus", "Número do Cartão do SUS:"),
                dateInput("data_nascimento", "Qual é a sua data de nascimento?"),
                numericInput("idade", label = "Qual é a sua idade?", 0, min = 0, max = 150),
                radioButtons("sexo", label = "Sexo:",
                  choices = list("Masculino" = "Masculino", "Feminino" = "Feminino"), inline = TRUE),
                shinyjs::hidden(radioButtons("gestante", label = "Se mulher - está grávida?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE)),
                selectInput("raca_cor", label = "Qual é sua cor/raça?",
                  choices = list("Não desejo declarar" = NA, "Branca" = "Branca", "Negra" = "Negra", "Parda" = "Parda", 
                    "Indígena" = "Indigena", "Amarela" = "Amarela")),
                textInput("nome_completo_mae", "Qual o nome completo da sua mãe?"),
                textInput("nacionalidade", "Qual a sua nacionalidade?"),
                selectInput("estado_civil", label = "Qual é seu estado civil?",
                  choices = list("Não desejo declarar" = NA, "Casado" = "Casado", "Solteiro" = "Solteiro", "Separado" = "Separado", 
                    "Viúvo" = "Viuvo", "União estável" = "UniaoEstavel")),
                selectInput("grau_escolaridade", label = "Estudou até que série?",
                  choices = list("Não estudou/Não desejo declarar" = NA, "Sabe apenas ler ou escrever o nome" = "Alfabetizado", 
                    "Ensino fundamental incompleto" = "FundamentalIncompleto", "Ensino fundamental completo" = "FundamentalCompleto", 
                    "Ensino médio incompleto" = "MedioIncompleto", "Ensino médio completo" = "MedioCompleto", 
                    "Ensino superior incompleto" = "SuperiorIncompleto", "Ensino superior completo" = "SuperiorCompleto", 
                    "Pós-Graduação" = "PosGraduacao")),
                textInput("ocupacao", "Qual é a sua ocupação?"),
                radioButtons("crianca", label = "Há crianças de 0 a 9 anos em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                shinyjs::hidden(textInput("crianca_cuidador", "Se há criança de 0 a 9 anos, com quem ficam?")),
                radioButtons("cuidador", label = "Há cuidador que frequenta o domicílio?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("plano_de_saude", label = "Possui plano de saúde?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("informar_orientacao", label = "Deseja informar orientação sexual?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                shinyjs::hidden(selectInput("orientacao_sexual", label = "Qual a sua orientação sexual?",
                  choices = list("Heterossexual" = "Heterossexual", "Homossexual" = "Homossexual", "Bissexual" = "Bissexual"))),
                radioButtons("deficiencia", label = "Possui algum tipo de deficiência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                selectInput("peso", label = "Sobre seu peso, como você se considera?",
                  choices = list("Não desejo declarar" = NA, "Acima do peso" = "AcimaPeso", "Peso adequado" = "PesoAdequado", 
                    "Abaixo do peso" = "AbaixoPeso")),
                numericInput("peso_kg", label = "Qual é o seu peso atual (kg)?", 0, min = 0),
                numericInput("altura_cm", label = "Qual é sua altura (cm)?", 0, min = 0),
                selectInput("imc", label = "Qual o seu IMC?",
                  choices = list("Não desejo declarar" = NA, "Maior que 30" = "Maior30", "Entre 25 e 29" = "Entre25e29", 
                    "Entre 19 e 24" = "Entre19e24", "Menor que 18" = "Menor18")),
                numericInput("ca_cm", label = "Qual é medida da Circunferência Abdominal?", 0, min = 0),
                numericInput("pa_mmhg", label = "Qual é a medida da Pressão Arterial?", 0, min = 0),
                numericInput("glicemia", label = "Qual é o seu nível de glicemia?", 0, min = 0),
                radioButtons("fumante", label = "É fumante?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("alcool", label = "Faz uso de bebidas alcoólicas?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("drogas", label = "Faz uso de drogas ilícitas?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("hipertensao", label = "Tem pressão alta?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("remedio_hipertensao", label = "Faz uso de medicamento para controlar a pressão?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("diabetes", label = "Tem diabetes?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("remedio_diabetes", label = "Faz uso de medicamento para controlar o açúcar no sangue?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("colesterol_alto", label = "Tem colesterol alto?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("remedio_colesterol", label = "Faz uso de medicamento para controlar o colesterol?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("avc_derrame", label = "Já teve AVC ou derrame?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("infarto", label = "Já teve infarto?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                selectInput("doenca_cardiaca", label = "Tem alguma doença cardíaca?",
                  choices = list("Não desejo declarar" = NA, "Insuficiência Cardíaca" = "InsuficienciaCardiaca", 
                    "Outro" = "Outro", "Não sabe" = "NaoSabe")),
                selectInput("doenca_renal", label = "Teve ou tem algum problema renal?",
                  choices = list("Não desejo declarar" = NA, "Insuficiência Renal" = "InsuficienciaRenal", 
                    "Outro" = "Outro", "Não sabe" = "NaoSabe")),
                selectInput("doenca_respiratoria", label = "Teve ou tem doença respiratória/ do pulmão?",
                  choices = list("Não desejo declarar" = NA, "Asma" = "Asma", "DPOC/Enfisema" = "Enfisema", 
                    "Outro" = "Outro", "Não sabe" = "NaoSabe")),
                radioButtons("hanseniase", label = "Teve ou tem hanseníase?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("tuberculose", label = "Teve ou tem tuberculose?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("cancer", label = "Teve ou tem câncer?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("internacao", label = "Teve que ficar internado por alguma razão nos últimos 12 meses?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("saude_mental", label = "Fez ou faz tratamento com psiquiatra ou teve internações por problemas de saúde mental?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("acamado", label = "Está acamado?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("plantas_medicinais", label = "Faz uso de plantas medicinais?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE)
              ),

              shinydashboard::box(
                id = "house",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                title = "Informações sobre o Domicílio",
                textInput("endereco_logradouro", "Qual o logradouro de residência?"),
                textInput("endereco_bairro", "Qual o bairro de residência?"),
                textInput("endereco_municipio", "Qual o município de residência?"),
                textInput("endereco_cep", "Qual o CEP de residência?"),
                selectInput("endereco_estado", label = "Qual o estado de residência",
                  choices = list(" " = "", "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM", 
                    "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF", 
                    "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA", "Mato Grosso" = "MT", 
                    "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG", "Pará" = "PA", "Paraíba" = "PB", 
                    "Paraná" = "PR", "Pernambuco" = "PE", "Piauí" = "PI", "Rio de Janeiro" = "RJ", 
                    "Rio Grande do Norte" = "RN", "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR", 
                    "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE", "Tocantins" = "TO")),
                selectInput("moradia", label = "Qual é a situação de moradia/posse da terra?",
                  choices = list("Não desejo declarar" = NA, "Próprio" = "Proprio", "Financiado" = "Financiado", 
                    "Arrendado" = "Arrendado", "Cedido" = "Cedido", "Ocupação" = "Ocupacao", "Situação de rua" = "SituacaoRua", "Outra" = "Outra")),
                radioButtons("localizacao_urbana", label = "Qual o tipo de localização da sua residência?",
                  choices = list("Urbana" = TRUE, "Rural" = FALSE), inline = TRUE),
                selectInput("domicilio", label = "Qual é o tipo de domicílio?",
                  choices = list("Não desejo declarar" = NA, "Casa" = "Casa", "Apartamento" = "Apartamento", "Cômodo" = "Comodo", "Outro" = "Outro")),
                selectInput("acesso_domicilio", label = "Tipo de acesso ao seu domicílio?",
                  choices = list("Não desejo declarar" = NA, "Pavimento" = "Pavimento", "Chão batido" = "ChaoBatido", "Fluvial" = "Fluvial", "Outro" = "Outro")),
                selectInput("construcao_domicilio", label = "Tipo de material usado na construção das paredes de sua residência?",
                  choices = list("Não desejo declarar" = NA, "Alvenaria com revestimento" = "AlvenariaComRevestimento", "Alvenaria sem revestimento" = "AlvenariaSemRevestimento", 
                    "Taipa com revestimento" = "TaipaComRevestimento", "Taipa sem revestimento" = "TaipaSemRevestimento", "Madeira" = "Madeira", 
                    "Material aproveitado" = "MaterialAproveitado", "Palha" = "Palha", "Outro" = "Outro")),
                selectInput("abastecimento_agua", label = "Como é feito o abastecimento de água de sua residência?",
                  choices = list("Não desejo declarar" = NA, "Rede encanada" = "RedeEncanada", "Carro pipa" = "CarroPipa",
                    "Poço ou nascente" = "Poco", "Cisterna" = "Cisterna", "Outro" = "Outro")),
                selectInput("tratamento_agua", label = "Como é feito o tratamento de água em sua residência?",
                  choices = list("Não desejo declarar" = NA, "Filtração" = "Filtracao", "Fervura" = "Fervura", "Cloração" = "Cloração", "Sem tratamento" = "SemTratamento")),
                selectInput("esgoto", label = "Como é feito o escoamento do banheiro em sua residência?",
                  choices = list("Não desejo declarar" = NA, "Rede coletora de esgoto ou pluvial" = "Esgoto", "Direto para o rio, lago ou mar" = "Rio",
                    "Fosse séptica" = "Fossa", "Céu aberto" = "CeuAberto", "Fossa rudimentar" = "FossaRudimentar", "Outra" = "Outra")),
                selectInput("lixo", label = "Como é feito o destino do lixo em sua residência?",
                  choices = list("Não desejo declarar" = NA, "Coletado" = "Coletado", "Queimado" = "Queimado",
                    "Enterrado" = "Enterrado", "Céu aberto" = "CeuAberto", "Outro" = "Outro")),
                selectInput("animais", label = "Você tem animais em sua residência?",
                  choices = list("Não desejo declarar" = NA, "Gato" = "Gato", "Cachorro" = "Cachorro", "Pássaro" = "Passaro",
                    "Porco" = "Porco", "Galinha" = "Galinha", "Outro" = "Outro")),
                selectInput("doenca_procura", label = "Em caso de doença, primeiro voce procura?",
                  choices = list("Não desejo declarar" = NA, "Hospital" = "Hospital", "Unidade de saúde" = "UnidadeSaude",
                    "Benzedeira" = "Benzedeira", "Farmácia" = "Farmacia", "Outro" = "Outro")),
                selectInput("comunicacao", label = "Qual é o meio de comunicação que mais utiliza?",
                  choices = list("Não desejo declarar" = NA, "Televisão" = "Televisao", "Rádio" = "Radio",
                    "Internet" = "Internet", "Outro" = "Outro")),
                selectInput("participacao_grupos", label = "Participa de grupos comunitários?",
                  choices = list("Não desejo declarar" = NA, "Cooperativa" = "Cooperativa", "Grupo religioso" = "GrupoReligioso",
                    "Associações" = "Associacoes", "Outro" = "Outro")),
                selectInput("transporte", label = "Qual é o meio de transporte que mais utiliza?",
                  choices = list("Não desejo declarar" = NA, "Carro" = "Carro", "Moto" = "Moto", "Ônibus" = "Onibus",
                    "Caminhão" = "Caminhao", "Carroça" = "Carroca", "Outro" = "Outro")),
                radioButtons("bolsa_familia", label = "A família é beneficiária do Bolsa Família?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("cad_unico", label = "A família está inscrita no Cadastramento Único de Programas Sociais do Governo Federal (CAD-Único)?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE)
              ),

              shinydashboard::box(
                id = "family",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                title = "Informações sobre a Família",
                radioButtons("familia_acamado", label = "Existem pessoas acamadas em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("deficiencia_fisica", label = "Existem pessoas com deficiência física em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("deficiencia_mental", label = "Existem pessoas com deficiência mental em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("saneamento", label = "Existe rede de tratamento de água e esgoto em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("desnutricao_grave", label = "Exitem pessoas desnutridas em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("drogadicao", label = "Existem pessoas que fazem uso de drogas em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("desemprego", label = "Existem pessoas desempregadas em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("analfabetismo", label = "Existem pessoas que não sabem ler e escrever em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("menor_6_anos", label = "Existem pessoas menores de 6 anos em sua residência",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("maior_70_anos", label = "Existem pessoas maiores de 70 anos em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("has", label = "Existem pessoas hipertensas em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                radioButtons("dm", label = "Existem pessoas diabéticas em sua residência?",
                  choices = list("Não" = FALSE, "Sim" = TRUE), inline = TRUE),
                numericInput("numero_moradores", label = "Quantos moradores há em sua residência?", 0, min = 0),
                numericInput("numero_comodos", label = "Quantos cômodos há em sua residência?", 0, min = 0)
              )
            ),

            shinyjs::hidden(
              shiny::div(
                id = "formAddressError",
                shinydashboard::box(
                  id = "addressError",
                  width = 12,
                  solidHeader = TRUE,
                  status = "danger",
                  title = "Erro: Endereço não encontrado",
                  h3("Não foi possível localizar o endereço fornecido", align = "center"),
                  h4("Por favor, insira a latitude e longitude do endereço:", align = "center"),
                  numericInput("endereco_error_latitude", "Qual a latitude do endereço de residência?", value = 0, min = -90, max = 90),
                  numericInput("endereco_error_longitude", "Qual a longitude do endereço de residência?", value = 0, min = -180, max = 180)
                )
              )
            ),

            shinyjs::hidden(
              shiny::div(
                id = "formAddress",
                shinydashboard::box(
                  id = "address",
                  width = 12,
                  solidHeader = TRUE,
                  status = "warning",
                  h3("Obrigado, o endereço foi encontrado", align = "center"),
                  h4("Por favor, confira se ele está correto", align = "center"),
                  leaflet::leafletOutput("map"),
                  h4("Se o endereço encontrado estiver incorreto, por favor insira o novamente"),
                  textInput("endereco_parser_logradouro", "Qual o logradouro de residência?"),
                  textInput("endereco_parser_bairro", "Qual o bairro de residência?"),
                  textInput("endereco_parser_municipio", "Qual o município de residência?"),
                  textInput("endereco_parser_cep", "Qual o CEP de residência?"),
                  selectInput("endereco_parser_estado", label = "Qual o estado de residência",
                    choices = list(" " = "", "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM", 
                      "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF", 
                      "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA", "Mato Grosso" = "MT", 
                      "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG", "Pará" = "PA", "Paraíba" = "PB", 
                      "Paraná" = "PR", "Pernambuco" = "PE", "Piauí" = "PI", "Rio de Janeiro" = "RJ", 
                      "Rio Grande do Norte" = "RN", "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR", 
                      "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE", "Tocantins" = "TO")
                  )
                )
              )
            ),

            shinyjs::hidden(
              shiny::div(
                id = "formSubmitted",
                shinydashboard::box(
                  id = "submitted",
                  width = 12,
                  solidHeader = TRUE,
                  status = "success",
                  h3("Obrigado, a sua resposta foi enviada com sucesso!", align = "center")
                )
              )
            ),

            shiny::div(
              id = "formSubmit",
              shinydashboard::box(
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                actionButton("submit", "Submit")
              )
            )

          ),
          column(2, NULL)
        )
      ),

      
      ## Admin page
      shinydashboard::tabItem(
        tabName = "admin",
        shiny::fluidRow(
          shiny::column(2, NULL),
          shiny::column(8,
            shiny::div(
              id = "admin_login",
              shinydashboard::box(
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                title = "Administração AUTOMAP-SUS",
                shiny::h3("Por favor, faça o login abaixo:"),
                textInput("admin_username", "Usuário:"),
                passwordInput("admin_password", "Senha:")
              )
            ),
            shiny::div(
              id = "admin_submit",
              shinydashboard::box(
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                actionButton("admin_login", "Login")
              )
            ),
            shinyjs::hidden(
              shiny::div(
                id = "admin_logged",
                shinydashboard::box(
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "primary",
                  title = "Download dos Dados",
                  shiny::p("Para fazer o download das informações em formato csv, clique no botão abaixo:"),
                  shiny::div(
                    shiny::downloadButton("downloadData", "Download"), 
                    align = "center"
                  )
                ),
                shinydashboard::box(
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "primary",
                  title = "Distribuição Geográfica",
                  leaflet::leafletOutput("admin_map")
                ),
                shinydashboard::box(
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "primary",
                  title = "Análise Univariada",
                  shiny::uiOutput("uniAnalysis")
                )
              )
            )
          ),
          shiny::column(2, NULL)
        )
      )
    ),

    tags$head(tags$script(src="jquery.mask.js")),
    tags$script(HTML("$('#numero_cartao_sus').mask('00000000000-0000-0');")),
    tags$script(HTML("$('#endereco_cep').mask('00000-000');")),
    tags$script(HTML("$('#endereco_error_cep').mask('00000-000');")),
    tags$script(HTML("$('#endereco_parser_cep').mask('00000-000');"))

  )

)