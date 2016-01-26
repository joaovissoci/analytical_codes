library(shiny)
library(shinyjs)
library(ggmap)
library(leaflet)

mandatoryFields <- c("nome", "numero_cartao_sus")

server = function(input, output, session) {

    observe({
        mandatoryFilled <-
          vapply(mandatoryFields,
              function(x) {
                  !is.null(input[[x]]) && input[[x]] != ""
              },
              logical(1)
          )
        mandatoryFilled <- all(mandatoryFilled)
        toggleState(id = "submit", condition = mandatoryFilled)
    })

    observeEvent(input$submit, {
        point <- geocode( input$endereco )
        print( point )

        output$map <- renderLeaflet({
            leaflet() %>%
                addProviderTiles("Stamen.TonerLite",
                    options = providerTileOptions(noWrap = TRUE)) %>%
                addMarkers(data = point)
        })

        reset("form")
        hide("form")
        show("submitted")
    })

}

ui = fluidPage(

    useShinyjs(),

    titlePanel("AUTOMAP-SUS"),

    fluidRow(
        column(2, NULL),

        column(8,
            wellPanel(

                id = "form",
                textInput("nome", "Nome Completo", ""),
                radioButtons("sexo", label = "Sexo",
                    choices = list("Masculino" = 1, "Feminino" = 2), selected = 1),
                textInput("numero_cartao_sus", "Número do Cartão do SUS"),
                dateInput("data_nascimento", "Data de Nascimento"),
                textInput("endereco", "Endereço"),
                actionButton("submit", "Submit")

            ),
            hidden(
                 wellPanel(
                    id = "submitted",
                    h3("Obrigado, a sua resposta foi enviada com sucesso!", align = "center"),
                    leafletOutput("map")
                )
            )  
        ),

        column(2, NULL)
    )

)

shinyApp( ui = ui, server = server )