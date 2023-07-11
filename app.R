library(readr)
Resultados_eleições_resultados_candidato <- read_csv("Resultados eleições - resultados_candidato.csv")
dados = Resultados_eleições_resultados_candidato

library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Resultados das eleições municipais em 1996"),
  h3("Neste Dashboard, você poderá explorar e analisar os resultados das eleições municipais ocorridas no ano de 1996 em diversas cidades. A base de dados escolhida inclui os resultados para os municipios do Acre e Alagoas."),
  h3("Utilizando uma interface interativa, este aplicativo Shiny permite que você selecione municípios, pelo seu código IBGE, específicos e obtenha informações detalhadas sobre os resultados das eleições de 1996. Além disso, você poderá visualizar a frequencia de canditos eleitos por partido."),
  titlePanel("Nº de votos por município"),
  sidebarLayout(
    sidebarPanel(
      selectInput("municipio", "Selecione o município:",
                  choices = unique(dados$id_municipio),
                  selected = NULL)
    ),
    mainPanel(
      plotOutput("grafico")
    )
  ),
  titlePanel("Candidatos eleitos por partido"),
  sidebarLayout(
    sidebarPanel(
      selectInput("partido", "Selecione o partido:",
                  choices = unique(dados$sigla_partido),
                  selected = NULL)
    ),
    mainPanel(
      plotOutput("grafico2")
    )
  )
)

# Defina o servidor Shiny
server <- function(input, output) {
  output$grafico <- renderPlot({
    dados_filtrados <- dados %>%
      filter(id_municipio == input$municipio)
    
    ggplot(dados_filtrados, aes(x = sigla_partido, y = votos)) +
      geom_bar(stat = "identity") +
      labs(x = "Sigla do Partido", y = "Votos",
           title = paste("Gráfico de Votos por Partido -", input$municipio))
  })
  output$grafico2 <- renderPlot({
    dados_filtrados <- dados %>%
      filter(sigla_partido == input$partido)
    
    ggplot(dados_filtrados, aes(x = "", fill = resultado)) +
      geom_bar(width = 1, stat = "count") +
      coord_polar("y", start = 0) +
      labs(fill = "Resultado",
           title = paste("Gráfico de Setor para o Partido", input$partido))
  })
}
# Execute o aplicativo Shiny
shinyApp(ui = ui, server = server)