## BCC - UFRPE 2022.1 - Estatistica Exploratoria ##
## Augusto Miranda ##
## projeto 2VA Estatistica Exploratoria ##

# Instala as library caso não esteja intalado #
if(!require('shiny')) install.packages('shiny')
library(shiny)
if(!require('OpenStreetMap')) install.packages('OpenStreetMap')
library(OpenStreetMap)
if(!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)
if(!require('sp')) install.packages('sp')
library(sp)
if(!require('tidyverse')) install.packages('tidyverse')
library(tidyverse)
if(!require('ggspatial')) install.packages('ggspatial')
library(ggspatial)
if(!require('readxl')) install.packages('readxl')
library(readxl)
if(!require('shinythemes')) install.packages('shinythemes')
library(shinythemes)

# Ler a library, caso não precise executar o de cima #
#library(shiny)
#library(ggplot2)
#library(OpenStreetMap)
#library(tidyverse)
#library(readxl)
#library(sp)
#library(shinythemes)

# Define a Interface do usuário
ui <- fluidPage(
  # Theme IRADO!!!
  theme = shinytheme("cyborg"), # Definir tema como Cyborg :-)
  
  # Titulo da Aplicação, formatada com uma barra de traço #
  tags$hr(),
  tags$head(
    tags$style(
      HTML(".h1-style {font-size: 0.5em; font-weight: bold; font-family: 'Comic Sans MS', cursive;}")
    )
  ),
  titlePanel(
    div(class = "h1-style", "Estatistica Exploratoria - BCC - UFRPE 2022.1 - Augusto Miranda")
  ),
  tags$hr(),
  # Barra Lateral
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu0", "Selecione mu0",
                  min = 0, max = 30, value = 15
      ),
      
      sliderInput("alfa", "Selecione alfa",
                  min = 0.01, max = 0.10, value = 0.05
      ),
      
      radioButtons("tipo", "Tipo do teste",
                   c("Bilateral" = "bi",
                     "Unilateral a Esquerda" = "esq",
                     "Unilateral a Direita" = "dir")),
      numericInput("sig2",
                   "Selecione a Variância",
                   value = 0)
    )
    ,
    # Mostrar um gráfico da distribuição gerada
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa", plotOutput('mapaCoordUFRPE')),
                  tabPanel("Testes de hipóteses",
                           tableOutput('table'),
                           plotOutput('hist')
                  ),
                  tabPanel("Intervalo de Confiança", 
                           sliderInput(
                             "intervalo", "Escolha o Nível de Confiança: ",
                             min = 90, max = 99.9, value = 95, post = '%'
                           ),tableOutput('table2')),
                  tabPanel("Regressão", plotOutput('reg'),
                           tableOutput('table3'),
                           tableOutput('table4'),
                           tableOutput('table5'))
      ),
      tags$hr(),
    )
  )
)

# Defina a lógica do servidor necessária para desenhar um histograma
server <- function(input, output) {
  
  ## dataFrameImport -  ESSA LINHA ABAIXO PRECISA SER ALTERADA PELO PROFESSOR
  
 # dataFrameImport = read_xlsx(file.choose())
  dataFrameImport = read_xlsx("C:/Users/kusan/Downloads/Teste/dados_de_caminhada_corrida.xlsx")

  dataFrameImport = dataFrameImport %>% separate(Coordenadas,c("lat","long"),",") %>%
    separate(Velocidade,c("velocidadeConvert","khm")," ") %>%
    mutate(long = as.numeric(long), lat = as.numeric(lat), velocidadeConvert = as.numeric(velocidadeConvert))
  long = dataFrameImport %>%
    select(long)
  lat = dataFrameImport %>%
    select(lat)
  
  #Coordenadas a serem aberta no mapa
  
  bb = matrix(c(-34.947, -34.957,
                -8.020, -8.0137), 2,2, byrow=T)
  rownames(bb) = c('long', 'lat')
  colnames(bb) = c('min', 'max')
  
  crs = CRS("+proj=utm +zone=25 +south +datum=WGS84")
  
  #cria o Data frame
 
  abrirFrame = data.frame(velocidadeConvert = dataFrameImport %>% select(velocidadeConvert),
                  long = dataFrameImport %>% select(long),
                  lat = dataFrameImport %>% select(lat))
  lonRight = bb[1,2]; latu = bb[2,2]
  lonLeft = bb[1,1]; latd = bb[2,1]
  
 
  abrirMapa = openmap(c(latu+0.001, lonLeft-0.001),
                   c(latd-0.001, lonRight+0.001),
                   type = "osm", mergeTiles = TRUE, minNumTiles = 9L)
  
  abrirMapa2 = openproj(abrirMapa)
  
  # Cria os pontos das coordendas
  
  mapaCoordUFRPE = OpenStreetMap::autoplot.OpenStreetMap(abrirMapa2) +
    geom_point(data = abrirFrame,
               aes(x = long, y = lat), 
               colour = "blue", size =  0.5) +
    xlab("Longitude") + ylab("Latitude")
  output$mapaCoordUFRPE <- renderPlot(mapaCoordUFRPE)
  
  dataFrame2 = dataFrameImport %>% separate(Hora, c("Dia", "Hora"), " ") %>% separate(Hora, c("Horas","minutos","segundos"), ":")
  dataFrame3 = dataFrame2 %>% filter(minutos == 40 & segundos >= 53 | minutos > 40) %>% filter(minutos == 45 & segundos <= 12 | minutos < 45)
  sig2 = reactive({
    as.numeric(input$sig2)
  })
  mu0 = reactive({
    as.integer(input$mu0)
  })
  x = dataFrame3 %>% select(velocidadeConvert)
  n = reactive(length(x$velocidadeConvert))
  xBarra = reactive(mean(x$velocidadeConvert))
  sig_xbar = reactive(sqrt(sig2())/sqrt(n()))
  teste = renderText(tipo())
  tipo = reactive(input$tipo)
  alfa = reactive(as.numeric(input$alfa))
  p = reactive({
    if(teste() == "bi"){
      p = 1 - alfa() + alfa()/2
    }else if(teste() == "esq"){
      p = alfa()
    }else{
      p = 1-alfa()
    }
  })
  ztab = reactive(
    as.numeric(qnorm(p()))
  )
  zcalc = reactive(
    as.numeric((xBarra()-mu0())/sig_xbar())
  )
  
  nCars = length(cars$speed)
  xCars = cars$dist
  yCars = cars$speed
  numCars = nCars*sum(xCars*yCars) - sum(xCars)*sum(yCars)
  denCars1 = sqrt(nCars*sum(xCars^2) - sum(xCars)^2)
  denCars2 = sqrt(nCars*sum(yCars^2) - sum(yCars)^2)
  
  r = numCars/(denCars1*denCars2)
  r2 = r^2
  retaCars = lm(yCars~xCars)
  a = round(retaCars$coefficients[1], 2)
  b = round(retaCars$coefficients[2], 2)
  
  output$reg <- renderPlot(plot(cars$dist,cars$speed,pch = 20,
            cex = 1.0, col = "purple",xlab="Distância", ylab="Velocidade Convertida", xlim = c(0,150), ylim = c(0 , 50), main = "Gráfico de Regressão") + abline(a = a, b = b))

  abrirFrame = dataFrameImport %>% separate(Hora, c("Dia", "Hora"), " ") %>% separate(Hora, c("Horas","minutos","segundos"), ":")
  dataFrame = dataFrame2 %>% filter(minutos == 45 & segundos >= 18 | minutos > 45) %>% filter(minutos == 49 & segundos <= 23 | minutos < 49)
  
  sxBar = reactive(sqrt(s2())/sqrt(nReact()))
  intervalo = reactive(as.numeric(input$intervalo))
  xBar = reactive(mean(x$velocidadeConvert))
  xFrame = dataFrame %>% select(velocidadeConvert)
  s2 = reactive(var(x$velocidadeConvert))
  nReact = reactive(length(x$velocidadeConvert))
  alpha = reactive((100-intervalo())/100)
  z = reactive(qnorm(1 - (alpha()/2)))
  intervaloConfiancaNegativo = reactive(xBar() - (z()*sxBar()))
  intervaloConfiancaPositivo = reactive(xBar() + (z()*sxBar()))
  
  output$table2 <- renderTable(
    data.frame(Result = paste0('Intervalo de Confiança: [ ',
                               intervaloConfiancaNegativo(), ' ; ', intervaloConfiancaPositivo(),' ]'))
  )
  output$table3 <- renderTable(
    data.frame("R" = paste0(r))
  )
  output$table4 <- renderTable(
    data.frame("R2" = paste0(r2))
  )
  output$table5 <- renderTable(
    data.frame("Equação da Reta" = paste0('y = ', b, ' + ', a , 'x'))
  )
  output$table <- renderTable(
    if(teste() == "bi" & zcalc() < ztab() & zcalc() > -ztab() |
       teste() == "esq" & zcalc() > ztab() |
       teste() == "dir" & zcalc() < ztab()
    ){
      data.frame(Result = paste0('Aceita H0 ', (alfa()*100)))
    }else{
      data.frame(Result = paste0('Rejeita H0 ', (alfa()*100)))
    }
  )
  output$hist = renderPlot({
    hist(x$velocidadeConvert, main='Gráfico Teste de Hipoteses', freq = FALSE)
    abline(v = mu0(), col= 'blue')
    abline(v = xBarra(), col= 'brown')
  })
}
# Executa a Aplicação
shinyApp(ui = ui, server = server)