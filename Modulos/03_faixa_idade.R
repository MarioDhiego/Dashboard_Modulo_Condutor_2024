# Funções de módulo Geral
# Função de UI.

faixa_idade_ui <- function(id) {
  fluidPage(
   #Painel contendo filtros e Mapa tabela e Gráfico de linnha----
    panel(
      #Controles de filtro 
      fluidRow(
        column(2,
        #Select Ano
        selectInput(
          inputId = NS(id, "ano1"),
          label = "Ano",
          choices = sort(unique(data_05[["ano"]]),decreasing = T),
          width = "200px"
        )
      ),
      column(2,
        #Select Nivel     
        selectInput(
          inputId = NS(id, "nvl"),
          label = "Nivel",
          choices = c("Estadual","Região de Integração","Municipal"),
          width = "200px"
        )
      ),
      column(6,
        #Select Localidade     
        selectInput(
          inputId = NS(id, "localidade"),
          label = "Localidade",
          choices = NULL,
          width = "200px"
        )
      )),

      #Mapa e tabela----
      fluidRow(
      ##Mapa1,tabela1----
      box(
        title = textOutput(NS(id,"titulo_map1")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
        selectInput(
          inputId = NS(id, "dimensao"),
          label = "Dimensão",width = "200px",
          choices = c("Predominância",as.character(unique(data_05$categoria)))
        ), 
        fluidRow(
          column(6,
                 withSpinner(
                   leafletOutput(NS(id,"mapa1")),
                   type = 8,
                   color = "#3C8DBD",
                   size = 0.5
                 )),
          column(6,
                 withSpinner(
                   reactableOutput(NS(id,"tabela1")),
                   type = 8,
                   color = "#3C8DBD",
                   size = 0.5
                 ))),
        footer = list(tags$h6(
          tags$b("Fonte:", style = 'font-family: sans-serif;'), "Detran-PA"
        ),
        tags$h6(tags$b("Elaboração:"), "Detran-PA"))
       )
      ),
      ##Gráfico de linha por ano----
      box(
        title = textOutput(NS(id,"titulo_grafico1")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
        withSpinner(
          echarts4rOutput(NS(id,"grafico1")),
          type = 8,
          color = "#3C8DBD",
          size = 0.5
        ),
        footer = list(tags$h6(
          tags$b("Fonte:", style = 'font-family: sans-serif;'), "Detran-PA"
        ),
        tags$h6(tags$b("Elaboração:"), "Detran-PA"))
      )
    )
  )
}
# Função do modulo servidor
faixa_idade_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
#Filtro Dinâmico----
    nivel <- reactive({
      req(input$nvl)
      data_01 %>% filter(nivel == input$nvl)
    })
    #Atualiz o filtro localidade
    observeEvent(nivel(), {
      choices <- unique(nivel()[["muni"]])
      updateSelectInput(inputId = "localidade", choices = choices, session)
    })
    ##Mapa1----
    
    t_01 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Condutores por Faixa Etária - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Condutores por Faixa Etária - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Condutores por Faixa Etária - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map1 <- renderText({
      t_01()
    })
    
   ###Mapa A---- 
    output$mapa1 <- renderLeaflet({
      
      if (input$dimensao == "Predominância") {
        
        if (input$nvl == "Estadual") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
        
       
        # Criar uma coluna que define a cor com base na faixa etária predominante
        
        dados <- dados %>% 
          mutate(
            valor = pmax(`18-24 anos`, `25-34 anos`, `35-44 anos`, `45-54 anos`, `55-64 anos`, `65-74 anos`, `75 anos mais`, na.rm = TRUE),
            faixa_predominante = case_when(
              valor == 0 ~ "Nenhum",
              `18-24 anos` == valor ~ "18-24 anos",
              `25-34 anos` == valor ~ "25-34 anos",
              `35-44 anos` == valor ~ "35-44 anos",
              `45-54 anos` == valor ~ "45-54 anos",
              `55-64 anos` == valor ~ "55-64 anos",
              `65-74 anos` == valor ~ "65-74 anos",
              `75 anos mais` == valor ~ "75 anos mais",
              TRUE ~ "Indefinido"
            ),
            cor = case_when(
              faixa_predominante == "Nenhum" ~ "gray",
              faixa_predominante == "18-24 anos" ~ "#fdae61",
              faixa_predominante == "25-34 anos" ~ "#fee08b",
              faixa_predominante == "35-44 anos" ~ "#d73027",
              faixa_predominante == "45-54 anos" ~ "#91bfdb",
              faixa_predominante == "55-64 anos" ~ "#4575b4",
              faixa_predominante == "65-74 anos" ~ "#313695",
              faixa_predominante == "75 anos mais" ~ "#542788",
              TRUE ~ "gray"
            )
          )

        # Criar conteúdo para as labels, mostrando o número de condutores por faixa etária e a faixa predominante
        conteudo <- sprintf(
          "<strong>Município de %s</strong><br/>
       <b>18-24 anos:</b> %s<br/>
       <b>25-34 anos:</b> %s<br/>
       <b>35-44 anos:</b> %s<br/>
       <b>45-54 anos:</b> %s<br/>
       <b>55-64 anos:</b> %s<br/>
       <b>65-74 anos:</b> %s<br/>
       <b>75 anos mais:</b> %s<br/>
       <b>Predominância:</b> %s",
          dados$name_muni,
          format(dados$`18-24 anos`, big.mark = ".", decimal.mark = ","),
          format(dados$`25-34 anos`, big.mark = ".", decimal.mark = ","),
          format(dados$`35-44 anos`, big.mark = ".", decimal.mark = ","),
          format(dados$`45-54 anos`, big.mark = ".", decimal.mark = ","),
          format(dados$`55-64 anos`, big.mark = ".", decimal.mark = ","),
          format(dados$`65-74 anos`, big.mark = ".", decimal.mark = ","),
          format(dados$`75 anos mais`, big.mark = ".", decimal.mark = ","),
          dados$faixa_predominante
        ) %>% lapply(htmltools::HTML)
        
        # Renderizar o mapa com leaflet
        leaflet(dados, options = leafletOptions(minZoom = 0, maxZoom = 15,zoomControl = FALSE)) %>%
          addTiles() %>%
          htmlwidgets::onRender(
            "function(el, x) {
            L.control.zoom({ position: 'topright' }).addTo(this);
            }"
          ) %>% 
          addPolygons(
            weight = 2,
            opacity = 1,
            color = "black",
            fillColor = ~cor,
            fillOpacity = 1,
            dashArray = 1,
            smoothFactor = 1.5,
            highlightOptions = highlightOptions(
              weight = 3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5,
              bringToFront = TRUE
            ),
            label = conteudo,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            colors = c("#fdae61", "#fee08b", "#d73027", "#91bfdb", "#4575b4", "#313695", "#542788", "gray"),
            labels = c("18-24 anos", "25-34 anos", "35-44 anos", "45-54 anos", "55-64 anos", "65-74 anos", "75 anos mais", "Indefinido"),
            opacity = 0.7,
            title = "Predominância por Faixa Etária",
            position = "bottomright"
          )
      } else {
        if (input$nvl == "Estadual") {
                dados <- data_05 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao)
                dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
                dados$valor[dados$valor == 0] <- NA
              } else if (input$nvl == "Região de Integração") {
                dados <- data_05 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao)
                dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
                dados <- dados %>% filter(ri == input$localidade)
                dados$valor[dados$valor == 0] <- NA
              } else if (input$nvl == "Municipal") {
                dados <- data_05 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao)
                dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
                dados <- dados %>% filter(name_muni == input$localidade)
              }
            # Quebras com getJenksBreaks, fallback para bins padrão
            z <- dados$valor
            bk <- unique(getJenksBreaks(z, 6, subset = NULL))
            bins <- if (length(bk) < 5)
              pretty(z, 5)
            else
              bk
            # Definir paleta de cores
            pal <- colorBin(palette = "Reds",
                            domain = dados$valor,
                            bins = bins)
            # Criar conteúdo para as labels
            conteudo <-
              sprintf(
                "<strong>Município de %s</strong><br/> <b>Condutores de %s:</b> %s",
                dados$name_muni,
                input$dimensao,
                ifelse(
                  is.na(dados$valor),
                  "Sem Registro",
                  format(dados$valor, big.mark = ".", decimal.mark = ",")
                )
              ) %>% lapply(htmltools::HTML)

## Mapa B ----
            leaflet(dados, options = leafletOptions(minZoom = 0, maxZoom = 15,,zoomControl = FALSE)) %>%
              addTiles() %>%
              htmlwidgets::onRender(
            "function(el, x) {
            L.control.zoom({ position: 'topright' }).addTo(this);
            }"
          ) %>% 
              addPolygons(
                weight = 2,
                opacity = 1,
                color = "black",
                fillColor = ~ pal(valor),
                fillOpacity = 1,
                dashArray = 1,
                smoothFactor = 1.5,
                highlightOptions =
                  highlightOptions(
                    weight = 3,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                label = conteudo,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"
                )
              ) %>%
              addLegend(
                pal = pal,
                values = ~ valor,
                opacity = 0.7,
                title = "Quantidade",
                position = "bottomright",
                na.label = "Não disponível",
                labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",")
              )
      }
      
      
    })
    
    ##Tabela1----
    output$tabela1 <- renderReactable({
      
      if (input$dimensao == "Predominância") {
        
        dt <- if (input$nvl == "Estadual") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% select(-c(nivel,ri,ano))
         
        } else if (input$nvl == "Região de Integração") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(-c(nivel,ri,ano))
          
        } else if (input$nvl == "Municipal") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1,muni == input$localidade)
          dados <- dados %>% select(-c(nivel,ri,muni,ano))
     
        }
        dt %>%
          reactable(
            defaultPageSize = 10,
            highlight = TRUE,
            bordered = TRUE,
            outlined = TRUE,
            resizable = TRUE,
            showSortable = TRUE,
            pagination = FALSE, 
            height = 400,
            list(
              ri = colDef(name = "Região de Integração"),
              muni = colDef(name = "Município",width = 200),
              categoria = colDef(name = "Faixa Etária"),
              `18-24 anos` = colDef(width = 120),
              `25-34 anos` = colDef(width = 120),
              `35-44 anos` = colDef(width = 120),
              `45-54 anos` = colDef(width = 120),
              `55-64 anos` = colDef(width = 120),
              `65-74 anos` = colDef(width = 120),
              `75 anos mais` = colDef(width = 125),
              valor = colDef(
                name = "Nº de Habilitados",
                format = colFormat(
                  separators = T,
                  locales = "pt-BR",
                  digits = 0
                ),
                align = "right"
              )
            ),             
            defaultColDef = 
              colDef(
              footerStyle = list(fontWeight = "bold"),
              headerStyle = list(background = "#f7f7f8")
            ), 
            language = reactableLang(
              noData = "Sem informação",
              pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
              pagePrevious = "Anterior",
              pageNext = "Próximo",
              pagePreviousLabel = "Anterior",
              pageNextLabel = "Proximo"
            )
          )
        
      } else {
        dt <- if (input$nvl == "Estadual") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_05 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao)
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_05 %>% filter(nivel == "Municipal",muni == input$localidade,categoria == input$dimensao)
          dados <- dados %>% select(ano,valor)
          
        }
        dt %>%
          reactable(
            defaultPageSize = 10,
            highlight = TRUE,
            bordered = TRUE,
            outlined = TRUE,
            resizable = TRUE,
            showSortable = TRUE,
            pagination = FALSE, 
            height = 400,
            list(
              Estatistica = colDef(name = "Estatística", width = 450),
              ri = colDef(name = "Região de Integração"),
              muni = colDef(name = "Município"),
              ano = colDef(name = "Ano"),
              categoria = colDef(name = "Faixa Etária"),
              valor = colDef(
                name = "Nº de Habilitados",
                format = colFormat(
                  separators = T,
                  locales = "pt-BR",
                  digits = 0
                ),
                align = "right"
              )
            ), defaultColDef = colDef(
              footerStyle = list(fontWeight = "bold"),
              headerStyle = list(background = "#f7f7f8")
            ), 
            language = reactableLang(
              noData = "Sem informação",
              pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
              pagePrevious = "Anterior",
              pageNext = "Próximo",
              pagePreviousLabel = "Anterior",
              pageNextLabel = "Proximo"
            )
          )
      }
    })
    
    #Gráfico de linha----
   
    tb_03 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Condutores por Faixa Etária - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Condutores por Faixa Etária - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Condutores por Faixa Etária - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_grafico1 <- renderText({
      tb_03()
    })
    
    output$grafico1 <- renderEcharts4r({
      #Base de dados

      dt <- if (input$nvl == "Estadual") {
        data_05 %>% filter(nivel == "Estadual", ano == input$ano1) %>%
          select(ano,categoria,valor)
      } else if (input$nvl == "Região de Integração") {
        data_05 %>% filter(nivel == "Região de Integração",ano == input$ano1) %>%
          filter(ri == input$localidade) %>%
          select(ano,categoria,valor)
      } else if(input$nvl == "Municipal"){
        data_05 %>% filter(nivel == "Municipal") %>%
          filter(muni == input$localidade,ano == input$ano1) %>%
          select(ano,categoria,valor)
      }
     
      
      dt %>% e_chart(x = categoria) %>%
        e_bar(
          serie = valor,
          name = "Quantidade",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(
            color = "#FF5733",
            borderRadius = c(10, 10, 0, 0)
        )
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            padding = c(0, 0, 0, 20),
            verticalAlign = "top",
            lineHeight = 70
          )
        ) %>%
        e_y_axis(
          name = "Quantidade",
          nameTextStyle = list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })

  })
}
# Play do Módulo
# ui = dashboardPage(
#   header = dashboardHeader(),
#            sidebar = dashboardSidebar(),
#            body = dashboardBody(fluidPage(faixa_idade_ui("faixa_idade"))))
# 
# server <- function(input, output) {
#   faixa_idade_Server("faixa_idade")
# }
# 
# shinyApp(ui, server)