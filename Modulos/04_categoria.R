# Funções de módulo Geral
# Função de UI.

categoria_ui <- function(id) {
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
          choices = sort(unique(data_01[["ano"]]),decreasing = T),
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
          inputId = NS(id, "dimensao1"),
          label = "Dimensão",width = "200px",
          choices = c("Predominância", 
                      "Categoria A" = "A",
                      "Categoria B" = "B",
                      "Categoria C" = "C",
                      "Categoria D" = "D",
                      "Categoria E" = "E")
        ), 
        fluidRow(
          column(5,
                 withSpinner(
                   leafletOutput(NS(id,"mapa1")),
                   type = 8,
                   color = "#3C8DBD",
                   size = 0.5
                 )),
          column(7,
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
      ),
      ##Mapa2,tabela2----
      box(
        title = textOutput(NS(id,"titulo_map2")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
        selectInput(
          inputId = NS(id, "dimensao2"),
          label = "Dimensão",width = "200px",
          choices = c("Predominância", 
                      "Categoria A" = "A",
                      "Categoria B" = "B",
                      "Categoria C" = "C",
                      "Categoria D" = "D",
                      "Categoria E" = "E")
        ), 
        
        fluidRow(
          column(5,
                 withSpinner(
                   leafletOutput(NS(id,"mapa2")),
                   type = 8,
                   color = "#3C8DBD",
                   size = 0.5
                 )),
          column(7,
                 withSpinner(
                   reactableOutput(NS(id,"tabela2")),
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
      ##Gráfico de barras por ano----
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
      ),
      ##Gráfico de linha anual----
      box(
        title = textOutput(NS(id,"titulo_grafico2")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
        withSpinner(
          echarts4rOutput(NS(id,"grafico2")),
          type = 8,
          color = "#3C8DBD",
          size = 0.5
        ),
        footer = list(tags$h6(
          tags$b("Fonte:", style = 'font-family: sans-serif;'), "Detran-PA"
        ),
        tags$h6(tags$b("Elaboração:"), "Detran-PA"))
      ),
      ##Gráfico de linha total----
      box(
        title = textOutput(NS(id,"titulo_grafico3")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
        withSpinner(
          echarts4rOutput(NS(id,"grafico3")),
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
categoria_Server <- function(id) {
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
    
    t_01 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Novos Condutores por Categoria - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Novos Condutores por Categoria - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Novos Condutores por Categoria - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map1 <- renderText({
      t_01()
    })
    
    
    #Mapa1----
    output$mapa1 <- renderLeaflet({
      
      if (input$dimensao1 == "Predominância") {
        
        if (input$nvl == "Estadual") {
          dados <- data_06 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_06 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_06 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
      
        dados <- dados %>% 
          mutate(
            valor = pmax(A, B, C, D, E, na.rm = TRUE),
            categoria_predominante = case_when(
              valor == 0 ~ "Nenhum",
              A == valor ~ "Categoria A",
              B == valor ~ "Categoria B",
              C == valor ~ "Categoria C",
              D == valor ~ "Categoria D",
              E == valor ~ "Categoria E",
              TRUE ~ "Indefinido"
            ),
            cor = case_when(
              categoria_predominante == "Nenhum" ~ "gray",
              categoria_predominante == "Categoria A" ~ "#fdae61",
              categoria_predominante == "Categoria B" ~ "#fee08b",
              categoria_predominante == "Categoria C" ~ "#d73027",
              categoria_predominante == "Categoria D" ~ "#91bfdb",
              categoria_predominante == "Categoria E" ~ "#4575b4",
              TRUE ~ "gray"
            )
          )
        
        conteudo <- sprintf(
          "<strong>Município de %s</strong><br/> 
  <b>Categoria A:</b> %s<br/> 
  <b>Categoria B:</b> %s<br/> 
  <b>Categoria C:</b> %s<br/> 
  <b>Categoria D:</b> %s<br/> 
  <b>Categoria E:</b> %s<br/> 
  <b>Predominância:</b> %s",
          dados$name_muni,
          format(dados$A, big.mark = ".", decimal.mark = ","),
          format(dados$B, big.mark = ".", decimal.mark = ","),
          format(dados$C, big.mark = ".", decimal.mark = ","),
          format(dados$D, big.mark = ".", decimal.mark = ","),
          format(dados$E, big.mark = ".", decimal.mark = ","),
          dados$categoria_predominante
        ) %>% 
          lapply(htmltools::HTML)
        ##Mapa A----
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
            colors = c("#fdae61", "#fee08b", "#d73027", "#91bfdb", "#4575b4", "gray"),
            labels = c("Categoria A", "Categoria B", "Categoria C", "Categoria D", "Categoria E", "Sem Registro"),
            opacity = 0.7,
            title = "Categoria de Habilitação",
            position = "bottomright"
          )
      } else {
        if (input$nvl == "Estadual") {
          dados <- data_06 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao1)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          if(sum(dados$valor != 0)){
          dados$valor[dados$valor == 0] <- NA
          }
        } else if (input$nvl == "Região de Integração") {
          dados <- data_06 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao1)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)         
          if(sum(dados$valor != 0)){
          dados$valor[dados$valor == 0] <- NA
          }
        } else if (input$nvl == "Municipal") {
          dados <- data_06 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao1)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
        # Quebras com getJenksBreaks, fallback para bins padrão
        z <- dados$valor
        bk <- unique(getJenksBreaks(z, 6, subset = NULL))

        if(input$nvl != "Municipal" & input$dimensao1 != "Predominância"){
          dados$valor[dados$valor == 0] <- NA
        }

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
            "<strong>%s</strong><br/> <b>Condutores de %s:</b> %s",
            dados$name_muni,
            input$dimensao1,
            ifelse(
              is.na(dados$valor),
              "Não disponível",
              format(dados$valor, big.mark = ".", decimal.mark = ",")
            )
          ) %>% lapply(htmltools::HTML)
        
        ##Mapa B----
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
      
      if (input$dimensao1 == "Predominância") {
        
        dt <- if (input$nvl == "Estadual") {
          dados <- data_06 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% select(ri,muni,A,B,C,D,E)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_06 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,A,B,C,D,E)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_06 %>% filter(nivel == "Municipal", muni == input$localidade,ano == input$ano1)
          dados <- dados %>% select(categoria,valor)
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
              ri = colDef(name = "Região de Integração",width = 200),
              muni = colDef(name = "Municípios",width = 200,sticky = "left"),
              categoria = colDef(name = "Categoria da Habilitação"),
              A = colDef(name = "Categoria A",width = 115),
              B = colDef(name = "Categoria B",width = 115),
              C = colDef(name = "Categoria C",width = 115),
              D = colDef(name = "Categoria D",width = 115),
              E = colDef(name = "Categoria E",width = 115),
              valor = colDef(name = "Nº de Condutores",
                              format = colFormat(separators = T,locales = "pt-BR",digits = 0),
                              align = "right"
              )
            ), 
            defaultColDef = colDef(
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
          dados <- data_06 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao1)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_06 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao1)
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_06 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == input$dimensao1)
          dados <- dados %>% select(ano,valor) %>% arrange(desc(ano))

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
              muni = colDef(name = "Municípios"),
              ano = colDef(name = "Ano"),
              valor = colDef(
                name = paste0("Nº de Habilitados - Categoria ",input$dimensao1),
                format = colFormat(
                  separators = T,
                  locales = "pt-BR",
                  digits = 0
                ),
                align = "right"
              )
            ), 
            defaultColDef = colDef(
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

    
    t_02 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total de Condutores por Categoria - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Total de Condutores por Categoria - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Total de Condutores por Categoria - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map2 <- renderText({
      t_02()
    })
    ##Mapa2----
    output$mapa2 <- renderLeaflet({
      
      if (input$dimensao2 == "Predominância") {
        
        if (input$nvl == "Estadual") {
          dados <- data_07 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_07 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_07 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
        
        dados <- dados %>% 
          mutate(
            valor = pmax(A, B, C, D, E, na.rm = TRUE),
            categoria_predominante = case_when(
              valor == 0 ~ "Nenhum",
              A == valor ~ "Categoria A",
              B == valor ~ "Categoria B",
              C == valor ~ "Categoria C",
              D == valor ~ "Categoria D",
              E == valor ~ "Categoria E",
              TRUE ~ "Indefinido"
            ),
            cor = case_when(
              categoria_predominante == "Nenhum" ~ "gray",
              categoria_predominante == "Categoria A" ~ "#fdae61",
              categoria_predominante == "Categoria B" ~ "#fee08b",
              categoria_predominante == "Categoria C" ~ "#d73027",
              categoria_predominante == "Categoria D" ~ "#91bfdb",
              categoria_predominante == "Categoria E" ~ "#4575b4",
              TRUE ~ "gray"
            )
          )
        
        conteudo <- sprintf(
          "<strong>%s</strong><br/> 
  <b>Categoria A:</b> %s<br/> 
  <b>Categoria B:</b> %s<br/> 
  <b>Categoria C:</b> %s<br/> 
  <b>Categoria D:</b> %s<br/> 
  <b>Categoria E:</b> %s<br/> 
  <b>Predominância:</b> %s",
          dados$name_muni,
          format(dados$A, big.mark = ".", decimal.mark = ","),
          format(dados$B, big.mark = ".", decimal.mark = ","),
          format(dados$C, big.mark = ".", decimal.mark = ","),
          format(dados$D, big.mark = ".", decimal.mark = ","),
          format(dados$E, big.mark = ".", decimal.mark = ","),
          dados$categoria_predominante
        ) %>% 
          lapply(htmltools::HTML)
        ##Mapa A----
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
            colors = c("#fdae61", "#fee08b", "#d73027", "#91bfdb", "#4575b4", "gray"),
            labels = c("Categoria A", "Categoria B", "Categoria C", "Categoria D", "Categoria E", "Sem Registro"),
            opacity = 0.7,
            title = "Categoria de Habilitação",
            position = "bottomright"
          )
      } else {
        if (input$nvl == "Estadual") {
          dados <- data_07 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao2)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Região de Integração") {
          dados <- data_07 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao2)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Municipal") {
          dados <- data_07 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao2)
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
            "<strong>%s</strong><br/> <b>Condutores de %s:</b> %s",
            dados$name_muni,
            input$dimensao2,
            ifelse(
              is.na(dados$valor),
              "Não disponível",
              format(dados$valor, big.mark = ".", decimal.mark = ",")
            )
          ) %>% lapply(htmltools::HTML)
        
        #Mapa B----
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
            na.label = "Sem Registro",
            labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",")
          )
      }
    })
    
    ##Tabela2----
     output$tabela2 <- renderReactable({
      
      if (input$dimensao2 == "Predominância") {
        
        dt <- if (input$nvl == "Estadual") {
          dados <- data_07 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% select(ri,muni,A,B,C,D,E)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_07 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,A,B,C,D,E)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_07 %>% filter(nivel == "Municipal", muni == input$localidade,ano == input$ano1)
          dados <- dados %>% select(categoria,valor)
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
              ri = colDef(name = "Região de Integração",width = 200),
              muni = colDef(name = "Municípios",width = 200,sticky = "left"),
              categoria = colDef(name = "Categoria da Habilitação"),
              A = colDef(name = "Categoria A",width = 115),
              B = colDef(name = "Categoria B",width = 115),
              C = colDef(name = "Categoria C",width = 115),
              D = colDef(name = "Categoria D",width = 115),
              E = colDef(name = "Categoria E",width = 115),
              valor = colDef(name = "Nº de Condutores",
                              format = colFormat(separators = T,locales = "pt-BR",digits = 0),
                              align = "right"
              )
            ), 
            defaultColDef = colDef(
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
          dados <- data_07 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao2)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_07 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao2)
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_07 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == input$dimensao2)
          dados <- dados %>% select(ano,valor) %>% arrange(desc(ano))
          
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
              muni = colDef(name = "Municípios"),
              ano = colDef(name = "Ano"),
              valor = colDef(
                name = paste0("Nº de Habilitados - Categoria ",input$dimensao2),
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
    
    #Gráfico de barras por ano----
    tb_03 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total e Novos Condutores por Categoria - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Total e Novos Condutores por Categoria - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Total e Novos Condutores por Categoria - Municípipio de ",input$localidade," - ",input$ano1)
        }
    })
    output$titulo_grafico1 <- renderText({
      tb_03()
    })
    
    output$grafico1 <- renderEcharts4r({
      #Base de dados  
      #Base de data_06
      a <- if (input$nvl == "Estadual") {
        data_06 %>% filter(nivel == "Estadual",ano == input$ano1)
          
      } else if (input$nvl == "Região de Integração") {
        data_06 %>% filter(nivel == "Região de Integração",ano == input$ano1) %>%
          filter(ri == input$localidade)
          
      } else if(input$nvl == "Municipal"){
        data_06 %>% filter(nivel == "Municipal",ano == input$ano1) %>%
          filter(muni == input$localidade)
          
      }

      #Base de data_07
      b <- if (input$nvl == "Estadual") {
        data_07 %>% filter(nivel == "Estadual",ano == input$ano1)
          
      } else if (input$nvl == "Região de Integração") {
        data_07 %>% filter(nivel == "Região de Integração",ano == input$ano1) %>%
          filter(ri == input$localidade)
          
      } else if(input$nvl == "Municipal"){
        data_07 %>% filter(nivel == "Municipal",ano == input$ano1) %>%
          filter(muni == input$localidade)
          
      }
      
      a <- a %>% mutate( 
        categoria = case_when(
                    categoria == "A" ~ "Categoria A",
                    categoria == "B" ~ "Categoria B",
                    categoria == "C" ~ "Categoria C",
                    categoria == "D" ~ "Categoria D",
                    categoria == "E" ~ "Categoria E"
    )
  )
     b <- b %>% mutate( 
        categoria = case_when(
                    categoria == "A" ~ "Categoria A",
                    categoria == "B" ~ "Categoria B",
                    categoria == "C" ~ "Categoria C",
                    categoria == "D" ~ "Categoria D",
                    categoria == "E" ~ "Categoria E"
    )
  )
#Gráfico 
      a %>% e_chart(categoria) %>%
        e_bar(
          serie = valor,
          name = "Novos Condutores",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          barWidth = 100, 
          color = "#006eff",
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_data(b, categoria) %>%
        e_bar(
          y_index = 1,
          serie = valor,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          barWidth = 100, 
          color = "#ff000d",
          itemStyle = list(barBorderRadius = 5)
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
          name = "Novos Condutores",
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
        e_y_axis(
          index = 1,
          name = "Total",
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
        e_grid(show = T)
    })
    #Gráfico de linha anual----
    tb_04 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Novos Condutores por Categoria - Pará")
      } else if (input$nvl == "Região de Integração") {
        paste0("Novos Condutores por Categoria - Região de integração ",input$localidade)
      } else if (input$nvl == "Municipal") {
        paste0("Novos Condutores por Categoria - Municípipio de ",input$localidade)}
    })
    output$titulo_grafico2 <- renderText({
      tb_04()
    })
    
    output$grafico2 <- renderEcharts4r({
      #Base de dados
      #Base de data_06
      a1 <- if (input$nvl == "Estadual") {
        data_06 %>% filter(nivel == "Estadual",categoria == "A") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_06 %>% filter(nivel == "Região de Integração",categoria == "A") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_06 %>% filter(nivel == "Municipal",categoria == "A") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      b1 <- if (input$nvl == "Estadual") {
        data_06 %>% filter(nivel == "Estadual",categoria == "B") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_06 %>% filter(nivel == "Região de Integração",categoria == "B") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_06 %>% filter(nivel == "Municipal",categoria == "B") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      c1 <- if (input$nvl == "Estadual") {
        data_06 %>% filter(nivel == "Estadual",categoria == "C") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_06 %>% filter(nivel == "Região de Integração",categoria == "C") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_06 %>% filter(nivel == "Municipal",categoria == "C") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      d1 <- if (input$nvl == "Estadual") {
        data_06 %>% filter(nivel == "Estadual",categoria == "D") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_06 %>% filter(nivel == "Região de Integração",categoria == "D") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_06 %>% filter(nivel == "Municipal",categoria == "D") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      e1 <- if (input$nvl == "Estadual") {
        data_06 %>% filter(nivel == "Estadual",categoria == "E") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_06 %>% filter(nivel == "Região de Integração",categoria == "E") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_06 %>% filter(nivel == "Municipal",categoria == "E") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      a1 %>% e_chart(ano) %>%
        e_line(
          serie = valor,
          name = "Categoria A",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(b1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria B",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(c1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria C",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(d1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria D",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(e1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria E",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
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
    #Gráfico de linha Total----
    tb_05 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total de Condutores por Categoria - Pará")
      } else if (input$nvl == "Região de Integração") {
        paste0("Total de Condutores por Categoria - Região de integração ",input$localidade)
      } else if (input$nvl == "Municipal") {
        paste0("Total de Condutores por Categoria - Municípipio de ",input$localidade)}
    })
    output$titulo_grafico3 <- renderText({
      tb_05()
    })
    
    output$grafico3 <- renderEcharts4r({
      #Base de dados
      #Base de data_07
      a1 <- if (input$nvl == "Estadual") {
        data_07 %>% filter(nivel == "Estadual",categoria == "A") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_07 %>% filter(nivel == "Região de Integração",categoria == "A") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_07 %>% filter(nivel == "Municipal",categoria == "A") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      b1 <- if (input$nvl == "Estadual") {
        data_07 %>% filter(nivel == "Estadual",categoria == "B") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_07 %>% filter(nivel == "Região de Integração",categoria == "B") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_07 %>% filter(nivel == "Municipal",categoria == "B") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      c1 <- if (input$nvl == "Estadual") {
        data_07 %>% filter(nivel == "Estadual",categoria == "C") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_07 %>% filter(nivel == "Região de Integração",categoria == "C") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_07 %>% filter(nivel == "Municipal",categoria == "C") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      d1 <- if (input$nvl == "Estadual") {
        data_07 %>% filter(nivel == "Estadual",categoria == "D") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_07 %>% filter(nivel == "Região de Integração",categoria == "D") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_07 %>% filter(nivel == "Municipal",categoria == "D") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      e1 <- if (input$nvl == "Estadual") {
        data_07 %>% filter(nivel == "Estadual",categoria == "E") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_07 %>% filter(nivel == "Região de Integração",categoria == "E") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_07 %>% filter(nivel == "Municipal",categoria == "E") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      a1 %>% e_chart(ano) %>%
        e_line(
          serie = valor,
          name = "Categoria A",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(b1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria B",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(c1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria C",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(d1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria D",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(e1, ano) %>%
        e_line(
          serie = valor,
          name = "Categoria E",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
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
#            body = dashboardBody(fluidPage(categoria_ui("categoria"))))
# 
# server <- function(input, output) {
#   categoria_Server("categoria")
# }
# 
# shinyApp(ui, server)