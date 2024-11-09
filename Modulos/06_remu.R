# Funções de módulo Geral
# Função de UI.

remu_ui <- function(id) {
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
          choices = c("Predominância","Sim","Não")
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
          choices = c("Predominância","Sim","Não")
        ), 
        
        fluidRow(
          column(6,
                 withSpinner(
                   leafletOutput(NS(id,"mapa2")),
                   type = 8,
                   color = "#3C8DBD",
                   size = 0.5
                 )),
          column(6,
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

      ##Gráfico de linha anual----
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
      ##Gráfico de linha total----
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
      )
    )
  )
}
# Função do modulo servidor
remu_Server <- function(id) {
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
#Mapa1----
    
    t_01 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Novos Condutores: Habilitação com ou sem Atividade Remunerada - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Novos Condutores: Habilitação com ou sem Atividade Remunerada - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Novos Condutores: Habilitação com ou sem Atividade Remunerada - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map1 <- renderText({
      t_01()
    })
     
    output$mapa1 <- renderLeaflet({
      
      if (input$dimensao1 == "Predominância") {
        
        if (input$nvl == "Estadual") {
          dados <- data_10 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_10 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_10 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
      
        dados <- dados %>% 
          mutate(
            valor = pmax(Sim,Não, na.rm = TRUE),
            categoria_predominante = case_when(
              valor == 0 ~ "Sem Registro",
              Sim == valor ~ "Sim",
              Não == valor ~ "Não",
              TRUE ~ "Indefinido"
            ),
            cor = case_when(
              categoria_predominante == "Sem Registro" ~ "gray",
              categoria_predominante == "Sim" ~ "#fdae61",
              categoria_predominante == "Não" ~ "#fee08b",
              TRUE ~ "gray"
            )
          )
        
        conteudo <- sprintf(
          "<strong>Município de %s</strong><br/> 
          <b>Sim:</b> %s<br/> 
          <b>Não:</b> %s<br/> 
          <b>Predominância:</b> %s",
          dados$name_muni,
          format(dados$Sim, big.mark = ".", decimal.mark = ","),
          format(dados$Não, big.mark = ".", decimal.mark = ","),
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
            colors = c("#fdae61", "#fee08b","gray"),
            labels = c("Sim", "Não","Sem Registro"),
            opacity = 0.7,
            title = "Atividade Remunerada",
            position = "bottomright"
          )
      } else {
        if (input$nvl == "Estadual") {
          dados <- data_10 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao1)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Região de Integração") {
          dados <- data_10 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao1)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Municipal") {
          dados <- data_10 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao1)
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
            ifelse(input$dimensao1 == "Sim",
            "<strong>Município de %s</strong><br/> <b>Atividade Remunerada %s:</b>",
            "<strong>Município de %s</strong><br/> <b>Atividade não Remunerada %s:</b>"
            ),
            dados$name_muni,
            ifelse(
              is.na(dados$valor),
              "Sem Registro",
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
            na.label = "Sem Registro",
            labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",")
          )
      }
    })
    ##Tabela1----
    output$tabela1 <- renderReactable({
      
      if (input$dimensao1 == "Predominância") {
        
        dt <- if (input$nvl == "Estadual") {
          dados <- data_10 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% select(ri,muni,Sim,Não)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_10 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,Sim,Não)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_10 %>% filter(nivel == "Municipal", muni == input$localidade)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% select(ano,Sim,Não) %>% arrange(desc(ano))
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
              categoria = colDef(name = "Categoria da Habilitação"),
              valor = colDef(name = "Nº de Habilitados",format = colFormat(separators = T,locales = "pt-BR",digits = 0),
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
        
      } else {
        dt <- if (input$nvl == "Estadual") {
          dados <- data_10 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao1)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_10 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao1)
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_10 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == input$dimensao1)
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
                name = paste0("Quantidade"),
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

    #Mapa2----
    t_02 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total de Condutores: Habilitação com ou sem Atividade Remunerada - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Total de Condutores: Habilitação com ou sem Atividade Remunerada - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Total de Condutores: Habilitação com ou sem Atividade Remunerada - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map2 <- renderText({
      t_02()
    })
    
    output$mapa2 <- renderLeaflet({
      
      if (input$dimensao2 == "Predominância") {
        
        if (input$nvl == "Estadual") {
          dados <- data_11 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_11 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_11 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
        
        dados <- dados %>% 
          mutate(
            valor = pmax(Sim,Não, na.rm = TRUE),
            categoria_predominante = case_when(
              valor == 0 ~ "Sem Registro",
              Sim == valor ~ "Sim",
              Não == valor ~ "Não",
              TRUE ~ "Sem Registro"
            ),
            cor = case_when(
              categoria_predominante == "Sem Registro" ~ "gray",
              categoria_predominante == "Sim" ~ "#fdae61",
              categoria_predominante == "Não" ~ "#fee08b",
              TRUE ~ "gray"
            )
          )
        
        conteudo <- sprintf(
          "<strong>Município de %s</strong><br/> 
          <b>Sim:</b> %s<br/> 
          <b>Não:</b> %s<br/> 
          <b>Predominância:</b> %s",
          dados$name_muni,
          format(dados$Sim, big.mark = ".", decimal.mark = ","),
          format(dados$Não, big.mark = ".", decimal.mark = ","),
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
            colors = c("#fdae61", "#fee08b","gray"),
            labels = c("Sim", "Não","Sem Registro"),
            opacity = 0.7,
            title = "Atividade Remunerada",
            position = "bottomright"
          )

      } else {
        if (input$nvl == "Estadual") {
          dados <- data_11 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao2)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Região de Integração") {
          dados <- data_11 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao2)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Municipal") {
          dados <- data_11 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao2)
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
            ifelse(input$dimensao2 == "Sim",
            "<strong>Município de %s</strong><br/> <b>Atividade Remunerada:</b> %s",
            "<strong>Município de %s</strong><br/> <b>Atividade não Remunerada:</b> %s"
            ),
            dados$name_muni,
            ifelse(
              is.na(dados$valor),
              "Sem Registro",
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
            na.label = "Sem Registro",
            labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",")
          )
      }
    })
    
    ##Tabela2----
    output$tabela2 <- renderReactable({
      
      if (input$dimensao2 == "Predominância") {
        
        dt <- if (input$nvl == "Estadual") {
          dados <- data_11 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% select(ri,muni,Sim,Não)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_11 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,Sim,Não)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_11 %>% filter(nivel == "Municipal", muni == input$localidade,ano == input$ano1)
          dados <- dados %>% select(ri,muni,Sim,Não)
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
              categoria = colDef(name = "Categoria da Habilitação"),
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
        
      } else {
         dt <- if (input$nvl == "Estadual") {
          dados <- data_11 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao2)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_11 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == input$dimensao2)
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_11 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == input$dimensao2)
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
                name = paste0("Quantidade"),
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
    
    #Gráfico de linha anual----
    tb_03 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Novos Condutores: Habilitação com ou sem Atividade Remunerada - Pará")
      } else if (input$nvl == "Região de Integração") {
        paste0("Novos Condutores: Habilitação com ou sem Atividade Remunerada - Região de integração ",input$localidade)
      } else if (input$nvl == "Municipal") {
        paste0("Novos Condutores: Habilitação com ou sem Atividade Remunerada - Municípipio de ",input$localidade)}
    })
    output$titulo_grafico1 <- renderText({
      tb_03()
    })
    
    output$grafico1 <- renderEcharts4r({
      #Base de dados
      #Base de data_10
      a1 <- if (input$nvl == "Estadual") {
        data_10 %>% filter(nivel == "Estadual",categoria == "Sim") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_10 %>% filter(nivel == "Região de Integração",categoria == "Sim") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_10 %>% filter(nivel == "Municipal",categoria == "Sim") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      b1 <- if (input$nvl == "Estadual") {
        data_10 %>% filter(nivel == "Estadual",categoria == "Não") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_10 %>% filter(nivel == "Região de Integração",categoria == "Não") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_10 %>% filter(nivel == "Municipal",categoria == "Não") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }    
      a1 %>% e_chart(ano) %>%
        e_line(
          serie = valor,
          name = "Sim",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(b1, ano) %>%
        e_line(
          serie = valor,
          name = "Não",
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
    tb_04 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total de Condutores: Habilitação com ou sem Atividade Remunerada - Pará")
      } else if (input$nvl == "Região de Integração") {
        paste0("Total de Condutores: Habilitação com ou sem Atividade Remunerada - Região de integração ",input$localidade)
      } else if (input$nvl == "Municipal") {
        paste0("Total de Condutores: Habilitação com ou sem Atividade Remunerada - Municípipio de ",input$localidade)}
    })
    output$titulo_grafico2 <- renderText({
      tb_04()
    })
    
    output$grafico2 <- renderEcharts4r({
      #Base de dados
      #Base de data_11
      a1 <- if (input$nvl == "Estadual") {
        data_11 %>% filter(nivel == "Estadual",categoria == "Sim") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_11 %>% filter(nivel == "Região de Integração",categoria == "Sim") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_11 %>% filter(nivel == "Municipal",categoria == "Sim") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      b1 <- if (input$nvl == "Estadual") {
        data_11 %>% filter(nivel == "Estadual",categoria == "Não") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_11 %>% filter(nivel == "Região de Integração",categoria == "Não") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_11 %>% filter(nivel == "Municipal",categoria == "Não") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      
      a1 %>% e_chart(ano) %>%
        e_line(
          serie = valor,
          name = "Sim",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_data(b1, ano) %>%
        e_line(
          serie = valor,
          name = "Não",
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
#            body = dashboardBody(fluidPage(remu_ui("remu"))))
# 
# server <- function(input, output) {
#   remu_Server("remu")
# }
# 
# shinyApp(ui, server)