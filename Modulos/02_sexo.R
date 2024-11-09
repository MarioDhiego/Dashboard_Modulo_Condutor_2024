# Funções de módulo Geral
# Função de UI.

sexo_ui <- function(id) {
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
          inputId = NS(id, "dimensao"),
          label = "Dimensão",width = "200px",
          choices = c("Predominância", "Feminino", "Masculino")
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
      ##Mapa2,tabela2,UI out----
      box(
        title = textOutput(NS(id,"titulo_map2")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
        selectInput(
          inputId = NS(id, "dimensao_1"),
          label = "Dimensão",width = "200px",
          choices = c("Predominância", "Feminino", "Masculino")
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
                   uiOutput(NS(id,"saida")),
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
sexo_Server <- function(id) {
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
        paste0("Novos Condutores por Sexo - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Novos Condutores por Sexo - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Novos Condutores por Sexo - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map1 <- renderText({
      t_01()
    })
    
    ##Mapa A----
    output$mapa1 <- renderLeaflet({
      
      if (input$dimensao == "Predominância") {
        
        if (input$nvl == "Estadual") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
        
        # Criar uma coluna que define a cor com base no sexo predominante
        dados <- dados %>%
          mutate(
            valor = ifelse(Masculino > Feminino, Masculino, Feminino),  # Usar o maior valor para a cor
            cor = case_when(
              Masculino == 0 & Feminino == 0 ~ "gray",    # Cinza para quando ambos forem 0
              Masculino > Feminino ~ "#1159df",              # Azul para mais homens
              Feminino > Masculino ~ "#e616d4",              # Rosa para mais mulheres
              Masculino == Feminino ~ "#33b60b",            # Verde para empate
              TRUE ~ "gray"                               # Caso de informação faltante
            )
          )
        
        # Criar conteúdo para as labels, mostrando o número de condutores por sexo e o sexo predominante
        conteudo <- sprintf(
          "<strong>Município de %s</strong><br/>
           <b>Homens:</b> %s<br/>
           <b>Mulheres:</b> %s<br/>
           <b>Predominância:</b> %s",
          dados$name_muni,
          format(dados$Masculino, big.mark = ".", decimal.mark = ","),
          format(dados$Feminino, big.mark = ".", decimal.mark = ","),
          ifelse(dados$Masculino > dados$Feminino, "Homens",
          ifelse(dados$Feminino == 0 & dados$Masculino == 0, "Sem Registro", 
                 ifelse(dados$Feminino > dados$Masculino, "Mulheres", "Empate")))
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
            fillColor = ~cor,  # Usar a cor baseada no sexo predominante ou cinza/verde
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
            colors = c("#1159df", "#e616d4", "#33b60b","gray"),  # Incluir a cor verde e cinza na legenda
            labels = c("Predominância Masculina", "Predominância Feminina", "Empate","Sem Registro"),
            opacity = 0.7,
            title = "Predominância por Sexo",
            position = "bottomright"
          )  
      } else {
        if (input$nvl == "Estadual") {
          dados <- data_03 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Região de Integração") {
          dados <- data_03 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Municipal") {
          dados <- data_03 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == input$dimensao)
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }

        #Parei aqui
        # Quebras com getJenksBreaks, fallback para bins padrão
        z <- dados$valor
        bk <- unique(getJenksBreaks(z, 6, subset = NULL))
        bins <- if (length(bk) < 5)
          pretty(z, 5)
        else
          bk
        # Definir paleta de cores
        pal <- colorBin(palette = ifelse(input$dimensao == "Feminino","PuRd","PuBu"),
                        domain = dados$valor,
                        bins = bins)
        # Criar conteúdo para as labels
        conteudo <-
          sprintf(
            "<strong> Município de %s</strong><br/> <b>Condutores do Sexo %s : </b> %s",
            dados$name_muni,
            input$dimensao,
            ifelse(
              is.na(dados$valor),
              "Sem Registro",
              format(dados$valor, big.mark = ".", decimal.mark = ",")
            )
          ) %>% lapply(htmltools::HTML)
        
        # Renderizar o mapa com leaflet
        leaflet(dados, options = leafletOptions(minZoom = 0, maxZoom = 15,zoomControl = FALSE)) %>%
          addTiles() %>%
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
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% select(ri,muni,Masculino,Feminino)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,Masculino,Feminino)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_03 %>% filter(nivel == "Municipal", muni == input$localidade,valor > 0)
          femin <- dados %>% filter(categoria == "Feminino")
          masc <- dados %>% filter(categoria == "Masculino")
          media_f = round(mean(femin$valor),digits = 0)
          media_m = round(mean(masc$valor),digits = 0)
          mediana_f <- median(femin$valor, na.rm = TRUE)
          mediana_m <- median(masc$valor, na.rm = TRUE)
          maximo_f = max(femin$valor)
          minimo_f = min(femin$valor)
          ano_max_f <- femin %>% slice_max(valor, n = 1) %>% pull(ano)
          ano_min_f <- femin %>% slice_min(valor, n = 1,with_ties = FALSE) %>% pull(ano)
          pri_reg_f <- femin$ano[femin$valor > 0][1] 
          pri_valor_f <- femin$valor[femin$valor > 0][1]
          maximo_m = max(masc$valor)
          minimo_m = min(masc$valor)
          ano_max_m <- masc %>% slice_max(valor, n = 1) %>% pull(ano)
          ano_min_m <- masc %>% slice_min(valor, n = 1,with_ties = FALSE) %>% pull(ano)
          pri_reg_m <- masc$ano[masc$valor > 0][1] 
          pri_valor_m <- masc$valor[masc$valor > 0][1]
          
          
          tab_est <-
            tibble(
              Estatistica = c("Média anual de Novos Habilitados do Sexo Feminino",
                              "Média anual de Novos Habilitados do Sexo Masculino",
                              "Mediana anual de Novos Habilitados do Sexo Feminino",
                              "Mediana anual de Novos Habilitados do Sexo Masculino",
                              paste0("Ano com maior Nº de Habilitados do Sexo Feminino ","(",ano_max_f,")"),
                              paste0("Ano com menor Nº de Habilitados do Sexo Feminino ","(",ano_min_f,")"),
                              paste0("Ano do Primeiro Registro do Sexo Feminino","(",pri_reg_f,")"),
                              paste0("Ano com maior Nº de Habilitados do Sexo Masculino ","(",ano_max_m,")"),
                              paste0("Ano com menor Nº de Habilitados do Sexo Masculino ","(",ano_min_m,")"),
                              paste0("Ano do Primeiro Registro do Sexo Masculino","(",pri_reg_m,")")
              ),
              valor = c(media_f,
                        media_m,
                        mediana_f,
                        mediana_m,
                        maximo_f,
                        minimo_f,
                        pri_valor_f,
                        maximo_m,
                        minimo_m,
                        pri_valor_m
              )
            )
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
        
      } else if (input$dimensao == "Feminino"){
        dt <- if (input$nvl == "Estadual") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Feminino")
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Feminino")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(ri,muni,valor)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_03 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == "Feminino")
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
        
      } else if (input$dimensao == "Masculino"){
        dt <- if (input$nvl == "Estadual") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Masculino")
          dados <- dados %>% select(muni,valor)
          
        } else if (input$nvl == "Região de Integração") {
          dados <- data_03 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Masculino")
          dados <- dados %>% filter(ri == input$localidade)
          dados <- dados %>% select(muni,valor)
          
        } else if (input$nvl == "Municipal") {
          dados <- data_03 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == "Masculino")
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
    ##Mapa2----
    
    t_02 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total de Condutores por Sexo - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Total de Condutores por Sexo - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Total de Condutores por Sexo - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map2 <- renderText({
      t_02()
    })
    
    
    output$mapa2 <- renderLeaflet({
      
      if (input$dimensao_1 == "Predominância") {
        
        if (input$nvl == "Estadual") {
          dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1)
          dados <- dados %>% pivot_wider(names_from = "categoria", values_from = "valor")
          dados <- full_join(geopa, dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(name_muni == input$localidade)
        }
        
        # Criar uma coluna que define a cor com base no sexo predominante
        dados <- dados %>%
          mutate(
            valor = ifelse(Masculino > Feminino, Masculino, Feminino),  # Usar o maior valor para a cor
            cor = case_when(
              Masculino == 0 & Feminino == 0 ~ "gray",    # Cinza para quando ambos forem 0
              Masculino > Feminino ~ "#1159df",              # Azul para mais homens
              Feminino > Masculino ~ "#e616d4",              # Rosa para mais mulheres
              Masculino == Feminino ~ "#33b60b",            # Verde para empate
              TRUE ~ "gray"                               # Caso de informação faltante
            )
          )
        
        # Criar conteúdo para as labels, mostrando o número de condutores por sexo e o sexo predominante
        conteudo <- sprintf(
          "<strong> Município de %s</strong><br/>
     <b>Homens:</b> %s<br/>
     <b>Mulheres:</b> %s<br/>
     <b>Predominância:</b> %s",
          dados$name_muni,
          format(dados$Masculino, big.mark = ".", decimal.mark = ","),
          format(dados$Feminino, big.mark = ".", decimal.mark = ","),
          ifelse(dados$Masculino > dados$Feminino, "Homens",
          ifelse(dados$Feminino == 0 & dados$Masculino == 0, "Sem Registro", 
                 ifelse(dados$Feminino > dados$Masculino, "Mulheres", "Empate")))
        ) %>% lapply(htmltools::HTML)
        
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
            fillColor = ~cor,  # Usar a cor baseada no sexo predominante ou cinza/verde
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
            colors = c("#1159df", "#e616d4", "#33b60b","gray"),  # Incluir a cor verde e cinza na legenda
            labels = c("Predominância Masculina", "Predominância Feminina", "Empate","Sem Registro"),
            opacity = 0.7,
            title = "Predominância por Sexo",
            position = "bottomright"
          )  
      } else if (input$dimensao_1 == "Feminino") {
        if (input$nvl == "Estadual") {
          dados <- data_04 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == "Feminino")
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Região de Integração") {
          dados <- data_04 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == "Feminino")
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
          dados$valor[dados$valor == 0] <- NA
        } else if (input$nvl == "Municipal") {
          dados <- data_04 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == "Feminino")
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
        pal <- colorBin(palette = "PuRd",
                        domain = dados$valor,
                        bins = bins)
        # Criar conteúdo para as labels
        conteudo <-
          sprintf(
            "<strong>  Município de %s</strong><br/> <b>Condutores do Sexo: %s</b> %s",
            dados$name_muni,
            input$dimensao_1,
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
      } else if (input$dimensao_1 == "Masculino") {
        if (input$nvl == "Estadual") {
          dados <- data_04 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == "Masculino")
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
        } else if (input$nvl == "Região de Integração") {
          dados <- data_04 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == "Masculino")
          dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
          dados <- dados %>% filter(ri == input$localidade)
        } else if (input$nvl == "Municipal") {
          dados <- data_04 %>% filter(nivel == "Municipal",ano == input$ano1,categoria == "Masculino")
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
        pal <- colorBin(palette = "PuBu",
                        domain = dados$valor,
                        bins = bins)
        # Criar conteúdo para as labels
        conteudo <-
          sprintf(
            "<strong>%s</strong><br/> <b>Condutores do sexo Masculino:</b> %s",
            dados$name_muni,
            ifelse(
              is.na(dados$valor),
              "Não disponível",
              format(dados$valor, big.mark = ".", decimal.mark = ",")
            )
          ) %>% lapply(htmltools::HTML)
        
        # Renderizar o mapa com leaflet
        leaflet(dados, options = leafletOptions(minZoom = 0, maxZoom = 15,zoomControl = FALSE)) %>%
          addTiles() %>%
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
    ##iu dinâmica----
    output$saida <- renderUI({
    
      if (input$nvl == "Municipal" & input$dimensao_1 == "Predominância") {
      
        dt <- data_04 %>% filter(nivel == "Municipal",ano == input$ano1,muni == input$localidade)
        dt %>%  
          e_charts(categoria,height = 400) %>% 
          e_pie(serie = valor) %>% 
          e_color(c("#e616d4","#1159df")) %>% 
          e_tooltip(
          trigger = "item", 
          formatter = htmlwidgets::JS("
          function(params) {
          var valor = params.data.value.toLocaleString('pt-BR');
          var percentual = params.percent.toFixed(2).replace('.', ',');
          return '<b>' + params.name + '</b>' + ' : ' + valor + ' (' + percentual + '%)';
          }
    ")
        ) %>% e_legend(
          bottom = 0
        
        )
          
               
      } else{
        
        if (input$dimensao_1 == "Predominância") {
          
          dt <- if (input$nvl == "Estadual") {
            dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1)
            dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
            dados <- dados %>% select(ri,muni,Masculino,Feminino)
            
          } else if (input$nvl == "Região de Integração") {
            dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1)
            dados <- dados %>% pivot_wider(names_from = "categoria",values_from = "valor")
            dados <- dados %>% filter(ri == input$localidade)
            dados <- dados %>% select(ri,muni,Masculino,Feminino)
            
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
          
        } else if (input$dimensao_1 == "Feminino"){
          dt <- if (input$nvl == "Estadual") {
            dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Feminino")
            dados <- dados %>% select(muni,valor)
            
          } else if (input$nvl == "Região de Integração") {
            dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Feminino")
            dados <- dados %>% filter(ri == input$localidade)
            dados <- dados %>% select(muni,valor)
            
          } else if (input$nvl == "Municipal") {
            dados <- data_04 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == "Feminino")
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
          
        } else if (input$dimensao_1 == "Masculino"){
          dt <- if (input$nvl == "Estadual") {
            dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Masculino")
            dados <- dados %>% select(muni,valor)
            
          } else if (input$nvl == "Região de Integração") {
            dados <- data_04 %>% filter(nivel == "Municipal", ano == input$ano1,categoria == "Masculino")
            dados <- dados %>% filter(ri == input$localidade)
            dados <- dados %>% select(muni,valor)
            
          } else if (input$nvl == "Municipal") {
            dados <- data_04 %>% filter(nivel == "Municipal", muni == input$localidade,categoria == "Masculino")
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
        
      } 
      
    })
      

    
    
    #Gráfico de linha por ano----
    
    tb_03 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Novos Condutores por sexo - Pará")
      } else if (input$nvl == "Região de Integração") {
        paste0("Novos Condutores por sexo - Região de integração ",input$localidade)
      } else if (input$nvl == "Municipal") {
        paste0("Novos Condutores por sexo - Municípipio de ",input$localidade)}
    })
    output$titulo_grafico1 <- renderText({
      tb_03()
    })
    
    output$grafico1 <- renderEcharts4r({
      #Base de dados
      #Base de Feminino
      a <- if (input$nvl == "Estadual") {
        data_03 %>% filter(nivel == "Estadual",categoria == "Feminino") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_03 %>% filter(nivel == "Região de Integração",categoria == "Feminino") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_03 %>% filter(nivel == "Municipal",categoria == "Feminino") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      #Base de data_02
      b <- if (input$nvl == "Estadual") {
        data_03 %>% filter(nivel == "Estadual",categoria == "Masculino") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_03 %>% filter(nivel == "Região de Integração",categoria == "Masculino") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_03 %>% filter(nivel == "Municipal",categoria == "Masculino") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      a <- a %>% rename(valor1 = valor)  
      b <- b %>% rename(valor2 = valor)
      ab <- full_join(a,b,by = "ano")
      ab %>% e_chart(x = ano) %>%
      e_color(c("#e616d4","#1159df")) %>% 
        e_bar(
          serie = valor1,
          name = "Feminino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_bar(
          serie = valor2,
          name = "Masculino",
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
    #Gráfico de linha total----
    
    tb_04 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total de Condutores por sexo - Pará")
      } else if (input$nvl == "Região de Integração") {
        paste0("Total de Condutores por sexo - Região de integração ",input$localidade)
      } else if (input$nvl == "Municipal") {
        paste0("Total de Condutores por sexo - Municípipio de ",input$localidade)}
    })
    output$titulo_grafico2 <- renderText({
      tb_04()
    })
    
    output$grafico2 <- renderEcharts4r({
      #Base de dados
      #Base de Feminino
      a <- if (input$nvl == "Estadual") {
        data_04 %>% filter(nivel == "Estadual",categoria == "Feminino") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_04 %>% filter(nivel == "Região de Integração",categoria == "Feminino") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_04 %>% filter(nivel == "Municipal",categoria == "Feminino") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      #Base de data_02
      b <- if (input$nvl == "Estadual") {
        data_04 %>% filter(nivel == "Estadual",categoria == "Masculino") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_04 %>% filter(nivel == "Região de Integração",categoria == "Masculino") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_04 %>% filter(nivel == "Municipal",categoria == "Masculino") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      a <- a %>% rename(valor1 = valor)  
      b <- b %>% rename(valor2 = valor)
      ab <- full_join(a,b,by = "ano")
      ab %>% e_chart(x = ano) %>%
      e_color(c("#e616d4","#1159df")) %>% 
        e_line(
          serie = valor1,
          name = "Feminino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
        e_line(
          serie = valor2,
          name = "Masculino",
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
#            body = dashboardBody(fluidPage(sexo_ui("sexo"))))
# 
# server <- function(input, output) {
#   sexo_Server("sexo")
# }
# 
# shinyApp(ui, server)