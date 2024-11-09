# Funções de módulo Geral
# Função de UI.

quantidade_ui <- function(id) {
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
      #Infobox's----
      panel(
      fluidRow(
        bs4InfoBoxOutput(NS(id,"total_pa_ano"),width = 6),
        bs4InfoBoxOutput(NS(id,"total_pa"),width = 6)
      )),
      #Mapa e tabela----
      fluidRow(
      ##Mapa1----
      box(
        title = textOutput(NS(id,"titulo_map1")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
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
      ##Mapa2----
      box(
        title = textOutput(NS(id,"titulo_map2")),
        status = "primary",
        collapsed = F,
        headerBorder = T,
        width = 12,
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
      ##Gráfico de linha----
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
quantidade_Server <- function(id) {
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
#Caixas de Valor----
    ##Total Habilitados no ano----
    output$total_pa_ano <- renderInfoBox({
      if (input$nvl == "Estadual" | input$nvl == "Região de Integração") {
        valor <- 
          data_01 %>% 
          filter(ri == input$localidade, ano == input$ano1) %>%
          summarise(total = sum(valor, na.rm = TRUE))  
      } else{
      valor <- 
        data_01 %>% 
        filter(muni == input$localidade, ano == input$ano1) %>%
        summarise(total = sum(valor, na.rm = TRUE))
      }
      bs4InfoBox(
        title = tags$strong(input$localidade),
        value = tags$h2(tags$strong(
          prettyNum(
            valor$total,
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE
          )
        )),
        subtitle = paste0("Novos Condutores em ",input$ano1),
        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        color = "purple",
        icon("users")
      )
    })
##Total Pará Habilitados----
    output$total_pa <- renderInfoBox({
      
      if (input$nvl == "Estadual" | input$nvl == "Região de Integração") {
        valor <- 
          data_02 %>% 
          filter(ri == input$localidade, ano == input$ano1) %>%
          summarise(total = sum(valor, na.rm = TRUE))
      } else{
        valor <- 
          data_02 %>% 
          filter(muni == input$localidade, ano == input$ano1) %>%
          summarise(total = sum(valor, na.rm = TRUE))
      }
      
      bs4InfoBox(
        title = tags$strong(input$localidade),
        value = tags$h2(tags$strong(
          prettyNum(
            valor$total,
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE
          )
        )),
        subtitle = paste0("Total de Condutores em ",input$ano1),
        color = "primary",
        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        icon = icon("map")
      )
    })

    
    ##Mapa1----
    t_01 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Novos Condutores - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Novos Condutores - Região de integração ",input$localidade," - em ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Novos Condutores - Municípipio de ",input$localidade," - em ",input$ano1)}
    })
    output$titulo_map1 <- renderText({
      t_01()
    })
    
    output$mapa1 <- renderLeaflet({
      if (input$nvl == "Estadual") {
        dados <- data_01 %>% filter(nivel == "Municipal",ano == input$ano1)
        dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
        dados$valor[dados$valor == 0] <- NA
      } else if (input$nvl == "Região de Integração") {
        dados <- data_01 %>% filter(nivel == "Municipal",ano == input$ano1)
        dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
        dados <- dados %>% filter(ri == input$localidade)
        dados$valor[dados$valor == 0] <- NA
      } else if (input$nvl == "Municipal") {
        dados <- data_01 %>% filter(nivel == "Municipal",ano == input$ano1)
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
          "<strong>%s</strong><br/> <b>Condutores:</b> %s",
          dados$name_muni,
          ifelse(
            is.na(dados$valor),
            "Não disponível",
            format(dados$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Renderizar o mapa com leaflet
      leaflet(dados, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          title = "Condutores",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",")
        )   
    })
    
    ##Tabela 1----
    output$tabela1 <- renderReactable({
      dt <- if (input$nvl == "Estadual") {
        data_01 %>% filter(nivel == "Municipal",ano == input$ano1) %>% 
          select(ri,muni,valor)
      } else if (input$nvl == "Região de Integração") {
        data_01 %>% filter(nivel == "Municipal",ano == input$ano1) %>% 
          filter(ri == input$localidade) %>% 
          select(ri,muni,valor)
      } else if(input$nvl == "Municipal"){
        est <- data_01 %>% filter(nivel == "Municipal") %>% 
          filter(muni == input$localidade,valor > 0) %>%  
          select(ano,valor)
        media = round(mean(est$valor),digits = 0)
        mediana <- median(est$valor, na.rm = TRUE)
        desvio_padrao <- sd(est$valor, na.rm = TRUE)
        cv_valor <- (desvio_padrao / media) * 100
        maximo = max(est$valor)
        minimo = min(est$valor)
        ano_max <- est %>% slice_max(valor, n = 1) %>% pull(ano)
        ano_min <- est %>% slice_min(valor, n = 1,with_ties = FALSE) %>% pull(ano)
        pri_reg <- est$ano[est$valor > 0][1] 
        pri_valor <- est$valor[est$valor > 0][1]
        tab_est <- 
          tibble(
          Estatistica = c("Média anual de Novos Habilitados",
                          "Mediana anual de novos habilitados",
                          "Desvio padrão anual de novos habilitados",
                          paste0("Ano com maior Nº de Habilitados ","(",ano_max,")"),
                          paste0("Ano com menor Nº de Habilitados ","(",ano_min,")"),
                          paste0("Ano do Primeiro Registro","(",pri_reg,")"),
                          "Coeficiente de Variação(%)"),
          valor = c(media,mediana,desvio_padrao,maximo,minimo,pri_valor,cv_valor)
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
            ri = colDef(name = "Região de Integração"),
            muni = colDef(name = "Municípios"),
            valor = colDef(name = "Nº de Habilitados",format = colFormat(separators = T, locales = "pt-BR",digits = 0),align = "right")
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
    })
    
    
##Mapa2----
    t_02 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total de Condutores - Pará - ",input$ano1)
      } else if (input$nvl == "Região de Integração") {
        paste0("Total de Condutores - Região de integração ",input$localidade," - ",input$ano1)
      } else if (input$nvl == "Municipal") {
        paste0("Total de Condutores - Municípipio de ",input$localidade," - ",input$ano1)}
    })
    output$titulo_map2 <- renderText({
      t_02()
    })

    output$mapa2 <- renderLeaflet({
       if (input$nvl == "Estadual") {
        dados <- data_02 %>% filter(nivel == "Municipal",ano == input$ano1)
        dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
        dados$valor[dados$valor == 0] <- NA
      } else if (input$nvl == "Região de Integração") {
        dados <- data_02 %>% filter(nivel == "Municipal",ano == input$ano1)
        dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
        dados <- dados %>% filter(ri == input$localidade)
        dados$valor[dados$valor == 0] <- NA
      } else if (input$nvl == "Municipal") {
        dados <- data_02 %>% filter(nivel == "Municipal",ano == input$ano1)
        dados <- full_join(geopa,dados, by = c("name_muni" = "muni"))
        dados <- dados %>% filter(name_muni == input$localidade)
      }
      req(dados)
      # Quebras com getJenksBreaks, fallback para bins padrão
      z <- dados$valor
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      bins <- if (length(bk) < 5)
        pretty(z, 5)
      else
        bk
      # Definir paleta de cores
      pal <- colorBin(palette = "Blues",
                      domain = dados$valor,
                      bins = bins)
      # Criar conteúdo para as labels
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Condutores:</b> %s",
          dados$name_muni,
          ifelse(
            is.na(dados$valor),
            "Não disponível",
            format(dados$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Renderizar o mapa com leaflet
      leaflet(dados, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          title = "Condutores",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",")
        )  
    })
    
    
    ##Tabela 2----
    
    output$tabela2 <- renderReactable({
      x <- if (input$nvl == "Estadual") {
        data_02 %>% filter(nivel == "Municipal",ano == input$ano1) %>% 
          select(ri,muni,valor)
      } else if (input$nvl == "Região de Integração") {
        data_02 %>% filter(nivel == "Municipal",ano == input$ano1) %>% 
          filter(ri == input$localidade) %>% 
          select(ri,muni,valor)
      } else if(input$nvl == "Municipal"){
        data_02 %>% filter(nivel == "Municipal") %>% 
          filter(muni == input$localidade) %>%  
          select(ano,valor)
      }
      x %>%
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
            valor = colDef(name = "Nº de Habilitados",format = colFormat(separators = T, locales = "pt-BR")),
            ano = colDef(name = "Ano")
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
    })
    #Gráfico de linha----
    tb_03 <- reactive({
      if (input$nvl == "Estadual") {
        paste0("Total e Novos Condutores - Pará")
      } else if (input$nvl == "Região de Integração") {
        paste0("Total e Novos Condutores - Região de integração ",input$localidade)
      } else if (input$nvl == "Municipal") {
        paste0("Total e Novos Condutores - Municípipio de ",input$localidade)}
    })
    output$titulo_grafico1 <- renderText({
      tb_03()
    })
    output$grafico1 <- renderEcharts4r({
      #Base de dados
      #Base de data_01
      a <- if (input$nvl == "Estadual") {
        data_01 %>% filter(nivel == "Estadual") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_01 %>% filter(nivel == "Região de Integração") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_01 %>% filter(nivel == "Municipal") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
      #Base de data_02
      b <- if (input$nvl == "Estadual") {
        data_02 %>% filter(nivel == "Estadual") %>%
          select(ano,valor)
      } else if (input$nvl == "Região de Integração") {
        data_02 %>% filter(nivel == "Região de Integração") %>%
          filter(ri == input$localidade) %>%
          select(ano,valor)
      } else if(input$nvl == "Municipal"){
        data_02 %>% filter(nivel == "Municipal") %>%
          filter(muni == input$localidade) %>%
          select(ano,valor)
      }
a <- a %>% rename(valor1 = valor)  
b <- b %>% rename(valor2 = valor)
ab <- full_join(a,b,by = "ano")
ab %>% e_chart(x = ano) %>%
 e_line(
          serie = valor1,
          name = "Habilitados ao Ano",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T
        ) %>%
      e_line(
          y_index = 1,
          serie = valor2,
          name = "Total de Condutores",
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
          name = "Total de Condutores",
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
#            body = dashboardBody(fluidPage(quantidade_ui("total"))))
# 
# server <- function(input, output) {
#   quantidade_Server("total")
# }
# 
# shinyApp(ui, server)