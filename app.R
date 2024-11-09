# Interface do usuário---------------------------------------
#Carregando----
source('global.R')
shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      title = dashboardBrand(
        title = tags$strong("Detran - PA"),
        href = "https://www.detran.pa.gov.br/index_.php",
        image = "detran_pa.png",
        color = "gray"  # Removida a vírgula extra
      )
    ), 
    # dbHeader,
    skin = "black",
    scrollToTop = T,
    options = list(sidebarExpandOnHover = T),
    sidebar = dashboardSidebar(
      skin = "dark",
      minified = T,
      collapsed = F,
      sidebarMenu(
        menuItem("Perfil dos Condutores", tabName = "perfil", icon = icon("chart-bar"),
                 menuSubItem("Quantidade", tabName = "quantidade"),
                 menuSubItem("Sexo", tabName = "sexo"),
                 menuSubItem("Faixa Etária", tabName = "faixa")
        ),
        menuItem("Categoria e Atividade", tabName = "habilitacao", icon = icon("id-card"),
                 menuSubItem("Categoria da Habilitação", tabName = "cat_hab"),
                 menuSubItem("Tipo de Habilitação", tabName = "tipo_hab"),
                 menuSubItem("Atividade Remunerada", tabName = "atividade")
        ),
        menuItem("Sobre", tabName = "sobre", icon = icon("info-circle"))
      )      
    ),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "quantidade", fluidPage(quantidade_ui("quantidade"))),
        tabItem(tabName = "sexo", fluidPage(sexo_ui("sexo"))),
        tabItem(tabName = "faixa", fluidPage(faixa_idade_ui("faixa_idade"))),
        tabItem(tabName = "cat_hab", fluidPage(categoria_ui("categoria"))),
        tabItem(tabName = "tipo_hab", fluidPage(tipo_hab_ui("tipo_hab"))),
        tabItem(tabName = "atividade", fluidPage(remu_ui("remu"))),  # Corrigido `fluidPage` duplicado
        tabItem(tabName = "sobre", fluidPage(
          panel(
            title = "Sobre R, ShinyR e bs4dash",
            width = 6,  # Ajuste a largura conforme necessário
            status = "primary",
            solidHeader = TRUE,
            align = "center",
            justify = "justify",
            fluidRow(
              column(4,
                     div(
                       style = "text-align: left; text-justify: inter-word;",  # Estilo CSS para centralizar e justificar o texto
                       HTML("
                <h2>Sobre as tecnologias usadas</h2>
                <p>R é uma linguagem de programação estatística amplamente utilizada. 
             ShinyR é um pacote para criar aplicativos web interativos usando R. 
             bs4dash é uma extensão do shinydashboard que utiliza o Bootstrap 4. 
             Ele fornece uma variedade de componentes e funcionalidades para criar dashboards modernos e responsivos em R. 
             O pacote é particularmente útil para a criação de aplicações web interativas e visualmente atraentes, 
             aproveitando as capacidades do Bootstrap 4 para o design de interface do usuário. 
             Além disso, a integração com o Shiny facilita a construção de dashboards dinâmicos e interativos 
             que respondem às interações do usuário em tempo real. 
             Ao combinar R, ShinyR e bs4dash, os desenvolvedores têm à disposição uma poderosa combinação 
             para a criação de aplicações web estatísticas avançadas e dashboards envolventes.</p>")
                     )),
              column(8, align = "center", img(src = "", height = 350))
              
            ),
            footer = fluidRow(
              column(4, align = "center", tags$img(src = "R_logo.svg.png", height = 100)),
              column(4, align = "center", tags$img(src = "hex-shiny.png", height = 100)),
              column(4, align = "center", tags$img(src = "bs4dash.png", height = 100))
            )
          )
        ))
      )
    ),
    footer = dashboardFooter(
      left = tags$b("Detran-PA"), 
      right = "Belém-PA, 2024 v.1"
    ),
    title = "Dashboard Detran-PA"
  ),
  server = function(input, output, session) {
    quantidade_Server("quantidade")
    sexo_Server("sexo")
    faixa_idade_Server("faixa_idade")
    categoria_Server("categoria")
    tipo_hab_Server("tipo_hab")
    remu_Server("remu")
    
  }
)
