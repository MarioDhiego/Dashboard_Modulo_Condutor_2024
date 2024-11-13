

# Interface do Usuário---------------------------------------------------------#
# Carregando
source('global.R')
shinyApp(
  ui = dashboardPage(
    tags$li(
      class = "dropdown",
      a(
        href = "https://www.facebook.com/detranPARA",
        class = "fa fa-facebook",
        target = "_blank"
      )
    ),
    
    header = dashboardHeader(
      border = TRUE,
      compact = TRUE,
      skin = "light",
      status = "white",
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("th"),
      # dashboardBrand
      title = dashboardBrand(
        title = tags$strong("CONDUTORES"),
        href = "https://www.detran.pa.gov.br/index_.php",
        image = "detran_pa.png",
        color = "primary"  # Removida a vírgula extra
      )
    ),
    #------------------------------------------------------------------------------#
    # dbHeader,
    skin = "black",
    scrollToTop = TRUE,
    options = list(sidebarExpandOnHover = TRUE),
    sidebar = dashboardSidebar(
      skin = "dark",
      minified = TRUE,
      collapsed = FALSE,
      sidebarMenu(
        menuItem(
          "ANUÁRIO",
          tabName = "anuario",
          icon = icon("address-card"),
          menuSubItem("Sobre Anuário", tabName = "sobre1", icon =
                        icon("book")),
          menuSubItem(
            "Vídeo Institucional",
            tabName = "video1",
            icon = icon("video")
          )
        ),
        menuItem("GLOSSÁRIO", tabName = "conceitos", icon = icon("book")),
        menuItem(
          "Perfil dos Condutores",
          tabName = "perfil",
          icon = icon("chart-bar"),
          menuSubItem("Quantidade", tabName = "quantidade"),
          menuSubItem("Sexo", tabName = "sexo"),
          menuSubItem("Faixa Etária", tabName = "faixa")
        ),
        menuItem(
          "Categoria e Atividade",
          tabName = "habilitacao",
          icon = icon("id-card"),
          menuSubItem("Categoria da Habilitação", tabName = "cat_hab"),
          menuSubItem("Tipo de Habilitação", tabName = "tipo_hab"),
          menuSubItem("Atividade Remunerada", tabName = "atividade")
        ),
        menuItem("Sobre", tabName = "sobre", icon = icon("info-circle"))
      )
    ),
    #------------------------------------------------------------------------------#
    controlbar = dashboardControlbar(skinSelector(), pinned = TRUE),
    #------------------------------------------------------------------------------#
    footer = dashboardFooter(
      left = tags$b("COPYRIGHT © DETRAN-PA - TODOS OS DIREITOS RESERVADOS"),
      right = tags$b("BELÉM-PA, 2024 v.1")
    ),
    #------------------------------------------------------------------------------#
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "sobre1",
          tabBox(
            id = "t1",
            width = 12,
            tabPanel(
              "ANUÁRIO WEB",
              icon = icon("address-card"),
              fluidRow(
                column(
                  width = 8,
                  position = "left",
                  tags$img(
                    id = "foto1",
                    src = "cnh1.jpg",
                    controls = "controls",
                    width = 650,
                    height = 450
                  ),
                  tags$br(),
                  tags$a("Photo by Asdecom"),
                  align = "left"
                ),
                column(
                  width = 4,
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "A Assembléia Geral da Organização das Nações Unidas(ONU), por meio da Resolução A/RES/74/299, de 31 de agosto de 2020, lançou a Década de Ação pela Segurança Viária 2021/2030, com objetivo de reduzir em pelo menos 50/% de vítimas fatais e feridos graves no trânsito, bem como solicita aos Estados-Membros que continuem adotando medidas até 2030 para alcançar as metas dos Objetivos de Desenvolvimento Sustentável relacionados a Segurança Viária, em especial a meta 3.6."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align: justify;font-si20pt",
                    strong(
                      "O DETRAN-PA, com sua missão fundamentada nos princípios da Política Nacional de Trânsito(Resolução do CONTRAN Nº514, de 18 de dezembro de 2014), apresenta, neste Painél, o Processo de Integração de Múltiplas Bases de Dados (Pareamento) sobre vítimas fatais por acidentes de trânsito."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align: justify;font-si20pt",
                    strong(
                      "O Processo de Integração de Dados foi Realizado junto aos 144 municípios que compõem o Estado do Pará. Como Resultados foi criado uma base de dados de frota registrada."
                    )
                  )
                )
              )
            ),
            tabPanel("LINHA DE BASE", icon = icon("hospital"), fluidRow(
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "A Definição de Mortes e VÍtimas Graves foi Estabelecida com Base no Padrão da Organização Mundial de São de (WHO, 2022)."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "1) VÍtima Fatal: uma pessoa morta imediatamente ou até 30 dias, como resultado do acidente de trânsito."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "2) VÍtima Grave: a pessoa hospitalizada por pelo menos 24 horas devido a ferimentos sofridos em acidentes de trânsito."
                  )
                )
              )
            )),
            
            tabPanel(
              "ALINHAMENTO TÉCNICO",
              icon = icon("layer-group"),
              fluidRow(
                column(
                  width = 4,
                  position = "center",
                  solidHeader = TRUE,
                  tags$br(),
                  tags$p(
                    style = "text-align: justify;font-si20pt",
                    strong(
                      "A Lei nº 13.614/2018 criou Plano Nacional de Redução de Mortes e Lesões no Trânsito(PNATRANS), acrescentando o artigo 326-A ao Código de Trânsito Brasileiro (CTB), e propôs um novo desafio para a gestão de trânsito no Brasil e para os orgãos integrados do Sistema Nacional de Trânsito(SNT)."
                    )
                  ),
                  tags$br(),
                  tags$a("Pnatrans", href = "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/anexo_i_pnatrans_2.pdf"),
                  tags$br(),
                  tags$p(
                    style = "text-align: justify;font-si20pt",
                    strong(
                      "Em 2020, foi realizado um diagnóstico, envolvendo todas as Unidades Federativas com o objetivo de estabelecer uma metodologia padronizada de coleta e tratamento dos dados sobre acidentes de trânsito e uma gestão baseada em análises e melhoria do sistema de segurança viária para todo Brasil. A fim de definir dados mínimos sobre acidentes de forma padronizada, alinhar as Unidades Federativas sobre o processo e sobre a implantação do modelo e, por fim, garantir análises e atuação sobre as causas críticas com intuito de minimizar os acidentes de trânsito no país."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align: justify;font-si20pt",
                    strong(
                      "Nesse Contexto, o Anuário Estatístico de Acidentes de Trânsito, será construído em Alinhamento ao Manual de Gestão do RENAEST (Resolução do CONTRAN Nº808/2020), utilizando metodologias factíveis com estatísticas de trânsito padronizada para todo o Território Nacional, e aos Objetivos de Desenvolvimento Sustentáveis (Resolução da ONU Nº70/2015)."
                    )
                  )
                )
              )
            ),
            tabPanel("SOFTWARE'S", icon = icon("computer"), fluidRow(
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Para Criação do Anuário em Formato Web com Dasboard Interativos, foi Desenvolvido um script em Linguagem de Programação R-PROJECT Versão 4.2.2, no formato de Projeto de Software Livre de Código Aberto (open source), ou seja, pode ser utilizado sem custos de licença (R DEVELOPMENT CORE TEAM, 2022)"
                  )
                ),
                tags$br(),
                tags$img(
                  id = "foto2",
                  src = "R.jpg",
                  controls = "controls",
                  width = 180,
                  height = 150
                ),
                tags$br(),
                tags$a("Software R", href = "https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe"),
                tags$br(),
              ),
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Foi utilizado um Ambiente de Desenvolvmento Integrado (IDE) Chamado Rstudio Versão 1.4.1.7, utilizando um Processo de Extração-Transformação-Carga(ETL) com uso de Várias bibliotecas (library), para o Ambiente Windows"
                  )
                ),
                tags$br(),
                tags$img(
                  id = "foto3",
                  src = "RStudio.png",
                  controls = "controls",
                  width = 180,
                  height = 150
                ),
                tags$br(),
                tags$a("RStudio", href = "https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe"),
                tags$br(),
              )
            )),
            tabPanel(
              "MATERIAL E MÉTODOS",
              icon = icon("book"),
              fluidRow(
                column(
                  width = 4,
                  position = "center",
                  tags$br(),
                  tags$br("Metodologia"),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "A Metodologia Adotada para o Planejamento e execução do Projeto foi apoiada na Estratégia de Proatividade e Parceria Desenvolvida pela GRSP (CARDITA e DI PIETRO, 2010)."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "A Estratégia de Proatividade e Parceria (EPP) consiste em um Modelo Desenvolvido para Tratar das questões de Segurança no Trânsito."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(" As Etapas a Serem Desenvolvidas Durante Aplicação do Projeto são:")
                  ),
                  tags$br(),
                  tags$p(style = "text-align:justify;font-si20pt", strong("1) Articulação Intersetorial e Formação")),
                  tags$p(style = "text-align: justify;font-si20pt", strong(
                    "2) Qualificação, Integração e Análise de Dados"
                  )),
                  tags$p(style = "text-align: justify;font-si20pt", strong(
                    "3) Ações Integradas de Segurança no Trânsito"
                  )),
                  tags$p(
                    style = "text-align: justify;font-si20pt",
                    strong("4) Monitoramento, Avaliação de Desenpenhp e Reconhecimento")
                  ),
                  tags$p(style = "text-align: justify;font-si20pt", strong("5) Revisão Geral Anual")),
                  tags$p(style = "text-align: justify;font-si20pt", strong("6) Renovação e Expansão"))
                ),
                tags$br(),
                column(
                  width = 4,
                  position = "center",
                  tags$br("Pareamento"),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "Para o Relacionamento das Múltiplas Bases de Dados(pareamento), utilizou-se o Método Probabilístico de Relacionamento de Registro desenvolvido por Fellegi e Sunter (1969)."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "A principal dificuldade do pareamento é a não existência de um identificador único que permita vincular um Boletim de Ocorrência à uma Autorização de Internação Hospitalar ou Declaração de Òbito."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "O Processo de Padronização de Variáveis, utilizando o Método Probabilístico, foi realizado para homogeneizar as variáveis das diferentes bases de dados, visando minimizar erros no processo de pareamento, com a alocação de registros da mesma vítima num bloco lógico para evitar: erros fonéticos, perda de informação, etc."
                    )
                  )
                )
              )
            ),
            tabPanel("CRÉDITOS", icon = icon("phone"), fluidRow(
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("DEPARTAMENTO DE TRÂNSITO DO ESTADO DO PARÁ - DETRAN/PA")
                ),
                tags$p(style = "text-align: justify;font-si20pt", strong("RENATA MIRELA COELHO")),
                tags$p(style = "text-align: justify;font-si20pt", strong("AVENIDA: AUGUSTO MONTENEGRO KM 03 S/N")),
                tags$p(style = "text-align: justify;font-si20pt", strong("CEP: 66635-918 - PARQUE VERDE - BELÉM - PARÁ")),
                tags$a("https://www.detran.pa.gov.br", href = "https://www.detran.pa.gov.br"),
                tags$br(),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Esta publicação deve ser citada como: Departamento de Trânsito do Estado do Pará (DETRAN-PA), Anuário Estatístico de Acidentes de Trânsito, 2023 (LC/PUB.2023/1-P), Belém, 2023."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "A autorização para a reprodução total ou parcial deste trabalho deve ser solicitada ao Departamento de Trânsito do Estado do Pará, Gerência de Análise e Estatística de Trânsito, gerest@detran.pa.gov.br. Os Estados membros das Nações Unidas e suas instituições governamentais podem reproduzir este trabalho sem autorização prévia. Solicita-se apenas que mencionem a fonte e informem ao DETRAN-PA de tal reprodução."
                  )
                ),
                tags$br(),
                
              ),
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                leafletOutput("mapa"),
              )
            )),
            tabPanel("SUGESTÕES", fluidRow(
              column(
                width = 4,
                position = "center",
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Reclamações, sugestões, críticas e elogios relacionados ao Anuário
Estatístico de Acidentes de Trânsito do DETRAN-PA podem ser registrados na Gerência de Análise Estatística de Trânsito, por intermédio do "
                  )
                ),
                tags$a("estatisticadetransito@detran.pa.gov.br", href = "gerest@detran.pa.gov.br"),
              )
            ))
          )
        ),
        
        tabItem(tabName = "video1", tabBox(
          id = "t2",
          width = 12,
          tabPanel(
            "Video Institucional",
            icon = icon("video"),
            fluidRow(
              column(
                width = 8,
                position = "center",
                tags$br("Projeto Strengthening Road Traffic Enforcement in Brazil"),
                tags$video(
                  id = "videoID",
                  type = "video/mp4",
                  src = "video_detran.mp4",
                  width = 750,
                  height = 500,
                  controls = "controls"
                ),
                tags$br() ,
                tags$a("Video: by Asdecom"),
                align = "left"
              ),
              column(
                width = 4,
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "O Departamento de Trânsito do Estado do Pará obteve o Projeto “Strengthening Road Traffic Enforcement
in Brazil” aprovado e financiado pela (United Road Safety Fund), com duração de 12 meses, se constituindo
o único selecionado do Brasil, que somado as propostas de alguns outros países, formam o conjunto de projetos
nacionais administrados pelo Fundo, coordenado e supervisionados por diversas Agências e Comissões
Regionais das Nações Unidas."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Concomitantemente, o Projeto Brasileiro é supervisionado pela Comissão Econômica das Nações
Unidas para América Latina e Caribe (CEPAL), coordenado e implementado pelo DETRAN-PA
em parceria com Conselho Estadual de Trânsito do Estado do Pará (CETRAN-PA), e tem como objetivo
contribuir para a redução de mortes e lesões no Trânsito através das atividades de Educação, Engenharia e
Fiscalização em nível Estadual."
                  )
                )
              )
            )
          )
        )),
        tabItem(
          tabName = "conceitos",
          tabBox(
            id = "t3",
            width = 12,
            tabPanel(
              "REQUISITO PRA TIRAR CNH",
              icon = icon("calendar"),
              fluidRow(
                column(
                  width = 4,
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "A CNH é a sigla de Carteira Nacional de Habilitação, documento obrigatório para conduzir um veículos automotores e elétricos no território nacional. Segundo o CTB - Lei nº9.503/1997, para poder ter a CNH são requisito obrigatórios (Artigo 140):"
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "1) Ser Penalmente Imputável, ou seja, alguém que responda pelos seus atos perante a Lei;"
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "2) Saber Ler e Escrever, ou seja, não depende de grau de escolaridade, basta ser alfabetizado;"
                    )
                  ),
                  tags$br(),
                  tags$p(style = "text-align:justify;font-si20pt", strong("3) Ter 18 anos Completo;")),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "4) Possuir Documento de Identificação: podendo ser qualquer documento de identificação emitido por Órgão Público que tenha fé pública (RG/ Carteira de Trabalho, Passaporte, ect.);"
                    )
                  ),
                ),
                column(
                  width = 4,
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$p(style = "text-align:justify;font-si20pt", strong(
                    "5) Possuir Cadastro de Pessoa Física (CPF);"
                  )),
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "6) Ter Cursado a Carga Horária e Ser Aprovado em todos os Teste do Processo de habilitação:"
                    )
                  ),
                  tags$br(),
                  tags$p(style = "text-align:justify;font-si20pt", strong("7) Fazer o Cadastro Biométrico em um DETRAN"))
                )
              )
            ) ,
            tabPanel("TIPOS DE CNH", icon = icon("calendar"), fluidRow(
              column(
                width =4,
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "Existem vários tipos de CNH em vigor no Brasil com destaque para os ultimos 4 modelos que estavam e/ou estão em vigor."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "1) Modelo Emitido de 30 de março de 2006 até dezembro de 2016",
                    tags$a("(Resolução CONTRAN Nº192/2006.)",
href = "http://www.sgc.goias.gov.br/upload/arquivos/2013-11/resolucao_192_06.pdf")
                  ),
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "2) Modelo Emitido de janeiro de 2017 até junho de 2019",
                    tags$a("(Resolução CONTRAN Nº598/2016.)",
href = "https://www.gov.br/infraestrutura/pt-br/assuntos/transito/conteudo-contran/resolucoes/resolucao59820162.pdf"),  
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "3) Modelo Emitido a partir de julho de 2019.",
                    tags$a("(Resolução CONTRAN Nº775/2019.)",
href = "https://www.gov.br/infraestrutura/pt-br/assuntos/transito/conteudo-contran/resolucoes/resolucao7752019.pdf"),
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "4) Modelo Emitido a partir de dezembro de 2021.",
                    tags$a("(Resolução CONTRAN Nº886/2021.)",
href = "https://www.gov.br/infraestrutura/pt-br/assuntos/transito/conteudo-contran/resolucoes/Resolucao8862021.pdf"),
                    
                    
                  )
                )
              )
            )),
            tabPanel("CATEGORIA DA CNH", icon = icon("person"), fluidRow(
              column(
                width = 4,
                solidHeader = TRUE,
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "Toda pessoa que conduza um veículo automotor, ou de outro tipo, incluindo os ciclos, ou que guie por uma via, cabeças de gado isoladas, rebanho, bando ou manadas, ou animais de tiro, carga ou sela. Sendo habilitados segundo CTB(Artigo 143) nas seguintes categorias;"
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "1) CATEGORIA A: Todos os veículos automotores e elétricos, de duas ou três rodas, com ou sem carro lateral."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "2) CATEGORIA B: Veículos automotores e elétricos, de quatro rodas cujo peso bruto total não exceda a três mil e quinhentos quilogramas e cuja lotação não exceda a 08 (oito) lugares, excluído o do motorista, contemplando a combinação de unidade acoplada, reboque, semi-reboque ou articulada, desde que atenda a lotação e capacidade de peso para a categoria."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "3) CATEGORIA C: Veículos automotores e elétricos, de quatro rodas cujo peso bruto total não exceda a três mil e quinhentos quilogramas e cuja lotação não exceda a 08 (oito) lugares, excluído o do motorista, contemplando a combinação de unidade acoplada, reboque, semi-reboque ou articulada, desde que atenda a lotação e capacidade de peso para a categoria."
                  )
                )
              ),
              column(
                width = 4,
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "4) CATEGORIA D: Veículos automotores e elétricos utilizados no transporte de passageiros, cuja lotação exceda a 08 (oito) lugares e, todos os veículos abrangidos nas categorias “B” e “C”."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "5) Categoria E: Combinação de veículos em que a unidade tratora se enquadre nas categorias B, C ou D e cuja unidade acoplada, reboque, semirreboque, trailer ou articulada tenha 6.000 kg (seis mil quilogramas) ou mais de peso bruto total, ou cuja lotação exceda a 8 (oito) lugares e, todos os veículos abrangidos pelas categorias “B”, “C” e “D”."
                  )
                )
              )
            )),
            tabPanel("REGRAS DE MANTER A CNH", icon = icon("car"), fluidRow(
              column(
                width = 4,
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong("As regras para manter a Carteira Nacional de Habilitação (CNH).")
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "1) Não Ultrapassar o Limite de Pontos (20/30/40) Pontos Lei Nº 14.071(2020)"
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong("2) Não Tomar Infração Auto Suspensivas (2 a 8 meses)")
                ),
                tags$br(),
                tags$p(style = "text-align:justify;font-si20pt", strong("2.1) Andar de Motocicleta Sem Capacetes;")),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "2.2) Recusar a Fazer o Teste do Etilômetro ou qualquer outro exame Clínico"
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong("2.3) Dirigir acima de 50% da Velocidade Permitida")
                ),
                tags$br(),
              ),
            )),
            tabPanel("RENOVAÇÃO DA CNH", icon = icon("car"), fluidRow(
              column(
                width = 4,
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "Renovação da CNH consiste no processo de solicitação de uma nova carteira nacional de habilitação para os motoristas do estado de moradia do condutor;"
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "1) O procedimento de Renovação deve ser realizado obrigatoriamente, quando a CNH:
Estiver perto de VenceR"
                  )
                ),
                tags$br(),
                tags$p(style = "text-align:justify;font-si20pt", strong("2) Estiver Vencida a mais de 30 dias;")),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "3) Quando Precisa Incluir novas informações no Campo Observação tais como exemplo(Hoje no Prontuário)"
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si20pt",
                  strong(
                    "4) Quando Precisa Incluir os Cursos Especializados e Obrigatórias conforme a Resolução Nº 789/2020"
                  )
                ),
                tags$br()
              ),
            )),

tabPanel("PRAZO DA CNH", icon = icon("car"), 
fluidRow(
  column(
    width = 4,
    tags$br(),
    tags$p(
      style = "text-align:justify;font-si20pt",
      strong(
"Com a alteração no Código de Trânsito Brasileiro (CTB) a Lei 14.071/2020, 
o exame de aptidão física e mental deve ser feito agora de acordo com os seguintes prazos:"
      )
    ),
    tags$br(),
    tags$p(
      style = "text-align:justify;font-si20pt",
strong("1) -   A cada 10 anos, para condutores com Idade de 18 a 50 anos;"
      )
    ),
    tags$br(),
    tags$p(style = "text-align:justify;font-si20pt", 
strong("2) -   A cada 5 anos, para condutores com Idade de 51 a 70 anos;")),
    tags$br(),
    tags$p(
      style = "text-align:justify;font-si20pt",
      strong(
        "3) -   A cada 3 anos, para condutores com Idade de 71 anos em diante."
      )
    ),
    tags$br(),
    tags$p(
      style = "text-align:justify;font-si20pt",
      strong(
        "4) Quando Precisa Incluir os Cursos Especializados e Obrigatórias conforme a Resolução Nº 789/2020"
      ),
      tags$p(
        style = "text-align:justify;font-si20pt",
        strong(
          "Esses são os prazos padrões estabelecidos pelo CTB, contudo o médico pode Reduzir a Validade do Exame Médico, de acordo com as condições físicas e mentais do motorista."
        )
      )
    )
  ),
))
          )
        ),
        tabItem(tabName = "quantidade", fluidPage(quantidade_ui("quantidade"))),
        tabItem(tabName = "sexo", fluidPage(sexo_ui("sexo"))),
        tabItem(tabName = "faixa", fluidPage(faixa_idade_ui("faixa_idade"))),
        tabItem(tabName = "cat_hab", fluidPage(categoria_ui("categoria"))),
        tabItem(tabName = "tipo_hab", fluidPage(tipo_hab_ui("tipo_hab"))),
        tabItem(tabName = "atividade", fluidPage(remu_ui("remu"))),
        # Corrigido `fluidPage` duplicado
        tabItem(tabName = "sobre", fluidPage(
          panel(
            title = "Sobre R, ShinyR e bs4dash",
            width = 6,
            # Ajuste a largura conforme necessário
            status = "primary",
            solidHeader = TRUE,
            align = "center",
            justify = "justify",
            fluidRow(column(
              4,
              div(
                style = "text-align: left; text-justify: inter-word;",
                # Estilo CSS para centralizar e justificar o texto
                HTML(
                  "
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
             para a criação de aplicações web estatísticas avançadas e dashboards envolventes.</p>"
                )
              )
            ), column(8, align = "center", img(
              src = "", height = 350
            ))),
            footer = fluidRow(
              column(4, align = "center", tags$img(src = "R_logo.svg.png", height = 100)),
              column(4, align = "center", tags$img(src = "hex-shiny.png", height = 100)),
              column(4, align = "center", tags$img(src = "bs4dash.png", height = 100))
            )
          )
        ))
      )
    ),
    
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
