dashboardPage(
    dashboardHeader(title = "Violencia IntraFamiliar"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Violencia en Estados",
                tabName = "dashboard",
                icon = icon("dashboard")
            )
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(
                    box(
                        leafletOutput("mapa_violencia"),
                        title = "Mapa de violencia en estados",
                        width = 6
                    ),
                    box(
                        leafletOutput("mapa_violencia_familiar"),
                        title = HTML("Mapa de violencia <b>IntraFamiliar</b> en estados"),
                        width = 6
                    )
                ),
                fluidRow(
                    box(
                        pickerInput(
                            inputId = "estado",
                            label = NULL,
                            choices = vector_estados,
                            options = list(
                                `actions-box` = TRUE,
                                size = 10,
                                `selected-text-format` = "count > 5"
                            ),
                            multiple = T,
                            selected = "Querétaro"
                        ),
                        title = "Estado seleccionado en Mapas:",
                        width = 12,
                        collapsible = TRUE
                    )
                ),
                fluidRow(
                    box(plotlyOutput("violencia_estatal"), title = "Violencia en estados", width = 6),
                    box(
                        plotlyOutput("violencia_estatal_familiar"),
                        title = HTML("Violencia <b>IntraFamiliar</b> en estados"),
                        width = 6
                    )
                ),
                fluidRow(
                    box(
                        fluidRow(
                            column(
                                width = 12,
                                selectInput(
                                    inputId = "estado_unico",
                                    label = NULL,
                                    choices = c("Todos", vector_estados),
                                    multiple = FALSE,
                                    selected = "Querétaro"
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 12,
                                plotlyOutput("violencia_familiar_mes_valido")
                            )
                        ),
                        title = HTML("Violencia <b>IntraFamiliar</b> para los meses con datos"),
                        width = 12
                    )
                )
            )
        )
    )
)
