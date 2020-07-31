dashboardPage(
    dashboardHeader(title = "Violencia Contra la Mujer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Violencia en Estados",
                tabName = "violencia_estado",
                icon = icon("dashboard")
            ),
            menuItem(
                text = "Violencia en el Tiempo",
                tabName = "violencia_tiempo",
                icon = icon("dashboard")
            ),
            menuItem(
                text = "Violencia Familiar",
                tabName = "violencia_familiar_tendencia",
                icon = icon("dashboard")
            )
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "violencia_estado",
                fluidRow(
                    box(
                        selectInput(
                            inputId = "estado",
                            label = NULL,
                            choices = rbind("Todos", vector_estados),
                            multiple = F,
                            selected = "Querétaro"
                        ),
                        title = "Estado seleccionado en mapas:",
                        width = 12,
                        collapsible = TRUE
                    )
                ),
                fluidRow(
                    box(
                        leafletOutput("mapa_violencia"),
                        title = "Mapa de violencia contra la mujer por estados",
                        width = 6
                    ),
                    box(
                        leafletOutput("mapa_violencia_familiar"),
                        title = HTML("Mapa de violencia <b>Familiar</b> contra la mujer en estados"),
                        width = 6
                    )
                ),

                fluidRow(
                    box(plotlyOutput("violencia_estatal"), title = "Violencia contra la mujer en estados", width = 6),
                    box(
                        plotlyOutput("violencia_estatal_familiar"),
                        title = HTML("Violencia <b>Familiar</b> contra la mujer en estados"),
                        width = 6
                    )
                ),
                fluidRow(
                    box(
                        fluidRow(
                            column(
                                width = 12,
                                plotlyOutput("violencia_familiar_mes_valido")
                            )
                        ),
                        title = HTML("Violencia <b>Familiar</b> contra la mujer para los meses con datos"),
                        width = 12
                    )
                )
            ),
            tabItem(
                tabName = "violencia_tiempo",
                fluidRow(
                    box(
                        selectInput(
                            inputId = "tipo",
                            label = NULL,
                            choices = vector_tipo_violencia,
                            multiple = F,
                            selected = "Violencia familiar"
                        ),
                        title = "Tipo de Violencia contra la Mujer:",
                        width = 6,
                        collapsible = TRUE
                    ),
                    box(
                        selectInput(
                            inputId = "estado_tiempo",
                            label = NULL,
                            choices = vector_estados,
                            multiple = F,
                            selected = "Querétaro"
                        ),
                        title = "Estado seleccionado:",
                        width = 6,
                        collapsible = TRUE
                    )
                ),
                fluidRow(
                    box(
                        plotOutput("rank_mexico"),
                        width = 6
                    ),
                    box(
                        plotOutput("rank_estado"),
                        width = 6
                    )
                ),
                fluidRow(
                    box(
                        plotOutput("anyos_mexico"),
                        width = 6
                    ),
                    box(
                        plotOutput("anyos_estado"),
                        width = 6
                    )
                )
            ),
            tabItem(
                tabName = "violencia_familiar_tendencia",
                fluidRow(
                    box(
                        selectInput(
                            inputId = "estado_vfamiliar",
                            label = NULL,
                            choices = vector_estados,
                            multiple = F,
                            selected = "Querétaro"
                        ),
                        title = "Estado seleccionado:",
                        width = 12,
                        collapsible = TRUE
                    )
                ),
                fluidRow(
                    box(
                        plotOutput("rank_violencia_mexico"),
                        width = 6
                    ),
                    box(
                        plotOutput("rank_violencia_estado"),
                        width = 6
                    )
                )
            )
        )
    )
)
