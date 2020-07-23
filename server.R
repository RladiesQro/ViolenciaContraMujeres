shinyServer(function(input, output) {
    output$violencia_estatal <- renderPlotly({
        violencia_anual_estados <- datos_violencia %>%
            group_by(Entidad, anyo = year(fecha)) %>%
            summarise(frecuencia_casos = sum(ocurrencia)) %>%
            filter(Entidad %in% input$estado)
        plot_violencia_estados <- ggplot(violencia_anual_estados) +
            geom_line(aes(x = anyo, y = frecuencia_casos, color = Entidad)) +
            geom_point(aes(x = anyo, y = frecuencia_casos, color = Entidad)) +
            theme_minimal()
        ggplotly(plot_violencia_estados) %>%
            config(
                displaylogo = FALSE,
                modeBarButtonsToRemove = list(
                    "lasso2d", "toImage", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian",
                    "select2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "resetScale2d", "zoom2d"
                )
            )
    })

    output$violencia_estatal_familiar <- renderPlotly({
        violencia_familiar_anual_estados <- datos_violencia %>%
            group_by(Entidad, Tipo, anyo = year(fecha)) %>%
            summarise(frecuencia_casos = sum(ocurrencia)) %>%
            filter(Entidad %in% input$estado, Tipo == "Violencia familiar")
        plot_violencia_familiar <- ggplot(violencia_familiar_anual_estados) +
            geom_line(aes(x = anyo, y = frecuencia_casos, color = Entidad)) +
            geom_point(aes(x = anyo, y = frecuencia_casos, color = Entidad)) +
            theme_minimal()
        ggplotly(plot_violencia_familiar) %>%
            config(
                displaylogo = FALSE,
                modeBarButtonsToRemove = list(
                    "lasso2d", "toImage", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian",
                    "select2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "resetScale2d", "zoom2d"
                )
            )
    })

    output$mapa_violencia <- renderLeaflet({
        datos_mapa_violencia <- CasosNormalizadosRepublica(datos_violencia) %>%
            inner_join({
                datos_violencia %>%
                    select(Entidad, geometry) %>%
                    distinct(Entidad, .keep_all = T)
            }, by = "Entidad") %>%
            mutate(labels = glue("Estado: {Entidad} <br> Casos reportados: {casos_por_estado}"))

        pal <- CreaPaletaPersonalizada(
            vector_usar = datos_mapa_violencia$casos_normalizados, vector_colores = c("#DE3303", "#F6CB20","#71B813")
        )
        leaflet(data = st_as_sf(datos_mapa_violencia), options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
            addPolygons(
                fillColor = ~ pal(casos_normalizados),
                color = "#FFFFFF",
                opacity = 0.7,
                fillOpacity = 0.4,
                weight = 2,
                highlight = highlightOptions(
                    weight = 4,
                    color = "#666",
                    fillOpacity = 0.5,
                    bringToFront = TRUE
                ),
                label = ~ map(labels, HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
            )
    })

    output$mapa_violencia_familiar <- renderLeaflet({
        datos_mapa_violencia <- CasosNormalizadosRepublica(datos_violencia, filtro.tipo = "Violencia familiar") %>%
            inner_join({
                datos_violencia %>%
                    select(Entidad, geometry) %>%
                    distinct(Entidad, .keep_all = T)
            }, by = "Entidad") %>%
            mutate(labels = glue("Estado: {Entidad} <br> Casos reportados: {casos_por_estado}"))

        pal <- CreaPaletaPersonalizada(
            vector_usar = datos_mapa_violencia$casos_normalizados, vector_colores = c("#DE3303", "#F6CB20","#71B813")
        )
        leaflet(data = st_as_sf(datos_mapa_violencia), options = leafletOptions(minZoom = 4, maxZoom = 7)) %>%
            addPolygons(
                fillColor = ~ pal(casos_normalizados),
                color = "#FFFFFF",
                opacity = 0.7,
                fillOpacity = 0.4,
                weight = 2,
                highlight = highlightOptions(
                    weight = 4,
                    color = "#666",
                    fillOpacity = 0.5,
                    bringToFront = TRUE
                ),
                label = ~ map(labels, HTML),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
            )
    })

})
