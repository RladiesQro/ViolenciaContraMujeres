shinyServer(function(input, output) {
    output$violencia_estatal <- renderPlotly({
        violencia_anual_estados <- violencia_anual %>%
            filter(Entidad %in% input$estado) %>%
            mutate(labels = glue(
                "Estado: {Entidad} <br>",
                "Casos promedios al mes: {comma(casos_promedio_mes, accuracy = 1)}<br>",
                "Casos por cada 100 mil habitantes {comma(tasa_100k, accuracy = 1)}<br>",
                "Población Estado (Inegi-2015): {comma(poblacion_total, accuracy = 1)}"
            ))
        plot_violencia_estados <- ggplot(violencia_anual_estados) +
            geom_line(aes(x = anyo, y = tasa_100k, color = Entidad)) +
            geom_point(aes(x = anyo, y = tasa_100k, color = Entidad, text = labels)) +
            theme_minimal()
        ggplotly(plot_violencia_estados, tooltip = c("text")) %>%
            config(
                displaylogo = FALSE,
                modeBarButtonsToRemove = list(
                    "lasso2d", "toImage", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian",
                    "select2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "resetScale2d", "zoom2d"
                )
            )
    })

    output$violencia_estatal_familiar <- renderPlotly({
        violencia_familiar_anual_estados <- violencia_familiar_anual %>%
            filter(Entidad %in% input$estado) %>%
            mutate(labels = glue(
                "Estado: {Entidad} <br>",
                "Casos promedios al mes: {comma(casos_promedio_mes, accuracy = 1)}<br>",
                "Casos por cada 100 mil habitantes {comma(tasa_100k, accuracy = 1)}<br>",
                "Población Estado (Inegi-2015): {comma(poblacion_total, accuracy = 1)}"
            ))
        plot_violencia_familiar <- ggplot(violencia_familiar_anual_estados) +
            geom_line(aes(x = anyo, y = tasa_100k, color = Entidad)) +
            geom_point(aes(x = anyo, y = tasa_100k, color = Entidad, text = labels)) +
            theme_minimal()
        ggplotly(plot_violencia_familiar, tooltip = c("text")) %>%
            config(
                displaylogo = FALSE,
                modeBarButtonsToRemove = list(
                    "lasso2d", "toImage", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian",
                    "select2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "resetScale2d", "zoom2d"
                )
            )
    })

    output$mapa_violencia <- renderLeaflet({
        datos_mapa_violencia <- CasosNormalizadosRepublica(datos_violencia, poblacion_inegi_2015) %>%
            AgregaPoligonos(., poligonos_mx) %>%
            mutate(labels = glue(
                "Estado: {Entidad} <br>",
                "Casos reportados: {comma(casos_por_estado, accuracy = 1)}<br>",
                "Casos por cada 100 mil habitantes {comma(tasa_100k, accuracy = 1)}<br>",
                "Población Estado (Inegi-2015): {comma(poblacion_total, accuracy = 1)}"
            ))

        pal <- CreaPaletaPersonalizada(
            vector_usar = datos_mapa_violencia$casos_normalizados, vector_colores = c("#E6E6EA", "#823038")
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
        datos_mapa_violencia <- CasosNormalizadosRepublica(datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar") %>%
            AgregaPoligonos(., poligonos_mx) %>%
            mutate(labels = glue(
                "Estado: {Entidad} <br>",
                "Casos reportados: {comma(casos_por_estado, accuracy = 1)}<br>",
                "Casos por cada 100 mil habitantes {comma(tasa_100k, accuracy = )}<br>",
                "Población Estado (Inegi-2015): {comma(poblacion_total, accuracy = 1)}"
            ))

        pal <- CreaPaletaPersonalizada(
            vector_usar = datos_mapa_violencia$casos_normalizados, vector_colores = c("#E6E6EA", "#823038")
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
