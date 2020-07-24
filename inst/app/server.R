shinyServer(function(input, output) {

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
            scale_color_viridis_d() +
            labs(x = "", y = "Casos por cada 100 mil habitantes", color = "") +
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
            scale_color_viridis_d() +
            labs(x = "", y = "Casos por cada 100 mil habitantes", color = "") +
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

    output$violencia_familiar_mes_valido <- renderPlotly({
        if(input$estado_unico == "Todos") {
            violencia_meses_con_datos <- violencia_meses_con_datos %>%
                group_by(fecha) %>%
                summarise(poblacion_total = sum(poblacion_total), casos_por_mes = sum(casos_por_mes)) %>%
                mutate(Entidad = "Toda la Republica", tasa_100k = (casos_por_mes / poblacion_total) * 100000)
        } else {
            violencia_meses_con_datos <- violencia_meses_con_datos %>%
                filter(Entidad %in% input$estado_unico)
        }
        violencia_meses_con_datos <- violencia_meses_con_datos%>%
            mutate(labels = glue(
                "Estado: {Entidad} <br>",
                "Fecha: {str_to_title(format(fecha, format = '%b %Y'))}<br>",
                "Casos promedios al mes: {comma(casos_por_mes, accuracy = 1)}<br>",
                "Casos por cada 100 mil habitantes: {comma(tasa_100k, accuracy = 1)}<br>",
                "Población Estado (Inegi-2015): {comma(poblacion_total, accuracy = 1)}"
            )) %>%
            mutate(
                anyo = factor(year(fecha)),
                mes = factor(
                    x = str_to_title(month(fecha, label = T)),
                    levels = str_to_title(month(sort(fecha), label = T)),
                    labels = str_to_title(month(sort(fecha), label = T)),
                    ordered = TRUE
                )
            )

        plot_violencia_mes_datos <- ggplot(violencia_meses_con_datos, aes(x = mes, y = tasa_100k, fill = anyo, text = labels)) +
            geom_col(position = "dodge") +
            geom_text(aes(label = anyo, y = tasa_100k - (tasa_100k * 0.2) ), position = position_dodge(0.9), color = "gray") +
            scale_fill_viridis_d() +
            labs(x = "", y = "Casos por cada 100 mil habitantes") +
            theme_minimal() +
            theme(legend.position = "none")
        ggplotly(plot_violencia_mes_datos, tooltip = c("text")) %>%
            config(
                displaylogo = FALSE,
                modeBarButtonsToRemove = list(
                    "lasso2d", "toImage", "toggleSpikelines", "hoverClosestCartesian", "hoverCompareCartesian",
                    "select2d", "zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d", "resetScale2d", "zoom2d"
                )
            )
    })

})
