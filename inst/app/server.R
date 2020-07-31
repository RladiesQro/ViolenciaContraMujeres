shinyServer(function(input, output, session) {

    output$mapa_violencia <- renderLeaflet({
        datos_mapa_violencia <- CasosNormalizadosRepublica(datos_violencia, poblacion_inegi_2015) %>%
            AgregaPoligonos(., {
                poligonos_mx %>%
                    mutate(nombre = ifelse(nombre == "Distrito Federal", "Ciudad de México", as.character(nombre)))
            }) %>%
            mutate(labels = glue(
                "Estado: {Entidad} <br>",
                "Casos reportados: {comma(casos_por_estado, accuracy = 1)}<br>",
                "Casos por cada 100 mil habitantes {comma(tasa_100k, accuracy = 1)}<br>",
                "Población Estado (Inegi-2015): {comma(poblacion_total, accuracy = 1)}"
            ))

        pal <- CreaPaletaPersonalizada(
            vector_usar = datos_mapa_violencia$casos_normalizados, vector_colores = c("#E6E6EA", "#823038")
        )
        leaflet(data = st_as_sf(datos_mapa_violencia), options = leafletOptions(minZoom = 4, maxZoom = 7, zoomControl = FALSE)) %>%
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
        datos_mapa_violencia <- CasosNormalizadosRepublica(
            datos_violencia, poblacion_inegi_2015, filtro.tipo = "Violencia familiar"
        ) %>%
            AgregaPoligonos(., {
                poligonos_mx %>%
                    mutate(nombre = ifelse(nombre == "Distrito Federal", "Ciudad de México", as.character(nombre)))
            }) %>%
            mutate(labels = glue(
                "Estado: {Entidad} <br>",
                "Casos reportados: {comma(casos_por_estado, accuracy = 1)}<br>",
                "Casos por cada 100 mil habitantes {comma(tasa_100k, accuracy = )}<br>",
                "Población Estado (Inegi-2015): {comma(poblacion_total, accuracy = 1)}"
            ))

        pal <- CreaPaletaPersonalizada(
            vector_usar = datos_mapa_violencia$casos_normalizados, vector_colores = c("#E6E6EA", "#823038")
        )
        leaflet(data = st_as_sf(datos_mapa_violencia), options = leafletOptions(minZoom = 4, maxZoom = 7, zoomControl = FALSE)) %>%
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

    observeEvent(input$mapa_violencia_familiar_shape_click, {
            map_click <- input$mapa_violencia_familiar_shape_click
            index_estado <- st_intersects(st_point(c(map_click$lng, map_click$lat)), poligonos_mx)
            estado_mapa <- poligonos_mx %>%
                mutate(nombre = ifelse(nombre == "Distrito Federal", "Ciudad de México", as.character(nombre))) %>%
                slice(as.numeric(index_estado)) %>%
                select(nombre) %>%
                st_drop_geometry() %>%
                pull() %>%
                as.character()
            updateVarSelectInput(session, inputId = "estado", selected = estado_mapa)
    })

    observeEvent(input$mapa_violencia_shape_click, {
        map_click <- input$mapa_violencia_shape_click
        index_estado <- st_intersects(st_point(c(map_click$lng, map_click$lat)), poligonos_mx)
        estado_mapa <- poligonos_mx %>%
            mutate(nombre = ifelse(nombre == "Distrito Federal", "Ciudad de México", as.character(nombre))) %>%
            slice(as.numeric(index_estado)) %>%
            select(nombre) %>%
            st_drop_geometry() %>%
            pull() %>%
            as.character()
        updateVarSelectInput(session, inputId = "estado", selected = estado_mapa)
    })

    output$violencia_estatal <- renderPlotly({
        violencia_anual_estados <- TasaPromedioMensual(datos_violencia, poblacion_inegi_2015, entidad = input$estado) %>%
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
            theme_minimal() +
            theme(legend.position = "none")
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
        violencia_familiar_anual_estados <- TasaPromedioMensual(datos_violencia, poblacion_inegi_2015, entidad = input$estado, filtro.tipo = "Violencia familiar")%>%
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
            theme_minimal() +
            theme(legend.position = "none")
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
        if(input$estado == "Todos") {
            violencia_meses_con_datos <- violencia_meses_con_datos %>%
                group_by(fecha) %>%
                summarise(poblacion_total = sum(poblacion_total), casos_por_mes = sum(casos_por_mes)) %>%
                mutate(Entidad = "Toda la Republica", tasa_100k = (casos_por_mes / poblacion_total) * 100000)
        } else {
            violencia_meses_con_datos <- violencia_meses_con_datos %>%
                filter(Entidad %in% input$estado)
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
            geom_text(
                aes(label = anyo, y = tasa_100k - (tasa_100k * 0.2)),
                position = position_dodge(0.9),
                color = "#5B6366",
                size = 4
            ) +
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

    violencia_mes_todo <-  reactive({
        DatosMesEstadoAgrupados(
            datos_violencia, entidad = "Todas", resaltar.tipo = input$tipo
        )
    })

    output$rank_mexico <- renderPlot({
        ggplot(violencia_mes_todo()) +
            geom_col(aes(x= mes, y = proporcion_tipo, fill = Tipo)) +
            facet_grid(. ~ anyo) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_viridis_d(direction = -1) +
            labs(
                title = "Violencia Familiar contra la Mujer Meses comparables",
                subtitle = "Porcentaje de del total de casos reportados por mes y año en todo México",
                x = "",
                y = "Porcentaje del total de casos",
                fill = ""
            ) +
            theme_ipsum() +
            theme(
                axis.title.y = element_text(size = 18),
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 12, angle = 90),
                legend.text = element_text(size = 15),
                strip.text.x = element_text(size = 14),
                legend.position="bottom"
            )
    })

    output$anyos_mexico <- renderPlot({
        violencia_mes_todo() %>%
            filter(Tipo == input$tipo) %>%
            ggplot() +
            geom_line(aes(x= mes, y = proporcion_tipo, color = factor(anyo), group = factor(anyo)), size = 1) +
            geom_point(aes(x= mes, y = proporcion_tipo, color = factor(anyo), group = factor(anyo)), size = 3) +
            scale_y_continuous(labels = scales::percent) +
            labs(
                title = "Porcentaje Violencia Familiar contra la Mujer",
                subtitle = "Comparativa meses con datos en todo México",
                x = "",
                y = "Porcentaje del total de casos",
                color = ""
            ) +
            scale_color_viridis_d(direction = -1) +
            theme_ipsum() +
            theme(
                axis.title.y = element_text(size = 18),
                axis.text.y = element_text(size = 11),
                axis.text.x = element_text(size = 11),
                legend.text = element_text(size = 14),
                strip.text.y = element_text(size = 18)
            )
    })


    violencia_mes_entidad <-  reactive({
        DatosMesEstadoAgrupados(
            datos_violencia, entidad = input$estado_tiempo, resaltar.tipo = input$tipo
        )
    })

    output$rank_estado <- renderPlot({
        ggplot(violencia_mes_entidad()) +
            geom_col(aes(x= mes, y = proporcion_tipo, fill = Tipo)) +
            facet_grid(. ~ anyo) +
            scale_y_continuous(labels = scales::percent) +
            scale_fill_viridis_d(direction = -1) +
            labs(
                title = "Violencia Familiar contra la Mujer Meses comparables",
                subtitle = "Porcentaje de del total de casos reportados por mes y año en Querétaro",
                x = "",
                y = "Porcentaje del total de casos",
                fill = ""
            ) +
            theme_ipsum() +
            theme(
                axis.title.y = element_text(size = 18),
                axis.text.y = element_text(size = 12),
                axis.text.x = element_text(size = 12, angle = 90),
                legend.text = element_text(size = 15),
                strip.text.x = element_text(size = 14),
                legend.position="bottom"
            )
    })

    output$anyos_estado <- renderPlot({
        violencia_mes_entidad() %>%
            filter(Tipo == input$tipo) %>%
            ggplot() +
            geom_line(aes(x= mes, y = proporcion_tipo, color = factor(anyo), group = factor(anyo)), size = 1) +
            geom_point(aes(x= mes, y = proporcion_tipo, color = factor(anyo), group = factor(anyo)), size = 3) +
            scale_y_continuous(labels = scales::percent) +
            labs(
                title = "Porcentaje Violencia Familiar contra la Mujer",
                subtitle = "Comparativa meses con datos en Querétaro",
                x = "",
                y = "Porcentaje del total de casos",
                color = ""
            ) +
            scale_color_viridis_d(direction = -1) +
            theme_ipsum() +
            theme(
                axis.title.y = element_text(size = 18),
                axis.text.y = element_text(size = 11),
                axis.text.x = element_text(size = 11),
                legend.text = element_text(size = 14),
                strip.text.y = element_text(size = 18)
            )
    })

    output$rank_violencia_mexico <- renderPlot({
        violencia_familiar_todo <- RankingTipoViolencia(datos_violencia, entidad = NULL, numero_posiciones = 5)
        violencia_tags_inicio <- violencia_familiar_todo$violencia_tags_inicio
        violencia_tags_final <- violencia_familiar_todo$violencia_tags_final
        colores <- violencia_familiar_todo$colores
        violencia_familiar_todo <- violencia_familiar_todo$ranking_violencia
        ggplot(data = violencia_familiar_todo, aes(anyo, rank, color = Tipo, group = Tipo, label = Tipo)) +
            geom_line(size = 3, alpha = 0.5) +
            geom_point(size = 6, alpha = 0.5) +
            geom_point(size = 3, color = "white") +
            geom_text(data = violencia_tags_inicio, x = 2013.5, size = 5) +
            geom_text(data = violencia_tags_final, x = 2021.5, size = 5) +
            scale_y_reverse(breaks = 1:5) +
            scale_x_continuous(
                breaks = seq(min(ranking_tipo_violencia_republica$anyo), max(ranking_tipo_violencia_republica$anyo)),
                limits = c(min(ranking_tipo_violencia_republica$anyo) - 2.3, max(ranking_tipo_violencia_republica$anyo) + 2)
            ) +
            scale_color_manual(values = colores) +
            theme_ipsum() +
            theme(
                axis.title.y = element_text(size = 20),
                axis.text.y = element_text(size = 14),
                axis.text.x = element_text(size = 14),
                legend.position = "",
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_line(color = 'grey60', linetype = 'dashed')) +
            labs(
                title = "Clasificación Violencia Familiar contra la Mujer",
                subtitle = "Top 5 tipos de violencia en todo México",
                x = "Año",
                y = "Rank"
            )
    })

    output$rank_violencia_estado <- renderPlot({
        violencia_familiar <- RankingTipoViolencia(datos_violencia, entidad = input$estado_vfamiliar, numero_posiciones = 5)
        violencia_tags_inicio <- violencia_familiar$violencia_tags_inicio
        violencia_tags_final <- violencia_familiar$violencia_tags_final
        colores <- violencia_familiar$colores
        violencia_familiar <- violencia_familiar$ranking_violencia
        ggplot(data = violencia_familiar, aes(anyo, rank, color = Tipo, group = Tipo, label = Tipo)) +
            geom_line(size = 3, alpha = 0.5) +
            geom_point(size = 6, alpha = 0.5) +
            geom_point(size = 3, color = "white") +
            geom_text(data = violencia_tags_inicio, x = 2013.5, size = 5) +
            geom_text(data = violencia_tags_final, x = 2021.3, size = 5) +
            scale_y_reverse(breaks = 1:5) +
            scale_x_continuous(
                breaks = seq(min(ranking_tipo_violencia$anyo), max(ranking_tipo_violencia$anyo)),
                limits = c(min(ranking_tipo_violencia$anyo) - 2.3, max(ranking_tipo_violencia$anyo) + 2)
            ) +
            scale_color_manual(values = colores) +
            theme_ipsum() +
            theme(
                axis.title.y = element_text(size = 20),
                axis.text.y = element_text(size = 14),
                axis.text.x = element_text(size = 14),
                legend.position = "",
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_line(color = 'grey60', linetype = 'dashed')) +
            labs(
                title = "Clasificación Violencia Familiar contra la Mujer",
                subtitle = "Top 5 tipos de violencia en Querétaro",
                x = "Año",
                y = "Rank"
            )
    })
})
