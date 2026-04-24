
library(shiny)
library(shinycssloaders)

### Formato Inputs ----

PickerHTML =  function(inputId, etiqueta, opciones, valor, fechaactual, tipo = "picker", 
                       Inicio = FALSE, Fin = FALSE, ReducirAncho = FALSE) {
  
  if (tipo == "picker"){
    Boton = paste0(
      '<div class="list chevron-center" style="margin: 0;">
      <ul style="margin: 0; padding: 0;">
        <li>
          <div class="item-content" style="padding: 4px 0; min-height: 32px;">
            <div class="item-inner" style="padding: 0;">
              <div class="item-input-wrap">
                <input id="', inputId, '" 
                style = "font-size: 13px;"
                class="picker-input" type="text" placeholder="Seleccionar opción"/>
                <script type="application/json" data-for="', inputId, '">
                    {"value":["', valor, '"],
                    "values":[', paste0('"', opciones, '"', collapse = ","),'],
                    "rotateEffect":true,"openIn":"auto","scrollToInput":false,
                    "closeByOutsideClick":true,"toolbar":true,
                    "toolbarCloseText":"Seleccionar","sheetSwipeToClose":false}
                </script>
              </div>
            </div>
          </div>
        </li>
      </ul>
    </div>'
    )
  }
  
  if (tipo == "checkbox"){
    Boton = paste0(
      '<div>
          <label class="checkbox">
            <input id="', inputId, '" type="checkbox" ', 
              ifelse(valor, 'checked="checked"', ''),
              '/>
            <i class="icon-checkbox"></i>
          </label>
      </div>'
    )
  }
  
  if (tipo == "date"){
    Boton = paste0(
      '<div class="list chevron-center">
      <ul>
        <li>
          <div class="item-content">
            <div class="item-inner">
              <div class="item-input-wrap">
                <input id="', inputId, '" class="calendar-input" type="text"/>
                <script type="application/json" 
                  data-for="', inputId, '">{"value":["', valor, '"],
                                    "multiple":false,
                                    "direction":"horizontal",
                                    "minDate":"1800-01-01","maxDate":"', fechaactual,'",
                                    "dateFormat":"yyyy-mm-dd",
                                    "openIn":"auto",
                                    "scrollToInput":false,
                                    "closeByClickOutside":true,
                                    "toolbar":true,
                                    "toolbarCloseText":"Seleccionar",
                                    "header":false,
                                    "headerPlaceholder":"Seleccionar fecha"}
                </script>
              </div>
            </div>
          </div>
        </li>
      </ul>
    </div>'
    )
  }
  
  if (tipo == "button"){
    Boton = paste0(
      '<div>
        <button class="btn btn-default action-button link popup-close" id="', inputId,
        '" style="width:80px;" type="button">Ver</button>
      </div>'
    )
  }
    
  paste0(
    ifelse(Inicio, paste0(
      '<div style="
          max-height: 60vh;
          overflow-y: auto;
          padding: 8px;
          display: flex;
          flex-direction: column;
          gap: 6px;
        ">
        <table style="width: ', ifelse(ReducirAncho, 130, 340),
          'px; table-layout: fixed; border-collapse: collapse;">'), ''),
    '<tr>
    <td style="width: ', ifelse(ReducirAncho, 65, 210),
    'px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;">
      <h3 style="margin: 0; white-space: nowrap; font-size: 13px;">', etiqueta, '</h3></td>
    <td style="width: ', ifelse(ReducirAncho, 65, 130),
    'px";><div style="flex: 1;">',
      Boton,
    '</div>
  </td></tr>',
    ifelse(Fin, '</table></div>', ''))
}

BotonNumerico = function(inputIds, etiquetas, valorPrincipal, listaNumeros, valor1, valor2){
  
  list(
    HTML(PickerHTML(
      inputId = inputIds[1], etiqueta = etiquetas[1], 
      valor = valorPrincipal, tipo = "checkbox", 
      Inicio = TRUE, Fin = TRUE
      
    )),
    conditionalPanel(
      condition = paste0("input.", inputIds[1], " == true"),
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "flex: 0 0 50%; display: flex; justify-content: flex-start; gap: 6px;",
              HTML(
                PickerHTML(
                  inputId = inputIds[2], etiqueta = etiquetas[2], 
                  opciones = as.character(listaNumeros), 
                  valor = valor1, Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
                )
              ),
              HTML(
                PickerHTML(
                  inputId = inputIds[3], etiqueta = etiquetas[3], 
                  opciones = as.character(listaNumeros), 
                  valor = valor2, Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
                )
              )
            )
          )
        )
      )
    )
  )
  
}

BotonFecha = function(inputIds, etiquetas, valorPrincipal, valor1mes, valor1dia, valor2mes, valor2dia){
  
  meses = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
            "septiembre", "octubre", "noviembre", "diciembre")
  
  list(
    HTML(PickerHTML(
      inputId = inputIds[1], etiqueta = etiquetas[1], 
      valor = valorPrincipal, tipo = "checkbox", 
      Inicio = TRUE, Fin = TRUE
    )),
    conditionalPanel(
      condition = paste0("input.", inputIds[1], " == true"),
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "flex: 0 0 50%; display: flex; justify-content: flex-start; gap: 6px;",
              HTML(
                PickerHTML(
                  inputId = inputIds[2], etiqueta = etiquetas[2], 
                  opciones = 1:31, 
                  valor = valor1dia, Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
                )
              ),
              HTML(
                PickerHTML(
                  inputId = inputIds[3], etiqueta = etiquetas[3], 
                  opciones = meses, 
                  valor = valor1mes, Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "flex: 0 0 50%; display: flex; justify-content: flex-start; gap: 6px;",
              HTML(
                PickerHTML(
                  inputId = inputIds[4], etiqueta = etiquetas[4], 
                  opciones = 1:31, 
                  valor = valor2dia, Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
                )
              ),
              HTML(
                PickerHTML(
                  inputId = inputIds[5], etiqueta = etiquetas[5], 
                  opciones = meses, 
                  valor = valor2mes, Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
                )
              )
            )
          )
        )
      )
    )
  )
  
}

PopUpRender = function(id, header, contenido, listaBotones) {
  
  ActionBotones = list()
  
  for (l in listaBotones){
    ActionBotones = append(ActionBotones, list(
      actionButton(
        inputId = l$id, 
        label = l$label,
        class = "link popup-close",
        width = "120px"
      )
    ))
  }
  
  f7Popup(
    id = id,
    title = "",
    closeButton = FALSE,
    closeByBackdropClick = FALSE,
    closeOnEscape = FALSE,
        
    f7Block(
      class = "compact-block",
      style = "padding: 0;",
      f7BlockHeader(h2(header)),
      div(
        style = "
          max-height: 60vh;
          overflow-y: auto;
          padding: 6px;
        ",
        contenido
      ),
      f7BlockFooter(
        div(
          style = "display: flex; justify-content: center;  gap: 8px;",
          ActionBotones
        )
      )
    )
  )
}

### Server ----

server = function(input, output, session) {

  ## reactive Values ----
  
  rv = reactiveValues(
    datosAct = 1,
    estado = "Todos",
    signo = "Todos",
    genero = "Todos",
    profesion = "Todos",
    pais = "Todos",
    FiltrarEdad = FALSE,
    edad1 = 0,
    edad2 = 150, 
    FiltrarNacim = FALSE,
    nac1 = 1900,
    nac2 = year(today()),
    FiltrarFall = FALSE,
    fac1 = 1900,
    fac2 = year(ahora),
    tipo_orden = "Menciones en Wikipedia",
    fecha = as.Date("2000-01-01"),
    relacion = "Todos",
    FiltrarCumple = FALSE,
    dia1 = 1, 
    mes1 = "enero",
    dia2 = 31,
    mes2 = "diciembre",
    FiltrarFechaMuerte = FALSE,
    dia3 = 1, 
    mes3 = "enero",
    dia4 = 31,
    mes4 = "diciembre"
  )
  
  ## Inicio ----
  
  rvPopUpInicio = reactiveValues(
    popup_visible = TRUE,
    popup_cumple = FALSE,
    popup_fechamuerte = FALSE,
    popup_edadmuertos = FALSE,
    popup_profsigno = FALSE, 
    popup_generopais = FALSE,
    popup_cercanianac = FALSE,
    cantidad_clicks = 0
  )
  
  observeEvent(input$VerMenu, {
    rvPopUpInicio$popup_visible = TRUE
  })

  output$popup_inicio = renderUI({
    
    if (rvPopUpInicio$popup_visible){
      PopUpRender(
        id = "popupInicio",
        header = "Bienvenido/a a la app",
        contenido = list(
          h4("Bienvenido/a a la app de consulta de personas. 
             Aquí tenés algunas opciones de visualización:"),
          HTML(paste(
            PickerHTML(
              inputId = "VerCumple",
              etiqueta = "Famosos que cumplen en una </br>fecha determinada:", 
              tipo = "button", Inicio = TRUE),
            PickerHTML(
              inputId = "VerFechaMuerte",
              etiqueta = "Famosos que murieron en una </br>fecha determinada:", 
              tipo = "button", Inicio = TRUE),
            PickerHTML(
              inputId = "VerProfSigno",
              etiqueta = "Famosos de una profesión y </br>un signo determinados:", 
              tipo = "button"),
            PickerHTML(
              inputId = "VerGeneroPais",
              etiqueta = "Famosos de un país y un género 
                          </br>determinados (incluídos 
                          </br>géneros no binarios):", 
              tipo = "button"),
            PickerHTML(
              inputId = "VerCercaniaNac",
              etiqueta = "Famosos con nacimientos 
                          </br>cercanos al tuyo", 
              tipo = "button"),
            PickerHTML(
              inputId = "VerEdadMuertos",
              etiqueta = "Famosos que murieron en una </br>determinada edad:", 
              tipo = "button", Fin = TRUE)
          ))
        ),
        listaBotones = list(list(id = "close_popup_inicio", label = "Continuar manualmente"))
      )
    }
  })
  
  output$popup_cumple = renderUI({
    
    if (rvPopUpInicio$popup_cumple) {
      
      PopUpRender(
        id = "popupCumple", 
        header = "Seleccione fecha de cumpleaños", 
        contenido = list(fluidRow(column(
          width = 12,
          div(
            style = "display:flex; gap:6px; align-items:center;",
            HTML(PickerHTML(
              inputId = "diaInicio",
              etiqueta = "Fecha:",
              opciones = 1:31,
              valor = rv$dia1,
              Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
            )),
            HTML(PickerHTML(
              inputId = "mesInicio",
              etiqueta = "de",
              opciones = meses,
              valor = rv$mes1,
              Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
            ))
          )
        ))), 
        listaBotones = list(
          list(id = "close_popup_cumple1", label = "Aplicar"),
          list(id = "close_popup_cumple2", label = "Volver al menú")
      )
    )
    }
    
  })
  
  output$popup_fechamuerte = renderUI({
    
    if (rvPopUpInicio$popup_fechamuerte) {
      
      PopUpRender(
        id = "popupFechaMuerte", 
        header = "Seleccione fecha de muerte", 
        contenido = list(fluidRow(column(
          width = 12,
          div(
            style = "display:flex; gap:6px; align-items:center;",
            HTML(PickerHTML(
              inputId = "diaInicioM",
              etiqueta = "Fecha:",
              opciones = 1:31,
              valor = rv$dia3,
              Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
            )),
            HTML(PickerHTML(
              inputId = "mesInicioM",
              etiqueta = "de",
              opciones = meses,
              valor = rv$mes3,
              Inicio = TRUE, Fin = TRUE, ReducirAncho = TRUE
            ))
          )
        ))), 
        listaBotones = list(
          list(id = "close_popup_fechamuerte1", label = "Aplicar"),
          list(id = "close_popup_fechamuerte2", label = "Volver al menú")
        )
      )
    }
    
  })
  
  output$popup_edadmuertos = renderUI({
    
    if (rvPopUpInicio$popup_edadmuertos){
      
      PopUpRender(
        id = "popup_edadmuertos", 
        header = "Seleccione edad de muerte", 
        contenido = list(
          HTML(PickerHTML(
            inputId = "edadMuerte", 
            etiqueta = "Edad", 
            opciones = as.character(0:150), 
            valor = rv$edad1, 
            Inicio = TRUE, Fin = TRUE
          ))
        ), 
        listaBotones = list(
          list(id = "close_popup_edadmuertos1", label = "Aplicar"),
          list(id = "close_popup_edadmuertos2", label = "Volver al menú")
        )
      )
    }
    
  })
  
  output$popup_profsigno = renderUI({
    
    if (rvPopUpInicio$popup_profsigno){
      
      PopUpRender(
        id = "popup_profsigno", 
        header = "Seleccione la profesión y el signo", 
        contenido = list(HTML(paste(
          PickerHTML(
            inputId = "profesion_profsigno", etiqueta = "Profesión", 
            opciones = c("Todos", names(categoriasProfesiones)),
            valor = rv$profesion, Inicio = TRUE),
          PickerHTML(
            inputId = "signo_profsigno", etiqueta = "Signo", 
            opciones = c(
              "Todos", "Desconocido", "Aries", "Tauro", "Géminis", "Cáncer", "Leo", "Virgo", 
              "Libra", "Escorpio", "Sagitario", "Capricornio", "Acuario", "Piscis"), 
            valor = rv$signo, Fin = TRUE)))), 
        listaBotones = list(
          list(id = "close_popup_profsigno1", label = "Aplicar"),
          list(id = "close_popup_profsigno2", label = "Volver al menú")
        )
      )
    }
    
  })
  
  output$popup_generopais = renderUI({
    
    if (rvPopUpInicio$popup_generopais){
      
      PopUpRender(
        id = "popup_generopais", 
        header = "Seleccione el país y el género", 
        contenido = list(HTML(paste(
          PickerHTML(
            inputId = "pais_generopais", etiqueta = "País", 
            opciones = c("Todos", names(categoriasNacionalidades)),
            valor = rv$pais, Inicio = TRUE),
          PickerHTML(
            inputId = "genero_generopais", etiqueta = "Género", 
            opciones = c(
              "Todos", "Hombre", "Hombre transgénero", "Intersexo",         
              "Mujer", "Mujer transgénero / Travesti", "Persona no binaria",
              "Sin especificar"), 
            valor = rv$genero, Fin = TRUE)))), 
        listaBotones = list(
          list(id = "close_popup_generopais1", label = "Aplicar"),
          list(id = "close_popup_generopais2", label = "Volver al menú")
        )
      )
    }
    
  })
  
  output$popup_cercanianac = renderUI({
    
    if (rvPopUpInicio$popup_cercanianac){
      
      PopUpRender(
        id = "popup_cercanianac", 
        header = "Indique su fecha de nacimiento", 
        contenido = list(HTML(
          PickerHTML(
            tipo = "date",
            inputId = "fecha_cercanianac",
            valor = rv$fecha, 
            etiqueta = "Tu fecha de nacimiento:", 
            fechaactual = ahora, 
            Inicio = TRUE, Fin = TRUE
          ))), 
        listaBotones = list(
          list(id = "close_popup_cercanianac1", label = "Aplicar"),
          list(id = "close_popup_cercanianac2", label = "Volver al menú")
        )
      )
    }
    
  })
  
  observeEvent(input$close_popup_inicio, {
    rvPopUpInicio$popup_visible = FALSE
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
  })
  
  observeEvent(input$VerCumple, {
    rvPopUpInicio$popup_visible = FALSE
    rvPopUpInicio$popup_cumple = TRUE
  })
  
  observeEvent(input$close_popup_cumple1, {
    
    rv$FiltrarCumple = TRUE
    rv$dia1 = input$diaInicio
    rv$dia2 = input$diaInicio
    rv$mes1 = input$mesInicio
    rv$mes2 = input$mesInicio
    
    rvPopUpInicio$popup_cumple = FALSE
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
    
  })
  
  observeEvent(input$close_popup_cumple2, {
    
    rvPopUpInicio$popup_cumple = FALSE
    rvPopUpInicio$popup_visible = TRUE
    
  })
  
  observeEvent(input$VerFechaMuerte, {
    rvPopUpInicio$popup_visible = FALSE
    rvPopUpInicio$popup_fechamuerte = TRUE
    
  })
  
  observeEvent(input$close_popup_fechamuerte1, {
    
    rv$FiltrarFechaMuerte = TRUE
    rv$dia3 = input$diaInicioM
    rv$dia4 = input$diaInicioM
    rv$mes3 = input$mesInicioM
    rv$mes4 = input$mesInicioM
    
    rvPopUpInicio$popup_fechamuerte = FALSE
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
    
  })
  
  observeEvent(input$close_popup_cumple2, {
    
    rvPopUpInicio$popup_fechamuerte = FALSE
    rvPopUpInicio$popup_visible = TRUE
    
  })
  
  observeEvent(input$VerEdadMuertos, {
    rvPopUpInicio$popup_visible = FALSE
    rvPopUpInicio$popup_edadmuertos = TRUE
  })
  
  observeEvent(input$close_popup_edadmuertos1, {
    
    rv$estado = "Muertos"
    rv$FiltrarEdad = TRUE
    rv$edad1 = input$edadMuerte
    rv$edad2 = input$edadMuerte
    
    rvPopUpInicio$popup_edadmuertos = FALSE
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
    
  })
  
  observeEvent(input$close_popup_edadmuertos2, {
    
    rvPopUpInicio$popup_edadmuertos = FALSE
    rvPopUpInicio$popup_visible = TRUE
    
  })
  
  observeEvent(input$VerProfSigno, {
    rvPopUpInicio$popup_visible = FALSE
    rvPopUpInicio$popup_profsigno = TRUE
  })
  
  observeEvent(input$close_popup_profsigno1, {
    
    rv$profesion = input$profesion_profsigno
    rv$signo = input$signo_profsigno
    
    rvPopUpInicio$popup_profsigno = FALSE
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
    
  })
  
  observeEvent(input$close_popup_profsigno2, {
    
    rvPopUpInicio$popup_profsigno = FALSE
    rvPopUpInicio$popup_visible = TRUE
    
  })
  
  observeEvent(input$VerGeneroPais, {
    rvPopUpInicio$popup_visible = FALSE
    rvPopUpInicio$popup_generopais = TRUE
  })
  
  observeEvent(input$close_popup_generopais1, {
    
    rv$pais = input$pais_generopais
    rv$genero = input$genero_generopais
    
    rvPopUpInicio$popup_generopais = FALSE
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
    
  })
  
  observeEvent(input$close_popup_generopais2, {
    
    rvPopUpInicio$popup_generopais = FALSE
    rvPopUpInicio$popup_visible = TRUE
    
  })
  
  observeEvent(input$VerCercaniaNac, {
    rvPopUpInicio$popup_visible = FALSE
    rvPopUpInicio$popup_cercanianac = TRUE
  })
  
  observeEvent(input$close_popup_cercanianac1, {
    
    rv$tipo_orden = "Cercanía a tu nacimiento"
    rv$fecha = input$fecha_cercanianac
    rv$FiltrarNacim = TRUE
    rv$nac1 = year(input$fecha_cercanianac) - 1
    rv$nac2 = year(input$fecha_cercanianac) + 1
    
    rvPopUpInicio$popup_cercanianac = FALSE
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
    
  })
  
  observeEvent(input$close_popup_cercanianac2, {
    
    rvPopUpInicio$popup_cercanianac = FALSE
    rvPopUpInicio$popup_visible = TRUE
    
  })
  
  output$popup_instrucciones = renderUI({
    
    if (rvPopUpInicio$cantidad_clicks == 1){
      
      PopUpRender(
        id = "popup_instrucciones", 
        header = "Mostrando tabla", 
        contenido = list(
          tags$ul(
            style = "font-size: 1.2em;",
            tags$div(HTML(
              "Muchas gracias por entrar a la app de consulta de personas. Recordá:</br></br>")),
            tags$li(HTML("Si querés volver a ver las opciones, presioná el <b>Menú principal</b>.")),
            tags$li(HTML("Para opciones avanzadas, presioná <b>Más filtros</b>.")),
            tags$li(HTML("Para cambiar el orden, presioná <b>Ordenar</b>.")),
            tags$li(HTML("Para más información acerca de la persona, presioná <b>Descripción</b> a
                         la derecha de la tabla."))
          )
        ), 
        listaBotones = list(
          list(id = "close_popup_instrucciones", label = "Continuar")
        )
      )
    }
    
  })
  
  observeEvent(input$close_popup_instrucciones, {
    rvPopUpInicio$cantidad_clicks = rvPopUpInicio$cantidad_clicks + 1
  })
  
  ## Filtro Opciones ----
  
  rvPopUp = reactiveValues(
    popup_visible = FALSE,
    popup_errores_visible = FALSE,
    listaerrores = list(),
    ErrorEdad = FALSE,
    ErrorNacimiento = FALSE
  )
  
  output$Opciones = renderUI({
    
    list(
      HTML(paste0(
        PickerHTML(
          inputId = "estado", etiqueta = "Estado", 
          opciones = c("Todos", "Vivos", "Muertos"),
          valor = rv$estado, 
          Inicio = TRUE),
        PickerHTML(
          inputId = "signo", etiqueta = "Signo", 
          opciones = c(
            "Todos", "Desconocido", "Aries", "Tauro", "Géminis", "Cáncer", "Leo", "Virgo", 
            "Libra", "Escorpio", "Sagitario", "Capricornio", "Acuario", "Piscis"), 
          valor = rv$signo),
        PickerHTML(
          inputId = "genero", etiqueta = "Género", 
          opciones = c(
            "Todos", "Hombre", "Hombre transgénero", "Intersexo",         
            "Mujer", "Mujer transgénero / Travesti", "Persona no binaria",
            "Sin especificar"), 
          valor = rv$genero),
        PickerHTML(
          inputId = "profesion", etiqueta = "Profesión", 
          opciones = c("Todos", names(categoriasProfesiones)),
          valor = rv$profesion),
        PickerHTML(
          inputId = "pais", etiqueta = "País", 
          opciones = c("Todos", names(categoriasNacionalidades)),
          valor = rv$pais,
          Fin = TRUE))),
      BotonNumerico(
        inputIds = c("FiltrarEdad", "edad1", "edad2"),
        etiquetas = c("Filtrar por edad", "Desde", "Hasta"),
        valorPrincipal = rv$FiltrarEdad,
        listaNumeros = 0:150,
        valor1 = rv$edad1,
        valor2 = rv$edad2),
      BotonNumerico(
        inputIds = c("FiltrarNacim", "nac1", "nac2"),
        etiquetas = c("Filtrar por año de nacimiento", "Desde", "Hasta"),
        valorPrincipal = rv$FiltrarNacim,
        listaNumeros = c(seq(0, 1750, 50), 1800:year(today())),
        valor1 = rv$nac1,
        valor2 = rv$nac2
      ),
      conditionalPanel(
        condition = "input.estado != 'Vivos'",
        BotonNumerico(
          inputIds = c("FiltrarFall", "fac1", "fac2"),
          etiquetas = c("Filtrar por año de fallecimiento", "Desde", "Hasta"),
          valorPrincipal = rv$FiltrarFall,
          listaNumeros = c(seq(0, 1750, 50), 1800:year(today())),
          valor1 = rv$fac1,
          valor2 = rv$fac2
        )
      ),
      BotonFecha(
        inputIds = c("FiltrarCumple", "dia1", "mes1", "dia2", "mes2"), 
        etiquetas = c("Filtrar por fecha de cumpleaños", "Desde", "de", "Hasta", "de"), 
        valorPrincipal = rv$FiltrarCumple, 
        valor1mes = rv$mes1, valor1dia = rv$dia1, valor2mes = rv$mes2, valor2dia = rv$dia2),
      BotonFecha(
        inputIds = c("FiltrarFechaMuerte", "dia3", "mes3", "dia4", "mes4"), 
        etiquetas = c("Filtrar por fecha de muerte", "Desde", "de", "Hasta", "de"), 
        valorPrincipal = rv$FiltrarFechaMuerte, 
        valor1mes = rv$mes3, valor1dia = rv$dia3, valor2mes = rv$mes4, valor2dia = rv$dia4)
    )
    
  })
  
  output$popup_ui = renderUI({
    
    if (rvPopUp$popup_visible) {
      
      PopUpRender(
        id = "popup_ui", 
        header = "Filtros", 
        contenido = uiOutput("Opciones"), 
        listaBotones = list(
          list(id = "close_popup", label = "Aplicar") 
        ))   
    }
    
  })
  
  output$popup_ui_errores = renderUI({
    
    if (rvPopUp$popup_errores_visible) {
      
      PopUpRender(
        id = "popup_error", 
        header = "Error en los filtros", 
        contenido = list(
          h3(textOutput("ErrorEdadTexto")),
          h3(textOutput("ErrorNacimientoTexto")),
          h3(textOutput("ErrorFallecimientoTexto")),
          h3(textOutput("ErrorFallecimiento2Texto")),
          h3(textOutput("ErrorCumple1Texto")),
          h3(textOutput("ErrorCumple2Texto")),
          h3(textOutput("ErrorFechaMuerte1Texto")),
          h3(textOutput("ErrorFechaMuerte2Texto"))
        ),
        listaBotones = list(
          list(id = "close_popup_errores1", label = "Corregir"),
          list(id = "close_popup_errores1", label = "Seguir de todas maneras")
        ))
    }
    
  })
  
  observeEvent(input$close_popup, {
    
    rv$estado = input$estado
    rv$signo = input$signo
    rv$genero = input$genero
    rv$profesion = input$profesion
    rv$pais = input$pais
    rv$FiltrarEdad = input$FiltrarEdad
    
    if (rv$FiltrarEdad){
      rv$edad1 = input$edad1
      rv$edad2 = input$edad2
    } else {
      rv$edad1 = 0
      rv$edad2 = 150
    }
    
    rv$FiltrarNacim = input$FiltrarNacim
    
    if (rv$FiltrarNacim){
      rv$nac1 = input$nac1
      rv$nac2 = input$nac2
    } else {
      rv$nac1 = 1900
      rv$nac2 = year(today())
    }
    
    rv$FiltrarFall = input$FiltrarFall
    
    if (rv$FiltrarFall){
      rv$fac1 = input$fac1
      rv$fac2 = input$fac2
    } else {
      rv$fac1 = 1900
      rv$fac2 = year(today())
    }
    
    rv$FiltrarCumple = input$FiltrarCumple
    
    if (rv$FiltrarCumple){
      rv$dia1 = input$dia1
      rv$mes1 = input$mes1
      rv$dia2 = input$dia2
      rv$mes2 = input$mes2
    } else {
      rv$dia1 = 1
      rv$mes1 = "enero"
      rv$dia2 = 31
      rv$mes2 = "diciembre"
    }
    
    rv$FiltrarFechaMuerte = input$FiltrarFechaMuerte
    
    if (rv$FiltrarFechaMuerte){
      rv$dia3 = input$dia3
      rv$mes3 = input$mes3
      rv$dia4 = input$dia4
      rv$mes4 = input$mes4
    } else {
      rv$dia3 = 1
      rv$mes3 = "enero"
      rv$dia4 = 31
      rv$mes4 = "diciembre"
    }
    
    rvPopUp$popup_visible = FALSE
    
    rvPopUp$ErrorEdad = (rv$edad1 > rv$edad2)
    rvPopUp$ErrorNacimiento = (rv$nac1 > rv$nac2)
    rvPopUp$ErrorFallecimiento = (rv$fac1 > rv$fac2)
    rvPopUp$ErrorFallecimiento2 = (rv$fac2 < rv$nac1)
    
    rvPopUp$ErrorCumple1 = FALSE
    if (rv$mes1 == "febrero" & as.numeric(rv$dia1) > 29) rvPopUp$ErrorCumple1 = TRUE
    if (as.numeric(rv$dia1) == 31){
      if (rv$mes1 %in% c("abril", "junio", "septiembre", "noviembre")){
        rvPopUp$ErrorCumple1 = TRUE
      }
    } 
    
    rvPopUp$ErrorCumple2 = FALSE
    if (rv$mes2 == "febrero" & as.numeric(rv$dia2) > 29) rvPopUp$ErrorCumple2 = TRUE
    if (as.numeric(rv$dia2) == 31){
      if (rv$mes2 %in% c("abril", "junio", "septiembre", "noviembre")){
        rvPopUp$ErrorCumple2 = TRUE
      }
    }

    rvPopUp$ErrorFechaMuerte1 = FALSE
    if (rv$mes3 == "febrero" & as.numeric(rv$dia3) > 29) rvPopUp$ErrorFechaMuerte1 = TRUE
    if (as.numeric(rv$dia3) == 31){
      if (rv$mes3 %in% c("abril", "junio", "septiembre", "noviembre")){
        rvPopUp$ErrorFechaMuerte1 = TRUE
      }
    } 
    
    rvPopUp$ErrorFechaMuerte2 = FALSE
    if (rv$mes4 == "febrero" & as.numeric(rv$dia4) > 29) rvPopUp$ErrorFechaMuerte2 = TRUE
    if (as.numeric(rv$dia4) == 31){
      if (rv$mes4 %in% c("abril", "junio", "septiembre", "noviembre")){
        rvPopUp$ErrorFechaMuerte2 = TRUE
      }
    }
    
    listaErrores = c(
      rvPopUp$ErrorEdad, rvPopUp$ErrorNacimiento, rvPopUp$ErrorFallecimiento, 
      rvPopUp$ErrorFallecimiento2, rvPopUp$ErrorCumple1, rvPopUp$ErrorCumple2,
      rvPopUp$ErrorFechaMuerte1, rvPopUp$ErrorFechaMuerte2
    )
    
    if (any(listaErrores)) rvPopUp$popup_errores_visible = TRUE
    
  })
  
  observeEvent(input$VerFiltros, {
    
    rvPopUp$popup_visible = TRUE
    
  })
  
  observeEvent(input$close_popup_errores1, {
    
    rvPopUp$popup_errores_visible = FALSE
    rvPopUp$popup_visible = TRUE
    
  })
  
  observeEvent(input$close_popup_errores2, {
    
    rvPopUp$popup_errores_visible = FALSE
    
  })
  
  output$ErrorEdadTexto = renderText({
    
    if (rvPopUp$ErrorEdad) r = "Error en el rango de edad" else r = ""
    
    return(r)
    
  })
  
  output$ErrorNacimientoTexto = renderText({
    
    if (rvPopUp$ErrorNacimiento) r = "Error en el rango de nacimiento" else r = ""
    
    return(r)
    
  })
  
  output$ErrorFallecimientoTexto = renderText({
    
    if (rvPopUp$ErrorFallecimiento) r = "Error en el rango de fallecimiento" else r = ""
    
    return(r)
    
  })
  
  output$ErrorFallecimiento2Texto = renderText({
    
    if (rvPopUp$ErrorFallecimiento2){
      r = "Fechas de fallecimiento posibles anteriores a las de nacimiento"
    } else r = ""
    
    return(r)
    
  })
  
  output$ErrorCumple1Texto = renderText({
    
    if (rvPopUp$ErrorCumple1) r = "Error en la fecha de cumpleaños inferior" else r = ""
    
    return(r)
    
  })
  
  output$ErrorCumple2Texto = renderText({
    
    if (rvPopUp$ErrorCumple2) r = "Error en la fecha de cumpleaños superior" else r = ""
    
    return(r)
    
  })
  
  output$ErrorFechaMuerte1Texto = renderText({
    
    if (rvPopUp$ErrorFechaMuerte1) r = "Error en la fecha de muerte inferior" else r = ""
    
    return(r)
    
  })
  
  output$ErrorFechaMuerte2Texto = renderText({
    
    if (rvPopUp$ErrorFechaMuerte2) r = "Error en la fecha de muerte superior" else r = ""
    
    return(r)
    
  })
  
  ## Orden ----
  
  popupOrden_visible = reactiveVal(FALSE)
  
  output$popup_orden = renderUI({
    
    if (popupOrden_visible()){
      
      f7Popup(
        id = "popup_orden_tabla",
        title = "",
        closeButton = FALSE,
        closeByBackdropClick = FALSE,
        closeOnEscape = FALSE,
        
        f7Block(
          class = "compact-block",
          style = "padding: 0;",
          f7BlockHeader(h2("Orden")),
          div(
            style = "
              max-height: 60vh;
              overflow-y: auto;
              padding: 6px;
            ",
            HTML(PickerHTML(
              inputId = "tipo_orden", 
              etiqueta = "Ordenar por", 
              opciones = c(
                "Menciones en Wikipedia", "Cercanía a tu nacimiento", "Cercanía a tu edad",
                "Alfabético", "De más joven a más viejo", "De más viejo a más joven", 
                "Fecha de nacimiento (ascendente)", "Fecha de nacimiento (descendente)",
                "Fecha de fallecimiento (ascendente)", "Fecha de fallecimiento (descendente)"), 
              valor = rv$tipo_orden, Inicio = TRUE, Fin = TRUE
            )), 
            conditionalPanel(
              condition = "
              input.tipo_orden == 'Cercanía a tu nacimiento' | 
              input.tipo_orden == 'Cercanía a tu edad'",
              HTML(paste(PickerHTML(
                tipo = "date",
                inputId = "fecha",
                valor = rv$fecha, 
                etiqueta = "Tu fecha de nacimiento:", 
                fechaactual = ahora, 
                Inicio = TRUE
              ),
              PickerHTML(
                inputId = "relacion", etiqueta = "Relación", 
                opciones = c("Todos", "Mayores", "Menores"), 
                valor = rv$relacion, 
                Fin = TRUE
              )))
            )
          ),
          f7BlockFooter(
            div(
              style = "display: flex; justify-content: center;",
              actionButton(
                "close_popup_orden",
                "Aplicar",
                class = "link popup-close",
                width = "120px"
              )
            )
          )
        )
      )
      
    }
    
  })
  
  observeEvent(input$close_popup_orden, {
    
    rv$tipo_orden = input$tipo_orden
    rv$fecha = input$fecha
    
    if (rv$tipo_orden %in% c("Cercanía a tu nacimiento", "Cercanía a tu edad")){
      rv$relacion = input$relacion
    } else {
      rv$relacion = "Todos"
    }
    
    popupOrden_visible(FALSE)
    
  })
  
  observeEvent(input$VerOrden, {
    
    popupOrden_visible(TRUE)
    
  })
  
  ## Filtro Texto ----
  
  filtro_texto = reactive({
    input$filtro
  })
  
  filtro_texto_d = filtro_texto %>% debounce(2000)
  
  lista_inputs = reactive({
    list(
      estado = rv$estado,
      signo = rv$signo,
      genero = rv$genero,
      profesion = rv$profesion,
      pais = rv$pais,
      FiltrarEdad = rv$FiltrarEdad,
      edad1 = as.numeric(rv$edad1),
      edad2 = as.numeric(rv$edad2),
      FiltrarNacim = rv$FiltrarNacim,
      nac1 = as.numeric(rv$nac1),
      nac2 = as.numeric(rv$nac2),
      FiltrarFall = rv$FiltrarFall,
      fac1 = as.numeric(rv$fac1),
      fac2 = as.numeric(rv$fac2),
      tipo_orden = rv$tipo_orden,
      fecha = as.Date(rv$fecha),
      relacion = rv$relacion,
      FiltrarCumple = rv$FiltrarCumple,
      dia1 = as.numeric(rv$dia1), 
      mes1 = rv$mes1,
      dia2 = as.numeric(rv$dia2),
      mes2 = rv$mes2,
      FiltrarFechaMuerte = rv$FiltrarFechaMuerte,
      dia3 = as.numeric(rv$dia3), 
      mes3 = rv$mes3,
      dia4 = as.numeric(rv$dia4),
      mes4 = rv$mes4
    )
  })
  
  lista_inputs_d = lista_inputs %>% debounce(1000)
  
  ## Tabla edades ----
  
  tablaEdades = reactive({
    
    r = data.frame()
    
    try({
      
      e = interval(as.Date(lista_inputs_d()$fecha), as.Date(ahora))
      
      e = as.period(e, unit = "year")
      
      r = listaEdades
      
      if (lista_inputs_d()$estado == "Vivos") r = r %>% filter(Vivo == "SI")
      if (lista_inputs_d()$estado == "Muertos") r = r %>% filter(Vivo == "NO")
      if (lista_inputs_d()$signo != "Todos") r = r %>% filter(signo == lista_inputs_d()$signo)
      
      if (lista_inputs_d()$genero == "Mujer transgénero / Travesti"){
        
        r = r %>% filter(genero %in% c("Mujer transgénero", "Travesti"))
        
      } else if (lista_inputs_d()$genero == "Intersexo") {
        
        r = r %>% filter(genero %in% c("Mujer intersexo", "Hombre intersexo", "Intersexo"))
        
      } else if (lista_inputs_d()$genero == "Persona no binaria") {
        
        r = r %>% filter(genero %in% c(
          "Persona no binaria", "Persona de género fluido", 
          "Persona agénero", "Persona dos espíritus"))
        
      } else if (lista_inputs_d()$genero != "Todos"){
        
        r = r %>% filter(genero == lista_inputs_d()$genero)
        
      } 
      
      if (lista_inputs_d()$FiltrarEdad){
        r = r %>% filter(anios >= lista_inputs_d()$edad1, anios <= lista_inputs_d()$edad2)
      } 
      
      if (lista_inputs_d()$FiltrarNacim){
        
        r = r %>% filter(
          year(fecha_nac) >= lista_inputs_d()$nac1, year(fecha_nac) <= lista_inputs_d()$nac2)
        
      }
      
      if (lista_inputs_d()$FiltrarFall & lista_inputs_d()$estado != "Vivos"){
        
        if (lista_inputs_d()$fac2 == year(today())){
          
          r = r %>% filter(year(fecha_fac) >= lista_inputs_d()$fac1 | is.na(fecha_fac))
          
        } else {
          r = r %>% 
            filter(year(fecha_fac) >= lista_inputs_d()$fac1, 
                   year(fecha_fac) <= lista_inputs_d()$fac2)
        }
        
      }
      
      if (lista_inputs_d()$FiltrarCumple){
        
        r = r %>% filter(!(EsNacPrimeroEnero & EstimadoNac))
        
        mes1num = which(meses == lista_inputs_d()$mes1)
        mes2num = which(meses == lista_inputs_d()$mes2)
        
        dia1 = as.numeric(lista_inputs_d()$dia1)
        dia2 = as.numeric(lista_inputs_d()$dia2)
        
        m = month(r$fecha_nac)
        d = day(r$fecha_nac)
        
        if (mes1num < mes2num || (mes1num == mes2num && dia1 <= dia2)) {
          
          # rango normal (no cruza año)
          r = r %>% filter(
            (m > mes1num | (m == mes1num & d >= dia1)) &
              (m < mes2num | (m == mes2num & d <= dia2))
          )
          
        } else {
          
          # rango cruza año
          r = r %>% filter(
            (m > mes1num | (m == mes1num & d >= dia1)) |
              (m < mes2num | (m == mes2num & d <= dia2))
          )
        }
        
      }
      
      if (lista_inputs_d()$FiltrarFechaMuerte & lista_inputs_d()$estado != "Vivos"){
        
        r = r %>% filter(!(EsFacPrimeroEnero & EstimadoFac))
        
        mes3num = which(meses == lista_inputs_d()$mes3)
        mes4num = which(meses == lista_inputs_d()$mes4)
        
        dia3 = as.numeric(lista_inputs_d()$dia3)
        dia4 = as.numeric(lista_inputs_d()$dia4)
        
        m = month(r$fecha_fac)
        d = day(r$fecha_fac)
        
        if (mes3num < mes4num || (mes3num == mes4num && dia3 <= dia4)) {
          
          # rango normal (no cruza año)
          r = r %>% filter(
            !is.na(fecha_fac),
            (m > mes3num | (m == mes3num & d >= dia3)) &
              (m < mes4num | (m == mes4num & d <= dia4))
          )
          
        } else {
          
          # rango cruza año
          r = r %>% filter(
            !is.na(fecha_fac),
            (m > mes3num | (m == mes3num & d >= dia3)) |
              (m < mes4num | (m == mes4num & d <= dia4))
          )
        }
        
      }
      
      if (lista_inputs_d()$pais != "Todos"){
        
        ap = which(sapply(ListaNacionalidades, function(x) x$Nombre) == lista_inputs_d()$pais)
        
        filtroPais = ListaNacionalidades[[ap]]$Categorias
        filtroPais = gsub("á", "(á|%C3%A1)", filtroPais)
        filtroPais = gsub("é", "(é|%C3%A9)", filtroPais)
        filtroPais = gsub("í", "(í|%C3%AD)", filtroPais)
        filtroPais = gsub("ó", "(ó|%C3%B3)", filtroPais)
        filtroPais = gsub("ú", "(ú|%C3%BA)", filtroPais)
        filtroPais = gsub(" ", "[ _]", filtroPais)
        
        filtroPais = paste0(
          "(^|[[:punct:]\\s/])(",
          paste(filtroPais, collapse = "|"),
          ")([[:punct:]\\s/]|$)"
        )
        
        r1 = r %>% 
          filter(
            !is.na(Nacionalidad), 
            grepl(filtroPais, Nacionalidad, ignore.case = TRUE, perl = TRUE))
        
        r2 = r %>% 
          filter(
            is.na(Nacionalidad),
            !titulo %in% ListaNacionalidades[[ap]]$ExcluirPersonas,
            grepl(filtroPais, categorias, ignore.case = FALSE, perl = TRUE) |
              grepl(filtroPais, descripcionCorta, ignore.case = FALSE, perl = TRUE) |
              titulo %in% ListaNacionalidades[[ap]]$IncluirPersonas
          )
        
        if ("Excluir" %in% names(ListaNacionalidades[[ap]])){
          
          filtroE = ListaNacionalidades[[ap]]$Excluir
          filtroE = paste0("(", paste(filtroE, collapse = "|"), ")")
          
          r2 = r2 %>% 
            filter(
              !grepl(filtroE, categorias, ignore.case = TRUE, perl = TRUE) |
                titulo %in% ListaNacionalidades[[ap]]$IncluirPersonas)
          
        }
        
        r = dplyr::bind_rows(r1, r2)
        
      }
      
      if (lista_inputs_d()$profesion != "Todos"){
        
        ap = (sapply(ListaProfesiones, function(x) x$Nombre) == lista_inputs_d()$profesion)
        ap = which(ap)[1]
        
        filtroP = ListaProfesiones[[ap]]$Categorias
        filtroP = paste0("(", paste(filtroP, collapse = "|"), ")")
        
        r = r %>% filter(grepl(filtroP, categorias, ignore.case = TRUE, perl = TRUE))
        
        if ("Excluir" %in% names(ListaProfesiones[[ap]])){
          
          filtroE = ListaProfesiones[[ap]]$Excluir
          filtroE = paste0("(", paste(filtroE, collapse = "|"), ")")
          
          r = r %>% filter(!grepl(filtroE, categorias, ignore.case = TRUE, perl = TRUE))
          
        }
        
      }
      
      if (trimws(filtro_texto_d()) != ""){

        filtro1 = trimws(filtro_texto_d())
        filtro1 = gsub("a", "(a|á|â)", tolower(filtro1))
        filtro1 = gsub("e", "(e|é|ê)", tolower(filtro1))
        filtro1 = gsub("i", "(i|í|î)", tolower(filtro1))
        filtro1 = gsub("o", "(o|ó|ô|ö)", tolower(filtro1))
        filtro1 = gsub("u", "(u|ú|ü|û)", tolower(filtro1))
        filtro1 = gsub(" ", ")(?=.*", filtro1)
        filtro1 = paste0("(?=.*", filtro1, ")")

        r = r %>%
          filter(grepl(filtro1, paste(titulo, Nombre), ignore.case = TRUE, perl = TRUE))

      }
      
      if (lista_inputs_d()$tipo_orden == "Menciones en Wikipedia"){
        r = r %>% arrange(desc(n))
      }
      
      if (lista_inputs_d()$tipo_orden == "Alfabético"){
        r = r %>% arrange(titulo)
      }
      
      if (lista_inputs_d()$tipo_orden == "De más joven a más viejo"){
        r = r %>% arrange(edadN)
      }
      
      if (lista_inputs_d()$tipo_orden == "De más viejo a más joven"){
        r = r %>% arrange(desc(edadN))
      }
      
      if (lista_inputs_d()$tipo_orden == "Cercanía a tu edad"){
        
        if (lista_inputs_d()$relacion == "Menores") r = r %>% filter(edadN <= e)
        if (lista_inputs_d()$relacion == "Mayores") r = r %>% filter(edadN >= e)
        
        r$total = ceiling(as.numeric(e - r$edadN) / 86400)
        r = r %>% arrange(abs(total))
        
      }
      
      if (lista_inputs_d()$tipo_orden == "Cercanía a tu nacimiento"){
        
        if (lista_inputs_d()$relacion == "Menores"){
          r = r %>% filter(fecha_nac >= lista_inputs_d()$fecha)
        } 
        if (lista_inputs_d()$relacion == "Mayores"){
          r = r %>% filter(fecha_nac <= lista_inputs_d()$fecha)
        } 
        
        r$total = as.numeric(lista_inputs_d()$fecha - r$fecha_nac)
        r = r %>% arrange(abs(total))
      }
      
      if (lista_inputs_d()$tipo_orden == "Fecha de nacimiento (ascendente)"){
        r = r %>% arrange(fecha_nac)
      }
      
      if (lista_inputs_d()$tipo_orden == "Fecha de nacimiento (descendente)"){
        r = r %>% arrange(desc(fecha_nac))
      }
      
      if (lista_inputs_d()$tipo_orden == "Fecha de fallecimiento (ascendente)"){
        r = r %>% arrange(is.na(fecha_fac), fecha_fac)
      }
      
      if (lista_inputs_d()$tipo_orden == "Fecha de fallecimiento (descendente)"){
        r = r %>% arrange(!is.na(fecha_fac), desc(fecha_fac))
      }
      
      opciones = c(
        "Menciones en Wikipedia", "Cercanía a tu nacimiento", "Cercanía a tu edad",
        "Alfabético", "De más joven a más viejo", "De más viejo a más joven", 
        "Fecha de nacimiento (ascendente)", "Fecha de nacimiento (descendente)",
        "Fecha de fallecimiento (ascendente)", "Fecha de fallecimiento (descendente)")
      
    })
    
    return(r)
    
  })
  
  ## Controles ----
  
  observeEvent(lista_inputs_d(),{
    
    rv$datosAct = 1
    
  })
  
  observeEvent(filtro_texto_d(), {
    rv$datosAct = 1
  })
  
  observeEvent(input$full_atras, {
    
    rv$datosAct = 1
    
  })
  
  observeEvent(input$atras, {
    
    if (rv$datosAct > 1) rv$datosAct = rv$datosAct - 6
    
  })
  
  observeEvent(input$adelante, {
    
    if (nrow(tablaEdades()) %% 6 == 0){
      limite = nrow(tablaEdades())
    } else {
      limite = floor(nrow(tablaEdades()) / 6) * 6 + 1
    }
    
    rv$datosAct = rv$datosAct + 6
    
    if (rv$datosAct > limite) rv$datosAct = limite
    
  })
  
  observeEvent(input$full_adelante, {
    
    if (nrow(tablaEdades()) %% 6 == 0){
      limite = nrow(tablaEdades())
    } else {
      limite = floor(nrow(tablaEdades()) / 6) * 6 + 1
    }
    
    rv$datosAct = limite
    
  })
  
  output$controlesCancelar = renderUI({
    
    r = list()
    
    for (i in c("estado", "signo", "genero", "profesion", "pais", "relacion")){
      
      if (lista_inputs()[[i]] != "Todos"){
        r = append(r, list(
          actionButton(
            inputId = paste0(i, "Cancelar"),
            label = tags$span(
              style = "
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
              ",
              span(lista_inputs_d()[[i]]),
              icon("close")
            ),
            style = "
              color: #fff;
              background-color: red;
              border-color: #fff;
              width: 120px;
              padding: 5px;
              margin: 5px;
            "
          )
        ))
      }
    }
    
    if (lista_inputs_d()$tipo_orden != "Menciones en Wikipedia"){
      
      if (lista_inputs_d()$tipo_orden == "Cercanía a tu nacimiento"){
        ancho = "180px"
        etiqueta = paste0(
          "Ordenados por cercania a tu nacimiento (", 
          day(lista_inputs_d()$fecha), " de ", meses[month(lista_inputs_d()$fecha)], " de ",
          year(lista_inputs_d()$fecha), ")")
      } else if (lista_inputs_d()$tipo_orden == "Cercanía a tu edad"){
        ancho = "180px"
        
        e = interval(as.Date(lista_inputs_d()$fecha), as.Date(ahora))
        e = as.period(e, unit = "years")
        e = paste(year(e), "años,", month(e), "meses,", day(e), "días")
        
        etiqueta = paste0("Ordenados por cercania a tu edad (", e, ")")
      } else {
        ancho = "120px"
        etiqueta = paste0("Ordenados por ", lista_inputs_d()$tipo_orden)
      }
      
      r = append(r, list(
        actionButton(
          inputId = paste0("tipo_orden", "Cancelar"),
          label = tags$span(
            style = "
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
              ",
            span(etiqueta),
            icon("close")
          ),
          style = paste0("
              color: #fff;
              background-color: red;
              border-color: #fff;
              width: ", ancho, ";
              padding: 5px;
              margin: 5px;
            ")
        )
      ))
      
    }
    
    if (lista_inputs_d()$FiltrarEdad){
      
      if (lista_inputs_d()$edad1 == lista_inputs_d()$edad2){
        etiqueta = paste(lista_inputs_d()$edad1, "años")
      } else {
        etiqueta = paste(
          "De", lista_inputs_d()$edad1, "a", lista_inputs_d()$edad2, "años")
      }
      
      r = append(r, list(
        actionButton(
          inputId = paste0("Edad", "Cancelar"),
          label = tags$span(
            style = "
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
              ",
            span(etiqueta),
            icon("close")
          ),
          style = "
              color: #fff;
              background-color: red;
              border-color: #fff;
              width: 120px;
              padding: 5px;
              margin: 5px;
            "
        )
      ))
    }
    
    if (lista_inputs_d()$FiltrarNacim){
      
      if (lista_inputs_d()$nac1 == lista_inputs_d()$nac2){
        etiqueta = paste("Nacido/as en", lista_inputs_d()$nac1)
      } else {
        etiqueta = paste(
          "Nacido/as entre", lista_inputs_d()$nac1, "y", lista_inputs_d()$nac2)
      }
      
      r = append(r, list(
        actionButton(
          inputId = paste0("Nacim", "Cancelar"),
          label = tags$span(
            style = "
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
              ",
            span(etiqueta),
            icon("close")
          ),
          style = "
              color: #fff;
              background-color: red;
              border-color: #fff;
              width: 120px;
              padding: 5px;
              margin: 5px;
            "
        )
      ))
    }
    
    if (lista_inputs_d()$FiltrarFall & lista_inputs_d()$estado != "Vivos"){
      
      if (lista_inputs_d()$fac2 == year(today())){
        etiqueta = paste("Vivo/as y fallecido/as luego de", lista_inputs_d()$fac1)
      } else if (lista_inputs_d()$fac1 == lista_inputs_d()$fac2){
        etiqueta = paste("Fallecido/as en", lista_inputs_d()$fac1)
      } else {
        etiqueta = paste(
          "Fallecido/as entre", lista_inputs_d()$fac1, "y", lista_inputs_d()$fac2)
      }
      
      r = append(r, list(
        actionButton(
          inputId = paste0("Fall", "Cancelar"),
          label = tags$span(
            style = "
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
              ",
            span(etiqueta),
            icon("close")
          ),
          style = "
              color: #fff;
              background-color: red;
              border-color: #fff;
              width: 120px;
              padding: 5px;
              margin: 5px;
            "
        )
      ))
    }
    
    if (lista_inputs_d()$FiltrarCumple){
      
      if (lista_inputs_d()$mes1 == lista_inputs_d()$mes2 & 
          lista_inputs_d()$dia1 == lista_inputs_d()$dia2){
        etiqueta = paste("Cumple años el", lista_inputs_d()$dia1, "de", lista_inputs_d()$mes1)
      } else {
        etiqueta = paste(
          "Cumple años entre el", lista_inputs_d()$dia1, "de", lista_inputs_d()$mes1, "y el",
          lista_inputs_d()$dia2, "de", lista_inputs_d()$mes2)
      }
      
      r = append(r, list(
        actionButton(
          inputId = paste0("Cumple", "Cancelar"),
          label = tags$span(
            style = "
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
              ",
            span(etiqueta),
            icon("close")
          ),
          style = "
              color: #fff;
              background-color: red;
              border-color: #fff;
              width: 150px;
              padding: 5px;
              margin: 5px;
            "
        )
      ))
    }
    
    if (lista_inputs_d()$FiltrarFechaMuerte & lista_inputs_d()$estado != "Vivos"){
      
      if (lista_inputs_d()$mes3 == lista_inputs_d()$mes4 & 
          lista_inputs_d()$dia3 == lista_inputs_d()$dia4){
        etiqueta = paste("Fallecido/a el", lista_inputs_d()$dia3, "de", lista_inputs_d()$mes3)
      } else {
        etiqueta = paste(
          "Fallecido/a entre el", lista_inputs_d()$dia3, "de", lista_inputs_d()$mes3, "y el",
          lista_inputs_d()$dia4, "de", lista_inputs_d()$mes4)
      }
      
      r = append(r, list(
        actionButton(
          inputId = paste0("FechaMuerte", "Cancelar"),
          label = tags$span(
            style = "
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
              ",
            span(etiqueta),
            icon("close")
          ),
          style = "
              color: #fff;
              background-color: red;
              border-color: #fff;
              width: 150px;
              padding: 5px;
              margin: 5px;
            "
        )
      ))
    }
    
    if (length(r) > 0){
      r = list(
        fluidRow(
          column(
            width = 12,
            div(
              style = "
                display: flex; align-items: center; justify-content: flex-start; 
                gap: 6px; overflow-x: auto;",
              r
            )
          )
        )
      )
    }
    
    return(r)
    
  })
  
  observeEvent(input$estadoCancelar, {rv$estado = "Todos"})
  observeEvent(input$signoCancelar, {rv$signo = "Todos"})
  observeEvent(input$generoCancelar, {rv$genero = "Todos"})
  observeEvent(input$profesionCancelar, {rv$profesion = "Todos"})
  observeEvent(input$paisCancelar, {rv$pais = "Todos"})

  observeEvent(input$EdadCancelar, {
    
    rv$FiltrarEdad = FALSE
    rv$edad1 = 0
    rv$edad2 = 150
    
  })
  
  observeEvent(input$NacimCancelar, {
    
    rv$FiltrarNacim = FALSE
    rv$nac1 = 1900
    rv$nac2 = year(today())
    
  })
  
  observeEvent(input$FallCancelar, {
    
    rv$FiltrarFall = FALSE
    rv$fac1 = 1900
    rv$fac2 = year(today())
    
  })
  
  observeEvent(rv$estado, {
    
    if (rv$estado == "Vivos"){
      rv$FiltrarFall = FALSE
      rv$fac1 = 1900
      rv$fac2 = year(today())
      rv$FiltrarFechaMuerte = FALSE
      rv$dia3 = 1
      rv$mes3 = "enero"
      rv$dia4 = 31
      rv$mes4 = "diciembre"
    }
    
  })
  
  observeEvent(input$tipo_ordenCancelar, {rv$tipo_orden = "Menciones en Wikipedia"})
  
  observeEvent(rv$tipo_orden,{
    
    if (!rv$tipo_orden %in% c("Cercanía a tu nacimiento", "Cercanía a tu edad")){
      
      rv$relacion = "Todos"
      
    }
    
  })
  
  observeEvent(input$CumpleCancelar, {
    
    rv$FiltrarCumple = FALSE
    rv$dia1 = "1" 
    rv$mes1 = "enero"
    rv$dia2 = "31"
    rv$mes2 = "diciembre"
    
  })
  
  observeEvent(input$FechaMuerteCancelar, {
    
    rv$FiltrarFechaMuerte = FALSE
    rv$dia3 = "1" 
    rv$mes3 = "enero"
    rv$dia4 = "31"
    rv$mes4 = "diciembre"
    
  })
  
  ## Output ----
  
  output$EdadActual = renderText({
    
    e = interval(as.Date(lista_inputs_d()$date), as.Date(ahora))
    
    e = as.period(e, unit = "years")
    
    r = paste(year(e), "años,", month(e), "meses,", day(e), "días")
    
    return(r)
    
  })
  
  output$tabla = renderUI({
    
    r = tablaEdades() 
    
    if (nrow(tablaEdades()) > 0){
      r = r %>% mutate(num = 1:n()) %>% slice(rv$datosAct:(rv$datosAct + 5))
    } else {
      r = r %>% mutate(num = numeric())
    }
    
    try({
      
      r = r %>%
        select(
          num, titulo, descripcionCorta, Nacimiento, Fallecimiento, edad, genero, signo) %>%
      set_names(
        "n","Título", "Descripción Corta", "Nacimiento", "Fallecimiento", "Edad", "Género", "Signo")
    })

    try({

      r = r %>%
        mutate(Descripción = glue::glue(
          '<button id="custom_btn1" ',
          'onclick="
            var c = Shiny.shinyapp.$inputValues.contador || 0;
            Shiny.setInputValue(\'contador\', c + 1, {{priority:\'event\'}});
            Shiny.onInputChange(\'button_des\', \'{n}\')
            "',
          '> Descripción </button>'))


    }, silent = TRUE)
    
    if (nrow(r) > 0) {
      tablaHTML = paste(apply(
        r, 1, 
        function(x) paste0(
          "<tr>", 
          paste0("<td>", ifelse(is.na(x), "", x), "</td>", collapse = " "), 
          "</tr>")), 
        collapse = "")
    } else {
      tablaHTML = paste0(
        "<td colspan=", ncol(r), 
        '><div style = "display: flex; justify-content: center;"><i>
        No se encontró ningún resultado</i></div></td>') 
      
    }

    HTML(paste0(
      '<table>
      <thead>
        <tr>',
          paste0('<th>', variable.names(r), '</th>', collapse = ' '),
        '</tr>
      </thead>
      <tbody>', tablaHTML,
      '</tbody>
      </table>'))
    
  })  
  
  output$Conteo = renderText({
    
    paste0(
      "Mostrando ", rv$datosAct, "-", 
      min(nrow(tablaEdades()), rv$datosAct + 5), 
      " de ", as.character(nrow(tablaEdades()))
    )
    
  })
  
  ## Descripcion ----
  
  output$Descripcion = renderUI({
    
    ListaEdad = tablaEdades() %>% slice(as.numeric(input$button_des))
    
    r = list(HTML(paste0(
      '<p style="font-size: 20px;font-weight: bold;">', ListaEdad$titulo, '</p>',
      ListaEdad$descripcion
    )))
    
    return(r)
    
  })
  
  observeEvent(input$contador, {
    
    f7Popup(
      id = "popup1",
      title = "",
      uiOutput("Descripcion")
    )
    
  })
  
  ## Informacion ----
  
  RvInfo = reactiveVal(FALSE)
  
  output$popup_info = renderUI({
    
    if (RvInfo()){
      PopUpRender( 
        id = "popup_info", 
        header = "Acerca de ...", 
        contenido = list(
          h3("Información obtenida mediante scrapping sobre Wikipedia."),
          h3(paste("Última actualización:", actualizacion)),
          InformacionContacto
        ),
        listaBotones = list(
          list(id = "close_popup_info", label = "Cerrar")
        )) 
    }
    
  })
  
  observeEvent(input$Info, {
    RvInfo(TRUE)
  })
  
  observeEvent(input$close_popup_info, {
    RvInfo(FALSE)
  })
  
}