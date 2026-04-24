
library(shiny)
library(shinycssloaders)


ui = f7Page(
  tags$head(
    tags$style(HTML("
      document.addEventListener('panel:closed', function (e) {
        if (e.detail.panel.side === 'left') {
          Shiny.setInputValue(
            'left_panel_closed',
            Date.now(),
            {priority: 'event'}
          );
        }
      });
      
      .compact-block {
        margin-bottom: 6px !important;
        margin-top: 6px !important;
      }
      
      .table-scroll {
        border: 2px solid #000;
        max-height: 400px;
        width: 100%;
        overflow-x: auto;
        overflow-y: auto;
      }
      
      .table-scroll table {
        table-layout: fixed;
        border-collapse: collapse;
        border: 2px solid #000;
        font-family: arial, sans-serif;
        width: 1000px; /* 👈 más grande que el contenedor */
      }


      .table-scroll thead th {
        position: sticky;
        top: 0;
        background: #4f81bd;
        color: #fff;
        z-index: 3;
        font-size: 12px;
        text-align: left;
        border-bottom: 2px solid #000;
      }
      
      .table-scroll td,
      .table-scroll th {
        border: 1px solid #000;
        padding: 8px;
        font-size: 12px;
        text-align: left;
        background: white;
      }
      
      .table-scroll th:nth-child(1),
      .table-scroll td:nth-child(1) { width: 30px; }

      .table-scroll th:nth-child(2),
      .table-scroll td:nth-child(2) { width: 100px; }
      
      /* Columna 1 */
      .table-scroll th:nth-child(1),
      .table-scroll td:nth-child(1) {
        position: sticky;
        left: 0;
        background: white;
        z-index: 4;
      }

      /* Columna 2 */
      .table-scroll th:nth-child(2),
      .table-scroll td:nth-child(2) {
        position: sticky;
        left: 30px; /* = ancho columna 1 */
        background: white;
        z-index: 4;
        border-right: 2px solid #000;
      }

      .table-scroll thead th:nth-child(1),
      .table-scroll thead th:nth-child(2) {
        z-index: 6;
        background: #4f81bd;
      }

      .table-scroll th:nth-child(3),
      .table-scroll td:nth-child(3) { width: 120px; }

      .table-scroll th:nth-child(4),
      .table-scroll td:nth-child(4) { width: 80px; }

      .table-scroll th:nth-child(5),
      .table-scroll td:nth-child(5) { width: 80px; }

      .table-scroll th:nth-child(6),
      .table-scroll td:nth-child(6) { width: 120px; }

    "))
  ),
  options = list(
    dark = FALSE, 
    filled = FALSE,
    theme = "md",  # Tema Material Design
    color = "#007aff"  # Color azul iOS
  ),
  title = "AppFamosos",
  f7SingleLayout(
    style = "overflow: hidden !important;",
    uiOutput("popup_inicio"), 
    uiOutput("popup_cumple"), uiOutput("popup_fechamuerte"), uiOutput("popup_edadmuertos"), 
    uiOutput("popup_profsigno"), uiOutput("popup_generopais"), uiOutput("popup_cercanianac"), 
    uiOutput("popup_instrucciones"),
    uiOutput("popup_ui"), uiOutput("popup_ui_errores"), uiOutput("popup_orden"), 
    navbar = f7Navbar(
      title = "Consulta de Personas",
      hairline = TRUE,
      leftPanel = FALSE,
      rightPanel = FALSE,
      
      subNavbar = f7SubNavbar(
        div(
          style = "
            display: flex;
            align-items: center;
            gap: 6px;
            width: 100%;
          ",
          h3("Filtrar:", style = "margin: 0 8px 0 0; white-space: nowrap;"),
          div(
            style = "flex: 1;",
            f7Text(
              inputId = "filtro",
              label = "",
              placeholder = "Buscar…"
            )
          ),
          div(
            style = "display: flex; flex-direction: column; gap: 2px;",
            actionButton(
              style = "width: 120px;",
              inputId = "VerMenu",
              label = "Menú Principal",
              class = "btn-xs"
            ),
            actionButton(
              style = "width: 120px;",
              inputId = "VerFiltros",
              label = "Más filtros",
              class = "btn-xs"
            ),
            actionButton(
              style = "width: 120px;",
              inputId = "VerOrden",
              label = "Ordenar",
              class = "btn-xs"
            )
          )
        )
      )
    ),
    f7Card(
      style = "overflow: hidden !important;",
      uiOutput("controlesCancelar") ,
      withSpinner(
        div(
          style = "max-height: 50vh; overflow-y: auto;",
          class = "table-scroll",
          uiOutput("tabla")
        ),
        type = 4,
        color = "#007aff"
      ),
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "
                flex: 0 0 50%;
                display: flex;
                justify-content: flex-start;
                gap: 6px;
              ",
              h5(textOutput("Conteo"))
            ),
            div(
              style = "
                flex: 0 0 50%;
                display: flex;
                justify-content: flex-end;
                gap: 6px;
              ",
              actionButton(
                inputId = "full_atras",
                label ="", 
                icon = icon("angle-double-left"), 
                size = "sm"),
              actionButton(
                inputId = "atras", 
                label = "", 
                icon = icon("angle-left"), 
                size = "sm"),
              actionButton(
                inputId = "adelante", 
                label = "", 
                icon = icon("angle-right"), 
                size = "sm"),
              actionButton(
                inputId = "full_adelante", 
                label = "", 
                icon = icon("angle-double-right"),
                size = "sm")
            )
          )
        )
      )
    ),
    toolbar = f7Toolbar(
      position = "bottom",
      div(
        style = "
          display: flex;
          align-items: center;
          width: 100%;
        ",
        actionButton(
          inputId = "Info",
          label = "Acerca de",
          size = "sm",
          style = "
            width: 180px;
            margin-left: auto;
            margin-right: 8px;
            "
        ),
        uiOutput("popup_info")
      )
    )
  )
)
