
library(lubridate)

# Parametros de actualizacion
# Se recomienda que la primera actualizacion sea de minimo 5 horas

ActualizarTodo = TRUE ### Muestra si se quieren actualizar sucesos recientes
TiempoMaximoActualizacion = 2000 # en segundos. 
TiempoMaximoTotal = 3600 #en segundos

## Referencias obligatorias ----

# paginas en donde se espera un contenido importante al que se quiera dar prioridad


refForzadas = c(
  "Anexo:Presidentes_de_la_Nación_Argentina",
  "Anexo:Jefes_de_Estado_y_de_Gobierno_actuales",
  paste0("Anexo:Argentina_en_", 2015:year(today())),
  as.character(2000:(year(today()) - 1)),
  as.character(year(today()) + 1))

## Inicio ----

source("proceso/funciones.R", encoding = "UTF-8")

try(load("data/ListaReferencias.RData"))

t = proc.time()

tiempo = TiempoMaximoActualizacion

if (!"lista" %in% ls(envir = .GlobalEnv)){
  
  lista = list()
  
  j = BuscarReferencias("2026", marcar = TRUE)
  
  lista$listaReferencias = data.frame(
    nombre = j$referencias, 
    n = 1, 
    consultado = FALSE, 
    Descrito = FALSE, 
    EsPersona = NA, 
    origen = "2026",
    agregado = Sys.time())
  
  ## Evitar paginas largas
  
  lista$listaReferencias = bind_rows(lista$listaReferencias, data.frame(
    nombre = c(
      "Categor%C3%ADa:Hombres", "Categor%C3%ADa:Mujeres", "Categor%C3%ADa:Personas_vivas",
      "Categoría:Hombres", "Categoría:Mujeres", "Categoría:Personas_vivas"), 
    n = 1, 
    consultado = TRUE, 
    Descrito = FALSE, 
    EsPersona = FALSE, 
    origen = "Categoría:Personas",
    agregado = Sys.time()))
  
  j$referencias = NULL
  
  lista$listadoCaracteristicas =  bind_rows(
    data.frame(
      pagina = character(),
      titulo = character(),
      descripcion = character(),
      Nombre = character(),
      Nacimiento = character(),
      Fallecimiento = character(),
      Nacimiento1 = character(),
      Fallecimiento1 = character(),
      descripcionCorta = character(),
      Genero = character(),
      categorias = character(),
      Nacionalidad = character()
    ),
    cbind.data.frame(as.data.frame(j), data.frame(agregadoCaracteristica = Sys.time()))
  ) 
  
  save(lista, file = "data/ListaReferencias.RData")
  
}

## Crear Lista Personas (se ejecuta una vez, tarda mucho) ----

try(load(file = "data/ListaPersonas.RData"))

if (!"listaPersonas" %in% ls(envir = .GlobalEnv)){
  
  listaGeneros = c(
    "Categoría:Hombres", 
    "Categoría:Mujeres", 
    "Categoría:Figuras_históricas_con_identidad_de_género_ambigua_o_en_disputa",
    "Categoría:Personas_intersexo",
    "Categoría:Hombres_intersexo",
    "Categoría:Mujeres_intersexo",
    "Categoría:Personas_no_binarias",
    "Categoría:Hombres_transgénero",
    "Categoría:Mujeres_transgénero",
    "Categoría:Personas_no_categorizadas_por_sexo"
  )
  
  listaPersonas = list()
  
  t = proc.time()
  
  for (i in listaGeneros){
    
    cat("\nInicio", i, ":", convertirHoras((proc.time() - t)[3]), "\n")
    
    listaPersonas = append(
      listaPersonas, 
      list(BuscarReferencias(
        nombre_pagina = i, AgregarReferencias = TRUE, 
        AgregarDatos = FALSE, marcar = TRUE, imprimircat = TRUE)))
    
    cat("\nTotal", i, ":", convertirHoras((proc.time() - t)[3]), "\n")
    
  }
  
  for (l in listaPersonas){
    
    lista$listaReferencias = bind_rows(
      lista$listaReferencias,
      data.frame(
        nombre = l$referencias[!l$referencias %in% lista$listaReferencias$nombre], 
        n = 1, 
        consultado = FALSE, 
        Descrito = FALSE, 
        EsPersona = TRUE, 
        origen = l$titulo,
        agregado = Sys.time())
    )
    
    
    lista$listaReferencias = lista$listaReferencias %>%
      mutate(EsPersona = case_when(
        !is.na(EsPersona) ~ EsPersona,
        nombre %in% l$referencias ~ TRUE,
        TRUE ~ NA
      ))
  }
  
  save(listaPersonas, file = "data/ListaPersonas.RData")
  
}

## Proceso ----

if (ActualizarTodo){
  
  lista = ActualizarDescripciones(
    lista = lista, nombre_pagina = as.character(year(today())), tiempo = 4000, 
    soloPersonas = TRUE)
  
  cat("\n\nTotal", as.character(year(today())), ":", 
      convertirHoras((proc.time() - t)[3]), "\n")
  cat("\nCantidad Referencias:", nrow(lista$listaReferencias))
  cat("\nCantidad Caracteristicas:", nrow(lista$listadoCaracteristicas))
  cat('\nCantidad Personas:', 
      lista$listadoCaracteristicas %>% 
        filter(!is.na(descripcion), !is.na(Nacimiento)) %>% nrow(.))
  cat("\n\n")
  
  
  meses = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
            "septiembre", "octubre", "noviembre", "diciembre")
  
  mesAhora = paste0(meses[month(today())], "_de_", year(today()))
  mesAntes = paste0(meses[month(today() - months(1))], "_de_", year(today() - months(1)))
  
  lista = ActualizarDescripciones(
    lista = lista, 
    nombre_pagina = paste0("Anexo:Fallecidos_en_", mesAhora), 
    tiempo = 4000, 
    soloPersonas = TRUE)
  
  cat("\n\nTotal", paste0("Anexo:Fallecidos_en_", mesAhora), ":", 
      convertirHoras((proc.time() - t)[3]), "\n")
  cat("\nCantidad Referencias:", nrow(lista$listaReferencias))
  cat("\nCantidad Caracteristicas:", nrow(lista$listadoCaracteristicas))
  cat('\nCantidad Personas:', 
      lista$listadoCaracteristicas %>% 
        filter(!is.na(descripcion), !is.na(Nacimiento)) %>% nrow(.))
  cat("\n\n")
  
  lista = ActualizarDescripciones(
    lista = lista, 
    nombre_pagina = paste0("Anexo:Fallecidos_en_", mesAntes), 
    tiempo = 4000, 
    soloPersonas = TRUE)
  
  cat("\n\nTotal", paste0("Anexo:Fallecidos_en_", mesAntes), ":", 
      convertirHoras((proc.time() - t)[3]), "\n")
  cat("\nCantidad Referencias:", nrow(lista$listaReferencias))
  cat("\nCantidad Caracteristicas:", nrow(lista$listadoCaracteristicas))
  cat('\nCantidad Personas:', 
      lista$listadoCaracteristicas %>% 
        filter(!is.na(descripcion), !is.na(Nacimiento)) %>% nrow(.))
  cat("\n\n")
  
}

a = length(refForzadas)

while (TRUE){
  
  if ((proc.time() - t)[3] > tiempo) break
  
  ref = refForzadas[a]
  
  cantDesc = nrow(lista$listaReferencias)
  
  try({
    
    lista = ForzarNuevas(lista, nueva_referencia = ref, tiempo = 300)
    
  }, silent = TRUE)
  
  if (nrow(lista$listaReferencias) == cantDesc) refForzadas = refForzadas[-a]
  
  cat("\n\nTotal", ref, ":", convertirHoras((proc.time() - t)[3]), "\n")
  cat("\nCantidad Referencias:", nrow(lista$listaReferencias))
  cat("\nCantidad Caracteristicas:", nrow(lista$listadoCaracteristicas))
  cat('\nCantidad Personas:', 
      lista$listadoCaracteristicas %>% 
        filter(!is.na(descripcion), !is.na(Nacimiento)) %>% nrow(.))
  cat("\n\n")
  
  if (length(refForzadas) == 0) break
  
  a = a - 1
  
  if (a == 0) a = length(refForzadas)
  
}

save(lista, file = "data/ListaReferencias.RData")

tiempo = TiempoMaximoTotal - (proc.time() - t)[3]

t = proc.time()

listaMujeres = listaPersonas %>% 
  keep(sapply(listaPersonas, function(x) x$titulo == "Categoría:Mujeres"))

listaMujeres = listaMujeres[[1]]$referencias

while (TRUE) {
  
  if ((proc.time() - t)[3] > tiempo) break
  
  lista = AgregarReferencias(lista, tiempo = 660, priorizarPersonas = TRUE)
  
  cat("\n\nFin Agregar Referencias.")
  cat("\nCantidad Referencias:", nrow(lista$listaReferencias))
  cat("\nCantidad Caracteristicas:", nrow(lista$listadoCaracteristicas))
  cat('\nCantidad Personas:', 
      lista$listadoCaracteristicas %>% filter(!is.na(descripcion), !is.na(Nacimiento)) %>% nrow(.))
  cat("\nTiempo:", convertirHoras((proc.time() - t)[3]), "\n\n")
  
  save(lista, file = "data/ListaReferencias.RData")
  cat("Hora:", as.character(as_datetime(Sys.time())), "\n")
  cat("Modificación:", as.character(as_datetime(file.mtime("data/ListaReferencias.RData"))), "\n")
  
  if ((proc.time() - t)[3] > tiempo) break
  
  mujeresNoCons = lista$listaReferencias %>% 
    filter(nombre %in% listaMujeres, !consultado) %>%
    arrange(desc(n), sample.int(nrow(.)))
  
  lista = ForzarNuevas(
    lista, nueva_referencia = mujeresNoCons$nombre, tiempo = 600, AgregarRef = FALSE)
  
  cat("\n\nFin Agregar Referencias Mujeres.")
  cat("\nN máximo mujeres:", max(mujeresNoCons$n, na.rm = TRUE))
  cat("\nCantidad Referencias:", nrow(lista$listaReferencias))
  cat("\nCantidad Caracteristicas:", nrow(lista$listadoCaracteristicas))
  cat('\nCantidad Personas:', 
      lista$listadoCaracteristicas %>% 
        filter(!is.na(descripcion), !is.na(Nacimiento)) %>% nrow(.))
  cat("\nTiempo:", convertirHoras((proc.time() - t)[3]), "\n\n")
  
  save(lista, file = "data/ListaReferencias.RData")
  cat("Hora:", as.character(as_datetime(Sys.time())), "\n")
  cat("Modificación:", as.character(as_datetime(file.mtime("data/ListaReferencias.RData"))), "\n")
  
  if ((proc.time() - t)[3] > tiempo) break
  
  if (nrow(lista$listaReferencias) == nrow(lista$listadoCaracteristicas)) break
  
  lista = AgregarReferencias(lista, tiempo = 700, priorizarPersonas = FALSE)
  
  cat("\n\nFin Agregar Referencias.")
  cat("\nCantidad Referencias:", nrow(lista$listaReferencias))
  cat("\nCantidad Caracteristicas:", nrow(lista$listadoCaracteristicas))
  cat('\nCantidad Personas:', 
      lista$listadoCaracteristicas %>% 
        filter(!is.na(descripcion), !is.na(Nacimiento)) %>% nrow(.))
  cat("\nTiempo:", convertirHoras((proc.time() - t)[3]), "\n\n")
  
  save(lista, file = "data/ListaReferencias.RData")
  cat("Hora:", as.character(as_datetime(Sys.time())), "\n")
  cat("Modificación:", as.character(as_datetime(file.mtime("data/ListaReferencias.RData"))), "\n")
  
  if ((proc.time() - t)[3] > tiempo) break
  
}

save(lista, file = "data/ListaReferencias.RData")

if (ActualizarTodo){
  
  actualizacion = substr(Sys.time(), 1, 19)
  
  save(actualizacion, file = "data/actualizacion.RData")
  save(actualizacion, file = "app/data/actualizacion.RData")
  
}

save.image()

## Analisis ----

resumenDesempenio = lista$listaReferencias %>%
  arrange(desc(n)) %>%
  group_by(origen, agregado) %>% 
  summarise(cantidadAgregados = n(), ejemplo = nombre[1]) %>%
  ungroup() %>% as.data.frame() %>%
  arrange(agregado) %>%
  mutate(acumulados = cumsum(cantidadAgregados), ciclo = 1:n())


ggplot(data = resumenDesempenio) +
  geom_line(mapping = aes(x = agregado, y = acumulados))


lista$listaReferencias %>%
  mutate(Consultado = ifelse(consultado, "SI", "NO")) %>%
  filter(n < 50, n > 5) %>%
  ggplot() +
  geom_bar(
    mapping = aes(x = n, fill = Consultado), 
    position = position_dodge(width = 0.9),  # Para poner las barras lado a lado
    stat = "count",
    color = "black",  # Borde negro para las barras
    width = 0.7       # Ancho de las barras
  ) +
  scale_fill_manual(
    values = c("NO" = "red", "SI" = "green"),  # Colores específicos
    name = "¿Consultado?"  # Título de la leyenda
  ) +
  labs(
    x = "Valor de n",
    y = "Cantidad",
    title = "Distribución por valor de n y estado de consulta"
  ) +
  theme_minimal()


