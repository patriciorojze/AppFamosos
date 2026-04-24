
library(tidyverse)
library(lubridate)


source("proceso/funciones.R", encoding = "UTF-8")
try(load("data/ListaReferencias.RData"))

TiempoMaximoOrden = 300
FechaMaximaOrden = "2026-02-28 00:00:00"

### Funciones Auxiliares ----

EdadTexto = function(anio, mes, dia){
  
  textoanio = case_when(
    is.na(anio) ~ "",
    anio == 1 ~ "año, ",
    anio > 1 ~ "años, ",
    TRUE ~ ""
  )
  
  textoanio2 = case_when(
    is.na(anio) ~ "",
    anio > 0 ~ paste0(anio, " "),
    TRUE ~ ""
  )
  
  textomeses = case_when(
    is.na(mes) ~ "",
    mes == 1 ~ "mes, ",
    mes > 1 ~ "meses, ",
    TRUE ~ ""
  )
  
  textomeses2 = case_when(
    is.na(mes) ~ "",
    mes > 0 ~ paste0(mes, " "),
    TRUE ~ ""
  )
  
  textodias = case_when(
    is.na(dia) ~ "",
    dia == 1 ~ "día, ",
    dia > 1 ~ "días, ",
    TRUE ~ ""
  )
  
  textodias2 = case_when(
    is.na(dia) ~ "",
    dia > 0 ~ paste0(dia, " "),
    TRUE ~ ""
  )
  
  r = paste0(textoanio2, textoanio, textomeses2, textomeses, textodias2, textodias)
  r = ifelse(substr(r, nchar(r)- 1, nchar(r)) == ", ", substr(r, 1, nchar(r) - 2), r)
  
  return(r)
  
}

AgregarBlank = function(x, class = TRUE) {
  gsub(
    '<a(?![^>]*target=)([^>]*href=)',
    '<a target="_blank" class = "link external" \\1',
    x,
    perl = TRUE
  )
}

CalcularSigno = function(x){
  
  m = month(x)
  d = day(x)
  
  case_when(
    m == 3 & d >= 21 | m == 4 & d < 20 ~ "Aries",
    m == 4 & d >= 20 | m == 5 & d < 21 ~ "Tauro",
    m == 5 & d >= 21 | m == 6 & d < 21 ~ "Géminis",
    m == 6 & d >= 21 | m == 7 & d < 23 ~ "Cáncer",
    m == 7 & d >= 23 | m == 8 & d < 23 ~ "Leo",
    m == 8 & d >= 23 | m == 9 & d < 23 ~ "Virgo",
    m == 9 & d >= 23 | m == 10 & d < 23 ~ "Libra",
    m == 10 & d >= 23 | m == 11 & d < 22 ~ "Escorpio",
    m == 11 & d >= 22 | m == 12 & d < 22 ~ "Sagitario",
    m == 12 & d >= 22 | m == 1 & d < 20 ~ "Capricornio",
    m == 1 & d >= 20 | m == 2 & d < 19 ~ "Acuario",
    m == 2 & d >= 19 | m == 3 & d < 21 ~ "Piscis",
    TRUE ~ ""
  )
  
}

BorrarSup = function(x){
  gsub(
    '<sup[^>]*id="cite[^"]*"[^>]*>.*?</sup>',
    '',
    x,
    perl = TRUE
  )
}

CambiarMayuscula = function(x){
  ifelse(
    is.na(x), x,
    paste0(
      ifelse(nchar(x) > 0, toupper(substr(x,1,1)), ""),
      ifelse(nchar(x) > 1, substr(x,2,nchar(x)), "")
    ))
}

### Listas auxiliares ----

# Presidentes

refPr = c()

for (i in c(
  "Categoría:Jefes_de_Gobierno", 
  "Categoría:Presidentes_por_país", 
  "Categoría:Primeros_ministros_por_país")){
  
  Jefes = BuscarReferencias(nombre_pagina = i, AgregarDatos = FALSE, Ver = "cuerpo")
  ref1 = Jefes$referencias
  refPr = c(refPr, ref1)
  
}

refPr = refPr[regexpr("Categor%C3%ADa:", refPr) == 1]
refPr = c(
  refPr,
  "Categor%C3%ADa:Presidentes_de_Argentina_que_fueron_derrocados_por_un_golpe_de_Estado",
  "Categor%C3%ADa:Presidentes_de_Costa_Rica_que_fueron_derrocados_por_un_golpe_de_Estado",
  "Categor%C3%ADa:Presidentes_de_Ecuador_que_fueron_derrocados_por_un_golpe_de_Estado",
  "Categor%C3%ADa:Presidentes_del_Gobierno_de_Espa%C3%B1a",
  "Categor%C3%ADa:Presidentes_del_Gobierno_de_Espa%C3%B1a_durante_la_dictadura_franquista",
  "Categor%C3%ADa:Presidentes_del_Gobierno_de_Espa%C3%B1a_en_democracia",
  "Categor%C3%ADa:Presidentes_del_Gobierno_de_Espa%C3%B1a_durante_el_reinado_de_Isabel_II",
  "Categor%C3%ADa:Presidentes_del_Consejo_de_Ministros_de_Espa%C3%B1a_durante_la_Restauraci%C3%B3n_borb%C3%B3nica",
  "Categor%C3%ADa:Jefes_de_Gobierno_durante_el_Sexenio_Democr%C3%A1tico",
  "Categor%C3%ADa:Presidentes_del_Gobierno_durante_la_Segunda_Rep%C3%BAblica_Espa%C3%B1ola",
  "Categor%C3%ADa:Presidentes_de_Juntas_de_Gobierno_de_Chile"
)

# Instrumentistas

j = BuscarReferencias(
  "Categoría:Instrumentistas", AgregarDatos = FALSE, Ver = "cuerpo")

refInstrumentos = j$referencias
refInstrumentos = refInstrumentos[regexpr("Categor%C3%ADa:", refInstrumentos) == 1]

# Deportistas

j = BuscarReferencias(
  "Categoría:Deportistas_por_deporte", AgregarDatos = FALSE, Ver = "cuerpo")

refDeportistas = j$referencias
refDeportistas = refDeportistas[regexpr("Categor%C3%ADa:", refInstrumentos) == 1]

refDeportistas = refDeportistas[
  !refDeportistas %in% c("Categor%C3%ADa:Ajedrecistas", "Categor%C3%ADa:Animadoras",
                         "Categor%C3%ADa:Jugadores_de_damas")]

# Medicos

j = BuscarReferencias(
  "Categoría:Médicos_por_especialidad", AgregarDatos = FALSE, Ver = "cuerpo")

refMedicos = j$referencias
refMedicos = refMedicos[regexpr("Categor%C3%ADa:", refMedicos) == 1]

# Paises

TextoPaises = try(suppressWarnings(scan(
  "data/paises.txt",
  what = "c", sep = "\n",
  encoding = "UTF-8", quiet = TRUE)), silent = TRUE)

TextoPaises = TextoPaises[TextoPaises != ""]

ListaNacionalidades = list()

for (i in 1:length(TextoPaises)){
  
  opciones = strsplit(TextoPaises[i], split = "\\|")[[1]]
  opciones = trimws(opciones)
  opciones = unique(opciones)
  
  nombreL = substr(opciones[1], 1, 3)
  if (nombreL %in% names(ListaNacionalidades)) nombreL = paste0(nombreL, i)
  
  ListaNacionalidades[[nombreL]] = list(
    Nombre = opciones[1],
    Categorias = opciones,
    IncluirPersonas = c(),
    ExcluirPersonas = c()
  )
  
}

nombresPaises = sapply(ListaNacionalidades, function(x) x$Nombre)
names(nombresPaises) = nombresPaises
nombresPaises["Libano"] = "el Líbano"
nombresPaises["Países Bajos"] = "los Países Bajos"
nombresPaises["Países Bajos"] = "los Países Bajos"
nombresPaises["Reino Unido"] = "el Reino Unido"
nombresPaises["República Checa"] = "la República Checa"

for (i in 1:length(ListaNacionalidades)){
  
  nombrePais = ListaNacionalidades[[i]]$Nombre
  
  nombreEmbajadas = nombresPaises[names(nombresPaises) != nombrePais]
  
  if (nombrePais != "Rusia") nombreEmbajadas = c(nombreEmbajadas, "la Unión Soviética")
  
  embajadores = paste0(
    "C:Embajadores_de_", nombreEmbajadas, "_en_", nombresPaises[nombrePais])
  
  if (nombrePais != "Vaticano"){
    embajadores = c(
      embajadores,
      paste0("C:Nuncios_apostólicos_en_", nombresPaises[nombrePais])
    )
  }
  
  if (nombrePais != "Reino Unido"){
    embajadores = c(
      embajadores,
      paste0("C:Altos_Comisionados_de_Reino_Unido_en_", nombresPaises[nombrePais])
    )
  }
  
  embajadores = c(embajadores, paste0("C:Embajadores_en_", nombresPaises[nombrePais]))
  
  embajadores = sub("de_el ", "de_", embajadores)
  
  embajadores = gsub("á", "(á|%C3%A1)", embajadores)
  embajadores = gsub("é", "(é|%C3%A9)", embajadores)
  embajadores = gsub("í", "(í|%C3%AD)", embajadores)
  embajadores = gsub("ó", "(ó|%C3%B3)", embajadores)
  embajadores = gsub("ú", "(ú|%C3%BA)", embajadores)
  embajadores = gsub(" ", "_", embajadores)
  
  ListaNacionalidades[[i]]$Excluir = embajadores
  
}

paisesPersonas = openxlsx::read.xlsx("data/correcciones/paisesPersonas.xlsx")

paisesPersonas = paisesPersonas %>% 
  filter(!is.na(Pais1)) %>% 
  mutate_if(is.character, trimws)

for (i in 1:nrow(paisesPersonas)) for (j in 1:length(ListaNacionalidades)){
  
  opcionesPais = c(
    paisesPersonas$Pais1[i], paisesPersonas$Pais2[i], paisesPersonas$Pais3[i],
    paisesPersonas$Pais4[i]
  )
  
  opcionesPais = opcionesPais[!is.na(opcionesPais)]
  
  if (ListaNacionalidades[[j]]$Nombre %in% opcionesPais){
    
    ListaNacionalidades[[j]]$IncluirPersonas = c(
      ListaNacionalidades[[j]]$IncluirPersonas, paisesPersonas$titulo[i])
    
  } else {
    
    ListaNacionalidades[[j]]$ExcluirPersonas = c(
      ListaNacionalidades[[j]]$ExcluirPersonas, paisesPersonas$titulo[i])
    
  }
  
  if (j == length(ListaNacionalidades)) cat("-")
  
}

AcademiaSueca = read.table("data/correcciones/AcademiaSueca.txt", header = TRUE)

pal = which(sapply(ListaNacionalidades, function(x) x$Nombre) == "Suecia")
ListaNacionalidades[[pal]]$ExcluirPersonas = unique(c(
  ListaNacionalidades[[pal]]$ExcluirPersonas, AcademiaSueca$titulo
))

AcademiaDanesa = read.table("data/correcciones/AcademiaDanesa.txt", header = TRUE)

pal = which(sapply(ListaNacionalidades, function(x) x$Nombre) == "Dinamarca")
ListaNacionalidades[[pal]]$ExcluirPersonas = unique(c(
  ListaNacionalidades[[pal]]$ExcluirPersonas, AcademiaDanesa$titulo
))


### Actualizacion ----

listaTitulos = merge(
  lista$listadoCaracteristicas,
  lista$listaReferencias %>% 
    group_by(nombre) %>% summarise(n = sum(n, na.rm = TRUE)) %>% as.data.frame(),
  by.x = "pagina", by.y = "nombre", all.x = TRUE
)

listaTitulos = suppressWarnings(
  listaTitulos %>% 
    group_by(titulo) %>%
    summarise(
      pagina = pagina[1], 
      n = sum(n, na.rm = TRUE), 
      Nacimiento = Nacimiento[!is.na(Nacimiento)][1],
      agregadoCaracteristica = max(agregadoCaracteristica, na.rm = TRUE)) %>%
    ungroup() %>% as.data.frame())


filtrarTitulos = (
  listaTitulos$agregadoCaracteristica < as_datetime(FechaMaximaOrden) | 
    is.infinite(listaTitulos$agregadoCaracteristica)) &
  !is.na(listaTitulos$Nacimiento)

cat("\nPor actualizar:", sum(filtrarTitulos), "\n")

listaTitulos = listaTitulos %>%
  filter(filtrarTitulos) %>%
  arrange(is.na(Nacimiento), desc(n))

if (nrow(listaTitulos) > 0){
  
  lista = ActualizarDescripciones(
    lista, nombre_pagina = listaTitulos$pagina, tiempo = TiempoMaximoOrden, 
    soloPersonas = TRUE, AgregarRef = FALSE)
  
  save(lista, file = "data/ListaReferencias.RData")
  
}

rm(listaTitulos)
  
### ListaEdades ----

listaEdades = merge(
  lista$listadoCaracteristicas, 
  lista$listaReferencias %>% select(nombre, n, agregado),
  by.x = "pagina", by.y = "nombre",
  all.x = TRUE
)

## Asignar Nacionalidad

paisesPersonas = openxlsx::read.xlsx("data/correcciones/paisesPersonas.xlsx")

paisesPersonas1 = paisesPersonas %>%
  mutate_at(
    .vars = c("Pais1", "Pais2", "Pais3", "Pais4"), 
    .funs = function(x) trimws(ifelse(is.na(x), "", x))) %>%
  mutate(Nacionalidad1 = trimws(paste(Pais1, Pais2, Pais3, Pais4, sep = ", "))) %>%
  mutate(Nacionalidad1 = trimws(gsub(",$", "", Nacionalidad1))) %>%
  mutate(Nacionalidad1 = trimws(gsub(",$", "", Nacionalidad1))) %>%
  mutate(Nacionalidad1 = trimws(gsub(",$", "", Nacionalidad1))) %>%
  select(titulo, Nacionalidad1)

listaEdades = merge(listaEdades, paisesPersonas1, by = "titulo", all.x = TRUE)

Nacimientos = scan(
  "data/paisesCiudades.txt", what = "c", encoding = "UTF-8", sep = "\n")

Nacimientos = strsplit(Nacimientos, "\\|")
Nacimientos = lapply(Nacimientos, trimws)
NacimientosPais = sapply(Nacimientos, function(x) x[1])

lugarNacimiento = paste(
  coalesce(listaEdades$Nacimiento, ""),
  coalesce(listaEdades$Nacimiento1, ""))

k = rep("", length(lugarNacimiento))

for (i in NacimientosPais){
  
  i1 = paste0("(^|[[:punct:]\\s/])(",i,")([[:punct:]\\s/]|$)")
  
  k[grepl(i1, lugarNacimiento, perl = TRUE)] = i
  
  cat("-")
  
}

k[grepl("Georgia", lugarNacimiento) & 
    grepl("Estados Unidos", lugarNacimiento)] = "Estados Unidos"       

for (i in Nacimientos){
  
  i1 = i[i != "Georgia"]
  i1 = paste0("(^|[[:punct:]\\s/])(", paste(i1, collapse = "|"), ")([[:punct:]\\s/]|$)")
  
  k[k == "" & grepl(i1, lugarNacimiento, perl = TRUE)] = i[1]
  
  cat("-")
  
}

nacDesc = rep("", nrow(listaEdades))

for (l in ListaNacionalidades){
  
  i1 = l$Categorias[l$Categorias != "Georgia"]
  i1 = paste0("(^|[[:punct:]\\s/])(", paste(i1, collapse = "|"), ")([[:punct:]\\s/]|$)")
  
  coincide = grepl(i1, listaEdades$descripcionCorta, perl = TRUE) 
  
  nacDesc[coincide] = paste(l$Nombre, nacDesc[coincide])
  
  cat("-")
}

nacDesc = gsub("\\s+", " ", nacDesc)
nacDesc = trimws(nacDesc)

listaEdades$Nacionalidad = paste(
  coalesce(listaEdades$Nacionalidad, ""), k, nacDesc
)

listaEdades$Nacionalidad = gsub("\\s+", " ", listaEdades$Nacionalidad)
listaEdades$Nacionalidad = trimws(listaEdades$Nacionalidad)

## Completar

listaEdades = listaEdades %>% filter(!is.na(Nacimiento) | !is.na(Nombre)) %>%
  mutate_at(
    .vars = c("Genero", "descripcionCorta", "categorias", "Nacionalidad"), 
    .funs = function(x) ifelse(trimws(x) %in% c("", "NA"), NA, trimws(x))) %>%
  mutate(Nacionalidad = coalesce(Nacionalidad1, Nacionalidad)) %>%
  arrange(desc(agregadoCaracteristica), desc(agregado), desc(n)) %>%
  group_by(titulo) %>%
  summarise(
    Nombre = Nombre[1],
    Nacimiento = Nacimiento[1],
    Fallecimiento = Fallecimiento[1],
    n = sum(n), 
    pagina = pagina[1], 
    descripcion = descripcion[1],
    genero = Genero[!is.na(Genero)][1],
    Nacionalidad = Nacionalidad[!is.na(Nacionalidad)][1],
    agregadoCaracteristica = agregadoCaracteristica[1],
    descripcionCorta = descripcionCorta[!is.na(descripcionCorta)][1],
    categorias = categorias[!is.na(categorias)][1]) %>% 
  ungroup() %>% as.data.frame()

listaEdades = listaEdades %>%
  mutate(
    genero = case_when(
      genero == "NA" | is.na(genero) | genero == "" ~ "No consultado", 
      TRUE ~ genero),
    EstimadoNac = grepl("(estimado en wikidata)", Nacimiento),
    EstimadoFac = grepl("(estimado en wikidata)", Fallecimiento),
    EsNacPrimeroEnero = grepl(
      "(^|[[:punct:]\\s/])1 de enero", Nacimiento, perl = TRUE),
    EsFacPrimeroEnero = grepl(
      "(^|[[:punct:]\\s/])1 de enero", Fallecimiento, perl = TRUE)) %>%
  mutate_at(
    c("Nacimiento", "Fallecimiento"), 
    function(x) gsub("&#\\d{1,4};", " ", x, perl = TRUE)) %>%
  mutate_at(
    c("Nacimiento", "Fallecimiento"), 
    function(x) gsub("\\s+", " ", x, perl = TRUE)) %>%
  mutate(
    fecha_nac = as.Date(TextoAFecha(Nacimiento)), 
    fecha_fac = as.Date(TextoAFecha(Fallecimiento))) %>%
  filter(
    !(!is.na(Fallecimiento) & is.na(fecha_fac) | !is.na(Nacimiento) & is.na(fecha_nac))) %>% 
  mutate(
    Nacimiento = paste0(
      ifelse(
        EstimadoNac & EsNacPrimeroEnero, "",
        paste0(day(fecha_nac), " de ", meses[month(fecha_nac)], " de ")),
      abs(year(fecha_nac)), 
      ifelse(year(fecha_nac) < 0, " a. C.", ""),
      ifelse(EstimadoNac, " <i>(Estimado)</i>", "")
    ),
    Fallecimiento = ifelse(
      is.na(fecha_fac), "",
      paste0(
        ifelse(
          EstimadoFac & EsFacPrimeroEnero, "",
          paste0(day(fecha_fac), " de ", meses[month(fecha_fac)], " de ")),
        abs(year(fecha_fac)), 
        ifelse(year(fecha_fac) < 0, " a. C.", ""),
        ifelse(EstimadoFac, " <i>(Estimado)</i>", "")
    ))
  ) 

ahora = Sys.Date()

listaEdades = listaEdades %>%
  mutate(edad_actual = as.period(interval(fecha_nac, ahora))) %>%
  filter(
    !is.na(fecha_nac), fecha_nac < fecha_fac | is.na(fecha_fac), 
    !is.na(fecha_fac) | edad_actual < as.period(123, unit = "years"),
    !(EstimadoNac & EsNacPrimeroEnero & (year(fecha_nac) %% 100) %in% c(0, 1, 99) |
        EstimadoFac & EsFacPrimeroEnero & (year(fecha_fac) %% 100) %in% c(0, 1, 99)),
    !(EstimadoNac & EsNacPrimeroEnero & year(fecha_nac) %% 50 == 0 |
        EstimadoFac & EsFacPrimeroEnero & year(fecha_fac) %% 50 == 0)) %>%
  mutate(
    Vivo = ifelse(is.na(fecha_fac), "SI", "NO"),
    descripcion = gsub(
      pattern = "<a href=\"/wiki", 
      replacement = "<a href=\"https://es.wikipedia.org/wiki", 
      x = descripcion),
    signo = ifelse(
      EstimadoNac & EsNacPrimeroEnero, "Desconocido",
      CalcularSigno(fecha_nac))
  ) %>%
  mutate(descripcion = paste0(
    ifelse(
      Nombre != titulo & !is.na(Nombre),
      paste0("<p><b><i>También ", case_when(
        genero %in% c("Hombre", "Hombre transgénero", "Hombre intersexo") ~ "conocido",
        genero %in% c(
          "Mujer", "Mujer transgénero", "Mujer intersexo", "Travesti") ~ "conocida",
        genero %in% c("Persona no binaria", "Intersexo", "Persona de género fluido", 
                      "Persona agénero", "Persona dos espíritus") ~ "conocidx",
        TRUE ~ "conocido/a",
      )," como</i></b>: ", Nombre, "</p>"), ""),
    AgregarBlank(descripcion), 
    "<br><br><a href=\"https://es.wikipedia.org/wiki/", 
    gsub(" ", "_", pagina), 
    "\" title = \"Descripción wikipedia\" ",
    "target=\"_blank\" class=\"link external\"  > Ver descripción completa </a>",
    ifelse(
      is.na(agregadoCaracteristica), "", 
      paste0(
        '<br><br><a style = "color:black; text-align: right; font-style: italic;">',
        ' Consultado el ', day(agregadoCaracteristica), " de ",
        meses[month(agregadoCaracteristica)], " de ", year(agregadoCaracteristica),
        "</a>"
      )))) %>%
  mutate(
    descripcion = BorrarSup(descripcion), 
    descripcionCorta = CambiarMayuscula(descripcionCorta)) %>%
  mutate(descripcion = gsub("</p><br><br>", "</p><br>", descripcion))


ahora = Sys.Date()
ahora2 = Sys.time()

listaEdades = listaEdades %>%
  mutate(
    edadN = suppressWarnings(
      ifelse(
        is.na(fecha_fac),
        as.character(as.period(interval(fecha_nac, ahora2), unit = "year")),
        as.character(as.period(interval(fecha_nac, fecha_fac), unit = "year"))))) %>%
  mutate(edadN = as.period(edadN)) %>%
  filter(as.character(edadN) != "", !is.na(edadN)) %>%
  mutate(anios = year(edadN), meses = month(edadN), dias = day(edadN)) %>%
  mutate(edad = ifelse(
    EstimadoNac & EsNacPrimeroEnero | EstimadoFac & EsFacPrimeroEnero,
    paste(anios, "años"),
    EdadTexto(anio = anios, mes = meses, dia = dias)
  )) %>% arrange(desc(n))

listaEdades = listaEdades %>% slice(1:150000)


print(table(listaEdades$genero))
cat("Descripciones")
print(table(!is.na(listaEdades$descripcionCorta) & listaEdades$descripcionCorta != ""))
cat("Categorias")
print(table(!is.na(listaEdades$categorias) & listaEdades$categorias != ""))
cat("Nacionalidades")
print(table(!is.na(listaEdades$Nacionalidad) & listaEdades$Nacionalidad != ""))


### Guardar ----

save(listaEdades, ContieneFecha, convertirHoras, EdadTexto, parsear_fecha_es, meses,
     refPr, refInstrumentos, refDeportistas, refMedicos, ListaNacionalidades,
     file = "app/data/ListaEdades.RData")


listaEdades %>%
  mutate(agregadoCaracteristica1 = as.Date(agregadoCaracteristica)) %>% 
  filter(!is.na(Nacimiento)) %>%
  ggplot(.) + 
  geom_bar(mapping = aes(x = agregadoCaracteristica1), stat = "count")
