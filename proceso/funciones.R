

library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)


### Auxiliares ----

ContieneFecha = function(x, sacarref = TRUE, estricto = FALSE){
  
  meses = c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre",
    "octubre", "noviembre", "diciembre")
  
  r  = grepl(
    paste0("\\b(", paste(meses, collapse = "|"), ")\\s+(de|del)\\s+\\d{1,4}\\b"),
    x,
    ignore.case = TRUE
  )
  
  if (!estricto){
    
    if (!r) if (!is.na(suppressWarnings(as.numeric(x)))) r = TRUE
    if (!r) if (regexpr("\\d{1,4}\\s+a. C.", substr(x, 1, 12)) == 1) r = TRUE
    if (!r) if (regexpr("c.\\s+\\d{1,4}", substr(x, 1, 10)) == 1) r = TRUE
    if (!r) if (regexpr("\\d{1,4},\\s+", substr(x, 1, 10)) == 1) r = TRUE
    if (!r) if (regexpr("\\d{1,4}\\s+\\(", substr(x, 1, 10)) == 1) r = TRUE
    
  }
  
  if (sacarref) if (grepl("<", x) | grepl(">", x)) r = FALSE

  return(r)
  
}

TextoAFecha = function(x){
  
  resultado = Date(length = length(x))
  
  meses = c(
    enero = 1, febrero = 2, marzo = 3, abril = 4, mayo = 5, junio = 6,
    julio = 7, agosto = 8, septiembre = 9, octubre = 10,
    noviembre = 11, diciembre = 12
  )
  
  try({
    
    x = tolower(x)
    x = gsub("(¿|\\?)", " ", x)
    
    patron = paste0(
      "(\\d{1,2})\\s+de\\s+(",
      paste(names(meses), collapse = "|"),
      ")\\s+(de|del)\\s+(\\d{1,4})"
    )
    
    hayFecha = grepl(patron, x, perl = TRUE)
    
    x1 = regmatches(x, regexpr(patron, x))
    
    # Extraer día
    dia = as.integer(regmatches(x1, regexpr("\\b\\d{1,2}\\b", x1)))
    
    # Extraer mes
    mes_txt = regmatches(x1, regexpr(
      paste(names(meses), collapse = "|"), x1
    ))
    
    mes = meses[mes_txt]
    
    # Extraer año
    anio = regmatches(x1, regexpr("(de|del)\\s+\\b\\d{1,4}\\b", x1))
    anio = gsub("(de|del)\\s+", "", anio)
    anio = as.integer(anio)
    
    r = as.Date(sprintf("%04d-%02d-%02d", anio, mes, dia))
    
    hayAC = mapply(grepl, paste(x1, "a. c."), x[hayFecha])
    
    if (any(hayAC)){
      
      a = 365 - as.numeric(as.Date(paste("1970", mes[hayAC], dia[hayAC], sep = "-"))) * 2
      
      rAC = as.numeric(as.Date(r[hayAC]))+ as.numeric(as.Date("1970-01-01") - as.Date("0001-01-01"))
      rAC = as.Date(-rAC, "0000-01-01")
      rAC = rAC - a
      
      r[hayAC] = rAC
      
    }
    
    resultado[hayFecha] = r
    
  }, silent = TRUE)
  
  return(resultado)
  
}

meses = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
          "septiembre", "octubre", "noviembre", "diciembre")

separar_html = function(f) {
  unlist(regmatches(
    f,
    gregexpr("<[^>]+>|[^<]+", f, perl = TRUE)
  ))
}

t = proc.time()

convertirHoras = function(t){
  
  h = floor(t / 3600)
  m = floor((t - h * 3600) / 60)
  s = floor(t - h * 3600 - m * 60)
  s1 = floor((t - h * 3600 - m * 60 - s) * 100)
  
  horas = ifelse(h == 1, "hora,", "horas,")
  minutos = ifelse(m == 1, "minuto,", "minutos,")
  segundos = ifelse(s == 1, "segundo", "segundos")
  
  if (t > 3600){
    r = paste(h, horas, m, minutos, s, segundos)
  } else if (t > 60){
    r = paste(m, minutos, s, segundos)
  } else {
    r = paste0(s, ",", s1, " ", segundos)
  }
  
  return(r)
  
}

convertirHoras((proc.time() - t)[3])

parsear_fecha_es = function(texto) {
  meses = c("enero" = 1, "febrero" = 2, "marzo" = 3, "abril" = 4,
             "mayo" = 5, "junio" = 6, "julio" = 7, "agosto" = 8,
             "septiembre" = 9, "octubre" = 10, "noviembre" = 11,
             "diciembre" = 12)
  
  sapply(texto, function(x) {
    partes = strsplit(tolower(x), " de ")[[1]]
    if (length(partes) == 3) {
      make_date(
        year = as.integer(partes[3]),
        month = meses[partes[2]],
        day = as.integer(partes[1])
      )
    } else {
      NA
    }
  })
}

BorrarSup = function(x){
  gsub(
    '<sup[^>]*id="cite[^"]*"[^>]*>.*?</sup>',
    '',
    x,
    perl = TRUE
  )
}

SiguienteTd = function(f1, Ubic) {
  
  a1 = which(regexpr("<td", f1) == 1 & 1:length(f1) >= Ubic)
  a2 = which(f1 == "</td>" & 1:length(f1) >= Ubic)
  a2 = a2[a2 >= min(a1)]
  
  if (length(a1) > 0 & length(a2) > 0){
    
    u = 1
    
    if (length(a1) > 1 & length(a2) > 1){
      while (a2[u] > min(a1[-c(1:u)]) & u < length(a1) & u < length(a2)) u = u + 1
    }
    
    u = min(u, length(a2))
    
    a1 = a1[1]
    a2 = a2[u]
    
    texto = paste0(f1[a1:a2], collapse = "")
    texto = gsub("<\\s*br\\s*/\\s*>", ", ", texto, perl = TRUE)
    texto = gsub("<\\s*br\\s*>", ", ", texto, perl = TRUE)
    texto = gsub("<\\s*/\\s*br\\s*>", ", ", texto, perl = TRUE)
    texto = trimws(texto)
    texto = gsub("\\s+", " ",texto, perl = TRUE)
    while (grepl(", , ", texto)) texto = gsub(", , ", ", ", texto, perl = TRUE)
    texto = gsub("<span[^>]+>", " ", texto, perl = TRUE)
    texto = gsub("<[^>]+>", "", texto, perl = TRUE)
    texto = trimws(texto)
    texto = gsub("\\s+", " ",texto, perl = TRUE)
    texto = gsub("\\s+,", ",", texto, perl = TRUE)
    
  } else {
    
    texto = NA
    
  }
  
  return(texto)
}

ConvertirFlag = function(x){
  
  a = which(
    regexpr("<a href=\"/wiki/Archivo:Flag", x) == 1 & 
      grepl('title="[^"]+"', x, perl = TRUE))
  
  for (i in a){
    x[i] = regmatches(
      x[i],
      regexpr('title="[^"]+"', x[i])
    )
    x[i] = sub('title="', "", x[i])
    x[i] = sub('"', "", x[i])
  }
  
  return(x)
}

limpiar_alias = function(x) {
  x2 = tolower(x)
  x2 = gsub("[«»'’\"]", "", x2)
  x2 = gsub("\\s+", " ", trimws(x2))
  x2 = gsub("á", "a", x2)
  x2 = gsub("é", "e", x2)
  x2 = gsub("í", "i", x2)
  x2 = gsub("ó", "o", x2)
  x2 = gsub("ú", "u", x2)
  x2
}

eliminar_redundantes = function(alias) {
  limpio = limpiar_alias(alias)
  
  n = length(alias)
  redundante = logical(n)
  
  for (i in seq_len(n)) {
    palabras_i =  strsplit(limpio[i], " ")[[1]]
    
    for (j in seq_len(n)) {
      if (i != j) {
        palabras_j = strsplit(limpio[j], " ")[[1]]
        
        if (all(palabras_i %in% palabras_j) && length(palabras_j) > length(palabras_i)) {
          redundante[i] = TRUE
          break
        }
      }
    }
  }
  
  for (i in seq_len(n-1)) for (j in (i+1):n){
    if (limpio[i] == limpio[j]){
      redundante[i] = TRUE
    }
  }
  
  alias[!redundante]
}

bajarJSON = function(url, tiempo = 300){
  
  res = GET(url = url, timeout(tiempo))
  
  if (status_code(res)==200){
    r = fromJSON(content(res, as = "text", encoding = "UTF-8"))
  } else {
    r = ""
  }

  return(r)
  
}

### Funcion ----

DescripcionCorta = function(nombre_pagina){
  
  r = list()
  
  pagina = try(suppressWarnings(scan(
    paste0("https://es.wikipedia.org/wiki/", nombre_pagina),
    what = "c", sep = "\n",
    encoding = "UTF-8", quiet = TRUE, skip = 22)), silent = TRUE)
  
  
  p = pagina[
    grepl("editar datos en Wikidata", pagina) & grepl("https://www.wikidata", pagina)]
  
  if (length(p) > 0){
    
    l = unlist(regmatches(
      p,
      gregexpr('"https://www.wikidata[^"]+"', p)
    ))
    
    l = gsub('"', '', l)
    l = l[!grepl("\\?", l)]
    l = sub("https://www.wikidata.org/wiki/", "", l)
    
  } else {
    
    p = pagina[
      grepl("Elemento de Wikidata", pagina) & grepl("https://www.wikidata", pagina)]
    
    l = unlist(regmatches(
      p,
      gregexpr('"https://www.wikidata.org/wiki/Special:EntityPage/[^"]+"', p)
    ))
    
    l = gsub('"', '', l)
    l = l[!grepl("\\?", l)]
    l = sub("https://www.wikidata.org/wiki/Special:EntityPage/", "", l)
    
  }
  
  url = paste0("https://www.wikidata.org/wiki/Special:EntityData/",l[1],".json")
  
  pp = try(bajarJSON(url), silent = TRUE)
  
  if (!"try-error" %in% class(pp)){
    
    r$DescripcionCortaTexto = pp$entities[[l[1]]]$descriptions$es$value
    
    if ("aliases" %in% names(pp$entities[[l[1]]])){
      if ("es" %in% names(pp$entities[[l[1]]]$aliases)){
        r$alias = unique(pp$entities[[l[1]]]$aliases$es$value)
      }
    } 
    
  }
  
  datosExtra = try(bajarJSON(paste0(
    "https://query.wikidata.org/sparql?query=",
    "SELECT",
    "%20?persona",
    "%20?personaLabel",
    "%20?fechaNacimiento",
    "%20?lugarNacimiento",
    "%20?lugarNacimientoLabel",
    "%20?fechaFallecimiento",
    "%20?nacionalidadLabel",
    "%20?ciudadaniaLabel",
    "%20?residencia",
    "%20?residenciaLabel",
    "%20?apodo",
    "%20?otrosNombres",
    "%20?nombreNacimiento",
    "%20?genero",
    "%20?generoLabel",
    "%20WHERE%20%7B",
    "%20VALUES%20?persona%20%7B%20wd:", l[1], "%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P569%20?fechaNacimiento%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P19%20?lugarNacimiento%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P570%20?fechaFallecimiento%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P27%20?nacionalidad%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P27%20?ciudadania%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P551%20?residencia%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P1449%20?apodo%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P2561%20?otrosNombres%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P1477%20?nombreNacimiento%20.%20%7D",
    "%20OPTIONAL%20%7B%20?persona%20wdt:P21%20?genero%20.%20%7D",
    "%20SERVICE%20wikibase:label%20%7B",
    "%20bd:serviceParam%20wikibase:language%20%22es%22.%20%7D",
    "%20%7D",
    "&format=json"
  )), silent = TRUE)
  
  if ("try-error" %in% class(datosExtra)) return(r)
  
  for (i in c(
    "fechaNacimiento", "fechaFallecimiento", "lugarNacimientoLabel", "nacionalidadLabel", 
    "ciudadaniaLabel", "residenciaLabel", "apodo", "otrosNombres", "nombreNacimiento", 
    "generoLabel")){
    
    if (i %in% names(datosExtra$results$bindings)){
      r[[i]] = unique(datosExtra$results$bindings[[i]]$value)
    } 
    
  }
  
  if ("lugarNacimiento" %in% names(datosExtra$results$bindings)) try({
    
    paises = c()
    
    for (i in unique(datosExtra$results$bindings$lugarNacimiento$value)){
      
      lugar = gsub("http://www.wikidata.org/entity/", "", i)
      
      datosPais = bajarJSON(paste0(
        "https://query.wikidata.org/sparql?query=",
        "SELECT",
        "%20?pais",
        "%20?paisLabel",
        "%20WHERE%20%7B",
        "%20wd:",lugar,"%20wdt:P17%20?pais%20.",
        "%20SERVICE%20wikibase:label%20%7B",
        "%20bd:serviceParam%20wikibase:language%20%22es%22.%20%7D",
        "%20%7D",
        "&format=json"
      ))
      
      paises = c(paises, paste(datosPais$results$bindings$paisLabel$value, collapse = " "))
      
    }
    
    r$lugarNacimientoLabel = paste0(r$lugarNacimientoLabel, ", ", paises, collapse = " ")
    
  }, silent = TRUE)
  
  if ("residencia" %in% names(datosExtra$results$bindings)) try({
    
    paises = c()
    
    for (i in unique(datosExtra$results$bindings$residencia$value)){
      
      lugar = gsub("http://www.wikidata.org/entity/", "", i)
      
      datosPais = bajarJSON(paste0(
        "https://query.wikidata.org/sparql?query=",
        "SELECT",
        "%20?pais",
        "%20?paisLabel",
        "%20WHERE%20%7B",
        "%20wd:",lugar,"%20wdt:P17%20?pais%20.",
        "%20SERVICE%20wikibase:label%20%7B",
        "%20bd:serviceParam%20wikibase:language%20%22es%22.%20%7D",
        "%20%7D",
        "&format=json"
      ))
      
      paises = c(paises, paste(datosPais$results$bindings$paisLabel$value, collapse = " "))
      
    }
    
    r$residenciaLabel = paste0(r$residenciaLabel, ", ", paises, collapse = " ")
    
  }, silent = TRUE)
  
  names(r) = gsub("Label", "", names(r))
  
  return(r)
  
}

BuscarCategorias = function(titulo){
  
  np1 = gsub(" ", "_", titulo)
  np2 = gsub("\\s*\\([^)]*\\)", "", titulo)

  categoriaPagina = c()
  
  pagina = try(suppressWarnings(scan(
    paste0("https://es.wikipedia.org/wiki/", np1),
    what = "c", sep = "\n",
    encoding = "UTF-8", quiet = TRUE, skip = 22)), silent = TRUE)
  
  if ("try-error" %in% class(pagina)) return(categoriaPagina)
  
  UbicCategoria = grep(pattern = '<div class="printfooter"', pagina)
  UbicCategoria2 = grep(pattern = 'mw-hidden-catlinks', pagina)
  if (length(UbicCategoria2) == 0){
    UbicCategoria2 = grep(pattern = 'footer-container', pagina)
  }
  
  pagina = pagina[(UbicCategoria[1]):(UbicCategoria2[length(UbicCategoria2)])]
  
  referencias = unlist(
    regmatches(
      pagina,
      gregexpr(
        '(href="/wiki/Categor%C3%ADa:|rel=\"mw:WikiLink\" href=\"//es.wikipedia.org/wiki/Categoría)[^"]+"', 
        pagina)
    )
  )
  
  referencias = gsub('href="/wiki/|\"', '', referencias)
  referencias = gsub('rel=mw:WikiLink href=//es.wikipedia.org/wiki/', '', referencias)
  
  referencias = substr(
    referencias, 1, 
    ifelse(grepl("#", referencias), regexpr("#", referencias) - 1, nchar(referencias)))
  
  referencias = substr(
    referencias, 1, 
    ifelse(grepl("\\?", referencias), regexpr("\\?", referencias) - 1, nchar(referencias)))
  
  referencias = URLdecode(referencias)
  
  referencias = referencias[!grepl("Wikipedia", referencias)]
  
  categoriaPagina = unique(c(categoriaPagina, referencias))
  
  opciones = c(paste0("Categoría:", titulo), paste0("Categoría:", np2))
  opciones = unique(opciones)
  
  opciones = opciones[sapply(opciones, function(x) any(grepl(x, pagina)))]
  
  if (length(opciones) > 0) opciones = gsub(" ", "_", opciones)
  
  for (p in opciones) try({
    
    pagina = try(suppressWarnings(scan(
      paste0("https://es.wikipedia.org/wiki/", p),
      what = "c", sep = "\n",
      encoding = "UTF-8", quiet = TRUE, skip = 22)), silent = TRUE)
    
    if ("try-error" %in% class(pagina)) next
    
    UbicCategoria = grep(pattern = '<div class="printfooter"', pagina)
    UbicCategoria2 = grep(pattern = 'mw-hidden-catlinks', pagina)
    if (length(UbicCategoria2) == 0){
      UbicCategoria2 = grep(pattern = 'footer-container', pagina)
    } 
    
    pagina = pagina[(UbicCategoria[1]):(UbicCategoria2[length(UbicCategoria2)])]
    
    referencias = unlist(
      regmatches(
        pagina,
        gregexpr(
          '(href="/wiki/Categor%C3%ADa:|rel=\"mw:WikiLink\" href=\"//es.wikipedia.org/wiki/Categoría)[^"]+"', 
          pagina)
      )
    )
    
    referencias = gsub('href="/wiki/|\"', '', referencias)
    referencias = gsub('rel=mw:WikiLink href=//es.wikipedia.org/wiki/', '', referencias)
    
    referencias = substr(
      referencias, 1, 
      ifelse(grepl("#", referencias), regexpr("#", referencias) - 1, nchar(referencias)))
    
    referencias = substr(
      referencias, 1, 
      ifelse(grepl("\\?", referencias), regexpr("\\?", referencias) - 1, nchar(referencias)))
    
    referencias = URLdecode(referencias)
    
    referencias = referencias[!grepl("Wikipedia", referencias)]
    
    categoriaPagina = unique(c(categoriaPagina, referencias))
    
  })
  
  return(categoriaPagina)
  
}

BuscarReferencias = function(nombre_pagina, AgregarReferencias = TRUE, nmax = 2000, 
                             marcar = FALSE, AgregarDatos = TRUE, imprimircat = FALSE,
                             Ver = "todo"){
  
  t = proc.time()
  
  Caracteristicas = list()
  Caracteristicas$pagina = nombre_pagina
  
  ### Titulo ----
  
  try({
    
    titulo = suppressWarnings(scan(
      paste0("https://es.wikipedia.org/wiki/", nombre_pagina),
      what = "c", sep = "\n", encoding = "UTF-8", quiet = TRUE, skip = 4, nmax = 1))
    
    titulo = sub("<title>", "", titulo)
    titulo = sub(" - Wikipedia, la enciclopedia libre</title>", "", titulo)
    titulo = gsub("&amp;", "&", titulo)
    Caracteristicas$titulo = titulo
    
  }, silent = TRUE)
  
  if (marcar) cat("titulo: ", convertirHoras((proc.time() - t)[3]), "-")
  
  ### Lectura ----
  
  pagina = try(suppressWarnings(scan(
    paste0("https://es.wikipedia.org/wiki/", nombre_pagina),
    what = "c", sep = "\n",
    encoding = "UTF-8", quiet = TRUE, skip = 22)), silent = TRUE)
  
  if (marcar) cat("lectura: ", convertirHoras((proc.time() - t)[3]), "-")
  
  if ("try-error" %in% class(pagina)) stop("No se pudo encontrar la pagina")
  
  if (Ver != "todo"){
    
    UbicCategoria = grep(pattern = '<div class="printfooter"', pagina)
    
    if (length(UbicCategoria) > 0){
      
      if (Ver == "cuerpo"){
        pagina = pagina[1:(UbicCategoria[1] - 1)]
      }
      if (Ver == "categoria"){
        pagina = pagina[(UbicCategoria[1] + 1):length(pagina)]
      }
      
    }
    
  }
  
  ### Referencias ----
  
  if (AgregarReferencias){
    
    referencias = unlist(
      regmatches(
        pagina,
        gregexpr(
          '(href="/wiki/|rel=\"mw:WikiLink\" href=\"//es.wikipedia.org/wiki/)[^"]+"', 
          pagina)
      )
    )
    
    referencias = gsub('href="/wiki/|\"', '', referencias)
    referencias = gsub('rel=mw:WikiLink href=//es.wikipedia.org/wiki/', '', referencias)
    
    referencias = substr(
      referencias, 1, 
      ifelse(grepl("#", referencias), regexpr("#", referencias) - 1, nchar(referencias)))
    
    referencias = substr(
      referencias, 1, 
      ifelse(grepl("\\?", referencias), regexpr("\\?", referencias) - 1, nchar(referencias)))
    
    referencias = URLdecode(referencias)
    
    referencias = unique(referencias)
    referencias = referencias[!grepl("Wiki", referencias)]
    referencias = referencias[!grepl("Portal:", referencias)]
    referencias = referencias[!grepl("Especial:", referencias)]
    referencias = referencias[!grepl("Ayuda:", referencias)]
    referencias = referencias[!grepl("Archivo:", referencias)]
    referencias = referencias[!grepl("Usuario", referencias)]
    
    if (grepl("Categor", nombre_pagina) & Ver != "categoria") suppressWarnings({
      
      refCategoria = unlist(
        regmatches(
          pagina,
          gregexpr('href="/w/index[^"]+"', pagina)
        )
      )
      
      refCategoria = gsub('href="/|\"', '', refCategoria)
      
      refCategoria = refCategoria[length(refCategoria)]
      refCategoria = gsub("amp;", "", refCategoria)
      
      while (TRUE){
        
        cat("-")
        
        if (imprimircat) cat(refCategoria, " ")
        
        largoRef = length(referencias)
        
        pagina2 = try(suppressWarnings(scan(
          paste0("https://es.wikipedia.org/", refCategoria),
          what = "c", sep = "\n",
          encoding = "UTF-8", quiet = TRUE, skip = 22)), silent = TRUE)
        
        if ("try-error" %in% class(pagina2)) break
        
        UbicCategoria = grep(pattern = '<div class="printfooter"', pagina2)
        
        if (length(UbicCategoria) > 0 & Ver == "cuerpo"){
          pagina2 = pagina2[1:(UbicCategoria[1] - 1)]
        }
        
        p = which(grepl("<a href=\"/wiki", pagina2) & grepl("title", pagina2))
        
        referencias = c(referencias, unlist(
          regmatches(
            pagina2,
            gregexpr(
              '(href="/wiki/|rel=\"mw:WikiLink\" href=\"//es.wikipedia.org/wiki/)[^"]+"', 
              pagina2)
          )
        ))
        
        referencias = gsub('href="/wiki/|\"', '', referencias)
        referencias = gsub('rel=mw:WikiLink href=//es.wikipedia.org/wiki/', '', referencias)
        
        referencias = substr(
          referencias, 1, 
          ifelse(grepl("#", referencias), regexpr("#", referencias) - 1, nchar(referencias)))
        
        referencias = substr(
          referencias, 1, 
          ifelse(grepl("\\?", referencias), regexpr("\\?", referencias) - 1, nchar(referencias)))
        
        referencias = URLdecode(referencias)
        
        referencias = unique(referencias)
        referencias = referencias[!grepl("Wiki", referencias)]
        referencias = referencias[!grepl("Portal:", referencias)]
        referencias = referencias[!grepl("Especial:", referencias)]
        referencias = referencias[!grepl("Ayuda:", referencias)]
        referencias = referencias[!grepl("Archivo:", referencias)]
        referencias = referencias[!grepl("Usuario", referencias)]
        
        if (length(referencias) <= largoRef) break
        if (imprimircat) cat(length(referencias), "      ")
        
        refCategoria = unlist(
          regmatches(
            pagina2,
            gregexpr('href="/w/index[^"]+"', pagina2)
          )
        )
        
        refCategoria = gsub('href="/|\"', '', refCategoria)
        
        refCategoria = refCategoria[length(refCategoria)]
        refCategoria = gsub("amp;", "", refCategoria)
        
      }
      
    })
    
    Caracteristicas$referencias = referencias
    
    if (marcar) cat("referencias: ", convertirHoras((proc.time() - t)[3]), "-")
  }
  
  if (!AgregarDatos) return(Caracteristicas)
  
  ### Descripcion ----
  
  descrCorta = try(suppressWarnings(DescripcionCorta(nombre_pagina)), silent = TRUE)
  
  if (!"try-error" %in% class(descrCorta)){
    
    if ("DescripcionCortaTexto" %in% names(descrCorta)){
      
      descrCortaTexto = descrCorta$DescripcionCortaTexto
      
    } else {
      descrCortaTexto = ""
    }
    
    if (length(descrCortaTexto) > 0 & any(!is.null(descrCortaTexto))){
      Caracteristicas$descripcionCorta = descrCortaTexto[!is.null(descrCortaTexto)][1]
    } else {
      Caracteristicas$descripcionCorta = ""
    }
    
  } else {
    
    Caracteristicas$descripcionCorta = ""
    
  }
  
  ### Categorias y genero ----
  
  try({
    
    categorias = BuscarCategorias(titulo = Caracteristicas$titulo)
    
    Caracteristicas$categorias = paste0(
      sub("(Categor%C3%ADa:|Categoría:)", "C:", categorias), collapse = "-")
    
  })
  
  if (marcar) cat("categorias: ", convertirHoras((proc.time() - t)[3]), "-")
  
  try({
    
    j = Caracteristicas$categorias
    
    genero = case_when(
      grepl("C:Hombres_transg%C3%A9nero", j) ~ "Hombre transgénero",
      grepl("C:Mujeres_transg%C3%A9nero", j) ~ "Mujer transgénero",
      grepl("C:Hombres_transgénero", j) ~ "Hombre transgénero",
      grepl("C:Mujeres_transgénero", j) ~ "Mujer transgénero",
      grepl("C:Hombres_intersexo", j) ~ "Hombre intersexo",
      grepl("C:Mujeres_intersexo", j) ~ "Mujer intersexo",
      grepl("C:Personas_de_g%C3%A9nero_fluido" , j) ~ "Persona de género fluido",
      grepl("C:Personas_ag%C3%A9nero", j) ~ "Persona agénero",
      grepl("C:Personas_dos_esp%C3%ADritus", j) ~ "Persona dos espíritus",
      grepl("C:Personas_de_género_fluido" , j) ~ "Persona de género fluido",
      grepl("C:Personas_agénero", j) ~ "Persona agénero",
      grepl("C:Personas_dos_espíritus", j) ~ "Persona dos espíritus",
      grepl("C:Fa%27afafine", j) ~ "Persona no binaria",
      grepl("C:Fa'afafine", j) ~ "Persona no binaria",
      grepl("C:Personas_no_binarias", j) ~ "Persona no binaria",
      grepl("C:Personas_intersexo", j) ~ "Intersexo",
      grepl(
        "C:Figuras_hist%C3%B3ricas_con_identidad_de_g%C3%A9nero_ambigua_o_en_disputa", 
        j) ~ "Género en disputa",
      grepl(
        "C:Figuras_históricas_con_identidad_de_género_ambigua_o_en_disputa", 
        j) ~ "Género en disputa",
      grepl("C:Travestis", j) ~ "Travesti",
      grepl("C:Hombres", j) ~ "Hombre",
      grepl("C:Mujeres", j) ~ "Mujer",
      TRUE ~ "Sin especificar"
    )
    
    Caracteristicas$Genero = genero
    
  })
  
  if (marcar) cat("genero: ", convertirHoras((proc.time() - t)[3]), "-")
  
  ### Descripcion larga ----
  
  a1 = which(grepl("class=\"mw-body-content\"", pagina))
  a2 = which(grepl("class=\"catlinks\"", pagina))
  
  if (length(a1) > 0 & length(a2) > 0){
    
    a1 = a1[1]
    a2 = a2[1]
    
    p = paste0(pagina[a1:a2], collapse = "")
    
    p = gsub("<table[\\s\\S]*?</table>", "", p, perl = TRUE, ignore.case = TRUE)
    p = gsub("<p><span id=\"WikipediaGrabada[\\s\\S]*?</p>", "", p, perl = TRUE, ignore.case = TRUE)
    p = gsub("<p class=\"mw-empty-elt\"></p>", "", p)
    
    dd = regmatches(p, regexpr("<p[\\s\\S]*?</p>", p, perl = TRUE))
    
    if (length(dd) > 0) Caracteristicas$descripcion = dd[1]
    
  }
  
  pagina = unlist(strsplit(pagina, "(?<=\\s)", perl = TRUE))
  pagina = unlist(strsplit(pagina, "(?<=\\>)", perl = TRUE))
  
  ### Identificar tabla de datos ----
  
  ExisteTabla = TRUE
  
  a = which(pagina %in% c("class=\"mw-body-content\">","class=\"mw-body-content\" "))
  
  if (length(a) > 0 ){
    a = a[1]
    
    a = which(
      pagina %in% c("class=\"infobox ", "class=\"infobox\"", "class=\"infobox\" ") & 
        1:length(pagina)>=a)
  } else {
    ExisteTabla = FALSE
  }
  
  if (length(a) > 0){
    
    a = a[1]
    a0 = which(pagina == "<table " & 1:length(pagina) < a)
    a0 = a0[length(a0)]
    a1 = which(pagina %in% c("<table ","<table>","<table> ") & 1:length(pagina) >= a)
    a1 = c(a0, a1)
    a2 = which(pagina %in% c("</table>", "</table> ") & 1:length(pagina) >= a)
    a2 = a2[a2 >= min(a1)]
  } else {
    ExisteTabla = FALSE
  }
  
  if (length(a1) == 0 | length(a2) == 0) ExisteTabla = FALSE
  
  ### Si no existe tabla ----
  
  if (!ExisteTabla){
    
    if (!"try-error" %in% class(descrCorta)){
      
      if ("fechaNacimiento" %in% names(descrCorta)){
        
        EsAC = (regexpr("-", descrCorta$fechaNacimiento[1]) == 1)
        
        if (EsAC){
          fecha = substr(
            descrCorta$fechaNacimiento[1], 2, nchar(descrCorta$fechaNacimiento[1]))
        } else {
          fecha = descrCorta$fechaNacimiento[1]
        }
        
        fecha = try(as.Date(fecha), silent = TRUE)
        
        if (!"try-error" %in% class(fecha)){
          fechaTexto = paste0(
            day(fecha), " de ", meses[month(fecha)], " de ", year(fecha),
            ifelse(EsAC, " a. C.", ""),
            " (estimado en wikidata)")
        } else {
          fechaTexto = as.character(descrCorta$fechaNacimiento[1])
        }
        
        Caracteristicas$Nacimiento = fechaTexto
        
        # A proposito, para que no le ponga nombre si no nacio
        
        listaNombres = c()
        tituloP = trimws(gsub("\\([^)]*\\)", "", titulo))
        for (nom in c("alias", "apodo", "otrosNombres", "nombreNacimiento")){
          if (nom %in% names(descrCorta)){
            alias1 = descrCorta[[nom]]
            alias1 = alias1[!alias1 %in% c(titulo, tituloP)]
            listaNombres = unique(c(listaNombres, alias1))
          }
        }
        
        if (length(listaNombres) == 0){
          
          Caracteristicas$Nombre = titulo
          
        } else {
          
          listaNombres = eliminar_redundantes(sort(listaNombres))
          
          Caracteristicas$Nombre = paste(unique(listaNombres), collapse = ", ")
          
        }
        
      }
      
      if ("fechaFallecimiento" %in% names(descrCorta)){
        
        EsAC = (regexpr("-", descrCorta$fechaFallecimiento[1]) == 1)
        
        if (EsAC){
          fecha = substr(
            descrCorta$fechaFallecimiento[1], 2, nchar(descrCorta$fechaFallecimiento[1]))
        } else {
          fecha = descrCorta$fechaFallecimiento[1]
        }
        
        fecha = try(as.Date(fecha), silent = TRUE)
        
        if (!"try-error" %in% class(fecha)){
          fechaTexto = paste0(
            day(fecha), " de ", meses[month(fecha)], " de ", year(fecha),
            ifelse(EsAC, " a. C.", ""),
            " (estimado en wikidata)")
        } else {
          fechaTexto = as.character(descrCorta$fechaFallecimiento[1])
        }
        
        Caracteristicas$Fallecimiento = fechaTexto
        
      }
      
      if ("lugarNacimiento" %in% names(descrCorta)){
        
        Caracteristicas$Nacimiento1 = descrCorta$lugarNacimiento
        
      }
      
      listanac = c()
      
      for (nom in c("nacionalidad", "ciudadania", "residencia")){
        if (nom %in% names(descrCorta)){
          listanac = unique(c(listanac, descrCorta[[nom]]))
        }
      }
      
      if (length(listanac) > 0){
        
        listanac = eliminar_redundantes(sort(listanac))
        
        Caracteristicas$Nacionalidad = paste(unique(listanac), collapse = ", ")
        
      }
      
    }
    
    return(Caracteristicas)
    
  }
  
  ### Si existe tabla ----
  
  u = 1
  
  if (length(a1) > 1 & length(a2) > 1){
    while (a2[u] > min(a1[-c(1:u)]) & u < length(a1) & u < length(a2)) u = u + 1
  }
  
  u = min(u, length(a2))
  
  a1 = a1[1]
  a2 = a2[u]
  
  f = paste0(pagina[a1:a2], collapse = "")
  f = BorrarSup(f)
  f = gsub("&#\\d{1,4};", " ", f, perl = TRUE)
  
  f1 = separar_html(f)
  f1 = ConvertirFlag(f1)
  
  if (marcar) cat("separar html: ", convertirHoras((proc.time() - t)[3]), "-")
  
  ### Nombres ----
  
  listaNombres = c()
  
  UbicNombres = which(regexpr("nombre", tolower(f1)) == 1)
  
  tituloP = trimws(gsub("\\([^)]*\\)", "", titulo))
  
  for (u in UbicNombres) try({
    
    nombre1 = SiguienteTd(f1, u)
    nombre1 = trimws(strsplit(nombre1, ",")[[1]])
    nombre1 = nombre1[!nombre1 %in% c(titulo, tituloP)]
    listaNombres = unique(c(listaNombres, nombre1))
    
  })
  
  for (u in c("apodo", "apodo/s", "apodo(s)", "seudónimo", "otros nombres")){
    
    UbicCabecera = which(tolower(f1) == u)
    
    if (length(UbicCabecera) > 0) try({
      
      nombre1 = SiguienteTd(f1, UbicCabecera[1])
      nombre1 = trimws(strsplit(nombre1, ",")[[1]])
      nombre1 = nombre1[!nombre1 %in% c(titulo, tituloP)]
      listaNombres = unique(c(listaNombres, nombre1))
      
    })
    
  }
  
  if (!"try-error" %in% class(descrCorta)){
    
    for (nom in c("alias", "apodo", "otrosNombres", "nombreNacimiento")){
      if (nom %in% names(descrCorta)){
        alias1 = descrCorta[[nom]]
        alias1 = alias1[!alias1 %in% c(titulo, tituloP)]
        listaNombres = unique(c(listaNombres, alias1))
      }
    } 
    
  }
  
  if (length(listaNombres) == 0){
    
    Caracteristicas$Nombre = titulo
    
  } else {
    
    listaNombres = eliminar_redundantes(sort(listaNombres))
    
    Caracteristicas$Nombre = paste(unique(listaNombres), collapse = ", ")
    
  }
  
  if (marcar) cat("nombre: ", convertirHoras((proc.time() - t)[3]), "-")
  
  ### Nacimiento ----
  
  UbicNac = which(f1 %in% c("Nacimiento", "Fecha de nacimiento"))
  
  Caracteristicas$Nacimiento = ""
  
  if (length(UbicNac) > 0) try({
    
    Caracteristicas$Nacimiento = SiguienteTd(f1, UbicNac[1])
    
  }, silent = TRUE)
  
  if (!"try-error" %in% class(descrCorta)) if ("fechaNacimiento" %in% names(descrCorta)){
    
    EsAC = (regexpr("-", descrCorta$fechaNacimiento[1]) == 1)
    
    if (EsAC){
      fecha = substr(descrCorta$fechaNacimiento[1], 2, nchar(descrCorta$fechaNacimiento[1]))
    } else {
      fecha = descrCorta$fechaNacimiento[1]
    }
    
    fecha = try(as.Date(fecha), silent = TRUE)
    
    if (!"try-error" %in% class(fecha)){
      fechaTexto = paste0(
        day(fecha), " de ", meses[month(fecha)], " de ", year(fecha),
        ifelse(EsAC, " a. C.", ""),
        " (estimado en wikidata)")
    } else {
      fechaTexto = as.character(descrCorta$fechaNacimiento[1])
    }
    
    if (Caracteristicas$Nacimiento == ""){
      
      if (!is.na(fecha)){
        
        Caracteristicas$Nacimiento = fechaTexto
        
      }
      
    } else {
      TratarFecha = TextoAFecha(Caracteristicas$Nacimiento)
      
      if (is.na(TratarFecha) & !is.na(fecha)){
        
        Caracteristicas$Nacimiento = paste(Caracteristicas$Nacimiento, "-", fechaTexto)
      
      }
    
    }
    
  }
  
  if (Caracteristicas$Nacimiento == "") Caracteristicas$Nacimiento = NULL
  
  lugNacimiento = c()
  
  UbicNac1 = which(f1 %in% c("Lugar de nacimiento"))
  
  if (length(UbicNac1) > 0) try({
    
    lugNacimiento = c(lugNacimiento, SiguienteTd(f1, UbicNac1[1]))
    
  })
  
  if (!"try-error" %in% class(descrCorta)) if ("lugarNacimiento" %in% names(descrCorta)){
    
    lugNacimiento = unique(c(lugNacimiento, trimws(descrCorta$lugarNacimiento)))
    
  }
  
  if (length(lugNacimiento) > 0){
    
    Caracteristicas$Nacimiento1 = paste(lugNacimiento, collapse = ", ")
    
  } 
  
  if (marcar) cat("nacimiento: ", convertirHoras((proc.time() - t)[3]), "-")
  
  ### Fallecimiento ----
  
  UbicFac = which(f1 %in% c("Fallecimiento", "Fecha de fallecimiento", "Desaparición"))
  
  Caracteristicas$Fallecimiento = ""
  
  if (length(UbicFac) > 0) try({
    
    Caracteristicas$Fallecimiento = SiguienteTd(f1, UbicFac[1])
    
  })
  
  if (!"try-error" %in% class(descrCorta)) if ("fechaFallecimiento" %in% names(descrCorta)){
    
    EsAC = (regexpr("-", descrCorta$fechaFallecimiento[1]) == 1)
    
    if (EsAC){
      fecha = substr(
        descrCorta$fechaFallecimiento[1], 2, nchar(descrCorta$fechaFallecimiento[1]))
    } else {
      fecha = descrCorta$fechaFallecimiento[1]
    }
    
    fecha = try(as.Date(fecha), silent = TRUE)
    
    if (!"try-error" %in% class(fecha)){
      fechaTexto = paste0(
        day(fecha), " de ", meses[month(fecha)], " de ", year(fecha),
        ifelse(EsAC, " a. C.", ""),
        " (estimado en wikidata)")
    } else {
      fechaTexto = as.character(descrCorta$fechaFallecimiento[1])
    }
    
    if (Caracteristicas$Fallecimiento == ""){
      
      if (!is.na(fecha)){
        
        Caracteristicas$Fallecimiento = fechaTexto
        
      }
      
    } else {
      TratarFecha = TextoAFecha(Caracteristicas$Fallecimiento)
      
      if (is.na(TratarFecha) & !is.na(fecha)){
        
        Caracteristicas$Fallecimiento = paste(Caracteristicas$Fallecimiento, "-", fechaTexto)
        
      }
      
    }
    
  }
  
  if (Caracteristicas$Fallecimiento == "") Caracteristicas$Fallecimiento = NULL
  
  UbicFac1 = which(f1 %in% c("Lugar de fallecimiento"))
  
  if (length(UbicFac1) > 0) try({
    
    Caracteristicas$Fallecimiento1 = SiguienteTd(f1, UbicFac1[1])
    
  })
  
  if (marcar) cat("fallecimiento: ", convertirHoras((proc.time() - t)[3]), "-")
  
  ### Nacionalidad ----
  
  listanac = c()
  
  for (u in c(
    "Nacionalidad", "Nacionalidad(es)", "Nacionalidades", "País", "País(es)", "Paises",
    "Ciudadanía", "Ciudadanía(s)", "Ciudadanías", "Residencia")){
    
    UbicNacionalidad = which(f1 == u)
    
    if (length(UbicNacionalidad) > 0) try({
      
      nombre1 = SiguienteTd(f1, UbicNacionalidad[1])
      nombre1 = trimws(strsplit(nombre1, ",")[[1]])
      listanac = unique(c(listanac, nombre1))
      
    })
    
  }
  
  if (!"try-error" %in% class(descrCorta)){
    
    for (nom in c("nacionalidad", "ciudadania", "residencia")){
      if (nom %in% names(descrCorta)){
        listanac = c(listanac, descrCorta[[nom]])
      }
    } 
    
  }
  
  if (length(listanac) > 0){
    
    listanac = eliminar_redundantes(sort(listanac))
    
    Caracteristicas$Nacionalidad = paste(unique(listanac), collapse = ", ")
    
  }
    
  listanac = c()
  
  if (marcar) cat("nacionalidad: ", convertirHoras((proc.time() - t)[3]), "-")
  
  ### Genero (segunda asignacion) ----
  
  if (!"try-error" %in% class(descrCorta)) if ("genero" %in% names(descrCorta)) try({
    
    GeneroL = case_when(
      "género fluido" %in% descrCorta$genero ~ "Persona de género fluido",
      "agénero" %in% descrCorta$genero ~ "Persona agénero",
      "no binaridad" %in% descrCorta$genero ~ "Persona no binaria",
      "travesti" %in% descrCorta$genero ~ "Travesti",
      "mujer transgénero" %in% descrCorta$genero ~ "Mujer transgénero",
      "hombre transgénero" %in% descrCorta$genero ~ "Hombre transgénero",
      "hombre intersexo" %in% descrCorta$genero ~ "Hombre intersexo",
      "mujer intersexo" %in% descrCorta$genero ~ "Mujer intersexo",
      "intersexualidad" %in% descrCorta$genero  ~ "Intersexo",
      "femenino" %in% descrCorta$genero ~ "Mujer",
      "masculino" %in% descrCorta$genero ~ "Hombre",
      TRUE ~ ""
    ) 
    
    if (GeneroL != "") Caracteristicas$Genero = GeneroL
    
  })
  
  ### Fin ----
  
  return(Caracteristicas)
  
}


### Agregar referencias ----

AgregarReferencias = function(lista, tiempo = 60, priorizarPersonas = TRUE){
 
  Descritos = lista$listadoCaracteristicas %>% .$pagina
  Personas = lista$listadoCaracteristicas %>% 
    filter(!is.na(Fallecimiento) | !is.na(Nacimiento)) %>% .$pagina
  NoPersonas = lista$listadoCaracteristicas %>% 
    filter(is.na(Fallecimiento), is.na(Nacimiento)) %>% .$pagina
  
  
  lista$listaReferencias = lista$listaReferencias %>%
    mutate(
      Descrito = ifelse(nombre %in% Descritos, TRUE, Descrito), 
      EsPersona = case_when(
        nombre %in% Personas ~ TRUE,
        nombre %in% NoPersonas ~ FALSE,
        TRUE ~ EsPersona
      )) %>%
    mutate(Consultar = ifelse(!is.na(EsPersona) & EsPersona, 1, 2))
    
  if (priorizarPersonas){
    lista$listaReferencias = lista$listaReferencias %>% 
      arrange(Consultar, desc(n), sample.int(nrow(.)))
  } else {
    lista$listaReferencias = lista$listaReferencias %>% 
      arrange(desc(n), sample.int(nrow(.)))
  }
  
  t = proc.time()
  
  while ((proc.time() - t)[3] < tiempo) {
    
    if (all(lista$listaReferencias$consultado)) break
    
    n1 = lista$listaReferencias$nombre[!lista$listaReferencias$consultado][1]
    
    cat(lista$listaReferencias$n[!lista$listaReferencias$consultado][1], "")
    
    j = try(BuscarReferencias(n1), silent = TRUE)
    
    if ("try-error" %in% class(j) | !is.list(j)){
      
      lista$listaReferencias$consultado[lista$listaReferencias$nombre == n1] = TRUE
      lista$listaReferencias$exito[lista$listaReferencias$nombre == n1] = FALSE
      
      cat(n1, "(página no encontrada)", convertirHoras((proc.time() - t)[3]), " - ")
      next
      
    }
    
    ahora = Sys.time()
    
    if ("referencias" %in% names(j)){
      ref1 = j$referencias
      
      lista$listaReferencias = lista$listaReferencias %>%
        mutate(n = n + ifelse(nombre %in% ref1, 1, 0))
      
      ref1 = dplyr::setdiff(ref1, lista$listaReferencias$nombre)
      
      if (length(ref1) > 0){
        
        lista$listaReferencias = dplyr::bind_rows(
          lista$listaReferencias, 
          data.frame(
            nombre = ref1, n = 1, consultado = FALSE, Descrito = FALSE, EsPersona = NA,
            origen = n1, agregado = ahora, Consultar = 2)
        )
      }
      
      j$referencias = NULL
      
    }
      
    lista$listaReferencias$consultado[lista$listaReferencias$nombre == n1] = TRUE
    lista$listaReferencias$exito[lista$listaReferencias$nombre == n1] = TRUE
      
    if (is.list(j) & length(j) > 0){
      
      if (n1 %in% lista$listadoCaracteristicas$pagina){
        
        lista$listadoCaracteristicas = lista$listadoCaracteristicas %>% filter(pagina != n1)
        
      }
      
      ahora = Sys.time()
      
      j = as.data.frame(j)
      j$agregadoCaracteristica = ahora
      
      lista$listadoCaracteristicas = dplyr::bind_rows(lista$listadoCaracteristicas, j)
      
    }
    
    Descritos = lista$listadoCaracteristicas %>% .$pagina
    Personas = lista$listadoCaracteristicas %>% 
      filter(!is.na(Fallecimiento) | !is.na(Nacimiento)) %>% .$pagina
    NoPersonas = lista$listadoCaracteristicas %>% 
      filter(is.na(Fallecimiento), is.na(Nacimiento)) %>% .$pagina
      
    lista$listaReferencias = lista$listaReferencias %>%
      mutate(
        Descrito = ifelse(nombre %in% Descritos, TRUE, Descrito), 
        EsPersona = case_when(
          nombre %in% Personas ~ TRUE,
          nombre %in% NoPersonas ~ FALSE,
          TRUE ~ EsPersona
        )) %>%
      mutate(Consultar = ifelse(!is.na(EsPersona) & EsPersona, 1, 2))
      
    if (priorizarPersonas){
      lista$listaReferencias = lista$listaReferencias %>% 
        arrange(Consultar, desc(n), sample.int(nrow(.)))
    } else {
      lista$listaReferencias = lista$listaReferencias %>% 
        arrange(desc(n), sample.int(nrow(.)))
    }
    
    cat(n1, convertirHoras((proc.time() - t)[3]), " - ")
    
  }
   
  return(lista)
  
}

### Forzar nuevas ----

ForzarNuevas = function(lista, nueva_referencia, tiempo = 600, AgregarRef = TRUE){
  
  if (AgregarRef){
    j = BuscarReferencias(nueva_referencia, marcar = TRUE, AgregarDatos = FALSE)
    
    listaNuevaReferencia = unique(c(nueva_referencia, j$referencias))
  } else {
    listaNuevaReferencia = unique(nueva_referencia)
  }
  
  t = proc.time()
  
  listaNuevaReferencia = dplyr::setdiff(
    listaNuevaReferencia,
    lista$listaReferencias$nombre[lista$listaReferencias$consultado])
  
  for (n1 in listaNuevaReferencia){
    
    if ((proc.time() - t)[3] > tiempo) break
    
    if (!n1 %in% lista$listaReferencias$nombre){
      lista$listaReferencias = plyr::rbind.fill(
        lista$listaReferencias,
          data.frame(nombre = n1, n = 0, consultado = FALSE)
      )
    }
    
    cat(lista$listaReferencias$n[lista$listaReferencias$nombre == n1][1], "")
      
    j = try(BuscarReferencias(n1), silent = TRUE)
      
    if ("try-error" %in% class(j) | !is.list(j)){
      
      lista$listaReferencias$consultado[lista$listaReferencias$nombre == n1] = TRUE
      lista$listaReferencias$exito[lista$listaReferencias$nombre == n1] = FALSE
      
      cat(n1, "(página no encontrada)", convertirHoras((proc.time() - t)[3]), " - ")
      next
    }
    
    ahora = Sys.time()
    
    if ("referencias" %in% names(j)){
      
      ref1 = j$referencias
      
      lista$listaReferencias = lista$listaReferencias %>%
        mutate(n = n + ifelse(nombre %in% ref1, 1, 0))
      
      ref1 = dplyr::setdiff(ref1, lista$listaReferencias$nombre)
      
      if (length(ref1) > 0){
        
        lista$listaReferencias = dplyr::bind_rows(
          lista$listaReferencias, 
          data.frame(
            nombre = ref1, n = 1, consultado = FALSE, Descrito = FALSE, EsPersona = NA,
            origen = n1, agregado = ahora, Consultar = 2)
        )
        
      }
      
      j$referencias = NULL
      
    }
      
    lista$listaReferencias$consultado[lista$listaReferencias$nombre == n1] = TRUE
    lista$listaReferencias$exito[lista$listaReferencias$nombre == n1] = TRUE
      
    if (n1 %in% lista$listadoCaracteristicas$pagina){
        
      lista$listadoCaracteristicas = lista$listadoCaracteristicas %>% filter(pagina != n1)
        
    }
    
    if (is.list(j) & length(j) > 0){
      
      ahora = Sys.time()
      
      j = as.data.frame(j)
      j$agregadoCaracteristica = ahora
      
      lista$listadoCaracteristicas = plyr::rbind.fill(lista$listadoCaracteristicas, j)
      
    }
      
    Descritos = lista$listadoCaracteristicas %>% .$pagina
    Personas = lista$listadoCaracteristicas %>% 
      filter(!is.na(Fallecimiento) | !is.na(Nacimiento)) %>% .$pagina
    NoPersonas = lista$listadoCaracteristicas %>% 
      filter(is.na(Fallecimiento), is.na(Nacimiento)) %>% .$pagina
      
    lista$listaReferencias = lista$listaReferencias %>%
      mutate(
        Descrito = ifelse(nombre %in% Descritos, TRUE, Descrito), 
        EsPersona = case_when(
          nombre %in% Personas ~ TRUE,
          nombre %in% NoPersonas ~ FALSE,
          TRUE ~ EsPersona
        )) %>%
      mutate(Consultar = ifelse(!is.na(EsPersona) & EsPersona, 1, 2)) %>%
      arrange(Consultar, desc(n))
      
    cat(n1, convertirHoras((proc.time() - t)[3]), " - ")
    
  } 
  
  return(lista)
  
}

### Actualizar ----

Actualizar = function(lista, lista_actualizar, tiempo = 600, imprimirCat = FALSE){

  t = proc.time()
  
  for (n1 in lista_actualizar){
    
    if ((proc.time() - t)[3] > tiempo) break
    
    if (!n1 %in% lista$listaReferencias$nombre){
      lista$listaReferencias = plyr::rbind.fill(
        lista$listaReferencias,
        data.frame(nombre = n1, n = 0, consultado = FALSE)
      )
    }
      
    j = try(BuscarReferencias(n1, imprimircat = imprimirCat), silent = TRUE)
    
    if ("try-error" %in% class(j) | !is.list(j)){
      
      lista$listaReferencias$consultado[lista$listaReferencias$nombre == n1] = TRUE
      lista$listaReferencias$exito[lista$listaReferencias$nombre == n1] = FALSE
      
      cat(n1, "(página no encontrada)", convertirHoras((proc.time() - t)[3]), " - ")
      next
      
    }
    
    ahora = Sys.time()
    
    if ("referencias" %in% names(j)){
      
      ref1 = j$referencias
      
      lista$listaReferencias = lista$listaReferencias %>%
        mutate(n = n + ifelse(nombre %in% ref1, 1, 0))
      
      ref1 = dplyr::setdiff(ref1, lista$listaReferencias$nombre)
      
      if (length(ref1) > 0){
        
        lista$listaReferencias = dplyr::bind_rows(
          lista$listaReferencias, 
          data.frame(
            nombre = ref1, n = 1, consultado = FALSE, Descrito = FALSE, EsPersona = NA,
            origen = n1, agregado = ahora, Consultar = 2)
        )
      }
      
      j$referencias = NULL
      
    }
      
      
    lista$listaReferencias$consultado[lista$listaReferencias$nombre == n1] = TRUE
    lista$listaReferencias$exito[lista$listaReferencias$nombre == n1] = TRUE
      
    if (n1 %in% lista$listadoCaracteristicas$pagina){
        
      lista$listadoCaracteristicas = lista$listadoCaracteristicas %>% filter(pagina != n1)
        
    }
      
    ahora = Sys.time()
    
    if (is.list(j) & length(j) > 0){
      
      j = as.data.frame(j)
      j$agregadoCaracteristica = ahora
      
      lista$listadoCaracteristicas = plyr::rbind.fill(lista$listadoCaracteristicas, j)
      
    }
      
    Descritos = lista$listadoCaracteristicas %>% .$pagina
    Personas = lista$listadoCaracteristicas %>% 
      filter(!is.na(Fallecimiento) | !is.na(Nacimiento)) %>% .$pagina
    NoPersonas = lista$listadoCaracteristicas %>% 
      filter(is.na(Fallecimiento), is.na(Nacimiento)) %>% .$pagina
      
    lista$listaReferencias = lista$listaReferencias %>%
      mutate(
        Descrito = ifelse(nombre %in% Descritos, TRUE, Descrito), 
        EsPersona = case_when(
          nombre %in% Personas ~ TRUE,
          nombre %in% NoPersonas ~ FALSE,
          TRUE ~ EsPersona
        )) %>%
      mutate(Consultar = ifelse(!is.na(EsPersona) & EsPersona, 1, 2)) %>%
      arrange(Consultar, desc(n))
      
    cat(n1, convertirHoras((proc.time() - t)[3]), " - ")
      
  } 
  
  return(lista)
  
}

### Agregar descripciones ----

AgregarDescripciones = function(lista, MaximoPersonas = 15, tiempo = 600){
  
  t = proc.time()
  
  faltantes = lista$listaReferencias %>% arrange(desc(n)) %>% .$nombre
  faltantes = dplyr::setdiff(faltantes, lista$listadoCaracteristicas$pagina)
  
  CantidadPersonas = 0
  a = 1
  
  while (CantidadPersonas < MaximoPersonas) {
    
    if ((proc.time() - t)[3] > tiempo) break
    a = a + 1
    if (a > length(faltantes)) break
    
    i = faltantes[a]
      
    l1 = try(BuscarReferencias(i, AgregarReferencias = FALSE), silent = TRUE)
    
    if ("try-error" %in% class(l1) | !is.list(l1)){
      
      cat(i, "(página no encontrada)" ,convertirHoras((proc.time() - t)[3]), " - ")
      next
    }
     
    if (length(l1) == 0){
      
      cat(i, "(atributos no encontrados)", convertirHoras((proc.time() - t)[3]), " - ")
      next
    }
     
    l1 = as.data.frame(l1)
    
    ahora = Sys.time()
    l1$agregadoCaracteristica = ahora
      
    if (any(c("Nacimiento", "Fallecimiento") %in% variable.names(l1))){
      CantidadPersonas = CantidadPersonas + 1
      cat("\n\nPersona encontrada:", i, "\n\n")
    } 
      
    lista$listadoCaracteristicas = plyr::rbind.fill(lista$listadoCaracteristicas, l1)
    
    cat(i, convertirHoras((proc.time() - t)[3]), " - ")
    
  }
  
  return(lista)
  
}


ActualizarDescripciones = function(lista, nombre_pagina, tiempo = 600, soloPersonas = TRUE,
                                   AgregarRef = TRUE, imprimirNac = FALSE){
  
  nombre_pagina = nombre_pagina[!is.na(nombre_pagina)]
  
  lista = ForzarNuevas(
    lista = lista, nueva_referencia = nombre_pagina, tiempo = tiempo, 
    AgregarRef = AgregarRef)
  
  if (AgregarRef){
    
    j = BuscarReferencias(nombre_pagina = nombre_pagina, AgregarDatos = FALSE)
    
    refLista = j$referencias
    
  } else {
    refLista = nombre_pagina
  }

  t = proc.time()
  
  if (soloPersonas){
    
    Descritos = lista$listadoCaracteristicas %>% .$pagina
    Personas = lista$listadoCaracteristicas %>% 
      filter(!is.na(Fallecimiento) | !is.na(Nacimiento)) %>% .$pagina
    NoPersonas = lista$listadoCaracteristicas %>% 
      filter(is.na(Fallecimiento), is.na(Nacimiento)) %>% .$pagina
    
    lista$listaReferencias = lista$listaReferencias %>%
      mutate(
        Descrito = ifelse(nombre %in% Descritos, TRUE, Descrito), 
        EsPersona = case_when(
          nombre %in% Personas ~ TRUE,
          nombre %in% NoPersonas ~ FALSE,
          TRUE ~ EsPersona
        )) %>%
      mutate(Consultar = ifelse(!is.na(EsPersona) & EsPersona, 1, 2))
    
    Personas = lista$listaReferencias %>% filter(EsPersona) %>% .$nombre
    
    refLista = intersect(refLista, Personas)
    
  } 
  
  cat("\ninicio\n")
  
  for (i in refLista){
    
    if ((proc.time() - t)[3] > tiempo) break
    
    l1 = try(BuscarReferencias(i, AgregarReferencias = FALSE, marcar = FALSE), silent = TRUE)
    
    if ("try-error" %in% class(l1) | !is.list(l1)){
      
      cat(i, "(página no encontrada)", convertirHoras((proc.time() - t)[3]), " - ")
      next
    }
    
    if (length(l1) == 0){
      
      cat(i, "(atributos no encontrados)", convertirHoras((proc.time() - t)[3]), " - ")
      next
    }
    
    l1 = as.data.frame(l1)
      
    ahora = Sys.time()
    l1$agregadoCaracteristica = ahora
      
    if (i %in% lista$listadoCaracteristicas$pagina){
        
      lista$listadoCaracteristicas = lista$listadoCaracteristicas %>%
        filter(pagina != i)
    }
      
    lista$listadoCaracteristicas = plyr::rbind.fill(lista$listadoCaracteristicas, l1)
      
    if (imprimirNac){
      
      nac = try(l1$Nacimiento, silent = TRUE)
      
      if ("try-error" %in% class(nac)) nac = "No encontrado"
     
      cat(i, nac, convertirHoras((proc.time() - t)[3]), "\n")
       
    } else {
      
      cat(i, convertirHoras((proc.time() - t)[3]), " - ")
      
    }
    
  }
  
  return(lista)
  
}


