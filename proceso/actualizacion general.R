
source("proceso/extraer - funciones.R")

try(load("data/ListaReferencias.RData"))

### Agregar ref de generos -----

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

for (i in listaGeneros){
  
  lista = Actualizar(lista, lista_actualizar = i, tiempo = 2700, imprimirCat = TRUE)
  save(lista, file = "data/ListaReferencias.RData")
  
}

source("proceso/proceso.R")

save.image()


### Caracterizar como personas ----

lista$listaReferencias = lista$listaReferencias %>%
  mutate(EsPersona = case_when(
    !is.na(EsPersona) ~ EsPersona,
    origen %in% listaGeneros ~ TRUE,
    grepl("Categor%C3%ADa:Nacidos_en", origen) ~ TRUE,
    TRUE ~ EsPersona
  ))

try(load("data/ListaGenero.RData"))

if (!"ListaGenero" %in% ls(.GlobalEnv)){
  ListaGenero = lista$listaReferencias %>%
    filter(EsPersona) %>% select(nombre, n) %>%
    mutate(Genero = "")
}

ListaGenero = dplyr::bind_rows(
  ListaGenero, 
  lista$listaReferencias %>% 
    filter(EsPersona, !nombre %in% ListaGenero$nombre) %>% 
    select(nombre, n) %>% mutate(Genero = "")
)

ListaGenero = merge(
  ListaGenero, 
  lista$listaReferencias %>% select(nombre, n), 
  by = "nombre", all.x = TRUE, suffixes = c("", "_1")) %>%
  mutate(n = pmax(n, n_1, na.rm = TRUE)) %>%
  select(-c(n_1)) %>% arrange(desc(n))


ListaHombres = lista$listaReferencias %>% filter(origen == "Categoría:Hombres") %>% .$nombre
ListaMujeres = lista$listaReferencias %>% filter(origen == "Categoría:Mujeres") %>% .$nombre
ListaHombreTrans = lista$listaReferencias %>% 
  filter(origen == "Categoría:Hombres transgénero") %>% .$nombre
ListaMujerTrans = lista$listaReferencias %>% 
  filter(origen == "Categoría:Mujeres transgénero") %>% .$nombre
ListaNoBin = lista$listaReferencias %>% 
  filter(origen %in% c(
    "Categoría:Personas_no_binarias",
    "Categoría:Personas de género fluido",
    "Categoría:Personas agénero",
    "Categoría:Personas dos espíritus")) %>% .$nombre

ListaGenero = ListaGenero %>%
  mutate(Genero = case_when(
    is.na(Genero) & Genero != "" ~ Genero,
    nombre %in% ListaHombres ~ "Hombre",
    nombre %in% ListaMujeres ~ "Mujer",
    nombre %in% ListaNoBin ~ "Persona no binaria",
    nombre %in% ListaHombreTrans ~ "Hombre transgénero",
    nombre %in% ListaMujerTrans ~ "Mujer transgénero",
    TRUE ~ Genero
  ))

save(ListaGenero, file = "data/ListaGenero.RData")  
save(lista, file = "data/ListaReferencias.RData")


### Corregir ----

problemas = listaEdades %>% filter(genero == "Sin especificar") %>% arrange(desc(fecha_nac))

problemas = c(
  'Silvio_Zavala',
  'Anthony_Asquith',
  'Francisco_Mugica',
  'Manoel_de_Oliveira',
  'George_Abbott',
  'Jacques_Feyder',
  'Miguel_Morayta_Martínez',
  'Rafael_Arévalo_Martínez',
  'Norman_Lloyd',
  'Jeanne_Calment',
  'Rosa_Tarlovsky_de_Roisinblit',
  'Ana_Carmen_Macri',
  'Lucile_Randon',
  'John_Gould_Fletcher',
  'Kane_Tanaka',
  'Sodimejo',
  'Jirōemon_Kimura'
)

try(load("data/ListaReferencias.RData"))

lista = ActualizarDescripciones(
  lista = lista, nombre_pagina = problemas$pagina, AgregarRef = FALSE, tiempo = 1800)

save(lista, file = "data/ListaReferencias.RData")

### Crear Lista Personas ----

try(load("data/ListaReferencias.RData"))
source("proceso/extraer - funciones.R", encoding = "UTF-8")


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
  
  lista$listaReferencias = lista$listaReferencias %>%
    mutate(EsPersona = case_when(
      !is.na(EsPersona) ~ EsPersona,
      nombre %in% l$referencias ~ TRUE,
      TRUE ~ NA
    ))
}

save(lista, file = "data/ListaReferencias.RData")
save(listaPersonas, file = "data extra/ListaPersonas.RData")

source("proceso/proceso.R", encoding = "UTF-8")

try(load("data/ListaGenero.RData"))
try(load("data extra/ListaPersonas.RData"))

if (!"ListaGenero" %in% ls(.GlobalEnv)){
  ListaGenero = lista$listaReferencias %>%
    filter(EsPersona) %>% select(nombre, n, EsPersona) %>%
    mutate(Genero = "")
}

ListaGenero = dplyr::bind_rows(
  ListaGenero, 
  lista$listaReferencias %>% 
    filter(EsPersona, !nombre %in% ListaGenero$nombre) %>% 
    select(nombre, n, EsPersona) %>% mutate(Genero = "")
)

ListaGenero = merge(
  ListaGenero, 
  lista$listaReferencias %>% select(nombre, n, EsPersona), 
  by = "nombre", all.x = TRUE, suffixes = c("", "_1")) %>%
  mutate(
    n = pmax(n, n_1, na.rm = TRUE), 
    EsPersona = coalesce(EsPersona_1, EsPersona)) %>%
  select(-c(n_1, EsPersona_1)) %>% arrange(desc(n))

ListaGenero$Descrip = (ListaGenero$nombre %in% lista$listadoCaracteristicas$pagina)

ListaGenero = ListaGenero %>% arrange(desc(EsPersona), desc(Descrip), desc(n))

indice = sapply(listaPersonas, function(x) x$pagina)
names(listaPersonas) = indice

ListaHombres = listaPersonas$`Categoría:Hombres`$referencias
ListaMujeres = listaPersonas$`Categoría:Mujeres`$referencias
ListaHombreTrans = listaPersonas$`Categoría:Hombres_transgénero`$referencias
ListaMujerTrans = listaPersonas$`Categoría:Mujeres_transgénero`$referencias
ListaNoBin = listaPersonas$`Categoría:Personas_no_binarias`$referencias

ListaGenero = ListaGenero %>%
  mutate(Genero = case_when(
    is.na(Genero) & Genero != "" ~ Genero,
    nombre %in% ListaHombres ~ "Hombre",
    nombre %in% ListaMujeres ~ "Mujer",
    nombre %in% ListaNoBin ~ "Persona no binaria",
    nombre %in% ListaHombreTrans ~ "Hombre transgénero",
    nombre %in% ListaMujerTrans ~ "Mujer transgénero",
    TRUE ~ Genero
  ))

save(ListaGenero, file = "data/ListaGenero.RData")  


### fecha de agregar caracteristicas ----

library(tidyverse)

try(load("data/ListaReferencias.RData"))

g = lista$listaReferencias %>% group_by(origen) %>% 
  summarise(agregado = min(agregado, na.rm = TRUE)) %>% 
  ungroup() %>% as.data.frame() %>%
  filter(!is.infinite(agregado))


lista$listadoCaracteristicas = merge(
  lista$listadoCaracteristicas, g,
  by.x = "pagina", by.y = "origen", all.x = TRUE, suffixes = c("", "_1")
)

lista$listadoCaracteristicas$agregado = as.POSIXct(
  lista$listadoCaracteristicas$agregado, tz = "America/Argentina/Buenos_Aires")

lista$listadoCaracteristicas = lista$listadoCaracteristicas %>%
  mutate(agregadoCaracteristica = coalesce(agregadoCaracteristica, agregado)) %>%
  select(-c(agregado))

save(lista, file = "data/ListaReferencias.RData")


### Corregir descripciones ----

try(load("data/ListaReferencias.RData"))

cambiar = listaEdades %>% 
  filter(regexpr("<p><span id=\"WikipediaGrabada", descripcion) == 1) %>%
  arrange(desc(n))


lista = ActualizarDescripciones(
  lista, nombre_pagina = c("Máximo_Kirchner", cambiar$pagina), 
  AgregarRef = FALSE, tiempo = 1500)

save(lista, file = "data/ListaReferencias.RData")


try(load("data/ListaReferencias.RData"))

meses = c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre",
  "octubre", "noviembre", "diciembre")

patron = paste0("\\b(", paste(meses, collapse = "|"), ")\\s+(de|del)\\s+\\d{1,4}\\b")


desc_corta <- substr(lista$listadoCaracteristicas$descripcion, 1, 120)

t1 <- lista$listadoCaracteristicas[
  is.na(lista$listadoCaracteristicas$Nacimiento) &
    grepl(patron, desc_corta, ignore.case = TRUE, perl = TRUE),
]

t1 = merge(
  t1, 
  lista$listaReferencias %>% 
    group_by(nombre) %>% summarise(n = sum(n, na.rm = TRUE)) %>% as.data.frame(),
  by.x = "pagina", by.y = "nombre", all.x = TRUE)

t1 = t1 %>% arrange(desc(n)) %>%
  filter(
    regexpr("Anexo", titulo) != 1, regexpr("Ataque", titulo) != 1, 
    regexpr("Atentado", titulo) != 1, regexpr("Cumbre", titulo) != 1,
    regexpr("Golpe de Estado", titulo) != 1, regexpr("Guerra", titulo) != 1,
    regexpr("Ley", titulo) != 1, regexpr("Masacre", titulo) != 1,
    regexpr("Motín", titulo) != 1, regexpr("Muerte y funeral", titulo) != 1,
    regexpr("Naufragio", titulo) != 1, regexpr("NJPW", titulo) != 1,
    regexpr("WWE", titulo) != 1, regexpr("NXT", titulo) != 1,
    regexpr("Ofensiva", titulo) != 1, regexpr("Ofensiva", titulo) != 1,
    regexpr("Ola de calor", titulo) != 1, regexpr("Partido", titulo) != 1,
    regexpr("Protesta", titulo) != 1, regexpr("Revolución", titulo) != 1,
    regexpr("Protocolo", titulo) != 1, regexpr("Provincia", titulo) != 1, 
    regexpr("Secuestro", titulo) != 1, regexpr("Sitio", titulo) != 1,
    regexpr("Terremoto", titulo) != 1, regexpr("Tiroteo", titulo) != 1,
    regexpr("Tragedia", titulo) != 1, regexpr("Tratado", titulo) != 1,
    regexpr("Vuelo", titulo) != 1, regexpr("Colisión", titulo) != 1,
    regexpr("WCW", titulo) != 1, regexpr("WWF", titulo) != 1,
    regexpr("Desaparición", titulo) != 1, regexpr("Ejército", titulo) != 1,
    regexpr("Incendio", titulo) != 1, regexpr("Explosión", titulo) != 1,
    regexpr("Inundaciones", titulo) != 1, regexpr("Invasión", titulo) != 1)

openxlsx::write.xlsx(t1, file = "Con nacimientos.xlsx", rownames = FALSE, overwrite = TRUE)

lista = ActualizarDescripciones(
  lista, nombre_pagina = t1$pagina, AgregarRef = FALSE, tiempo = 300)

save(lista, file = "data/ListaReferencias.RData")


### Corregir Listado ----

try(load("data/ListaReferencias.RData"))

cambiar = lista$listadoCaracteristicas %>%
  filter(is.na(descripcion), !is.na(Nombre)) %>%
  merge(
    ., lista$listaReferencias %>% select(n, nombre),
    by.x = "pagina", by.y = "nombre", all.x = TRUE) %>%
  arrange(desc(n))

lista = ActualizarDescripciones(
  lista, nombre_pagina = cambiar$pagina, AgregarRef = FALSE, tiempo = 300)

save(lista, file = "data/ListaReferencias.RData")


try(load("data/ListaReferencias.RData"))

cambiar = lista$listadoCaracteristicas %>%
  filter(
    grepl(
      "(apodo|nombre artístico|seudónimo|(popularmente|también) conocid(o|a)|apodad(o|a)|más conocid(o|a) como|conocid(o|a) artísticamente|conocid(o|a) popularmente)", 
      descripcion, ignore.case = TRUE, perl = TRUE), 
    agregadoCaracteristica < as_datetime("2026-01-27 00:00:00"), 
    !is.na(Nacimiento)) %>%
  merge(
    ., lista$listaReferencias %>% select(n, nombre),
    by.x = "pagina", by.y = "nombre", all.x = TRUE) %>%
  arrange(desc(n))

lista = ActualizarDescripciones(
  lista, nombre_pagina = cambiar$pagina, AgregarRef = FALSE, tiempo = 1200)

save(lista, file = "data/ListaReferencias.RData")

### Contribuciones mias ----


pagina = try(suppressWarnings(scan(
  paste0("https://es.wikipedia.org/w/index.php?title=Especial:Contribuciones/",
         "Patriciohernan89&target=Patriciohernan89&offset=&limit=500"),
  what = "c", sep = "\n",
  encoding = "UTF-8", quiet = TRUE, skip = 22)), silent = TRUE)


referencias = unlist(
  regmatches(
    pagina,
    gregexpr('href="/wiki/[^"]+"', pagina)
  )
)

referencias = gsub('href="/wiki/|\"', '', referencias)
referencias = substr(
  referencias, 1, 
  ifelse(grepl("#", referencias), regexpr("#", referencias) - 1, nchar(referencias)))

referencias = unique(referencias)
referencias = referencias[!grepl("Wiki", referencias)]
referencias = referencias[!grepl("Portal:", referencias)]
referencias = referencias[!grepl("Especial:", referencias)]
referencias = referencias[!grepl("Ayuda:", referencias)]
referencias = referencias[!grepl("Archivo:", referencias)]
referencias = referencias[!grepl("Usuario", referencias)]

try(load("data/ListaReferencias.RData"))

lista = ActualizarDescripciones(
  lista, nombre_pagina = referencias, 
  AgregarRef = FALSE, tiempo = 180, soloPersonas = FALSE)

save(lista, file = "data/ListaReferencias.RData")


### Fechas raras ----

try(load("data/ListaReferencias.RData"))

cambiar = lista$listadoCaracteristicas %>%
  merge(
    ., lista$listaReferencias %>% select(n, nombre, EsPersona),
    by.x = "pagina", by.y = "nombre", all.x = TRUE) %>%
  filter(!is.na(Nacimiento) | !is.na(Fallecimiento) | EsPersona) %>%
  mutate(fecha_nac = TextoAFecha(Nacimiento), fecha_fac = TextoAFecha(Fallecimiento)) %>%
  filter(is.na(fecha_nac) | is.na(fecha_fac) & !is.na(Fallecimiento) |
           !is.na(fecha_nac) & fecha_nac == fecha_fac) %>%
  arrange(desc(n)) 

# cambiar = cambiar %>%
#   filter(year(agregadoCaracteristica) == 2025 | day(agregadoCaracteristica) < 19)

lista = ActualizarDescripciones(
  lista, nombre_pagina = cambiar$pagina, AgregarRef = FALSE, tiempo = 10800, 
  soloPersonas = FALSE)

save(lista, file = "data/ListaReferencias.RData")

try(load("data/ListaReferencias.RData"))

cambiar = lista$listadoCaracteristicas %>%
  merge(
    ., lista$listaReferencias %>% select(n, nombre, EsPersona),
    by.x = "pagina", by.y = "nombre", all.x = TRUE) %>%
  filter(
    !is.na(Nacimiento1) & is.na(Nacimiento) | 
      !is.na(Fallecimiento1) & is.na(Fallecimiento)) %>%
  arrange(desc(n)) 


lista = ActualizarDescripciones(
  lista, nombre_pagina = cambiar$pagina, AgregarRef = FALSE, tiempo = 1800, 
  soloPersonas = FALSE)

save(lista, file = "data/ListaReferencias.RData")


### Agregar generos y categorias ----

try(load("data/ListaReferencias.RData"))
try(load("data/ListaGenero.RData"))
try(load(file = "data/ListaPres.RData"))


listadoCaracteristicas1 = merge(
  lista$listadoCaracteristicas,
  ListaGenero %>% select(nombre, Genero) %>%
    filter(!is.na(nombre), !is.na(Genero), nombre != "", Genero != ""), 
  by.x = "pagina", by.y = "nombre", all.x = TRUE
)

listadoCaracteristicas1 = merge(
  listadoCaracteristicas1,
  listaTitulos %>% select(titulo, categorias) %>% 
    filter(!is.na(titulo), categorias != "", !is.na(categorias), titulo != ""), 
  by.x = "titulo", by.y = "titulo", all.x = TRUE
)

lista$listadoCaracteristicas = listadoCaracteristicas1

save(lista, file = "data/ListaReferencias.RData")

### Asignacion nacionalidad ----

# listaEdades %>% arrange(desc(n)) %>% slice(1:1000) %>% 
#   openxlsx::write.xlsx(., file = "data/importantes.xlsx", rownames = FALSE)

j = BuscarReferencias(
  nombre_pagina = "Categoría:Miembros_de_la_Real_Academia_de_las_Ciencias_de_Suecia", 
  AgregarDatos = FALSE)

AcademiaSueca = data.frame()

for (i in j$referencias){
  
  AcademiaSueca = dplyr::bind_rows(
    AcademiaSueca, BuscarReferencias(i, AgregarReferencias = FALSE))
  
  cat(i, "-")
}


AcademiaSueca %>%
  filter(
    descripcionCorta != "", 
    !grepl("(sueco|sueca|suecia)", descripcionCorta, perl = TRUE, ignore.case = TRUE), 
    titulo != "Bert Allard", !is.na(Nacimiento)) %>%
  write.table(
    ., file = "data/correcciones/AcademiaSueca.txt", row.names = FALSE, col.names = TRUE, 
    append = TRUE)


j = BuscarReferencias(
  nombre_pagina = "Categoría:Miembros_de_la_Real_Academia_Danesa_de_Ciencias_y_Letras", 
  AgregarDatos = FALSE)

AcademiaDanesa = data.frame()

for (i in j$referencias){
  
  AcademiaDanesa = dplyr::bind_rows(
    AcademiaDanesa, BuscarReferencias(i, AgregarReferencias = FALSE))
  
  cat(i, "-")
}


AcademiaDanesa %>%
  filter(
    descripcionCorta != "", 
    !grepl("(danés|danesa|dinamarca)", descripcionCorta, perl = TRUE, ignore.case = TRUE), 
    titulo != "Johannes Schmidt", !is.na(Nacimiento)) %>%
  write.table(
    ., file = "data/correcciones/AcademiaDanesa.txt", row.names = FALSE, col.names = TRUE, 
    append = TRUE)


### Corregir nacimientos ----

load("data extra/ListaPersonas.RData")
try(load("data/ListaReferencias.RData"))

listaCambiar = merge(
  lista$listadoCaracteristicas, 
  lista$listaReferencias %>% select(n, nombre, EsPersona),
  by.x = "pagina", by.y = "nombre", all.x = TRUE)

listaCambiar = listaCambiar %>% 
  filter(
    pagina %in% unlist(lapply(listaPersonas, function(x) x$referencias))|
      !is.na(Nacimiento1) | !is.na(Fallecimiento) | !is.na(Fallecimiento1), 
    is.na(Nacimiento)) %>%
  filter(regexpr("Categor", pagina) != 1) %>%
  arrange(desc(n))

lista = ActualizarDescripciones(
  lista, nombre_pagina = listaCambiar$pagina, AgregarRef = FALSE, tiempo = 9000, 
  soloPersonas = FALSE)

save(lista, file = "data/ListaReferencias.RData")


### Tratar de vuelta los sin exito ----

try(load("data/ListaReferencias.RData"))

listaCambiar = lista$listaReferencias %>% filter(!exito)

lista$listaReferencias = lista$listaReferencias %>% filter(exito | !consultado)

lista = ForzarNuevas(lista, listaCambiar$nombre, AgregarRef = FALSE)

save(lista, file = "data/ListaReferencias.RData")


### Fallecidos viejos ----

try(load("data/ListaReferencias.RData"))


lista = ActualizarDescripciones(
  lista, nombre_pagina = "Anexo:Fallecidos_en_enero_de_2026", 
  AgregarRef = TRUE, tiempo = 1500)

lista = ActualizarDescripciones(
  lista, nombre_pagina = "Anexo:Fallecidos_en_diciembre_de_2025", 
  AgregarRef = TRUE, tiempo = 1500)

lista = ActualizarDescripciones(
  lista, nombre_pagina = "Anexo:Fallecidos_en_noviembre_de_2025", 
  AgregarRef = TRUE, tiempo = 1500)

save(lista, file = "data/ListaReferencias.RData")


