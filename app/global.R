
library(shiny)
library(shinyMobile)
library(tidyverse)
library(lubridate)

try(load("data/ListaEdades.RData"), silent = TRUE)
try(load("data/actualizacion.RData"), silent = TRUE)

try(load("app/data/ListaEdades.RData"), silent = TRUE)
try(load("app/data/actualizacion.RData"), silent = TRUE)

if (!"listaEdades" %in% ls(envir = .GlobalEnv)){
  try(load("data/ListaEdades - copia.RData"), silent = TRUE)
  try(load("app/data/ListaEdades - copia.RData"), silent = TRUE)
}
  
ahora = Sys.Date()
ahora2 = Sys.time()

cat("1")

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

cat("2")

### Profesiones ----

ListaProfesiones = list(
  Papas = list(
    Nombre = "Papas",
    Categorias = "C:Papas"
  ),
  Presidentes = list(
    Nombre = "Presidente/as, primero/as ministro/as y jefe/as de gobierno",
    Categorias = sub("Categor%C3%ADa:", "C:", refPr)
  ),
  Actores = list(
    Nombre = "Actores y actrices",
    Categorias = c("C:Actrices", "C:Actores")
  ),
  Futbolistas = list(
    Nombre = "Futbolistas",
    Categorias = c("C:Futbolistas")
  ),
  Reyes = list(
    Nombre = "Reyes, reinas, emperadores y emperatrices",
    Categorias = c("C:Reyes", "C:Reinas", "C:Emperadores", "C:Emperatrices"),
    Excluir = c(
      "C:Reyes_ficticios", "C:Reinas_del_Festival", "C:Reinas_guachaca", 
      "C:Reinas_de_la_Pampilla_de_Coquimbo", "C:Reyes_guachaca")
  ),
  Cantantes = list(
    Nombre = "Cantantes",
    Categorias = c("C:Cantantes", "C:Cantautores", "C:Cantautoras")
  ),
  Escritores = list(
    Nombre = "Escritore/as",
    Categorias = c("C:Escritores", "C:Escritoras", "C:Poetas", "C:Poetisas")
  ),
  Pensadores = list(
    Nombre = "Pensadore/as",
    Categorias = c("C:Fil%C3%B3sofos", "C:Fil%C3%B3sofas", "C:Pensadores", "C:Pensadoras")
  ),
  Periodistas = list(
    Nombre = "Periodistas",
    Categorias = c("C:Periodistas")
  ),
  Cientificos = list(
    Nombre = "Científico/as",
    Categorias = c("C:Cient%C3%ADficos", "C:Cient%C3%ADficas")
  ),
  Modelos = list(
    Nombre = "Modelos",
    Categorias = c("C:Modelos")
  ),
  Tenistas = list(
    Nombre = "Tenistas",
    Categorias = c("C:Tenistas")
  ),
  Deportistas = list(
    Nombre = "Deportistas",
    Categorias = unique(c(
      sub("Categor%C3%ADa:", "C:", refDeportistas), "C:Medallistas_ol%C3%ADmpicos",
      "C:Deportistas", "C:Atletas"))
  ),
  Sociologos = list(
    Nombre = "Sociólogo/as",
    Categorias = c("C:Soci%C3%B3logos", "C:Soci%C3%B3logas")
  ),
  Economistas = list(
    Nombre = "Economistas",
    Categorias = c("C:Economistas")
  ),
  Medicos = list(
    Nombre = "Médico/as",
    Categorias = unique(c(
      "C:Cardi%C3%B3logos", "C:Cirujanos", "C:M%C3%A9dicos", 
      "C:Cardi%C3%B3logas", "C:Cirujanas", "C:M%C3%A9dicas",
      sub("Categor%C3%ADa:", "C:", refMedicos)))
  ),
  Pintor = list(
    Nombre = "Pintore/as y escultore/as",
    Categorias = c(
      "C:Pintores", "C:Pintoras", "C:Escultores", "C:Escultoras"
    )
  )
)

ListaProfesiones$Politicos = list(
  Nombre = "Político/as",
  Categorias = unique(c(
    "C:Pol%C3%ADticos", "C:Pol%C3%ADticas", "C:Legisladores", "C:Diputados", "C:Senadores",
    ListaProfesiones$Presidentes$Categorias))
)

ListaProfesiones$Musicos = list(
  Nombre = "Músicos",
  Categorias = unique(c(
    ListaProfesiones$Cantantes$Categorias, "C:Compositores", "C:M%C3%BAsicos", 
    sub("Categor%C3%ADa:", "C:", refInstrumentos)))
)

categoriasProfesiones = names(ListaProfesiones)
names(categoriasProfesiones) = sapply(ListaProfesiones, function(x) x$Nombre)
categoriasProfesiones = categoriasProfesiones[sort(names(categoriasProfesiones))]

for (i in 1:length(ListaProfesiones)){
  
  ListaProfesiones[[i]]$Categorias = unique(c(
    ListaProfesiones[[i]]$Categorias, URLdecode(ListaProfesiones[[i]]$Categorias)))
  
}


### Nacionalidades ----

pal = which(sapply(ListaNacionalidades, function(x) x$Nombre) == "Palestina")
ListaNacionalidades[[pal]]$Excluir = c(
  ListaNacionalidades[[pal]]$Excluir,
  "C:Emigrantes_hacia_el_Mandato_brit%C3%A1nico_de_Palestina",
  "C:Jud%C3%ADos_del_Mandato_brit%C3%A1nico_de_Palestina",
  paste0(
    "C:Activistas_por_Palestina_de_", 
    c("Estados_Unidos", "Argentina", "Israel", "Reino_Unido")),
  paste0(
    "C:Emigrantes_",
    c("alemanes", "austr%C3%ADacos", "checoslovacos", "egipcios", "polacos", 
      "sovi%C3%A9ticos", "rusos", "ucranianos"),
    "_hacia_el_Mandato_brit%C3%A1nico_de_Palestina"))

pal = which(sapply(ListaNacionalidades, function(x) x$Nombre) == "Reino Unido")
ListaNacionalidades[[pal]]$Excluir = c(
  ListaNacionalidades[[pal]]$Excluir,
  "premio_%C3%93scar_a_la_mejor_pel%C3%ADcula_en_lengua_no_inglesa",
  "C:Miembros_extranjeros_de_la_Royal_Society"
)

pal = which(sapply(ListaNacionalidades, function(x) x$Nombre) == "Italia")
ListaNacionalidades[[pal]]$Excluir = c(
  ListaNacionalidades[[pal]]$Excluir,
  "C:Miembros_extranjeros_de_la_Academia_Nacional_de_Ciencias_de_Italia"
)

pal = which(sapply(ListaNacionalidades, function(x) x$Nombre) == "Japón")

ListaNacionalidades[[pal]]$Excluir = c(
  ListaNacionalidades[[pal]]$Excluir,
  "C:Miembros_honorarios_de_la_Academia_Japonesa"
)


pal = which(sapply(ListaNacionalidades, function(x) x$Nombre) == "Georgia")

ListaNacionalidades[[pal]]$Excluir = c(
  ListaNacionalidades[[pal]]$Excluir,
  "Georgia_\\(Estados_Unidos\\)", "C:Personas_de_la_%C3%A9poca_georgiana",
  "C:Mujeres_de_la_%C3%A9poca_georgiana",
  "C:Deportistas_de_la_Universidad_de_Georgia",
  "C:Baloncestistas_de_los_Georgia_Bulldogs",
  "C:Golfistas_de_Georgia_Bulldogs",
  "C:Tenistas_de_Georgia_Bulldogs",
  "C:Georgia_Bulldogs",
  "C:Alumnado_de_la_Universidad_de_Georgia",
  "C:Baloncestistas_de_los_Georgia_Tech_Yellow_Jackets",
  "C:Tenistas_masculinos_de_Georgia_Tech_Yellow_Jackets",
  "C:Tenistas_femeninos_de_Georgia_Tech_Yellow_Jackets",
  "C:Deportistas_del_Instituto_de_Tecnolog%C3%ADa_de_Georgia",
  "C:Jugadores_de_f%C3%BAtbol_americano_de_Georgia_Tech_Yellow_Jackets",
  "C:Tenistas_masculinos_de_Georgia_Bulldogs",
  "C:Deportistas_de_la_Universidad_de_Georgia",
  "C:Baloncestistas_de_los_Georgia_Bulldogs",
  "C:Golfistas_de_Georgia_Bulldogs",
  "C:Golfistas_masculinos_de_Georgia_Bulldogs",
  "C:Tenistas_de_Georgia_Bulldogs",
  "C:Georgia_Bulldogs",
  "C:Grandes_collares_de_la_Orden_del_%C3%81guila_de_Georgia",
  "C:Miembros_de_la_C%C3%A1mara_de_Representantes_de_Estados_Unidos_por_Georgia",
  "C:Alumnado_de_la_Universidad_de_Georgia",
  "C:Profesores_del_Instituto_de_Tecnolog%C3%ADa_de_Georgia",
  "C:Alumnado_del_Instituto_de_Tecnolog%C3%ADa_de_Georgia",
  "C:Dem%C3%B3cratas_de_Georgia",
  "C:Miembros_de_la_C%C3%A1mara_de_Representantes_de_Georgia",
  "C:Senadores_de_Estados_Unidos_por_Georgia",
  "C:Nacidos_en_Atlanta",
  "C:Republicanos_de_Georgia",
  "C:Profesores_de_la_Universidad_de_Georgia",
  "C:Alumnado_de_la_Universidad_de_Georgia",
  "C:Baloncestistas_de_los_Georgia_Southern_Eagles",
  "C:Baloncestistas_de_los_Georgia_State_Panthers"
)


categoriasNacionalidades = names(ListaNacionalidades)
names(categoriasNacionalidades) = sapply(ListaNacionalidades, function(x) x$Nombre)
categoriasNacionalidades = categoriasNacionalidades[sort(names(categoriasNacionalidades))]

cat("3")
### Informacion contacto ----

InformacionContacto = list(
  div(
    style = 'padding-right:20px; margin-right:3px;
                        padding-left:25px; margin-left:5px;
                        padding-top:0px; margin-top:5px;
                        padding-bottom:20px; margin-bottom:5px;',
    HTML(
      '<a style = "font-size:20px; color:black"> Cualquier consulta dirigirse a </a>
       <a style = "font-size:20px; color:blue" 
        href = "mailto:patriciorojze@gmail.com?subject=Consulta"
        target = "_blank">
        patriciorojze@gmail.com </a> <br>
       <a style = "font-size:20px; color:black"> Contacto LinkedIn: </a>
       <a style = "font-size:20px; color:blue" 
        href = "https://www.linkedin.com/in/patricio-rojze/"
        target = "_blank">
        patricio-rojze </a><br>
       <a style = "font-size:20px; color:black"> Contacto GitHub: </a>
       <a style = "font-size:20px; color:blue" 
        href = "https://github.com/patriciorojze"
        target = "_blank">
        patriciorojze </a>'
    )
  )
)
