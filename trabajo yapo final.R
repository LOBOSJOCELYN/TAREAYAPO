setwd("/Users/jocel/Desktop/Universidad/bigdata")

#Librerias
library("xml2")
library("rvest")
library("ggplot2")

# SUBSITIO

ObtenerTipo <- function(linkSubPagina){
  ResultadoTipo <- NA
  
  SubPaginaYapo <- read_html(linkSubPagina)
  
  BoxProductoYapo <- html_nodes(SubPaginaYapo, css=".info")
  
  # Tipo de transacción
  
  detailProductoYapo <- html_nodes(BoxProductoYapo, css=".details")
  trDetailsProductos <- html_nodes(detailProductoYapo,css="tr")
  
  for (detail in trDetailsProductos) {
    thDetail <- html_nodes(detail, css="th")
    if(html_text(thDetail)=="Tipo"){
      tdDetail <- html_nodes(detail,css="td")
      print(html_text(tdDetail))
      ResultadoTipo <- html_text(tdDetail)
    }
  }
  return(ResultadoTipo)
}


# CARGANDO ARCHIVOS

if(file.exists("DatosYapo.csv")){
  DatosEnArchivosCsv <- read.csv("DatosYapo.csv")
  # Borrar columna X
  DatosEnArchivosCsv["X"] <- NULL
}
seCargoAntiguo=FALSE
if(file.exists("DatosYapo2.csv")){
  DatosEnArchivosCsv2 <- read.csv2("DatosYapo2.csv")
  # Borrar columna X
  DatosEnArchivosCsv2["X"] <- NULL
  seCargoAntiguo=TRUE
}

#ASIGNACION DE VALORES
valorUF <- 30887.21
precioNormal <- c()
PrecioCalculado <- c()
vectorValorUF <- c()
tipoMoneda <- c()
tipoTransaccion <- c()
#IDEALMENTE HUBIESEMOS LEIDO HASTA LA PAGINA 421 
#PARA ABARCAR LOS 3 DIAS EN YAPO PERO MI PC NO PUDO CORRER ESA CANTIDAD DE DATOS
for (pagina in 1:3) {
  #leyendo pagina
  
  PaginaYapo <- read_html(paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",pagina,sep = ""))
  print(html_text(PaginaYapo))
  
  # Clase listing_thumbs
  listingThumbs <- html_nodes(PaginaYapo, css=".listing_thumbs")
  print(html_text(listingThumbs))
  
  #####OBTENIENDO LAS VARIABLES#######
  # Obteniendo los titulos de los productos
  nombresProductos <- html_nodes(listingThumbs, css=".title")
  print(html_text(nombresProductos))
  
  #Obteniendo Fecha de los productos
  Fechaproductos <- html_nodes(listingThumbs, css=".date")
  print(html_text(Fechaproductos))
  
  # Obteniendo los categoria de los productos
  categoriaProductos <- html_nodes(listingThumbs, css=".category")
  print(html_text(categoriaProductos))
  
  
  # Obteniendo los precios
  preciosProductos <- html_nodes(listingThumbs, css=".price")
  print(html_text(preciosProductos))
  
  # Obteniendo la region de los productos
  regionProductos <- html_nodes(listingThumbs, css=".region")
  print(html_text(regionProductos))
  
  
  # Obteniendo las comunas de los productos
  comunaProductos <- html_nodes(listingThumbs, css=".commune")
  print(html_text(comunaProductos))
  
  # Obteniendo el link de los productos
  
  seccionImagenProductoYapo <- html_nodes(listingThumbs,css=".listing_thumbs_image")
  linkProductoYapo <- html_nodes(seccionImagenProductoYapo,css="a")
  hrefProductoYapo <- html_attr(linkProductoYapo,"href")
  
  
  
  for (elemento in 2:length(listingThumbs)) {
    print(paste("################Elemento", elemento-1, "- pagina", pagina, "###############"))
    print(html_text(nombresProductos[elemento-1]))
    
    # Obteniendo tipo
    tipo <- ObtenerTipo (hrefProductoYapo[elemento-1])
    tipoTransaccion <- c(tipoTransaccion,tipo)
    precio <- html_nodes(listingThumbs[elemento],css=".price")
    if(length(precio)>0){
      precio <- html_text(precio)
      precio <- gsub("\t","",precio)
      precio <- gsub("\n","",precio)
      precio <- gsub("[$]","",precio)
      precio <- gsub("[.]","",precio)
      precio <- gsub(",",".",precio)
      precio <- gsub(" ","",precio)
      
      if(substr(precio, 1, 2) == 'UF'){
        precio <- gsub("UF","",precio)
        precio <- as.numeric(precio)
        
        # guardando precio normal
        precioNormal <- c(precioNormal,precio)
        
        precio <- precio*valorUF
        PrecioCalculado <- c(PrecioCalculado,precio)
        vectorValorUF <- c(vectorValorUF, valorUF)
        tipoMoneda <- c(tipoMoneda,"UF")
      }else{
        precio <- as.numeric(precio)
        precioNormal <- c(precioNormal,precio)
        PrecioCalculado <- c(PrecioCalculado,NA)
        vectorValorUF <- c(vectorValorUF, NA)
        tipoMoneda <- c(tipoMoneda,"peso")
      }
    }else{
      precio <- NA
      precioNormal <- c(precioNormal,precio)
      PrecioCalculado <- c(PrecioCalculado,NA)
      vectorValorUF <- c(vectorValorUF, NA)
      tipoMoneda <- c(tipoMoneda,NA)
    }
    print(precio)
  }
  
}


# CREAMOS DATA FRAME

nuestrosDatos <- data.frame(nombre=html_text(nombresProductos),
                            categoria=html_text(categoriaProductos),
                            region=html_text(regionProductos),
                            comuna=html_text(comunaProductos),
                            tipoTransaccion=tipoTransaccion,
                            precioNormal=precioNormal,
                            PrecioCalculado=PrecioCalculado,
                            tipoMoneda=tipoMoneda,
                            valorUF=vectorValorUF,
                            linkProducto=hrefProductoYapo)

#SE UNE INFORMACION

if(seCargoAntiguo){
  nuestrosDatos <- rbind(DatosEnArchivosCsv,nuestrosDatos)
}


write.csv(nuestrosDatos,"DatosYapo.csv")
write.csv2(nuestrosDatos,"DatosYapo2.csv")

#duplicados

distinct(nuestrosDatos)
nrow(nuestrosDatos[duplicated(nuestrosDatos), ])
nuestrosDatos[duplicated(nuestrosDatos), ]

#Revisando NA y NULL

sapply(nuestrosDatos, function(x) sum(is.na(x)))
sapply(nuestrosDatos,function(x) sum(is.null(x)))

#Limpiando NA

nuestrosDatos <-nuestrosDatos[!is.na(nuestrosDatos$tipoTransaccion),]
nuestrosDatos <-nuestrosDatos[!is.na(nuestrosDatos$precioNormal),]
nuestrosDatos <-nuestrosDatos[!is.na(nuestrosDatos$PrecioCalculado),]
nuestrosDatos <-nuestrosDatos[!is.na(nuestrosDatos$tipoMoneda),]
nuestrosDatos <-nuestrosDatos[!is.na(nuestrosDatos$valorUF),]


setwd("~/GitHub/Bigdata2021")
install.packages("ggplot2")
library(ggplot2)
library(graphics)


###CREACION DE TABLE##
table(DatosEnArchivosCsv2["categoria"])
table(DatosEnArchivosCsv2["region"])
table(DatosEnArchivosCsv2["comuna"])
table(DatosEnArchivosCsv2["tipoMoneda"])
table(DatosEnArchivosCsv2["tipoTransaccion"])

#######DIFERENTES TIPOS DE GRAFICOS############ 


#GGPLOT
ggplot(DatosEnArchivosCsv2,aes(x=tipoTransaccion))+
  geom_bar() +
  coord_flip()

ggplot(DatosEnArchivosCsv2,aes(x=comuna))+
  geom_bar() +
  coord_flip()

# BOXPLOT TRANSACCION Y COMUNAS Y COMUNAS

ggplot(DatosEnArchivosCsv2,aes(x=comuna, y=tipoTransaccion))+
  geom_boxplot() +
  coord_flip()



#GRAFICO CAJA Y BIGOTE
boxplot(PrecioCalculado~tipoMoneda,col="red",xlab="tipoMoneda",ylab="Calculado",
        main="Grafico caja y bigote")

#GRAFICO FILTRADO POR CATEGORIA
data_yapoventa <- filter( DatosEnArchivosCsv2, comuna == "puente alto")
ggplot(data = data_yapoventa, aes(x=PrecioCalculado, y=categoria)) + geom_bar(stat="identity") + ggtitle("DATOS COMUNA PUENTE ALTO")

##Histograma
ggplot(data = DatosEnArchivosCsv2, aes(x=comuna , y = tipoTransaccion)) + geom_histogram() + ggtitle("Comunas") + theme_light()
ggplot(data = DatosEnArchivosCsv2, aes(x = comuna, y = tipoTransaccion)) +
  geom_jitter(aes(color = "darkslategray"), size = 1, alpha = 0.7) +
  geom_boxplot(aes(color = "darkslategray"), alpha = 0.7) +
  xlab('COMUNAS') +
  ylab('TIPOS DE TRANSACCIONES') +
  ggtitle('RELACION COMUNAS Y TIPO DE TRANSACCIONES') +
  theme_minimal()