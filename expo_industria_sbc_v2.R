rm(list = ls())

library(data.table)
#library(stringr)
library(tidyverse)
library(fst)
library(openxlsx) 
library(googlesheets4)
library(Mectritas)
library(DatosAbiertosCEP)

#googlesheets4::gs4_auth('guidosanchez22290@gmail.com')
#ss <- gs4_create(name = "Expo_industria_sbc_v2", sheets = '2. expo_grupo')

# Seteamos ruta general
ruta_pc <- "C:/Users/Usuario/"
ruta <- paste0(ruta_pc,"Documents/Cep Pedidos/Pedidos Puntuales/expo_industria_sbc/")
setwd(ruta)

# Seteamos ruta de bases
ruta_bases <- "C:/Users/Usuario/Documents/Bases CEP XXI/"

# Seteamos la ruta de las mectras
ruta_mectras <- paste0(ruta_bases,"MECTRA/Mectra FST")

# Seteamos la ruta de comex
ruta_comex <- paste0(ruta_bases,"COMEX/")

# Levantamos la base cuit_clae
cuit_clae <- fread(paste0(ruta_bases, "cuit_clae6_final_v04_2022.csv"))
cuit_clae <- cuit_clae[,.(cuit, clae6_final)]
setnames(cuit_clae, "clae6_final", "clae6")
cuit_clae <- cuit_clae[,cuit:=as.double(cuit)]

## Pedido especial, los cuits que pertenecen a los servicios basados en conocimiento

#empresas_sbc<- read.xlsx(paste0(ruta,"cuits_sbc_22.xlsx"))
#setDT(empresas_sbc)
#empresas_sbc <- empresas_sbc[,cuit:=as.double(cuit)]

# Llamo los data table que luego voy a llenar
#data_mas100 <- data.table()

# armo una lista con las bases de expo para guardar

bases <- list.files(ruta_comex,
                      pattern='*.csv',
                      full.names = T)

bases <- bases[str_detect(bases, 'e2017|e2018|e2019|e2020|e2021|e2022')==TRUE]


bases1 <- list.files(ruta_comex,
                    pattern='*.csv',
                    full.names = T)

bases1 <- bases1[str_detect(bases1, 'i2017|i2018|i2019|i2020|i2021|i2022')==TRUE]

años <- c(2017, 2018, 2019, 2020, 2021, 2022)


for(i in 1:length(bases)){

  ## Abro la base de expo de  2022

  expo <- fread(bases[i], integer64= 'numeric', 
                  select = c("anyo", "mes", "cuit", "posic_sim", "fob"))

  # Mergeo con base cuit clae

  expo <- merge(expo, cuit_clae, by='cuit')

  # primero me quedo solo con los claes de industria

  expo_industria <- expo[(clae6>101010 & clae6<332001)]
  expo_industria$Grupo <- "Industria"
  
  # Se saca un cuit que despues va a dar problemas, le adjudican un clae6 q luego no existe
  expo_industria <- expo_industria %>% filter(cuit != 20243573093)
  expo_industria <- expo_industria %>% limpia_cuits(elimina_publico = T, indica_publico = F)

  # colapso expo2022 para llegar a cuanto exporta cada cuit
  expo_industria_cuit <- expo_industria[,.(expo_cuit=sum(fob, na.rm=T)), by=c('cuit', 'clae6', 'anyo', 'Grupo')] 

  # Voy a armar otra base que tenga solo los de sbc, luego los voy a mergear

  #expo2022_sbc <- expo2022[cuit %in% empresas_sbc$cuit]
  #expo2022_sbc$Grupo <- "SBC"

  #expo2022_sbc_cuit <- expo2022_sbc[,.(expo_cuit=sum(fob, na.rm=T)), by=c('cuit', 'clae6', 'anyo', 'Grupo')] 
  #rm(empresas_sbc)


  # Me fijo por las dudas que los cuits de sbc y de industria no sean los mismos. 

  #cuits_sbc <- unique(expo2022_sbc_cuit$cuit)
  #cuits_industria <- unique(expo2022_industria_cuit$cuit)

  #setequal(cuits_sbc, cuits_industria)
  #rm(cuits_sbc, cuits_industria)

  # Perfecto, no se repiten. Unamos entonces las dos bases

  #data_pedido <- union_all(expo2022_industria_cuit, expo2022_sbc_cuit)
  # Se saca un cuit que despues va a dar problemas, le adjudican un clae6 q luego no existe
  #expo_industria_cuit <- expo_industria_cuit %>% filter(cuit != 20243573093)
  #expo_industria_cuit <- expo_industria_cuit %>% limpia_cuits(elimina_publico = T, indica_publico = F)


  # Vamos a hallar ahora la cantidad de productos exportados y el principal producto exportado.
  ## Ojo me tengo que quedar con los cuits de industria antes seleccionados

  tmp_principal <- expo_industria[,. (expo_prod_princ = sum(fob, na.rm=T)), by=c('cuit', 'posic_sim')]
  #tmp_sbc <- expo2022_sbc[,. (expo_prod= sum(fob, na.rm=T)), by=c('cuit', 'posic_sim')]
  #tmp_principal <- union_all(tmp_industria, tmp_sbc)

  tmp_principal <- tmp_principal[, productos_distintos_exp := .N, by=cuit]
  setorder(tmp_principal, cuit, -expo_prod_princ)

  # ahora ordenado pongo un indice y me quedo con los primeros
  tmp_principal <- tmp_principal[,index:=1:.N, by =cuit]
  tmp_principal <- tmp_principal[index ==1]

  # Termino de limpiar
  #tmp_principal <- tmp_principal %>% filter(cuit != 20243573093)
  #tmp_principal <- tmp_principal %>% limpia_cuits(elimina_publico = T, indica_publico = F)
  setnames(tmp_principal, 'posic_sim', 'posic_prod_princ')

  # Mergeo la base data_pedido con la que tiene los datos de los productos principales y las expo de los mismos

  expo_industria_cuit <- merge(expo_industria_cuit, tmp_principal, by='cuit')
  expo_industria_cuit <- expo_industria_cuit[, prop_expo_princ := expo_prod_princ/expo_cuit]

  # Borro para no tener tantas cosas

  rm(expo_industria, tmp_principal)
  expo_industria_cuit$index <- NULL
  
  # Ahora armo dummies por las categorias pedidas 

  # Dummy mayor a cien
  #expo_industria_cuit <- expo_industria_cuit[, mayor_cien := 0]
  expo_industria_cuit <- expo_industria_cuit[, paste0("mayor_cien", años[i]) := fifelse(expo_cuit > 99999, 1 , 0)]

  # Dummy entre 100 y 500
  expo_industria_cuit <- expo_industria_cuit[, entre_100_500 := 0]
  expo_industria_cuit <- expo_industria_cuit[, entre_100_500 := fifelse((expo_cuit > 99999 & expo_cuit < 500000), 1 , entre_100_500)]

  # Dummy entre 500 y 1 millon
  expo_industria_cuit <- expo_industria_cuit[, entre_500_millon := 0]
  expo_industria_cuit <- expo_industria_cuit[, entre_500_millon := fifelse((expo_cuit > 499999 & expo_cuit < 1000000), 1 , entre_500_millon)]

  # Dummy mas de 1 millon
  expo_industria_cuit <- expo_industria_cuit[, mayor_millon := 0]
  expo_industria_cuit <- expo_industria_cuit[, mayor_millon := fifelse((expo_cuit > 999999 ), 1 , mayor_millon)]
  

  # Creo una columna con la mediana por clae6
  expo_industria_cuit <- expo_industria_cuit[,mediana_expo_clae6 := median(expo_cuit, na.rm=T), by= c('clae6')]
  

  # Creo una columna con el p80 clae6
  expo_industria_cuit <- expo_industria_cuit[,percentil80_expo_clae6 := quantile(expo_cuit, probs=0.8), by= c('clae6')]
  
  
  # dummies si superan la mediana y el p80
  expo_industria_cuit <- expo_industria_cuit[, supera_mediana_clae6 := fifelse((expo_cuit >= mediana_expo_clae6), 1 , 0)]
  expo_industria_cuit <- expo_industria_cuit[, supera_p80_clae6 := fifelse((expo_cuit >= percentil80_expo_clae6), 1 , 0)]
  
  # redondeo de porcentajes
  expo_industria_cuit <- expo_industria_cuit[,expo_cuit:=round(expo_cuit, digits = 0)]
  expo_industria_cuit <- expo_industria_cuit[,expo_prod_princ:=round(expo_prod_princ, digits = 0)]
  expo_industria_cuit <- expo_industria_cuit[,mediana_expo_clae6:=round(mediana_expo_clae6, digits = 0)]
  expo_industria_cuit <- expo_industria_cuit[,percentil80_expo_clae6:=round(percentil80_expo_clae6, digits = 0)]
  expo_industria_cuit <- expo_industria_cuit[,prop_expo_princ:=round(prop_expo_princ, digits = 2)]
  
  

  
  
  
  #expo_industria_cuit <- fread(bases2[5], integer64= 'numeric')
  # Abro la base de importacion del año para sumar las impo por empresa y la balanza comercial por cuit
  
  impo <- fread(bases1[i], integer64= 'numeric', 
                select = c("cuit", "cif"))
  
  # Me tengo q quedar solo con los mismos cuits que ese año exportaron
  #cuits <- unique(impo$cuit)
  
  impo <- impo[cuit %in% expo_industria_cuit$cuit]
  # colapso expo2022 para llegar a cuanto exporta cada cuit
  impo_cuit <- impo[,.(impo_cuit=sum(cif, na.rm=T)), by=c('cuit')] 
  
  #mergeo con la base que estamos trabajando y redondeo lo agregado
  expo_industria_cuit <- merge(expo_industria_cuit, impo_cuit, by='cuit', all.x=T)
  expo_industria_cuit <- expo_industria_cuit[,impo_cuit:=round(impo_cuit, digits = 0)]
  expo_industria_cuit[is.na(expo_industria_cuit)] <- 0
  
  # Balanza comercial de los cuits que importaron
  expo_industria_cuit <- expo_industria_cuit[, balanza_comercial := (expo_cuit - impo_cuit)]
  
  # Dummy de si fue deficitaria
  expo_industria_cuit <- expo_industria_cuit[, paste0("deficitaria_", años[i]) := fifelse(balanza_comercial < 0, 1, 0)]
  
  
  # Agregamos las descripciones que faltan y ordenamos un poco
  expo_industria_cuit <- expo_industria_cuit %>% add_claes(agregacion_deseada = c('clae6', 'clae2'))
  setorder(expo_industria_cuit, clae6, cuit)
  
  # orden de columnas
  setcolorder(expo_industria_cuit, c('cuit', 'clae6', 'clae6_desc', 'clae2', 'clae2_desc'))
  
  # Guardamos un archivo csv con lo trabajado hasta aqui y limpiamos
  write.csv(expo_industria_cuit, paste0(ruta, "expo_", años[i], ".csv"), row.names = F)
  print(i)
  rm(expo_industria_cuit, expo, impo, impo_cuit)
}


### Listo las base para armar la columna con los años que se exporto mas de 100 mil usd y tambien cuantos años fue deficitaria

bases2 <- list.files(ruta,
                    pattern='*.csv',
                    full.names = T)


expo2017 <- fread(bases2[1], integer64= 'numeric')
expo2017 <- expo2017[,. (cuit, mayor_cien2017, deficitaria_2017)]

expo2018 <- fread(bases2[2], integer64= 'numeric')
expo2018 <- expo2018[,. (cuit, mayor_cien2018, deficitaria_2018)]

expo <- merge(expo2017, expo2018, by='cuit', all.x=T, all.y=T)

expo2019 <- fread(bases2[3], integer64= 'numeric')
expo2019 <- expo2019[,. (cuit, mayor_cien2019, deficitaria_2019)]

expo <- merge(expo, expo2019, by='cuit', all.x=T, all.y=T)

# Le sumo 2020 solo para no tener que hacerlo deaspues, pero no va a contar en la suma
expo2020 <- fread(bases2[4], integer64= 'numeric')
expo2020 <- expo2020[,. (cuit, mayor_cien2020, deficitaria_2020)]

expo <- merge(expo, expo2020, by='cuit', all.x=T, all.y=T)

expo2021 <- fread(bases2[5], integer64= 'numeric')
expo2021 <- expo2021[,. (cuit, mayor_cien2021, deficitaria_2021)]

expo <- merge(expo, expo2021, by='cuit', all.x=T, all.y=T)

expo2022 <- fread(bases2[6], integer64= 'numeric')
expo2022 <- expo2022[,. (cuit, mayor_cien2022, deficitaria_2022)]

expo <- merge(expo, expo2022, by='cuit', all.x=T, all.y=T)


# Calculo ahora la columna que cuente cuantos años exporto mas de 100 mil usd 

expo[is.na(expo)] <- 0

expo <- expo[, anios_mas100 := (mayor_cien2017 + mayor_cien2018 + mayor_cien2019 + mayor_cien2021 + mayor_cien2022 )]
expo <- expo[, anios_deficitaria := (deficitaria_2017 + deficitaria_2018 + deficitaria_2019 + deficitaria_2021 + deficitaria_2022 )]

# Me quedo solo con la columna de interes

expo <- expo[,. (cuit, anios_mas100, anios_deficitaria)]

rm(expo2017, expo2018, expo2019, expo2020, expo2021, expo2022)


# loop para mergear a cada mini base la columna trabajada

for(i in 1:length(bases2)){
  tmp <- fread(bases2[i], integer64= 'numeric')
  #tmp$V1 <- NULL
  tmp <- merge(tmp, expo, by='cuit')
  setorder(tmp, clae6, cuit)
  write.csv(tmp, paste0(ruta, "expo_", años[i], ".csv"), row.names = F)
  print(i)
  rm(tmp)
}


# Ahora me queda encontrar el año de primera aparicion de empresas
# En expo, buscar los cuits unicos y quedarme con eso de la base cuit de nico, luego puedo poner un indice y
# quedarme con la primera aparicion de cada cuit


# Abro la base de cuit tamaño de nico
cuit_tamano <- fread(paste0(ruta_bases, "CUIT Tamaño.csv"), integer64= 'numeric')

# Me quedo con los cuits de interes
cuit_tamano <- cuit_tamano[cuit %in% expo$cuit]

cuit_tamano <- cuit_tamano[, index:= 1:.N, by=cuit ]

# Ahora me quedo con la primera aparicion de cada uno y elijo las q aparicieron desde el 2015

cuit_todos <- cuit_tamano %>% filter(index ==1)
cuit_tamano <- cuit_todos %>% filter(year > 2014)

cuit_tamano <- cuit_tamano[,. (cuit, year)]

# La base cuit tamaño llega hasta 2021, al mergear encontre 8222 del listado, tal vez las q faltan son nuevas
# Voy a agarrar una mectra para fijarme si aparecen cuits en 2022

# Abro una mectra de septiembre
mectra <-  read.fst(paste0(ruta_mectras, "/2022/m202209.fst"), columns = c("cuit", "cuil"), as.data.table = TRUE)

# me quedo primero en la mectra con los cuits que aparecen en expo
mectra <- mectra[cuit %in% expo$cuit]

# me quedo con una sola observacion por cuit
mectra <- mectra[,.(puestos=.N), by=cuit]

# tengo 7613 cuits, ahora queda ver cuales de esos no aparecen en el listado de cuit todos, lo q me indica q aparecieron en 2022

mectra <- mectra[!cuit %in% cuit_todos$cuit]

# Hay 13 empresas que estan solo en ese mes y nunca antes, las voy a considerar las nuevas

mectra <- mectra[, year := 2022]
mectra$puestos <- NULL

cuit_tamano <- rbind(cuit_tamano, mectra)

# Ahora si tengo todas las fechas de  las empresas que surgieron a partir de 2015
# Armamos las dummies y luego mergeamos con cada año para agregarlas

cuit_tamano <- cuit_tamano[, nueva_2021_2022 := fifelse(year>2020, 1, 0)]
cuit_tamano <- cuit_tamano[, antiguedad_menor_7 := 1]


# Ahora tengo q mergear en cada base para agregarla

for(i in 1:length(bases2)){
  tmp <- fread(bases2[i], integer64= 'numeric')
  #tmp$V1 <- NULL
  tmp <- merge(tmp, cuit_tamano, by='cuit', all.x=T)
  tmp[is.na(tmp)] <- 0
  tmp <- tmp[,año_aparicion:=fifelse(year==0, NA_real_, year )]
  tmp$year <- NULL
  setorder(tmp, clae6, cuit)
  write.csv(tmp, paste0(ruta, "expo_", años[i], ".csv"), row.names = F)
  print(i)
  rm(tmp)
}



### Por ultimo mergeamos con la base de registro pyme

registro <- fread(paste0(ruta_bases, "/registro_pyme/base_06.02.23.csv"), integer64 = 'numeric')

registro$V1 <- NULL

registro <- registro %>% filter(anyo != 0)
setnames(registro, 'CUIT', 'cuit')

registro <- registro[,. (cuit, Facturacion, anyo)]


# me quedo con los cuits de interes

registro <- registro[cuit %in% expo$cuit]

registro2021 <- registro %>% filter(anyo ==2021)
registro2022 <- registro %>% filter(anyo ==2022)



########## Mergeo con la facturacion para la base 2021
# Mergeo con las bases de cada año para agregarle la facturacion 
tmp <- fread(bases2[5], integer64= 'numeric')

tmp <- merge(tmp, registro2021, by=c ('cuit', 'anyo'), all.x=T)

# La mayoría de los resumenes son a diciembre del 2021, pero como se suma en pesos corrientes meses del todo 
# año voy a tomar el TC promedio del año que es 96

tmp <- tmp[, Fac_usd:= Facturacion / 96]

# Calculo cociente de exportaciones/ facturacion
tmp <- tmp[,cociente_expo_fac := (expo_cuit/Fac_usd)]

# Redondeos
tmp <- tmp[,Facturacion:=round(Facturacion, digits = 0)]
tmp <- tmp[,Fac_usd:=round(Fac_usd, digits = 0)]
tmp <- tmp[,cociente_expo_fac:=round(cociente_expo_fac, digits = 2)]

# Dummy si aparece en el registro
tmp <- tmp[,aparece_reg_pyme := fifelse(cuit %in% registro$cuit, 1, 0)]
setorder(tmp, clae6, cuit)
write.csv(tmp, paste0(ruta, "expo_", años[5], ".csv"), row.names = F)



########## Mergeo con la facturacion para la base 2022
# Mergeo con las bases de cada año para agregarle la facturacion 
tmp <- fread(bases2[6], integer64= 'numeric')

tmp <- merge(tmp, registro2022, by=c ('cuit', 'anyo'), all.x=T)

# En este caso, la amplia mayoria de balances no cerraron a fin de año. Por lo cual, voy a actuar bajo el supuesto que cerraron
# en julio. Entonces se estarian sumando pesos corrientes desde julio 2021 a junio 2022. Voy a tomar el TC promedio para ese
# periodo que es 115

tmp <- tmp[, Fac_usd:= Facturacion / 115]

# Calculo cociente de exportaciones/ facturacion
tmp <- tmp[,cociente_expo_fac := (expo_cuit/Fac_usd)]

# Redondeos
tmp <- tmp[,Facturacion:=round(Facturacion, digits = 0)]
tmp <- tmp[,Fac_usd:=round(Fac_usd, digits = 0)]
tmp <- tmp[,cociente_expo_fac:=round(cociente_expo_fac, digits = 2)]

# Dummy si aparece en el registro
tmp <- tmp[,aparece_reg_pyme := fifelse(cuit %in% registro$cuit, 1, 0)]
setorder(tmp, clae6, cuit)
write.csv(tmp, paste0(ruta, "expo_", años[6], ".csv"), row.names = F)


# Que mas falta? Creo q nada. Exportamos a un excel



expo2017 <- fread(bases2[1], integer64= 'numeric')
expo2018 <- fread(bases2[2], integer64= 'numeric')
expo2019 <- fread(bases2[3], integer64= 'numeric')
expo2020 <- fread(bases2[4], integer64= 'numeric')
expo2021 <- fread(bases2[5], integer64= 'numeric')
expo2022 <- fread(bases2[6], integer64= 'numeric')

# Creamos el objeto del workbook acá en R que luego será el excel

wb <- createWorkbook()
addWorksheet(wb, "expo2017")
addWorksheet(wb, "expo2018")
addWorksheet(wb, "expo2019")
addWorksheet(wb, "expo2020")
addWorksheet(wb, "expo2021")
addWorksheet(wb, "expo2022")


# Le escribimos lo solicitado en cada una de las dos pestañas

writeDataTable(wb,
               sheet = "expo2017",
               expo2017)

writeDataTable(wb,
               sheet = "expo2018",
               expo2018)

writeDataTable(wb,
               sheet = "expo2019",
               expo2019)

writeDataTable(wb,
               sheet = "expo2020",
               expo2020)

writeDataTable(wb,
               sheet = "expo2021",
               expo2021)

writeDataTable(wb,
               sheet = "expo2022",
               expo2022)

# Exportamos

saveWorkbook(wb,
             paste0(ruta,"results/base_expo_industria.xlsx"),
             overwrite = TRUE)

