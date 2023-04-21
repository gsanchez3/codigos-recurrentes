rm(list = ls())

library(data.table)
library(stringr)
library(tidyverse)
library(fst)
library(openxlsx) 
library(googlesheets4)
library(Mectritas)
googlesheets4::gs4_auth('guidosanchez22290@gmail.com')
ss <- gs4_create(name = "Mision 1", sheets = 'resultados por grupo')


# Seteamos ruta general
ruta_pc <- "C:/Users/Usuario/"
ruta <- paste0(ruta_pc,"Documents/Cep Pedidos/Pedidos Puntuales/puestos_agro/")
setwd(ruta)

# Seteamos ruta de bases
ruta_bases <- "C:/Users/Usuario/Documents/Bases CEP XXI/"

# Seteamos la ruta de las mectras
ruta_mectras <- "C:/Users/Usuario/Documents/Bases CEP XXI/MECTRA/Mectra FST"


# Armamos el archivo de las mectras

mectras <- list.files(ruta_mectras,
                    pattern='*.fst',
                    full.names = T,
                    recursive = T)

mectras <- mectras[str_detect(mectras, 'm2007|m2008|m2009|m2010|m2011|m2022')==FALSE]


# Levantamos la base cuit_clae
#cuit_clae <- fread(paste0(ruta_bases, "cuit_clae6_final_v04_2022.csv"))
#cuit_clae <- cuit_clae[,.(cuit, clae6_final)]
#setnames(cuit_clae, "clae6_final", "clae6")

# base zona_loc. No va mas al estar en mectritas ya guardada
#zona_loc <- fread(paste0(ruta_bases, "zona_loc.csv"), encoding = "UTF-8",
#                  integer64 = "numeric")
#zona_loc$zona_desc <- NULL

# umbral salario
umbral_salario <- fread(paste0(ruta_bases, "Umbral_remuneracion_minima_MECTRA.csv"),
                        encoding = "UTF-8", dec = ",")
umbral_salario <- umbral_salario[,umbral:=as.double(umbral)]

## Descripcion de los claes

#clae_agg <- fread(r'(C:\Users\Usuario\Documents\Bases CEP XXI\clae_agg.csv)',
#                  encoding = "UTF-8")
#clae_agg <- clae_agg[,.(clae6, clae6_desc, letra)]
#claes_simp <- fread(r'(C:\Users\Usuario\Documents\Bases CEP XXI\CLAE letra simplificado.csv)',
#                    encoding = "UTF-8")
#claes_simp <- claes_simp[,.(letra, letra_desc_simp_2)]
#clae_agg <- merge(clae_agg, claes_simp, by="letra", all.x = TRUE)

### Base de Genero

padron_cuil_genero <- fread(paste0(ruta_bases, "padron_cuil_genero.csv"),
                            integer64 = "numeric")

### Llamo tres data tables que luego voy a llenar..

data_grupo <- data.table()
data_provincia <- data.table()
data_empresa <- data.table()

#### Pedido
#### Abro la base con los cuits q necesito
empresas_mision1<- read.xlsx("C:/Users/guido/OneDrive/Documentos/Cep Pedidos/Pedidos Puntuales/pedido_nico_barcos/Listado de CUITs II.xlsx")
setDT(empresas_mision1)
empresas_mision1 <- empresas_mision1[,CUIT:=as.double(CUIT)]



for(i in 1:length(mectras)){
  tmp <- read.fst(mectras[i], columns = c("cuit", "cuil","zona", "remuneracion", "mes"), as.data.table = TRUE
  )
  tmp <- tmp %>% mectra_numerica()
  #tmp <- tmp[,cuit:=as.double(cuit)]
  #tmp <- tmp[,cuil:=as.double(cuil)]
  # Filtramos las cajas y falopa
  tmp <- tmp %>% limpia_cuits()
  #selecciono empresas de interes
  tmp <- tmp[cuit %in% empresas_mision1$CUIT]
  #agrego actividad principal
  #tmp <- merge(tmp, cuit_clae, by="cuit", all.x = TRUE)
  #agrego info de las empresas
  tmp <- merge(tmp, empresas_mision1, by.x = "cuit", by.y="CUIT", all.x = TRUE)
  #zona
  tmp <- merge(tmp, zona_loc, by="zona", all.x = TRUE)
  #umbral salario, dejo como NA las q estan debajo del umbral
  tmp <- tmp[, anio := as.numeric(substr(mes, 1, 4))]
  tmp <- tmp[, mes := as.numeric(substr(mes, 5, 6))]
  tmp <- merge(tmp, umbral_salario, by = "anio", all.x = TRUE)
  
  tmp <- tmp %>%  mutate(remuneracion = if_else(remuneracion < umbral,
                                                  NA_real_,  remuneracion))
  
  tmp$Descripcion <- "Mision 1"
  #Asignaci?n de descripcion de los claes
  tmp <- tmp %>% add_claes(agregacion_deseada = 'clae6')
  #Asignaci?n de genero
  tmp <- merge(tmp, padron_cuil_genero, by="cuil", all.x = TRUE)
  #cuando no est?n en padron_cuil_genero
  tmp <- tmp %>% mectra_demografia()
  tmp <- tmp[, genero_final2 := fcase(is.na(genero_final)&mujer==0, 1,
                                      is.na(genero_final)&mujer==1, 2,
                                      default = NA_real_)]
  #table(tmp$genero_final, useNA = "always")
  tmp <- tmp[, genero_final := fifelse(is.na(genero_final), genero_final2, genero_final)]
  tmp <- tmp[, genero_final := fifelse(genero_final==2, 1, 0)]
  tmp$genero_final2 <- NULL
  tmp$mujer <- NULL
  #tmp <- tmp[,cuil_dos_cif := str_sub(cuil,1,2)]
  #tmp <- tmp[,cuil_ulti := str_sub(cuil,11,11)]
  #genero la variable sexo_calc = 1, asumo que son varones a menos se cumplan las condiciones
  #tmp <- tmp[,sexo_calc := 1]
  #tmp <- tmp[,sexo_calc := fifelse(cuil_dos_cif==27,2,sexo_calc)]
  #tmp <- tmp[,sexo_calc := fifelse(cuil_dos_cif==23 & cuil_ulti==4,2,sexo_calc)]
  #tmp <- tmp[,genero_final := fifelse(is.na(genero_final),sexo_calc, genero_final)]
  
  #tmp <- tmp[, genero_final := fifelse(genero_final==2, 1, 0)]
  
  setnames(tmp, 'zona_prov', 'provincia')
  #setnames(tmp, 'clae6_desc', 'sector')
  setnames(tmp, 'anio', 'a?o')
  
  ## Resultados agrupados por grupo y provincia
  
  
  tmp_grupo <- tmp[,.(Empleados = .N,
                          share_mujer = mean(genero_final,na.rm=T),
                          Remu_media = mean(remuneracion,na.rm=T),
                          Remu_mediana = median(remuneracion,na.rm=T)
  ),
  by=c('Descripcion', 'a?o', 'mes')]
  
  
  data_grupo <- rbind(data_grupo, tmp_grupo)
  
  tmp_provincia <- tmp[,.(Empleados = .N,
                     share_mujer = mean(genero_final,na.rm=T),
                     Remu_media = mean(remuneracion,na.rm=T),
                     Remu_mediana = median(remuneracion,na.rm=T)
  ),
  by=c('cuit', 'Empresa', 'provincia', 'año', 'mes')]
  setorder(tmp_provincia, provincia, cuit)
  
  ### Resultados agrupados por empresa
  
  data_provincia <- rbind(data_provincia, tmp_provincia)
  # tmp_salario <- tmp[remuneracion>lower,.(mean_w = mean(remuneracion)), by=c("actividad_principal", "mes")]
  # tmp_empresas <- tmp[,.(empresas = uniqueN(cuit)), by=c("actividad_principal", "mes")]
  #por provincia
  tmp_cuit <- tmp[,.(Empleados = .N,
                      share_mujer = mean(genero_final,na.rm=T),
                      Remu_media = mean(remuneracion,na.rm=T),
                      Remu_mediana = median(remuneracion,na.rm=T)
  ),
  by=c('cuit', 'Empresa', 'a?o', 'mes')]
  setorder(tmp_cuit, cuit)
  
  data_empresa <- rbind(data_empresa, tmp_cuit)
  print(i)
  rm(tmp, tmp_provincia, tmp_grupo, tmp_cuit)
  gc()
}

##### Para exportar en google sheets


sheet_write(data_grupo, ss="https://docs.google.com/spreadsheets/d/1ttipCLc6HY4F7bXRj6LfGogUmUkPjedERRZbiSjekeU/edit#gid=2047019984", sheet="resultados por grupo")

sheet_write(data_empresa, ss="https://docs.google.com/spreadsheets/d/1ttipCLc6HY4F7bXRj6LfGogUmUkPjedERRZbiSjekeU/edit#gid=2047019984", sheet="resultados por empresa")

sheet_write(data_provincia, ss="https://docs.google.com/spreadsheets/d/1ttipCLc6HY4F7bXRj6LfGogUmUkPjedERRZbiSjekeU/edit#gid=2047019984", sheet="resultados por provincia")


