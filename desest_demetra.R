#install.packages('RJDemetra')


if (i == 2 | i == 3) {
  
  ts_prov <- ts(tmp_prov[, c(3)],start = c(2007), frequency = 12)
  
  #Descomponemos con RJDemetra
  
  ts_prov <- RJDemetra::x13(ts_prov)
  
  # Llevamos el resultado a dataframe
  
  ts_prov <- data.frame(.preformat.ts(ts_prov$final$series), stringsAsFactors = FALSE)
  
  # Pasar el nombre de las rows (fecha) a variable
  
  ts_prov <- tibble::rownames_to_column(ts_prov, var = 'fecha')
  
  # Damos formato a la fecha
  
  ts_prov$fecha <- lubridate::my(ts_prov$fecha)
  
  # Nos quedamos sólo con la serie desestacionalizada
  
  setDT(ts_prov)
  ts_prov <- ts_prov[, list(fecha, sa)]
  
  # Mergeamos con la serie anterior
  
  tmp_prov <- merge(tmp_prov[, list(fecha, zona_prov)], ts_prov, by='fecha')
  
  # Renombramos la desestacionalizada como una común
  
  setnames(tmp_prov, "sa", variables[i])