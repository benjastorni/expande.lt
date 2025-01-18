expande.lt = function(puente, muestra, estratos, cc, sp, mas = F, filt.est = NULL, graph = F){

options(scipen = 999) # Quitar notacion cientifica


###########################################################
################# CARGA DE DATOS #################
###########################################################

#### PUENTE Y CAPTURAS ####

# Primer stop: Estructura de puente
if(colnames(puente)[54] != "ESPECIE_1"){
    stop("La base PUENTE no tiene el formato correcto.\n La columna 54 debe ser 'ES
    PECIE_1'!")
}
if(length(which(is.na(puente$AREA_BARR))) != 0){
    stop(paste0("La base PUENTE no tiene el formato correcto.\n Tiene ", length(which(is.na(puente$AREA_BARR))), " lance(s) sin dato de area barrida"))
}

# Como las capturas estan en formato wide, las paso a longer.
# Para eso separo las capturas del resto de la info del puente y hago el pivot:
capturas <- puente %>%
    select(LANCE, ESPECIE_1:ncol(puente)) %>%
    pivot_longer(cols = !LANCE, # Uso lance como referencia sin pivotear
    cols_vary = "slowest", # De esta forma especie y kilogramo quedan vinculados
    names_to = ".value", # Nombre de las columnas de salida. Ignorar
    names_pattern = "(.)", # Separación de nombres. Ignorar
    values_drop_na = T) %>% # Elimina todos los NA estructurales durante el pivot
    rename(ESPECIE = E, KG = K) %>% # Renombra las columnas
    filter(ESPECIE == sp) # Filtra las capturas por especie de interés

# Uno los datos de captura ya pivotados y las primeras 53 columnas del puente:
puente_capt <- puente[, 1:53] %>%
    filter(COD_CAMP == as.character(cc)) %>% # Filtro por codigo de campania
    left_join(capturas, by = "LANCE") %>% # Uno las capturas por lance
    mutate(KG = ifelse(is.na(KG), 0, KG), # Las capturas que tuvieran NA las paso a cero para considerarlas
    DENSIDAD = KG/1000/AREA_BARR) %>% # Calculamos densidad en toneladas/mn2
    arrange(LANCE) # Reordeno los datos por lance

# Segundo stop: Estructura de capturas pivoteadas
if(dim(puente_capt)[1] > dim(puente[, 1:53])[1]){
    stop("La base PUENTE no tiene el formato adecuado!\n Revise las capturas en la base PUENTE")
}

#### MUESTRAS ####

# Tercer stop: Estructura base muestra
if(colnames(muestra)[11] != "TALLA_1"){
    stop("El formato de la base MUESTRA no es correcto.\n La columna 11 debe ser 'TALLA_1'!")
}

# Filtro las muestras por especie:
muestra_filtro <- muestra %>%
    filter(ESPECIE == sp) %>%
    arrange(LANCE)

# Cuarto stop: Estructura de muestras filtradas
if(dim(muestra_filtro)[1] > dim(puente_capt)[1]){
    stop("La base MUESTRA no tiene el formato adecuado!\n Hay mas muestras que lances!")
}

# Uno las bases de puente_capt que tenia con las primeras columnas de muestra y los estratos.
pue_capt_mues_est <- puente_capt %>%
    left_join(muestra_filtro[, 1:11], by = c("LANCE", "ESPECIE")) %>%
    left_join(estratos, by = "ESTRATO") %>%
    mutate(PESO_MUES = ifelse(is.na(PESO_MUES) | TALLA_1 %in% c(0, NA), 0, PESO_MUES)) # Filtro aquellas muestras sin informacion real de tallas o con talla ausente

# Agregamos un filtro si se quiere trabajar con estratos específicos
if(is.null(filt.est) == F){
    pue_capt_mues_est <- pue_capt_mues_est %>%
    filter(ESTRATO %in% filt.est)
}


###########################################################
################# CALCULO BIOMASAS ################
###########################################################

if(mas == T){
    pm_sum <- pue_capt_mues_est %>%
    summarise(COD_CAMP = as.character(cc), # Asigno el codigo de campania
    suma_muestra_kg = sum(PESO_MUES), # Sumo los pesos de muestra en kilos
    suma_captura_kg = sum(KG), # Sumo las capturas en kilos
    capc_mues_kg = sum(KG[PESO_MUES > 0]), # Sumo las capturas con muestra en kilos
    nlance = length(LANCE), # Veo cantidad de lances en cada estrato
    lances_cap = length(LANCE[KG > 0]), # Veo la cantidad de lances con captura
    mdens_ton.mn2 = round(mean(DENSIDAD), digits = 5), # Calculo la densidad media en toneladas por milla nautica cuadrada
    var_dens = var(DENSIDAD), # Varianza de la densidad
    var_mdens = var_dens / nlance, # Varianza de la densidad media
    area_mn2 = sum(unique(AREA))) # Area total en millas nauticas cuadradas

biom_total <- data.frame(biom_total_ton = pm_sum$mdens_ton.mn2 * pm_sum$area_mn2, # Biomasa total

var_biom_tot = pm_sum$var_mdens * (pm_sum$area_mn2 ^ 2), # Varianza de la biomasa

desvio_biom_tot = sqrt(pm_sum$var_mdens * (pm_sum$area_mn2 ^ 2))) # Desvio de la biomasa
} else{
# Armo un data.frame con los datos resumidos para cada lance:
pme_sum <- pue_capt_mues_est %>%
    group_by(ESTRATO) %>% # Agrupo por estrato para calcular las medias
    summarise(COD_CAMP = as.character(cc), # Asigno el codigo de campania
    suma_muestra_kg = sum(PESO_MUES), # Sumo los pesos de muestra en kilos
    suma_captura_kg = sum(KG), # Sumo las capturas en kilos
    capc_mues_kg = sum(KG[PESO_MUES > 0]), # Sumo las capturas con muestra en kilos
    nlance = length(LANCE), # Veo cantidad de lances en cada estrato
    lances_cap = length(LANCE[KG > 0]), # Veo la cantidad de lances con captura
    n_cuadriculas = unique(CUADRICULS), # Numero de cuadriculas para cada estrato
    mdens_ton.mn2 = round(mean(DENSIDAD), digits = 5), # Calculo la densidad media en toneladas por milla nautica cuadrada
    var_mdens = round((var(DENSIDAD) / nlance) * (1-(nlance / n_cuadriculas)), digits = 5), # Varianza de la densidad media
    area_mn2 = unique(AREA)) %>% # Area de cada estrato en millas nauticas cuadradas
    mutate(biom_est_ton = (n_cuadriculas / sum(n_cuadriculas)) * mdens_ton.mn2 * sum(area_mn2), # Calculo la biomasa de cada estrato en toneladas
    var_biom_est = ((n_cuadriculas / sum(n_cuadriculas)) ^ 2) * (sum(area_mn2) ^ 2) * var_mdens) # Varianza de la biomasa en cada estrato

# Calculo la biomasa total y su varianza
biom_total <- data.frame(biom_total_ton = sum(pme_sum$biom_est_ton), # Biomasa total

var_biom_tot = sum(pme_sum$var_biom_est, na.rm = T), #Varianza de la biomasa

desvio_biom_tot = sqrt(sum(pme_sum$var_biom_est, na.rm = T))) # Desvio de la biomasa

# Calculo la biomasa en los estratos con muestra
biom_estc_mues_ton = sum(pme_sum$biom_est_ton[pme_sum$suma_muestra_kg > 0])
}


##################################################################
################### FACTORES DE EXPANSION ################
##################################################################

if(mas == T){
    ponds = pue_capt_mues_est %>%
    right_join(pm_sum, by = c("COD_CAMP")) %>%
    mutate(biom_total_ton = biom_total$biom_total_ton,
    factor_lance = KG / PESO_MUES, # Calculo factores de expansion de los lances
    factor_biomasa = biom_total_ton / (capc_mues_kg / 1000),
    coef_exp = factor_lance * factor_biomasa) %>% # Calculo el coeficiente de expansion
    select("COD_CAMP", "LANCE", "KG", "PESO_MUES", "capc_mues_kg", "biom_total_ton", "factor_lance", "factor_biomasa", "coef_exp")
} else{
ponds = pue_capt_mues_est %>%
    right_join(pme_sum, by = c("COD_CAMP","ESTRATO")) %>%
    mutate(biom_total_ton = biom_total$biom_total_ton,
    biom_estc_mues_ton = biom_estc_mues_ton,
    factor_lance = KG / PESO_MUES, # Calculo factores de expansion de los lances
    factor_estrato = biom_est_ton / (capc_mues_kg / 1000), # Calculo factores de expansion de los estratos
    factor_total = biom_total_ton / biom_estc_mues_ton, # Calculo factores de expansion totales
    coef_exp = factor_lance * factor_estrato * factor_total) %>% # Calculo el coeficiente de expansion
    select("COD_CAMP", "LANCE", "KG", "PESO_MUES", "biom_est_ton", "capc_mues_kg", "biom_total_ton", "biom_estc_mues_ton", "factor_lance", "factor_estrato", "factor_total", "coef_exp")
}

# Warning para avisar que los pesos de muestra son mayores a los pesos de capturaen un 5%
if(length(which(ponds$factor_lance < 1)) > 0){
    warning("Hay lances con peso de capturas menores al peso de muestra!")
}


################################################################
################# TALLAS E INDIVIDUOS #################
################################################################

# Separo las tallas y las paso a formato longer. Misma sintaxis que lo hecho en capturas. Luego, extraigo la informacion en los codigos de talla

tallas <- muestra_filtro %>%
    select(LANCE, TALLA_1:ncol(muestra_filtro)) %>%
    pivot_longer(!LANCE,
    cols_vary = "slowest",
    names_to = ".value",
    names_pattern = "(.)",
    values_drop_na = T) %>%
    rename(TALLA_CODIGO = T) %>%
    mutate(lt_cm = as.numeric(str_sub(TALLA_CODIGO, -15, -13)), # Extraigo el largo total en cm
    n_machos = as.numeric(str_sub(TALLA_CODIGO, -12, -10)), # Extraigo el numero de machos
    n_hembras = as.numeric(str_sub(TALLA_CODIGO, -9, -7)), # Extraigo el numero de hembras
    n_indet = as.numeric(str_sub(TALLA_CODIGO, -6, -4)), # Extraigo el numerode indeterminados
    n_total = as.numeric(str_sub(TALLA_CODIGO, -3, -1))) %>% # Extraigo el total de individuos
    filter(n_total > 0) # Filtro los datos que no tuvieran informacion real sobre esa talla


####################################################################
################### FRECUENCIA DE TALLAS ####################
####################################################################

# Al numero de individuos por lance y clase de tallas lo multiplico por el coeficiente de expansion que corresponde
    individuos_pond <- tallas %>%
    right_join(ponds, by = "LANCE") %>%
    select(COD_CAMP, LANCE, lt_cm, n_machos, n_hembras, n_indet, # Selecciono las columnas de interes
    n_total, coef_exp) %>%
    filter(is.na(coef_exp) == F) %>% # Filtro los coeficientos para hace las cuentas
    mutate(pob_machos = floor(n_machos * coef_exp), pob_hembras = floor(n_hembras *
    coef_exp), # Pondero
    pob_indet = floor(n_indet * coef_exp), pob_total = floor(n_total * coef_exp))

# Warning que avisa si hay errores en las tallas
tallas_na <- which(is.na(individuos_pond$lt_cm))

if(length(tallas_na) > 0){
    warning(paste0("Lance(s) ", individuos_pond$LANCE[tallas_na], " contiene codigos de tallas con valores erróneos.\n Revise los codigos de talla"))

    individuos_pond <- individuos_pond %>%
        filter(is.na(lt_cm) == F)
}

# Quinto stop: Codigos de tallas erróneos
totales <- with(individuos_pond,(n_machos + n_hembras + n_indet) - n_total) != 0
if(sum(totales) > 0){
    stop(paste0("La suma de los individuos no es igual al total de individuos.\n Revise los códigos de tallas en lance(s) ", unique(individuos_pond[totales, "LANCE"]), " en la base MUESTRA\n Existen ", sum(totales), " códigos de talla que no tienen el formato adecuado!"))
}

# Agrupo por tallas entre todos los estratos y calculo los totales poblacionales y muestreados
rango_lt = data.frame(lt_cm = seq(min(individuos_pond$lt_cm), max(individuos_pond$lt_cm), by = 1))

frecuencia <- individuos_pond %>%
    group_by(lt_cm) %>%
    right_join(rango_lt, by = "lt_cm") %>%
    summarise(COD_CAMP = as.character(cc),
    machos_mues = sum(n_machos), hembras_mues = sum(n_hembras),
    indet_mues = sum(n_indet), total_mues = sum(n_total),
    tot_machos = sum(pob_machos), tot_hembras = sum(pob_hembras),
    tot_indet = sum(pob_indet), tot_total = sum(pob_total)) %>%
    relocate(COD_CAMP) %>%
    arrange(lt_cm) %>%
    replace_na(list(machos_mues = 0, hembras_mues = 0, indet_mues = 0, total_mues = 0, tot_machos = 0, tot_hembras = 0, tot_indet = 0, tot_total = 0))


#################################################################
################# GRAFICOS EXPLORATORIOS ################
#################################################################

if(graph == T){

# Gráfico de capturas vs muestras
    plot_capt_mues <- ggplot() +
        geom_point(data = pue_capt_mues_est, aes(x = PESO_MUES, y = KG)) +
        geom_abline(slope = 1, color = "red", linetype = 2, linewidth = .85) +
        geom_label_repel(data = pue_capt_mues_est %>% filter(KG / PESO_MUES < 1),
        aes(x = PESO_MUES, y = KG, label = LANCE), max.overlaps = 15) +
        xlab("Peso muestra (Kg)") +
        ylab("Peso captura (Kg)") +
        labs(title = "Capturas vs Muestras", subtitle = "Recta roja con pendiente 1") +
        theme_classic()

# Gráfico de capturas, area barrida y densidad
    plot_capturas <- ggplot() +
        geom_point(data = pue_capt_mues_est, aes(x = LANCE, y = KG)) +
        geom_abline(slope = 0, intercept = mean(pue_capt_mues_est$KG),
        color = "red", linetype = 2, linewidth = .85) +
        xlab("Número de lance") +
        ylab("Peso captura (Kg)") +
        labs(title = "Capturas vs Lances", subtitle = "Recta roja en media de Capturas") +
        theme_classic()

    plot_area_barr <- ggplot() +
        geom_point(data = pue_capt_mues_est, aes(x = LANCE, y = AREA_BARR)) +
        geom_abline(slope = 0, intercept = mean(pue_capt_mues_est$AREA_BARR),
        color = "red", linetype = 2, linewidth = .85) +
        xlab("Número de lance") +
        ylab("Area barrida (Mn2)") +
        labs(title = "Area barrida vs Lances", subtitle = "Recta roja en media de Area barrida") +
        theme_classic()

    plot_densidad <- ggplot() +
        geom_point(data = pue_capt_mues_est, aes(x = LANCE, y = DENSIDAD)) +
        geom_abline(slope = 0, intercept = mean(pue_capt_mues_est$DENSIDAD),
        color = "red", linetype = 2, linewidth = .85) +
        xlab("Número de lance") +
        ylab("Densidad (Ton/Mn2)") +
        labs(title = "Densidad vs Lances", subtitle = "Recta roja en media de Densidad") +
        theme_classic()

# Ploteamos los graficos en conjunto
print(plot_capt_mues)
grid.arrange(plot_capturas, plot_area_barr, ncol = 2) # Acomodo los de captura y area barrida en un solo panel
print(plot_densidad)
}


######################################################
############ ESCRITURA DE RESULTADOS ############
######################################################

# Escribo el conjunto de resultados para verlos en un solo objeto
if(mas == T){
    resultados = list(resumen_resultados = as.data.frame(pm_sum), biomasa_total = biom_total, distribucion_longitudes = as.data.frame(frecuencia), factores_expansion = as.data.frame(ponds))

resultados
} else{
    resultados = list(resumen_resultados = as.data.frame(pme_sum), biomasa_total = biom_total, distribucion_longitudes = as.data.frame(frecuencia), factores_expansion = as.data.frame(ponds))
    resultados
}
}