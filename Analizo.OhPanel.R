# Cargo paquetes
library(tidyverse)
library(Hmisc)
library(ggplot2)


# Levanto base de respuestas
ohpanel <- read_delim("ohpanel_base_20190725.txt", delim = "\t")
# Levanto labels de las preguntas
preguntas <- read_delim("ohpanel_labels_20190725.txt", delim = "\t")

# Genero etiquetas para las preguntas de la base
ohpanel <- Hmisc::upData(ohpanel, labels = preguntas$Label)



##############################
# Agrupaciones
##############################

# Edades
ohpanel <- ohpanel %>%
              mutate(agrupo_edad = case_when(P73 <= 24 ~ "Centennial",
                                             P73 > 24 & P73 <= 38 ~ "Millennial",
                                             TRUE ~ "X+BB"))

# Gastos TC
ohpanel <- ohpanel %>%
  mutate(agrupo_gasto_TC = case_when( is.na(P6) | P6 == 0 ~ "00 Sin info",
                                      P6 > 0 & P6 <= 10000   ~ "01 0 a 10k",
                                      P6 > 10000 & P6 <= 20000 ~ "02 10k a 20k",
                                      P6 > 20000 & P6 <= 50000 ~ "03 20k a 50k",
                                      P6 > 50000 & P6 <= 80000 ~ "04 50k a 80k",
                                      P6 > 80000 & P6 <= 150000 ~ "05 80k a 150k",
                                      TRUE ~ "06 + 150k")
         )


# Hijos a 0




###################
# Bi-variados
prop.table(table(ohpanel$P82, ohpanel$P75, exclude=NULL),2)

prop.table(table(ohpanel$agrupo_edad, ohpanel$P12, exclude=NULL),1)
prop.table(table(ohpanel$P12, ohpanel$P80, exclude=NULL),1)
prop.table(table(ohpanel$P12, ohpanel$P74, exclude=NULL),2)

prop.table(table(ohpanel$agrupo_edad, ohpanel$P34, exclude=NULL),1)
prop.table(table(ohpanel$agrupo_edad, ohpanel$P75, exclude=NULL),1)
prop.table(table(ohpanel$agrupo_edad, ohpanel$P80, exclude=NULL),1)
prop.table(table(ohpanel$agrupo_edad, ohpanel$P81, exclude=NULL),1)
prop.table(table(ohpanel$agrupo_edad, ohpanel$P82, exclude=NULL),1)
prop.table(table(ohpanel$agrupo_edad, ohpanel$P83, exclude=NULL),1)
prop.table(table(ohpanel$agrupo_edad, ohpanel$agrupo_gasto_TC),1)



table(ohpanel$P12)
# 5% desconocimiento
table(ohpanel$P12, ohpanel$P55, exclude=NULL)


# Plotting
ohpanel %>%
  filter(P73 < 100, P6 < 100000) %>%
  ggplot(aes(x=agrupo_edad, y =P6)) +
  geom_boxplot()
  #geom_jitter(alpha=0.4)






##############################
# Agrupaciones para análisis

########
# Tipo de Banco
bancos <- ohpanel %>%
            select(id, P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,
                   P3_7,P3_8,P3_9,P3_10,P3_11,P3_12,P3_13) %>%
              replace(is.na(.),0)

bancos <- bancos %>%
            mutate(tipo_banco = case_when( P3_2 > 0 | P3_5 > 0 ~ "01 COMPETENCIA",
                                           P3_3 > 0 & P3_1 == 0 & P3_2 == 0 & P3_4 == 0 & P3_5 == 0 & P3_6 == 0 & P3_7 == 0 & P3_8 == 0 & P3_9 == 0 & P3_10 == 0 & P3_11 == 0 & P3_12 == 0 & P3_13 == 0 ~ "02 SOLO SANTANDER",
                                           TRUE ~ "03 OTROS"
            ),
                  cantidad_bancos = (P3_1 + P3_2 + P3_3 + P3_4 + P3_5 + P3_6 + P3_7 + P3_8 + P3_9 + P3_10 + P3_11 + P3_12 + P3_13)
            )


table(bancos$tipo_banco)
table(bancos$cantidad_bancos)
# un 25% de los clientes tmb tienen algún producto con la competencia.
# el 50% sólo tiene productos con Santander



#####
# Productos Financieros

productos <- ohpanel %>%
              select(id, P4a_4,P4a_5,P4a_6,P4a_7,P4a_8,P4a_9,P4a_10,
                     P4b_4,P4b_5,P4b_6,P4b_7,P4b_8,P4b_9,P4b_10,
                     P4c_4,P4c_5,P4c_6,P4c_7,P4c_8,P4c_9,P4c_10,
                     P4d_4,P4d_5,P4d_6,P4d_7,P4d_8,P4d_9,P4d_10,
                     P4e_4,P4e_5,P4e_6,P4e_7,P4e_8,P4e_9,P4e_10,
                     P4f_4,P4f_5,P4f_6,P4f_7,P4f_8,P4f_9,P4f_10,
                     P4g_4,P4g_5,P4g_6,P4g_7,P4g_8,P4g_9,P4g_10,
                     P4h_4,P4h_5,P4h_6,P4h_7,P4h_8,P4h_9,P4h_10,
                     P4i_4,P4i_5,P4i_6,P4i_7,P4i_8,P4i_9,P4i_10,
                     P4j_4,P4j_5,P4j_6,P4j_7,P4j_8,P4j_9,P4j_10,
                     P4k_4,P4k_5,P4k_6,P4k_7,P4k_8,P4k_9,P4k_10,
                     P4l_4,P4l_5,P4l_6,P4l_7,P4l_8,P4l_9,P4l_10,
                     P4m_4,P4m_5,P4m_6,P4m_7,P4m_8,P4m_9,P4m_10,
                     P4n_4,P4n_5,P4n_6,P4n_7,P4n_8,P4n_9,P4n_10)

# TC
productos <- productos %>%
  mutate(
    tenencia_TC = case_when(
      P4a_4 > 0 | P4b_4 > 0 |
        P4c_4 > 0 | P4d_4 > 0 |
        P4e_4 > 0 | P4f_4 > 0 |
        P4g_4 > 0 | P4h_4 > 0 |
        P4i_4 > 0 | P4j_4 > 0 |
        P4k_4 > 0 | P4l_4 > 0 | 
        P4m_4 > 0 | 
        P4n_4 > 0 ~ 1,
      TRUE ~ 0
    ))

# Préstamos
productos <- productos %>%
  mutate( tenencia_prestamo = case_when(
          P4a_5 > 0 | P4a_6 > 0 | P4a_7 > 0 |
          P4b_5 > 0 | P4b_6 > 0 | P4b_7 > 0 | P4c_5 > 0 | P4c_6 > 0 | P4c_7 > 0 | P4d_5 > 0 | P4d_6 > 0 | P4d_7 > 0 |
          P4e_5 > 0 | P4e_6 > 0 | P4e_7 > 0 | P4f_5 > 0 | P4f_6 > 0 | P4f_7 > 0 | P4g_5 > 0 | P4g_6 > 0 | P4g_7 > 0 |
          P4h_5 > 0 | P4h_6 > 0 | P4h_7 > 0 | P4i_5 > 0 | P4i_6 > 0 | P4i_7 > 0 | P4j_5 > 0 | P4j_6 > 0 | P4j_7 > 0 |
          P4k_5 > 0 | P4k_6 > 0 | P4k_7 > 0 | P4l_5 > 0 | P4l_6 > 0 | P4l_7 > 0 | P4m_5 > 0 | P4m_6 > 0 | P4m_7 > 0 | 
          P4n_5 > 0 | P4n_6 > 0 | P4n_7 > 0 ~ 1,
      TRUE ~ 0
    ))

# Inversiones
productos <- productos %>%
  mutate(tenencia_inversion = case_when(
        P4a_8 > 0 | P4a_9 > 0 | P4b_8 > 0 | P4b_9 > 0 | P4c_8 > 0 | P4c_9 > 0 | P4d_8 > 0 | P4d_9 > 0 | P4e_8 > 0 |
        P4e_9 > 0 | P4f_8 > 0 | P4f_9 > 0 | P4g_8 > 0 | P4g_9 > 0 | P4h_8 > 0 | P4h_9 > 0 | P4i_8 > 0 | P4i_9 > 0 | 
        P4j_8 > 0 | P4j_9 > 0 | P4k_8 > 0 | P4k_9 > 0 | P4l_8 > 0 | P4l_9 > 0 | P4m_8 > 0 | P4m_9 > 0 | P4n_8 > 0 | 
        P4n_9 > 0 ~ 1,
      TRUE ~ 0
    ))

# Débito Automático
productos <- productos %>%
  mutate(tenencia_debito_aut = case_when(
        P4a_10 > 0 | P4b_10 > 0 | P4c_10 > 0 |  P4d_10 > 0 |P4e_10 > 0 | P4f_10 > 0 | P4g_10 > 0 |
        P4h_10 > 0 | P4i_10 > 0 | P4j_10 > 0 |  P4k_10 > 0 | P4l_10 > 0 | P4m_10 > 0 |
        P4n_10 > 0 ~ 1,
      TRUE ~ 0
    ))


table(productos$tenencia_TC, productos$tenencia_inversion)


############
# Antig en Bancos
antig <- ohpanel %>%
            select(id, P5a,	P5b,	P5c,	P5d,	P5e,	P5f,	P5g,	P5h,	P5i,	P5j,	P5k,	P5l,	P5m,	P5n)

antig <- antig %>%
          group_by(id) %>%
          mutate(max_antig = max(P5a,	P5b,	P5c,	P5d,	P5e,	P5f,	P5g,	P5h,	P5i,	P5j,	P5k,	P5l,	P5m,	P5n),
                 max_antig_otro = max(P5a,	P5b,	P5d,	P5e,	P5f,	P5g,	P5h,	P5i,	P5j,	P5k,	P5l,	P5m,	P5n))

antig <- antig %>%
  mutate(max_antig_gr = case_when(max_antig == 1 ~ "01 Hasta 6 meses",
                                  max_antig == 2 ~ "02 Entre 7 y 12 meses",
                                  max_antig == 3 ~ "03 Entre 13 y 24 meses",
                                  max_antig == 4 ~ "04 25 meses o más",
                                  TRUE ~ "00 revisar"))

antig <- antig %>%
  mutate(max_antig_otro_gr = case_when(max_antig_otro == 1 ~ "01 Hasta 6 meses",
                                       max_antig_otro == 2 ~ "02 Entre 7 y 12 meses",
                                       max_antig_otro == 3 ~ "03 Entre 13 y 24 meses",
                                       max_antig_otro == 4 ~ "04 25 meses o más",
                                       TRUE ~ "00 revisar"))


table(antig$max_antig_gr)
table(antig$max_antig_otro_gr)



###############
# Puntaje por Empresa

puntaje <- ohpanel %>%
              select(id, P9_1,P9_2,P9_3,P9_4,P9_5,P9_6,P9_7,P9_8,P9_9,P9_10,P9_11,P9_12,P9_13,P9_14)

puntaje_sum <- puntaje %>%
                  summarise(La_Caja = mean(P9_1, na.rm=T),
                            Provincia_Seguros = mean(P9_2, na.rm=T),
                            SanCor = mean(P9_3, na.rm=T),
                            Allianz = mean(P9_4, na.rm=T),
                            Mapfre = mean(P9_5, na.rm=T),
                            Federación_Patronal = mean(P9_6, na.rm=T),
                            Santander_Seguros = mean(P9_7, na.rm=T),
                            Zurich_Santander = mean(P9_8, na.rm=T),
                            Zurich = mean(P9_9, na.rm=T),
                            Galicia = mean(P9_10, na.rm=T),
                            BBVA = mean(P9_11, na.rm=T),
                            Hipotecario = mean(P9_12, na.rm=T),
                            Nación = mean(P9_13, na.rm=T),
                            Iunigo = mean(P9_14, na.rm=T)) %>%
                  gather(Cia, Puntaje_Medio) %>%
                  arrange(desc(Puntaje_Medio))



###############################
# Características por Empresa

caracteristica <- ohpanel %>%
                  select(id,
                         P10_1_1, P10_1_2, P10_1_3, P10_1_4, P10_1_5, P10_1_6, P10_1_7, P10_1_8, P10_1_9, P10_1_10, P10_1_11, P10_1_12, P10_1_13, P10_1_14, 
                         P10_2_1, P10_2_2, P10_2_3, P10_2_4, P10_2_5, P10_2_6, P10_2_7, P10_2_8, P10_2_9, P10_2_10, P10_2_11, P10_2_12, P10_2_13, P10_2_14, 
                         P10_3_1, P10_3_2, P10_3_3, P10_3_4, P10_3_5, P10_3_6, P10_3_7, P10_3_8, P10_3_9, P10_3_10, P10_3_11, P10_3_12, P10_3_13, P10_3_14, 
                         P10_4_1, P10_4_2, P10_4_3, P10_4_4, P10_4_5, P10_4_6, P10_4_7, P10_4_8, P10_4_9, P10_4_10, P10_4_11, P10_4_12, P10_4_13, P10_4_14, 
                         P10_5_1, P10_5_2, P10_5_3, P10_5_4, P10_5_5, P10_5_6, P10_5_7, P10_5_8, P10_5_9, P10_5_10, P10_5_11, P10_5_12, P10_5_13, P10_5_14, 
                         P10_6_1, P10_6_2, P10_6_3, P10_6_4, P10_6_5, P10_6_6, P10_6_7, P10_6_8, P10_6_9, P10_6_10, P10_6_11, P10_6_12, P10_6_13, P10_6_14, 
                         P10_7_1, P10_7_2, P10_7_3, P10_7_4, P10_7_5, P10_7_6, P10_7_7, P10_7_8, P10_7_9, P10_7_10, P10_7_11, P10_7_12, P10_7_13, P10_7_14, 
                         P10_8_1, P10_8_2, P10_8_3, P10_8_4, P10_8_5, P10_8_6, P10_8_7, P10_8_8, P10_8_9, P10_8_10, P10_8_11, P10_8_12, P10_8_13, P10_8_14, 
                         P10_9_1, P10_9_2, P10_9_3, P10_9_4, P10_9_5, P10_9_6, P10_9_7, P10_9_8, P10_9_9, P10_9_10, P10_9_11, P10_9_12, P10_9_13, P10_9_14, 
                         P10_10_1, P10_10_2, P10_10_3, P10_10_4, P10_10_5, P10_10_6, P10_10_7, P10_10_8, P10_10_9, P10_10_10, P10_10_11, P10_10_12, P10_10_13, P10_10_14, 
                         P10_11_1, P10_11_2, P10_11_3, P10_11_4, P10_11_5, P10_11_6, P10_11_7, P10_11_8, P10_11_9, P10_11_10, P10_11_11, P10_11_12, P10_11_13, P10_11_14, 
                         P10_12_1, P10_12_2, P10_12_3, P10_12_4, P10_12_5, P10_12_6, P10_12_7, P10_12_8, P10_12_9, P10_12_10, P10_12_11, P10_12_12, P10_12_13, P10_12_14)


map <- caracteristica %>%
          #select(c(2:15)) %>%
          gather(codigo,valor,P10_1_1:P10_12_14) %>%
          separate(codigo, c("hash","pregunta","compañía")) %>%
          group_by(pregunta,compañía) %>%
          summarise(n = sum(valor))


map <- map %>%
        mutate(Cia = case_when(pregunta == 1 ~ "01 Amplia Trayectoria",
                               pregunta == 2 ~ "02 Amplia Red",
                               pregunta == 3 ~ "03 Soluciones Digitales",
                               pregunta == 4 ~ "04 Buenos Costos",
                               pregunta == 5 ~ "05 Productos Distintos",
                               pregunta == 6 ~ "06 Solvente",
                               pregunta == 7 ~ "07 Poco Burocrática",
                               pregunta == 8 ~ "08 Confiable",
                               pregunta == 9 ~ "09 Líder",
                               pregunta == 10 ~ "10 Atención Rápida y Personalizada",
                               pregunta == 11 ~ "11 Innovadora",
                               pregunta == 12 ~ "12 Con Personalidad",
                               TRUE ~ "revisar"),
               Caracter = case_when(compañía == 1 ~ "01 La Caja",
                                    compañía == 2 ~ "02 Provincia Seguros",
                                    compañía == 3 ~ "03 Sancor",
                                    compañía == 4 ~ "04 Allianz",
                                    compañía == 5 ~ "05 Mapfre",
                                    compañía == 6 ~ "06 Federación Patronal",
                                    compañía == 7 ~ "07 Santander",
                                    compañía == 8 ~ "08 Zurich Santander",
                                    compañía == 9 ~ "09 Zurich",
                                    compañía == 10 ~ "10 Galicia",
                                    compañía == 11 ~ "11 BBVA",
                                    compañía == 12 ~ "12 Hipotecario",
                                    compañía == 13 ~ "13 Nación",
                                    compañía == 14 ~ "14 Iúnigo",
                                    TRUE ~ "revisar"
                                    )
               )



table(map$Caracter, map$Cia)

# Hago spread para pasar las Cias a columnas
final_map <- map %>%
                select(Cia, Caracter, n) %>%
                spread(Caracter,n)
# Escribo archivo para colorear en Excel
write.table(final_map, "caracter_vs_empresa.txt", sep = "\t")



# Preferencia de medios de contacto
contacto <- ohpanel %>%
              select(id,P27_1,P27_2,P27_3,P27_4,P27_5,P27_6,P27_7,P27_8,P27_9,P27_10,P27_11)

contacto <- contacto %>%
              mutate(telefono = P27_1,
                     online = ((P27_2+P27_3+P27_4)/3),
                     SMS = P27_6,
                     mail = P27_5,
                     whatsapp = P27_7,
                     redes = ((P27_8+P27_9+P27_10+P27_11)/4)
                     ) %>%
              select(id, telefono, online, SMS, mail, whatsapp, redes)




# Utilidad de Seguros
utilidad <- ohpanel %>%
              select(id,P32_1,P32_2,P32_3,P32_4,P32_5,P32_6,P32_7,P32_8,P32_9,P32_10,P32_11,P32_12,P32_13,P32_14,P32_15,P32_16,P32_17,P32_18,P32_19,P32_20)


utilidad2 <- utilidad %>%
              summarise( vida = sum(P32_1, na.rm = T),
                         vivienda = sum(P32_2, na.rm=T),
                         robo_cajero = sum(P32_3, na.rm=T),
                         robo_celular = sum(P32_4, na.rm=T),
                         robo_electronica = sum(P32_5, na.rm=T),
                         robo_cartera = sum(P32_6, na.rm=T),
                         desempleo_TC = sum(P32_7, na.rm=T),
                         salud = sum(P32_8, na.rm=T),
                         vida_ahorro = sum(P32_9, na.rm=T),
                         fallecimiento_acc = sum(P32_10, na.rm=T),
                         lesiones_acc = sum(P32_11, na.rm=T),
                         mascotas = sum(P32_12, na.rm=T),
                         falla_electro = sum(P32_13, na.rm=T),
                         seguro_viaje = sum(P32_14, na.rm=T),
                         pago_colegio = sum(P32_15, na.rm=T),
                         desempleo_alquiler = sum(P32_16, na.rm=T),
                         robo_musical = sum(P32_17, na.rm=T),
                         robo_deportivo = sum(P32_18, na.rm=T),
                         robo_bici = sum(P32_19, na.rm=T),
                         inasistencia_viaje = sum(P32_20, na.rm=T)) %>%
              gather(seguro, respuestas)


utilidad2 %>%
    ggplot(aes(x= reorder(seguro, -respuestas), y=respuestas)) +
    geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    xlab("Seguro") + ylab("") + ggtitle("Percepción de Utilidad de cada Seguro")
  


# Proximas Contrataciones
prox_contrato <- ohpanel %>%
  select(id,P33_1,P33_2,P33_3,P33_4,P33_5,P33_6,P33_7,P33_8,P33_9,P33_10,P33_11,P33_12,P33_13,P33_14,P33_15,P33_16,P33_17,P33_18,P33_19,P33_20)


prox_contrato2 <- prox_contrato %>%
  summarise(vida = sum(P33_1, na.rm=T),
    vivienda = sum(P33_2, na.rm=T),
    robo_cajero = sum(P33_3, na.rm=T),
    robo_celular = sum(P33_4, na.rm=T),
    robo_electronica = sum(P33_5, na.rm=T),
    robo_cartera = sum(P33_6, na.rm=T),
    desempleo_TC = sum(P33_7, na.rm=T),
    salud = sum(P33_8, na.rm=T),
    vida_ahorro = sum(P33_9, na.rm=T),
    fallecimiento_acc = sum(P33_10, na.rm=T),
    lesiones_acc = sum(P33_11, na.rm=T),
    mascotas = sum(P33_12, na.rm=T),
    falla_electro = sum(P33_13, na.rm=T),
    seguro_viaje = sum(P33_14, na.rm=T),
    pago_colegio = sum(P33_15, na.rm=T),
    desempleo_alquiler = sum(P33_16, na.rm=T),
    robo_musical = sum(P33_17, na.rm=T),
    robo_deportivo = sum(P33_18, na.rm=T),
    robo_bici = sum(P33_19, na.rm=T),
    inasistencia_viaje = sum(P33_20, na.rm=T)) %>%
      gather(seguro, respuestas)


prox_contrato2 %>%
  ggplot(aes(x= reorder(seguro, -respuestas), y=respuestas)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Seguro") + ylab("") + ggtitle("Próximos Seguros a ser contratados")



# Seguros Ofrecidos últimos 12 meses
seguros_ofrec <- ohpanel %>%
  select(id,P35_1,P35_2,P35_3,P35_4,P35_5,P35_6,P35_7,P35_8,P35_9,P35_10,P35_11,P35_12,P35_13,P35_14,P35_15,P35_16,P35_17,P35_18,P35_19,P35_20)


seguros_ofrec2 <- seguros_ofrec %>%
  summarise(vida = sum(P35_1),
    vivienda = sum(P35_2, na.rm=T),
    robo_cajero = sum(P35_3, na.rm=T),
    robo_celular = sum(P35_4, na.rm=T),
    robo_electronica = sum(P35_5, na.rm=T),
    robo_cartera = sum(P35_6, na.rm=T),
    desempleo_TC = sum(P35_7, na.rm=T),
    salud = sum(P35_8, na.rm=T),
    vida_ahorro = sum(P35_9, na.rm=T),
    fallecimiento_acc = sum(P35_10, na.rm=T),
    lesiones_acc = sum(P35_11, na.rm=T),
    mascotas = sum(P35_12, na.rm=T),
    falla_electro = sum(P35_13, na.rm=T),
    seguro_viaje = sum(P35_14, na.rm=T),
    pago_colegio = sum(P35_15, na.rm=T),
    desempleo_alquiler = sum(P35_16, na.rm=T),
    robo_musical = sum(P35_17, na.rm=T),
    robo_deportivo = sum(P35_18, na.rm=T),
    robo_bici = sum(P35_19, na.rm=T),
    inasistencia_viaje = sum(P35_20, na.rm=T)) %>%
      gather(seguro, respuestas)


seguros_ofrec2 %>%
  ggplot(aes(x= reorder(seguro, -respuestas), y=respuestas)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Seguro") + ylab("") + ggtitle("Seguros más ofertados a nuestros clientes")



# Oferta de Contratación directa por compañía
oferta_directa <- ohpanel %>%
  select(id,P37_1,P37_2,P37_3,P37_4,P37_5,P37_6,P37_7,P37_8,P37_9,P37_10,P37_11,P37_12,P37_13,P37_14,P37_15,P37_16,P37_17,P37_18,P37_19,P37_20)


oferta_directa2 <- oferta_directa %>%
  summarise(La_Caja = sum(P37_1),
    Provincia_Seguros = sum(P37_2, na.rm=T),
    SanCor = sum(P37_3, na.rm=T),
    Cardiff = sum(P37_4, na.rm=T),
    Federación_Patronal = sum(P37_5, na.rm=T),
    Allianz = sum(P37_6, na.rm=T),
    La_Segunda = sum(P37_7, na.rm=T),
    QBE = sum(P37_8, na.rm=T),
    La_Meridional = sum(P37_9, na.rm=T),
    Mercantil_Andina = sum(P37_10, na.rm=T),
    Mapfre = sum(P37_11, na.rm=T),
    Nacion_Seguros = sum(P37_12, na.rm=T),
    RSA_Sura = sum(P37_13, na.rm=T),
    San_Cristobal = sum(P37_14, na.rm=T),
    Santander_Seguros = sum(P37_15, na.rm=T),
    Zurich_Santander = sum(P37_16, na.rm=T),
    Zurich = sum(P37_17, na.rm=T),
    Galicia = sum(P37_18, na.rm=T),
    BBVA = sum(P37_19, na.rm=T),
    Iunigo = sum(P37_20, na.rm=T)) %>%
      gather(cia, respuestas)


oferta_directa2 %>%
  ggplot(aes(x= reorder(cia, -respuestas), y=respuestas)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Compañía") + ylab("") + ggtitle("Cías con más oferta directa a nuestros clientes")



# Oferta de Contratación por Broker
oferta_broker <- ohpanel %>%
  select(id,P38_1,P38_2,P38_3,P38_4,P38_5,P38_6,P38_7,P38_8,P38_9,P38_10,P38_11,P38_12,P38_13,P38_14,P38_15,P38_16,P38_17,P38_18,P38_19,P38_20)


oferta_broker2 <- oferta_broker %>%
  summarise(La_Caja = sum(P38_1),
            Provincia_Seguros = sum(P38_2, na.rm=T),
            SanCor = sum(P38_3, na.rm=T),
            Cardiff = sum(P38_4, na.rm=T),
            Federación_Patronal = sum(P38_5, na.rm=T),
            Allianz = sum(P38_6, na.rm=T),
            La_Segunda = sum(P38_7, na.rm=T),
            QBE = sum(P38_8, na.rm=T),
            La_Meridional = sum(P38_9, na.rm=T),
            Mercantil_Andina = sum(P38_10, na.rm=T),
            Mapfre = sum(P38_11, na.rm=T),
            Nacion_Seguros = sum(P38_12, na.rm=T),
            RSA_Sura = sum(P38_13, na.rm=T),
            San_Cristobal = sum(P38_14, na.rm=T),
            Santander_Seguros = sum(P38_15, na.rm=T),
            Zurich_Santander = sum(P38_16, na.rm=T),
            Zurich = sum(P38_17, na.rm=T),
            Galicia = sum(P38_18, na.rm=T),
            BBVA = sum(P38_19, na.rm=T),
            Iunigo = sum(P38_20, na.rm=T)) %>%
  gather(cia, respuestas)


oferta_broker2 %>%
  ggplot(aes(x= reorder(cia, -respuestas), y=respuestas)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Compañía") + ylab("") + ggtitle("Cías con más oferta x Broker a nuestros clientes")



# Info buscada por el cliente
info_previa <- ohpanel %>%
  select(id,P46_1,P46_2,P46_3,P46_4,P46_5,P46_6,P46_7)


info_previa2 <- info_previa %>%
  summarise(Google_otros = sum(P46_1, na.rm=T),
    Google_propio = sum(P46_2, na.rm=T),
    Amigos = sum(P46_3, na.rm=T),
    Redes_Sociales = sum(P46_4, na.rm=T),
    Productor = sum(P46_5, na.rm=T),
    Mail_Cia = sum(P46_6, na.rm=T),
    Nada = sum(P46_7, na.rm=T)) %>%
      gather(medio, respuestas)


info_previa2 %>%
  ggplot(aes(x= reorder(medio, -respuestas), y=respuestas)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Medios") + ylab("") + ggtitle("Cómo buscan info de Seguros nuestros clientes")




# Razón de No Contratación
no_contrato <- ohpanel %>%
  select(id,P47_1,P47_2,P47_3,P47_4,P47_5,P47_6,P47_7)


no_contrato2 <- no_contrato %>%
  summarise(no_necesito = sum(P47_1, na.rm=T),
            poco_probable_siniestro = sum(P47_2, na.rm=T),
            medidas_preventivas = sum(P47_3, na.rm=T),
            seguro_no_confiable = sum(P47_4, na.rm=T),
            costo_alto = sum(P47_5, na.rm=T),
            no_pienso_seguro = sum(P47_6, na.rm=T),
            precio = sum(P47_7, na.rm=T)) %>%
  gather(razon, respuestas)


no_contrato2 %>%
  ggplot(aes(x= reorder(razon, -respuestas), y=respuestas)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Razones") + ylab("") + ggtitle("Razones de NO contratación de Seguros")



# Relación con el Santander
contacto_san <- ohpanel %>%
  select(id,P48_1,P48_2,P48_3,P48_4,P48_5,P48_6,P48_7,P48_8,P48_9)


contacto_san2 <- contacto_san %>%
  summarise(Sucursal_presencial = sum(P48_1, na.rm=T),
            Telefono_ejecutivo = sum(P48_2, na.rm=T),
            Telefono_superlinea = sum(P48_3, na.rm=T),
            Online_Banking = sum(P48_4, na.rm=T),
            APP = sum(P48_5, na.rm=T),
            ATM = sum(P48_6, na.rm=T),
            SMS_Whatsapp = sum(P48_7, na.rm=T),
            Mailing = sum(P48_8, na.rm=T),
            Redes_Sociales = sum(P48_9, na.rm=T)) %>%
  gather(medio, respuestas)


contacto_san2 %>%
  ggplot(aes(x= reorder(medio, -respuestas), y=respuestas)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  xlab("Medio Contacto") + ylab("") + ggtitle("Medios más usados de Contacto con Santander")



#####################################
# Preferencias Contratación LEGO
lego <- ohpanel %>%
          select(id, agrupo_edad, 
                 P59_1,P59_2,P59_3,P59_4,P59_5,P59_6,P59_7,P59_8,P59_9,P59_10,
                 P33_1, P33_10, P33_11, P33_16, P33_8)


lego2 <-  lego %>%
            filter(P33_1 + P33_10 + P33_11 + P33_16 + P33_8 > 0) %>%
            filter(P59_1 + P59_2 + P59_3 + P59_4 + P59_5 + P59_6 + P59_7 + P59_8 + P59_9 + P59_10 > 0) %>%
            gather(codigo, valor, P59_1:P59_10) %>%
            mutate(vida = P33_1,
                   AP = max(P33_10, P33_11),
                   Desempleo = P33_16,
                   Salud = P33_8) %>%
            mutate(qty_prod = vida + AP + Desempleo + Salud,
                   canal = case_when(codigo == "P59_1" ~ "Sucursal",
                                     codigo == "P59_2" ~ "Telefonicamente",
                                     codigo == "P59_3" ~ "OLB",
                                     codigo == "P59_4" ~ "APP",
                                     codigo == "P59_5" ~ "Chat",
                                     codigo == "P59_6" ~ "ATM",
                                     codigo == "P59_7" ~ "Mail",
                                     codigo == "P59_8" ~ "SMS/Whatsapp",
                                     codigo == "P59_9" ~ "Redes_Sociales",
                                     codigo == "P59_10" ~ "Otros",
                                     TRUE ~ "revisar"),
                   canal_2 = case_when(codigo == "P59_1" ~ "Sucursal",
                                       codigo == "P59_2" ~ "Telefonicamente",
                                       codigo == "P59_3" ~ "OLB",
                                       codigo == "P59_4" ~ "App",
                                       TRUE ~ "Otro")) %>%
            select(id, agrupo_edad, canal, canal_2, valor, vida, AP, Desempleo, Salud, qty_prod) %>%
            group_by(agrupo_edad, canal, canal_2, valor, qty_prod) %>%
            summarise(suma_prod = n())

write.table(lego2, "histo_LEGO.txt")




#####################################
# Seguros de Personas vs Otros Bancos

personas <- ohpanel %>%
              filter(P33_1 + P33_10 + P33_11 + P33_16 + P33_8 > 0) %>%
              select(id, P33_1, P33_10, P33_11, P33_16, P33_8, P17_2, P17_5)


personas_vs_bancos <- inner_join(personas, bancos, by="id")

personas_vs_bancos <- personas_vs_bancos %>%
                        mutate(P17_2 = case_when(P17_2 > 0 ~ 1,
                                                 TRUE ~ 0),
                               P17_5 = case_when(P17_5 > 0 ~ 1,
                                                 TRUE ~ 0))
# Nuestros clientes en otros bancos
prop.table(table(personas_vs_bancos$tipo_banco))
# Nuestros clientes con seguros de otros bancos
prop.table(table(personas_vs_bancos$tipo_banco, personas_vs_bancos$P17_5),1)



#####################################
# Modos de contacto

contacto_personas <- ohpanel %>%
                        filter(P33_1 + P33_10 + P33_11 + P33_16 + P33_8 > 0) %>%
                        select(id, P29_1,P29_2,P29_3,P30_1,P30_2,P30_3,P31_1,P31_2,P31_3,
                               P27_1,P27_2,P27_3,P27_4,P27_5,P27_6,P27_7,P27_8,P27_9,P27_10,P27_11,
                               P23_1,P23_2,P23_3,P23_4,P23_5,P23_6,P23_7,P23_8,P23_9,P23_10,P23_11)


table(contacto_personas$P29_1) # Consultas
table(contacto_personas$P30_1) #Siniestros



contacto_personas2 <- contacto_personas %>%
  mutate(telefono = P23_1,
         online = ((P23_2+P23_3+P23_4)/3),
         SMS = P23_6,
         mail = P23_5,
         whatsapp = P23_7,
         redes = ((P23_8+P23_9+P23_10+P23_11)/4)) %>%
  select(id, telefono, online, SMS, mail, whatsapp, redes)

contacto_personas2 %>% summarise(telefono = mean(telefono),
                                 online = mean(online),
                                 SMS = mean(SMS),
                                 Mail = mean(mail),
                                 Whatsapp = mean(whatsapp),
                                 Redes = mean(redes))



#########################################3
# Puntaje por Empresa x grupo de edad

puntaje_edad <- ohpanel %>%
  select(id, agrupo_edad, P9_1,P9_2,P9_3,P9_4,P9_5,P9_6,P9_7,P9_8,P9_9,P9_10,P9_11,P9_12,P9_13,P9_14)

puntaje_edad_sum <- puntaje_edad %>%
  group_by(agrupo_edad) %>%
  summarise(La_Caja = mean(P9_1, na.rm=T),
            Provincia_Seguros = mean(P9_2, na.rm=T),
            SanCor = mean(P9_3, na.rm=T),
            Allianz = mean(P9_4, na.rm=T),
            Mapfre = mean(P9_5, na.rm=T),
            Federación_Patronal = mean(P9_6, na.rm=T),
            Santander_Seguros = mean(P9_7, na.rm=T),
            Zurich_Santander = mean(P9_8, na.rm=T),
            Zurich = mean(P9_9, na.rm=T),
            Galicia = mean(P9_10, na.rm=T),
            BBVA = mean(P9_11, na.rm=T),
            Hipotecario = mean(P9_12, na.rm=T),
            Nación = mean(P9_13, na.rm=T),
            Iunigo = mean(P9_14, na.rm=T))




#####################################
# Razones compra de Seguro en Santander

razones <- ohpanel %>%
  filter(P33_1 + P33_10 + P33_11 + P33_16 + P33_8 > 0) %>%
  select(id, agrupo_edad, P23_1,P23_2,P23_3,P23_4,P23_5,P23_6,P23_7,P23_8,P23_9)


razones2 <- razones %>%
  mutate(P23_1 = case_when(P23_1 > 0 ~ 1,
                           TRUE ~ 0),
         P23_2 = case_when(P23_2 > 0 ~ 1,
                           TRUE ~ 0),
         P23_3 = case_when(P23_3 > 0 ~ 1,
                           TRUE ~ 0),
         P23_4 = case_when(P23_4 > 0 ~ 1,
                           TRUE ~ 0),
         P23_5 = case_when(P23_5 > 0 ~ 1,
                           TRUE ~ 0),
         P23_6 = case_when(P23_6 > 0 ~ 1,
                           TRUE ~ 0),
         P23_7 = case_when(P23_7 > 0 ~ 1,
                           TRUE ~ 0),
         P23_8 = case_when(P23_8 > 0 ~ 1,
                           TRUE ~ 0),
         P23_9 = case_when(P23_9 > 0 ~ 1,
                           TRUE ~ 0)) %>%
  group_by(agrupo_edad) %>%
      summarise(personalmente = sum(P23_1),
                telefonicamente = sum(P23_2),
                web = sum(P23_3),
                app = sum(P23_4),
                chat_online = sum(P23_5),
                mail = sum(P23_6),
                sms = sum(P23_7),
                whatsapp = sum(P23_8),
                redes = sum(P23_9))



#####################################
# Coberturas Vida + AP valoradas

valoracion_cobertura <- ohpanel %>%
  filter(P33_1 + P33_10 + P33_11 + P33_16 + P33_8 > 0) %>%
  select(id, agrupo_edad, P63_1,P63_2,P63_3,P63_4,P65_1,P65_2,P65_3,P65_4,P65_5,P65_6,P65_7,P65_8,P65_9,P65_10,P65_11)


valoracion_cobertura2 <- valoracion_cobertura %>%
  mutate(P63_1 = case_when(P63_1 > 0 ~ 1,
                           TRUE ~ 0),
         P63_2 = case_when(P63_2 > 0 ~ 1,
                           TRUE ~ 0),
         P63_3 = case_when(P63_3 > 0 ~ 1,
                           TRUE ~ 0),
         P63_4 = case_when(P63_4 > 0 ~ 1,
                           TRUE ~ 0),
         P65_1 = case_when(P65_1 > 0 ~ 1,
                           TRUE ~ 0),
         P65_2 = case_when(P65_2 > 0 ~ 1,
                           TRUE ~ 0),
         P65_3 = case_when(P65_3 > 0 ~ 1,
                           TRUE ~ 0),
         P65_4 = case_when(P65_4 > 0 ~ 1,
                           TRUE ~ 0),
         P65_5 = case_when(P65_5 > 0 ~ 1,
                           TRUE ~ 0),
         P65_6 = case_when(P65_6 > 0 ~ 1,
                           TRUE ~ 0),
         P65_7 = case_when(P65_7 > 0 ~ 1,
                           TRUE ~ 0),
         P65_8 = case_when(P65_8 > 0 ~ 1,
                           TRUE ~ 0),
         P65_9 = case_when(P65_9 > 0 ~ 1,
                           TRUE ~ 0),
         P65_10 = case_when(P65_10 > 0 ~ 1,
                           TRUE ~ 0),
         P65_11 = case_when(P65_11 > 0 ~ 1,
                           TRUE ~ 0)) %>%
  group_by(agrupo_edad) %>%
  summarise(lesion_accidente = sum(P63_1),
            muerte_accidente = sum(P63_2),
            muerte_enfermedad = sum(P63_3),
            invalidez = sum(P63_4),
            muerte_cualquier_causa = sum(P65_1),
            doble_indemniza = sum(P65_2),
            invalidez_total = sum(P65_3),
            perdida_fisica = sum(P65_4),
            enfermedades_criticas = sum(P65_5),
            transplantes = sum(P65_6),
            segunda_opinion_mundo = sum(P65_7),
            cubre_hijos = sum(P65_8),
            segunda_opinion = sum(P65_9),
            cobertura_conyuges = sum(P65_10),
            cobertura_mundial = sum(P65_11))



#####################################
# Preferencias Contratación Vivienda
vivienda <- ohpanel %>%
  select(id, agrupo_edad, 
         P59_1,P59_2,P59_3,P59_4,P59_5,P59_6,P59_7,P59_8,P59_9,P59_10,
         P33_2)


vivienda2 <-  vivienda %>%
  filter(P33_2 > 0) %>%
  gather(codigo, valor, P59_1:P59_10) %>%
  mutate(canal = case_when(codigo == "P59_1" ~ "Sucursal",
                           codigo == "P59_2" ~ "Telefonicamente",
                           codigo == "P59_3" ~ "OLB",
                           codigo == "P59_4" ~ "APP",
                           codigo == "P59_5" ~ "Chat",
                           codigo == "P59_6" ~ "ATM",
                           codigo == "P59_7" ~ "Mail",
                           codigo == "P59_8" ~ "SMS/Whatsapp",
                           codigo == "P59_9" ~ "Redes_Sociales",
                           codigo == "P59_10" ~ "Otros",
                           TRUE ~ "revisar")) %>%
  select(id, agrupo_edad, canal, valor) %>%
  group_by(agrupo_edad, canal, valor) %>%
  filter(valor == 1) %>%
  summarise(suma_prod = n())


##############################################
# Comparación contra segmentos de Marketing
##############################################

# Proximas Contrataciones
segments <- ohpanel %>%
  select(id,P6,P73,P74,P75,P76,P77,P78,P80,
         agrupo_edad, agrupo_gasto_TC,
         P33_1,P33_2,P33_3,P33_4,P33_5,P33_6,P33_7,P33_8,P33_9,P33_10,P33_11,P33_12,P33_13,P33_14,P33_15,P33_16,P33_17,P33_18,P33_19,P33_20)


comprometides <- segments %>%
            filter(P74 == "Casado/a con hijos/as", agrupo_edad == "Millennial")
conectades <- segments %>%
            filter(agrupo_edad %in% c("Millennial","Centennial"))
viajeres <- segments %>%
  filter(P74 == "Soltero/a sin hijos/as", P73 >= 18, P73 <=30)
decolover <- segments %>%
  filter(P74 %in% c("Soltero/a con hijos/as","Casado/a con hijos/as"), P73 >= 25, P73 <=39)
healthy <- segments %>%
  filter(P74 %in% c("Soltero/a con hijos/as","Soltero/a sin hijos/as"),
         P77 %in% c("Posgrado Universitario","Universitario completo","Universitario incompleto"),
         P73 >= 25, P73 <=39)
masalla <- segments %>%
  filter(P74 %in% c("Casado/a con hijos/as","Casado/a sin hijos/as","Viudo/a con hijos/as","Separado/a €“ Divorciado/a con hijos/as"),
         P77 %in% c("Posgrado Universitario","Universitario completo","Universitario incompleto"),
         P73 >= 55)
comercio <- segments %>%
  filter(P78 == "Comerciante")


prop.table(table(comercio$P33_1))
prop.table(table(comercio$P33_2))
prop.table(table(comercio$P33_4))
prop.table(table(comercio$P33_5))
prop.table(table(comercio$P33_7))
prop.table(table(comercio$P33_8))
prop.table(table(comercio$P33_9))
prop.table(table(comercio$P33_12))
prop.table(table(comercio$P33_13))
prop.table(table(comercio$P33_14))
prop.table(table(comercio$P33_15))
prop.table(table(comercio$P33_16))
prop.table(table(comercio$P33_17))
prop.table(table(comercio$P33_18))
prop.table(table(comercio$P33_19))
prop.table(table(comercio$P33_20))

prop.table(table(comprometides$P33_11))
prop.table(table(comprometides$P33_3))


