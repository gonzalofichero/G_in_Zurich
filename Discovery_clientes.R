# Loading necessary packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(fmsb)
library(formattable)


# Loading the CLIENT file
client <- read.csv("C:/Users/B037123/Documents/04 - Mapa Clientes/TABLON_CLIENTES_201903.txt", sep = "|")

# Checking what's inside the file
glimpse(client)


###################
# EXTRA VARIABLES
client$qty_polizas <- (client$QTY_AP + client$QTY_ATM + client$QTY_OTRO + client$QTY_PRCARTERA + client$QTY_SALUD + client$QTY_VIDA + client$QTY_VIVIENDA)
client$qty_polizas_noatm <- (client$QTY_AP + client$QTY_OTRO + client$QTY_PRCARTERA + client$QTY_SALUD + client$QTY_VIDA + client$QTY_VIVIENDA)
client$percent_sucursal <- round(client$QTY_POLIZAS_X_SUCURSAL / client$qty_polizas, 4)
client$percent_TC <- round(client$QTY_POLIZAS_TC / client$qty_polizas, 4)
client$percent_digital <- round(client$QTY_POLIZAS_DIGITALES / client$qty_polizas, 4)


####################
# DISCOVERY

# 1) Clients per Segment
client$SEGMENTO %>%
    table() %>%
        prop.table() %>%
            '*'(100) %>%
                round(2)

# Most important segments:
# RENTA SELECT, RENTA MASIVA, RENTA MEDIA, RENTA ALTA, COMERCIOS
# ELSE OTROS

client2 <- client %>%
        mutate(SEGMENTO2 = case_when(client$SEGMENTO == "RENTA SELECT" ~ "04 renta_select",
                                    client$SEGMENTO == "RENTA MASIVA" ~ "01 renta_masiva",
                                    client$SEGMENTO == "RENTA MEDIA" ~ "02 renta_media",
                                    client$SEGMENTO == "RENTA ALTA" ~ "03 renta_alta",
                                    client$SEGMENTO == "COMERCIOS" ~ "05 comercios",
                                    TRUE ~ "06 otro")
        )


# Let's check again
client2$SEGMENTO2 %>%
                  table() %>%
                    prop.table() %>%
                      '*'(100) %>%
                        round(2)



#################################
# Taking a sample for plotting
c_sample <- sample_n(client2, 10000)



# By Segment and Gender
prop.table(table(client2$SEGMENTO2, client2$GENERO),2)
prop.table(table(client2$SEGMENTO2, client2$GENERO),1)


# By Age and Segment
prop.table(table(client2$SEGMENTO2, client2$EDAD_A),2)
client2 %>%
  filter(EDAD_A < 100) %>%
  ggplot(aes(y=EDAD_A, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()


# Premio by Segment
client2 %>%
    filter(TOTAL_PREMIO < 2500, SEGMENTO2 != "06 otro") %>%
      ggplot(aes(x=TOTAL_PREMIO, color=as.factor(SEGMENTO2)))  + geom_density()

c_sample %>%
  filter(TOTAL_PREMIO < 2500, SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=TOTAL_PREMIO, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot(show.legend = F) + ylab("Premio Cliente") + xlab("Segmento")


# SA/Premio by Segment
c_sample %>%
  filter(TOTAL_PREMIO < 10000, (TOTAL_SA/TOTAL_PREMIO) < 1000, SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=(TOTAL_SA/qty_polizas), x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()


# Premio by Poliza by Segment
c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=qty_polizas, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()

c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=qty_polizas_noatm, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()

c_sample %>%
  filter(TOTAL_PREMIO < 2500, SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=(TOTAL_PREMIO/qty_polizas), x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot(show.legend = F) + xlab("Segmento") + ylab("Premio x Poliza x Cliente")


# % TC by Segment
c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=percent_TC, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()


# % Sucursal by Segment
c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=percent_sucursal, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()

# % Digital by Segment
c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=percent_digital, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()


# Antig of each client for <> Segments

c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=(MAX_ANTIG_ACTUAL/MAX_ANTIG_TOTAL), x=SEGMENTO2, color=as.factor(SEGMENTO2)))  + geom_boxplot()

c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(x=(MAX_ANTIG_ACTUAL/MAX_ANTIG_TOTAL), color=as.factor(SEGMENTO2)))  + geom_density()

c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios", MAX_ANTIG_ACTUAL <= 60) %>%
  ggplot(aes(y=MAX_ANTIG_ACTUAL, x=MAX_ANTIG_TOTAL, color=as.factor(SEGMENTO2)))  + geom_point(show.legend = F)# + scale_y_continuous(breaks = c(seq(0,300,5)))
# Inyección de Cartera en 201601, 33% Renta Alta...
client3 %>% filter(MAX_ANTIG_ACTUAL <= 40, MAX_ANTIG_ACTUAL >= 37) %>% select(SEGMENTO2) %>%
  table %>% prop.table()

c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=MAX_ANTIG_ACTUAL, x=SEGMENTO2, color=as.factor(SEGMENTO2)))  + geom_boxplot(show.legend = F) + xlab("Segmento") + ylab("Antig. Vigente")


# % TC by Gender
c_sample %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=percent_TC, x=as.factor(GENERO), color=as.factor(GENERO)))  + geom_boxplot()


# NPS by Segment
c_sample %>%
  #filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
  ggplot(aes(y=NPS, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()


# Age by Segment
c_sample %>%
  filter(EDAD_A < 100, SEGMENTO2 != "06 otro") %>%
  ggplot(aes(y=EDAD_A, x=as.factor(SEGMENTO2), color=as.factor(SEGMENTO2)))  + geom_boxplot()


# Vivienda by Segment
c_sample %>% 
  with(table(SEGMENTO2, QTY_VIVIENDA)) %>% 
    prop.table(margin=1)

# Vida by Segment
c_sample %>% 
  with(table(SEGMENTO2, QTY_VIDA)) %>% 
  prop.table(margin=1)

# AP by Segment
c_sample %>% 
  with(table(SEGMENTO2, QTY_AP)) %>% 
  prop.table(margin=1)

# ATM by Segment
c_sample %>% 
  with(table(SEGMENTO2, QTY_ATM)) %>% 
  prop.table(margin=1)

# Prot Cartera by Segment
c_sample %>% 
  with(table(SEGMENTO2, QTY_PRCARTERA)) %>% 
  prop.table(margin=1)

# Otro by Segment
c_sample %>% 
  with(table(SEGMENTO2, QTY_OTRO)) %>% 
  prop.table(margin=1)

# Salud by Segment
c_sample %>% 
  with(table(SEGMENTO2, QTY_SALUD)) %>% 
  prop.table(margin=1)



# Create binary variables to understand cross product ownership
client3 <- client2 %>%
    mutate(Big_3 = case_when(client2$QTY_VIVIENDA > 0 &
                             client2$QTY_VIDA > 0 &
                             client2$QTY_AP > 0 ~ 1,
                             TRUE ~ 0),
           Big_4 = case_when(client2$QTY_VIVIENDA > 0 &
                             client2$QTY_VIDA > 0 &
                             client2$QTY_PRCARTERA > 0 &
                             client2$QTY_AP > 0 ~ 1,
                             TRUE ~ 0),
         Vida_AP = case_when(client2$QTY_VIDA > 0 &
                             client2$QTY_AP > 0 ~ 1,
                             TRUE ~ 0),
         Vida_AP_Salud = case_when(client2$QTY_SALUD > 0 &
                                   client2$QTY_VIDA > 0 &
                                   client2$QTY_AP > 0 ~ 1,
                                   TRUE ~ 0),
           Vida_Vivienda = case_when(client2$QTY_VIVIENDA > 0 &
                                     client2$QTY_VIDA > 0 &
                                     client2$QTY_AP == 0 ~ 1,
                                     TRUE ~ 0),
             AP_Vivienda = case_when(client2$QTY_VIVIENDA > 0 &
                                     client2$QTY_AP > 0 &
                                     client2$QTY_VIDA == 0 ~ 1,
                                     TRUE ~ 0))

###########################################################
# Prop Tables to check clients with cross for <> Segments

#  Vivienda + AP + Vida
client3 %>% 
  with(table(SEGMENTO2, Big_3)) %>% 
  prop.table(margin=1)

#  Vivienda + AP + Vida + Proteccion Cartera
client3 %>% 
  with(table(SEGMENTO2, Big_4)) %>% 
  prop.table(margin=1)

#  AP + Vida
client3 %>% 
  with(table(SEGMENTO2, Vida_AP)) %>% 
  prop.table(margin=1)

#  AP + Vida + Salud
client3 %>% 
  with(table(SEGMENTO2, Vida_AP_Salud)) %>% 
  prop.table(margin=1)

#  Vida + Vivienda (no AP)
client3 %>% 
  with(table(SEGMENTO2, Vida_Vivienda)) %>% 
  prop.table(margin=1)

#  AP + Vivienda (no Vida)
client3 %>% 
  with(table(SEGMENTO2, AP_Vivienda)) %>% 
  prop.table(margin=1)




###############################################
# Grouping by info into radarchart structure
client_radar <- client3 %>%
  select(SEGMENTO2, qty_polizas, percent_sucursal, percent_TC,
         percent_digital, MAX_ANTIG_ACTUAL, TOTAL_PREMIO, EDAD_A) %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
        group_by(SEGMENTO2) %>%
          summarize(avg_qty_poliza = mean(qty_polizas, na.rm = T),
                    avg_pct_sucursal = mean(percent_sucursal, na.rm = T),
                    avg_pct_TC = mean(percent_TC, na.rm = T),
                    avg_pct_digital = mean(percent_digital, na.rm = T),
                    avg_premio = mean(TOTAL_PREMIO, na.rm = T),
                    avg_antig = mean(MAX_ANTIG_ACTUAL, na.rm = T),
                    avg_edad = mean(EDAD_A, na.rm = T)) %>%
                        as.data.frame() 


##################################
# Configuring radarchart options
rownames(client_radar) <- client_radar$SEGMENTO2
colors_border <- c("red", "blue", "green", "orange")
colors_in <- c("pink", "lightblue", "lightgreen", "lightyellow")


client_radar %>%
    select(-1) %>%
      radarchart(axistype = 1,
                 maxmin=FALSE,
                 pcol=colors_border,
                 plwd=2, 
                 plty=1,
                 #customize the grid
                 cglcol="grey", 
                 cglty=1, 
                 axislabcol="grey", 
                 caxislabels=seq(0,1,0.2), 
                 cglwd=0.8,
                 #custom labels
                 vlcex=0.8)
legend(x=1.5, y=1.5, legend=rownames(client_radar[,]), 
       bty="n", pch=15 , col=colors_border , 
       text.col="grey", cex=1.2, pt.cex=3)



###############################################
# Grouping by info into radarchart structure
client_cross <- client3 %>%
  select(SEGMENTO2, QTY_VIVIENDA, QTY_VIDA, QTY_AP,
         QTY_PRCARTERA, QTY_ATM, QTY_SALUD, QTY_OTRO) %>%
  filter(SEGMENTO2 != "06 otro", SEGMENTO2 != "05 comercios") %>%
      mutate(QTY_VIVIENDA = case_when(QTY_VIVIENDA > 0 ~ 1,
                                      TRUE ~ 0),
             QTY_VIDA = case_when(QTY_VIDA > 0 ~ 1,
                                      TRUE ~ 0),
             QTY_AP = case_when(QTY_AP > 0 ~ 1,
                                      TRUE ~ 0),
             QTY_PRCARTERA = case_when(QTY_PRCARTERA > 0 ~ 1,
                                      TRUE ~ 0),
             QTY_ATM = case_when(QTY_ATM > 0 ~ 1,
                                      TRUE ~ 0),
             QTY_SALUD = case_when(QTY_SALUD > 0 ~ 1,
                                      TRUE ~ 0),
             QTY_OTRO = case_when(QTY_OTRO > 0 ~ 1,
                                      TRUE ~ 0)) %>%
      group_by(SEGMENTO2) %>%
        summarize(p_vivienda = mean(QTY_VIVIENDA, na.rm = T),
                  p_vida = mean(QTY_VIDA, na.rm = T),
                  p_ap = mean(QTY_AP, na.rm = T),
                  p_cartera = mean(QTY_PRCARTERA, na.rm = T),
                  p_atm = mean(QTY_ATM, na.rm = T),
                  p_salud = mean(QTY_SALUD, na.rm = T),
                  p_otro = mean(QTY_OTRO, na.rm = T)) %>%
          as.data.frame() 


##################################
# Configuring radarchart options
rownames(client_cross) <- client_cross$SEGMENTO2
colors_border <- c("red", "blue", "green", "orange")
colors_in <- c("pink", "lightblue", "lightgreen", "lightyellow")


client_cross %>%
  select(-1) %>%
  radarchart(axistype = 1,
             maxmin=FALSE,
             pcol=colors_border, 
             pfcol=colors_in,
             plwd=2, 
             plty=1,
             pdensity = 0,
             #customize the grid
             cglcol="grey", 
             cglty=1, 
             axislabcol="grey", 
             caxislabels=seq(0,1,0.2), 
             cglwd=0.8,
             #custom labels
             vlcex=0.8)
legend(x=1.5, y=1.5, legend=rownames(client_radar[,]), 
       bty="n", pch=15 , col=colors_border , 
       text.col="grey", cex=1.2, pt.cex=3)
