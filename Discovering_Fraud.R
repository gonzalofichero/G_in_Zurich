# Loading libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(caret)
library(rattle)
library(fpc)
library(dbscan)


# Importing the data set
vivi03 <- read.csv("C:/Users/B037123/Documents/06 - Red Flag Siniestros Vivienda/SINIESTROS_VIVI_201811_201903_v2.txt", sep="|",header = T, encoding = "UTF-8")


glimpse(vivi03)

# Counting types of circuits
table(vivi03$CIRCUITO_LIQUIDACION_ID)

# Eliminating the 1 type 5 case
vivi03 <- vivi03 %>% filter(CIRCUITO_LIQUIDACION_ID <= 3)



# Circuito de Liquidación
# 1 = Fast Track
# 2 = Administrativo
# 3 = Liquidador
# 4 = Judicial



# 1) Creating Time Variables for analysis
# 2) Creating index between Cobertura and Damage estimated by Client
# 3) Percentage of previous Siniestros from Fast Track
vivi03 <- vivi03 %>%
              mutate(MONTHS_SINIESTRO_FIRSTDATE = interval(dmy(PRIMER_POLIZA_FC), dmy(OCURRENCIA_FC)) %/% months(1),
                     MONTHS_SINIESTRO_DECLARACION = interval(dmy(DECLARACION_FC), dmy(OCURRENCIA_FC)) %/% months(1),
                     MONTHS_SINIESTRO_INICIO_POL = interval(dmy(VIGENCIA_CERTIFICADO_DESDE_FC), dmy(OCURRENCIA_FC)) %/% months(1),
                     MONTHS_SINIESTRO_FIN_POL = interval(dmy(VIGENCIA_CERTIFICADO_HASTA_FC), dmy(OCURRENCIA_FC)) %/% months(1),
                     INDEX_DECLARA_COBERTURA = round(ESTIMACION_DAÑO_CLIENTE_DE / SUMA_ASEGURADA_DE, 4),
                     PERC_SIN_1MONTH_FS = round(QTY_SINIESTRO_1MONTH_FS / QTY_SINIESTRO_1MONTH, 4),
                     PERC_SIN_3MONTH_FS = round(QTY_SINIESTRO_3MONTH_FS / QTY_SINIESTRO_3MONTH, 4),
                     PERC_SIN_6MONTH_FS = round(QTY_SINIESTRO_6MONTH_FS / QTY_SINIESTRO_6MONTH, 4)
                     ) %>%
              replace(is.na(.),0)

# Checking that the %'s make sense
vivi03 %>% group_by(SINIESTRO_ID) %>% filter(PERC_SIN_6MONTH_FS > 1) %>% summarise(n = n())
vivi03 %>% group_by(SINIESTRO_ID) %>% filter(PERC_SIN_3MONTH_FS > 1) %>% summarise(n = n())
# Eliminating what makes no sense: there is a duplicity for some Certificados in the query. Should be at Siniestro_Id level, not Certificado
vivi03 <- vivi03 %>% filter(PERC_SIN_6MONTH_FS <= 1)
vivi03 <- vivi03 %>% filter(PERC_SIN_3MONTH_FS <= 1)


#####################
# DISCOVERY

##################################
# Tasa de Aprobación por Circuito
vivi03 %>% 
  group_by(CIRCUITO_LIQUIDACION_ID, ESTADO_SINIESTRO_TX) %>%
    summarise(n = n())
# Dado que muchos quedan abiertos, me quedo sólo con los Siniestros
# Rechazados y Pagados de Circuito 2 (Analista)



#########
# Comparación por fechas de inicio y fin de pólizas (con y sin renovación)
########
vivi03 %>%
  filter(ESTIMACION_DAÑO_CLIENTE_DE < 250000) %>%
        ggplot(aes(x=ESTIMACION_DAÑO_CLIENTE_DE,y=MONTHS_SINIESTRO_FIRSTDATE, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
        geom_point(alpha=0.5) + guides(color=guide_legend(title="Circuito"))

vivi03 %>%
  filter(ESTIMACION_DAÑO_CLIENTE_DE < 100000) %>%
  ggplot(aes(x=ESTIMACION_DAÑO_CLIENTE_DE,y=MONTHS_SINIESTRO_INICIO_POL, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_point(alpha=0.5) + guides(color=guide_legend(title="Circuito"))


with(vivi03,prop.table(table(CIRCUITO_LIQUIDACION_ID, MONTHS_SINIESTRO_INICIO_POL),1))
# 23% of Fast Track claims have less than 3 months since the Poliza renovó
with(vivi03[vivi03$MONTHS_SINIESTRO_FIRSTDATE < 37 & vivi03$MONTHS_SINIESTRO_FIRSTDATE > 11,],prop.table(table(CIRCUITO_LIQUIDACION_ID, MONTHS_SINIESTRO_INICIO_POL),1))
with(vivi03,prop.table(table(CIRCUITO_LIQUIDACION_ID, MONTHS_SINIESTRO_FIRSTDATE),1))


vivi03 %>%
  filter(MONTHS_SINIESTRO_FIRSTDATE < 49, MONTHS_SINIESTRO_INICIO_POL <= 6) %>%
  ggplot(aes(x=MONTHS_SINIESTRO_INICIO_POL,y=MONTHS_SINIESTRO_FIRSTDATE, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_jitter(alpha=0.5, height = 1, width = 1) + guides(color=guide_legend(title="Circuito"))

vivi03 %>%
  filter(MONTHS_SINIESTRO_FIRSTDATE < 61) %>%
  ggplot(aes(x=MONTHS_SINIESTRO_FIN_POL,y=MONTHS_SINIESTRO_FIRSTDATE, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_jitter(alpha=0.5, height = 1, width = 1) + guides(color=guide_legend(title="Circuito"))



#########
# Comparación por sumas aseguradas, daños declarados
########


vivi03 %>%
  filter(ESTIMACION_DAÑO_CLIENTE_DE < 100000, SUMA_ASEGURADA_DE < 3000000) %>%
  ggplot(aes(x=ESTIMACION_DAÑO_CLIENTE_DE,y=SUMA_ASEGURADA_DE, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_point(alpha=0.5) + guides(color=guide_legend(title="Circuito"))


vivi03 %>%
  filter(INDEX_DECLARA_COBERTURA < 5, ESTIMACION_DAÑO_CLIENTE_DE < 15000) %>%
  ggplot(aes(x=ESTIMACION_DAÑO_CLIENTE_DE,y=INDEX_DECLARA_COBERTURA, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_point(alpha=0.3) + guides(color=guide_legend(title="Circuito"))

vivi03 %>%
  filter(ESTIMACION_DAÑO_CLIENTE_DE < 30000, SUMA_ASEGURADA_DE < 35000) %>%
  ggplot(aes(x=ESTIMACION_DAÑO_CLIENTE_DE,y=SUMA_ASEGURADA_DE, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_point(alpha=0.3) + guides(color=guide_legend(title="Circuito"))

vivi03 %>%
  filter(ESTIMACION_DAÑO_CLIENTE_DE < 30000, SUMA_ASEGURADA_DE < 35000) %>%
  ggplot(aes(x=CIRCUITO_LIQUIDACION_ID,y=INDEX_DECLARA_COBERTURA, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_boxplot() + guides(color=guide_legend(title="Circuito"))

vivi03 %>%
  filter(INDEX_DECLARA_COBERTURA < 3, ESTIMACION_DAÑO_CLIENTE_DE < 250000) %>%
  ggplot(aes(x=ESTIMACION_DAÑO_CLIENTE_DE,y=MONTHS_SINIESTRO_INICIO_POL, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_point() + guides(color=guide_legend(title="Circuito"))

vivi03 %>%
  filter(MONTHS_SINIESTRO_FIRSTDATE < 150) %>%
  ggplot(aes(x=MONTHS_SINIESTRO_INICIO_POL,y=MONTHS_SINIESTRO_FIRSTDATE, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_jitter(alpha=0.5, width = 1, height = 1) + guides(color=guide_legend(title="Circuito"))

vivi03 %>%
  filter(MONTHS_SINIESTRO_FIRSTDATE < 25) %>%
  ggplot(aes(x=MONTHS_SINIESTRO_INICIO_POL,y=MONTHS_SINIESTRO_FIRSTDATE, color=as.factor(PRODUCTO_EMPLEADOS_FL))) + 
  geom_jitter(alpha=0.5, width = 1, height = 1) + guides(color=guide_legend(title="Empleado"))

vivi03 %>%
  filter(ESTIMACION_DAÑO_CLIENTE_DE < 55000, SUMA_ASEGURADA_DE < 150000) %>%
  ggplot(aes(x=ESTIMACION_DAÑO_CLIENTE_DE,y=SUMA_ASEGURADA_DE, color=as.factor(PRODUCTO_EMPLEADOS_FL))) + 
  geom_point(alpha=0.5) + guides(color=guide_legend(title="Empleado"))


vivi03 %>%
  #filter(ESTIMACION_DAÑO_CLIENTE_DE < 55000, SUMA_ASEGURADA_DE < 150000) %>%
  ggplot(aes(x=as.factor(PRODUCTO_EMPLEADOS_FL),y=MONTHS_SINIESTRO_INICIO_POL, color=as.factor(PRODUCTO_EMPLEADOS_FL))) + 
  geom_boxplot() + guides(color=guide_legend(title="Empleado")) + xlab("Empleado = S, No Empleado = N")

with(vivi03, prop.table(table(PRODUCTO_EMPLEADOS_FL,ESTADO_SINIESTRO_TX),1))
with(vivi03, prop.table(table(CIRCUITO_LIQUIDACION_ID,PRODUCTO_EMPLEADOS_FL),2))
with(vivi03, prop.table(table(CIRCUITO_LIQUIDACION_ID,ESTADO_SINIESTRO_TX,PRODUCTO_EMPLEADOS_FL),1))

vivi03 %>% group_by(PRODUCTO_EMPLEADOS_FL) %>% summarise(avg = mean(MONTHS_SINIESTRO_FIRSTDATE),
                                                         med = median(MONTHS_SINIESTRO_FIRSTDATE))

vivi03 %>% filter(CIRCUITO_LIQUIDACION_ID == 2) %>% group_by(PRODUCTO_EMPLEADOS_FL) %>% summarise(avg = mean(ESTIMACION_DAÑO_CLIENTE_DE),
                                                         med = median(ESTIMACION_DAÑO_CLIENTE_DE))


#########
# Comparación por Siniestros anteriores
########
siniestro <- vivi03 %>%
  select(CIRCUITO_LIQUIDACION_ID,
         ESTIMACION_DAÑO_CLIENTE_DE,
         SUMA_ASEGURADA_DE,QTY_SINIESTRO_1MONTH_FS,
         QTY_SINIESTRO_1MONTH,QTY_SINIESTRO_3MONTH,QTY_SINIESTRO_6MONTH,
         QTY_SINIESTRO_3MONTH_FS, QTY_SINIESTRO_6MONTH_FS,
         MONTHS_SINIESTRO_FIRSTDATE, MONTHS_SINIESTRO_DECLARACION,
         MONTHS_SINIESTRO_INICIO_POL,MONTHS_SINIESTRO_FIN_POL,
         INDEX_DECLARA_COBERTURA,PERC_SIN_1MONTH_FS,
         PERC_SIN_3MONTH_FS,PERC_SIN_6MONTH_FS) %>%
  replace(is.na(.),0)

# Siniestros anteriores
siniestro %>%
  ggplot(aes(x=as.factor(CIRCUITO_LIQUIDACION_ID),y=QTY_SINIESTRO_1MONTH)) + geom_boxplot()
siniestro %>%
  ggplot(aes(x=as.factor(CIRCUITO_LIQUIDACION_ID),y=QTY_SINIESTRO_3MONTH)) + geom_boxplot()
siniestro %>%
  ggplot(aes(x=as.factor(CIRCUITO_LIQUIDACION_ID),y=QTY_SINIESTRO_6MONTH)) + geom_boxplot()

# Analizar porcentajes

# % Siniestros FS sobre total por Circuito
siniestro %>%
  filter(INDEX_DECLARA_COBERTURA < 10) %>%
  ggplot(aes(x = PERC_SIN_6MONTH_FS, y=INDEX_DECLARA_COBERTURA, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_jitter(alpha=0.4) + guides(color=guide_legend(title="Circuito"))

siniestro %>%
  filter(INDEX_DECLARA_COBERTURA < 10) %>%
  ggplot(aes(x = QTY_SINIESTRO_3MONTH, y=QTY_SINIESTRO_6MONTH, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_jitter(alpha=0.4) + guides(color=guide_legend(title="Circuito"))

siniestro %>%
  filter(INDEX_DECLARA_COBERTURA < 10, ESTIMACION_DAÑO_CLIENTE_DE < 30000, SUMA_ASEGURADA_DE < 35000) %>%
  ggplot(aes(x = QTY_SINIESTRO_6MONTH, y=PERC_SIN_6MONTH_FS, color=as.factor(CIRCUITO_LIQUIDACION_ID))) + 
  geom_jitter(alpha=0.4) + guides(color=guide_legend(title="Circuito"))




####################################
###################################
# EXTRACT FAST TRACK RULES
viv_rules <- vivi03 %>%
  filter(CIRCUITO_LIQUIDACION_ID < 3) %>%
  select(CIRCUITO_LIQUIDACION_ID,
         ESTIMACION_DAÑO_CLIENTE_DE,
         SUMA_ASEGURADA_DE,
         PRODUCTO_EMPLEADOS_FL,
         QTY_SINIESTRO_1MONTH_FS,
         QTY_SINIESTRO_1MONTH,QTY_SINIESTRO_3MONTH,QTY_SINIESTRO_6MONTH,
         QTY_SINIESTRO_3MONTH_FS, QTY_SINIESTRO_6MONTH_FS,
         MONTHS_SINIESTRO_FIRSTDATE, MONTHS_SINIESTRO_DECLARACION,
         MONTHS_SINIESTRO_INICIO_POL,MONTHS_SINIESTRO_FIN_POL,
         INDEX_DECLARA_COBERTURA,PERC_SIN_1MONTH_FS,
         PERC_SIN_3MONTH_FS,PERC_SIN_6MONTH_FS) %>%
  replace(is.na(.),0)

# Factoring the Objective 
viv_rules$CIRCUITO_LIQUIDACION_ID <- as.factor(viv_rules$CIRCUITO_LIQUIDACION_ID)

# Setting random seed
set.seed(1956451)

# Training and testing (75-25)
inTrain <- createDataPartition(y = viv_rules$CIRCUITO_LIQUIDACION_ID, p = .75,  list = FALSE)
train_fs <- viv_rules[inTrain,]
test_fs <- viv_rules[-inTrain,]

# No fit_control needed. We actually want to overfit to find the hidden rules
fit_control <- trainControl(method = "cv", number = 10)

rf_fit <- train(CIRCUITO_LIQUIDACION_ID ~ ESTIMACION_DAÑO_CLIENTE_DE +
                  PRODUCTO_EMPLEADOS_FL +
                  SUMA_ASEGURADA_DE +
                  QTY_SINIESTRO_3MONTH +
                  QTY_SINIESTRO_3MONTH_FS +
                  QTY_SINIESTRO_6MONTH +
                  MONTHS_SINIESTRO_DECLARACION +
                  MONTHS_SINIESTRO_FIRSTDATE +
                  MONTHS_SINIESTRO_INICIO_POL +
                  INDEX_DECLARA_COBERTURA,
                data = train_fs, # trying to overfit...
                trControl = fit_control,
                method = "rpart2", maxdepth = 5)

# Predicting with the model and comparing
predict_fs <- predict(rf_fit, newdata = test_fs)

# 93.46% accuracy for FastTrack category (Sensitivity)
confusionMatrix(data = predict_fs, test_fs$CIRCUITO_LIQUIDACION_ID)

# Extracting the rules: Daño Declarado, Indice Declarado/Cobertura, Siniestros last 3 months
fancyRpartPlot(rf_fit$finalModel)



############################
# PCA

# Genero componentes principales sobre las variables más importantes
# Paso a 0 los Nulls
vivi_pca <- vivi03 %>%
            select(SINIESTRO_ID,CIRCUITO_LIQUIDACION_ID,
                   QTY_SINIESTRO_3MONTH, QTY_SINIESTRO_6MONTH,
                   QTY_SINIESTRO_3MONTH_FS, QTY_SINIESTRO_6MONTH_FS,
                   MONTHS_SINIESTRO_FIRSTDATE,
                   MONTHS_SINIESTRO_INICIO_POL,MONTHS_SINIESTRO_FIN_POL,
                   PERC_SIN_3MONTH_FS,PERC_SIN_6MONTH_FS) %>%
            replace(is.na(.),0)

# Checking everything is OK
glimpse(vivi_pca)


# Find correlations for PCA variables
pairs(vivi_pca[,7:9], col = as.factor(vivi_pca[,2]), upper.panel = NULL, pch = 16, cex = 0.5)
pairs(vivi_pca[,3:6], col = as.factor(vivi_pca[,2]), upper.panel = NULL, pch = 16, cex = 0.5)


# Principal Component Analysis
set.seed(110245)

pca_time <- prcomp(vivi_pca[,3:8], scale = T, center = T)
pca_sin <- prcomp(vivi_pca[,3:6], scale = T, center = T)

# Suma de variabilidades de las PCA's
summary(pca_time)
summary(pca_sin)
# Composición de variables de cada PCA
pca_time$rotation[,1:4]
# Valores del data set convertidos a PCA para primeras 2 PCA
vivi_pca2 <- cbind(vivi_pca[,1:2],pca_time$x[,1:2])
names(vivi_pca2) <- c("ID_Certificado","circuito","pca1","pca2")


# Grafico cada Siniestro por PCA y agrupo por Circuito
vivi_pca2 %>%
    ggplot(aes(x=pca1, y=pca2, col=as.factor(circuito))) + 
    geom_jitter(alpha=0.4, height= 0.01, width = 0.01) + guides(color=guide_legend(title="Circuito"))



#############################
# 

# Taking each Circuit separately for cumulative sum
cum_circuito_inicio <- data.frame(cbind(
  seq(0,11,1),
  cumsum(with(vivi03[vivi03$CIRCUITO_LIQUIDACION_ID == 1,],prop.table(table(MONTHS_SINIESTRO_INICIO_POL)))),
  cumsum(with(vivi03[vivi03$CIRCUITO_LIQUIDACION_ID == 2,],prop.table(table(MONTHS_SINIESTRO_INICIO_POL)))),
  cumsum(with(vivi03[vivi03$CIRCUITO_LIQUIDACION_ID == 3,],prop.table(table(MONTHS_SINIESTRO_INICIO_POL))))
))
cum_circuito_inicio[12,4] <- 1
names(cum_circuito_inicio) <- c("Time","Circuito_1","Circuito_2","Circuito_3")

# Plotting by Circuit the Cum% of Siniestros
# Fast Track goes for Siniestros longer from Init of current Poliza
ggplot(cum_circuito_inicio, aes(x=Time, y=Circuito_1)) + 
  geom_line(color="blue") +
  geom_line(aes(y=Circuito_2), color = "red") +
  geom_line(aes(y=Circuito_3), color = "darkgreen")



#######################
# Clustering
library(cluster)
set.seed(43278)

# Mutating the Employee Flag to numeric
vivi03 <- vivi03 %>%
              mutate(MARCA_EMPLEADO = case_when(vivi03$PRODUCTO_EMPLEADOS_FL == "S" ~ 1.0,
                                                                               TRUE ~ -1.0))


# Using PCA 1 and 2 + Monto + Index
vivi_final <- vivi03 %>%
                select(SINIESTRO_ID, CIRCUITO_LIQUIDACION_ID,
                       MARCA_EMPLEADO,
                       ESTIMACION_DAÑO_CLIENTE_DE,INDEX_DECLARA_COBERTURA,
                       QTY_SINIESTRO_3MONTH,QTY_SINIESTRO_3MONTH_FS,
                       QTY_SINIESTRO_6MONTH,QTY_SINIESTRO_6MONTH_FS,
                       MONTHS_SINIESTRO_FIRSTDATE, MONTHS_SINIESTRO_INICIO_POL)
# cbind(vivi03[,c(1,5,6,7,12,15,17,19,21)],pca$x[,1])


# Determine the optimal amount of Clusters
library(factoextra)

fviz_nbclust(scale(vivi_final[,-c(1,2)]), kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) +
  labs(subtitle = "Elbow method")
# k = 7

fviz_nbclust(scale(vivi_final[,-c(1,2)]), kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# k = 2

fviz_nbclust(scale(vivi_final[,-c(1,2)]), kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")
# k = 9, but no convergence


# Now creating the clusters using Kmeans
vivi_kmeans <- kmeans(scale(vivi_final[,-c(1,2)]), 7)
vivi_kmeans$cluster
vivi_kmeans$centers


####################
# DBscan Clustering
set.seed(43278)

dbscan::kNNdistplot(scale(vivi_final[,-c(1,2)]), k=10)
abline(h = 2.5, lty = 2)

vivi_dbscan <- fpc::dbscan(scale(vivi_final[,-c(1,2)]), eps=4)
# eps = 1.5 => 18 clusters
# eps = 1.6 => 18 clusters
# eps = 1 => 33 clusters
# eps = 2 => 4 clusters
# eps = 1.7 => 16 clusters
# eps = 1.75 => 10 clusters
# eps = 1.77 => 10 clusters
# eps = 1.8 => 10 clusters
# eps = 1.85 => 5 clusters
# eps = 1.9 => 8 clusters

table(vivi_dbscan$cluster)


###########################################################
# Defining the new data set for clustering rule extraction
clusters <- data.frame(vivi_kmeans$cluster)
clusters_scan <- data.frame(vivi_dbscan$cluster)
other_data <- data.frame(vivi_final[,-c(1)])
#pca_data <- data.frame(vivi2[,c(3,4,5,6,7,8,9)])

vivi_tree_set <- data.frame(cbind(clusters, other_data))
vivi_scan_set <- data.frame(cbind(clusters_scan, other_data))
names(vivi_tree_set) <- c("cluster","circuito","flag_empleado",
                          "estimacion_daño","indice_cociente",
                          "qty_sin_3m","qty_sin_3m_fs",
                          "qty_sin_6m","qty_sin_6m_fs",
                          "months_first_date","months_inicio_pol")
names(vivi_scan_set) <- c("cluster","circuito","flag_empleado",
                          "estimacion_daño","indice_cociente",
                          "qty_sin_3m","qty_sin_3m_fs",
                          "qty_sin_6m","qty_sin_6m_fs",
                          "months_first_date","months_inicio_pol")


# Factoring the Objective 
vivi_tree_set$cluster <- as.factor(vivi_tree_set$cluster)
vivi_tree_set$circuito <- as.factor(vivi_tree_set$circuito)
vivi_scan_set$cluster <- as.factor(vivi_scan_set$cluster)
vivi_scan_set$circuito <- as.factor(vivi_scan_set$circuito)


#####################################################
# Some Discovery of Clusters (before running a tree)
vivi_tree_set %>%
    filter(cluster == 4, estimacion_daño < 100000) %>%
    ggplot(aes(x=estimacion_daño, y=indice_cociente, color=circuito)) +
    geom_jitter(alpha=0.45, width=0.5, height=0.5)


vivi_scan_set %>%
  filter(cluster != 1, estimacion_daño < 100000, months_first_date < 37) %>%
  ggplot(aes(x=months_first_date, y=months_inicio_pol, color=cluster)) +
  geom_jitter(alpha=0.45, width=0.5, height=0.5)


# Circuit by cluster
prop.table(table(vivi_tree_set$cluster, vivi_tree_set$circuito),1)
prop.table(table(vivi_scan_set$cluster, vivi_scan_set$circuito),1)

discovery_scan <- vivi_scan_set %>%
  group_by(cluster) %>%
  summarise(avg_daño= mean(estimacion_daño),
            avg_index= mean(indice_cociente),
            avg_sin3= mean(qty_sin_3m),
            avg_sin3_fs= mean(qty_sin_3m_fs),
            avg_first_date= mean(months_first_date),
            avg_inicio_pol= mean(months_inicio_pol),
            avg_flag_empleado= mean(flag_empleado),
            median_daño= median(estimacion_daño),
            median_index= median(indice_cociente),
            median_sin3= median(qty_sin_3m),
            median_sin3_fs= median(qty_sin_3m_fs),
            median_first_date= median(months_first_date),
            median_inicio_pol= median(months_inicio_pol),
            median_flag_empleado= median(flag_empleado)
  ) %>%
  data.frame()



# Cluster 7 is mainly composed by FS (7% of all FS claims)
  # PCA1 negativo
  # Daño / Cobertura muy arriba de 1
  # Sumas Aseguradas muy bajas
  # varios claims previos por FS
# Cluster 3 and 4 are mainly composed by Administrativos, but around 10% of FS (check on that)
# Cluster 4:
  # Young Polizas (o Inicio de Renovación o Nuevas)
  # bajo indice daño/cobertura
# Cluster 6 is about big Claims (mostly "young" Polizas)
# Cluster 1 (27% FS)
    # biggest Cluster
    # more previous claims (6 months) than other clusters, many of them by fast track
    # indice daño/cobertura alrededor de 1
# Cluster 5:
    # pólizas cerca de renovación
    # 75% daño/cobertura
    # pocos claims anteriores por FS
# All Circuitos are distributed equally (more or less) between clusters

vivi_tree_set %>%
  filter(indice_cociente < 15) %>%
  ggplot(aes(x=cluster, y=indice_cociente)) +
  geom_boxplot()

vivi_tree_set %>%
  filter(qty_sin_3m != 0) %>%
  ggplot(aes(x=cluster, y=(qty_sin_3m_fs / qty_sin_3m))) +
  geom_boxplot()



##################################
# Extracting Rules of Clusters

# Setting random seed
set.seed(1956451)

# Training and testing (75-25)
inTrain <- createDataPartition(y = vivi_scan_set$cluster, p = .75,  list = FALSE)
train_fs <- vivi_scan_set[inTrain,]
test_fs <- vivi_scan_set[-inTrain,]

# No fit_control needed. We actually want to overfit to find the hidden rules
fit_control <- trainControl(method = "cv", number = 10)

rf_fit <- train(cluster ~ estimacion_daño + indice_cociente +
                          qty_sin_3m + qty_sin_3m_fs + 
                          months_first_date + months_inicio_pol,
                data = train_fs,
                #preProc = c("center", "scale"),
                trControl = fit_control,
                method = "rpart2", maxdepth = 6)

# Predicting with the model and comparing
predict_fs <- predict(rf_fit, newdata = test_fs)

# 93.46% accuracy for FastTrack category (Sensitivity)
confusionMatrix(data = predict_fs, test_fs$cluster)

# Extracting the rules: Daño Declarado, Indice Declarado/Cobertura, Siniestros last 3 months
fancyRpartPlot(rf_fit$finalModel)



############################
# Extracting Rules

# Setting random seed
set.seed(1956451)

# Training and testing (75-25)
inTrain <- createDataPartition(y = vivi1$CIRCUITO_LIQUIDACION_ID, p = .75,  list = FALSE)
train_fs <- vivi1[inTrain,]
test_fs <- vivi1[-inTrain,]

# No fit_control needed. We actually want to overfit to find the hidden rules
#fit_control <- trainControl(method = "cv", number = 10)

rf_fit <- train(CIRCUITO_LIQUIDACION_ID ~ CANTIDAD_REAPERTURAS + 
                  ESTIMACION_DAÑO_CLIENTE_DE +
                  QTY_SINIESTRO_3MONTH +
                  QTY_SINIESTRO_3MONTH_FS +
                  QTY_SINIESTRO_6MONTH +
                  MONTHS_SINIESTRO_DECLARACION +
                  MONTHS_SINIESTRO_INICIO_POL +
                  INDEX_DECLARA_COBERTURA,
                data = vivi1, # trying to overfit...
                method = "rpart2", maxdepth = 6)

# Predicting with the model and comparing
predict_fs <- predict(rf_fit, newdata = vivi1)

# 93.46% accuracy for FastTrack category (Sensitivity)
confusionMatrix(data = predict_fs, vivi1$CIRCUITO_LIQUIDACION_ID)

# Extracting the rules: Daño Declarado, Indice Declarado/Cobertura, Siniestros last 3 months
fancyRpartPlot(rf_fit$finalModel)




############################################
###########################################
# Entendiendo a los Analistas
admin <- vivi03 %>% filter(CIRCUITO_LIQUIDACION_ID == 2, ESTADO_SINIESTRO_TX == "Pagado" | ESTADO_SINIESTRO_TX == "Rechazado") %>% droplevels()

# Agrego las causas de siniestros
causas <- read.csv("CAUSAS_SINIESTROS_VIVIENDA.txt", quote="", row.names = NULL, sep="|", header = T, encoding = "ISO 8859-1")

admin2 <- inner_join(admin, causas, by="SINIESTRO_ID")

rechazos <- admin2 %>% filter(ESTADO_SINIESTRO_TX == "Rechazado")
prop.table(table(admin2$CAUSA_SINIESTRO_TX, 
                 admin2$ESTADO_SINIESTRO_TX),1)

table(rechazos$MOTIVO_ESTADO_SINIESTRO_TX, rechazos$CAUSA_SINIESTRO_TX)
# Balance 75-25 overall
# MIN: Incendio 50-50
# MAX: Pérdida Frío 97.5-2.5


############
# Discovery

# Pérdida de Frío
admin2 %>%
  filter(CAUSA_SINIESTRO_TX == "PERDIDA DE FRIO") %>%
  ggplot(aes(x = QTY_SINIESTRO_6MONTH, y = INDEX_DECLARA_COBERTURA, color=as.factor(ESTADO_SINIESTRO_TX))) +
  geom_jitter(alpha = 0.7, width = 0.5, height = 0.5) +
  theme(legend.position = "none")
# Sólo 2 siniestros (sobre 82) rechazados, por falta de pago. No hay variables de comportamiento para el Rechazo
# Conclusión: pasar Pérdida de Frío a FS, con check sobre pago últimas X cuotas


# Daños y Roturas
admin2 %>%
  filter(CAUSA_SINIESTRO_TX == "DAÑOS / ROTURAS") %>%
  ggplot(aes(x = SUMA_ASEGURADA_DE, y = ESTIMACION_DAÑO_CLIENTE_DE, color=as.factor(ESTADO_SINIESTRO_TX))) +
  geom_jitter(alpha = 0.7, width = 0.5, height = 0.5) +
  theme(legend.position = "none")


# Eventos Climáticos
admin2 %>%
  filter(CAUSA_SINIESTRO_TX == "EVENTOS CLIMATICOS", ESTIMACION_DAÑO_CLIENTE_DE < 750000) %>%
  ggplot(aes(x = SUMA_ASEGURADA_DE, y = QTY_SINIESTRO_3MONTH, color=as.factor(ESTADO_SINIESTRO_TX))) +
  geom_jitter(alpha = 0.5, width = 0.5, height = 0.5) +
  theme(legend.position = "none")



############################
# Extracting Rules DAÑOS

danos <- admin2 %>%
          filter(CAUSA_SINIESTRO_TX == "DAÑOS / ROTURAS") %>%
          droplevels()

# Setting random seed
set.seed(1956451)

# Training and testing (75-25)
inTrain <- createDataPartition(y = danos$ESTADO_SINIESTRO_TX, p = 0.85,  list = FALSE)
train <- danos[inTrain,]
test <- danos[-inTrain,]

# No fit_control needed. We actually want to overfit to find the hidden rules
fit_control <- trainControl(method = "cv", number = 10)

rf_fit <- train(ESTADO_SINIESTRO_TX ~ ESTIMACION_DAÑO_CLIENTE_DE +
                  QTY_SINIESTRO_3MONTH +
                  MONTHS_SINIESTRO_FIRSTDATE +
                  MONTHS_SINIESTRO_INICIO_POL +
                  PRODUCTO_EMPLEADOS_FL +
                  SUMA_ASEGURADA_DE,
                data = train, # trying to overfit...
                method = "rpart2", maxdepth = 3,
                trControl = fit_control)

# Predicting with the model and comparing
predict <- predict(rf_fit, newdata = test)

# 97.14% accuracy for Siniestros Pagados (vs 80% en muestra)
confusionMatrix(data = predict, test$ESTADO_SINIESTRO_TX)

# Extracting the rules: Relación entre Daño Estimado vs Suma Asegurada
fancyRpartPlot(rf_fit$finalModel)
