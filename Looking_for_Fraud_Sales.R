# Cargo librerías
library(tidyverse)
library(ggplot2)
library(readr)


# Importo datos
sales <- read.delim("Analizo_Vendedores_201808_201907.txt", sep="\t")

glimpse(sales)


####################
# Transformo datos
sales$USUARIO_ALTA_ID <- as.factor(sales$USUARIO_ALTA_ID)

sales <- sales %>% 
          mutate(ventas_perc_atm = QTY_VENTAS_ATM / QTY_VENTAS,
                 ventas_perc_ap = QTY_VENTAS_AP / QTY_VENTAS,
                 clientes_perc_no_mail = CLIENTE_CON_BAD_MAIL / QTY_CLIENTES_VENTAS,
                 venta_diaria = QTY_VENTAS / QTY_DIAS_VENTA) %>% 
          replace(is.na(.), 0)

sales <- sales %>% 
          mutate(ventas_perc_ap_atm = ventas_perc_atm + ventas_perc_ap)

#####################
# Discovery

# Hist de % ventas ATM
sales %>% 
  ggplot(aes(x=ventas_perc_atm)) +
  geom_histogram(bins=50)

# Hist de % ventas AP
sales %>% 
  ggplot(aes(x=ventas_perc_ap)) +
  geom_histogram(bins=50)

# Hist de % ventas AP + ATM
sales %>% 
  ggplot(aes(x=ventas_perc_ap_atm)) +
  geom_histogram(bins=50)

# Hist de % Clientes sin mail
sales %>%
  #filter(ventas_perc_ap_atm <= 0.9) %>% 
  #filter(ventas_perc_ap > 0.8) %>% 
  ggplot(aes(x=clientes_perc_no_mail)) +
  geom_histogram(bins=50)

# Hist de ventas diarias promedio
sales %>% 
  ggplot(aes(x=venta_diaria)) +
  geom_histogram(bins=50)

# Hist de MAX ventas a 1 cliente
sales %>% 
  ggplot(aes(x=MAX_VENTAS_X_CLIENTE)) +
  geom_histogram(bins=50)

# Hist de días con ventas
sales %>% 
  ggplot(aes(x=QTY_DIAS_VENTA)) +
  geom_histogram(bins=50)

# Hist de días con ventas
sales %>% 
  ggplot(aes(x=QTY_CLIENTES_VENTAS)) +
  geom_histogram(bins=50)


# Clientes sin mail vs % ATM
sales %>% 
  ggplot(aes(x=clientes_perc_no_mail, y=ventas_perc_atm, color=QTY_VENTAS)) +
  geom_point()


# Clientes sin mail vs % AP
sales %>% 
  ggplot(aes(x=clientes_perc_no_mail, y=ventas_perc_ap, color=QTY_VENTAS)) +
  geom_point()


# Clientes sin mail vs % AP + ATM
sales %>%
  filter(clientes_perc_no_mail > 0) %>% 
  ggplot(aes(x=clientes_perc_no_mail, y=ventas_perc_ap_atm, color=venta_diaria)) +
  geom_point()


###############
# Sospecho!! #
#############
sales %>% filter(ventas_perc_ap_atm > 0.8, clientes_perc_no_mail > 0.65)



###############################
# Clustering

# Data set for clustering
sales_clust <- sales %>% 
                select(USUARIO_ALTA_ID, QTY_DIAS_VENTA, QTY_CLIENTES_VENTAS,
                       venta_diaria,
                       ventas_perc_atm, ventas_perc_ap, clientes_perc_no_mail)

# Determine the optimal amount of Clusters
library(factoextra)

fviz_nbclust(scale(sales_clust[,-c(1)]), kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(subtitle = "Elbow method")
# k = 2

fviz_nbclust(scale(sales_clust[,-c(1)]), kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# k = 2

fviz_nbclust(scale(sales_clust[,-c(1)]), kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")
# no convergence


# Now creating the clusters using Kmeans
sales_kmeans <- kmeans(scale(sales_clust[,-c(1)]), 2)
sales_kmeans$cluster
sales_kmeans$centers

# Mando el cluster a la base principal para análisis
sales$cluster_kmeans <- sales_kmeans$cluster




####################
# DBscan Clustering
set.seed(43278)

dbscan::kNNdistplot(scale(sales_clust[,-c(1)]), k=15)
abline(h = 1.75, lty = 2)

sales_dbscan <- fpc::dbscan(scale(sales_clust[,-c(1)]), eps=1.8)
# eps = 1.65 => 2 clusters
# eps = 1.5 => 2 clusters
# eps = 1.4 => 2 clusters
# eps = 2 => 2 clusters
# eps = 1.3 => 2 clusters

table(sales_dbscan$cluster)

# Mando el cluster a la base principal para análisis
sales$cluster_dbscan <- sales_dbscan$cluster



########################
# Discovery on Clusters
sales %>% 
  #group_by(cluster_kmeans) %>% 
  group_by(cluster_dbscan) %>% 
  summarise(mean_qty_dias = mean(QTY_DIAS_VENTA), 
            mean_clientes = mean(QTY_CLIENTES_VENTAS),
            mean_max_venta = mean(MAX_VENTAS_X_CLIENTE), 
            mean_ventas = mean(QTY_VENTAS), 
            mean_venta_diaria = mean(venta_diaria),
            mean_atm = mean(ventas_perc_atm),
            mean_ap = mean(ventas_perc_ap), 
            mean_no_mail = mean(clientes_perc_no_mail),
            n = n()) %>% 
  data.frame() %>% 
  write.table("vendedores_v4.txt")

# Check
sales %>%
  filter(cluster_dbscan == 0) %>% 
  ggplot(aes(x=clientes_perc_no_mail, y=ventas_perc_atm, size=venta_diaria, color=as.factor(cluster_dbscan))) +
  geom_point(alpha=0.35)


# Exporto los vendedores que quedaron en el cluster 0
sales %>% filter(cluster_dbscan == 0) %>% write.table("vendedores_v4.txt", sep="\t")
