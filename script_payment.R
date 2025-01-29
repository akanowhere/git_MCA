

cat("hello\n")






pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "sjPlot",
             "FactoMineR",
             "amap",
             "readxl",
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias



if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#base <- read.csv("extract.csv", sep = ";", dec = ".")


# Especifique o número desejado de linhas a serem lidas
num_linhas_a_ler <- 1000000  # Substitua pelo número desejado

# Carregamento da base de dados com um número limitado de linhas
base <- read.csv("extract.csv", sep = ";", dec = ".", nrows = num_linhas_a_ler)



# Exemplo com lapply
contagens_por_coluna <- lapply(base, function(x) table(x))







# Tratamento de variávoes quantitivas para qualitativas

#payment-Amout


base <- base %>% 
  mutate(Categ_amount = case_when(payment_amount <= quantile(payment_amount, 0.2, na.rm = TRUE) ~ "muito_baixo",
                                  payment_amount > quantile(payment_amount, 0.2, na.rm = TRUE) & payment_amount <= quantile(payment_amount, 0.4, na.rm = TRUE) ~ "baixo",
                                  payment_amount > quantile(payment_amount, 0.4, na.rm = TRUE) & payment_amount <= quantile(payment_amount, 0.6, na.rm = TRUE) ~ "médio",
                                  payment_amount > quantile(payment_amount, 0.6, na.rm = TRUE) & payment_amount <= quantile(payment_amount, 0.8, na.rm = TRUE) ~ "alto",
                                  payment_amount > quantile(payment_amount, 0.8, na.rm = TRUE) ~ "muito_alto"))


base <- base %>% 
  mutate(Categ_age = case_when(patient_age <= quantile(patient_age, 0.2, na.rm = TRUE) ~ "muito_baixo",
                                  patient_age > quantile(patient_age, 0.2, na.rm = TRUE) & patient_age <= quantile(patient_age, 0.4, na.rm = TRUE) ~ "baixo",
                                  patient_age > quantile(patient_age, 0.4, na.rm = TRUE) & patient_age <= quantile(patient_age, 0.6, na.rm = TRUE) ~ "médio",
                                  patient_age > quantile(patient_age, 0.6, na.rm = TRUE) & patient_age <= quantile(patient_age, 0.8, na.rm = TRUE) ~ "alto",
                                  patient_age > quantile(patient_age, 0.8, na.rm = TRUE) ~ "muito_alto"))



# Vamos remover as variáveis que não utilizaremos (quantitativas)
base <- base %>% 
  select(-payment_id, -payment_created_at, -payment_amount, -patient_age, - patient_firstName,  -payment_instalments, -payment_secondary_card_type, -payment_card_type, -clinic_concate_name, -payment_is_paid_by_card,-clinic_city)
#select(-patient_age, -payment_amount)



  


# A função para a criação da ACM pede que sejam utilizados "fatores"
#base <- as.data.frame(unclass(base), stringsAsFactors=TRUE)





  
  
  
  
# Tabela de contingência com frequências absolutas observadas
tabela_contingencia <- table(base$Categ_amount,base$payment_payment_type_name)
tabela_contingencia

# Definição da quantidade de observações na tabela de contingência
n <- sum(tabela_contingencia)
n

# Estatística qui-quadrado e teste


qui2 <- chisq.test(x = tabela_contingencia)
qui2

# Tabela de contingência com frequências absolutas observadas
qui2$observed

# Tabela de contingência com frequências absolutas esperadas
qui2$expected

# Tabela de contingência com frequências absolutas observadas e esperadas
sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_specialty_name,
         show.exp = TRUE)

# Resíduos – diferenças entre frequências absolutas observadas e esperadas
qui2$observed - qui2$expected

# Valores de qui-quadrado por célula
((qui2$observed - qui2$expected)^2)/qui2$expected

# Resíduos padronizados
qui2$residuals

# Resíduos padronizados ajustados
qui2$stdres
--------

  
  

  
  

# Tabelas de contingência

sjt.xtab(var.row = base$Categ_amount,
         var.col = base$clinic_city,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")



sjt.xtab(var.row = base$Categ_amount,
         var.col = base$clinic_group_name,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


sjt.xtab(var.row = base$Categ_amount,
         var.col = base$clinic_concate_name,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")



sjt.xtab(var.row = base$Categ_amount,
         var.col = base$clinic_classificacao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")




sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_specialty_name,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")




sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_payment_type_name,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")



sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_instalments,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")



sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_is_paid_by_card,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_card_type,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")




sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_secondary_card_type,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")






sjt.xtab(var.row = base$Categ_amount,
         var.col = base$payment_card_brand,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


sjt.xtab(var.row = base$Categ_amount,
         var.col = base$Categ_age,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")



# A função para a criação da ACM pede que sejam utilizados "fatores"
base <- as.data.frame(unclass(base), stringsAsFactors=TRUE)

#Vamos gerar a ACM


# Exemplo com lapply
contagens_por_coluna <- lapply(base, function(x) table(x))
df_contagens <- melt(contagens_por_coluna)

colnames(df_contagens) <- c("Coluna", "Valor", "Contagem")

print(df_contagens)

-------------
  # Gerar a tabela de frequências
frequencia_absoluta  <- table(base)
df_tabela <- as.data.frame(frequencia_absoluta)

# Renomear as colunas
colnames(df_tabela) <- c("Valor", "Freq")

# Ordenar o data frame por Valor
df_tabela <- df_tabela[order(df_tabela$Valor), ]

# Adicionar colunas para Frequência Relativa, Frequência Acumulada e Freq. Relativa Acumulada
df_tabela$Freq_Relativa <- df_tabela$Frequencia / sum(df_tabela$Frequencia)
df_tabela$Freq_Acumulada <- cumsum(df_tabela$Frequencia)
df_tabela$Freq_Relativa_Acumulada <- cumsum(df_tabela$Freq_Relativa)



print(df_tabela)

# Converter a tabela de frequências para um data frame
df_tabela <- as.data.frame(tabela_frequencias)
df_tabela$Valor <- as.numeric(rownames(df_tabela))

# Calcular as colunas adicionais
df_tabela$Freq_Relativa <- df_tabela$Freq / sum(df_tabela$Freq)
df_tabela$Freq_Acumulada <- cumsum(df_tabela$Freq)
df_tabela$Freq_Relativa_Acumulada <- cumsum(df_tabela$Freq_Relativa)

# Exibir a tabela de frequências
print(df_tabela)

# Plotar o gráfico de barras no formato linha x coluna
ggplot(df_tabela, aes(x = as.factor(Valor), y = Freq)) +
  geom_tile(aes(fill = Freq), color = "black") +
  labs(title = "Gráfico de Barras - Tabela de Frequências",
       x = "Valor", y = "Frequência") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


------------------------------------------

ACM <- dudi.acm(base, scannf = FALSE)


# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(base,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Criando um gráfico de barras para a quantidade de categorias por variável
barplot(quant_categorias, 
        main = "Quantidade de Categorias por Variável",
        xlab = "Variável",
        ylab = "Quantidade de Categorias",
        col = "skyblue",
        border = "black",
        names.arg = names(quant_categorias),
        las = 2)  # Rotacionando os rótulos no eixo x




# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Poderíamos fazer o mapa com as coordenadas obtidas por meio da matriz de Burt

# Consolidando as coordenadas-padrão obtidas por meio da matriz de Burt
df_ACM_B <- data.frame(ACM$co, Variável = rep(names(quant_categorias),
                                              quant_categorias))

print(df_ACM_B)

# Plotando o mapa perceptual
df_ACM_B %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# É possível obter as coordenadas das observações
df_coord_obs <- ACM$li

# Plotando o mapa perceptual
df_coord_obs %>%
  ggplot(aes(x = Axis1, y = Axis2, color = base$Categ_amount)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%")),
       color = "Valores") +
  theme_bw()



df_coord_obs %>% 
  ggplot(aes(x = Axis1, y = Axis2, label = base$Categ_amount)) +
  geom_point(shape = 20, color = "red", size = 2) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey48") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey48") +
  geom_text_repel(max.overlaps = 200, size = 3) +
  geom_density2d(color = "gray") +
  geom_label_repel(data = df_ACM, 
                   aes(x = CS1, y = CS2, 
                       label = rownames(df_ACM), 
                       fill = Variável), 
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")
  scale_x_continuous(trans = "log2", breaks = c(0.1, 1, 10, 100)) +
  scale_y_continuous(trans = "log2", breaks = c(0.1, 1, 10, 100))
  
  

# Fim!
  
  

  
  
  
  