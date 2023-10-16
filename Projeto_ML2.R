#Trabalho: Cynthia Oliveira, Antonio Henrique e Felipe Castelo // BBB23
#CARREGANDO PACOTES

library(data.table)
pacman:: p_load (corrplot, 
                 dplyr, 
                 ggplot2, 
                 janitor, 
                 ade4, 
                 arules,
                 arulesViz,
                 forcats)

Dados_Marketing <- fread("C:/Users/cynthia.oliveira/Downloads/projeto ap-maq/dados_marketing.csv")
str(Dados_Marketing)


# Definir os cortes para discretização "Compras Web"
cortes <- quantile(Dados_Marketing$`Numero de Compras na Web`, probs = c(0, 1/3, 2/3, 1))




# Discretizar a variável e criar uma nova coluna chamada "Compras_Web_Categoria"
Dados_Marketing <- Dados_Marketing %>%
  mutate(Compras_Web_Categoria = cut(`Numero de Compras na Web`,
                                     breaks = cortes,
                                     labels = c("Baixo", "Médio", "Alto"),
                                     include.lowest = TRUE))

# Definir os cortes para discretização "Compras na Loja"
cortes <- quantile(Dados_Marketing$`Numero de Compras na Loja`, probs = c(0, 1/3, 2/3, 1))

# Discretizar a variável e criar uma nova coluna chamada "Compras_Loja_Categoria"
Dados_Marketing <- Dados_Marketing %>%
  mutate(Compras_Loja_Categoria = cut(`Numero de Compras na Loja`,
                                      breaks = cortes,
                                      labels = c("Baixo", "Médio", "Alto"),
                                      include.lowest = TRUE))
str(Dados_Marketing)

# Selecionar apenas as variáveis numéricas
dados_numericos <- Dados_Marketing %>%
  select_if(is.numeric)

# Resumo estatístico das variáveis numéricas
summary(dados_numericos)

#Análise Exploratória com Gráficos

# Gráfico 1: Distribuição da Escolaridade dos Clientes
ggplot(data = Dados_Marketing, aes(x = Escolaridade, fill = Escolaridade)) +
  geom_bar() +
  labs(title = "Distribuição da Escolaridade dos Clientes",
       x = "Escolaridade",
       y = "Contagem") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Gráfico 2: Distribuição do Estado Civil dos Clientes
ggplot(data = Dados_Marketing, aes(x = `Estado Civil`, fill = `Estado Civil`)) +
  geom_bar() +
  labs(title = "Distribuição do Estado Civil dos Clientes",
       x = "Estado Civil",
       y = "Contagem") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Gráfico 3: Distribuição de Gasto com Alimentos dos Clientes
ggplot(data = Dados_Marketing, aes(x = `Gasto com Alimentos`)) +
  geom_histogram(binwidth = 100, fill = "orange", color = "black") +
  labs(title = "Distribuição de Gasto com Alimentos dos Clientes",
       x = "Gasto com Alimentos",
       y = "Frequência")

# Gráfico 4: Distribuição do Salário Anual dos Clientes
ggplot(data = Dados_Marketing, aes(x = `Salario Anual`)) +
  geom_histogram(binwidth = 10000, fill = "orange", color = "black") +
  labs(title = "Distribuição do Salário Anual dos Clientes",
       x = "Salário Anual",
       y = "Frequência")

# Gráfico 5: Gráfico de Dispersão entre 'Salario Anual' e 'Gasto com Eletronicos'
ggplot(data = Dados_Marketing, aes(x = `Salario Anual`, y = `Gasto com Eletronicos`)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão: Salario Anual vs. Gasto com Eletronicos",
       x = "Salario Anual",
       y = "Gasto com Eletronicos")

# Gráfico 6: Gráfico de Dispersão entre 'Numero de Compras na Web' e 'Numero de Compras na Loja'
ggplot(data = Dados_Marketing, aes(x = `Numero de Compras na Web`, y = `Numero de Compras na Loja`)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão: Numero de Compras na Web vs. Numero de Compras na Loja",
       x = "Numero de Compras na Web",
       y = "Numero de Compras na Loja")

# Gráfico 7: Gráfico de Barras Empilhadas para 'Pais' e 'Estado Civil'
ggplot(data = Dados_Marketing, aes(x = Pais, fill = `Estado Civil`)) +
  geom_bar() +
  labs(title = "Gráfico de Barras Empilhadas: Pais por Estado Civil",
       x = "Pais",
       y = "Contagem") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#Gráfico 8: Distribuição de Dias Desde Última Compra dos Clientes

ggplot(data = Dados_Marketing, aes(x = `Dias Desde Ultima Compra`)) +
  geom_histogram(binwidth = 30, fill = "orange", color = "black") +
  labs(title = "Distribuição de Dias Desde Última Compra dos Clientes",
       x = "Dias Desde Última Compra",
       y = "Frequência")

# Gráfico 9: Gráfico de Correlação para Variáveis Numéricas
matriz_correlacao <- cor(dados_numericos)
corrplot(matriz_correlacao, method = "circle", type = "full", tl.cex = 0.7, tl.col = "black")


#Visão por regra de associação com técnica de aprendizado de máquina:
# Definição dos parâmetros de suporte mínimo e confiança
support_min <- 0.1
confidence_min <- 0.5

# Criação das regras de associação
regras_associacao <- apriori(Dados_Marketing, 
                             parameter = list(support = support_min, confidence = confidence_min))

# Gráfico 10: Criação do gráfico de regras de associação
plot(regras_associacao, method = "graph", control = list(type = "items"))

# Gráfico 11: Criação do gráfico de matriz de dispersão das regras
plot(regras_associacao, method = "matrix", measure = "lift")

