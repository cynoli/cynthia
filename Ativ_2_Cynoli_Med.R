#Base de dados médica
set.seed(123)  # Definir semente para reprodução
medicos <- data.frame(
  Paciente = paste("Paciente", 1:100),
  Especialidade = sample(c("Cardiologia", "Ortopedia", "Pediatria", "Dermatologia"), 100, replace = TRUE),
  Idade = sample(28:65, 100, replace = TRUE),
  Experiencia = sample(1:30, 100, replace = TRUE),
  Qualificacao = sample(c("Básica", "Intermediária", "Avançada"), 100, replace = TRUE)
)

# Visualizar a estrutura da base
str(medicos)

# Conversão das colunas relevantes em fatores
cols_para_fatores <- c("Especialidade", "Qualificacao")

medicos[cols_para_fatores] <- lapply(medicos[cols_para_fatores], as.factor)

# Visualizar a estrutura da base de dados após a conversão em fatores
str(medicos)

# One Hot Encoding para a coluna 'Especialidade'
library(dplyr)
library(tidyr)

medicos_encoded <- medicos %>%
  pivot_wider(names_from = Especialidade, values_from = c(Idade, Experiencia), values_fill = 0)

# Visualizar a estrutura da base de dados após One Hot Encoding
str(medicos_encoded)

# Discretização da coluna 'Idade'
medicos$Idade.Disc <- cut(medicos$Idade, breaks = c(28, 40, 50, 65), labels = c("Jovem", 'Meia Idade', 'Idoso'))

# Visualizar a estrutura da base de dados após discretização
str(medicos$Idade.Disc)

# Transformação dos fatores em 3 tipos: mais frequente, segundo mais frequente e outros
library(forcats)

medicos$Qualificacao <- fct_lump(medicos$Qualificacao, n = 2)

# Visualizar a estrutura da base de dados após a transformação dos fatores
str(medicos)

# Plotar um gráfico de dispersão entre idade e experiência
library(ggplot2)

ggplot(medicos, aes(x = Idade, y = Experiencia)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão: Idade vs. Experiência dos Médicos",
       x = "Idade",
       y = "Experiência")

# Média da variável Idade dos médicos
mean(medicos$Idade)

# Mediana da variável Idade dos médicos
median(medicos$Idade)

# Separatrizes da variável Experiencia dos médicos
quantile(medicos$Experiencia, probs = 0.75)
quantile(medicos$Experiencia, probs = 0.10)
quantile(medicos$Experiencia, probs = 0.95)
boxplot(medicos$Experiencia)  # Boxplot - gráfico que resume as separatrizes

# Desvio-padrão da variável Experiencia dos médicos
sd(medicos$Experiencia)
plot(medicos$Experiencia)

# Sumário descritivo básico das variáveis
summary(medicos)

# Tabela de frequência absoluta da variável Especialidade dos médicos
table(medicos$Especialidade)

# Tabela de frequência relativa da variável Especialidade dos médicos
prop.table(table(medicos$Especialidade))

# Histograma da variável Idade dos médicos - gráfico que permite conhecer a curva dos dados
hist(medicos$Idade)

