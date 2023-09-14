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

# Gráfico de Caixas Univariado - Idade dos Médicos
medicos %>% ggplot(aes(y = Idade)) + geom_boxplot()

# Gráfico de Caixas Multivariado - Experiência dos Médicos por Especialidade
medicos %>% ggplot(aes(y = Experiencia, x = Especialidade)) + geom_boxplot()

# Histograma - Idade dos Médicos
medicos %>% ggplot(aes(x = Idade)) + geom_histogram()

# Densidade - Idade dos Médicos
medicos %>% ggplot(aes(x = Idade)) + geom_density()

# Gráfico de Barras com a variável "Qualificacao"
medicos %>% ggplot(aes(x = Qualificacao, fill = Especialidade)) + geom_bar(position = "dodge")

