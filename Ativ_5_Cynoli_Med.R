# Criar a segunda base de médicos
set.seed(456)  # Definir semente para reprodução

medicos2 <- data.frame(
  Paciente = paste("Paciente", 1:100),
  Especialidade = sample(c("Cardiologia", "Ortopedia", "Pediatria", "Dermatologia"), 100, replace = TRUE),
  Idade = sample(28:65, 100, replace = TRUE),
  Experiencia = sample(1:30, 100, replace = TRUE),
  Qualificacao = sample(c("Básica", "Intermediária", "Avançada"), 100, replace = TRUE),
  Consultas = sample(1:10, 100, replace = TRUE)  # Nova variável "Consultas" adcionada
)

# Visualizar a estrutura da base de dados médicos2
str(medicos2)


# CARREGAR OS PACOTES
pacman::p_load(corrplot, dplyr, ggplot2)

# TABELA DE CORRELAÇÃO COM AS VARIÁVEIS IDADE E EXPERIENCIA
correlacao_medicos <- cor(medicos %>% select(Idade, Experiencia))

cat("Correlação entre Idade e Experiencia:", "\n")
print(correlacao_medicos)

# GRÁFICOS DE DISPERSÃO PAREADOS DAS VARIÁVEIS
pairs(medicos %>% select(Idade, Experiencia))

# CORRPLOT DAS VARIÁVEIS
corrplot(correlacao_medicos, method = "number", order = 'alphabet')
corrplot(correlacao_medicos, order = 'alphabet')

