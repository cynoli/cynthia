#CARREGAR OS PACOTES
pacman::p_load(data.table, ggplot2)

# CRIAÇÃO DA BASE DE DADOS DE MÉDICOS
set.seed(123)  # Definir semente para reprodução

medicos <- data.frame(
  Paciente = paste("Paciente", 1:100),
  Especialidade = sample(c("Cardiologia", "Ortopedia", "Pediatria", "Dermatologia"), 100, replace = TRUE),
  Idade = sample(28:65, 100, replace = TRUE),
  Experiencia = sample(1:30, 100, replace = TRUE),
  Qualificacao = sample(c("Básica", "Intermediária", "Avançada"), 100, replace = TRUE)
)

# TABELA DE CONTINGÊNCIA ENTRE Especialidade E Qualificacao
medicos_table <- table(medicos$Especialidade, medicos$Qualificacao)
medicos_table

# GRÁFICOS DE DISPERSÃO PAREADOS DAS VARIÁVEIS
ggplot(medicos, aes(x = Especialidade, fill = Qualificacao)) + geom_bar(position = "fill")

# TESTE QUI-QUADRADO
medicos_test <- chisq.test(medicos_table)
medicos_test
medicos_test$observed
medicos_test$expected

# CORRPLOT DAS VARIÁVEIS
corrplot(medicos_test$residuals, is.cor = FALSE)

