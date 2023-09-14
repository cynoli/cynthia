#Base de dados médica
set.seed(123)  # Definir semente para reprodução
medicos <- data.frame(
  Paciente = paste("Paciente", 1:100),
  Especialidade = sample(c("Cardiologia", "Ortopedia", "Pediatria", "Dermatologia"), 100, replace = TRUE),
  Idade = sample(28:65, 100, replace = TRUE),
  Experiencia = sample(1:30, 100, replace = TRUE),
  Qualificacao = sample(c("Básica", "Intermediária", "Avançada"), 100, replace = TRUE)
)

# Calcular a correlação entre Idade e Experiencia
correlacao <- cor(medicos$Idade, medicos$Experiencia)

cat("Correlação entre Idade e Experiencia:", correlacao, "\n")

# Gráfico de Dispersão
library(ggplot2)

ggplot(medicos, aes(x = Idade, y = Experiencia)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão: Idade vs. Experiência",
       x = "Idade",
       y = "Experiência")

