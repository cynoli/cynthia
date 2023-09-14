# Criação da base de pacientes
set.seed(789)  # Definir semente para reprodução

pacientes <- data.frame(
  Paciente = paste("Paciente", 1:100),
  Especialidade_Medico = sample(c("Cardiologia", "Ortopedia", "Pediatria", "Dermatologia"), 100, replace = TRUE),
  Diagnostico = sample(c("Hipertensão", "Fratura", "Dermatite", "Asma"), 100, replace = TRUE),
  Tratamento = sample(c("Medicação", "Cirurgia", "Fisioterapia", "Repouso"), 100, replace = TRUE)
)

# Tabela de contingência entre especialidade de médico e tratamento de pacientes
tabela_pacientes <- table(pacientes$Especialidade_Medico, pacientes$Tratamento)

# Realizar o teste exato
teste_fisher_pacientes <- fisher.test(tabela_pacientes)

# Comparar os resultados
cat("Teste de associação entre especialidade de médico e tratamento dos pacientes:", "\n")
print(teste_fisher_pacientes)

