library(dplyr)
library(GGally)
library(reshape2)
library(car)
library(HSAUR)
library(heplots)
library(lattice)

install.packages("reshape2")

#O estudo examina os fatores que influenciam a decisão de permanência ou saída de 
#funcionários de uma empresa hipotética, utilizando Análise de Variância Multivariada 
#(MANOVA) para avaliar o impacto de múltiplas variáveis, como idade, salário e satisfação no 
#trabalho, sobre a rotatividade dos funcionários. Através da comparação entre grupos de funcionários
#que ficaram e que saíram, busca-se identificar diferenças significativas e informar estratégias de
#retenção de talentos.

#Carregando os dados
dados <- read.csv("C:/Users/ccabr/OneDrive/Área de Trabalho/Data science/Pos Estatistica CD/Análise Multivariada/WA_Fn-UseC_-HR-Employee-Attrition.csv")


# Verificando valores ausentes
sum(is.na(dados))

#Não existem valores vazios na base

# Verificando tipos de dados e possíveis problemas
str(dados)

# Estatísticas resumidas para variáveis numéricas
summary(dados)

#Demografia e Informações Básicas
#Idade: Os funcionários têm uma faixa etária de 18 a 60 anos, com a média de idade
#em torno de 36,92 anos. A distribuição sugere uma força de trabalho relativamente jovem e madura.
#Distância de Casa ao Trabalho: A distância varia significativamente, de 1 a 29 km, com uma média
#de 9,19 km, indicando que a maioria dos funcionários não mora muito distante do local de trabalho.
#Educação: A educação dos funcionários varia de 1 a 5, com uma média de aproximadamente 2,91,
#sugerindo um nível educacional diversificado dentro da empresa.

#Características do Trabalho
#Renda Mensal: A renda mensal varia bastante, de $1.009 a $19.999, com uma média de $6.503,
#refletindo uma possível diversidade de cargos e responsabilidades.
#Tempo na Empresa: O tempo de permanência na empresa varia de 0 a 40 anos, com uma média de
#7 anos. Isso pode indicar tanto lealdade quanto a presença de funcionários bastante novos.
#Satisfação no Trabalho: A satisfação no trabalho varia de 1 a 4, com uma média de 2,73,
#indicando um nível moderado de satisfação entre os funcionários.

#Relacionamentos e Desempenho
#Satisfação com o Relacionamento: Semelhante à satisfação no trabalho, a satisfação com os
#relacionamentos no trabalho também mostra uma média de 2,71, indicando satisfação moderada.
#Avaliação de Desempenho: A maioria dos funcionários tem uma avaliação de desempenho de 3 ou 4,
#sugerindo um desempenho geralmente bom entre os funcionários.

#Horas de Trabalho e Extras
#Horas de Trabalho: As horas de trabalho padrão são 80 horas, o que é consistente para todos
#os funcionários, provavelmente refletindo um período de pagamento bi-semanal típico.

#Horas Extras: As informações sobre horas extras não foram detalhadas no resumo, mas seria
#uma variável crucial para entender a carga de trabalho e seu impacto na rotatividade.

# Converter a variável 'Attrition' de fator para numérico
dados$Attrition <- as.numeric(ifelse(dados$Attrition == "Yes", 1, 0))

# Selecionar apenas variáveis numéricas para a matriz de correlação
dados_numericos <- dados %>% select_if(is.numeric)

# Calcular a matriz de correlação
matriz_correlacao <- cor(dados_numericos, use = "pairwise.complete.obs")

#uma ou mais das suas variáveis numéricas não variam (ou seja, têm desvio padrão
#igual a zero). Isso significa que todas as observações têm o mesmo valor, o que
#torna impossível calcular a correlação para essas variáveis.

# Identificando variáveis com desvio padrão zero
vars_constantes <- sapply(dados_numericos, function(x) sd(x, na.rm = TRUE) == 0)

# Imprimindo as variáveis constantes para verificação
print(names(dados_numericos)[vars_constantes])

# Removendo variáveis constantes dos dados
dados_numericos <- dados_numericos[, !vars_constantes]

# Verificando se ainda existem variáveis constantes
print(sum(sapply(dados_numericos, function(x) sd(x, na.rm = TRUE) == 0)))


# Recalculando a matriz de correlação
matriz_correlacao <- cor(dados_numericos, use = "pairwise.complete.obs")

# Removendo variáveis constantes, recalculando a matriz de correlação
dados_sem_constantes <- dados_numericos[, !vars_constantes]

# Extraindo correlações de Attrition e organizando em ordem decrescente de magnitude
correlacoes_attrition <- matriz_correlacao["Attrition", ]
correlacoes_attrition <- correlacoes_attrition[order(-abs(correlacoes_attrition))]

# Criando um data frame para melhor visualização
tabela_correlacoes <- data.frame(
  Variavel = names(correlacoes_attrition),
  Correlacao = correlacoes_attrition
)

# Imprimindo a tabela
print(tabela_correlacoes)


#Age (Idade): A idade teve uma correlação negativa com a rotatividade, 
#sugerindo que funcionários mais jovens são mais propensos a deixar a empresa, 
#o que pode refletir diferenças em aspirações de carreira ou estabilidade.

#TotalWorkingYears (Total de Anos de Trabalho): Essa variável também mostrou
#uma correlação negativa com a rotatividade. Isso implica que quanto mais um funcionário
#trabalha, mais provável é que ele permaneça na empresa, possivelmente devido a um maior
#comprometimento ou satisfação acumulada.

#MonthlyIncome (Renda Mensal): A renda é um fator significativo que pode influenciar
#a decisão de um funcionário de permanecer na empresa, especialmente se ele sentir que suas
#necessidades financeiras estão sendo atendidas.

#YearsAtCompany (Anos na Empresa): Semelhante ao TotalWorkingYears, o tempo que um
#funcionário passa em uma empresa está negativamente correlacionado com a rotatividade.
#Isso sugere que os laços que se formam ao longo dos anos podem desencorajar a saída.

#Por que focar nessas variáveis?
#Relevância Empírica: Elas demonstraram ter uma influência substancial sobre a rotatividade,
#conforme análises anteriores.
#Interpretabilidade: São variáveis fáceis de entender e comuns em estudos de RH para análises
#de retenção de funcionários.
#Disponibilidade de Dados: Estas variáveis são frequentemente disponíveis em conjuntos de dados
#de RH, tornando-as práticas para análise.

# Selecione as variáveis para visualização
vars_vis <- c("Age", "TotalWorkingYears", "MonthlyIncome", "YearsAtCompany")


age<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Age",
    Media = mean(Age, na.rm = TRUE),
    Mediana = median(Age, na.rm = TRUE),
    Desvio_Padrao = sd(Age, na.rm = TRUE),
    Minimo = min(Age, na.rm = TRUE),
    Maximo = max(Age, na.rm = TRUE)
  )

monthincome<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Monthly income",
    Media = mean(MonthlyIncome, na.rm = TRUE),
    Mediana = median(MonthlyIncome, na.rm = TRUE),
    Desvio_Padrao = sd(MonthlyIncome, na.rm = TRUE),
    Minimo = min(MonthlyIncome, na.rm = TRUE),
    Maximo = max(MonthlyIncome, na.rm = TRUE)
  )


totalyears<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Total working years",
    Media = mean(TotalWorkingYears, na.rm = TRUE),
    Mediana = median(TotalWorkingYears, na.rm = TRUE),
    Desvio_Padrao = sd(TotalWorkingYears, na.rm = TRUE),
    Minimo = min(TotalWorkingYears, na.rm = TRUE),
    Maximo = max(TotalWorkingYears, na.rm = TRUE)
  )

yearscompany<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Years at Company",
    Media = mean(YearsAtCompany, na.rm = TRUE),
    Mediana = median(YearsAtCompany, na.rm = TRUE),
    Desvio_Padrao = sd(YearsAtCompany, na.rm = TRUE),
    Minimo = min(YearsAtCompany, na.rm = TRUE),
    Maximo = max(YearsAtCompany, na.rm = TRUE)
  )

tabela_final_heart <- bind_rows(age, totalyears, monthincome, yearscompany)
tabela_final_heart%>%
  kable("html") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE)

# Boxplot
par(mfrow = c(1, 4))
for (var in vars_vis) {
  boxplot(dados[[var]], main = var)
}

# Histograma
par(mfrow = c(1, 4))
for (var in vars_vis) {
  hist(dados[[var]], prob = TRUE, main = var)
}


# Instalar o pacote MVN, se ainda não estiver instalado
if (!require("MVN")) install.packages("MVN", dependencies = TRUE)

# Carregar o pacote
library(MVN)


# Teste de normalidade multivariada usando o pacote MVN
mvn_result <- mvn(data = dados[vars_vis], mvnTest = "mardia")
print(mvn_result)

# Modelo de regressão linear para MANOVA
dados$Attrition <- as.factor(dados$Attrition)  # Garantindo que Attrition seja fator
mod_manova <- lm(cbind(Age, TotalWorkingYears, MonthlyIncome, YearsAtCompany) ~ Attrition, data=dados)
summary(mod_manova)

# Executando MANOVA
manova_res <- manova(mod_manova)
summary(manova_res, test="Wilks")  # Teste de Wilks
summary(manova_res, test="Pillai") # Teste de Pillai

library(scatterplot3d)

# Gráfico de Dispersão Multivariada
scatterplot3d(dados$Age, dados$TotalWorkingYears, dados$MonthlyIncome, color = as.numeric(dados$Attrition), pch = 16,
              main = "Gráfico de Dispersão Multivariada", 
              xlab = "Age", ylab = "TotalWorkingYears", zlab = "MonthlyIncome")


dados_long <- melt(dados, id.vars="Attrition")
bwplot(value ~ Attrition | variable, data=dados_long, scales="free",
       ylab="Valor da Variável", xlab="Attrition",
       strip=strip.custom(factor.levels=paste(vars_vis, " (", levels(dados_long$variable), ")", sep="")),
       panel=function(x, y, ...) {
         panel.bwplot(x, y, ...)
         panel.linejoin(x, y, col="red", ...)
       })

age<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Idade",
    Media = mean(Age, na.rm = TRUE),
    Mediana = median(Age, na.rm = TRUE),
    Desvio_Padrao = sd(Age, na.rm = TRUE),
    Minimo = min(Age, na.rm = TRUE),
    Maximo = max(Age, na.rm = TRUE)
  )

monthincome<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Renda mensal",
    Media = mean(MonthlyIncome, na.rm = TRUE),
    Mediana = median(MonthlyIncome, na.rm = TRUE),
    Desvio_Padrao = sd(MonthlyIncome, na.rm = TRUE),
    Minimo = min(MonthlyIncome, na.rm = TRUE),
    Maximo = max(MonthlyIncome, na.rm = TRUE)
  )


totalyears<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Total de anos de trabalho",
    Media = mean(TotalWorkingYears, na.rm = TRUE),
    Mediana = median(TotalWorkingYears, na.rm = TRUE),
    Desvio_Padrao = sd(TotalWorkingYears, na.rm = TRUE),
    Minimo = min(TotalWorkingYears, na.rm = TRUE),
    Maximo = max(TotalWorkingYears, na.rm = TRUE)
  )

yearscompany<-dados%>%
  group_by(Attrition) %>%
  summarise(
    Variavel = "Anos na empresa",
    Media = mean(YearsAtCompany, na.rm = TRUE),
    Mediana = median(YearsAtCompany, na.rm = TRUE),
    Desvio_Padrao = sd(YearsAtCompany, na.rm = TRUE),
    Minimo = min(YearsAtCompany, na.rm = TRUE),
    Maximo = max(YearsAtCompany, na.rm = TRUE)
  )

tabela_final_heart <- bind_rows(age, totalyears, monthincome, yearscompany)
tabela_final_heart%>%
  kable("html") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE)

caminho <- file.choose("C:/Users/ccabr/OneDrive/Área de Trabalho/Data science/Pos Estatistica CD/Análise Multivariada")
save_kable(tabela_final_heart, "tabela descritiva.html", file=caminho)



