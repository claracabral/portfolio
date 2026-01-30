# Limpar a tela
cat("\014")

# Mostrar o diretório atual
getwd()

# Mudar o diretório para onde está o arquivo
setwd("C:/Users/Helio/Downloads")

# Verificar se o diretório foi alterado corretamente
getwd()

# Instalar e carregar as bibliotecas necessárias
if(!require(survival)) {install.packages("survival")}
if(!require(survminer)) {install.packages("survminer")}
if(!require(survMisc)) {install.packages("survMisc")}

library(survival)
library(survminer)
library(survMisc)

# Leitura dos dados
dt <- read.csv("addicts.csv", header=TRUE, sep=";")

# Parte I - Estimativa de KM dos tempos
ekm <- survfit(Surv(time, status) ~ 1, data = dt)
print(ekm)
summary(ekm)
ggsurvplot(ekm, conf.int = TRUE)

# Parte II - Estimativa de KM dos tempos por "clinica" e "prisão"
# Por clínica
ekmc <- survfit(Surv(time, status) ~ clinic, data = dt)
print(ekmc)
summary(ekmc)
ggsurvplot(ekmc, pval=TRUE, conf.int=TRUE)

# Por prisão
ekmp <- survfit(Surv(time, status) ~ prision, data = dt)
print(ekmp)
summary(ekmp)
ggsurvplot(ekmp, pval=TRUE, conf.int=TRUE)

# Teste de significância das EKM: log-rank: rho=0 e Peto-Peto: rho=1
survdiff(Surv(time, status) ~ clinic, data = dt, rho=0)
survdiff(Surv(time, status) ~ prision, data = dt, rho=0)

## PARTE III - Regressão exponencial
# Regressão clinica + prisão + dose
m01 <- survreg(Surv(time, status) ~ clinic + prision + dose, 
               data= dt, dist="exponential")
m01
summary(m01)
# Regressão clinica + dose + interação
m02 <- survreg(Surv(time, status) ~ clinic + dose + clinic*dose, data= 
                 dt, dist="exponential")
m02
summary(m02)
# Regressão clinica + dose
m02 <- survreg(Surv(time, status) ~ clinic + dose + clinic*dose, data= 
                 dt, dist="exponential")
m02
summary(m02)


## PARTE IV - Regressão de Cox
m03 <- coxph(Surv(time, status) ~ clinic + dose, data= dt) 
m03
summary(m03)
# Qualidade de ajuste: Coeficiente de determinação
rsq(m03, sigD=3)
# Avaliando a proporcionalidade das taxas
temp <- cox.zph(m03) 
temp
par(mfrow=c(1,2))
plot(temp) 
abline(h=0)
