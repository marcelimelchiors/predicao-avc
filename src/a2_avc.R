#importa as bibliotecas
library(ggplot2)
library(corrplot)
library(pROC)

#le o dataset
stroke <- read.csv('C:/Users/marceli/Documents/modelagem/healthcare-dataset-stroke-data.csv')

#informações principais
summary(stroke) 

#plota todos os gráficos
plot(stroke)

# Verificar se as variáveis são numéricas
variaveis_numericas <- speople_quantityply(stroke, is.numeric)
print(variaveis_numericas)

#transforma a variável work_type em numérica
stroke$work_type <- ifelse(stroke$work_type == "children", 1,
                                ifelse(stroke$work_type == "Govt_job", 2,
                                              ifelse(stroke$work_type == "Never_worked", 3,
                                                     ifelse(stroke$work_type == "Private", 4,
                                                            ifelse(stroke$work_type == "Self_employed", 5,0)))))

#transforma a variável ever_married em numérica
stroke$ever_married <- ifelse(stroke$ever_married == "Yes", 1,
                              ifelse(stroke$ever_married == "No", 0, 0))

#transforma a variável categórica gender em numérica
stroke$gender <- ifelse(stroke$gender == "Male", 1,
                              ifelse(stroke$gender == "Female", 0, 0))

#transforma a variável categórica Residence_type em numérica
stroke$Residence_type <- ifelse(stroke$Residence_type  == "Urban", 1,
                              ifelse(stroke$Residence_type  == "Rural", 0, 0))

#transforma a variável categórica smoking_status em numérica
stroke$smoking_status <- ifelse(stroke$smoking_status == "formerly smoked", 1,
                           ifelse(stroke$smoking_status == "never smoked", 2,
                                  ifelse(stroke$smoking_status == "smokes", 3,
                                         ifelse(stroke$smoking_status == "Unknown", 4,0))))

#verifica se há valores ausentes
is.na(stroke)
sum(is.na(stroke))

#exclui valores ausentes
stroke <- subset(stroke, !grepl("N/A|NA", hypertension))
stroke <- subset(stroke, !grepl("N/A|NA", ever_married))
stroke <- subset(stroke, !grepl("N/A|NA", smoking_status))

#encontra a matriz de correlação
matriz_correlacao <- cor(stroke)

#plota a matriz de correlação
corrplot(cor(matriz_correlacao), method = "circle")

#analise visual dos dados

#gender

#plota um gráfico de barra para stroke e gênero
people_quantity <- table(stroke$stroke, stroke$gender)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "gender", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "ever_married", ylab = "people_quantity")

#age
#plota gráfico de dispersão entre stroke e age
ggplot(stroke, aes(x=stroke,y=age)) + 
  geom_point()


#hypertension
#plota um gráfico de barra para stroke e hypertension
people_quantity <- table(stroke$stroke, stroke$hypertension)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "hypertension", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "hypertension", ylab = "people_quantity")

#heart_disease
#plota um gráfico de barra para stroke e heart_disease
people_quantity <- table(stroke$stroke, stroke$heart_disease)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "heart_disease", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "heart_diasease", ylab = "people_quantity")

#ever_married
#plota um gráfico de barra para stroke e ever_married
people_quantity <- table(stroke$stroke, stroke$ever_married)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "ever_married", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "ever_married", ylab = "people_quantity")

#work_type
#plota um gráfico de barra para stroke e work_type
people_quantity <- table(stroke$stroke, stroke$work_type)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "work_type", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "work_type", ylab = "people_quantity")

#Residence_type
#plota um gráfico de barra para stroke e Residence_type
people_quantity<- table(stroke$stroke, stroke$Residence_type)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "Residence_type", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "Residence_type", ylab = "people_quantity")

#avg_glucose_level
#plota um gráfico de barra para stroke e avg_glucose_level
people_quantity <- table(stroke$stroke, stroke$avg_glucose_level)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "avg_glucose_level", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "avg_glucose_level", ylab = "people_quantity")

#plota gráfico de dispersão entre stroke e avg_glucose_level
ggplot(stroke, aes(x=stroke,y=avg_glucose_level)) + 
  geom_point()

#bmi
#plota um gráfico de barra para stroke e bmi
people_quantity <- table(stroke$stroke, stroke$bmi)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "bmi", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "bmi", ylab = "people_quantity")
ggplot(stroke, aes(x=stroke,y=bmi)) + 
  geom_point()

#smoking_status
#plota um gráfico de barra para stroke e smoking_status
people_quantity <- table(stroke$stroke, stroke$smoking_status)
barplot(people_quantity["1", ], main = "Pessoas que tiveram AVC",
        xlab = "smoking_status", ylab = "people_quantity")
barplot(people_quantity["0", ], main = "Pessoas que não tiveram AVC",
        xlab = "smoking_status", ylab = "people_quantity")

#modelos utilizando glm

#modelo com todas as variaveis
logit1=glm(stroke~gender+age+hypertension+ever_married+work_type+Residence_type+avg_glucose_level+smoking_status, data=stroke, family = binomial(link="logit"))
summary(logit1)
probs1 <- predict(logit1, type = "response")
roc1 <- roc(stroke$stroke, probs1)

#modelo com as variaveis idade, hipertensão, glicose
logit2=glm(stroke~age+hypertension+avg_glucose_level, data=stroke, family = binomial(link="logit"))
summary(logit2)
sum(logit2)
probs2 <- predict(logit2, type = "response")
roc2 <- roc(stroke$stroke, probs2)
#devolve o intervalo de confiança
ICbeta1=confint.default(logit2,level=0.95);ICbeta1

#modelo com as variaveis idade, hipertensão, glicose, status de fumante
logit3=glm(stroke~age+hypertension+avg_glucose_level+smoking_status, data=stroke, family = binomial(link="logit"))
summary(logit3)
probs3 <- predict(logit3, type = "response")
roc3 <- roc(stroke$stroke, probs3)

#modelo com idade, hipertensão, glicose, estado civil
logit4=glm(stroke~age+hypertension+avg_glucose_level+ever_married, data=stroke, family = binomial(link="logit"))
summary(logit4)
probs4 <- predict(logit4, type = "response")
roc4 <- roc(stroke$stroke, probs4)

#modelo com hipertensão, doença cardiaca, estado civil, glicose
logit5=glm(stroke~hypertension+heart_disease+ever_married+avg_glucose_level, data=stroke, family = binomial(link="logit"))
summary(logit5)
probs5 <- predict(logit5, type = "response")
roc5 <- roc(stroke$stroke, probs5)

#modelo com idade, hipertensão, doença cardiaca, estado civil, glicose
logit6=glm(stroke~age+hypertension+heart_disease+ever_married+avg_glucose_level, data=stroke, family = binomial(link="logit"))
summary(logit6)
ICbeta1=confint.default(logit6,level=0.95);ICbeta1
probs6 <- predict(logit6, type = "response")
roc6 <- roc(stroke$stroke, probs6)

#modelo com idade, hipertensão, doença cardiaca, estado civil, glicose, status de fumante 
logit7=glm(stroke~age+hypertension+heart_disease+ever_married+avg_glucose_level+smoking_status, data=stroke, family = binomial(link="logit"))
summary(logit7)
probs7 <- predict(logit7, type = "response")
roc7 <- roc(stroke$stroke, probs7)

#modelo com idade, hipertensão, doença cardiaca, estado civil, glicose e idade*estadocivil
logit8=glm(stroke~age+hypertension+ever_married+heart_disease+avg_glucose_level+age*ever_married, data=stroke, family = binomial(link="logit"))
summary(logit8)
probs8 <- predict(logit8, type = "response")
roc8 <- roc(stroke$stroke, probs8)

#modelo com idade, hipertensao, estado civil, doença cardiaca, glicose, idade*estadocivil, idade*statusdefumante, statussdefumante*estadocivil
logit9=glm(stroke~age+hypertension+ever_married+heart_disease+avg_glucose_level+smoking_status+age*ever_married+age*smoking_status+smoking_status*ever_married, data=stroke, family = binomial(link="logit"))
summary(logit9)
probs9 <- predict(logit9, type = "response")
roc9 <- roc(stroke$stroke, probs9)

#plota a curva ROC
plot(roc1, print.auc = TRUE, auc.polygon = TRUE, grid = FALSE, print.thres = TRUE, col = "red")
lines(roc2, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "green")
lines(roc3, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "yellow")
lines(roc4, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "black")
lines(roc5, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "green")
lines(roc6, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "blue")
lines(roc7, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "purple")
lines(roc8, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "brown")
lines(roc9, print.auc = TRUE, auc.polygon = TRUE, grid = TRUE, print.thres = TRUE, col = "pink")

