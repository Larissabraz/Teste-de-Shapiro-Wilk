
## PACOTES
install.packages("ggplot2") 
library(ggplot2)
library(readxl)

## INSERINDO OS DADOS ##

library(readxl)

dados <- read_excel("' Trabalhos/Trabalho Estatística/dados.xlsx", 
                    +     col_types = c("numeric", "text", "text", 
                                        +         "text", "text", "text", "numeric", 
                                        +         "text", "numeric", "numeric", "text", 
                                        +         "text", "text"))

#View(dados)
 
names(dados)

## TESTE DE NORMALIDADE VARIAVEIS QUANTITATIVAS

str(dados$`Peso(kg)`)
# Converter a coluna Peso(kg) para numérico
dados$`Peso(kg)` <- as.numeric(dados$`Peso(kg)`)

# Verificar novamente o tipo da coluna
str(dados$`Peso(kg)`)

# Teste de normalidade para Peso(kg)
shapiro.test(dados$`Peso(kg)`)

# Teste de normalidade para Altura(m)
shapiro.test(dados$`Altura(m)`)

# Teste de normalidade para IMC
shapiro.test(dados$IMC)

# Teste de normalidade para Eventos
shapiro.test(dados$Eventos)

# Teste de normalidade para %eventos
shapiro.test(dados$`%eventos`)




 
 
### Calcular a porcentagem de cada variável ###
 
# Para a coluna 'Grupo'
table(dados$Grupo)
porcentagem_grupo <- prop.table(table(dados$Grupo)) * 100
print(porcentagem_grupo)
 
# Para a coluna 'Gravidade'
table(dados$Gravidade)
porcentagem_gravidade <- prop.table(table(dados$Gravidade)) * 100
print(porcentagem_gravidade)
 
# Para a coluna 'Sexo'
table(dados$Sexo)
porcentagem_sexo <- prop.table(table(dados$Sexo)) * 100
print(porcentagem_sexo)
 
# Para a coluna 'Resultado teste A'
table(dados$ResultadoTesteA)
porcentagem_resultado <- prop.table(table(dados$`ResultadoTesteA`)) * 100
print(porcentagem_resultado)
 
# Para a coluna 'Idade'
table(dados$Idade)
porcentagem_resultado <- prop.table(table(dados$`Idade`)) * 100
print(porcentagem_resultado)
 
# Para a coluna 'Altura'
table(dados$Altura(m))
porcentagem_resultado <- prop.table(table(dados$`Altura(m)`)) * 100
print(porcentagem_resultado)
 
# Para a coluna 'IMC'
table(dados$IMC)
porcentagem_resultado <- prop.table(table(dados$`IMC`)) * 100
print(porcentagem_resultado)
 
# Para a coluna 'Eventos'
porcentagem_resultado <- prop.table(table(dados$`Eventos`)) * 100
print(porcentagem_resultado)
 
# Para a coluna 'Resultado teste A'
porcentagem_resultado <- prop.table(table(dados$`Resultado teste A`)) * 100
 print(porcentagem_resultado)
 
# Para a coluna 'Frequência'
porcentagem_resultado <- prop.table(table(dados$`Frequência`)) * 100
print(porcentagem_resultado)
 
 
### calculando qui -quadrado ###

# Sexo

tabela <- table(dados$Sexo, dados$Grupo);tabela
prop.table(tabela, margin = 1) * 100
chisq.test(table(dados$Sexo, dados$Grupo))

fisher.test(table(dados$Sexo, dados$Grupo))

## faixa etária 

tabela <- table(dados$Idade, dados$Grupo);tabela
prop.table(tabela, margin = 1) * 100
chisq.test(table(dados$Idade, dados$Grupo))
fisher.test(table(dados$Idade, dados$Grupo))

## Gravidade 

tabela <- table(dados$Gravidade, dados$Grupo);tabela
prop.table(tabela, margin = 1) * 100
chisq.test(table(dados$Gravidade, dados$Grupo))
fisher.test(table(dados$Gravidade, dados$Grupo))

## IMC 

tabela <- table(dados$IMC, dados$Grupo);tabela
prop.table(tabela, margin = 1) * 100
chisq.test(table(dados$IMC, dados$Grupo))
fisher.test(table(dados$IMC, dados$Grupo))

## ResultadoTesteA

tabela <- table(dados$ResultadoTesteA, dados$Grupo);tabela
prop.table(tabela, margin = 1) * 100
chisq.test(table(dados$ResultadoTesteA, dados$Grupo))
fisher.test(table(dados$ResultadoTesteA, dados$Grupo))










 

 
 
 
 