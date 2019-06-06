#################################################################################
#           
#                     Natalia Sales Mesquita RA: 21341077
#                  Atividade 2 - Exercicios 10 e 11
#
#################################################################################
library(readxl)
library(dplyr)

dados <- read_excel("cancer.xlsx")

View(dados)

# Exercício 10 (Computacional): Considerando os dados do arquivo
# cancer.xlsx, defina dois grupos de pacientes: um de jovens, com idades inferiores
# ou iguais a 54 anos e um de idosos com idades superiores a 54 anos. Os
# grupos deverão conter 191 e 171 pacientes. Considere a variável nitrogênio na
# uréia (N).

# A) É de interesse verificar se a média populacional da variável N para os
# pacientes idosos é superior a 15. Sendo a variância desconhecida, qual
# conclusão pode ser obtida para um nível de significância de 5%?

Dados_idosos <-  dados %>% filter(Idade > 54)

amostra_idosos <- Dados_idosos$N


t.test(amostra_idosos, mu=15, conf.level = 0.95, alternative = 'greater')

#Com o pvalor << alfa,(6.841e-05<<0.05) conclui-se que H0 é rejeitado e aceito H1.
#Ou seja,de acordo coma hipotese alternativa, a verdadeira média populacional
#de idosos é superior que 15.


# B) Considerando agora o grupo de pacientes mais jovens, verifique se a média
# populacional para N é menor do que 15. Obtenha o nível descritivo e conclua
# ao nível de 5%.
Dados_jovens <-  dados %>% filter(Idade <=54, )
amostra_jovens <- Dados_jovens$N


t.test(amostra_jovens, mu = 15, conf.level = 0.95, alternative = 'less')

#O nível descritivo encontrado(pvalor) foi de (pvalor=3.263e-13). 
#Como pvalor<<alfa, conclui-se que H0 é rejeitado e aceita H1. Isto é, a média é 
#menor que 15

# C) Construa intervalos de confiança para a média populacional da variável N 
#para os dois grupos com 95% de confiança. Compare os intervalos.

t.test(amostra_idosos, mu = 15, conf.level = 0.95, alternative = 'two.sided')$conf.int
t.test(amostra_jovens, mu = 15, conf.level = 0.95, alternative = 'two.sided')$conf.int

# D) Com base nos resultados do itens B e C, discuta o comportamento das médias
#da variável N para os dois grupos de pacientes.

#A média de idosos está entre 16.1 e 18.3 enquanto a média de jovens (idade<60) está ente 
#12,3 e 13,4. Portanto a média de idosos é superior a média de jovens na situação de pacientes
#com cancer

# Exercício 11 (Computacional): Um criador tem constatado uma proporção de 10% do rebanho
# com verminose. O veterinário alterou a dieta dos animais e acredita que a doença diminuiu de
# intensidade. Um exame de 100 cabeças do rebanho, escolhidas ao acaso, indicou 8 delas com verminose.
# Ao nível de 5%, há indícios de que a proporção diminuiu?
?prop.test
prop.test(x=8, n=100, p = 10/100,
          alternative = 'l',
          conf.level = 0.95, correct = FALSE)
#Pvalor < alfa; Rejeita H0 ,aceita H1, conclusão: p é menor que 10%
#p-value = 0.2525
#alfa=0.05

prop.test(x=8, n=100, p = 10/100,
          alternative = 'g',
          conf.level = 0.95, correct = FALSE)
#Pvalor > alfa; Aceita H0 ,rejeita H1, conclusão: p é menor que 10%
#p-value = 0.7475
#alfa=0.05
# Conclusao com alternative=less ou alternative=greater a conclusao é a mesma, istoé,
#a proporção de animais com verminose diminiu

prop.test(x=8, n=100, p = 10/100,
          conf.level = 0.95, correct = FALSE)

#Para o caso default,i.e, alternative="two.sided"
#Pvalor > alfa; Aceita H0 ,rejeita H1, conclusão: p não é igual que 10%
#p-value = 0.505
#alfa=0.05
