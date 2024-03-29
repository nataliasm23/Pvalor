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

# Exerc�cio 10 (Computacional): Considerando os dados do arquivo
# cancer.xlsx, defina dois grupos de pacientes: um de jovens, com idades inferiores
# ou iguais a 54 anos e um de idosos com idades superiores a 54 anos. Os
# grupos dever�o conter 191 e 171 pacientes. Considere a vari�vel nitrog�nio na
# ur�ia (N).

# A) � de interesse verificar se a m�dia populacional da vari�vel N para os
# pacientes idosos � superior a 15. Sendo a vari�ncia desconhecida, qual
# conclus�o pode ser obtida para um n�vel de signific�ncia de 5%?

Dados_idosos <-  dados %>% filter(Idade > 54)

amostra_idosos <- Dados_idosos$N


t.test(amostra_idosos, mu=15, conf.level = 0.95, alternative = 'greater')

#Com o pvalor << alfa,(6.841e-05<<0.05) conclui-se que H0 � rejeitado e aceito H1.
#Ou seja,de acordo coma hipotese alternativa, a verdadeira m�dia populacional
#de idosos � superior que 15.


# B) Considerando agora o grupo de pacientes mais jovens, verifique se a m�dia
# populacional para N � menor do que 15. Obtenha o n�vel descritivo e conclua
# ao n�vel de 5%.
Dados_jovens <-  dados %>% filter(Idade <=54, )
amostra_jovens <- Dados_jovens$N


t.test(amostra_jovens, mu = 15, conf.level = 0.95, alternative = 'less')

#O n�vel descritivo encontrado(pvalor) foi de (pvalor=3.263e-13). 
#Como pvalor<<alfa, conclui-se que H0 � rejeitado e aceita H1. Isto �, a m�dia � 
#menor que 15

# C) Construa intervalos de confian�a para a m�dia populacional da vari�vel N 
#para os dois grupos com 95% de confian�a. Compare os intervalos.

t.test(amostra_idosos, mu = 15, conf.level = 0.95, alternative = 'two.sided')$conf.int
t.test(amostra_jovens, mu = 15, conf.level = 0.95, alternative = 'two.sided')$conf.int

# D) Com base nos resultados do itens B e C, discuta o comportamento das m�dias
#da vari�vel N para os dois grupos de pacientes.

#A m�dia de idosos est� entre 16.1 e 18.3 enquanto a m�dia de jovens (idade<60) est� ente 
#12,3 e 13,4. Portanto a m�dia de idosos � superior a m�dia de jovens na situa��o de pacientes
#com cancer

# Exerc�cio 11 (Computacional): Um criador tem constatado uma propor��o de 10% do rebanho
# com verminose. O veterin�rio alterou a dieta dos animais e acredita que a doen�a diminuiu de
# intensidade. Um exame de 100 cabe�as do rebanho, escolhidas ao acaso, indicou 8 delas com verminose.
# Ao n�vel de 5%, h� ind�cios de que a propor��o diminuiu?
?prop.test
prop.test(x=8, n=100, p = 10/100,
          alternative = 'l',
          conf.level = 0.95, correct = FALSE)
#Pvalor < alfa; Rejeita H0 ,aceita H1, conclus�o: p � menor que 10%
#p-value = 0.2525
#alfa=0.05

prop.test(x=8, n=100, p = 10/100,
          alternative = 'g',
          conf.level = 0.95, correct = FALSE)
#Pvalor > alfa; Aceita H0 ,rejeita H1, conclus�o: p � menor que 10%
#p-value = 0.7475
#alfa=0.05
# Conclusao com alternative=less ou alternative=greater a conclusao � a mesma, isto�,
#a propor��o de animais com verminose diminiu

prop.test(x=8, n=100, p = 10/100,
          conf.level = 0.95, correct = FALSE)

#Para o caso default,i.e, alternative="two.sided"
#Pvalor > alfa; Aceita H0 ,rejeita H1, conclus�o: p n�o � igual que 10%
#p-value = 0.505
#alfa=0.05
