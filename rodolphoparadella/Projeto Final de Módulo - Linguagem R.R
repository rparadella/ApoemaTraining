####---------------Rodolpho Paradella - 21/07/2017------------------####
####-------------Projeto Final do Módulo - Linguagem R--------------####


###Dados de Cidades### 

#Numero total de cidades para simulacao
n = 750 

##Campos Numéricos##

#PIB: (R$:mil) 1 bilhao à 500 bilhoes
set.seed(1)
pib.uniforme <- runif(n, 1000000, 500000000)
set.seed(1)
pib <- abs(round(rnorm (pib.uniforme, mean(pib.uniforme), 100000000)))

#Populacao
set.seed(1)
pop.uniforme <- runif(n, 200000, 50000000)
set.seed(1)
pop <- abs(round(rnorm (pop.uniforme, mean(pop.uniforme), 20000000)))

rm(pib.uniforme, pop.uniforme)

#Área (km^2)
set.seed(1)
area <- c(abs(round((rnorm(n/2, 30000, 10000)))), 
          abs(round(rnorm(n/2, 5000, 2000))))

#Temperatura Média
set.seed(1)
temp <- abs(round(rnorm(n, 26, 5)))

#Vagas de emprego
set.seed(1)
nvagas <- c(abs(round((rnorm(n/3, 1000, 250)))), 
            abs(round(rnorm(n/3, 2000, 300))), 
            abs(round(rnorm(n/3, 500, 100))))


##Campos categóricos##

#Origem
origem <- factor (rep(c('Natural', 'Planejada'), n/2))

#Funcao Principal
fprincipal <- factor (rep(c('Industrial', 'Portuária', 'Histórica', 'Tecnológica', 'Turística'), n/5))

#Classificacao
classif <- factor(c(rep('Capital', 40), rep('Municipio', n-40)))


##Dataframe##

#Amostragem: 26 registros
set.seed(1)
cities <- data.frame(Cidade = letters, 
                     PIB = sample(pib, 26), 
                     Pop. = sample(pop, 26), 
                     Area = sample(area, 26), 
                     Temp.Media = sample(temp, 26), 
                     Nvagas = sample(nvagas, 26), 
                     Origem = sample(origem, 26), 
                     Funcao = sample(fprincipal, 26), 
                     Classif. = sample(classif, 26))

#Visualização do maior para o menor PIB
order <- cities[rev(order(cities$PIB)),]

#Gravar arquivo no disco
write.csv(cities, file = "D:\\DF_Cities.csv")
write.csv(order, file = "D:\\DF_Cities (ordem PIB).csv")


##SPLIT/?APPLY##

soma <- apply (cities[,c(2:6)], 2, sum)
round(soma)
media <- apply (cities[,c(2:6)], 2, mean)
round(media)

summary (cities[, c(2:6)])

s <- split(cities, cities$Funcao)

lapply (s, function(x) colMeans(x[,c("Pop.", "Nvagas")])) 
sapply(s, function(x) colMeans(x[,c("Pop.", "Nvagas")])) 


##Grafico/Histograma##

par(mfrow=c(1,2))

plot(order$PIB/1000000~order$Nvagas,
     main="Relação PIB x Número de empregos", 
     xlab = "Vagas de Emprego",
     ylab = "PIB (Bilhões)")

hist(cities[,"Temp.Media"], 
     main="Histograma da Tempereatura", 
     xlab = "Temperatura Média",
     ylab = "Frequência",
     col="red")