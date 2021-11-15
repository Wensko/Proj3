library(readr)
library(dplyr)
library(kohonen)
library(plm)
library(stargazer)
library(factoextra)
library(prediction)
base.completa <- read_csv("inv_in_crime (1).csv")
View(inv_in_crime_1_)
unique(base.completa$Ano)
##Temos dados de 8 anos. Dessa forma, 2 anos vão ser anos de teste, e 6 vão ser treino
names(base.completa) <- c('cidade', 'code','ano','gini','densidade','emp','gdp','educ','inv.sec',
                                'urb','pop', 'doloso','furto','roubo','veiculo','crime',
                                'txseg','txcrime')

## Passando log em investimento, p ajduar a dimensionar melhor a base
base.completa$inv.sec <- log(base.completa$inv.sec)

#função p calcular moda
get.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

{
# base.treino <- base.completa[base.completa$ano<2016,]
# base.teste <- base.completa[base.completa$ano>2015,]

# base.treino = pdata.frame(base.treino, index = c('cidade', 'ano'))
# base.teste = pdata.frame(base.teste, index = c('cidade', 'ano'))
# 
# ##Regressões p cada um dos tipos de crime
# reg.simples <- list()
# for (i in c('doloso','furto','roubo','veiculo')) {
#   reg.simples[[i]] <- plm(formula = paste(i, '~gini+densidade+emp+gdp+educ+inv.sec+lag(inv.sec)+urb',''),
#             data = base.treino, model = "pooling")
# }
# summary(reg.simples[['furto']])
# 
# prev.simples <- list()
# 
# for (i in c('doloso','furto','roubo','veiculo')) {
#   paste('reg.',) <- predict(reg.simples[[i]], newdata = base.teste)
# }
#   prev.simples[['doloso']] <- predict(reg.simples[['doloso']], newdata = base.teste)
# 

}## Isso é a parte de previsão, que deu super errado.


## Aqui eu pego os dados em painel e transformo em 8 cross-sections - tudo dentro de uma lista
lista.anos <- list()
for (i in paste(unique(base.completa$ano))) {
  lista.anos[[i]] <- as.data.frame(base.completa[base.completa$ano==i,])
}

#Sempre normaliza as unidades antes de fazer o mapa, por questões de escala
normalized.anos <- list()
for (i in names(lista.anos)) {
  normalized.anos[[i]] <- lista.anos[[i]]
  normalized.anos[[i]][,-c(1:3)] <- scale(normalized.anos[[i]][,-c(1:3)])
}

#tamanho dos mapas
grid.dim <- 4
grid <- somgrid(xdim=grid.dim, ydim=grid.dim, topo = 'hexagonal', toroidal = F)

soms.crime <- list()
soms.all <- list()
soms.ses <- list()
for (i in names(normalized.anos)) {
  set.seed(101)
  soms.crime[[i]] <- som(as.matrix(normalized.anos[[i]][12:18]), grid = grid, keep.data =T)
  set.seed(101)
  soms.all[[i]] <- som(as.matrix(normalized.anos[[i]][4:18]), grid = grid, keep.data =T)
  set.seed(101)
  soms.ses[[i]] <- som(as.matrix(normalized.anos[[i]][4:12]), grid = grid, keep.data =T)
  #Essas linhas guardam a localização da cidade no mapa de cada coisa (crime, socioeconomico, e todos)
  normalized.anos[[i]]$class.crime <- soms.crime[[i]]$unit.classif
  normalized.anos[[i]]$class.ses <- soms.ses[[i]]$unit.classif
  normalized.anos[[i]]$class.all <- soms.all[[i]]$unit.classif
  
  }

## Encontrando o que seria a "localização equivalente" para cada localização no mapa do crime
## Pra isso, uso a moda da localização no mapa de socio-economico dada a localização no mapa do crime
## e vice-versa

for (j in names(normalized.anos)){
  for (i in 1:max(normalized.anos[[j]]$class.crime)) {
  normalized.anos[[j]]$mode.sesclass.given.crime[normalized.anos[[j]]$class.crime==i] <- get.mode(normalized.anos[[j]]$class.ses[normalized.anos[[j]]$class.crime==i])
  normalized.anos[[j]]$mode.crimeclass.given.ses[normalized.anos[[j]]$class.ses==i] <- get.mode(normalized.anos[[j]]$class.crime[normalized.anos[[j]]$class.ses==i])
  
  ##Distância entre a localização equivalente e a localização real da cidade no mapa 
  normalized.anos[[j]]$distses <- unit.distances(grid)[normalized.anos[[j]]$mode.sesclass.given.crime,  normalized.anos[[j]]$class.ses]
  normalized.anos[[j]]$distcrime <- unit.distances(grid)[normalized.anos[[j]]$mode.crimeclass.given.ses,  normalized.anos[[j]]$class.crime]
  
  }
}

distancias.medias <- list()
for (i in names(normalized.anos)) {
  ## Essa é a distancia 
  distancias.medias[[i]]$ses <- mean(normalized.anos[[i]]$distses)
  distancias.medias[[i]]$crime <- mean(normalized.anos[[i]]$distcrime)
}



# Para todos os anos, parece que na média, há menos de 2 de distância entre a "localização"
# equivalente e a localização real em cada um dos mapas. Além disso, parece que na média
# os indicadores socioeconômicos e os indicadores de crime geram mapas diferentes, e, 
# mesmo que não estejam na mesma vizinhança equivalente, a cidade se situa próxima
# a ela.

mean(unit.distances(grid))


heatmap.som <- function(model){
  for (i in 1:length(colnames(getCodes(model)))) {
    plot(model, type = "property", property = getCodes(model)[,i], 
         main = colnames(getCodes(model))[i]) 
  }
}

  





