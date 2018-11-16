# Victor Miranda (vmm)

# início da questão (1) ---------------------------------
#Descarregue o arquivo .csv da planilha e imprima o dataframe obtido exatamente do jeito que ele se encontra.
sandlerdf <- read.csv(file="sandler.csv", header=TRUE, sep=",", encoding = "UTF-8")
print(sandlerdf)


# início da questão (2) ---------------------------------
# Encontre a média das notas (sem utilizar a função pronta do R).
My.media <- function(lista) {  
  retorno <- sum(lista) / length(lista)
  
  return(retorno)
}
print(My.media(sandlerdf[["NOTAS"]]))


# início da questão (3) ---------------------------------
# Encontre o desvio padrão das notas (sem utilizar a função pronta do R).
My.dp <- function() {
  variancia = 0
  media = My.media(sandlerdf[["NOTAS"]])
  for(i in sandlerdf[["NOTAS"]]){
    variancia = variancia + ((i - media) ^ 2)
  }
  variancia <- variancia / length(sandlerdf[["NOTAS"]])
  dp = sqrt(variancia)
  return(dp)
}
print(My.dp())
print(sandlerdf[["NOTAS"]])


# início da questão (4) ---------------------------------
# Encontre a moda das notas (sem utilizar a função pronta do R).
My.moda <- function() {
  # salva os valores unicos da lista na variavel ux
  ux <- unique(sandlerdf[["NOTAS"]])
  # conta as ocorrências de cada valor unico na lista
  tab <- tabulate(match(sandlerdf[["NOTAS"]], ux))
  # retorna o valor unico que tem mais ocorrencias
  return(ux[tab == max(tab)])
}
print(My.moda())


# início da questão (5) ---------------------------------
# Faça uma função que retorna apenas os nomes dos filmes que possuem notas maiores ou iguais a seis (6).
# filtra o dataframe pela coluna NOTAS maior ou igual a 6 e retorna o valor da coluna TITULOS
notasmaiorseis = sandlerdf[sandlerdf$NOTAS >= 6,]["TITULOS"]
print(notasmaiorseis)


# início da questão (6) ---------------------------------
# Faça uma função que retorna quantos filmes possuem notas abaixo de seis (6).
# mesmo procedimento da questao anterior, para filmes com nota menor que 6
notasmenorseis = sandlerdf[sandlerdf$NOTAS < 6,]["TITULOS"]
# retorna o numero de linhas do dataframe filtrado
print(nrow(notasmenorseis))


# início da questão (7) ---------------------------------
# Faça uma função que retorna o nome do filme com menor pontuação e o nome do filme com maior pontuação, nessa ordem. 
# Por fim, faça um dataframe com os dois filmes encontrados com as colunas TÍTULO, NOTA, ANO assim como o original.
# funcao que ordena o df pelas notas, ascendente e retorna uma lista com menor e maior usando head e tail
My.extremos <- function() {
  titulos <- sandlerdf[order(sandlerdf$NOTAS),]["TITULOS"]
  menor <- head(titulos, 1)
  maior <- tail(titulos, 1)
  return(c(menor, maior))
}
lista <- My.extremos()
# pega do df original as linhas que o título é igual aos maior ou menor nota
extremos <- sandlerdf[sandlerdf$TITULOS == lista[[1]] | sandlerdf$TITULOS == lista[[2]] ,]
print(extremos)


# início da questão (8) ---------------------------------
# Faça uma função que retorne o ano em que saíram mais filmes com notas acima de seis e meio (6,5).
My.maisfilmes <- function() {
  
}

# início da questão (9) ---------------------------------
# Faça um histograma onde mostra a frequência de filmes com notas maiores ou iguais a seis de cada ano. 
# Não esqueça de dar um título e fazer ele de forma colorida, facilitando a visualização. 







