# Para ajudar a abstrair a mudança:
#=======  =========
# ===============
# o maior eh de quem?
# pega o maior e chama pro menor, pra ver se tem algum texto diferente do que jah tem 
# se tiver, concatena e devolve
localizarIntervalo <- function (tempo) {
  colunas = c(2,3,4,10,11,12)
  if (class(tempo) == "numeric") tempo = lubridate::seconds(tempo)
  
  retorno = legenda[(legenda$V2 <= tempo) & (legenda$V3 >= tempo), colunas]
  
  #possivelmente unir a legenda seguinte SE uma ficar muito menor que outra
  menor = ifelse (localizarMaior(retorno) == "Bot", "Top", "Bot")
  maior = ifelse (localizarMaior(retorno) == "Top", "Top", "Bot")
  tempoDoMaior = retorno[retorno$V4 == maior,2]
  linha = localizarIntervaloSimples(tempoDoMaior, menor)
  x = linha$V10
  if (length(x) > 0  )
    if (linha$V10 != retorno[retorno$V4 == menor,4]){
      # no tempo final da maior, no texto da menor, tem um texto diferente do atual da menor
      retorno[retorno$V4 == menor,4] = paste(retorno[retorno$V4 == menor,4], linha$V10)
    }
  return (retorno)
}

localizarIntervaloSimples <- function (tempo, indice){
  colunas = c(2,3,4,10,11,12)
  if (class(tempo) == "numeric") tempo = lubridate::seconds(tempo)
  retorno = legenda[(legenda$V2 <= tempo) & (legenda$V3 >= tempo), colunas]
  retorno = retorno[retorno$V4 == indice,]  
  return (retorno)
}

#top ou bot?
localizarMaior <- function (registro){
  return (ifelse( horaFinalMaior (registro) == registro[registro$V4 == "Bot",6], "Bot", "Top"))
}

horaFinalMaior <- function(itemLista){
  if (nrow(itemLista) == 1)
    return (itemLista$V12)
  else
    return(ifelse (itemLista$V3[1] > itemLista$V3[2], itemLista$V12[1], 
                   itemLista$V12[2] ))
}
# lista[[29]]$V2[2] = 1976.282125