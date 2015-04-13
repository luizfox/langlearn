library(lubridate)
library (rChoiceDialogs)
###########################################################
# VARIAVEIS A SEREM ALTERADAS DE ACORDO COM O AMBIENTE
###########################################################
ffmpeg = "J:/downloads/legendas/ffmpeg-20150402-git-d759844-win64-static/bin/ffmpeg.exe"
modoDev = T
audioOnly = F
###########################################################
diretorioSaida = jchoose.dir(default = getwd(), caption = "Diretorio de saida", modal = canUseJavaModal())
setwd(diretorioSaida)
arquivoLista = jchoose.files(multi = F, caption = "Arquivo lista.txt")
arquivoOrigem = jchoose.files(multi = F, caption = "Arquivo de origem (filme)")
arquivoLegenda = jchoose.files(multi = F, caption = "Arquivo de legenda combinado")

extensao = ifelse(audioOnly, "mp3",
                substr(arquivoOrigem, nchar(arquivoOrigem) -2, nchar(arquivoOrigem)))
legenda = read.fwf (arquivoLegenda,
                    widths= c(12, 12, 12, 3, 2, 4, 4, 4,1,1000), skip=18, fileEncoding="latin1")

head(legenda$V10)
legenda$V11 = substr(legenda$V2, 1,11); legenda$V12 = substr(legenda$V3, 1,11); 
legenda$V2 = lubridate::hms(substr(legenda$V2, 1,11))
legenda$V3 = lubridate::hms(substr(legenda$V3, 1,11))
legenda$V10 = iconv(legenda$V10, from = "UTF-8", to="latin1")
legenda$V10 = substr(legenda$V10, 4,length(legenda$V10))
legenda$V10 = gsub("\n"," ",legenda$V10); legenda$V10 = gsub("\\N"," ",legenda$V10)
legenda$V10 = gsub("\\\\","",legenda$V10)

# Para ajudar a abstrair a mudança:
#======= =========
#===============
localizarIntervalo <- function (tempo, indice = NA) {
  colunas = c(2,3,4,10,11,12)
  if (class(tempo) == "numeric") tempo = lubridate::seconds(tempo)
  retorno = legenda[(legenda$V2 <= tempo) & (legenda$V3 >= tempo), colunas]
  
  if (!is.na(indice))
    retorno = retorno[retorno$V4 == indice,]
  return (retorno)
}

lista = read.table(arquivoLista, header = F) # arquivo que cont?m os pontos marcados pelo VLC
lista = lapply (lista, lubridate::seconds)
lista = lapply(lista[[1]], localizarIntervalo)

inserirParametros <- function (tempoInicial, tempoFinal, arquivoDestino){
  if (audioOnly)
    strffmpeg = "\"%s\" -i \"%s\" -ab 320k -ac 2 -ar 44100 -vn -ss %s -t %f \"%s\""
  else
    strffmpeg = "\"%s\" -i \"%s\" -vcodec copy -acodec copy -ss %s -t %f \"%s\""
  sprintf(strffmpeg,
          ffmpeg, arquivoOrigem, tempoInicial, ifelse(tempoFinal <= 2, 2, tempoFinal), 
          arquivoDestino)
          #substr(arquivoDestino, 1, nchar(arquivoOrigem) -4), extensao)
  #alternativas pra MKV:
  #ffmpeg -i output3.mkv -t 00:04:20 -c:v copy -c:a copy output-cut.mkv
  #ffmpeg -i output3.mkv -t 00:04:20 -c:v libx264 -c:a libfaac output-cut.mkv (sync prob)
}

horaInicialMenor <- function(itemLista){
  if (nrow(itemLista) == 1)
    return (itemLista$V11)
  else
    return(ifelse (itemLista$V2[1] > itemLista$V2[2], itemLista$V11[2], 
                   itemLista$V11[1] ))
}

tempoAAdicionar <- function(itemLista){
  if (nrow(itemLista) == 1) return (as.numeric(hms(itemLista$V12) - 
                                                 hms(itemLista$V11)) + 1.2)
  else{
    maior = ifelse (itemLista$V3[1] > itemLista$V3[2], itemLista$V12[1], itemLista$V12[2])
    return(as.numeric(hms(maior)) -  as.numeric(hms(horaInicialMenor(itemLista))) +1.2)
  }
}

listaAnki = matrix(ncol = 3)
for (i in 1:length(lista)){
  if (nrow(lista[[i]]) == 0) next;
  x = lista[[i]]
  arquivoSaida = sprintf ("saida-%i.%s", i, extensao)
  saida = sprintf ("%s/%s", diretorioSaida, arquivoSaida)
  campoAnkiArquivo = sprintf("[sound:%s]", arquivoSaida)
  if (modoDev)
    print(inserirParametros(horaInicialMenor(x), tempoAAdicionar(x), saida))
  else
    system(inserirParametros(horaInicialMenor(x), tempoAAdicionar(x), saida))
  
  if (nrow(lista[[i]]) == 1) 
    linha = c(as.character(x$V10), as.character(x$V10), campoAnkiArquivo)
  else
    linha = c(as.character(x$V10[x$V4 == "Bot"]), as.character(x$V10[x$V4 == "Top"]), campoAnkiArquivo)
  listaAnki = rbind (listaAnki, linha)
}
write.table(listaAnki[-1,], paste0( diretorioSaida ,"/listaAnki.txt"), sep="\t", quote=F, 
            col.names=F, row.names=F, fileEncoding="UTF-8")
