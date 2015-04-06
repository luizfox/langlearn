library(lubridate)
###########################################################
# VARIAVEIS A SEREM ALTERADAS DE ACORDO COM O AMBIENTE
###########################################################
ffmpeg = "J:/downloads/legendas/ffmpeg-20150402-git-d759844-win64-static/bin/ffmpeg.exe"
arquivoOrigem = "e:/S02E07 A Man Without Honor.mp4"
diretorioSaida = "e:/"
pastaLegendaELista = "C:/Users/hu/Dropbox/diversos/legendas/GoT/"
pastaLegendaELista = "J:/dropbox/Dropbox/diversos/legendas/GoT"
arquivoLegenda = "Game.of.Thrones.S02E07.720p.BluRay.x264.MIKY.Everything.ass"
###########################################################

setwd(pastaLegendaELista)
legenda = read.fwf (arquivoLegenda
                    widths= c(12, 12, 12, 3, 2, 4, 4, 4,1,1000), skip=18)

#head(legenda)
legenda$V11 = substr(legenda$V2, 1,11); legenda$V12 = substr(legenda$V3, 1,11); 
legenda$V2 = lubridate::hms(substr(legenda$V2, 1,11))
legenda$V3 = lubridate::hms(substr(legenda$V3, 1,11))
legenda$V10 = substr(legenda$V10, 4,length(legenda$V10))
legenda$V10 = gsub("\n"," ",legenda$V10); legenda$V10 = gsub("\\N"," ",legenda$V10)
legenda$V10 = gsub("\\\\","",legenda$V10)


localizarIntervalo <- function (tempo) {
  if (class(tempo) == "numeric") tempo = lubridate::seconds(tempo)
  legenda[(legenda$V2 <= tempo) & (legenda$V3 >= tempo),c(2,3,4,10,11,12)]
}

lista = read.table("lista.txt", header = F) # arquivo que cont?m os pontos marcados pelo VLC
lista = lapply (lista, lubridate::seconds)
lista = lapply(lista[[1]], localizarIntervalo)

inserirParametros <- function (tempoInicial, tempoFinal, arquivoDestino){
  sprintf("\"%s\" -i \"%s\" -vcodec copy -acodec copy -ss %s -t %f %s", 
          ffmpeg, arquivoOrigem, tempoInicial, ifelse(tempoFinal <= 2, 2, tempoFinal), arquivoDestino)
}

horaInicialMenor <- function(itemLista){
  if (nrow(itemLista) == 1)
    return (itemLista$V11)
  else
    return(ifelse (itemLista$V2[1] > itemLista$V2[2], itemLista$V11[2], 
                   itemLista$V11[1] ))
}

tempoAAdicionar <- function(itemLista){
  if (nrow(itemLista) == 1) return (as.numeric(hms(itemLista$V12) - hms(itemLista$V11)))
  else{
    maior = ifelse (itemLista$V3[1] > itemLista$V3[2], itemLista$V12[1], itemLista$V12[2])
    return(as.numeric(hms(maior)) -  as.numeric(hms(horaInicialMenor(itemLista))))
  }
}
tempoAAdicionar(lista[[19]])

listaAnki = matrix(ncol = 3)
for (i in 1:length(lista)){
  if (nrow(lista[[i]]) == 0) next;
  x = lista[[i]]
  arquivoSaida = sprintf ("saida-%i.mp4", i)
  saida = sprintf ("%s%s", diretorioSaida, arquivoSaida)
  campoAnkiArquivo = sprintf("[sound:%s]", arquivoSaida)
  system(inserirParametros(horaInicialMenor(x), tempoAAdicionar(x) + 1.2, saida))
  if (nrow(lista[[i]]) == 1) 
    linha = c(as.character(x$V10), as.character(x$V10), campoAnkiArquivo)
  else
    linha = c(as.character(x$V10[x$V4 == "Bot"]), as.character(x$V10[x$V4 == "Top"]), campoAnkiArquivo)
  listaAnki = rbind (listaAnki, linha)
}
write.table(listaAnki[-1,], paste0( diretorioSaida ,"listaAnki.txt"), sep="\t", quote=F, 
            col.names=F, row.names=F, fileEncoding="UTF-8")
#wget -q -U Mozilla -O output.mp3 "http://translate.google.com/translate_tts?ie=UTF-8&tl=en&q=hello world, I'm here to tell you some news"