mudarPasta <- function (){
  mainDir <- "~/"
  subdir <- "langlearn/"
  setwd(mainDir)
  if (file.exists(subdir)){
    setwd(subdir)
  }else {
    dir.create(file.path(mainDir, subdir), showWarnings = FALSE)
    setwd(file.path(mainDir, subdir))
  }
  #showWarnings = TRUE
}

salvarPastasPassadas <- function()  {
  mudarPasta()
  write.table (diretorioSaida, "diretorioSaida.csv", row.names = F, col.names =F)
  write.table (arquivoLista, "arquivoLista.csv", row.names = F, col.names =F)
  write.table (arquivoOrigem, "arquivoOrigem.csv", row.names = F, col.names =F)
  write.table (arquivoLegenda, "arquivoLegenda.csv", row.names = F, col.names =F)
  write.table (ffmpeg, "ffmpeg.csv", row.names = F, col.names =F)
}

carregarDir <- function (variavel){
  mudarPasta()
  retorno = ""
  if (file.exists(paste0(variavel, ".csv")))
    retorno = read.table(paste0(variavel, ".csv"), row.names = NULL, stringsAsFactors = F)$V1
  return (retorno)
}
