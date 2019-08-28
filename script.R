if (!'data.table' %in% installed.packages() ) {
  install.packages("data.table")
}
library(data.table)

## Para executar o programa basta adicionar o caminho até o arquivo na linha 9 e executar com o 
## comando ´Rscript script.R´ após baixar a linguagem R

# alterar caminho do arquivo de texto se preciso
texto <- readLines("DemCifrado.txt")

### tabelas de frequencia de letras
frequency_table <- data.table(letter = c("A","E","O","S","R","I","N","D",
                                         "M","U","T","C","L","P","V","G",
                                         "H","Q","B","F","Z","J",
                                         "X", "K", "Y", "W"), 
                            normal_freq = c(14.63,12.57,10.73,7.81,6.53,6.18,5.05,
                                            4.99,4.74,4.63,4.34,3.88,2.78,2.52,1.67,
                                            1.30,1.28,1.20,1.04,1.02,0.47,0.40,
                                            0.21, 0.02, 0.01, 0.01))

eng_frequency_table <- data.table(letter = c('A','B','C','D','E','F','G','H','I','J',
                                         'K','L','M','N','O','P','Q',
                                         'R','S','T','U','V','W','X','Y','Z'), 
                              normal_freq = c(8.167,1.492,2.782,4.253,12.702,2.228,2.015,6.094,6.966,
                                              0.153,0.772,4.025,2.406,6.749,7.507,1.929,0.095,5.987,
                                              6.327,9.056,2.758,0.978,2.360,0.150,1.974,0.074))


## gambiarra pra eu fazer o next letra
alfabeto <- data.table(text = c(
  "a","b","c","d","e","f","g","h","i","j","k","l","m",
  "n","o","p","q","r","s","t","u","v","w","x","y","z"),
  next_text = c(
    "b","c","d","e","f","g","h","i","j","k","l","m",
    "n","o","p","q","r","s","t","u","v","w","x","y","z","a"))
alfabeto[, index := .I-1]

## algoritmo para o indice de coincidencia
indice_coincidencia <- function() {
  best_value <- 0
  best_key <- 0
  ## testa para cada tamanho de chave de 1 a 13
  for (key_size in 1:13) {
    # transforma o texto em data table, 1 letr apor linha
    text_table <- data.table(text = unlist(strsplit(texto, "", fixed = T)))
    # para cada letra, faz o mod com o tamanho da chave para definir o subgrupo para cada letra vai
    text_table[, group := .I%%key_size]
    # especificando o tamanho de cada subgrupo
    text_table[, group_size := .N , by="group"]
    
    # calculando a frequencia que cada letra aparece no seu subgrupo
    # tanto em porcentagem como apenas a quantidade de vezes total que ela aparece
    text_table <- text_table[ , .(freq = .N/first(group_size), 
                                  letter_freq = .N),by=.(text, group,group_size)]
    
    # revisar, faz o somatório de frequencias ao quadrado de cada letra em seu subgrupo
    # depois faz a média das frequencias dos key_size grupes 
    print(paste(key_size,' - ',text_table[, sum(freq^2), by=.(group)][,mean(V1)]))
    
    # a mesma coisa do que o print, só salva o tamanho de chave que conseguiu o maior indice de coincidencia
    if (text_table[, sum(freq^2), by=.(group)][,mean(V1)] > best_value) {
      best_value <- text_table[, sum(freq^2), by=.(group)][,mean(V1)]
      best_key <- key_size
    }
  }
  return(best_key)
}

key_size <- indice_coincidencia()

# transforma o texto em data table, 1 letra por linha
text_table <- data.table(text = unlist(strsplit(texto, "", fixed = T)))
text_table[, group := .I%%key_size]
text_table[, group_size := .N , by="group"]
text_table[ , c("freq", "letter_freq") :=  .(freq = .N/group_size, 
                              letter_freq = .N),by=.(text, group,group_size)]
# depois de tu adicionar a frequencia encontrada de cada letra no dataset
# adiciona uma coluna com a frequencia esperada para aquele dataset
# mudar a tabela de frequencia dependendo da lingua do dataset
text_table[, expected := (frequency_table[letter == toupper(text),normal_freq] * group_size)/100
           , by=.(text, group_size)]

decrypt <- function(letters, letter_freq, expected) {
  depara <- setDT(data.table(letters, letter_freq))
  depara <- depara[,.N,by=.(letters)] 
  
  # imagina isso como uma heuristica
  # tem uma formula de diferenca da frequencia encontrada pela esperada 
  # o valor encontrado fica salvo na variavel best ( foi mal pelos nomes ruins )
  # para cada shift, ou seja, trocar o A pelo B e o B pelo C ...
  # ele realiza o calculo da heuristica e ve se a frequencia bate com o esperado novamente
  # fica salvo a distancia com a melhor nota encontrada
  # retorna a melhor distancia de cada grupo
  
  best <- sum((letter_freq - expected)^2/expected)
  best_shifted <- letters
  best_freq_shifted <- letter_freq
  shifted <- letters
  freq_shifted <- letter_freq
  
  best_distance <- 0
  for (distance in 1:25) {
    shifted <- alfabeto[shifted,,on="text"]
    shifted <- shifted$next_text
    freq_shifted <- depara[shifted,,on="letters"] 
    freq_shifted <- freq_shifted$N
    freq_shifted[is.na(freq_shifted)] <- 0
    new_best <- sum((freq_shifted - expected)^2/expected)
    
    if (new_best < best){
      best <- new_best
      best_shifted <- shifted
      best_freq_shifted <- freq_shifted
      best_distance <- distance
    }
  }
  
  print(alfabeto[index == best_distance,text])
  return(as.integer(best_distance))
}

shift_letra <- function(letra, dist) {
  saida <- letra
  if (dist > 0) {
    index <- alfabeto[text == letra, index]
    novo_index <- (index - dist) %% 26
    saida <- alfabeto[index == novo_index,text]
    
  }
  return(saida)
}

# retorna a distancia de shift necessário para cada grupo 
text_table[, lower_chi := as.integer(decrypt(text, letter_freq,expected)),by=.(group)]
# realiza o shift
text_table[, decifrado := shift_letra(text, lower_chi) ,by=.(lower_chi,group,text)]

texto_decifrado <- paste0(text_table$decifrado, collapse = "")
print(texto_decifrado)

