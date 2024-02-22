Frequencia <- function(dados, NtotalAnimais, var){
  var_freq <- dados%>%
    group_by_at(var)%>%
    reframe(freqAbs = sum(n_animais),
            freqRel = (sum(n_animais)/NtotalAnimais),
            freqRelPer = (sum(n_animais)/NtotalAnimais)*100, 
            foo = ".")
  return(var_freq)
}