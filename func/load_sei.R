
# Function load_sei()
# This function loads data on the socio economic index (sei) of the schools.

load_sei <- function(){
  sei <- read_excel('data.nosync/sei_2015.xlsx')%>%
    mutate(CO_MUN = as.numeric(substr(CO_MUNICIPIO, 1, nchar(CO_MUNICIPIO)-1))) %>% # adjusts CO_MUN to 6 digits
    select(-CO_MUNICIPIO,-NOME_MUNICIPIO) %>% 
    rename(code_school = CO_ESCOLA,
           name_school = NOME_ESCOLA,
           code_state = CO_UF,
           name_state = NOME_UF,
           id_area = ID_AREA,
           type = TP_DEPENDENCIA,
           location = TP_LOCALIZACAO,
           number_stud = QTD_ALUNOS_INSE,
           absolute_sei = INSE_VALOR_ABSOLUTO,
           categorical_sei = INSE_CLASSIFICACAO,
           code_mun = CO_MUN)
  return(sei)
}