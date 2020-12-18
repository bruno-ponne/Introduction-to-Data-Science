# Function load_ideb_primary()
# This function loads IDEB data for Primary Schools

load_ideb_primary <- function(){
  df <- read_excel("data.nosync/ideb_primary_school.xlsx") %>% 
    rename(abbr_state = SG_UF,
           code_mun = CO_MUNICIPIO,
           name_mun = NO_MUNICIPIO,
           code_school = ID_ESCOLA,
           name_school = NO_ESCOLA,
           type = REDE,
           IDEB_2005 = VL_OBSERVADO_2005,
           IDEB_2007 = VL_OBSERVADO_2007,
           IDEB_2009 = VL_OBSERVADO_2009,
           IDEB_2011 = VL_OBSERVADO_2011,
           IDEB_2013 = VL_OBSERVADO_2013,
           IDEB_2015 = VL_OBSERVADO_2015,
           IDEB_2017 = VL_OBSERVADO_2017,
           IDEB_2019 = VL_OBSERVADO_2019) %>% 
    gather(year, ideb, IDEB_2005:IDEB_2019) %>% 
    replace_with_na(replace=list(ideb = "-")) %>% 
    mutate(year = as.Date(ISOdate((str_replace(year, "IDEB_","")),1,1))) %>%
    mutate(ideb = as.numeric(ideb)) %>% 
    mutate(code_mun = as.numeric(substr(code_mun, 1, nchar(code_mun)-1)))
  return(df)
}




