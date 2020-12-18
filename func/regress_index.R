# Function for the Linear Model

regress_index <- function(){
  library(stargazer)
  
  br_lsec_2005 <- ideb_lower_secondary %>% 
    filter(!abbr_state=="CE") %>% 
    filter(year == as.Date(ISOdate(2005,1,1))) %>% 
    mutate(unit = "Brazil")
  
  ce_lsec_2005 <- ideb_lower_secondary%>% 
    filter(abbr_state=="CE") %>% 
    filter(year == as.Date(ISOdate(2005,1,1))) %>% 
    mutate(unit = "CE")
  
  data_2005 <- rbind(ce_lsec_2005, br_lsec_2005)
  
  atlas <- read_excel("data.nosync/atlas.xlsx")
  atlas <- atlas %>% 
    select(ANO, UF, Codmun6, IDHM) %>% 
    filter(ANO == 2010) %>% 
    rename(year = ANO, code_state = UF, code_mun = Codmun6, hdi = IDHM)
  
  data_2005 <- left_join(data_2005,
                         atlas %>% select(code_mun, hdi),
                         by = "code_mun")
  
  br_lsec_2019 <- ideb_lower_secondary %>% 
    filter(!abbr_state=="CE") %>% 
    filter(year == as.Date(ISOdate(2019,1,1))) %>% 
    mutate(unit = "Brazil")
  
  ce_lsec_2019 <- ideb_lower_secondary%>% 
    filter(abbr_state=="CE") %>% 
    filter(year == as.Date(ISOdate(2019,1,1))) %>% 
    mutate(unit = "CE")
  
  data_2019 <- rbind(ce_lsec_2019, br_lsec_2019)
  
  data_2019 <- left_join(data_2019,
                         atlas %>% select(code_mun, hdi),
                         by = "code_mun")
  
  lm_2005 <- lm(ideb~unit, data=data_2005)
  
  lm_2005_hdi <- lm(ideb~unit+hdi, data=data_2005)
  
  lm_2019 <- lm(ideb~unit, data=data_2019)
  
  lm_2019_hdi <- lm(ideb~unit+hdi, data=data_2019)

  
  return(stargazer(lm_2005, lm_2005_hdi, lm_2019, lm_2019_hdi, type= "html",
                   title = "Table 1 - OLS Model: Education Quality Index per group.",
                   dep.var.labels= c("Education Quality Index"),
                   covariate.labels = c("Ceará", "HDI", "Constant"),
                   model.numbers= FALSE,
                   column.labels = c("2005    ","2005    ","2019    ","2019    "),
                   omit.stat = c("f", "adj.rsq", "ser"),
                   column.sep.width = "2pt",
                   align = TRUE,
                   notes.append = FALSE,
                   notes = "<sup>&sstarf;</sup>p<0.1; <sup>&sstarf;&sstarf;</sup>p<0.05; <sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"))}


  