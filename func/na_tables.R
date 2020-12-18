# Returns a table with data on NAs per state


na_primary <- function(){
  library(xtable)
  
  na_count <- ideb_primary %>% 
    group_by(abbr_state) %>% 
    summarise(number_obs = n(), 
              number_nas = sum(is.na(ideb)), 
              percentage_na = round((number_nas/number_obs)*100, digits = 2)) %>% 
    arrange(percentage_na, )
  
  xt <- xtable(na_count, caption = "NAs in Primary Schools: 2005 - 2019")
  
  names(xt) <- c('State','Number of Observations','Number of NAs','Percentage of NAs' )
  return(xt)
}

na_secondary <- function(){
  library(xtable)
  
  na_count <- ideb_lower_secondary %>% 
    group_by(abbr_state) %>% 
    summarise(number_obs = n(), 
              number_nas = sum(is.na(ideb)), 
              percentage_na = round((number_nas/number_obs)*100, digits = 2)) %>% 
    arrange(percentage_na, )
  
  xt <- xtable(na_count, caption = "NAs in Lower Secondary Schools: 2005 - 2019")
  
  names(xt) <- c('State','Number of Observations','Number of NAs','Percentage of NAs' )
  return(xt)
}

