
# This function analyses spatial data

spatial_data <- function(){
  municipalities_map <- read_sf("maps.nosync/municipio5564/municipio5564.shp") %>% 
    select(CODIGO_MUN, NOME_UF, geometry) %>% 
    rename(code_mun = CODIGO_MUN, abbr_state = NOME_UF, geometry = geometry)
  states_map <- read_sf("maps.nosync/vetor_EstadosBR_LLWGS84/EstadosBR_IBGE_LLWGS84.shp")
  
  
  #Preparing data for analysis 3:
  analysis_3_data <- ideb_lower_secondary %>% 
    group_by(code_mun, year) %>% 
    summarise(mean_ideb = mean(ideb, na.rm = T)) %>% 
    filter(year == as.Date(ISOdate(2007,1,1)))
  analysis_3_data$ideb_level = cut(analysis_3_data$mean_ideb,
                                   breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                                   labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                                   right  = FALSE)
  analysis_3_data <- left_join(municipalities_map, analysis_3_data, by="code_mun")
  
  #Preparing data for analysis 4:
  analysis_4_data <- ideb_lower_secondary %>% 
    group_by(code_mun, year) %>% 
    summarise(mean_ideb = mean(ideb, na.rm = T)) %>% 
    filter(year == as.Date(ISOdate(2019,1,1)))
  analysis_4_data$ideb_level = cut(analysis_4_data$mean_ideb,
                                   breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                                   labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                                   right  = FALSE)
  analysis_4_data <- left_join(municipalities_map, analysis_4_data, by="code_mun")
  
  analysis_3_4_data <- rbind(analysis_3_data,analysis_4_data) %>% 
    mutate(year = if_else(year == as.Date(ISOdate(2019,1,1)),2019,2007))
  
  spatial_plot1 <- ggplot()+ 
    geom_sf(data = states_map, size=0)+
    geom_sf(data = na.omit(analysis_3_4_data), aes(fill=ideb_level), size=0)+
    scale_fill_manual(name="Education Quality (IDEB)", values = c('#d73027','#f46d43','#fdae61','#fee090','#abd9e9','#74add1','#4575b4','#313695'))+
    geom_sf(data = filter(states_map, ESTADO=="CE"), size=0.5, color="white", fill="transparent")+
    ggtitle(label ="Education Quality",
            subtitle = "Lower Secondary Schools")+
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())+
    facet_wrap(~year)
  ggsave("plots/spatial_plot_sec.png", spatial_plot1)
  
  
  #Preparing data for analysis 5:
  analysis_5_data<- ideb_primary %>% 
    group_by(code_mun, year) %>% 
    summarise(mean_ideb = mean(ideb, na.rm = T)) %>% 
    filter(year == as.Date(ISOdate(2007,1,1)))
  analysis_5_data$ideb_level = cut(analysis_5_data$mean_ideb,
                                   breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                                   labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                                   right  = FALSE)
  analysis_5_data <- left_join(municipalities_map, analysis_5_data, by="code_mun")
  
  #Preparing data for analysis 6:
  analysis_6_data <- ideb_primary %>% 
    group_by(code_mun, year) %>% 
    summarise(mean_ideb = mean(ideb, na.rm = T)) %>% 
    filter(year == as.Date(ISOdate(2019,1,1)))
  analysis_6_data$ideb_level = cut(analysis_6_data$mean_ideb,
                                   breaks = c(0,1,2,3,4,5,6,7,8,9,10),
                                   labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                                   right  = FALSE)
  analysis_6_data <- left_join(municipalities_map, analysis_6_data, by="code_mun")
  
  analysis_5_6_data <- rbind(analysis_5_data,analysis_6_data) %>% 
    mutate(year = if_else(year == as.Date(ISOdate(2019,1,1)),2019,2007))
  
  spatial_plot2 <- ggplot()+ 
    geom_sf(data = states_map, size=0)+
    geom_sf(data = na.omit(analysis_5_6_data), aes(fill=ideb_level), size=0)+
    scale_fill_manual(name="Education Quality (IDEB)", values = c('#d73027','#f46d43','#fdae61','#fee090','#abd9e9','#74add1','#4575b4','#313695'))+
    ggtitle(label ="Education Quality",
            subtitle = "Primary Schools")+
    geom_sf(data = filter(states_map, ESTADO=="CE"), size=0.5, color="white", fill="transparent")+
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())+
    facet_wrap(vars(year))
  ggsave("plots/spatial_plot_pri.png", spatial_plot2)

}



