# Function plot_socioeconomic_vs_ideb
# This function analyses education quality index vs socioeconomic level

plot_socioeconomic_vs_ideb <- function(){
  # Primary Schools
  analisys_1_data <- left_join(ideb_lower_secondary %>% 
                                 select(abbr_state,type,code_school,year,ideb) %>% 
                                 filter(year == as.Date(ISOdate(2019,1,1))) %>% 
                                 filter(type == "Municipal"),
                               sei %>% select(code_school, absolute_sei),
                               by = "code_school") %>% 
    mutate(ceara=if_else(abbr_state=="CE","Ceara","Brazil")) %>% 
    arrange(ceara)
  
  
  plot_socio1 <- ggplot()+
    geom_point(data = analisys_1_data, aes(x=absolute_sei, y=ideb, color = ceara))+
    scale_color_manual(values= c("#fee090","#2166ac"), name="Legend", labels=c("Brazil","Ceará"))+
    ggtitle(label ="Education Index (IDEB 2019) vs. Socioeconomic condition (2015)",
            subtitle = "Lower Secondary Municipal Schools")+
    ylab("IDEB")+
    xlab("Socio Economic Status")+
    theme_classic()
  
  ggsave("plots/plot_socio1.png", plot_socio1)
  
  # Primary School
  
  analisys_2_data <- left_join(ideb_primary %>% 
                                 select(abbr_state,type,code_school,year,ideb) %>% 
                                 filter(year == as.Date(ISOdate(2019,1,1))) %>% 
                                 filter(type == "Municipal"),
                               sei %>% select(code_school, absolute_sei),
                               by = "code_school") %>% 
    mutate(ceara=if_else(abbr_state=="CE","Ceara","Brazil")) %>% 
    arrange(ceara)
  
  plot_socio2 <- ggplot()+
    geom_point(data = analisys_2_data, aes(x=absolute_sei, y=ideb, color = ceara))+
    scale_color_manual(values= c("#fee090","#2166ac"), name="Legend", labels=c("Brazil","Ceará"))+
    ggtitle(label ="Education Index (IDEB 2019) vs. Socioeconomic condition (2015)",
            subtitle = "Primary Municipal Schools")+
    ylab("IDEB")+
    xlab("Socio Economic Status")+
    theme_classic()

  
  ggsave("plots/plot_socio2.png", plot_socio2)}

