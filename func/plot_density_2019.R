# This functions creates the density plot of IDEB for 2019

plot_density_2019 <- function(){
  br_lsec_2019 <- ideb_lower_secondary %>% 
    filter(!abbr_state=="CE") %>% 
    filter(year == as.Date(ISOdate(2019,1,1))) %>% 
    mutate(unit = "Brazil")
  
  ce_lsec_2019 <- ideb_lower_secondary%>% 
    filter(abbr_state=="CE") %>% 
    filter(year == as.Date(ISOdate(2019,1,1))) %>% 
    mutate(unit = "CE")
  
  data_2019 <- rbind(ce_lsec_2019, br_lsec_2019)
  
  plot_2005 <- ggplot(data = data_2019)+
    geom_density(aes(x=ideb, fill = unit, color= unit), alpha=.6)+
    scale_x_continuous(limits = c(1.5, 9))+
    ggtitle(label = "Density Plot Education Quality Index (IDEB) - 2019",
            subtitle = "Lower Secondary Schools")+
    ylab("Density")+
    xlab("Education Quality Index")+
    scale_fill_manual(name="Legend", values= c("#fee090","#2166ac"))+
    scale_color_manual(name="Legend", values= c("#fee090","#2166ac"))+
    theme_classic()
  
  
  
  ggsave("plots/density2019.png", plot_2005)
}

