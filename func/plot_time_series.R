# Function load_sei()
# This function analyses education in time and delivers time series plots
plot_time_series <- function(){
  # Primary Schools
  br_primary <- ideb_primary %>% 
    filter(!abbr_state=="CE") %>% 
    group_by(year) %>% 
    summarise(mean_ideb=mean(ideb, na.rm=TRUE)) %>% 
    mutate(unit = "Brazil")
  
  ce_primary <- ideb_primary %>% 
    filter(abbr_state=="CE") %>% 
    group_by(year) %>% 
    summarise(mean_ideb=mean(ideb, na.rm=TRUE)) %>% 
    mutate(unit= "CE")
  
  plot_series_primary <- rbind(ce_primary, br_primary)
  
  plot_s_p <- ggplot(data = plot_series_primary, aes(x=year, y=mean_ideb, linetype=unit))+
    geom_line()+
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
    scale_linetype_manual(values = c("dashed", "solid"), name="Legend")+
    geom_vline(xintercept = as.Date(ISOdate(2008,1,1)), linetype = "longdash", color="#2166ac")+
    annotate("text", x = as.Date(ISOdate(2010,10,1)), y = 6, label = "Policy Intervention", color = "#2166ac")+
    ggtitle(label ="Education Quality Index (IDEB) evolution",
            subtitle = "Primary Schools")+
    ylab("Education Quality Index")+
    xlab("Year")+
    theme_classic()
  
  ggsave("plots/plot_s_p.png", plot_s_p)
  
  # Lower Secondary School
  
  br_lsec <- ideb_lower_secondary %>% 
    filter(!abbr_state=="CE") %>% 
    group_by(year) %>% 
    summarise(mean_ideb=mean(ideb, na.rm=TRUE)) %>% 
    mutate(unit = "Brazil")
  
  ce_lsec <- ideb_lower_secondary%>% 
    filter(abbr_state=="CE") %>% 
    group_by(year) %>% 
    summarise(mean_ideb=mean(ideb, na.rm=TRUE)) %>% 
    mutate(unit= "CE")
  
  plot_series_lsec <- rbind(ce_lsec, br_lsec)
  
  plot_ls_p <- ggplot(data = plot_series_lsec, aes(x=year, y=mean_ideb, linetype=unit))+
    geom_line()+
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
    scale_linetype_manual(values = c("dashed", "solid"), name="Legend")+
    geom_vline(xintercept = as.Date(ISOdate(2008,1,1)), linetype = "longdash", color="#2166ac")+
    annotate("text", x = as.Date(ISOdate(2011,1,1)), y = 5, label = "Policy Intervention", color = "#2166ac")+
    ggtitle(label ="Education Quality Index (IDEB) evolution",
            subtitle = "Lower Secondary Schools")+
    ylab("Education Quality Index")+
    xlab("Year")+
    theme_classic()
  
  ggsave("plots/plot_ls_p.png", plot_ls_p)
  
}



