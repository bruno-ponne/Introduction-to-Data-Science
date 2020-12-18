
# This function analyses spatial data

plot_ceara <- function(){
  states_map <- read_sf("maps.nosync/vetor_EstadosBR_LLWGS84/EstadosBR_IBGE_LLWGS84.shp")
  states_map <- states_map %>% mutate(ceara = ESTADO=="CE")
  plot_ceara <- ggplot()+ 
    geom_sf(data = states_map, aes(fill=ceara), size=0.5, color="black")+
    scale_fill_manual(values= c("#fee090","#2166ac"))+
    ggtitle(label ="Ceará",
            subtitle = "Highlighted in blue")+
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.position = "none")
  ggsave("plots/plot_ceara.png", plot_ceara)
}