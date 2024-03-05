usa <- ne_states(country = "United States of America", returnclass = "sf")
dados_latlongUS <-read_csv("../Dados/Processados/USA_latlong.csv")

Dor_estado <- totais%>%
  filter(utilizado == "sim")%>%
  group_by(estado,State_Name,dor)%>%
  summarise(Nanimais = sum(n_animais))

Dor_estado_geo <- left_join(Dor_estado,
                        dados_latlongUS , 
                        by = c("estado" = "estado"))

Dor_estado_geo_convert <- Dor_estado_geo%>%
  group_by(estado,dor) %>%
  mutate(longitude = longitude + runif(1, -1, 1),  
         latitude = latitude + runif(1, -1, 1)) %>%
  ungroup() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326))

Dor_estado_geo_convert  <- Dor_estado_geo_convert [!(Dor_estado_geo_convert $estado %in% c("HI", "AK", "PR")), ]

MAPA3 <- ggplot() +
  geom_sf(data = usa, color = "white", fill = "gray") +
  geom_sf(data = Dor_estado_geo_convert, aes(size = Nanimais,
                              color = dor, 
                              text = paste0("Estado: ",State_Name,"\nExposição a dor: ", dor,
                                            "\n Nº de animais: ", Nanimais)), alpha = 0.7)+
  scale_size_continuous(range = c(2, 10), name = "") +
  scale_color_manual(values = c("não" = "#e64a19", "sim" = "#052935"),
                     label = c("sim"="Sim", "nao" = "não"))+
  labs(color = "Exposição a dor",
       size = " ")+
  theme_void()+
  coord_sf(xlim = c(-125, -66), ylim = c(25, 49))

ggplotly(MAPA3, tooltip = "text")%>%
  layout( xaxis = list( linecolor = 'white'), 
          yaxis = list( linecolor = 'white'),
          legend = list(
            bgcolor = "transparent", 
            bordercolor = "transparent",  
            itemsizing = "constant", 
            itemwidth = 30
          ))
