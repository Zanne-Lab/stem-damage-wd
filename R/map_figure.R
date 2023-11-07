library(tidyverse)
library(ggmap)
library(ggspatial)
library(colorspace)
library(patchwork)

read_csv("data/stem_damage.csv") %>%
  dplyr::select(Site, longitude, latitude ,prec_90m,species_matched) %>%
  dplyr::distinct(Site,latitude, longitude,prec_90m, species_matched) %>%
  dplyr::count(Site,latitude, longitude, prec_90m,name = "SpeciesNumber") %>%
  dplyr::arrange(desc(prec_90m)) %>%
  dplyr::mutate(Site = paste0(Site," (n = " ,SpeciesNumber,")"))->p

location <- c(109,-42.33,155.33,-10.1)
myMap <- get_map(location=location,
                 source="google", maptype="satellite", crop=FALSE,zoom = 4)

oz <- ggmap(myMap)+
  ylim(c(-39,-11))+
  xlim(c(114,153))+
  geom_rect(aes(xmin = 143, xmax = 147, ymin = -19, ymax = -15), color = "white", fill = NA, linewidth =1.2)+
  labs(x="Longitude",y="Latitude")
s <- "style=feature:administrative.locality|element:labels|visibility:off&style=feature:landscape|element:all|color:0xdddddd"
# Generated on https://mapstyle.withgoogle.com/

fnq_location<-"Daintree National Park"   
fnqmap <- get_googlemap(center = fnq_location, maptype="terrain",
                        crop=FALSE,zoom = 10, style = s)
site <- ggmap(fnqmap)+
  geom_point(aes(x= longitude, y = latitude, fill = prec_90m), 
             data = p, size =6, colour = "black",shape = 21)+
  scale_fill_continuous_sequential(name = "Precipitation (mm)",palette = "Green-Yello")+
  geom_spatial_label_repel(aes(label=Site,x =longitude, y = latitude),
                           box.padding = 1, data = p, size =7)+
  labs(x="Longitude",y="Latitude")+
  theme(legend.position = c(0.88, 0.15),
legend.background = element_rect(fill = "white", color = "black"))

map_fig <- site + inset_element(oz, 0,0.6,0.49,1, clip = TRUE) &
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 14))

#ggsave(filename = "output/figure_1.png", plot = map_fig)
ggsave(filename = "output/figure_1.pdf", plot = map_fig, dpi = 600, width = 10, height = 10)

