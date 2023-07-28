library(tidyverse)
library(phyndr)
library(khroma)
vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

split_genus <- function(str) {
  str_split <- strsplit(str, "[_ ]+")
  vcapply(str_split, "[[", 1L)
}
data <- read_csv("data/stem_damage.csv")
data %>%
  group_by(site, species_matched) %>% 
  mutate(species_matched = gsub(pattern = " ",replacement = "_",x = species_matched),
         species_matched = ifelse(species_matched =="Pleioluma_macrocarpa", "Beccariella_macrocarpa", species_matched)) %>%
  filter(species_matched != "Sapindaceae") %>%
  add_count() %>%
  summarise(damage_m =mean(damage_d_half)/100, n_spp = mean(n), wd = mean(wood_density,na.rm = TRUE)) %>%
  mutate(genus = split_genus(species_matched))-> mm
mm$species_matched[mm$species_matched=="Eucalyptus_resinifera"]<-"Eucalyptus_resinifera_subsp._resinifera"
mm$species_matched[mm$species_matched=="Syzygium"]<-"Syzygium_sp."
mm$species_matched[mm$species_matched=="Endiandra"]<-"Endiandra_sp."

mm %>%
  dplyr::select(-n_spp) %>%
  pivot_wider(names_from = site, values_from = damage_m) %>%
  relocate(-pnw)-> mmm

mm %>%
  dplyr::select(site, species_matched)%>%
  pivot_wider(names_from = site, values_from = site) %>%
  unite(dro:stck,col = "site",sep = "|",na.rm = TRUE) -> sites


mm %>% filter(n_spp >2) %>%ungroup%>% mutate(damage_rank = ntile(-damage_m,n = 20)) %>% slice_max(order_by = damage_m,n = 5, with_ties = FALSE)->max

mm %>% filter(n_spp >2) %>% ungroup %>% mutate(damage_rank = ntile(-damage_m,n = 20)) %>% slice_min(order_by = damage_m,n = 5, with_ties = FALSE)->min



library(ggtree)
library(ape)
library(phytools)
library(pez)
library(taxonlookup)
library(ggnewscale)
library(viridis)
library(phyndr)
trait_data <- as.data.frame(mmm)
rownames(trait_data) <- trait_data$species_matched
trait_data <- trait_data[,-c(1:2)]

#read  tree
phylo <- read.tree('data/ALLMB.tre')

spp <- unique(c(phylo$tip.label,row.names(trait_data)))
tax_spp <- lookup_table(row.names(trait_data),by_species=TRUE)
tax_spp<-tax_spp[,c('genus','family','order')]




            
data_spp <- data.frame(species=phylo$tip.label)
data_spp$genus <- split_genus(phylo$tip.label)
data_spp <-left_join(data_spp,tax_spp)
spp_selected <- data_spp[data_spp$genus %in% mmm$genus,]
setdiff( mmm$genus,spp_selected$genus)
#spp_selected <- data_spp[data_spp$species %in% mmm$species_matched,]
spp_delete <- data_spp[!(data_spp$genus %in% spp_selected$genus),]
spp_delete <- as.character(spp_delete$species)
phyl_collapse <- pez::drop_tip(phylo,spp = spp_delete)
phyl_collapse <- force.ultrametric(phyl_collapse)

spp <- unique(c(phyl_collapse$tip.label,row.names(trait_data)))
tax_spp <- lookup_table(spp,by_species=TRUE)
tax_spp<-tax_spp[,c('genus','family','order')]


phy <- phyndr_taxonomy(phyl_collapse,row.names(trait_data),tax_spp)
gen_only <-grep(pattern = "genus::",phy$tip.label,value = TRUE)
gen_only <- gsub(pattern = "genus::",replacement = "",gen_only)
phy$tip.label <- gsub(pattern = "genus::",replacement = "",phy$tip.label)


ancs <-mrca(phy, full = FALSE)
ancs <- cbind(species = row.names(ancs),as.data.frame(ancs))
longancs <-pivot_longer(ancs, -species)

setdiff(rownames(trait_data),phy$tip.label)
length(phy$tip.label)
#mrca(phyl_collapse)
#MRCA(phy,c("Lophostemon_suaveolens","Syzygium_cormiflorum")) #Myrtaceae
#MRCA(phy,c("Endiandra_impressicosta","Daphnandra"))
#MRCA(phy,c("Placospermum_coriaceum","Cardwellia_sublimis"))#Laurales
#MRCA(phyl_collapse,c("Ceratophyllum","Adoxa"))


write.tree(phy,file = "output/phy.tre")
phy <- read.tree("output/phy.tre")
phy$edge.length[152] <- 50
phy$edge.length[1] <- 13

biomass <- read_csv("data/agb.csv")

biomass %>%
  mutate(species_matched = gsub(pattern = " ",replacement = "_",species_matched)) %>%
  filter(!is.na(species_matched)) %>%
  group_by(site, species_matched) %>%
  summarise(agb= sum(agb, na.rm = TRUE)) %>%
  pivot_wider(names_from = site, values_from= agb, names_prefix = "agb_")-> agb

mm %>%
  group_by(species_matched) %>%
  summarise(wd = mean(wd, na.rm = TRUE))-> wd
mmm_2 <-left_join(mmm,agb)
mmm_2 <-left_join(mmm_2,wd)

#species matched to genus nam for genus only tips  in phyndr
ii <- mmm_2$genus %in% gen_only
mmm_2$species_matched[ii] <-mmm_2$genus[ii]

low <- "#364B9A"
mid <- '#F99858'
high <- '#A50026'

q <- ggtree(phy) %<+% mmm_2 
q +
  #scale_color_gradient(low = "yellow",high = "red")+
  geom_tippoint(aes(fill = dro, size= agb_dro),pch=21,position = position_nudge(x = 140,0))+
  geom_tippoint(aes(fill = mtlwrf, size= agb_mtlwrf),pch=21,position = position_nudge(x = 120,0))+
  geom_tippoint(aes(fill = mtlwsc, size= agb_mlsc),pch=21,position = position_nudge(x = 100,0))+
  geom_tippoint(aes(fill = stck, size= agb_station_creek),pch=21,position = position_nudge(x = 80,0))+
  geom_tippoint(aes(fill = pnw, size= agb_pnw),pch=21,position = position_nudge(x = 60,0))+
  khroma::scale_fill_grayC(name = "Proportion\ndamage",range = c(0.2,1),reverse = FALSE)+
  guides(size = guide_legend(title = "Aboveground\nbiomass",nrow=2,byrow=TRUE))+
  #new_scale_color()+
  new_scale(new_aes = "colour")+
  scale_colour_gradient2(low=low,mid = mid ,high=high, name='Wood\ndensity',
                         midpoint = 0.7)+
  geom_tippoint(aes(colour = wd),size=3,shape = "square",position = position_nudge(x = 35,0))+
  geom_cladelabel(node=113, label="Myrtaceae", align=FALSE, angle =  90, color='black',offset.text=4)+
  geom_cladelabel(node=143, label="Laurales", align=TRUE, angle =  90, color='black',offset.text=4)+
  geom_cladelabel(node=134, label="Proteales", align=TRUE, angle =  90, color='black',offset.text=4)+
  geom_cladelabel(node=121, label="Superasterids", align=TRUE, angle =  90, color='black',offset.text=4)+
  geom_cladelabel(node=88, label="Rosids", align=TRUE, angle =  90, color='black',offset.text=4)+
  geom_cladelabel(node=102, label="Malvidae", align=TRUE, angle =  90, color='black',offset.text=4)+
  theme(text = element_text(size =15),legend.position = "bottom")

ggsave("output/phylo_damagev1.pdf",height = 12.5)
#print(g)
#dev.off()

