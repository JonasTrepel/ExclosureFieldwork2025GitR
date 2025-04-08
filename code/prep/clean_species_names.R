
library(data.table)
library(tidyverse)
library(gridExtra)

plot_meta <-  fread("data/raw/plot_level_data_2025.csv")

plot_species <- fread("data/raw/plot_species_2025.csv") 

sp <- plot_species %>%
  dplyr::select(species) %>% 
  arrange(species) %>% 
  unique() %>% 
  pull()
sp

#[62] "Crassula spathulata"          
#[63] "Crassula splathulata" 


# "DIchrostachys cinerea"


#[126] "Ipomea ficifolia"             
#[127] "Ipomoea plebeia"              
#[128] "Ipomoea sinensis"

species_traits <- fread("data/raw/species_traits_2025.csv") 

st <- species_traits %>%
  dplyr::select(species) %>% 
  arrange(species) %>% 
  unique() %>% 
  pull()
st

#[125] "Ipomea ficifolia"             
#[126] "Ipomoea plebeia"              
#[127] "Ipomoea sinensis" 

#[64] "Phyllanthus maderapatensis"   
#[165] "Phyllanthus maderaspatensis" 

setdiff(st, sp)
setdiff(sp, st)


#### traits                     ## species 
#[1] "Bolusanthes speciosus"     "Bolusanthus speciosus" - res
#[2] "Cardiospermum halicabum"   "Cardiospermum halicacabum" - res
#[3] "Cissus cornifolia"         "Cissus cornifolius" - res 
#[4] "Cordia monoeca"            "Cordia monoica" - res
#[5] "Corylostigma virgatum"     ? - res
#           ?                    "Crassula splathulata" - res
#[6] "Cynanchum elipticum"       "Cynanchum ellipticum" - res
#[7] "Cynanchum viminale"        "Cynanchum viminalie" - res
#           ?                    "DIchrostachys cinerea" - res
#[8] "Delospemum subincanun"     "Delosperma sabincanum" - res
#[9] "Dovyalis zayheri"          "Dovyalis zeyheri" - res 
#[10] "Drosanthemum hispidium"   "Drosanthemum hispidum" - res
#[11] "Euphorbia maurtanica"     "Euphorbia mauritanica" - res 
#[12] "Heliotropium stigosum"    "Heliotropium strigosum"  - res
#[13] "Jasminum stenolosum"      "Jasminum stenolobum" - res 
#[14] "Lannea schweinfurtii"     "Lannea schweinfurthii"  - res
#[15] "Lauridea tetragona"       "Lauridia tetragona"  - res
#[16] "Mearua caffra"            "Maerua caffra" - res
#[17] "Mytroxylon aethiopicum"   "Mystroxylon aethiopicum" - res
#[18] "Paramullugo nudicaullis"  "Paramollugo nudicaulis" - res 
#[19] "Phyllanthus maderapatensis"       ? - res
#[20] "Plumbego auriculata"      "Plumbago auriculata" - res
#[21] "Puppilea luppacea"        "Pupalia lappacea" - res
#[22] "Stachytapheta indica"     "Stachytarpheta indica" - res
#[23] "Thunbergia natalenis"     "Thunbergia natalensis" - res 
#[24] "Volkameuria glabra"       "Volkameria glabra" -res 
#           ?                    " glochidiata" 
