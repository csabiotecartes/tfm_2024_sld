library(odbc)

# db String Connection
DB_SERVER <- 'synw-aquas.sql.azuresynapse.net'
DB_DATABASE <- 'aquas'
DB_USERNAME <- 'clara.sabiote@aquas.cat'

# Crear connexió ODBC
conn <- dbConnect(odbc::odbc(), .connection_string = paste0('Driver={ODBC Driver 17 for SQL Server};Server=',DB_SERVER,';Port=1433;Database=',DB_DATABASE,';Uid=',DB_USERNAME, ';Authentication=ActiveDirectoryInteractive'), timeout = 10)

# Funció per obtenir les taules disponibles pels investigadors 
get_all_inv_tables <- function(){
  result<-dbGetQuery(conn, "SELECT v.name FROM sys.views as v WHERE OBJECT_SCHEMA_NAME(v.object_id) = 'z_inv'")
  return(result$name)
}

# Executa la funció
tables = get_all_inv_tables()
tables

# Funció per llegir una taula
get_inv_table <- function(tableName){
  result<-dbGetQuery(conn, paste0("SELECT * FROM [z_inv].[",  tableName, "]"))
  return(result)
}

library(dplyr)
library(tidyr)
library(stringr)

# Aquest codi serveix per generar les taules amb les variables d'interes
# un cop es tenen els pacients seleccionats segons criteris d'incl i excl

# Llegir la cohort seleccionada

masld_dx <- read.csv("masld_dx.csv")
masld_s <- read.csv("masld_s.csv")
ald_dx <- read.csv("ald_dx.csv")
ald_s <- read.csv("ald_s.csv")
metald_dx <- read.csv("metald_dx.csv")
metald_s <- read.csv("metald_s.csv")


masld_dx <- masld_dx %>%
  select(IdCas)
masld_s <- masld_s %>%
  select(IdCas)
metald_dx <- metald_dx %>%
  select(IdCas)
metald_s <- metald_s %>%
  select(IdCas)
ald_dx <- ald_dx %>%
  select(IdCas)
ald_s <- ald_s %>%
  select(IdCas)


masld <- rbind(masld_dx, masld_s)
metald <- rbind(metald_dx, metald_s)
ald <- rbind(ald_dx, ald_s)

dx <- rbind(masld_dx, ald_dx)
sospita <- rbind(masld_s, ald_s)


sld <- rbind(masld_dx, masld_s, ald_dx, ald_s)

sld <- sld %>%
  mutate(classificacio = if_else(IdCas %in% metald$IdCas, 2, if_else(IdCas %in% ald$IdCas, 3, 1)))

sld <- sld %>%
  mutate(diagnostic = if_else(IdCas %in% dx$IdCas, 1, 0))

# Any diagnòstic
# Pels grups amb dx això serà molt fàcil.
# Pel grup masld_s la data de dx serà la data amb el 1r hsi alterat,
# pel grup ald_s la data de dx serà la data de consum d'oh de risc

c_cex = get_inv_table("935_cohort_cmbd_aea")
c_cap = get_inv_table("935_cohort_redics_problemessalut")



# MASLD/MetALD
# DX
## Info CAPs
c_dx_masld_cap <- c_cap %>%
  filter(problema_salut_c == 'K7581' | problema_salut_c == 'K760')

## Info CEX
c_dx_masld_cex <- c_cex %>%
  filter(dx_pral_c == 'K7581' | dx_pral_c == 'K760' |
           dx_secundari_1_c == 'K7581' | dx_secundari_1_c == 'K760' |
           dx_secundari_2_c == 'K7581' | dx_secundari_2_c == 'K760' |
           dx_secundari_3_c == 'K7581' | dx_secundari_3_c == 'K760' |
           dx_secundari_4_c == 'K7581' | dx_secundari_4_c == 'K760' |
           dx_secundari_5_c == 'K7581' | dx_secundari_5_c == 'K760' |
           dx_secundari_6_c == 'K7581' | dx_secundari_6_c == 'K760' |
           dx_secundari_7_c == 'K7581' | dx_secundari_7_c == 'K760' |
           dx_secundari_8_c == 'K7581' | dx_secundari_8_c == 'K760' |
           dx_secundari_9_c == 'K7581' | dx_secundari_9_c == 'K760' |
           dx_secundari_10_c == 'K7581' | dx_secundari_10_c == 'K760' |
           dx_secundari_11_c == 'K7581' | dx_secundari_11_c == 'K760' |
           dx_secundari_12_c == 'K7581' | dx_secundari_12_c == 'K760' |
           dx_secundari_13_c == 'K7581' | dx_secundari_13_c == 'K760' |
           dx_secundari_14_c == 'K7581' | dx_secundari_14_c == 'K760')

# Agafarem preferentment la data de diagnòstic del cap, en el seu defecte
# la de cex
# Primer ordenem per pacient i ens quedem amb la primera data de dx 

c_dx_masld_cap_u <- c_dx_masld_cap %>%
  arrange(IdCas, data_problema_salut) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup()

c_dx_masld_cex_u <- c_dx_masld_cex %>%
  arrange(IdCas, data_visita) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup()

# Dates dx pacients masld_dx i metald_dx
sld_masld_dx <- filter(sld, (classificacio == 1 | classificacio == 2) & diagnostic == 1)

sld_masld_dx <- sld_masld_dx %>%
  left_join(c_dx_masld_cap_u, by = "IdCas") %>%
  rename(data_dx_cap = data_problema_salut) %>%
  left_join(c_dx_masld_cex_u, by = "IdCas") %>%
  rename(data_dx_cex = data_visita) %>%
  mutate(
    data_dx = coalesce(data_dx_cap, data_dx_cex)
  ) %>%
  select(IdCas, classificacio, diagnostic, data_dx, data_dx_cap, data_dx_cex)

rm(c_dx_masld_cap, c_dx_masld_cap_u, c_dx_masld_cex, c_dx_masld_cex_u)
# Sospita: a partir de la data del HSI
# Dates HSI (analitica) pels masld_s i metald_s
sld_masld_s <- filter(sld, (classificacio == 1 | classificacio == 2) & diagnostic == 0)
sld_masld_s <- sld_masld_s %>%
  left_join(hsi_alterat, by = "IdCas") %>%
  rename(data_dx = data_analitica) %>%
  mutate(data_dx_cap = NA, data_dx_cex = NA) %>%
  select(IdCas, classificacio, diagnostic, data_dx, data_dx_cap, data_dx_cex)

# ALD
# DX
# Info CAPs
c_dx_ald_cap <- c_cap %>%
  filter(str_detect(problema_salut_c, 'K70'))



# Info CEX
c_dx_ald_cex <- c_cex %>%
  filter(str_detect(dx_pral_c, 'K70') | str_detect(dx_secundari_1_c , 'K70') |
           str_detect(dx_secundari_2_c, 'K70') | str_detect(dx_secundari_3_c, 'K70') |
           str_detect(dx_secundari_4_c, 'K70') | str_detect(dx_secundari_5_c, 'K70') |
           str_detect(dx_secundari_6_c, 'K70') | str_detect(dx_secundari_7_c, 'K70') |
           str_detect(dx_secundari_8_c, 'K70') | str_detect(dx_secundari_9_c, 'K70') |
           str_detect(dx_secundari_10_c, 'K70') | str_detect(dx_secundari_11_c, 'K70') |
           str_detect(dx_secundari_12_c, 'K70') | str_detect(dx_secundari_13_c, 'K70') |
           str_detect(dx_secundari_14_c, 'K70') )


# Agafarem preferentment la data de diagnòstic del cap, en el seu defecte
# la de cex
# Primer ordenem per pacient i ens quedem amb la primera data de dx 

c_dx_ald_cap_u <- c_dx_ald_cap %>%
  arrange(IdCas, data_problema_salut) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup()

c_dx_ald_cex_u <- c_dx_ald_cex %>%
  arrange(IdCas, data_visita) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup()

# Dates dx pacients ald_dx
sld_ald_dx <- filter(sld, classificacio == 3 & diagnostic == 1)

sld_ald_dx <- sld_ald_dx %>%
  left_join(c_dx_ald_cap_u, by = "IdCas") %>%
  rename(data_dx_cap = data_problema_salut) %>%
  left_join(c_dx_ald_cex_u, by = "IdCas") %>%
  rename(data_dx_cex = data_visita) %>%
  mutate(
    data_dx = coalesce(data_dx_cap, data_dx_cex)
  ) %>%
  select(IdCas, classificacio, diagnostic, data_dx, data_dx_cap, data_dx_cex)

rm(c_dx_ald_cap, c_dx_ald_cap_u, c_dx_ald_cex, c_dx_ald_cex_u)

# Sospita: a partir de la data del primer consum d'OH de risc

oh_risc <- oh_risc %>%
  arrange(IdCas, data_dx) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup()

sld_ald_s <- filter(sld, classificacio == 3 & diagnostic == 0)

sld_ald_s <- sld_ald_s %>%
  left_join(oh_risc, by = "IdCas") %>%
  mutate(data_dx_cap = NA, data_dx_cex = NA)
  

taula_sld <- rbind(sld_masld_dx, sld_masld_s, sld_ald_dx, sld_ald_s)

rm(sld_masld_dx, sld_masld_s, sld_ald_dx, sld_ald_s, masld, masld_dx, 
   masld_s, metald, metald_dx, metald_s, sospita, ald, ald_dx, ald_s)

# Any naixement, sexe, RS, AGA, UP, nacionalitat, niv socioeconomic

c_sociodemo = get_inv_table("935_cohort_rca_talloficial")

seleccio_sociodemo <- c_sociodemo %>%
  select(IdCas, ANY_NAIX, sexe, rs_c, rs, aga_c, aga, up_assignacio_c, 
         up_assignacio, nacionalitat_c, nacionalitat, nivell_socio_economic_c)

taula_sld <- taula_sld %>%
  left_join(seleccio_sociodemo, by = "IdCas")

rm(seleccio_sociodemo)

# Tabac
c_tabac = get_inv_table("935_cohort_redics_tabac")
taula_dates_dx <- taula_sld %>%
  select(IdCas, data_dx)

tabac <- c_tabac %>%
  filter(IdCas %in% taula_sld$IdCas)

tabac <- tabac %>%
  left_join(taula_dates_dx, by = "IdCas")

tabac <- tabac %>%
  filter(data_dx >= INICI & data_dx <= FINAL)

tabac_u <- tabac %>%
  distinct(IdCas, .keep_all = TRUE)



taula_sld <- taula_sld %>%
  left_join(tabac_u, by = c("IdCas", "data_dx")) %>%
  rename (tabac = VALOR, i_tabac = INICI, f_tabac = FINAL) 

rm(tabac, tabac_u, c_tabac, c_sociodemo)

# Consum OH
c_oh_imc = get_inv_table("935_cohort_redics_seguimentusuari")

c_oh <- c_oh_imc %>%
  filter(VU_COD_VS == 'ALSET' | VU_COD_VS == 'ALHAB' |
           VU_COD_VS == 'ALDIA')

oh_data_dx <- c_oh %>%
  left_join(taula_dates_dx, by = "IdCas")

oh_data_dx$data_dx <- as.POSIXct(oh_data_dx$data_dx)

oh_data_dx <- oh_data_dx %>%
  mutate(dif_oh_dx = data_dx - VU_DAT_ACT) %>%
  mutate(dif_oh_dx = abs(dif_oh_dx))

oh_data_dx <- oh_data_dx %>%
  group_by(IdCas) %>%
  arrange(dif_oh_dx) %>%
  slice(1) %>%
  ungroup()

oh_info <- oh_data_dx %>%
  select(IdCas, VU_DAT_ACT, VU_VAL) %>%
  rename(data_oh = VU_DAT_ACT, UBEs_set = VU_VAL)
  
taula_sld <- taula_sld %>%
  left_join(oh_info, by = "IdCas")

rm(c_oh, oh_data_dx, oh_info)

# Mesures antropomètriques
imc <- c_oh_imc %>%
  filter(VU_COD_VS == 'TT101' | VU_COD_VS == 'TT102' | VU_COD_VS == 'TT103')

# Funció que enganxa les dates de dx dels pacients a les taules i selecciona
# l'observació més propera a aquesta
obs_propera_dx <- function(df, data_obs) {
  df <- df %>%
    left_join(taula_dates_dx, by = "IdCas")
  df <- df %>%
    mutate(dif_obs_dx = data_dx - data_obs) %>%
    mutate(dif_obs_dx = abs(dif_obs_dx))
  df <- df %>%
    group_by(IdCas) %>%
    arrange(dif_obs_dx) %>%
    slice(1) %>%
    ungroup()
}
taula_dates_dx$data_dx <- as.POSIXct(taula_dates_dx$data_dx)


# Afegir talla
talla <- c_oh_imc %>%
  filter(VU_COD_VS == 'TT101')
talla <- talla %>%
  filter(VU_VAL > 1)

talla <- obs_propera_dx(talla, talla$VU_DAT_ACT)
talla <- talla %>%
  rename(data_talla = VU_DAT_ACT, talla = VU_VAL)
talla <- talla %>%
  select(IdCas, data_talla, talla)

taula_sld <- taula_sld %>%
  left_join(talla, by = "IdCas")

# Afegir pes
pes <- c_oh_imc %>%
  filter(VU_COD_VS == 'TT102')
pes <- pes %>%
  filter(VU_VAL > 1)

pes <- obs_propera_dx(pes, pes$VU_DAT_ACT)
pes <- pes %>%
  rename(data_pes = VU_DAT_ACT, pes = VU_VAL)
pes <- pes %>%
  select(IdCas, data_pes, pes)

taula_sld <- taula_sld %>%
  left_join(pes, by = "IdCas")

# Afegir IMC 
imc <- c_oh_imc %>%
  filter( VU_COD_VS == 'TT103')
imc <- imc %>%
  filter(VU_VAL > 1)

imc <- obs_propera_dx(imc, imc$VU_DAT_ACT)
imc <- imc %>%
  rename(data_imc = VU_DAT_ACT, imc = VU_VAL)
imc <- imc %>%
  select(IdCas, data_imc, imc)

taula_sld <- taula_sld %>%
  left_join(imc, by = "IdCas")
rm(pes, talla, imc)

# Afegir FRCV

# Dx sobrepès i obesitat
ob_cap <- c_cap %>% 
  filter(str_detect(problema_salut_c, 'E66')) %>%
  select(IdCas, data_problema_salut) %>%
  rename(data_ob = data_problema_salut)

ob_cex <- c_cex %>%
  filter(str_detect(dx_pral_c, 'E66') | str_detect(dx_secundari_1_c, 'E66') |
           str_detect(dx_secundari_2_c, 'E66') | str_detect(dx_secundari_3_c, 'E66')|
           str_detect(dx_secundari_4_c, 'E66') | str_detect(dx_secundari_5_c, 'E66')|
           str_detect(dx_secundari_6_c, 'E66') | str_detect(dx_secundari_7_c, 'E66')|
           str_detect(dx_secundari_8_c, 'E66') | str_detect(dx_secundari_9_c, 'E66')|
           str_detect(dx_secundari_10_c, 'E66') | str_detect(dx_secundari_11_c, 'E66')|
           str_detect(dx_secundari_12_c, 'E66') | str_detect(dx_secundari_13_c, 'E66')|
           str_detect(dx_secundari_14_c, 'E66'))

ob_cex <- ob_cex %>%
  select(IdCas, data_visita) %>%
  rename(data_ob = data_visita)

ob_dx <- rbind(ob_cap, ob_cex)
rm(ob_cap, ob_cex)

ob_dx <- ob_dx %>%
  group_by(IdCas) %>%
  arrange(data_ob) %>%
  slice(1)

taula_sld <- taula_sld %>%
  mutate(ob = if_else(IdCas %in% ob_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(ob_dx, by = "IdCas")

# Dx DM2
dm_cap <- c_cap %>%
  filter(str_detect(problema_salut_c, 'E11')) %>%
  select(IdCas, data_problema_salut) %>%
  rename(data_dm2 = data_problema_salut)

dm_cex <- c_cex %>%
  filter(str_detect(dx_pral_c, 'E11') | str_detect(dx_secundari_1_c , 'E11') |
           str_detect(dx_secundari_2_c, 'E11') | str_detect(dx_secundari_3_c, 'E11') |
           str_detect(dx_secundari_4_c, 'E11') | str_detect(dx_secundari_5_c, 'E11') |
           str_detect(dx_secundari_6_c, 'E11') | str_detect(dx_secundari_7_c, 'E11') |
           str_detect(dx_secundari_8_c, 'E11') | str_detect(dx_secundari_9_c, 'E11') |
           str_detect(dx_secundari_10_c, 'E11') | str_detect(dx_secundari_11_c, 'E11') |
           str_detect(dx_secundari_12_c, 'E11') | str_detect(dx_secundari_13_c, 'E11') |
           str_detect(dx_secundari_14_c, 'E11') )
dm_cex <- dm_cex %>%
  select(IdCas, data_visita) %>%
  rename(data_dm2 = data_visita)

dm2_dx <- rbind(dm_cap, dm_cex)
rm(dm_cap, dm_cex)

dm2_dx <-dm2_dx %>%
  group_by(IdCas) %>%
  arrange(data_dm2) %>%
  slice(1)

taula_sld <- taula_sld %>%
  mutate(dm2 = if_else(IdCas %in% dm2_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(dm2_dx, by = "IdCas")

# Dx HTA
hta_cap <- c_cap %>%
  filter(problema_salut_c == 'I10') %>%
  select(IdCas, data_problema_salut) %>%
  rename(data_hta = data_problema_salut)

hta_cex <- c_cex %>%
  filter(dx_pral_c == 'I10' | 
           dx_secundari_1_c == 'I10' | 
           dx_secundari_2_c == 'I10' | 
           dx_secundari_3_c == 'I10' | 
           dx_secundari_4_c == 'I10' | 
           dx_secundari_5_c == 'I10' | 
           dx_secundari_6_c == 'I10' | 
           dx_secundari_7_c == 'I10' | 
           dx_secundari_8_c == 'I10' | 
           dx_secundari_9_c == 'I10' | 
           dx_secundari_10_c == 'I10' | 
           dx_secundari_11_c == 'I10' | 
           dx_secundari_12_c == 'I10' | 
           dx_secundari_13_c == 'I10' | 
           dx_secundari_14_c == 'I10') %>%
  select(IdCas, data_visita) %>%
  rename(data_hta = data_visita)
  
hta_dx <- rbind(hta_cap, hta_cex)
rm(hta_cap, hta_cex)

hta_dx <- hta_dx %>%
  group_by(IdCas) %>%
  arrange(data_hta) %>%
  slice(1)

taula_sld <- taula_sld %>%
  mutate(hta = if_else(IdCas %in% hta_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(hta_dx, by = "IdCas")

# Dx DLP
dlp_cap <- c_cap %>%
  filter(str_detect(problema_salut_c, 'E78'))%>%
  select(IdCas, data_problema_salut) %>%
  rename(data_dlp = data_problema_salut)

dlp_cex <- c_cex %>%
  filter(str_detect(dx_pral_c, 'E78') | str_detect(dx_secundari_1_c , 'E78') |
           str_detect(dx_secundari_2_c, 'E78') | str_detect(dx_secundari_3_c, 'E78') |
           str_detect(dx_secundari_4_c, 'E78') | str_detect(dx_secundari_5_c, 'E78') |
           str_detect(dx_secundari_6_c, 'E78') | str_detect(dx_secundari_7_c, 'E78') |
           str_detect(dx_secundari_8_c, 'E78') | str_detect(dx_secundari_9_c, 'E78') |
           str_detect(dx_secundari_10_c, 'E78') | str_detect(dx_secundari_11_c, 'E78') |
           str_detect(dx_secundari_12_c, 'E78') | str_detect(dx_secundari_13_c, 'E78') |
           str_detect(dx_secundari_14_c, 'E78') ) %>%
  select(IdCas, data_visita) %>%
  rename(data_dlp = data_visita)

dlp_dx <- rbind(dlp_cap, dlp_cex)
rm(dlp_cap, dlp_cex)

dlp_dx <- dlp_dx %>%
  group_by(IdCas) %>%
  arrange(data_dlp) %>%
  slice(1)

taula_sld <- taula_sld %>%
  mutate(dlp = if_else(IdCas %in% dlp_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(dlp_dx, by = "IdCas")

rm(ob_dx, dm2_dx, hta_dx, dlp_dx)


# Atenció hospitalaria aguts
c_ah = get_inv_table("935_cohort_cmbd_ah")
# Urgències
c_urg = get_inv_table("935_cohort_cmbd_urgencies") 



# Dataframe amb totes les malalties cardiovasculars 

# Faré una funció que busqui un dx als 4 dataframes (cap, cex, urg i ah)
# per a que sigui més ràpid
buscar_cap_cex_urg_ah <- function(...) {
  
  dxs <- list(...)
  
  resultat <- do.call(rbind, lapply(dxs, function(dx){
    
    dx_urg <- c_urg %>%
      filter(str_detect(motiu_c, dx)) %>%
      select(IdCas, data_entrada, motiu_c) %>%
      rename(data_dx = data_entrada) 
    
    dx_ah <- c_ah %>%
      filter(str_detect(DP, dx) | str_detect(DS1 , dx) |
               str_detect(DS2, dx) | str_detect(DS3, dx)) %>%
      select(IdCas, data_ingres) %>%
      rename(data_dx = data_ingres)
    
    dx_cap <- c_cap %>%
      filter(str_detect(problema_salut_c, dx)) %>%
      select(IdCas, data_problema_salut) %>%
      rename(data_dx = data_problema_salut)
    
    dx_cex <- c_cex %>%
      filter(str_detect(dx_pral_c, dx) | str_detect(dx_secundari_1_c , dx) |
               str_detect(dx_secundari_2_c, dx) | str_detect(dx_secundari_3_c, dx) |
               str_detect(dx_secundari_4_c, dx) | str_detect(dx_secundari_5_c, dx) |
               str_detect(dx_secundari_6_c, dx) | str_detect(dx_secundari_7_c, dx) |
               str_detect(dx_secundari_8_c, dx) | str_detect(dx_secundari_9_c, dx) |
               str_detect(dx_secundari_10_c, dx) | str_detect(dx_secundari_11_c, dx) |
               str_detect(dx_secundari_12_c, dx) | str_detect(dx_secundari_13_c, dx) |
               str_detect(dx_secundari_14_c, dx) ) %>%
      select(IdCas, data_visita) %>%
      rename(data_dx = data_visita)
    
    dx_dx <- rbind(dx_urg, dx_ah, dx_cex, dx_cap)    
  }))
  return(resultat)
} 

# Malalties cardiovasculars

mcv_dx <- buscar_cap_cex_urg_ah('I20', 'I21', 'I22', 'I23', 'I24', 'I25', 
                                'I50', 'I60', 'I61', 'I62', 'I63', 'I64', 
                                'I65', 'I66', 'I67', 'I68', 'I69', 'I71', 
                                'I73', 'Z867', 'I48', 'I49', 'I74', 'I75')

# Ara hauria d'enganxar les dates del dx a la taula del les malalties
# restar i dividir en pre-dx i post-dx
mcv_dx_dates_dx <- mcv_dx %>%
  left_join(taula_dates_dx, by = 'IdCas')

mcv_dx_dates_dx <- mcv_dx_dates_dx %>%
  mutate(dif_m_dx = data_dx.x - data_dx.y)

mcv_pre_dx <- mcv_dx_dates_dx %>%
  filter(dif_m_dx < 0)

mcv_post_dx <- mcv_dx_dates_dx %>%
  filter(dif_m_dx > 0)

# Un cop tinc els pacients que tenen mcv abans i després ho enganxo a la taula_sld

mcv_pre_dx <- mcv_pre_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

mcv_pre_dx <- mcv_pre_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_mcv_pre = data_dx.x)

mcv_post_dx <- mcv_post_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

mcv_post_dx <- mcv_post_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_mcv_post = data_dx.x)

taula_sld <- taula_sld %>%
  mutate(mcv_pre_dx = if_else(IdCas %in% mcv_pre_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(mcv_pre_dx, by = 'IdCas')

taula_sld <- taula_sld %>%
  mutate(mcv_post_dx = if_else(IdCas %in% mcv_post_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(mcv_post_dx, by = 'IdCas')

rm(mcv_dx, mcv_dx_dates_dx, mcv_pre_dx, mcv_post_dx)

# Faré el mateix amb les descompensacions
descomp_dx <- buscar_cap_cex_urg_ah('K7682', 'R18', 'I85', 'K652', 
                                    'C228', 'K922', 'K767')
descomp_dx_dates_dx <- descomp_dx %>%
  left_join(taula_dates_dx, by = 'IdCas')

descomp_dx_dates_dx <- descomp_dx_dates_dx %>%
  mutate(dif_d_dx = data_dx.x - data_dx.y)

descomp_pre_dx <- descomp_dx_dates_dx %>%
  filter(dif_d_dx < 0)

descomp_post_dx <- descomp_dx_dates_dx %>%
  filter(dif_d_dx > 0)

# Incorporem descompensacions a la taula sld

descomp_pre_dx <- descomp_pre_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

descomp_pre_dx <- descomp_pre_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_descomp_pre = data_dx.x)

descomp_post_dx <- descomp_post_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

descomp_post_dx <- descomp_post_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_descomp_post = data_dx.x)

taula_sld <- taula_sld %>%
  mutate(descomp_pre_dx = if_else(IdCas %in% descomp_pre_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(descomp_pre_dx, by = 'IdCas')

taula_sld <- taula_sld %>%
  mutate(descomp_post_dx = if_else(IdCas %in% descomp_post_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(descomp_post_dx, by = 'IdCas')

rm(descomp_dx, descomp_dx_dates_dx, descomp_pre_dx, descomp_post_dx)

# Hipertensió portal
htp_dx <- buscar_cap_cex_urg_ah('K766', 'R161')

htp_dx_dates_dx   <- htp_dx %>%
  left_join(taula_dates_dx, by = 'IdCas')

htp_dx_dates_dx  <- htp_dx_dates_dx %>%
  mutate(dif_d_dx = data_dx.x - data_dx.y)

htp_pre_dx <- htp_dx_dates_dx %>%
  filter(dif_d_dx < 0)

htp_post_dx <- htp_dx_dates_dx %>%
  filter(dif_d_dx > 0)

# Incorporar HTP a taula_sld
htp_pre_dx <- htp_pre_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

htp_pre_dx <- htp_pre_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_htp_pre = data_dx.x)

htp_post_dx <- htp_post_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

htp_post_dx <- htp_post_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_htp_post = data_dx.x)

taula_sld <- taula_sld %>%
  mutate(htp_pre_dx = if_else(IdCas %in% htp_pre_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(htp_pre_dx, by = 'IdCas')

taula_sld <- taula_sld %>%
  mutate(htp_post_dx = if_else(IdCas %in% htp_post_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(htp_post_dx, by = 'IdCas')

rm(htp_dx, htp_dx_dates_dx, htp_pre_dx, htp_post_dx)

# Transplantament hepàtic
tx_dx <- buscar_cap_cex_urg_ah('Z944')

tx_dx_dates_dx   <- tx_dx %>%
  left_join(taula_dates_dx, by = 'IdCas')

tx_dx_dates_dx  <- tx_dx_dates_dx %>%
  mutate(dif_d_dx = data_dx.x - data_dx.y)

tx_pre_dx <- tx_dx_dates_dx %>%
  filter(dif_d_dx < 0)

tx_post_dx <- tx_dx_dates_dx %>%
  filter(dif_d_dx > 0)

# Incorporar tx a taula_sld
tx_pre_dx <- tx_pre_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

tx_pre_dx <- tx_pre_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_tx_pre = data_dx.x)

tx_post_dx <- tx_post_dx %>%
  group_by(IdCas) %>%
  arrange(data_dx.x) %>%
  slice(1)

tx_post_dx <- tx_post_dx %>%
  select(IdCas, data_dx.x) %>%
  rename(data_tx_post = data_dx.x)

taula_sld <- taula_sld %>%
  mutate(tx_pre_dx = if_else(IdCas %in% tx_pre_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(tx_pre_dx, by = 'IdCas')

taula_sld <- taula_sld %>%
  mutate(tx_post_dx = if_else(IdCas %in% tx_post_dx$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(tx_post_dx, by = 'IdCas')

rm(tx_dx, tx_dx_dates_dx, tx_pre_dx, tx_post_dx)

# Defuncions
c_rca = get_inv_table("935_cohort_rca")
# No tenim disponible causa de mort
#c_reg_mort = get_inv_table("935_cohort_registre_mortalitat")

defuncions <- c_rca %>%
  filter(situacio_assegurat == 'Defunció') %>%
  select(IdCas, data_defuncio)

taula_sld <- taula_sld %>%
  mutate(defuncio = if_else(IdCas %in% defuncions$IdCas, 1, 0))

taula_sld <- taula_sld %>%
  left_join(defuncions, by = 'IdCas')

rm(defuncions)
rm(c_cap, c_cex, c_ah, c_urg, c_oh_imc, c_rca, c_sociodemo)

# Valors analítics basals i fib4


# Funcio per seleccionar un paràmetre, seleccionar el valor més proper al dx i
# obtenir un dataframe llest per enganxar a taula_sld

buscar_prova_lab <- function(codi, taula){

  prova <- taula %>%
    filter(lab_prova_c == codi) %>%
    filter(unitat_mesura == 'mg/dL')
  
  prova <- prova %>%
    left_join(taula_dates_dx, by = 'IdCas')
  prova <- prova %>%
    mutate(abs_dif_a_dx = abs(data_visita - data_dx))
  prova <- prova %>%
    group_by(IdCas) %>%
    arrange(abs_dif_a_dx) %>%
    slice(1) %>%
    ungroup()
  prova <- prova %>%
    select(IdCas, lab_resultat, data_visita) %>%
    rename(data_prova = data_visita) %>%
    rename(valor = lab_resultat)
  prova$valor <- gsub(",", ".", prova$valor) # reemplaçar coma per punt decimal
  prova$valor <- as.numeric(prova$valor)
  return(prova)
}

c_lab1 = get_inv_table("935_cohort_redics_lab1")

# Glucosa
glucosa <- buscar_prova_lab('Q32685', c_lab1)
glucosa <- glucosa %>%
  rename(data_glu = data_prova) %>%
  rename(glu = valor)

# Colesterol
colesterol <- buscar_prova_lab('Q13285', c_lab1)
colesterol <- colesterol %>%
  rename(data_col = data_prova) %>%
  rename(col = valor)

# Triglicèrids
triglicerids <- buscar_prova_lab('Q53585', c_lab1)
triglicerids <- triglicerids %>%
  rename(data_tgc = data_prova) %>%
  rename(tgc = valor)

# Colesterol HDL
colesterol_hdl <- buscar_prova_lab('Q12785', c_lab1)
colesterol_hdl <- colesterol_hdl %>%
  rename(data_hdl = data_prova) %>%
  rename(hdl = valor)

# Plaquetes
#pla_prova_lab1 <- c_lab1 %>%
#  filter(lab_prova_c == 'H20572') %>%
#  distinct(IdCas, .keep_all = TRUE)
#pla_prova_taula <- taula_sld %>%
#  filter(!is.na(pla))
  
pla <- pla %>%
  filter(lab_prova_c == 'H12172')
pla$lab_resultat <- gsub(",", ".", pla$lab_resultat)

pla$lab_resultat <- as.numeric(pla$lab_resultat)
pla <- pla %>% # excloem errors plaquetes
  filter(lab_resultat > 10 & lab_resultat < 1000)

pla <- pla %>%
  left_join(taula_dates_dx, by = 'IdCas')

pla <- pla %>%
  mutate(abs_dif_a_dx = abs(data_visita - data_dx))
pla <- pla %>%
  group_by(IdCas) %>%
  arrange(abs_dif_a_dx) %>%
  slice(1) %>%
  ungroup()
pla <- pla %>%
  select(IdCas, lab_resultat, data_visita) %>%
  rename(data_pla = data_visita) %>%
  rename(pla = lab_resultat)


# Hemoglobina glicada
hb_glicada <- buscar_prova_lab('Q32036', c_lab1)
hb_glicada <- hb_glicada %>%
  rename(data_hba1c = data_prova) %>%
  rename(hba1c = valor)

rm(c_lab1)
c_lab2 = get_inv_table("935_cohort_redics_lab2")

# Bilirrubina
bilirrubina <- buscar_prova_lab('Q08585', c_lab2)
bilirrubina <- bilirrubina %>%
  rename(data_bili = data_prova) %>%
  rename(bili = valor)

# Albúmina
albumina <- buscar_prova_lab('Q02485', c_lab2)
albumina <- albumina %>%
  rename(data_alb = data_prova) %>%
  rename(alb = valor)

# INR
inr <- buscar_prova_lab('C02266', c_lab2)
inr <- inr %>%
  rename(data_inr = data_prova) %>%
  rename(inr = valor)

rm(c_lab2)
c_lab3 = get_inv_table("935_cohort_redics_lab3")

# AST
ast <- buscar_prova_lab('Q07585', c_lab3)
ast <- ast %>%
  rename(data_ast = data_prova) %>%
  rename(ast = valor)

# ALT
alt <- buscar_prova_lab('Q02185', c_lab3)
alt <- alt %>%
  rename(data_alt = data_prova) %>%
  rename(alt = valor)

# GGT
ggt <- buscar_prova_lab('Q31585', c_lab3)
ggt <- ggt %>%
  rename(data_ggt = data_prova) %>%
  rename(ggt = valor)

rm(c_lab3)

#c_lab4 = get_inv_table("935_cohort_redics_lab4")

## Creatinina
#creatinina <- buscar_prova_lab('W71885', c_lab4)
#creatinina <- creatinina %>%
#  rename(data_creat = data_prova) %>%
#  rename(creat = valor)
## Sodi
#sodi <- buscar_prova_lab('W71885', c_lab4)
#creatinina <- creatinina %>%
#  rename(data_creat = data_prova) %>%
#  rename(creat = valor)

# Fusionar parametres

taula_sld <- taula_sld %>%
  left_join(hb_glicada, by = 'IdCas') %>%
  left_join(glucosa, by = 'IdCas') %>%
  left_join(colesterol, by = 'IdCas') %>%
  left_join(colesterol_hdl, by = 'IdCas') %>%
  left_join(triglicerids, by = 'IdCas') %>%
  left_join(ast, by = 'IdCas') %>%
  left_join(alt, by = 'IdCas') %>%
  left_join(ggt, by = 'IdCas') %>%
  left_join(plaquetes, by = 'IdCas') %>%
  left_join(albumina, by = 'IdCas') %>%
  left_join(inr, by = 'IdCas') %>%
  left_join(bilirrubina, by = 'IdCas')

#taula_sld <- taula_sld %>%
#  left_join(pla, by = 'IdCas')
#taula_sld <- taula_sld %>%
#  select(-pla.x, -data_pla.x) %>%
#  rename(pla = pla.y, data_pla = data_pla.y)
#taula_sld <- taula_sld %>%
#  relocate(77, 78, .after = 60)
#rm(pla)
rm(hb_glicada, glucosa, colesterol, colesterol_hdl, triglicerids, ast,
   alt, ggt, pla, albumina, inr, bilirrubina)

# Calcular fib4

# Primer edat a la data del dx

taula_sld <- taula_sld %>%
  mutate(across(c(data_dx, data_dx_cap, data_dx_cex, data_oh, data_talla, data_pes, 
                   data_imc, data_ob, data_dm2, data_hta, data_dlp, data_defuncio, 
                   data_hba1c, data_glu, data_col, data_hdl, data_tgc, data_ast, 
                   data_alt, data_ggt, data_alb, data_inr, data_bili), 
                ~ as.POSIXct(.)))
taula_sld$ANY_NAIX <- as.POSIXct(paste0(taula_sld$ANY_NAIX, "-06-30"))
taula_sld <- taula_sld %>%
  mutate(edat = data_dx - ANY_NAIX) %>%
  mutate(edat = round(as.numeric(edat)/365.25))

taula_sld <- taula_sld %>%
  mutate(fib4 = if_else(
    !is.na(edat) & !is.na(ast) & !is.na(pla) & !is.na(alt), 
    (edat*ast)/(pla*sqrt(alt)), 
    NA_real_
    ))

# Neteja formats i errors
taula_sld <- taula_sld %>%
  mutate(classificacio = factor(classificacio, 
                                levels = c(1, 2, 3), 
                                labels = c("MASLD", "METALD", "ALD")))
taula_sld$classificacio <- as.factor(taula_sld$classificacio)
taula_sld$diagnostic <- as.factor(taula_sld$diagnostic)

## Englobar nacionalitats segons regions globals
taula_sld$nacionalitat <- as.factor(taula_sld$nacionalitat)
taula_sld$nacionalitat_c <- as.numeric(taula_sld$nacionalitat_c)
africa_sub <- c(204, 24, 854, 108, 120, 132, 140, 178, 180, 384, 232, 231, 266,
                270, 288, 324, 624, 226, 404, 430, 450, 454, 466, 480, 508, 562,
                566, 638, 646, 686, 694, 710, 834, 768, 148, 800) # 1
america_llatina <- c(32, 84, 68, 76, 170, 188, 192, 212, 214, 218, 320, 328, 332,
                     340, 388, 484, 558, 591, 600, 604, 222, 740, 858, 862, 152) # 2
america_nord <- c(44, 124, 840)
asia_central <- c(51, 31, 268, 398, 417, 496, 860)
# asia_est 156
asia_pacific <- c(410, 408, 392)
asia_sud <- c(50, 356, 524, 586, 144, 764)
asia_sud_est <- c(116, 608, 360, 418, 458, 104, 702, 704)
australasia <- c(36, 162, 242, 540, 554)
europa_est <- c(112, 100, 643, 498, 804)
europa_oest <- c(8, 276, 20, 40, 56, 70, 191, 208, 703, 705, 724, 233, 246, 250,
                 300, 348, 372, 352, 380, 428, 434, 440, 442, 807, 470, 499, 578,
                 528, 616, 620, 826, 642, 688, 752, 756, 203, 196)
mena <- c(4, 12, 682, 818, 887, 364, 368, 376, 400, 414, 422, 504, 478, 275, 732,
          760, 706, 729, 788, 792)

taula_sld <- taula_sld %>%
  mutate(regio_global = case_when(
    nacionalitat_c %in% africa_sub ~ "Àfrica subsahariana",
    nacionalitat_c %in% america_llatina ~ "Amèrica llatina",
    nacionalitat_c %in% america_nord ~ "Amèrica del nord",
    nacionalitat_c %in% asia_central ~ "Àsia central",
    nacionalitat_c %in% asia_pacific ~ "Àsia pacífic",
    nacionalitat_c %in% asia_sud ~ "Àsia sud", 
    nacionalitat_c == 156 ~ "Àsia est",
    nacionalitat_c %in% asia_sud_est ~ "Àsia sud-est",
    nacionalitat_c %in% australasia ~ "Australàsia", 
    nacionalitat_c %in% europa_est ~ "Europa est", 
    nacionalitat_c %in% europa_oest ~ "Europa oest",
    nacionalitat_c %in% mena ~ "MENA"
  ))

taula_sld$regio_global <- as.factor(taula_sld$regio_global)

# Excloem errors AST ALT  < 1500 i > 10

taula_sld <- taula_sld %>%
  mutate(ast = case_when(
    ast > 1500 ~ NA_real_,
    ast < 10 ~ NA_real_,
    TRUE ~ ast #mantenir els valors que no compleixen cap condició

  ))
taula_sld <- taula_sld %>%
  mutate(alt = case_when(
    alt > 1500 ~ NA_real_,
    alt < 10 ~ NA_real_,
    TRUE ~ alt #mantenir els valors que no compleixen cap condició
    
  ))

taula_sld <- taula_sld %>%
  mutate(nivell_socio_economic_c = factor(nivell_socio_economic_c, 
                                levels = c(1, 2, 3, 4), 
                                labels = c("Exempts", "< 18000", "18001-100000", 
                                           "> 100000")))
write.csv(taula_sld, file = "taula_sld.csv")


