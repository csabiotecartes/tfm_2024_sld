library(odbc)
#install.packages("dplyr")
library(dplyr)

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

# Llegir les taules

# Tabac
tabac = get_inv_table("935_redics_tabac")

# Atenció hospitalaria aguts
ah = get_inv_table("935_cmbd_ah")

# Consultes externes hospitalaries
cex = get_inv_table("935_cmbd_aea")

# Urgències
urg = get_inv_table("935_cmbd_urgencies") 

# Sociodemogràfiques
sociodemo = get_inv_table("935_rca_talloficial")

# ECap
cap = get_inv_table("935_redics_problemessalut")

# Laboratori
lab = get_inv_table("935_redics_lab")

# Laboratori ALT AST GGT
lab_transas = get_inv_table("935_redics_lab_astaltggt")

# Laboratori bili alb inr
lab_hepato = get_inv_table("935_redics_lab_bilalbinr")

# IMC
oh_imc = get_inv_table("935_redics_seguimentusuari")

# Prescripcions farmacs
farmacs = get_inv_table("935_farmacia_prescrita")

# Pacients únics cap i cex
p_cap_cex = get_inv_table("935_aea_problemessalut_n_pacients_unics")


# Selecció de la cohort

## Grup MASLD

#### Pacients amb algun dx de MASLD
##### Info CAPs
dx_cap <- cap %>%
  filter(problema_salut_c == 'K7581' | problema_salut_c == 'K760')
dx_cap <- dx_cap %>%
  select(1)
##### Info Consultes externes hospitalaries
dx_cex <- cex %>%
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
dx_cex <- dx_cex %>%
  select(1)

masld_dx <- rbind(dx_cap, dx_cex)
  

rm(dx_cap, dx_cex) # esborro df intermitjos
##### Eliminar duplicats

masld_dx <- masld_dx %>%
  distinct(IdCas, .keep_all = TRUE)

##### Em quedo amb els majors d'edat
edat <- sociodemo %>%
  select(IdCas, ANY_NAIX)

masld_dx <- masld_dx %>%
  inner_join(edat, by = "IdCas")

masld_dx <- masld_dx %>%
  filter(ANY_NAIX <=2000)
masld_dx <- masld_dx %>%
  select(1)

#### Pacients amb sospita de MASLD
##### Info per calcular index HSI
###### Laboratori ALT AST
lab_astalt <- lab_transas %>%
  filter(lab_prova_c == 'Q07585' | lab_prova_c == 'Q02185') %>%
  filter(unitat_mesura == 'U/L')

####### Unificar noms
lab_noms <- lab_astalt %>%
  filter(!is.na(lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ASPARTAT AMINOTRANSFERASA-SÈRUM', 'AST', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ASPARTAT AMINOTRANSFERASA (SÈRUM); C.CAT', 'AST', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ASPARTAT AMINOTRANSFERASA; C.CAT (sèrum)', 'AST', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ASPARTAT-AMINOTRANSFERASA; C. CAT (sèrum)', 'AST', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ALANINA AMINOTRANSFERASA-SÈRUM', 'ALT', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ALANINA AMINOTRANSFERASA (SÈRUM); C.CAT.', 'ALT', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ALANINA AMINOTRANSFERASA; C.CAT. (sèrum)', 'ALT', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ALANINA-AMINOTRANSFERASA; C.CAT. (sèrum)', 'ALT', 
                            lab_prova)) %>%
  mutate(lab_prova = ifelse(lab_prova == 'ALANINA-AMINOTRANSFERASA; C. CAT. (sèrum)', 'ALT', 
                            lab_prova))

####### Pivotar les dades
#install.packages("tidyr")
library(tidyr)
lab_pivot <- lab_noms %>% 
  pivot_wider(
    names_from = lab_prova,
    values_from = lab_resultat, 
    id_cols = c(IdCas, data_visita) # em quedo em els valors de ALT/AST obtinguts el mateix dia
  )
rm(lab_astalt, lab_noms)

####### Eliminar els valors NULL i les llistes dins de cada cel·la
lab_pivot <- lab_pivot %>%
  filter(AST != 'NULL') %>%
  filter(ALT != 'NULL')

lab_pivot$ALT <- sapply(lab_pivot$ALT, '[', 1) 
lab_pivot$AST <- sapply(lab_pivot$AST, '[', 1)


###### Seleccionar BMIs
####### IMC
imc <- oh_imc %>%
  filter(VU_COD_VS == 'TT103')
imc <- imc %>%
  filter(VU_VAL > 0)
imc <- imc %>%
  select(-VU_COD_VS, -VS_DES, -any_referencia)

####### IMC calculat

######## Talla
talla <- oh_imc %>%
  filter(VU_COD_VS == 'TT101')


######### Excloc els valors no fiables o registres de nens (>100cm)
talla <- talla %>%
  filter(VU_VAL > 100)
######### Em quedo amb el primer registre disponible de talla (2018-2024)
talla_u <- talla %>%
  arrange(IdCas) %>%
  group_by(IdCas) %>%
  arrange(VU_DAT_ACT) %>%
  slice(1) %>%
  ungroup()

######## Pes
pes <- oh_imc %>%
  filter(VU_COD_VS == 'TT102')

######### Excloc els valors no fiables o registres de nens (>30kg)
pes <- pes %>%
  filter(VU_VAL > 30)

######### Em quedo amb el primer registre disponible de pes (2018-2024)
#pes_u <- pes %>%
#  arrange(IdCas) %>%
#  group_by(IdCas) %>%
#  arrange(VU_DAT_ACT) %>%
#  slice(1) %>%
#  ungroup()

######## Fusionar per IdCas
imc_calculat <- talla_u %>% inner_join(pes, by = "IdCas")

imc_calculat <- imc_calculat  %>%
  mutate(imc_c = VU_VAL.y/(VU_VAL.x/100)^2) %>% # calcul IMC
  select(IdCas, VU_DAT_ACT.y, imc_c)


rm(pes, talla, talla_u) # esborro dataframes que ja no em calen

####### IMCs totals 

imc <- imc %>%
  rename(data_imc = VU_DAT_ACT, imc = VU_VAL)

imc_calculat <- imc_calculat %>%
  rename(data_imc = VU_DAT_ACT.y, imc = imc_c)

imc_t <- rbind(imc, imc_calculat)


imc_t <- imc_t %>% # esborro mesures repetides
  group_by(IdCas) %>%
  distinct(data_imc, .keep_all = TRUE) 

rm(imc, imc_calculat) # esborro df que no em calen

#rm(oh_imc)



###### Dones

dones <- sociodemo %>%
  filter(sexe == 1)
dones <- dones %>%
  select(IdCas)

###### Diabètics
####### Dx DM2
######## Info CAPs

#install.packages("stringr")
library(stringr)
dm_cap <- cap %>%
  filter(str_detect(problema_salut_c, 'E11'))

dm_cap <- dm_cap %>%
  select(IdCas)

######## Info Consultes externes hospitalaries
dm_cex <- cex %>%
  filter(str_detect(dx_pral_c, 'E11') | str_detect(dx_secundari_1_c , 'E11') |
           str_detect(dx_secundari_2_c, 'E11') | str_detect(dx_secundari_3_c, 'E11') |
           str_detect(dx_secundari_4_c, 'E11') | str_detect(dx_secundari_5_c, 'E11') |
           str_detect(dx_secundari_6_c, 'E11') | str_detect(dx_secundari_7_c, 'E11') |
           str_detect(dx_secundari_8_c, 'E11') | str_detect(dx_secundari_9_c, 'E11') |
           str_detect(dx_secundari_10_c, 'E11') | str_detect(dx_secundari_11_c, 'E11') |
           str_detect(dx_secundari_12_c, 'E11') | str_detect(dx_secundari_13_c, 'E11') |
           str_detect(dx_secundari_14_c, 'E11') )
dm_cex <- dm_cex %>%
  select(IdCas)
          
dm_dx <- rbind(dm_cap, dm_cex)
dm_dx <- dm_dx %>%
  distinct(IdCas, .keep_all = TRUE)
rm(dm_cap, dm_cex)

####### Fàrmacs antidiabètics # 717645 px vs. 277204 amb dx! NO HO UILITZAREM 

#farma_dm <- farmacs %>%
#  filter(atc3_c == 'A10')
  
#rm(farmacs)
#farma_dm <- farma_dm %>%
#  distinct(IdCas, .keep_all = TRUE) 
#rm(farma_dm)
####### Hemoglobina glicada elevada
lab_hbac1 <- lab %>%
  filter(lab_prova_c == 'Q32036') %>%
  filter(unitat_mesura == '%')

#rm(lab)

lab_hbac1_6.5 <- lab_hbac1 %>%
  filter(lab_resultat >= 6.5)

rm(lab_hbac1)

lab_hbac1_6.5 <- lab_hbac1_6.5 %>%
  select(IdCas)
lab_hbac1_6.5 <- lab_hbac1_6.5 %>%
  distinct(IdCas, .keep_all = TRUE)

###### Diabètics totals
dm_t <- rbind(dm_dx, lab_hbac1_6.5)
dm_t <- dm_t %>%
  distinct(IdCas, .keep_all = TRUE)

rm(dm_dx, lab_hbac1_6.5)


##### Calcular HSI
lab_imc <- merge(imc_t, lab_pivot, by = "IdCas")
#rm(lab_pivot)

lab_imc <- lab_imc %>%
  filter(!is.na(imc))

hsi <- lab_imc %>%
  mutate(dif_lab_imc = data_imc - data_visita)

rm(lab_imc)


hsi$ALT <- gsub(",", ".", hsi$ALT) # reemplaçar coma per punt decimal
hsi$ALT <- as.numeric(hsi$ALT)

hsi$AST <- gsub(",", ".", hsi$AST) # reemplaçar coma per punt decimal
hsi$AST <- as.numeric(hsi$AST)

hsi <- hsi %>%
  filter(!is.na(ALT), !is.na(AST), ALT > 1, AST > 1)

hsi <- hsi %>%
  rename(data_analitica = data_visita)

# Si son diabetics afegim un 1 a una nova columna, el mateix si son dones
hsi <- hsi %>%
  mutate(dona = if_else(IdCas %in% dones$IdCas, 1, 0)) %>%
  mutate(dm = if_else(IdCas %in% dm_t$IdCas, 1, 0))

# Assegurar que el temps entre la A/S i el BMI es <6m
hsi$dif_lab_imc <- as.numeric(hsi$dif_lab_imc, units = "days")
hsi <- hsi %>%
  filter(abs(dif_lab_imc) < 183)

# Calcul formula
hsi <- hsi %>%
  mutate(hsi_resultat = 8 * ALT/AST + imc + if_else(dm == 1, 2, 0) + 
           if_else(dona == 1, 2, 0))


hsi_unics <- hsi %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)
  


##### Factors de risc CV

###### Ja tenim seleccionats els px diabetics

###### Obesitat
####### Dx obesitat
######## CAPs
imc_cap <- cap %>%
  filter(str_detect(problema_salut_c, 'Z68')) %>%
  select(IdCas)

ob_cap <- cap %>% 
  filter(str_detect(problema_salut_c, 'E66')) %>%
  select(IdCas)
######## CEXs
imc_cex <- cex %>%
  filter(str_detect(dx_pral_c, 'Z68') | str_detect(dx_secundari_1_c, 'Z68') |
           str_detect(dx_secundari_2_c, 'Z68') | str_detect(dx_secundari_3_c, 'Z68')|
           str_detect(dx_secundari_4_c, 'Z68') | str_detect(dx_secundari_5_c, 'Z68')|
           str_detect(dx_secundari_6_c, 'Z68') | str_detect(dx_secundari_7_c, 'Z68')|
           str_detect(dx_secundari_8_c, 'Z68') | str_detect(dx_secundari_9_c, 'Z68')|
           str_detect(dx_secundari_10_c, 'Z68') | str_detect(dx_secundari_11_c, 'Z68')|
           str_detect(dx_secundari_12_c, 'Z68') | str_detect(dx_secundari_13_c, 'Z68')|
           str_detect(dx_secundari_14_c, 'Z68')) 

# codis <Z6825 i >Z6845 que queden presents
c_eliminar <- c("Z681", "Z6820", "Z6822", "Z6854") # codis <Z6825 que queden presents
cols_revisar <- c("dx_pral_c", "dx_secundari_1_c","dx_secundari_2_c",
                  "dx_secundari_3_c", "dx_secundari_4_c", "dx_secundari_5_c", 
                  "dx_secundari_6_c", "dx_secundari_7_c", "dx_secundari_8_c",
                  "dx_secundari_9_c", "dx_secundari_10_c", "dx_secundari_11_c", 
                  "dx_secundari_12_c", "dx_secundari_13_c", "dx_secundari_14_c")
imc_cex <- imc_cex[!apply(imc_cex[cols_revisar], 1, 
                          function(fila) any(fila %in% c_eliminar)), ]
imc_cex <- imc_cex %>%
  select(IdCas)

ob_cex <- cex %>%
  filter(str_detect(dx_pral_c, 'E66') | str_detect(dx_secundari_1_c, 'E66') |
           str_detect(dx_secundari_2_c, 'E66') | str_detect(dx_secundari_3_c, 'E66')|
           str_detect(dx_secundari_4_c, 'E66') | str_detect(dx_secundari_5_c, 'E66')|
           str_detect(dx_secundari_6_c, 'E66') | str_detect(dx_secundari_7_c, 'E66')|
           str_detect(dx_secundari_8_c, 'E66') | str_detect(dx_secundari_9_c, 'E66')|
           str_detect(dx_secundari_10_c, 'E66') | str_detect(dx_secundari_11_c, 'E66')|
           str_detect(dx_secundari_12_c, 'E66') | str_detect(dx_secundari_13_c, 'E66')|
           str_detect(dx_secundari_14_c, 'E66')) %>%
  select(IdCas)

ob_dx <- rbind(imc_cap, ob_cap, imc_cex, ob_cex)

ob_dx <- ob_dx %>%
  distinct(IdCas, .keep_all = TRUE)

rm(imc_cap, ob_cap, imc_cex, ob_cex)

####### IMC > 25
imc_25 <- imc_t %>%
  filter(imc > 25)

imc_25 <- imc_25 %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)

ob_t <- rbind(imc_25, ob_dx)

ob_t <- ob_t %>%
  distinct(IdCas, .keep_all = TRUE)

rm(ob_dx, imc_25)
rm(imc_t)

###### HTA
####### CAPs
hta_cap <- cap %>%
  filter(problema_salut_c == 'I10')

hta_cap <- hta_cap %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)

####### CEXs
hta_cex <- cex %>%
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
           dx_secundari_14_c == 'I10')
hta_cex <- hta_cex %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)

hta_t <- rbind(hta_cap, hta_cex)

hta_t <- hta_t %>%
  distinct(IdCas, .keep_all = TRUE)

rm(hta_cap, hta_cex)
###### DLP
####### Dx DLP

######## CAPs
dlp_cap <- cap %>%
  filter(str_detect(problema_salut_c, 'E78'))
dlp_cap <- dlp_cap %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)

######## CEXs
dlp_cex <- cex %>%
  filter(str_detect(dx_pral_c, 'E78') | str_detect(dx_secundari_1_c , 'E78') |
           str_detect(dx_secundari_2_c, 'E78') | str_detect(dx_secundari_3_c, 'E78') |
           str_detect(dx_secundari_4_c, 'E78') | str_detect(dx_secundari_5_c, 'E78') |
           str_detect(dx_secundari_6_c, 'E78') | str_detect(dx_secundari_7_c, 'E78') |
           str_detect(dx_secundari_8_c, 'E78') | str_detect(dx_secundari_9_c, 'E78') |
           str_detect(dx_secundari_10_c, 'E78') | str_detect(dx_secundari_11_c, 'E78') |
           str_detect(dx_secundari_12_c, 'E78') | str_detect(dx_secundari_13_c, 'E78') |
           str_detect(dx_secundari_14_c, 'E78') )
dlp_cex <- dlp_cex %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)

dlp_dx <- rbind(dlp_cap, dlp_cex)

dlp_dx <- dlp_dx %>%
  distinct(IdCas, .keep_all = TRUE)

rm(dlp_cap, dlp_cex)

####### TG >= 150

tg_hdl <- lab %>%
  filter(lab_prova_c == 'Q53585' | lab_prova_c == 'Q12785')
rm(lab)

tg <- tg_hdl %>%
  filter(lab_prova_c == 'Q53585')
tg <- tg %>% # filtrem unitat de mesura a mg/dL sense importar majus/minuscules
  filter(grepl('mg/dL', unitat_mesura, ignore.case = TRUE))
tg$lab_resultat <- gsub(",", ".", tg$lab_resultat) # reemplacar coma per punt decimal
tg$lab_resultat <- as.numeric(tg$lab_resultat)
tg_alterat <- tg %>%
  filter(lab_resultat >= 150)
tg_alterat <- tg_alterat %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)
rm(tg)

####### HDL <= 50 D HDL <= 39 H

hdl <- tg_hdl %>%
  filter(lab_prova_c == 'Q12785') 
hdl <- hdl %>% # filtrem unitat de mesura a mg/dL sense importar majus/minuscules
  filter(grepl('mg/dL', unitat_mesura, ignore.case = TRUE))
hdl$lab_resultat <- gsub(",", ".", hdl$lab_resultat) # reemplacar coma per punt decimal
hdl$lab_resultat <- as.numeric(hdl$lab_resultat)
hdl_alterat <- hdl %>%
  filter(lab_resultat <= if_else(IdCas %in% dones$IdCas, 50, 39))
hdl_alterat <- hdl_alterat %>%
  select(IdCas) %>%
  distinct(IdCas, .keep_all = TRUE)
rm(hdl)

dlp_lab <- rbind(tg_alterat, hdl_alterat)
dlp_lab <- dlp_lab %>%
  distinct(IdCas, .keep_all = TRUE)


rm(tg_hdl, tg_alterat, hdl_alterat)

dlp_t <- rbind(dlp_dx, dlp_lab)

dlp_t <- dlp_t %>%
  distinct(IdCas, .keep_all = TRUE)

rm(dlp_dx, dlp_lab)

### Taula combinatoria - Pacients amb sospita de MASLD

hsi_alterat <- hsi %>%
  filter(hsi_resultat > 36)

hsi_alterat <- hsi_alterat %>%
  distinct(IdCas, .keep_all = TRUE)

# HE D'ARRIBAR FINS AQUI UN ALTRE COP I TINDRE LES DATES DEL HSI
hsi_alterat <- hsi_alterat %>%
  arrange(IdCas, data_analitica) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup()


masld_sospita <- hsi_alterat %>%
  mutate(ob = if_else(IdCas %in% ob_t$IdCas, 1, 0)) %>%
  mutate(hta = if_else(IdCas %in% hta_t$IdCas, 1, 0)) %>%
  mutate(dlp = if_else(IdCas %in% dlp_t$IdCas, 1, 0))


masld_sospita <- masld_sospita %>%
  mutate(masld_sosp = if_else((dm == 1 | ob == 1) | (hta == 1 & dlp == 1), 1, 0))

masld_s <- masld_sospita %>%
  filter(masld_sosp == 1) %>%
  select(IdCas) 

write.csv(hsi_alterat, file = "hsi_alterat.csv")

rm(masld_sospita, dlp_t, dm_t, hsi_unics, hsi, hta_t, ob_t)


## Grup ALD

### Dx Hepatopatia alcoholica
#### Info CAPs
ald_cap <- cap %>%
  filter(str_detect(problema_salut_c, 'K70'))

ald_cap <- ald_cap %>%
  select(IdCas)


##### Info Consultes externes hospitalaries
ald_cex <- cex %>%
  filter(str_detect(dx_pral_c, 'K70') | str_detect(dx_secundari_1_c , 'K70') |
           str_detect(dx_secundari_2_c, 'K70') | str_detect(dx_secundari_3_c, 'K70') |
           str_detect(dx_secundari_4_c, 'K70') | str_detect(dx_secundari_5_c, 'K70') |
           str_detect(dx_secundari_6_c, 'K70') | str_detect(dx_secundari_7_c, 'K70') |
           str_detect(dx_secundari_8_c, 'K70') | str_detect(dx_secundari_9_c, 'K70') |
           str_detect(dx_secundari_10_c, 'K70') | str_detect(dx_secundari_11_c, 'K70') |
           str_detect(dx_secundari_12_c, 'K70') | str_detect(dx_secundari_13_c, 'K70') |
           str_detect(dx_secundari_14_c, 'K70') )
ald_cex <- ald_cex %>%
  select(1)

ald_dx <- rbind(ald_cap, ald_cex)

ald_dx <- ald_dx %>%
  distinct(IdCas, .keep_all = TRUE)

rm(ald_cap, ald_cex)

### Sospita ALD

#### Dx Abús/dependencia alcohol
##### Info CAPs
abus_cap <- cap %>%
  filter(str_detect(problema_salut_c, 'F101') | str_detect(problema_salut_c, 'F102'))
abus_cap <- abus_cap %>%
  select(IdCas, data_problema_salut) %>%
  rename(data_dx = data_problema_salut)

##### Info Consultes externes hospitalaries
abus_cex <- cex %>%
  filter(str_detect(dx_pral_c, 'F101') | str_detect(dx_secundari_1_c , 'F101') |
           str_detect(dx_secundari_2_c, 'F101') | str_detect(dx_secundari_3_c, 'F101') |
           str_detect(dx_secundari_4_c, 'F101') | str_detect(dx_secundari_5_c, 'F101') |
           str_detect(dx_secundari_6_c, 'F101') | str_detect(dx_secundari_7_c, 'F101') |
           str_detect(dx_secundari_8_c, 'F101') | str_detect(dx_secundari_9_c, 'F101') |
           str_detect(dx_secundari_10_c, 'F101') | str_detect(dx_secundari_11_c, 'F101') |
           str_detect(dx_secundari_12_c, 'F101') | str_detect(dx_secundari_13_c, 'F101') |
           str_detect(dx_secundari_14_c, 'F101') |
           str_detect(dx_pral_c, 'F102') | str_detect(dx_secundari_1_c , 'F102') |
           str_detect(dx_secundari_2_c, 'F102') | str_detect(dx_secundari_3_c, 'F102') |
           str_detect(dx_secundari_4_c, 'F102') | str_detect(dx_secundari_5_c, 'F102') |
           str_detect(dx_secundari_6_c, 'F102') | str_detect(dx_secundari_7_c, 'F102') |
           str_detect(dx_secundari_8_c, 'F102') | str_detect(dx_secundari_9_c, 'F102') |
           str_detect(dx_secundari_10_c, 'F102') | str_detect(dx_secundari_11_c, 'F102') |
           str_detect(dx_secundari_12_c, 'F102') | str_detect(dx_secundari_13_c, 'F102') |
           str_detect(dx_secundari_14_c, 'F102') )
abus_cex <- abus_cex %>%
  select(IdCas, data_visita) %>%
  rename(data_dx = data_visita)

abus_dx <- rbind(abus_cap, abus_cex)
rm(abus_cap, abus_cex)

abus_dx <- abus_dx %>%
  arrange(IdCas, data_dx) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup() 

#### OH
oh <- oh_imc %>%
  filter(VU_COD_VS == 'AUDIT'| VU_COD_VS == 'ALSET' | VU_COD_VS == 'ALHAB' |
           VU_COD_VS == 'ALDIA')

oh_unics <- oh %>% # recompte pacients amb info consum OH disponible
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)


oh_audit <- oh %>%
  filter(VU_COD_VS == 'AUDIT')

oh_set <- oh %>%
  filter(VU_COD_VS == 'ALSET' | VU_COD_VS == 'ALHAB' | VU_COD_VS == 'ALDIA')

oh_audit_risc <- oh_audit %>%
  filter(VU_VAL >= 8)
oh_audit_risc <- oh_audit_risc %>%
  arrange(IdCas, VU_DAT_ACT) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup() %>%
  select(IdCas, VU_DAT_ACT)%>%
  rename(data_dx = VU_DAT_ACT)

oh_set_risc <- oh_set %>%
  filter(VU_VAL >= if_else(IdCas %in% dones$IdCas, 35, 42))

oh_set_risc <- oh_set_risc %>%
  arrange(IdCas, VU_DAT_ACT) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup() %>%
  select(IdCas, VU_DAT_ACT) %>%
  rename(data_dx = VU_DAT_ACT)

rm(oh_audit, oh_set)

oh_risc <- rbind(abus_dx, oh_audit_risc, oh_set_risc)
oh_risc <- oh_risc %>%
  arrange(IdCas, data_dx) %>%
  group_by(IdCas) %>%
  slice(1) %>%
  ungroup() 

write.csv(oh_risc, file = "oh_risc.csv")


### Criteris d'hepatopatia cronica
#### Estigmes fisics (rinofima) o descompensacions
estig_desc_cap <- cap %>%
  filter(problema_salut_c == 'L711' | problema_salut_c == 'K766' | 
           problema_salut_c == 'K4682'| problema_salut_c == 'R161' | 
           problema_salut_c == 'R18' | problema_salut_c == 'I85' | 
           problema_salut_c == 'Z8505' | problema_salut_c == 'K921' | 
           problema_salut_c == 'K922' | problema_salut_c == 'K652' | 
           problema_salut_c == 'K767')
estig_desc_cap <- estig_desc_cap %>%
  select(IdCas)

estig_desc_cex <- cex %>%
  filter(dx_pral_c == 'L711' | 
           dx_secundari_1_c == 'L711' | 
           dx_secundari_2_c == 'L711' | 
           dx_secundari_3_c == 'L711' | 
           dx_secundari_4_c == 'L711' | 
           dx_secundari_5_c == 'L711' | 
           dx_secundari_6_c == 'L711' | 
           dx_secundari_7_c == 'L711' | 
           dx_secundari_8_c == 'L711' | 
           dx_secundari_9_c == 'L711' |
           dx_secundari_10_c == 'L711' | 
           dx_secundari_11_c == 'L711' | 
           dx_secundari_12_c == 'L711' | 
           dx_secundari_13_c == 'L711' | 
           dx_secundari_14_c == 'L711' |
           dx_pral_c == 'K766' | 
           dx_secundari_1_c == 'K766' | 
           dx_secundari_2_c == 'K766' | 
           dx_secundari_3_c == 'K766' | 
           dx_secundari_4_c == 'K766' | 
           dx_secundari_5_c == 'K766' | 
           dx_secundari_6_c == 'K766' | 
           dx_secundari_7_c == 'K766' | 
           dx_secundari_8_c == 'K766' | 
           dx_secundari_9_c == 'K766' |
           dx_secundari_10_c == 'K766' | 
           dx_secundari_11_c == 'K766' | 
           dx_secundari_12_c == 'K766' | 
           dx_secundari_13_c == 'K766' | 
           dx_secundari_14_c == 'K766' |
           dx_pral_c == 'K4682' | 
           dx_secundari_1_c == 'K4682' | 
           dx_secundari_2_c == 'K4682' | 
           dx_secundari_3_c == 'K4682' | 
           dx_secundari_4_c == 'K4682' | 
           dx_secundari_5_c == 'K4682' | 
           dx_secundari_6_c == 'K4682' | 
           dx_secundari_7_c == 'K4682' | 
           dx_secundari_8_c == 'K4682' | 
           dx_secundari_9_c == 'K4682' |
           dx_secundari_10_c == 'K4682' | 
           dx_secundari_11_c == 'K4682' | 
           dx_secundari_12_c == 'K4682' | 
           dx_secundari_13_c == 'K4682' | 
           dx_secundari_14_c == 'K4682' |
           dx_pral_c == 'R161' | 
           dx_secundari_1_c == 'R161' | 
           dx_secundari_2_c == 'R161' | 
           dx_secundari_3_c == 'R161' | 
           dx_secundari_4_c == 'R161' | 
           dx_secundari_5_c == 'R161' | 
           dx_secundari_6_c == 'R161' | 
           dx_secundari_7_c == 'R161' | 
           dx_secundari_8_c == 'R161' | 
           dx_secundari_9_c == 'R161' |
           dx_secundari_10_c == 'R161' | 
           dx_secundari_11_c == 'R161' | 
           dx_secundari_12_c == 'R161' | 
           dx_secundari_13_c == 'R161' | 
           dx_secundari_14_c == 'R161' |
           dx_pral_c == 'R18' | 
           dx_secundari_1_c == 'R18' | 
           dx_secundari_2_c == 'R18' | 
           dx_secundari_3_c == 'R18' | 
           dx_secundari_4_c == 'R18' | 
           dx_secundari_5_c == 'R18' | 
           dx_secundari_6_c == 'R18' | 
           dx_secundari_7_c == 'R18' | 
           dx_secundari_8_c == 'R18' | 
           dx_secundari_9_c == 'R18' |
           dx_secundari_10_c == 'R18' | 
           dx_secundari_11_c == 'R18' | 
           dx_secundari_12_c == 'R18' | 
           dx_secundari_13_c == 'R18' | 
           dx_secundari_14_c == 'R18' |
           dx_pral_c == 'I85' | 
           dx_secundari_1_c == 'I85' | 
           dx_secundari_2_c == 'I85' | 
           dx_secundari_3_c == 'I85' | 
           dx_secundari_4_c == 'I85' | 
           dx_secundari_5_c == 'I85' | 
           dx_secundari_6_c == 'I85' | 
           dx_secundari_7_c == 'I85' | 
           dx_secundari_8_c == 'I85' | 
           dx_secundari_9_c == 'I85' |
           dx_secundari_10_c == 'I85' | 
           dx_secundari_11_c == 'I85' | 
           dx_secundari_12_c == 'I85' | 
           dx_secundari_13_c == 'I85' | 
           dx_secundari_14_c == 'I85' |
           dx_pral_c == 'Z8505' | 
           dx_secundari_1_c == 'Z8505' | 
           dx_secundari_2_c == 'Z8505' | 
           dx_secundari_3_c == 'Z8505' | 
           dx_secundari_4_c == 'Z8505' | 
           dx_secundari_5_c == 'Z8505' | 
           dx_secundari_6_c == 'Z8505' | 
           dx_secundari_7_c == 'Z8505' | 
           dx_secundari_8_c == 'Z8505' | 
           dx_secundari_9_c == 'Z8505' |
           dx_secundari_10_c == 'Z8505' | 
           dx_secundari_11_c == 'Z8505' | 
           dx_secundari_12_c == 'Z8505' | 
           dx_secundari_13_c == 'Z8505' | 
           dx_secundari_14_c == 'Z8505' |
           dx_pral_c == 'K921' | 
           dx_secundari_1_c == 'K921' | 
           dx_secundari_2_c == 'K921' | 
           dx_secundari_3_c == 'K921' | 
           dx_secundari_4_c == 'K921' | 
           dx_secundari_5_c == 'K921' | 
           dx_secundari_6_c == 'K921' | 
           dx_secundari_7_c == 'K921' | 
           dx_secundari_8_c == 'K921' | 
           dx_secundari_9_c == 'K921' |
           dx_secundari_10_c == 'K921' | 
           dx_secundari_11_c == 'K921' | 
           dx_secundari_12_c == 'K921' | 
           dx_secundari_13_c == 'K921' | 
           dx_secundari_14_c == 'K921' |
           dx_pral_c == 'K922' | 
           dx_secundari_1_c == 'K922' | 
           dx_secundari_2_c == 'K922' | 
           dx_secundari_3_c == 'K922' | 
           dx_secundari_4_c == 'K922' | 
           dx_secundari_5_c == 'K922' | 
           dx_secundari_6_c == 'K922' | 
           dx_secundari_7_c == 'K922' | 
           dx_secundari_8_c == 'K922' | 
           dx_secundari_9_c == 'K922' |
           dx_secundari_10_c == 'K922' | 
           dx_secundari_11_c == 'K922' | 
           dx_secundari_12_c == 'K922' | 
           dx_secundari_13_c == 'K922' | 
           dx_secundari_14_c == 'K922' |
           dx_pral_c == 'K652' | 
           dx_secundari_1_c == 'K652' | 
           dx_secundari_2_c == 'K652' | 
           dx_secundari_3_c == 'K652' | 
           dx_secundari_4_c == 'K652' | 
           dx_secundari_5_c == 'K652' | 
           dx_secundari_6_c == 'K652' | 
           dx_secundari_7_c == 'K652' | 
           dx_secundari_8_c == 'K652' | 
           dx_secundari_9_c == 'K652' |
           dx_secundari_10_c == 'K652' | 
           dx_secundari_11_c == 'K652' | 
           dx_secundari_12_c == 'K652' | 
           dx_secundari_13_c == 'K652' | 
           dx_secundari_14_c == 'K652' |
           dx_pral_c == 'K767' | 
           dx_secundari_1_c == 'K767' | 
           dx_secundari_2_c == 'K767' | 
           dx_secundari_3_c == 'K767' | 
           dx_secundari_4_c == 'K767' | 
           dx_secundari_5_c == 'K767' | 
           dx_secundari_6_c == 'K767' | 
           dx_secundari_7_c == 'K767' | 
           dx_secundari_8_c == 'K767' | 
           dx_secundari_9_c == 'K767' |
           dx_secundari_10_c == 'K767' | 
           dx_secundari_11_c == 'K767' | 
           dx_secundari_12_c == 'K767' | 
           dx_secundari_13_c == 'K767' | 
           dx_secundari_14_c == 'K767'
           )
estig_desc_cex <- estig_desc_cex %>%
  select(IdCas)
estig_desc <- rbind(estig_desc_cap, estig_desc_cex)
rm(estig_desc_cap, estig_desc_cex)
estig_desc <- estig_desc %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)

#### Alteracions analitiques
lab_bili <- lab_hepato %>%
  filter(lab_prova_c == 'Q08585')

lab_alb <- lab_hepato %>%
  filter(lab_prova_c == 'Q02485')

lab_inr <- lab_hepato %>%
  filter(lab_prova_c == 'C02266')

rm(lab_hepato)

lab_pla <- lab %>%
  filter(lab_prova_c == 'H20572')

rm(lab)

lab_ggt <- lab_transas %>%
  filter(lab_prova_c == 'Q31585') %>%
  filter(unitat_mesura == 'U/L')

rm(lab_transas)

# GGT > 100
lab_ggt$lab_resultat <- gsub(",", ".", lab_ggt$lab_resultat) 
lab_ggt$lab_resultat <- as.numeric(lab_ggt$lab_resultat)

lab_ggt <- lab_ggt %>%
  filter(!is.na(lab_resultat))

lab_ggt_alt <- lab_ggt %>%
  filter(lab_resultat > 100)
lab_ggt_alt <- lab_ggt_alt %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)

# Bili > 3
lab_bili$lab_resultat <- gsub(",", ".", lab_bili$lab_resultat) 
lab_bili$lab_resultat <- as.numeric(lab_bili$lab_resultat)

lab_bili <- lab_bili %>%
  filter(!is.na(lab_resultat))

lab_bili_alt <- lab_bili %>%
  filter(lab_resultat > 3) 

lab_bili_alt <- lab_bili_alt %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)

# AST/ALT > 1.5
lab_pivot$ALT <- gsub(",", ".", lab_pivot$ALT)
lab_pivot$AST <- gsub(",", ".", lab_pivot$AST)

lab_pivot$ALT <- as.numeric(lab_pivot$ALT)
lab_pivot$AST <- as.numeric(lab_pivot$AST)

lab_pivot <- lab_pivot %>%
  filter(!is.na(ALT) & !is.na(AST))

lab_pivot <- lab_pivot %>%
  mutate(ast_alt = AST/ALT)

lab_pivot_alt <- lab_pivot %>%
  filter(ast_alt > 1.5) 

lab_pivot_alt <- lab_pivot_alt %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)

# Plaquetes < 100
lab_pla$lab_resultat <- gsub(",", ".", lab_pla$lab_resultat)
lab_pla$lab_resultat <- as.numeric(lab_pla$lab_resultat)

lab_pla <- lab_pla %>%
  filter(!is.na(lab_resultat))

lab_pla_alt <- lab_pla %>%
  filter(lab_resultat < 100)

lab_pla_alt <- lab_pla_alt %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)
  
# Albumina < 3.5
lab_alb$lab_resultat <- gsub(",", ".", lab_alb$lab_resultat)
lab_alb$lab_resultat <- as.numeric(lab_alb$lab_resultat)

lab_alb <- lab_alb %>%
  filter(!is.na(lab_resultat))

lab_alb_alt <- lab_alb %>%
  filter(lab_resultat < 3.5)

lab_alb_alt <- lab_alb_alt %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)

# INR > 1.5
lab_inr$lab_resultat <- gsub(",", ".", lab_inr$lab_resultat)
lab_inr$lab_resultat <- as.numeric(lab_inr$lab_resultat)

lab_inr <- lab_inr %>%
  filter(!is.na(lab_resultat))

lab_inr_alt <- lab_inr %>%
  filter(lab_resultat > 1.5)

lab_inr_alt <- lab_inr_alt %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)

rm(lab_alb, lab_bili, lab_inr, lab_pla)
rm(lab_pivot, lab_ggt)

### Taula combinatoria - Sospita ALD
ald_sospita <- oh_risc %>%
  mutate(estig_desc = if_else(IdCas %in% estig_desc$IdCas, 1, 0)) %>%
  mutate(alter_analitica = if_else((IdCas %in% lab_ggt_alt$IdCas |
                                     IdCas %in% lab_pivot_alt$IdCas |
                                     IdCas %in% lab_bili_alt$IdCas |
                                     IdCas %in% lab_pla_alt$IdCas |
                                     IdCas %in% lab_alb_alt$IdCas |
                                     IdCas %in% lab_inr_alt$IdCas), 1, 0))
ald_s <- ald_sospita %>%
  filter(estig_desc == 1 | alter_analitica == 1)

ald_s <- ald_s %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)


rm(lab_ggt_alt, lab_bili_alt, lab_pivot_alt, lab_pla_alt, 
   lab_alb_alt, lab_inr_alt, estig_desc)  
rm(ald_sospita)






# Flowchart pacients
edat <- sociodemo %>%
  select(IdCas, ANY_NAIX) %>%
  distinct(IdCas, .keep_all = TRUE)


more_18 <- edat %>%
  filter(ANY_NAIX <= 2000)
less_18 <- edat %>%
  filter(ANY_NAIX > 2000)

p_cap_cex_edat <- p_cap_cex %>%
  filter(IdCas %in% edat$IdCas)

m_18_p <- p_cap_cex_edat %>%
  filter(IdCas %in% more_18$IdCas)

l_18_p <- p_cap_cex_edat %>%
  filter(IdCas %in% less_18$IdCas)

rm(p_cap_cex, p_cap_cex_edat, l_18_p, m_18_p)

# HSI i OH disponible
hsi_oh <- rbind(hsi_unics, oh_unics)

hsi_oh <- hsi_oh %>%
  distinct(IdCas, .keep_all = TRUE)

p_info_disp <- m_18_p %>%
  filter(IdCas %in% hsi_oh$IdCas)

# Excloure menors d'edat a tots els grups


masld_dx <- masld_dx %>%
  filter(IdCas %in% m_18_p$IdCas)
masld_s <- masld_s %>%
  filter(IdCas %in% m_18_p$IdCas)
ald_dx <- ald_dx %>%
  filter(IdCas %in% m_18_p$IdCas)
ald_s <- ald_s %>%
  filter(IdCas %in% m_18_p$IdCas)





################################################################################

# Criteris d'exclusió


# Dx d'exclusio
altres_hepatitis_cap <- cap %>%
  filter(problema_salut_c == 'B16' | problema_salut_c == 'B17' | 
           problema_salut_c == 'B18' | problema_salut_c == 'B19' |
           problema_salut_c == 'K754')
altres_hepatitis_cap <- altres_hepatitis_cap %>%
  select(IdCas)

altres_hepatiti_cex <- cex %>%
  filter(dx_pral_c == 'B16' | 
           dx_secundari_1_c == 'B16' | 
           dx_secundari_2_c == 'B16' | 
           dx_secundari_3_c == 'B16' | 
           dx_secundari_4_c == 'B16' | 
           dx_secundari_5_c == 'B16' | 
           dx_secundari_6_c == 'B16' | 
           dx_secundari_7_c == 'B16' | 
           dx_secundari_8_c == 'B16' | 
           dx_secundari_9_c == 'B16' |
           dx_secundari_10_c == 'B16' | 
           dx_secundari_11_c == 'B16' | 
           dx_secundari_12_c == 'B16' | 
           dx_secundari_13_c == 'B16' | 
           dx_secundari_14_c == 'B16' |
           dx_pral_c == 'B17' | 
           dx_secundari_1_c == 'B17' | 
           dx_secundari_2_c == 'B17' | 
           dx_secundari_3_c == 'B17' | 
           dx_secundari_4_c == 'B17' | 
           dx_secundari_5_c == 'B17' | 
           dx_secundari_6_c == 'B17' | 
           dx_secundari_7_c == 'B17' | 
           dx_secundari_8_c == 'B17' | 
           dx_secundari_9_c == 'B17' |
           dx_secundari_10_c == 'B17' | 
           dx_secundari_11_c == 'B17' | 
           dx_secundari_12_c == 'B17' | 
           dx_secundari_13_c == 'B17' | 
           dx_secundari_14_c == 'B17' |
           dx_pral_c == 'B18' | 
           dx_secundari_1_c == 'B18' | 
           dx_secundari_2_c == 'B18' | 
           dx_secundari_3_c == 'B18' | 
           dx_secundari_4_c == 'B18' | 
           dx_secundari_5_c == 'B18' | 
           dx_secundari_6_c == 'B18' | 
           dx_secundari_7_c == 'B18' | 
           dx_secundari_8_c == 'B18' | 
           dx_secundari_9_c == 'B18' |
           dx_secundari_10_c == 'B18' | 
           dx_secundari_11_c == 'B18' | 
           dx_secundari_12_c == 'B18' | 
           dx_secundari_13_c == 'B18' | 
           dx_secundari_14_c == 'B18' |
           dx_pral_c == 'B19' | 
           dx_secundari_1_c == 'B19' | 
           dx_secundari_2_c == 'B19' | 
           dx_secundari_3_c == 'B19' | 
           dx_secundari_4_c == 'B19' | 
           dx_secundari_5_c == 'B19' | 
           dx_secundari_6_c == 'B19' | 
           dx_secundari_7_c == 'B19' | 
           dx_secundari_8_c == 'B19' | 
           dx_secundari_9_c == 'B19' |
           dx_secundari_10_c == 'B19' | 
           dx_secundari_11_c == 'B19' | 
           dx_secundari_12_c == 'B19' | 
           dx_secundari_13_c == 'B19' | 
           dx_secundari_14_c == 'B19' |
           dx_pral_c == 'K754' | 
           dx_secundari_1_c == 'K754' | 
           dx_secundari_2_c == 'K754' | 
           dx_secundari_3_c == 'K754' | 
           dx_secundari_4_c == 'K754' | 
           dx_secundari_5_c == 'K754' | 
           dx_secundari_6_c == 'K754' | 
           dx_secundari_7_c == 'K754' | 
           dx_secundari_8_c == 'K754' | 
           dx_secundari_9_c == 'K754' |
           dx_secundari_10_c == 'K754' | 
           dx_secundari_11_c == 'K754' | 
           dx_secundari_12_c == 'K754' | 
           dx_secundari_13_c == 'K754' | 
           dx_secundari_14_c == 'K754' |
           dx_pral_c == 'K754'
  )
altres_hepatiti_cex <- altres_hepatiti_cex %>%
  select(IdCas)
altres_hepatitis <- rbind(altres_hepatitis_cap, altres_hepatiti_cex)

altres_hepatitis <- altres_hepatitis %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)

# Excloure altres hepatitis de tots els grups 
masld_dx <- masld_dx %>%
  filter(!IdCas %in% altres_hepatitis$IdCas)

masld_s <- masld_s %>%
  filter(!IdCas %in% altres_hepatitis$IdCas)

ald_dx <- ald_dx %>%
  filter(!IdCas %in% altres_hepatitis$IdCas)

ald_s <- ald_s %>%
  filter(!IdCas %in% altres_hepatitis$IdCas)

# Consum oh

oh_set <- oh %>%
  filter(VU_COD_VS == 'ALSET' | VU_COD_VS == 'ALHAB' | VU_COD_VS == 'ALDIA')

# Filtrem pacients amb consum d'alcohol moderat (> 2UBE/dia i < 5UBE/dia per 
# dones i >= 3UBE/dia i <= 6UBE/dia per homes)

# OH moderat
oh_set_d <- oh_set %>%
  filter(IdCas %in% dones$IdCas)
oh_set_moderat_d <- oh_set_d %>%
  filter(VU_VAL > 14 & VU_VAL < 35)

oh_set_h <- oh_set %>%
  filter(!IdCas %in% dones$IdCas) 
oh_set_moderat_h <- oh_set_h %>%
  filter(VU_VAL > 21 & VU_VAL < 42)


oh_moderat <- rbind(oh_set_moderat_d, oh_set_moderat_h)

oh_moderat <- oh_moderat %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)



# OH moderat excloent OH risc 
oh_moderat_e <- oh_moderat %>%
  filter(!IdCas %in% oh_risc$IdCas)

rm(ald_abus, oh_moderat, oh_set_d, oh_set_h, oh_set_moderat_d, oh_set_moderat_h,
   oh_set_risc, oh_audit_risc)

# Excloure OH risc de masld 
masld_dx <- masld_dx %>%
  filter(!IdCas %in% oh_risc$IdCas)
masld_s <- masld_s %>%
  filter(!IdCas %in% oh_risc$IdCas)


# Excloure dels grups amb sospita aquells que tenen dx
masld_s <- masld_s %>%
  filter(!IdCas %in% masld_dx$IdCas)

ald_s <- ald_s %>%
  filter(!IdCas %in% ald_dx$IdCas)

# Excloure ald dels masld
ald <- rbind(ald_dx, ald_s)

masld_dx <- masld_dx %>%
  filter(!IdCas %in% ald$IdCas)

masld_s <- masld_s %>%
  filter(!IdCas %in% ald$IdCas)

# Trobar pacients grup masld que estiguin al MetALD

metald_dx <- masld_dx %>% # MAL CLASSIFICATS
  filter(IdCas %in% oh_moderat_e$IdCas)
metald_s <- masld_s %>%
  filter(IdCas %in% oh_moderat_e$IdCas)

total_pacients <- rbind(masld_dx, masld_s, ald_dx, ald_s)
total_pacients <- total_pacients %>%
  distinct(IdCas, .keep_all = TRUE) %>%
  select(IdCas)



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

write.csv(masld_dx, file = "masld_dx.csv")
write.csv(masld_s, file = "masld_s.csv")
write.csv(metald_dx, file = "metald_dx.csv")
write.csv(metald_s, file = "metald_s.csv")
write.csv(ald_dx, file = "ald_dx.csv")
write.csv(ald_s, file = "ald_s.csv")
write.csv(total_pacients, file = "total_pacients_actualitzat.csv")

