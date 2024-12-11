# Anàlisi estadístic

### Taula descriptiva
install.packages("pacman")
pacman::p_load(
  rio, 
  here, 
  skim, 
  tidyverse,
  gtsummary, 
  rstatix, 
  janitor, 
  scales, 
  flextable
)

## n TOTAL
n_total <- nrow(taula_sld)

# n MASLD
taula_masld <- taula_sld %>%
  filter(classificacio == "MASLD")
n_masld <- nrow(taula_masld)

# n METALD
taula_metald <- taula_sld %>%
  filter(classificacio == "METALD")
n_metald <- nrow(taula_metald)

# n ALD
taula_ald <- taula_sld %>%
  filter(classificacio == "ALD")
n_ald <- nrow(taula_ald)

## n diagnostic total
n_diagnostic <- taula_sld %>%
  filter(diagnostic == 1) %>%
  nrow()

# n diagnostic masld
n_diagnostic_masld <- taula_masld %>%
  filter(diagnostic == 1) %>%
  nrow()

# n diagnostic ald
n_diagnostic_ald <- taula_ald %>%
  filter(diagnostic == 1) %>%
  nrow()

## Edat TOTAL
m_edat <- mean(na.omit(taula_sld$edat))
sd_edat <- sd(na.omit(taula_sld$edat))

# Edat MASLD
m_edat_masld <- mean(na.omit(taula_masld$edat))
sd_edat_masld <- sd(na.omit(taula_masld$edat))

# Edat METALD
m_edat_metald <- mean(na.omit(taula_metald$edat))
sd_edat_metald <- sd(na.omit(taula_metald$edat))

# Edat ALD
m_edat_ald <- mean(na.omit(taula_ald$edat))
sd_edat_ald <- sd(na.omit(taula_ald$edat))

## Sexe dona
s_dona <- taula_sld %>%
  filter(sexe == 1) %>%
  nrow()

# Sexe MASLD
s_dona_masld <- taula_masld %>%
  filter(sexe == 1) %>%
  nrow()

# Sexe METALD
s_dona_metald <- taula_metald %>%
  filter(sexe == 1) %>%
  nrow()

# Sexe ALD
s_dona_ald <- taula_ald %>%
  filter(sexe == 1) %>%
  nrow()

## Nacionalitats
# Africa subsahariana 
africa_s <- taula_sld %>%
  filter(regio_global == "Àfrica subsahariana") %>%
  nrow()

# Africa subsahariana MASLD
africa_s_masld <- taula_masld %>%
  filter(regio_global == "Àfrica subsahariana") %>%
  nrow()

# Africa subsahariana METALD
africa_s_metald <- taula_metald %>%
  filter(regio_global == "Àfrica subsahariana") %>%
  nrow()

# Africa subsahariana ALD
africa_s_ald <- taula_ald %>%
  filter(regio_global == "Àfrica subsahariana") %>%
  nrow()

## Amèrica llatina 
america_ll <- taula_sld %>%
  filter(regio_global == "Amèrica llatina") %>%
  nrow()

# Amèrica llatina MASLD
america_ll_masld <- taula_masld %>%
  filter(regio_global == "Amèrica llatina") %>%
  nrow()

# Amèrica llatina METALD
america_ll_metald <- taula_metald %>%
  filter(regio_global == "Amèrica llatina") %>%
  nrow()

# Amèrica llatina ALD
america_ll_ald <- taula_ald %>%
  filter(regio_global == "Amèrica llatina") %>%
  nrow() 

## Àsia pacífic 
asia_p <- taula_sld %>%
  filter(regio_global == "Àsia pacífic") %>%
  nrow()

# Àsia pacífic MASLD
asia_p_masld <- taula_masld %>%
  filter(regio_global == "Àsia pacífic") %>%
  nrow()

# Àsia pacífic METALD
asia_p_metald <- taula_metald %>%
  filter(regio_global == "Àsia pacífic") %>%
  nrow()

# Àsia pacífic ALD
asia_p_ald <- taula_ald %>%
  filter(regio_global == "Àsia pacífic") %>%
  nrow() 

## Àsia central 
asia_c <- taula_sld %>%
  filter(regio_global == "Àsia central") %>%
  nrow()

# Àsia central MASLD
asia_c_masld <- taula_masld %>%
  filter(regio_global == "Àsia central") %>%
  nrow()

# Àsia central METALD
asia_c_metald <- taula_metald %>%
  filter(regio_global == "Àsia central") %>%
  nrow()

# Àsia central ALD
asia_c_ald <- taula_ald %>%
  filter(regio_global == "Àsia central") %>%
  nrow() 

## Àsia est 
asia_e <- taula_sld %>%
  filter(regio_global == "Àsia est") %>%
  nrow()

# Àsia est MASLD
asia_e_masld <- taula_masld %>%
  filter(regio_global == "Àsia est") %>%
  nrow()

# Àsia est METALD
asia_e_metald <- taula_metald %>%
  filter(regio_global == "Àsia est") %>%
  nrow()

# Àsia est ALD
asia_e_ald <- taula_ald %>%
  filter(regio_global == "Àsia est") %>%
  nrow() 

## Àsia sud-est 
asia_se <- taula_sld %>%
  filter(regio_global == "Àsia sud-est") %>%
  nrow()

# Àsia sud-est MASLD
asia_se_masld <- taula_masld %>%
  filter(regio_global == "Àsia sud-est") %>%
  nrow()

# Àsia sud-est METALD
asia_se_metald <- taula_metald %>%
  filter(regio_global == "Àsia sud-est") %>%
  nrow()

# Àsia sud-est ALD
asia_se_ald <- taula_ald %>%
  filter(regio_global == "Àsia sud-est") %>%
  nrow() 

## Australàsia 
australasia <- taula_sld %>%
  filter(regio_global == "Australàsia") %>%
  nrow()

# Australàsia MASLD
australasia_masld <- taula_masld %>%
  filter(regio_global == "Australàsia") %>%
  nrow()

# Australàsia METALD
australasia_metald <- taula_metald %>%
  filter(regio_global == "Australàsia") %>%
  nrow()

# Australàsia ALD
australasia_ald <- taula_ald %>%
  filter(regio_global == "Australàsia") %>%
  nrow() 

## Europa est 
europa_e <- taula_sld %>%
  filter(regio_global == "Europa est") %>%
  nrow()

# Europa est MASLD
europa_e_masld <- taula_masld %>%
  filter(regio_global == "Europa est") %>%
  nrow()

# Europa est METALD
europa_e_metald <- taula_metald %>%
  filter(regio_global == "Europa est") %>%
  nrow()

# Europa est ALD
europa_e_ald <- taula_ald %>%
  filter(regio_global == "Europa est") %>%
  nrow() 

## Europa oest 
europa_o <- taula_sld %>%
  filter(regio_global == "Europa oest") %>%
  nrow()

# Europa oest MASLD
europa_o_masld <- taula_masld %>%
  filter(regio_global == "Europa oest") %>%
  nrow()

# Europa oest METALD
europa_o_metald <- taula_metald %>%
  filter(regio_global == "Europa oest") %>%
  nrow()

# Europa oest ALD
europa_o_ald <- taula_ald %>%
  filter(regio_global == "Europa oest") %>%
  nrow() 

## MENA
mena <- taula_sld %>%
  filter(regio_global == "MENA") %>%
  nrow()

# MENA MASLD
mena_masld <- taula_masld %>%
  filter(regio_global == "MENA") %>%
  nrow()

# MENA METALD
mena_metald <- taula_metald %>%
  filter(regio_global == "MENA") %>%
  nrow()

# MENA ALD
mena_ald <- taula_ald %>%
  filter(regio_global == "MENA") %>%
  nrow() 

## Regió sanitaria
# SLD
taula_sld$rs <- as.factor(taula_sld$rs)
table(taula_sld$rs)

# MASLD
table(taula_masld$rs)

# METALD
table(taula_metald$rs)

# ALD
table(taula_ald$rs)

## Nivell socioeconòmic
table(taula_sld$nivell_socio_economic_c)
# MASLD 
table(taula_masld$nivell_socio_economic_c)
# METALD 
table(taula_metald$nivell_socio_economic_c)
# ALD 
table(taula_ald$nivell_socio_economic_c)

## Consum tabac
table(taula_sld$tabac)
# MASLD
table(taula_masld$tabac)
# METALD
table(taula_metald$tabac)
# ALD
table(taula_ald$tabac)

## Consum OH UBEs
m_ubes <- mean(na.omit(taula_sld$UBEs_set))
sd_ubes <- sd(na.omit(taula_sld$UBEs_set))

# MASLD
m_ubes_masld <- mean(na.omit(taula_masld$UBEs_set))
sd_ubes_masld <- sd(na.omit(taula_masld$UBEs_set))

# METALD
m_ubes_metald <- mean(na.omit(taula_metald$UBEs_set))
sd_ubes_metald <- sd(na.omit(taula_metald$UBEs_set))

# ALD
m_ubes_ald <- mean(na.omit(taula_ald$UBEs_set))
sd_ubes_ald <- sd(na.omit(taula_ald$UBEs_set))

## Factors de risc cardiovascular

## IMC
m_imc <- mean(na.omit(taula_sld$imc))
sd_imc <- sd(na.omit(taula_sld$imc))

# MASLD
m_imc_masld <- mean(na.omit(taula_masld$imc))
sd_imc_masld <- sd(na.omit(taula_masld$imc))

# METALD
m_imc_metald <- mean(na.omit(taula_metald$imc))
sd_imc_metald <- sd(na.omit(taula_metald$imc))

# ALD
m_imc_ald <- mean(na.omit(taula_ald$imc))
sd_imc_ald <- sd(na.omit(taula_ald$imc))

## Obesitat
table(taula_sld$ob)

# MASLD
table(taula_masld$ob)

# METALD 
table(taula_metald$ob)

# ALD
table(taula_ald$ob)


## Diabetis
table(taula_sld$dm2)

# MASLD
table(taula_masld$dm2)

# METALD 
table(taula_metald$dm2)

# ALD
table(taula_ald$dm2)

## Hipertensio
table(taula_sld$hta)

# MASLD
table(taula_masld$hta)

# METALD 
table(taula_metald$hta)

# ALD
table(taula_ald$hta)

## Dislipemia
table(taula_sld$dlp)

# MASLD
table(taula_masld$dlp)

# METALD 
table(taula_metald$dlp)

# ALD
table(taula_ald$dlp)

## Paràmetres analítics

## Hemoglobina glicada
m_hba1c <- mean(na.omit(taula_sld$hba1c))
sd_hba1c <- sd(na.omit(taula_sld$hba1c))
# MASLD
m_hba1c_masld <- mean(na.omit(taula_masld$hba1c))
sd_hba1c_masld <- sd(na.omit(taula_masld$hba1c))
# METALD
m_hba1c_metald <- mean(na.omit(taula_metald$hba1c))
sd_hba1c_metald <- sd(na.omit(taula_metald$hba1c))
# ALD
m_hba1c_ald <- mean(na.omit(taula_ald$hba1c))
sd_hba1c_ald <- sd(na.omit(taula_ald$hba1c))

## Glucosa
m_glu <- mean(na.omit(taula_sld$glu))
sd_glu <- sd(na.omit(taula_sld$glu))
# MASLD
m_glu_masld <- mean(na.omit(taula_masld$glu))
sd_glu_masld <- sd(na.omit(taula_masld$glu))
# METALD
m_glu_metald <- mean(na.omit(taula_metald$glu))
sd_glu_metald <- sd(na.omit(taula_metald$glu))
# ALD
m_glu_ald <- mean(na.omit(taula_ald$glu))
sd_glu_ald <- sd(na.omit(taula_ald$glu))

## Colesterol
m_col <- mean(na.omit(taula_sld$col))
sd_col <- sd(na.omit(taula_sld$col))
# MASLD
m_col_masld <- mean(na.omit(taula_masld$col))
sd_col_masld <- sd(na.omit(taula_masld$col))
# METALD
m_col_metald <- mean(na.omit(taula_metald$col))
sd_col_metald <- sd(na.omit(taula_metald$col))
# ALD
m_col_ald <- mean(na.omit(taula_ald$col))
sd_col_ald <- sd(na.omit(taula_ald$col))

## Colesterol HDL
m_hdl <- mean(na.omit(taula_sld$hdl))
sd_hdl <- sd(na.omit(taula_sld$hdl))
# MASLD
m_hdl_masld <- mean(na.omit(taula_masld$hdl))
sd_hdl_masld <- sd(na.omit(taula_masld$hdl))
# METALD
m_hdl_metald <- mean(na.omit(taula_metald$hdl))
sd_hdl_metald <- sd(na.omit(taula_metald$hdl))
# ALD
m_hdl_ald <- mean(na.omit(taula_ald$hdl))
sd_hdl_ald <- sd(na.omit(taula_ald$hdl))

## Triglicèrids
m_tgc <- mean(na.omit(taula_sld$tgc))
sd_tgc <- sd(na.omit(taula_sld$tgc))
# MASLD
m_tgc_masld <- mean(na.omit(taula_masld$tgc))
sd_tgc_masld <- sd(na.omit(taula_masld$tgc))
# METALD
m_tgc_metald <- mean(na.omit(taula_metald$tgc))
sd_tgc_metald <- sd(na.omit(taula_metald$tgc))
# ALD
m_tgc_ald <- mean(na.omit(taula_ald$tgc))
sd_tgc_ald <- sd(na.omit(taula_ald$tgc))

## AST
m_ast <- mean(na.omit(taula_sld$ast))
sd_ast <- sd(na.omit(taula_sld$ast))
# MASLD
m_ast_masld <- mean(na.omit(taula_masld$ast))
sd_ast_masld <- sd(na.omit(taula_masld$ast))
# METALD
m_ast_metald <- mean(na.omit(taula_metald$ast))
sd_ast_metald <- sd(na.omit(taula_metald$ast))
# ALD
m_ast_ald <- mean(na.omit(taula_ald$ast))
sd_ast_ald <- sd(na.omit(taula_ald$ast))

## ALT
m_alt <- mean(na.omit(taula_sld$alt))
sd_alt <- sd(na.omit(taula_sld$alt))
# MASLD
m_alt_masld <- mean(na.omit(taula_masld$alt))
sd_alt_masld <- sd(na.omit(taula_masld$alt))
# METALD
m_alt_metald <- mean(na.omit(taula_metald$alt))
sd_alt_metald <- sd(na.omit(taula_metald$alt))
# ALD
m_alt_ald <- mean(na.omit(taula_ald$alt))
sd_alt_ald <- sd(na.omit(taula_ald$alt))

## GGT
m_ggt <- mean(na.omit(taula_sld$ggt))
sd_ggt <- sd(na.omit(taula_sld$ggt))
# MASLD
m_ggt_masld <- mean(na.omit(taula_masld$ggt))
sd_ggt_masld <- sd(na.omit(taula_masld$ggt))
# METALD
m_ggt_metald <- mean(na.omit(taula_metald$ggt))
sd_ggt_metald <- sd(na.omit(taula_metald$ggt))
# ALD
m_ggt_ald <- mean(na.omit(taula_ald$ggt))
sd_ggt_ald <- sd(na.omit(taula_ald$ggt))

## Albumina
m_alb <- mean(na.omit(taula_sld$alb))
sd_alb <- sd(na.omit(taula_sld$alb))
# MASLD
m_alb_masld <- mean(na.omit(taula_masld$alb))
sd_alb_masld <- sd(na.omit(taula_masld$alb))
# METALD
m_alb_metald <- mean(na.omit(taula_metald$alb))
sd_alb_metald <- sd(na.omit(taula_metald$alb))
# ALD
m_alb_ald <- mean(na.omit(taula_ald$alb))
sd_alb_ald <- sd(na.omit(taula_ald$alb))

## INR
m_inr <- mean(na.omit(taula_sld$inr))
sd_inr <- sd(na.omit(taula_sld$inr))
# MASLD
m_inr_masld <- mean(na.omit(taula_masld$inr))
sd_inr_masld <- sd(na.omit(taula_masld$inr))
# METALD
m_inr_metald <- mean(na.omit(taula_metald$inr))
sd_inr_metald <- sd(na.omit(taula_metald$inr))
# ALD
m_inr_ald <- mean(na.omit(taula_ald$inr))
sd_inr_ald <- sd(na.omit(taula_ald$inr))

## Bilirubina
m_bili <- mean(na.omit(taula_sld$bili))
sd_bili <- sd(na.omit(taula_sld$bili))
# MASLD
m_bili_masld <- mean(na.omit(taula_masld$bili))
sd_bili_masld <- sd(na.omit(taula_masld$bili))
# METALD
m_bili_metald <- mean(na.omit(taula_metald$bili))
sd_bili_metald <- sd(na.omit(taula_metald$bili))
# ALD
m_bili_ald <- mean(na.omit(taula_ald$bili))
sd_bili_ald <- sd(na.omit(taula_ald$bili))

## Plaquetes
m_pla <- mean(na.omit(taula_sld$pla))
sd_pla <- sd(na.omit(taula_sld$pla))
# MASLD
m_pla_masld <- mean(na.omit(taula_masld$pla))
sd_pla_masld <- sd(na.omit(taula_masld$pla))
# METALD
m_pla_metald <- mean(na.omit(taula_metald$pla))
sd_pla_metald <- sd(na.omit(taula_metald$pla))
# ALD
m_pla_ald <- mean(na.omit(taula_ald$pla))
sd_pla_ald <- sd(na.omit(taula_ald$pla))

## FIB4
m_fib4 <- mean(na.omit(taula_sld$fib4))
sd_fib4 <- sd(na.omit(taula_sld$fib4))
# MASLD
m_fib4_masld <- mean(na.omit(taula_masld$fib4))
sd_fib4_masld <- sd(na.omit(taula_masld$fib4))
# METALD
m_fib4_metald <- mean(na.omit(taula_metald$fib4))
sd_fib4_metald <- sd(na.omit(taula_metald$fib4))
# ALD
m_fib4_ald <- mean(na.omit(taula_ald$fib4))
sd_fib4_ald <- sd(na.omit(taula_ald$fib4))

## Events 
# Events cardiovasculars pre dx

table(taula_sld$mcv_pre_dx)

# MASLD
table(taula_masld$mcv_pre_dx)

# METALD 
table(taula_metald$mcv_pre_dx)

# ALD
table(taula_ald$mcv_pre_dx)

# Events cardiovasculars post dx

table(taula_sld$mcv_post_dx)

# MASLD
table(taula_masld$mcv_post_dx)

# METALD 
table(taula_metald$mcv_post_dx)

# ALD
table(taula_ald$mcv_post_dx)

# Events hepatics pre dx

table(taula_sld$descomp_pre_dx)

# MASLD
table(taula_masld$descomp_pre_dx)

# METALD 
table(taula_metald$descomp_pre_dx)

# ALD
table(taula_ald$descomp_pre_dx)

# Events hepatics post dx

table(taula_sld$descomp_post_dx)

# MASLD
table(taula_masld$descomp_post_dx)

# METALD 
table(taula_metald$descomp_post_dx)

# ALD
table(taula_ald$descomp_post_dx)

# Hipertensió portal pre dx

table(taula_sld$htp_pre_dx)

# MASLD
table(taula_masld$htp_pre_dx)

# METALD 
table(taula_metald$htp_pre_dx)

# ALD
table(taula_ald$htp_pre_dx)

# Hipertensió portal post dx

table(taula_sld$htp_post_dx)

# MASLD
table(taula_masld$htp_post_dx)

# METALD 
table(taula_metald$htp_post_dx)

# ALD
table(taula_ald$htp_post_dx)

# Transplantament pre dx

table(taula_sld$tx_pre_dx)

# MASLD
table(taula_masld$tx_pre_dx)

# METALD 
table(taula_metald$tx_pre_dx)

# ALD
table(taula_ald$tx_pre_dx)

# Transplantament post dx

table(taula_sld$tx_post_dx)

# MASLD
table(taula_masld$tx_post_dx)

# METALD 
table(taula_metald$tx_post_dx)

# ALD
table(taula_ald$tx_post_dx)

# Exitus

table(taula_sld$defuncio)

# MASLD
table(taula_masld$defuncio)

# METALD 
table(taula_metald$defuncio)

# ALD
table(taula_ald$defuncio)
