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



##############################################################################
install.packages("dplyr")
library(dplyr)

taula_sld_neta <- read.csv("P:/P935/taula_sld_neta.csv")
taula_sld <- read.csv("P:/P935/taula_sld.csv")
taula_sld_neta <- taula_sld_neta %>%
  select(-1)

taula_sld_neta <- taula_sld_neta %>%
  group_by(IdCas) %>%
  arrange() %>%
  slice(1)

# Últimes correccions
taula_sld_neta <- taula_sld_neta %>%
  mutate(across(c(classificacio, diagnostic, sexe, sobrepes, obesitat, dm2, dlp,
                  hta, mcv_pre_dx, mcv_post_dx, fibr_cirr_pre_dx, 
                  fibr_cirr_post_dx, htp_pre_dx, htp_post_dx, descomp_pre_dx, 
                  descomp_post_dx, tx_pre_dx, tx_post_dx, progressor, defuncio,
                  mcv_finals, fibr_cirr_finals, htp_finals, descomp_finals,
                  tx_finals),
                ~ as.factor(.)))

taula_sld_neta <- taula_sld_neta %>%
  mutate(alb = if_else(alb > 8, NA_real_, alb)) %>%
  mutate(ggt = if_else(ggt < 10 | ggt > 1500, NA_real_, ggt))

taula_sld_neta <- taula_sld_neta %>%
  mutate(fib4 = if_else(
    !is.na(edat) & !is.na(ast) & !is.na(pla) & !is.na(alt), 
    (edat*ast)/(pla*sqrt(alt)), 
    NA_real_
  ))

taula_sld_neta <- taula_sld_neta %>%
  mutate(fib4 = if_else(fib4 > 26, NA_real_, fib4))

taula_sld_neta <- taula_sld_neta %>% 
  select(-tabac, -UBEs_set)

taula_sld_neta <- taula_sld_neta %>%
  mutate(imc = if_else(imc < 15 | imc > 70, NA_real_, imc))

# Derivats
taula_sld$data_dx_cap <- as.POSIXct(taula_sld$data_dx_cap)
taula_sld$data_dx_cex <- as.POSIXct(taula_sld$data_dx_cex)
  
taula_sld <- taula_sld %>%
  mutate(derivats = if_else(data_dx_cap < data_dx_cex, 1, 0))

derivacions <- taula_sld %>%
  filter(derivats == 1)

taula_sld_neta <- taula_sld_neta %>%
  mutate(derivats = if_else(IdCas %in% derivacions$IdCas, 1, 0))
taula_sld_neta$derivats <- as.factor(taula_sld_neta$derivats)

# Sumatori events
taula_sld_neta <- taula_sld_neta %>%
  mutate(mcv_finals = if_else(mcv_pre_dx == 1 | mcv_post_dx == 1, 1, 0)) %>%
  mutate(descomp_finals = if_else(descomp_pre_dx == 1 | descomp_post_dx == 1, 
                                  1, 0)) %>%
  mutate(htp_finals = if_else(htp_pre_dx == 1 | htp_post_dx == 1, 1, 0)) %>%
  mutate(fibr_cirr_finals = if_else(fibr_cirr_pre_dx == 1 | 
                                      fibr_cirr_post_dx == 1, 1, 0)) %>%
  mutate(tx_finals =if_else(tx_pre_dx == 1 | tx_post_dx == 1, 1, 0))

taula_sld_neta <- taula_sld_neta %>%
  mutate(across(c(mcv_finals, fibr_cirr_finals, htp_finals, descomp_finals,
                  tx_finals),
                ~ as.factor(.)))
taula_sld_neta <- taula_sld_neta %>%
  select(-X, -X.1)
# MELD-Na
taula_sld_neta <- taula_sld_neta %>%
  mutate(
    bili_ajustada = if_else(is.na(bili), NA_real_, pmax(bili, 1)),
    creat_ajustada = if_else(is.na(creat), NA_real_, pmax(pmin(creat), 1)),
    inr_ajustat = if_else(is.na(inr), NA_real_, pmax(inr, 1)),
    na_ajustat = if_else(is.na(na), NA_real_, pmax(pmin(na, 137), 125)),
    meld = 
      3.78 * log(bili_ajustada) +
      11.2 * log(inr_ajustat) +
      9.57 * log(creat_ajustada) +
      6.43, 
    meld_na = if_else(
      meld <= 11, 
      meld, 
      meld + 1.32 * (137 - na_ajustat) -0.033 * meld * (137 - na_ajustat)
      )
    ) %>%
  select(-bili_ajustada, -creat_ajustada, -inr_ajustat, -na_ajustat)

##################### Estudiar distribucio####################################
# GRUPS SEGONS CLASSIFICACIO
# Exemple distribució normal
taula_masld <- taula_sld_neta %>%
  filter(classificacio == "MASLD")
with(taula_masld, qqnorm(col, main = "QQplot Colesterol grup MASLD"))
with(taula_masld, qqline(col))
with(taula_masld, hist(col, main = "Histograma Colesterol grup MASLD"))
# Exemple distribució no normal
with(taula_masld, qqnorm(ast, main = "QQplot AST grup MASLD"))
with(taula_masld, qqline(ast))
with(taula_masld, hist(ast, main = "Histograma AST grup MASLD"))

# GRUPS SEGONS PROGRESSIO
# Exemple distribució normal
taula_prog <- taula_sld_neta %>%
  filter(progressor == 1)
with(taula_prog, qqnorm(col, main = "QQplot Colesterol grup Progressors"))
with(taula_prog, qqline(col))
with(taula_prog, hist(col, main = "Histograma Colesterol grup Progressors"))
# Exemple distribució no normal
with(taula_prog, qqnorm(ast, main = "QQplot AST grup Progressors"))
with(taula_prog, qqline(ast))
with(taula_prog, hist(ast, main = "Histograma AST grup Progressors"))

# Transformació logarítmica

taula_sld_quanti <- taula_sld_neta %>%
  select(IdCas, classificacio, progressor, imc, edat, hba1c, glu, col, hdl, tgc, 
         ast, alt, ggt, pla, alb, inr, bili, creat, na, meld_na, fib4)

taula_sld_log <- taula_sld_quanti %>%
  mutate(across(c(imc, hba1c, glu, hdl, tgc, ast, alt, ggt, alb, inr, bili, creat, 
                  meld_na, fib4), ~ log(.+1)))



# Correccio en les distribucions no normals
taula_masld_log <- taula_sld_log %>%
  filter(classificacio == "MASLD")
with(taula_masld_log, qqnorm(ast, main = "QQplot log(AST+1) grup MASLD"))
with(taula_masld_log, qqline(ast))
with(taula_masld_log, hist(ast, main = "Histograma log(AST+1) grup MASLD"))

taula_prog_log <- taula_sld_log %>%
  filter(progressor == 1)
with(taula_prog_log, qqnorm(ast, main = "QQplot log(AST+1) grup Progressors"))
with(taula_prog_log, qqline(ast))
with(taula_prog_log, hist(ast, main = "Histograma log(AST+1) grup Progressors"))

install.packages("tidyr")
library(tidyr)
# MASLD vs. METALD vs. ALD
# ANOVA
## Estadística descriptiva

descriptius_class <- taula_sld_neta %>%
  pivot_longer(cols = c(imc, edat, hba1c, glu, col, hdl, tgc, ast, alt, ggt, 
                        pla, alb, inr, bili, creat, na, meld_na, fib4), 
               names_to = "variable", 
               values_to ="valor") %>%
  group_by(variable, classificacio) %>%
  summarise(
    mitjana = mean(valor, na.rm = TRUE), 
    de = sd(valor, na.rm = TRUE), 
    .groups = "drop"
  )

## Resultats anova
taula_sld_log_classificacio <- taula_sld_log %>%
  select(-IdCas, -progressor)

install.packages("purrr")
install.packages("broom")
library(purrr)
library(tidyr)
library(dplyr)
library(broom)
p_anova_class <- taula_sld_log_classificacio %>%
  select(-classificacio) %>%
  summarise(across(everything(), ~ list(
    aov(. ~ taula_sld_log_classificacio$classificacio))), .groups = "drop") %>%
  pivot_longer(everything(), names_to = "variable", values_to = "model") %>%
  mutate(
    p_valor = map_dbl(model, ~ summary(.x)[[1]]$`Pr(>F)`[1])
  )

tukey_class <- p_anova_class %>%
  mutate(tukey = map(model, ~ tryCatch(
    TukeyHSD(.x) %>% tidy(),
    error = function(e) NULL
  ))) %>%
  select(variable, tukey) %>%
  unnest(tukey)

write.csv(tukey_class, "tukey_posthoc_classificacio.csv")

d_class_m <- descriptius_class %>%
  select(-de)
d_class_de <- descriptius_class %>%
  select(-mitjana)

r_class_wide_m <- d_class_m %>%
  pivot_wider(names_from = classificacio, values_from = mitjana)

r_class_wide_m <- r_class_wide_m %>%
  rename(MASLD_m = MASLD, METALD_m = METALD, ALD_m = ALD)

r_class_wide_de <- d_class_de %>%
  pivot_wider(names_from = classificacio, values_from = de)

r_class_wide_de <- r_class_wide_de %>%
  rename(MASLD_de = MASLD, METALD_de = METALD, ALD_de = ALD)

r_anova_class <- r_class_wide_m %>%
  left_join(r_class_wide_de, by = "variable") %>%
  left_join(p_anova_class, by = "variable")
rm(descriptius_class, d_class_m, d_class_de, r_class_wide_m, r_class_wide_de, 
   p_anova_class)

write.csv(r_anova_class, "resultats_anova_classificacio.csv")
# Chi-quadrat
chi_squared_test <- function(data, target){
  results <- list()
  cat_vars <- data %>% select(where(is.factor))
  for (var in names(cat_vars)){
    if (var != target){
      contingency_table <- table(data[[var]], data[[target]])
      chi_test <- chisq.test(contingency_table)
      percentages <- prop.table(contingency_table, margin = 2)*100
      results[[var]] <- list(
        ContingencyTable = as.data.frame.matrix(contingency_table),
        Percentages = as.data.frame.matrix(round(percentages, 2)),
        PValue = chi_test$p.value
      )
    }
  }
  return(results)
}



summary_table <- function(results){
  summary <- data.frame(Variable = character(),
                        Category = character(),
                        Target = character(),
                        Count = integer(),
                        Percentage = numeric(),
                        PValue = numeric(),
                        stringsAsFactors = FALSE)
  for (var in names(results)) {
    result <- results[[var]]
    table_data <- result$ContingencyTable
    percentages <- result$Percentages
    p_value <- result$PValue
    
    for(cat in rownames(table_data)){
      for (target in colnames(table_data)) {
        summary <- rbind(summary, data.frame(
          Variable = var, 
          Category = cat,
          Target = target,
          Count = table_data[cat, target],
          Percentage = percentages[cat, target],
          PValue = p_value
        ))
        
      }
      
    }
    
  }
  return(summary)
}

chi_class <- chi_squared_test(taula_sld_neta, "classificacio")
r_chi_class <- summary_table(chi_class)


r_chi_class <- r_chi_class %>%
  pivot_wider(names_from = Target, values_from = c(Count, Percentage))
rm(chi_class)
write.csv(r_chi_class, "P:/P935/resultats_chi_classificacio.csv")
# PROGRESSORS vs. NO PROGRESSORS
# ANOVA
## Estadística descriptiva
descriptius_prog <- taula_sld_neta %>%
  pivot_longer(cols = c(imc, edat, hba1c, glu, col, hdl, tgc, ast, alt, ggt, 
                        pla, alb, inr, bili, creat, na, meld_na, fib4), 
               names_to = "variable", 
               values_to ="valor") %>%
  group_by(variable, progressor) %>%
  summarise(
    mitjana = mean(valor, na.rm = TRUE), 
    de = sd(valor, na.rm = TRUE), 
    .groups = "drop"
  )

## Resultats anova
taula_sld_log_progressor <- taula_sld_log %>%
  select(-IdCas, -classificacio)

p_anova_prog <- taula_sld_log_progressor %>%
  select(-progressor) %>%
  summarise(across(everything(), ~ list(
    aov(. ~ taula_sld_log_progressor$progressor))), .groups = "drop") %>%
  pivot_longer(everything(), names_to = "variable", values_to = "test") %>%
  mutate(
    p_valor = map_dbl(test, ~ summary(.x)[[1]]$`Pr(>F)`[1])
  ) %>%
  select(variable, p_valor)

d_prog_m <- descriptius_prog %>%
  select(-de)
d_prog_de <- descriptius_prog %>%
  select(-mitjana)

r_prog_wide_m <- d_prog_m %>%
  pivot_wider(names_from = progressor, values_from = mitjana)

r_prog_wide_m <- r_prog_wide_m %>%
  rename(progressor_m = "1", no_progressor_m = "0")

r_prog_wide_de <- d_prog_de %>%
  pivot_wider(names_from = progressor, values_from = de)

r_prog_wide_de <- r_prog_wide_de %>%
  rename(progressor_de = "1", no_progressor_de = "0")

r_anova_prog <- r_prog_wide_m %>%
  left_join(r_prog_wide_de, by = "variable") %>%
  left_join(p_anova_prog, by = "variable")
rm(d_prog_m, d_prog_de, r_prog_wide_m, r_prog_wide_de, p_anova_prog)
rm(descriptius_prog)
write.csv(r_anova_prog, "resultats_anova_progressors.csv")
# Chi-quadrat
chi_prog <- chi_squared_test(taula_sld_neta, "progressor")
r_chi_prog <- summary_table(chi_prog)


r_chi_prog <- r_chi_prog %>%
  pivot_wider(names_from = Target, values_from = c(Count, Percentage))

rm(chi_prog)
write.csv(r_chi_prog, "P:/P935/resultats_chi_progressors.csv")
# REPETIM SEPARANT MALSD-METALD DE ALD
# PROGRESSORS vs. NO PROGRESSORS MASLD-METALD
# ANOVA
## Estadística descriptiva
taula_masld_metald <- taula_sld_neta %>%
  filter(classificacio == "MASLD" | classificacio == "METALD")
descriptius_prog_masld_metald <- taula_masld_metald %>%
  pivot_longer(cols = c(imc, edat, hba1c, glu, col, hdl, tgc, ast, alt, ggt, 
                        pla, alb, inr, bili, creat, na, meld_na, fib4), 
               names_to = "variable", 
               values_to ="valor") %>%
  group_by(variable, progressor) %>%
  summarise(
    mitjana = mean(valor, na.rm = TRUE), 
    de = sd(valor, na.rm = TRUE), 
    .groups = "drop"
  )

## Resultats anova
taula_log_prog_masld_metald <- taula_sld_log %>%
  select(-IdCas, -classificacio)

p_anova_prog <- taula_log_prog_masld_metald %>%
  select(-progressor) %>%
  summarise(across(everything(), ~ list(
    aov(. ~ taula_log_prog_masld_metald$progressor))), .groups = "drop") %>%
  pivot_longer(everything(), names_to = "variable", values_to = "test") %>%
  mutate(
    p_valor = map_dbl(test, ~ summary(.x)[[1]]$`Pr(>F)`[1])
  ) %>%
  select(variable, p_valor)

d_prog_m <- descriptius_prog_masld_metald %>%
  select(-de)
d_prog_de <- descriptius_prog_masld_metald %>%
  select(-mitjana)

r_prog_wide_m <- d_prog_m %>%
  pivot_wider(names_from = progressor, values_from = mitjana)

r_prog_wide_m <- r_prog_wide_m %>%
  rename(progressor_m = "1", no_progressor_m = "0")

r_prog_wide_de <- d_prog_de %>%
  pivot_wider(names_from = progressor, values_from = de)

r_prog_wide_de <- r_prog_wide_de %>%
  rename(progressor_de = "1", no_progressor_de = "0")

r_anova_prog_masld_metald <- r_prog_wide_m %>%
  left_join(r_prog_wide_de, by = "variable") %>%
  left_join(p_anova_prog, by = "variable")
rm(d_prog_m, d_prog_de, r_prog_wide_m, r_prog_wide_de, p_anova_prog)
rm(descriptius_prog_masld_metald)
write.csv(r_anova_prog_masld_metald, "P:/P935/resultats_anova_progressors_masld_metald.csv")
# Chi-quadrat
chi_prog <- chi_squared_test(taula_masld_metald, "progressor")
r_chi_prog <- summary_table(chi_prog)


r_chi_prog_masld_metald <- r_chi_prog %>%
  pivot_wider(names_from = Target, values_from = c(Count, Percentage))

rm(chi_prog, r_chi_prog)
write.csv(r_chi_prog_masld_metald, "P:/P935/resultats_chi_progressors_masld_metald.csv")


#########
taula_sld_neta <- taula_sld_neta %>%
  select(-1, -2)

taula_sld_neta <- taula_sld_neta %>%
  group_by(IdCas) %>%
  arrange(data_dx) %>%
  slice(1) %>%
  ungroup()

taula_sld_nse <- taula_sld %>%
  select(IdCas, nivell_socio_economic_c)

taula_sld_neta <- taula_sld_neta %>%
  left_join(taula_sld_nse, by = "IdCas")

taula_sld_neta <- taula_sld_neta %>%
  mutate(across(c(classificacio, diagnostic, regio_global, rs, 
                  nivell_socio_economic_c ,sexe, sobrepes, obesitat, dm2, dlp,
                  hta, derivats,mcv_pre_dx, mcv_post_dx, fibr_cirr_pre_dx, 
                  fibr_cirr_post_dx, htp_pre_dx, htp_post_dx, descomp_pre_dx, 
                  descomp_post_dx, tx_pre_dx, tx_post_dx, progressor, defuncio,
                  mcv_finals, fibr_cirr_finals, htp_finals, descomp_finals,
                  tx_finals),
                ~ as.factor(.)))
##############################################################################
write.csv(taula_sld_neta, "P:/P935/taula_sld_neta.csv")

