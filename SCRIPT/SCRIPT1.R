
rm(list=ls())

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(readr)
library(ggplot2)
library(glmmTMB)

df_or<-read_excel("DATI/DATABASE Lorenzo.xlsx")
unique(df_or$`2CurvaFV(mesiEC)`)

df_raw <- read_excel("DATI/DF_Lore.xlsx")
#View(df_raw)
summary(df_raw)
head(df_raw)
names(df_raw)

#ho modificato a mano gli ID TRAKCARE usando codice LA + numero
# adesso controlliamo che ogni soggetto ha un ID differente
check_ID<-df_raw %>% count(df_raw$ID)
summary(check_ID)

#i nomi delle colonne con parentesi e caratteri
# speciali saranno un problema quindi li ho modificati a mano
#togliendo parentesi e caratteri speciali.
# inoltre il df dovrà essere trasformato da wide a long

#conversione da wide a long con pivot:

unique(df_raw$Time_2)
class(df_raw$Time_1)
class(df_raw$Time_2)


df_long <- df_raw %>%
  pivot_longer(
    cols = matches("_(\\d+)$"),   # tutte le colonne che finiscono con _numero
    names_to = c(".value", "timepoint"),
    names_pattern = "^(.*)_(\\d+)$"
  ) %>%
  mutate(timepoint = as.integer(timepoint))


#check:
grep("tPTE", names(df_long), value = TRUE)


head(df_long)

#vedo che mancano tante osservazioni soprattutto ai primi timepoint:
#controlliamo:
#quanti soggetti hanno outcome in ciascun timepoint
df_long %>%
  filter(!is.na(tPTEF_perc_tE)) %>%
  distinct(ID, timepoint) %>%
  count(timepoint)
#Guarda quante osservazioni valide dell’outcome hai per timepoint:
df_long %>%
  group_by(timepoint) %>%
  summarise(
    n_outcome = sum(!is.na(tPTEF_perc_tE)),
    n_time    = sum(!is.na(Time)),
    n_both    = sum(!is.na(Time) & !is.na(tPTEF_perc_tE))
  )

n_valid <- df_long %>%
filter(!is.na(Time), !is.na(tPTEF_perc_tE)) %>%
count(ID, name = "n_valid")
  
summary(n_valid$n_valid)
table(n_valid$n_valid)
  
n_valid %>% arrange(n_valid) %>% head(10)

#vediamo se abbiamo perso osservazioni nel passaggio
#da wide a long:
out_cols <- grep("^tPTEF%tE \\(mean\\) preB2", names(df_or), value = TRUE)
out_cols
length(out_cols)
colSums(!is.na(df_or[, out_cols, drop = FALSE]))

wide_counts <- df_or %>%
  transmute(
    ID = ID,
    n_out_wide = rowSums(!is.na(across(all_of(out_cols))))
  )
summary(wide_counts$n_out_wide)
table(wide_counts$n_out_wide)

long_counts <- df_long %>%
  group_by(ID) %>%
  summarise(n_out_long = sum(!is.na(tPTEF_perc_tE)), .groups = "drop")

summary(long_counts$n_out_long)
table(long_counts$n_out_long)


chk <- wide_counts %>%
  inner_join(long_counts, by = "ID") %>%
  mutate(diff = n_out_wide - n_out_long)

table(chk$diff)
chk %>% filter(diff != 0) %>% arrange(desc(abs(diff)))

## non ho perso osservazioni...

# vediamo come si 
# distribuisce tPTEF/Te % che nel nostro df
# si chiama tPTEF_perc_tE
  
#VISUALIZZIAMO:

df_long %>%
  filter(!is.na(tPTEF_perc_tE)) %>%
  count(timepoint) %>%
  ggplot(aes(x = factor(timepoint), y = n)) +
  geom_col(fill = "grey80", color = "black") +
  labs(x = "Timepoint", y = "Number of observations",
       title = "Number of valid tPTEF%tE observations by timepoint") +
  theme_classic(base_size = 14)


df_use <- df_long %>% filter(!is.na(tPTEF_perc_tE))

x_min <- min(df_use$tPTEF_perc_tE, na.rm = TRUE)
x_max <- max(df_use$tPTEF_perc_tE, na.rm = TRUE)

# -----------------------
# 1) HISTOGRAM (count)
# -----------------------
p_hist_dens <- ggplot(df_use, aes(x = tPTEF_perc_tE)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    fill = "grey85",
    color = "black",
    linewidth = 0.35
  ) +
  geom_density(
    color = "red3",   # se la vuoi rossa
    linewidth = 1,
    trim = TRUE,
    cut = 0
  ) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  labs(
    title = "Distribution of tPTEF%tE",
    x = "tPTEF%tE",
    y = "Density"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

p_hist_dens

# la modellerei con la gamma
# I VLBW (i bb. nello studio) con BPD presentano 
# valori peggiori degli indici di funzione respiratoria.

#L’esposizione a hsPDA è associata a peggior funzione e 
#l’associazione è più forte 
#nei VLBW con BPD (interazione positiva in senso sfavorevole).

#Outcome Primario: tPTEF/Te %
#capiamo:

# Controlliamo quanti NA ci sono nelle variabili che vuoi usare nel modello
colSums(is.na(df_long[, c("Time", "tPTEF_perc_tE", "BPD_Jensen", "HSPDAgg", "sex1M2F", "LOS", "PH")]))


df_mod <- df_long %>%
  filter(!is.na(Time), !is.na(tPTEF_perc_tE),!is.na(PH)) %>%
  mutate(
    ID = factor(ID),
    
    BPD = factor(BPD_Jensen, levels = c(0,1)),#0=No;1=YES
    sex = factor(sex1M2F, levels = c(1,2)),#1=M;2=F
    
    LOS = factor(LOS),
    PH  = factor(PH),
    GA=  as.numeric(EG),
    hsPDA = as.numeric(HSPDAgg)
    
    )
vars_model <- c("Time","tPTEF_perc_tE","BPD","hsPDA","GA","sex","LOS","PH")
colSums(is.na(df_mod[, vars_model]))

###salviamolo
library(here)

out_dir  <- here("data", "derived")
out_file <- here("data", "derived", "df_mod.rds")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(df_mod, out_file)


########





# (A) Modello “livello medio” (più semplice)
m_level <- glmmTMB(
  tPTEF_perc_tE ~ Time + BPD*HSPDAgg + GA + sex + LOS + PH +
    (1 + Time | ID),
  family = Gamma(link = "log"),
  data = df_mod
)


summary(m_level)
#dà warning:
# Messaggi di avvertimento:
#   1: In doTryCatch(return(expr), name, parentenv, handler) :
#   aggiornamento elenco display non completo
# 2: In doTryCatch(return(expr), name, parentenv, handler) :
#   invalid graphics state
# 3: In doTryCatch(return(expr), name, parentenv, handler) :
#   invalid graphics state
# Il modello che hai chiesto ha una parte “random” troppo 
# complessa rispetto all’informazione nei dati, quindi alcuni 
# parametri dei random effects finiscono “sul bordo” (≈ 0) o 
# diventano perfettamente collineari.
# Risultato: la matrice var-cov dei random effects è singolare 
# (non pieno rango) e l’algoritmo segnala singular convergence (7).
#il random component della slope è quasi zero:
# Time_c      1.418e-05 0.003765 -1.00 

m_level0 <- glmmTMB(
  tPTEF_perc_tE ~ Time + BPD*hsPDA + GA + sex + LOS + PH +
    (1 |  ID),
  family = Gamma(link = "log"),
  data = df_mod
)

# ricordiamo che BW,GA,SEX,LOS (late onset sepsis),PH (ipertensione
# polmonare) sono confondenti a priori

summary(m_level0)



# Salva m_level0
saveRDS(m_level0, here("data", "derived", "m_level0.rds"))

#confrontando i due modelli con AIC e BIC
#SPIEGAZIONE:


# Criteri di informazione
# Guardando AIC e BIC, entrambi favoriscono il 
# modello più semplice. L'AIC passa da 1230.8 a 
# 1233.9 
# aggiungendo lo slope random, mentre il BIC 
# mostra una differenza ancora più marcata (da 
# 1266.8 a 1276.4). Il BIC penalizza maggiormente 
# la complessità del modello, e una differenza di 
# circa 10 punti è considerata sostanziale nella 
# selezione dei modelli.
# Il problema della correlazione al boundary
# L'elemento più preoccupante nell'output di 
# m_level è la correlazione di -1.00 tra 
# intercetta e slope random. Questa correlazione 
# al boundary è un segnale d'allarme che indica 
# problemi nella stima. La varianza dello slope 
# random risulta infatti estremamente piccola 
# (0.00001418), praticamente indistinguibile da 
# zero. Quando un parametro di varianza viene 
# stimato al limite del suo spazio parametrico, 
# significa che il modello sta cercando di stimare qualcosa che i dati non supportano.
# Guadagno in verosimiglianza trascurabile
# La differenza nella devianza (-2logL) tra i 
# due modelli è di appena 0.9 unità, a fronte 
# di due parametri aggiuntivi (la varianza dello 
# slope e la correlazione). Un test del rapporto 
# di verosimiglianza non risulterebbe minimamente 
# significativo, confermando che lo slope random non 
# aggiunge capacità esplicativa.

#sul modello m_level0 vediamo i residui:
# Residui di Pearson
# res_pearson <- residuals(m_level0, type = "pearson")
# 
# # QQ-plot per valutare la distribuzione
# qqnorm(res_pearson)
# qqline(res_pearson)
# 
# # Residui vs valori fittati
# plot(fitted(m_level0), res_pearson,
#      xlab = "Valori fittati", ylab = "Residui di Pearson")
# abline(h = 0, lty = 2)

#oppure piu semplicemente:
library(performance)
performance::check_model(m_level0)
check_model(m_level0)

r2(m_level0)


# y_sim <- simulate(m_level0, nsim = 200)  # lista di 200 vettori simulati
# 
# #e a mano:
# y_obs <- df_mod$tPTEF_perc_tE
# y_sim <- simulate(m_level0, nsim = 200)
# 
# sim_mat <- do.call(cbind, y_sim)   # n_obs x 200
# 
# # prendi poche simulazioni per non fare un groviglio
# set.seed(1)
# idx <- sample(ncol(sim_mat), 30)
# 
# plot(density(y_obs), lwd = 3, main = "Predictive check: densità", xlab = "tPTEF%tE")
# for (j in idx) lines(density(sim_mat[, j]), col = "steelblue", lwd = 1)
# lines(density(y_obs), lwd = 3)  # ridisegna osservata sopra
# legend("topright", legend = c("Observed", "Simulated (subset)"),
#        lwd = c(3,1), col = c("black","steelblue"), bty = "n")
# 


#altra diagnostica con DHARMA:
# simulare dati dal modello 
#fittato e confrontarli con i dati osservati.
library(DHARMa)
# Crea residui simulati (di default 250 simulazioni)
sim_res <- simulateResiduals(m_level0, n = 1000)


# Plot diagnostico complessivo
plot(sim_res)

    
testUniformity(sim_res) # Test KS per uniformità dei residui
#p=0.6782
# H0 (testUniformity / KS): 
# i residui scalati sono compatibili con Uniform(0,1) 
# (quindi modello ok su quel punto).
# 
# p = 0.68 ⇒ nessuna evidenza contro H0 ⇒ bene.

#La diagnostica dei residui (test KS: p = 0.68) 
#ha confermato la corretta specificazione del modello."




# (B) Modello “traiettorie” (più aderente al razionale del Word)
m_traj <- glmmTMB(
  tPTEF_perc_tE ~ Time * BPD * hsPDA + GA + sex + LOS + PH +
    (1 | ID),
  family = Gamma(link = "log"),
  data = df_mod
)

summary(m_traj)

#diagnostica:
library(performance)

check_model(m_traj)

r2(m_traj)


library(DHARMa)
# Crea residui simulati (di default 250 simulazioni)
sim_res_t <- simulateResiduals(m_traj, n = 1000)


# Plot diagnostico complessivo
plot(sim_res_t)


testUniformity(sim_res_t) 
#p-value = 0.7017

#QUALE MODELLO è IL MIGLIORE TRA QUELLO TRAJ E M_LEVEL0?
anova(m_level0, m_traj)
AIC(m_level0, m_traj)
BIC(m_level0, m_traj)
# 
# AIC: 1230.8 vs 1235.3 → favorisce m_level0
# BIC: 1266.8 vs 1281.1 → favorisce m_level0 (differenza di ~14 punti, sostanziale)
# LRT: χ² = 1.52, df = 3, p = 0.68 → l'interazione tripla con il tempo non aggiunge nulla
# 
# L'aggiunta del tempo nell'interazione 
# non migliora il modello. Questo significa che 
# le traiettorie temporali non differiscono tra i 
# gruppi BPD × hsPDA: l'effetto di BPD e hsPDA sui 
# livelli di tPTEF%Te è costante nel tempo, non cambia 
# con l'età corretta.

#analisi di sensitività levano GA e mettendo BW, nota bene che sono
#centrati quindi cambia interpretazione intercetta

m_sens_BW <- glmmTMB(
  tPTEF_perc_tE ~ Time + BPD * hsPDA + PN + sex + LOS + PH + (1 | ID),
  family = Gamma(link = "log"),
  data = df_mod
)


#Outcome Secondari

# * RR
# * VT/kg
# * tPTEF 

# RR
m_RR <- glmmTMB(
  RR ~ Time + BPD * hsPDA + GA + sex + LOS + PH + (1 | ID),
  family = Gamma(link = "log"),
  data = df_mod
)
summary(m_RR)

library(performance)
check_model(m_RR)
summary(df_mod$RR)

p_hist_dens_RR <- ggplot(df_mod, aes(x = RR)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    fill = "grey85",
    color = "black",
    linewidth = 0.35
  ) +
  geom_density(
    color = "red3",   # se la vuoi rossa
    linewidth = 1,
    trim = TRUE,
    cut = 0
  ) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  labs(
    title = "Distribution of RR",
    x = "RR",
    y = "Density"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

p_hist_dens_RR







# VT/kg
m_VTkg <- glmmTMB(
  Tidal_volume ~ Time + BPD * hsPDA + GA + sex + LOS + PH + (1 | ID),
  family = Gamma(link = "log"),
  data = df_mod
)
summary(m_VTkg)
performance::check_model(m_VTkg)
summary(df_mod$Tidal_volume)
plot((df_mod$Tidal_volume))

p_hist_dens_Volume <- ggplot(df_mod, aes(x = Tidal_volume)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 25,
    fill = "grey85",
    color = "black",
    linewidth = 0.35
  ) +
  geom_density(
    color = "red3",   # se la vuoi rossa
    linewidth = 1,
    trim = TRUE
  ) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  labs(
    title = "Distribution of RR",
    x = "RR",
    y = "Density"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

p_hist_dens_Volume


# tPTEF
m_tPTEF <- glmmTMB(
  Time_PTEF ~ Time + BPD * hsPDA + GA + sex + LOS + PH + (1 | ID),
  family = Gamma(link = "log"),
  data = df_mod
)
summary(m_tPTEF)
summary(df_mod$Time_PTEF)
plot(df_mod$Time_PTEF)
# Valutare se i parametri di funzione respiratoria 
# misurati ripetutamente nei VLBW nei primi 2 anni di vita 
# cambiano nel tempo (in funzione dell’età corretta all’esame) 
# e se traiettorie e/o livelli medi differiscono in base a BPD, 
# hsPDA e alla loro interazione, aggiustando per confondenti.

# primario: tPTEF%Te
# Endpoint secondari RR, VT/kg, tPTEF


# Confondenti (a priori):
# o	BW (peso alla nascita) e/o GA (gestational age): 
# per collinearità, in genere usare uno come principale (es. GA) + analisi di sensibilità con BW.
# sesso
# sepsi (late-onset sepsis o LOS sì/no)
# ipertensione polmonare (PH, presente/assente entro dimissione)

# Analisi di sensibilità 
# 1. Severità BPD: sostituire BPD sì/no con mild/mod/severe (trend) oppure interazione con severità


length(unique(df_mod$ID))
unique(df_mod$Grado123)
head(df_mod)

table(df_mod$ID,df_mod$Grado123)

df_mod$Grado123<-ifelse(is.na(df_mod$Grado123),0,df_mod$Grado123)
unique(df_mod$Grado123)

nrow(df_mod$Grado123)
length(df_mod$Grado123)







  library(dplyr)
  library(glmmTMB)
  
  df_sens <- df_mod %>%
    mutate(
      BPD_sev = case_when(
        Grado123 == 0 ~ "noBPD",
        BPD_Jensen == 1 & Grado123 == 1 ~ "mild",
        BPD_Jensen == 1 & Grado123 == 2 ~ "moderate",
        BPD_Jensen == 1 & Grado123 == 3 ~ "severe",
        TRUE ~ NA_character_
      ),
      BPD_sev = factor(BPD_sev, levels = c("noBPD", "mild", "moderate", "severe"))
    ) %>%
    filter(!is.na(BPD_sev))
  
  library(dplyr)
  library(ggplot2)
  
  freq_sev <- df_mod %>%
    filter(BPD_Jensen == 1, !is.na(Grado123)) %>%
    distinct(ID, Grado123) %>%   # <-- una riga per ID (con la sua severità)
    mutate(Grado123 = factor(
      Grado123, levels = c(1,2,3),
      labels = c("mild (1)", "moderate (2)", "severe (3)")
    )) %>%
    count(Grado123, name = "n_subj")
  
  
  ggplot(freq_sev, aes(x = Grado123, y = n_subj)) +
    geom_col() +
    geom_text(aes(label = n_subj), vjust = -0.4) +
    labs(x = "BPD severity (Jensen grade)", y = "Number of subjects (unique ID)",
         title = "Distribuzione severità BPD (solo soggetti con BPD)") +
    theme_classic(base_size = 14)
  
  
  
  m_sev <- glmmTMB(
    tPTEF_perc_tE ~ Time + BPD_sev * hsPDA + GA + sex + LOS + PH + (1|ID),
    family = Gamma(link="log"),
    data = df_sens
  )
  summary(m_sev)
  
  
  


# 4. Categorico PDA: 0 / 1–7 / >7 per robustezza,
df_pda_cat <- df_mod %>%
  mutate(
    hsPDA_cat = cut(hsPDA, breaks = c(-Inf, 0, 7, Inf),
                    labels = c("0", "1-7", ">7")),
    hsPDA_cat = relevel(hsPDA_cat, ref = "0")
  )

m_pda_cat <- glmmTMB(
  tPTEF_perc_tE ~ Time + BPD * hsPDA_cat + GA + sex + LOS + PH + (1 | ID),
  family = Gamma(link = "log"),
  data = df_pda_cat
)
summary(m_pda_cat)

#piecewise (0–7 e >7)
# per essere coerente con concetto da studio precedente di “critical window”.


df_piece <- df_mod %>%
  mutate(
    pda_0_7 = pmin(hsPDA, 7),
    pda_gt7 = pmax(hsPDA - 7, 0)
  )

m_piece <- glmmTMB(
  tPTEF_perc_tE ~ Time + BPD * (pda_0_7 + pda_gt7) + GA + sex + LOS + PH + (1 | ID),
  family = Gamma(link = "log"),
  data = df_piece
)

summary(m_piece)
AIC(m_level0, m_sev, m_pda_cat, m_piece)

#TOGLIERE TUTTE LE CENTRATURE....

# Nei modelli principali abbiamo usato hsPDA 
# (operazionalizzato come numero di giorni 
#   di esposizione, HSPDAgg, inserito come 
#   variabile continua e centrata) invece della 
# variabile PDA dicotomica (0/1), perché hsPDA 
# rappresenta una misura più informativa della 
# “dose” di esposizione. In altre parole, 
# il semplice indicatore presenza/assenza di 
# PDA non distingue tra neonati con un’esposizione 
# minima e neonati con un’esposizione prolungata, 
# mentre la durata in giorni consente di testare 
# in modo più diretto un’eventuale relazione 
# dose–response con gli indici di funzione 
# respiratoria. Questa scelta è inoltre 
# coerente con l’ipotesi clinica secondo 
# cui l’impatto del dotto può dipendere non 
# solo dalla presenza, ma anche dalla persistenza 
# dell’esposizione (es. concetto di “critical window”), 
# e permette di valutare l’interazione tra 
# hsPDA e BPD in modo quantitativo.

# Cartella target (Windows-friendly)
models_dir <- "C:/Users/39348/Desktop/BPD/data/derived/models"
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

models <- list(
  m_level0  = m_level0,
  m_traj    = m_traj,
  m_sens_BW = m_sens_BW,
  m_RR      = m_RR,
  m_VTkg    = m_VTkg,
  m_tPTEF   = m_tPTEF,
  m_sev     = m_sev,
  m_pda_cat = m_pda_cat,
  m_piece   = m_piece
)

saveRDS(models, file = file.path(models_dir, "all_models.rds"))

