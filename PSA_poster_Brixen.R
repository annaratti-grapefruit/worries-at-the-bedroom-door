
# Research question: is pre-sleep arousal (PSA) related to perceived sleep
# quality beyond actigraphic sleep efficiency (SE), and to the discrepancy
# between perceived and actigraphic sleep?
#
# Input data (not included in this repository):
#   - EMA file: one row per EMA prompt (morning prompts contain sleep quality
#     and the six pre-sleep arousal items)
#   - actigraphy file: one row per participant x study day (SE) plus baseline
#     variables (age, gender, PSQI)
# The working directory must contain the folder with the two files.


library(readxl)
library(dplyr)
library(lme4)
library(lmerTest)    
library(sjPlot)      
library(ggeffects)   
library(ggplot2)

file_ema <- "dati/data_EMA_05_16.xlsx"
file_act <- "dati/dataset_poster_CSA.xlsx"


# -----------------------------------------------------------------------------
# 1. Data
# -----------------------------------------------------------------------------
ema <- read_excel(file_ema)
act <- read_excel(file_act, sheet = "EMA_act_baseline")

# Morning questionnaire: perceived sleep quality (0-8) and pre-sleep arousal
# (six items, 1-5: three cognitive, three somatic). If a morning was completed
# twice, the last questionnaire is kept.
morning <- ema %>%
  filter(survey_completed == 1, grepl("EMA_morning_", survey)) %>%
  arrange(ID, DayNumber, time) %>%
  group_by(ID, DayNumber) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(EMAq    = SLEEPQ,
         PSA     = rowMeans(cbind(AR_EVEN, AR_OFF, AR_THOU, AR_MUS, AR_JITT, AR_BREA), na.rm = TRUE),
         PSA_cog = rowMeans(cbind(AR_EVEN, AR_OFF, AR_THOU), na.rm = TRUE),
         PSA_som = rowMeans(cbind(AR_MUS, AR_JITT, AR_BREA), na.rm = TRUE)) %>%
  select(ID, DayNumber, EMAq, PSA, PSA_cog, PSA_som)

# Actigraphic sleep efficiency (%) of the same night
actigraphy <- act %>%
  transmute(ID, DayNumber, SE = as.numeric(SE))

# Nights with perceived quality, SE and PSA all available
d <- morning %>%
  inner_join(actigraphy, by = c("ID", "DayNumber")) %>%
  filter(!is.na(EMAq), !is.na(SE), !is.na(PSA)) %>%
  as.data.frame()

nrow(d)            # nights
n_distinct(d$ID)   # participants


# 2. Within- / between-person decomposition
# _w = night value minus the participant's mean (night vs. usual)
# _b = participant's mean minus the grand mean of participant means
for (v in c("EMAq", "SE", "PSA", "PSA_cog", "PSA_som")) {
  person_mean <- ave(d[[v]], d$ID, FUN = function(x) mean(x, na.rm = TRUE))
  d[[paste0(v, "_w")]] <- d[[v]] - person_mean
  d[[paste0(v, "_b")]] <- person_mean - mean(tapply(d[[v]], d$ID, mean, na.rm = TRUE))
}

# Perceived-actigraphic discrepancy: z(EMAq) - z(SE) if > 0: sleep perceived as better than actigraphy indicates
d$DISCR <- as.numeric(scale(d$EMAq)) - as.numeric(scale(d$SE))

# Standard deviations used to express effects per 1 SD
SD_EMAq  <- sd(d$EMAq)
SD_SE    <- sd(d$SE)
SD_PSA_w <- sd(d$PSA_w)
SD_PSA_b <- sd(tapply(d$PSA, d$ID, mean))



# 3. Descriptive statistics

baseline <- act[!duplicated(act$ID), c("ID", "age", "gender", "PSQI_Totale")]
baseline <- baseline[baseline$ID %in% d$ID, ]

data.frame(
  variable = c("Age (years)", "PSQI total", "Sleep quality (EMAq)", "PSA total",
               "PSA cognitive", "PSA somatic", "Sleep efficiency (SE, %)"),
  mean = c(mean(as.numeric(baseline$age)), mean(as.numeric(baseline$PSQI_Totale)),
           mean(d$EMAq), mean(d$PSA), mean(d$PSA_cog), mean(d$PSA_som), mean(d$SE)),
  sd   = c(sd(as.numeric(baseline$age)), sd(as.numeric(baseline$PSQI_Totale)),
           sd(d$EMAq), sd(d$PSA), sd(d$PSA_cog), sd(d$PSA_som), sd(d$SE)))
table(baseline$gender)
range(as.numeric(baseline$age))



# 4. Agreement between perceived sleep quality and SE

cor.test(d$EMAq_w, d$SE_w)                                   # within persons

persons <- d %>% group_by(ID) %>% summarise(EMAq = mean(EMAq), SE = mean(SE))
cor.test(persons$EMAq, persons$SE)                           # between persons



# 5. Model comparison (maximum likelihood)

M0 <- lmer(EMAq ~ 1 + (1 | ID), data = d, REML = FALSE)
M1 <- lmer(EMAq ~ SE_w + SE_b + (1 | ID), data = d, REML = FALSE)
M2 <- lmer(EMAq ~ SE_w + SE_b + PSA_w + PSA_b + (1 | ID), data = d, REML = FALSE)
M3 <- lmer(EMAq ~ SE_w + SE_b + PSA_cog_w + PSA_cog_b + PSA_som_w + PSA_som_b + (1 | ID),
           data = d, REML = FALSE)
M4 <- lmer(EMAq ~ PSA_w + PSA_b + (1 | ID), data = d, REML = FALSE)

models <- list(M0 = M0, M1 = M1, M2 = M2, M3 = M3, M4 = M4)
fit <- data.frame(
  model    = names(models),
  npar     = sapply(models, function(m) attr(logLik(m), "df")),
  logLik   = sapply(models, function(m) as.numeric(logLik(m))),
  deviance = sapply(models, deviance),
  AIC      = sapply(models, AIC))
fit$AIC_weight <- exp(-(fit$AIC - min(fit$AIC)) / 2) / sum(exp(-(fit$AIC - min(fit$AIC)) / 2))
fit

# Likelihood ratio tests
anova(M0, M1)   # SE vs. null model
anova(M1, M2)   # PSA added to SE (main test)
anova(M2, M3)   # cognitive and somatic PSA separately vs. total PSA
anova(M0, M4)   # PSA only vs. null model
anova(M4, M2)   # SE added to PSA



# 6. Coefficients of the selected model (REML)

M2_reml <- lmer(EMAq ~ SE_w + SE_b + PSA_w + PSA_b + (1 | ID), data = d)
tab_model(M2_reml, show.re.var = FALSE, df.method = "satterthwaite")



# 7. PSA and perceived sleep, actigraphic sleep and their discrepancy

M4_reml <- lmer(EMAq  ~ PSA_w + PSA_b + (1 | ID), data = d)   # perceived quality
E1      <- lmer(SE    ~ PSA_w + PSA_b + (1 | ID), data = d)   # actigraphic SE
D1      <- lmer(DISCR ~ PSA_w + PSA_b + (1 | ID), data = d)   # discrepancy
tab_model(M4_reml, E1, D1, show.re.var = FALSE, df.method = "satterthwaite")

# Standardised effects: change in the outcome (SD) per 1 SD of PSA,
# with 95% CI based on Satterthwaite degrees of freedom
std_effect <- function(model, term, outcome, sd_outcome) {
  cf <- summary(model)$coefficients[term, ]
  q  <- qt(0.975, cf["df"])
  sd_psa <- if (term == "PSA_w") SD_PSA_w else SD_PSA_b
  data.frame(level   = ifelse(term == "PSA_w", "Within-person", "Between-person"),
             outcome = outcome,
             est     = cf["Estimate"] * sd_psa / sd_outcome,
             lower   = (cf["Estimate"] - q * cf["Std. Error"]) * sd_psa / sd_outcome,
             upper   = (cf["Estimate"] + q * cf["Std. Error"]) * sd_psa / sd_outcome,
             p       = cf["Pr(>|t|)"], row.names = NULL)
}

effects <- do.call(rbind, lapply(c("PSA_w", "PSA_b"), function(t) rbind(
  std_effect(M4_reml, t, "Perceived sleep quality",           SD_EMAq),
  std_effect(M2_reml, t, "Perceived sleep quality at equal SE", SD_EMAq),
  std_effect(E1,      t, "Sleep efficiency (actigraphy)",     SD_SE),
  std_effect(D1,      t, "Perceived - actigraphic discrepancy", 1))))   # DISCR is already in SD units
effects


# 8. Figures
col_line <- "#009999"
theme_poster <- theme_minimal() +
  theme(axis.text  = element_text(face = "bold", colour = "black", size = 14),
        axis.title = element_text(face = "bold", colour = "black", size = 17))

# Figures 1-2: perceived sleep quality predicted by M2 (95% CI)
plot_prediction <- function(term) {
  pr <- ggpredict(M2_reml, terms = paste(term, "[all]"))
  ggplot(pr, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = col_line, alpha = 0.15) +
    geom_line(colour = col_line, linewidth = 1) +
    labs(x = term, y = "EMAq") +
    theme_poster
}
plot_prediction("PSA_w")
plot_prediction("SE_w")

# Figure 3: standardised effects of PSA (open circle = model adjusted for SE)
effects$outcome  <- factor(effects$outcome, levels = rev(unique(effects$outcome)))
effects$level    <- factor(effects$level, levels = c("Within-person", "Between-person"))
effects$adjusted <- grepl("equal SE", effects$outcome)
effects$p_label  <- ifelse(effects$p < .001, "p < .001",
                           paste("p =", sub("^0", "", formatC(effects$p, digits = 3, format = "f"))))

ggplot(effects, aes(x = est, y = outcome)) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_linerange(aes(xmin = lower, xmax = upper), colour = col_line, linewidth = 1) +
  geom_point(aes(shape = adjusted), colour = col_line, fill = "white", size = 4, stroke = 1.5) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21), guide = "none") +
  geom_text(aes(x = 0.32, label = p_label), hjust = 1, size = 4.5, colour = "grey30") +
  facet_wrap(~ level, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(-0.4, 0.32), breaks = seq(-0.4, 0.2, by = 0.1)) +
  labs(x = "Change in outcome (SD) per 1 SD increase in PSA", y = NULL) +
  theme_poster +
  theme(strip.text = element_text(face = "bold", colour = "black", size = 15, hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank())
