# =========================
# Replication base script
# =========================

# Packages
library(dplyr)
library(ez)
library(papaja)
library(haven)
library(knitr)
# =========================
# STUDY 1
# =========================

# Load data
Data_online_s1 <- read.csv("Study1-Online.csv")
Data_field_s1  <- read.csv("Study1-Field.csv")

# Keep complete cases for the key filter variable used in the original script
Data_online_s1 <- Data_online_s1 %>% filter(!is.na(Q54))
Data_field_s1  <- Data_field_s1  %>% filter(!is.na(Q54))

# Keep only participants currently in a relationship
Data_online_s1 <- Data_online_s1 %>% filter(Q6 == 1)
Data_field_s1  <- Data_field_s1  %>% filter(Q6 == 1)

select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
transmute <- dplyr::transmute
summarise <- dplyr::summarise
group_by <- dplyr::group_by
# -------------------------
# Build online dataset
# -------------------------
Data_analysis_s1_online <- Data_online_s1 %>%
  dplyr::select(
    Q4, Q23:Q37, Q39, Q40, Q41, Q42, Q43, Q44,
    FL_21_DO_Scenario5_attractiveandnotdominantfemale,
    FL_21_DO_Scenario7_unattractiveanddominantfemale,
    FL_21_DO_Scenario6_attractiveanddominantfemale,
    FL_21_DO_Scenario8_unattractiveandnonedominantfemale,
    FL_17_DO_Scenario_attractiveandnon_dominantmale,
    FL_17_DO_Scenario3_dominantandunattractivemale,
    FL_17_DO_Scenario1_attractiveanddominantmale,
    FL_17_DO_Scenario4_NotDominantandUnattractivemale,
    Q93:Q98
  )

names(Data_analysis_s1_online) <- c(
  "Gender", "Suspicious", "Betrayed", "Worried", "Distrustful", "Jealousy",
  "Rejected", "Hurt", "Anxious", "Angry", "Threatened", "Sad", "Upset",
  "Attractiveness_check1", "Attractiveness_check2",
  "Assertiveness", "Self_confidence", "Extroverted", "Influential",
  "Socially_competent", "Good_judge_of_character",
  "Attractive_not_dominant_woman", "Unattractive_dominant_woman",
  "Attractive_dominant_woman", "Unattractive_not_dominant_woman",
  "Attractive_not_dominant_man", "Unattractive_dominant_man",
  "Attractive_dominant_man", "Unattractive_not_dominant_man",
  "Mate_value1", "Mate_value2", "Mate_value3",
  "Mate_value4", "Mate_value5", "Mate_value6"
)

Data_analysis_s1_online$Sample <- "Online"

# -------------------------
# Build field dataset
# -------------------------
# Note: field version has slightly different column positions
Data_analysis_s1_field <- Data_field_s1 %>%
  dplyr::select(
    Q4, Q23:Q34, Q36:Q37, Q39, Q40, Q41, Q42, Q43, Q44,
    Q74, Q77, Q83, Q80, Q68, Q71, Q89, Q86,
    Q93:Q98
  )

names(Data_analysis_s1_field) <- c(
  "Gender", "Suspicious", "Betrayed", "Worried", "Distrustful", "Jealousy",
  "Rejected", "Hurt", "Anxious", "Angry", "Threatened", "Sad", "Upset",
  "Attractiveness_check1", "Attractiveness_check2",
  "Assertiveness", "Self_confidence", "Extroverted", "Influential",
  "Socially_competent", "Good_judge_of_character",
  "Attractive_not_dominant_woman", "Unattractive_dominant_woman",
  "Attractive_dominant_woman", "Unattractive_not_dominant_woman",
  "Attractive_not_dominant_man", "Unattractive_dominant_man",
  "Attractive_dominant_man", "Unattractive_not_dominant_man",
  "Mate_value1", "Mate_value2", "Mate_value3",
  "Mate_value4", "Mate_value5", "Mate_value6"
)

Data_analysis_s1_field$Sample <- "Field"

# -------------------------
# Combine Study 1
# -------------------------
Data_analysis_s1 <- bind_rows(Data_analysis_s1_online, Data_analysis_s1_field) %>%
  mutate(
    Attractive_condition = case_when(
      Attractive_not_dominant_woman == 1 |
        Attractive_dominant_woman == 1 |
        Attractive_not_dominant_man == 1 |
        Attractive_dominant_man == 1 ~ "Attractive",
      TRUE ~ "Not Attractive"
    ),
    Dominant_condition = case_when(
      Attractive_dominant_woman == 1 |
        Unattractive_dominant_woman == 1 |
        Attractive_dominant_man == 1 |
        Unattractive_dominant_man == 1 ~ "Dominant",
      TRUE ~ "Not Dominant"
    ),
    ID = 1:n()
  )

# Optional saves
write.csv(Data_analysis_s1, "Dataset1_clean.csv", row.names = FALSE)

# Optional study-specific subsets
Data_analysis_s1_men   <- Data_analysis_s1 %>% filter(Gender == 1)
Data_analysis_s1_women <- Data_analysis_s1 %>% filter(Gender == 2)

# Main Study 1 ANOVA
Ez_ANOVA_jealousy_s1 <- ezANOVA(
  data = Data_analysis_s1,
  dv = Jealousy,
  wid = ID,
  between = .(Attractive_condition, Dominant_condition, Gender),
  detailed = TRUE,
  type = 3
)

Ez_ANOVA_jealousy_s1

latex_s1 <- anova_apa(Ez_ANOVA_jealousy_s1, es = "ges", format = "latex")
cat(latex_s1)


# =========================
# STUDY 2
# =========================

# Load data
Data_online_s2 <- read.csv("Study2-Online.csv")
Data_field_s2  <- read.csv("Study2-Field.csv")

# Keep complete cases for the key filter variable used in the original script
Data_online_s2 <- Data_online_s2 %>% filter(!is.na(Q98))
Data_field_s2  <- Data_field_s2  %>% filter(!is.na(Q98))

# Keep only participants currently in a relationship
Data_online_s2 <- Data_online_s2 %>% filter(Q6 == 1)
Data_field_s2  <- Data_field_s2  %>% filter(Q6 == 1)

# -------------------------
# Build online dataset
# -------------------------
Data_analysis_s2_online <- Data_online_s2 %>%
  dplyr::select(
    Q4, Q23:Q37, Q39, Q40, Q41, Q42, Q43, Q44,
    FL_21_DO_Scenario5_attractiveandnotdominantfemale,
    FL_21_DO_Scenario7_unattractiveanddominantfemale,
    FL_21_DO_Scenario6_attractiveanddominantfemale,
    FL_21_DO_Scenario8_unattractiveandnotdominantfemale,
    FL_17_DO_Scenario_attractiveandnon_dominantmale,
    FL_17_DO_Scenario3_dominantandunattractivemale,
    FL_17_DO_Scenario1_attractiveanddominantmale,
    FL_17_DO_Scenario4_NotDominantandUnattractivemale,
    Q93:Q98
  )

names(Data_analysis_s2_online) <- c(
  "Gender", "Suspicious", "Betrayed", "Worried", "Distrustful", "Jealousy",
  "Rejected", "Hurt", "Anxious", "Angry", "Threatened", "Sad", "Upset",
  "Attractiveness_check1", "Attractiveness_check2",
  "Assertiveness", "Self_confidence", "Extroverted", "Influential",
  "Socially_competent", "Good_judge_of_character",
  "Attractive_not_dominant_woman", "Unattractive_dominant_woman",
  "Attractive_dominant_woman", "Unattractive_not_dominant_woman",
  "Attractive_not_dominant_man", "Unattractive_dominant_man",
  "Attractive_dominant_man", "Unattractive_not_dominant_man",
  "Mate_value1", "Mate_value2", "Mate_value3",
  "Mate_value4", "Mate_value5", "Mate_value6"
)

Data_analysis_s2_online$Sample <- "Online"

# -------------------------
# Build field dataset
# -------------------------
Data_analysis_s2_field <- Data_field_s2 %>%
  dplyr::select(
    Q4, Q23:Q34, Q36:Q37, Q39, Q40, Q41, Q42, Q43, Q44,
    FL_21_DO_Scenario5_attractiveandnotdominantfemale,
    FL_21_DO_Scenario7_unattractiveanddominantfemale,
    FL_21_DO_Scenario6_attractiveanddominantfemale,
    FL_21_DO_Scenario8_unattractiveandnotdominantfemale,
    FL_17_DO_Scenario_attractiveandnon_dominantmale,
    FL_17_DO_Scenario3_dominantandunattractivemale,
    FL_17_DO_Scenario1_attractiveanddominantmale,
    FL_17_DO_Scenario4_NotDominantandUnattractivemale,
    Q93:Q98
  )

names(Data_analysis_s2_field) <- c(
  "Gender", "Suspicious", "Betrayed", "Worried", "Distrustful", "Jealousy",
  "Rejected", "Hurt", "Anxious", "Angry", "Threatened", "Sad", "Upset",
  "Attractiveness_check1", "Attractiveness_check2",
  "Assertiveness", "Self_confidence", "Extroverted", "Influential",
  "Socially_competent", "Good_judge_of_character",
  "Attractive_not_dominant_woman", "Unattractive_dominant_woman",
  "Attractive_dominant_woman", "Unattractive_not_dominant_woman",
  "Attractive_not_dominant_man", "Unattractive_dominant_man",
  "Attractive_dominant_man", "Unattractive_not_dominant_man",
  "Mate_value1", "Mate_value2", "Mate_value3",
  "Mate_value4", "Mate_value5", "Mate_value6"
)

Data_analysis_s2_field$Sample <- "Field"

# -------------------------
# Combine Study 2
# -------------------------
Data_analysis_s2 <- bind_rows(Data_analysis_s2_online, Data_analysis_s2_field) %>%
  mutate(
    Attractive_condition = case_when(
      Attractive_not_dominant_woman == 1 |
        Attractive_dominant_woman == 1 |
        Attractive_not_dominant_man == 1 |
        Attractive_dominant_man == 1 ~ "Attractive",
      TRUE ~ "Not Attractive"
    ),
    Dominant_condition = case_when(
      Attractive_dominant_woman == 1 |
        Unattractive_dominant_woman == 1 |
        Attractive_dominant_man == 1 |
        Unattractive_dominant_man == 1 ~ "Dominant",
      TRUE ~ "Not Dominant"
    ),
    ID = 1:n()
  )

# Optional saves
write.csv(Data_analysis_s2, "Dataset2_clean.csv", row.names = FALSE)


# Optional study-specific subsets
Data_analysis_s2_men   <- Data_analysis_s2 %>% filter(Gender == 1)
Data_analysis_s2_women <- Data_analysis_s2 %>% filter(Gender == 2)

# Main Study 2 ANOVA
Ez_ANOVA_jealousy_s2 <- ezANOVA(
  data = Data_analysis_s2,
  dv = Jealousy,
  wid = ID,
  between = .(Attractive_condition, Dominant_condition, Gender),
  detailed = TRUE,
  type = 3
)

Ez_ANOVA_jealousy_s2







# =========================
# EXTENSION: Compare Study 1 vs Study 2
# =========================

library(dplyr)
library(ggplot2)

# -------------------------
# 1. Helper: standardise one study dataset
# -------------------------
prep_study <- function(dat, study_label) {
  
  out <- dat %>%
    mutate(
      Study = study_label,
      Jealousy = suppressWarnings(as.numeric(Jealousy)),
      Gender_raw = as.character(Gender),
      Attractive_raw = as.character(Attractive_condition),
      Dominant_raw = as.character(Dominant_condition)
    ) %>%
    mutate(
      Gender = case_when(
        Gender_raw %in% c("1", "Male", "Men") ~ "Men",
        Gender_raw %in% c("2", "Female", "Women") ~ "Women",
        TRUE ~ NA_character_
      ),
      Attractive_condition = case_when(
        Attractive_raw %in% c("Attractive") ~ "Attractive",
        Attractive_raw %in% c("Not Attractive") ~ "Not Attractive",
        TRUE ~ NA_character_
      ),
      Dominant_condition = case_when(
        Dominant_raw %in% c("Dominant") ~ "Dominant",
        Dominant_raw %in% c("Not Dominant") ~ "Not Dominant",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(
      !is.na(Jealousy),
      !is.na(Gender),
      !is.na(Attractive_condition),
      !is.na(Dominant_condition)
    ) %>%
    mutate(
      Study = factor(Study, levels = c("Study 1", "Study 2")),
      Gender = factor(Gender, levels = c("Men", "Women")),
      Attractive_condition = factor(
        Attractive_condition,
        levels = c("Not Attractive", "Attractive")
      ),
      Dominant_condition = factor(
        Dominant_condition,
        levels = c("Not Dominant", "Dominant")
      )
    ) %>%
    droplevels()
  
  # hard checks so model cannot fail mysteriously
  if (nlevels(out$Gender) < 2) {
    stop(paste0(study_label, ": Gender has fewer than 2 levels after cleaning."))
  }
  if (nlevels(out$Attractive_condition) < 2) {
    stop(paste0(study_label, ": Attractive_condition has fewer than 2 levels after cleaning."))
  }
  if (nlevels(out$Dominant_condition) < 2) {
    stop(paste0(study_label, ": Dominant_condition has fewer than 2 levels after cleaning."))
  }
  
  return(out)
}

# -------------------------
# 2. Build clean extension datasets
# -------------------------
s1 <- prep_study(Data_analysis_s1, "Study 1")
s2 <- prep_study(Data_analysis_s2, "Study 2")

combined <- bind_rows(s1, s2) %>%
  mutate(
    Study = factor(Study, levels = c("Study 1", "Study 2"))
  ) %>%
  droplevels()

# -------------------------
# 3. Sanity checks
# -------------------------
cat("\n--- Study 1 ---\n")
print(table(s1$Gender, s1$Attractive_condition))
print(table(s1$Dominant_condition))

cat("\n--- Study 2 ---\n")
print(table(s2$Gender, s2$Attractive_condition))
print(table(s2$Dominant_condition))

cat("\n--- Combined ---\n")
print(table(combined$Study))
print(table(combined$Gender))
print(table(combined$Attractive_condition))
print(table(combined$Dominant_condition))

# -------------------------
# 4. Same model in each study
# -------------------------
model_s1 <- lm(
  Jealousy ~ Attractive_condition * Dominant_condition * Gender,
  data = s1
)

model_s2 <- lm(
  Jealousy ~ Attractive_condition * Dominant_condition * Gender,
  data = s2
)

anova_s1 <- anova(model_s1)
anova_s2 <- anova(model_s2)

print(anova_s1)
print(anova_s2)

# -------------------------
# 5. Extract the key interaction
# -------------------------
get_term_row <- function(anova_obj, possible_terms, study_label) {
  rn <- rownames(anova_obj)
  hit <- rn[rn %in% possible_terms]
  
  if (length(hit) == 0) {
    stop(
      paste0(
        "Could not find the target interaction in ", study_label, ".\nAvailable rows:\n",
        paste(rn, collapse = "\n")
      )
    )
  }
  
  term <- hit[1]
  idx <- which(rn == term)
  
  data.frame(
    Study = study_label,
    Term = term,
    Df = anova_obj[idx, "Df"],
    Sum_Sq = anova_obj[idx, "Sum Sq"],
    Mean_Sq = anova_obj[idx, "Mean Sq"],
    F_value = anova_obj[idx, "F value"],
    p_value = anova_obj[idx, "Pr(>F)"],
    row.names = NULL
  )
}

interaction_terms <- c(
  "Attractive_condition:Gender",
  "Gender:Attractive_condition"
)

interaction_s1 <- get_term_row(anova_s1, interaction_terms, "Study 1")
interaction_s2 <- get_term_row(anova_s2, interaction_terms, "Study 2")

interaction_results <- bind_rows(interaction_s1, interaction_s2)
print(interaction_results)

write.csv(interaction_results, "interaction_results_by_study.csv", row.names = FALSE)

# -------------------------
# 6. Summary table for plot and slides
# -------------------------
summary_table <- combined %>%
  group_by(Study, Gender, Attractive_condition) %>%
  summarise(
    N = n(),
    Mean_jealousy = mean(Jealousy, na.rm = TRUE),
    SD_jealousy = sd(Jealousy, na.rm = TRUE),
    SE_jealousy = SD_jealousy / sqrt(N),
    CI_low = Mean_jealousy - 1.96 * SE_jealousy,
    CI_high = Mean_jealousy + 1.96 * SE_jealousy,
    .groups = "drop"
  )

print(summary_table)
write.csv(summary_table, "jealousy_summary_table.csv", row.names = FALSE)

# -------------------------
# 7. Main extension plot
# -------------------------
p_main <- ggplot(
  summary_table,
  aes(
    x = Attractive_condition,
    y = Mean_jealousy,
    group = Gender,
    shape = Gender,
    linetype = Gender
  )
) +
  geom_line(position = position_dodge(width = 0.10)) +
  geom_point(size = 3, position = position_dodge(width = 0.10)) +
  geom_errorbar(
    aes(ymin = CI_low, ymax = CI_high),
    width = 0.08,
    position = position_dodge(width = 0.10)
  ) +
  facet_wrap(~ Study) +
  labs(
    title = "Jealousy by rival attractiveness, gender, and study",
    x = "Rival attractiveness condition",
    y = "Mean jealousy"
  ) +
  theme_minimal(base_size = 12)

print(p_main)

ggsave(
  filename = "extension_plot_main.png",
  plot = p_main,
  width = 9,
  height = 5,
  dpi = 300
)

# -------------------------
# 8. Pooled model
# -------------------------
model_pooled <- lm(
  Jealousy ~ Study * Attractive_condition * Dominant_condition * Gender,
  data = combined
)

anova_pooled <- anova(model_pooled)
print(anova_pooled)

write.csv(as.data.frame(anova_pooled), "anova_pooled.csv", row.names = TRUE)

# -------------------------
# 9. Extract pooled comparison term
# -------------------------
pooled_terms <- c(
  "Study:Attractive_condition:Gender",
  "Study:Gender:Attractive_condition",
  "Attractive_condition:Gender:Study",
  "Gender:Attractive_condition:Study",
  "Attractive_condition:Study:Gender",
  "Gender:Study:Attractive_condition"
)

study_difference_test <- get_term_row(
  anova_pooled,
  pooled_terms,
  "Pooled comparison"
)

print(study_difference_test)

write.csv(study_difference_test, "study_difference_test.csv", row.names = FALSE)

# -------------------------
# 10. Compact slide-ready table
# -------------------------
results_for_slides <- interaction_results %>%
  mutate(
    F_value = round(F_value, 3),
    p_value = round(p_value, 4)
  )

print(results_for_slides)
write.csv(results_for_slides, "results_for_slides.csv", row.names = FALSE)



# =========================
# EXTENSION 2: ORDERED LOGIT + OLS COMPARISON
# Uses the existing cleaned objects: s1 and s2
# =========================

library(MASS)   # polr
library(dplyr)
library(ggplot2)
library(knitr)

# -------------------------
# 1. Prepare ordered-logit datasets from existing s1 and s2
# -------------------------
prep_ord_from_existing <- function(dat, study_label) {
  
  out <- dat %>%
    transmute(
      Study = study_label,
      Jealousy_num = suppressWarnings(as.numeric(Jealousy)),
      Gender = factor(as.character(Gender), levels = c("Men", "Women")),
      Attractive_condition = factor(
        as.character(Attractive_condition),
        levels = c("Not Attractive", "Attractive")
      )
    ) %>%
    filter(
      !is.na(Jealousy_num),
      !is.na(Gender),
      !is.na(Attractive_condition)
    ) %>%
    droplevels()
  
  # Safety checks
  if (nlevels(out$Gender) < 2) {
    stop(paste0(study_label, ": Gender has fewer than 2 levels."))
  }
  if (nlevels(out$Attractive_condition) < 2) {
    stop(paste0(study_label, ": Attractive_condition has fewer than 2 levels."))
  }
  if (length(unique(out$Jealousy_num)) < 2) {
    stop(paste0(study_label, ": Jealousy has fewer than 2 observed values."))
  }
  
  # Ordered outcome
  out$Jealousy_ord <- ordered(
    as.character(out$Jealousy_num),
    levels = as.character(sort(unique(out$Jealousy_num)))
  )
  
  return(out)
}

s1_ord <- prep_ord_from_existing(s1, "Study 1")
s2_ord <- prep_ord_from_existing(s2, "Study 2")

# -------------------------
# 2. Sanity checks
# -------------------------
cat("\n--- Ordered logit: Study 1 ---\n")
print(table(s1_ord$Gender, useNA = "ifany"))
print(table(s1_ord$Attractive_condition, useNA = "ifany"))
print(table(s1_ord$Jealousy_ord, useNA = "ifany"))

cat("\n--- Ordered logit: Study 2 ---\n")
print(table(s2_ord$Gender, useNA = "ifany"))
print(table(s2_ord$Attractive_condition, useNA = "ifany"))
print(table(s2_ord$Jealousy_ord, useNA = "ifany"))

# -------------------------
# 3. Ordered logit models
# Focused on the main project question:
# Attractive_condition x Gender
# -------------------------
ord_model_s1 <- polr(
  Jealousy_ord ~ Attractive_condition * Gender,
  data = s1_ord,
  Hess = TRUE,
  method = "logistic",
  model = TRUE
)

ord_model_s2 <- polr(
  Jealousy_ord ~ Attractive_condition * Gender,
  data = s2_ord,
  Hess = TRUE,
  method = "logistic",
  model = TRUE
)

print(summary(ord_model_s1))
print(summary(ord_model_s2))

# -------------------------
# 4. Extract coefficient tables with p-values
# -------------------------
get_polr_results <- function(model, study_label) {
  ct <- coef(summary(model))
  pvals <- 2 * pnorm(abs(ct[, "t value"]), lower.tail = FALSE)
  
  out <- data.frame(
    Study = study_label,
    Term = rownames(ct),
    Estimate = ct[, "Value"],
    Std_Error = ct[, "Std. Error"],
    t_value = ct[, "t value"],
    p_value = pvals,
    row.names = NULL
  )
  
  return(out)
}

ord_results_s1 <- get_polr_results(ord_model_s1, "Study 1")
ord_results_s2 <- get_polr_results(ord_model_s2, "Study 2")

print(ord_results_s1)
print(ord_results_s2)

write.csv(ord_results_s1, "ordered_logit_study1.csv", row.names = FALSE)
write.csv(ord_results_s2, "ordered_logit_study2.csv", row.names = FALSE)

# -------------------------
# 5. Extract the key interaction term
# -------------------------
get_ord_interaction <- function(results_df, study_label) {
  possible_terms <- c(
    "Attractive_conditionAttractive:GenderWomen",
    "Attractive_condition1:Gender1",
    "Gender1:Attractive_condition1",
    "GenderWomen:Attractive_conditionAttractive"
  )
  
  idx <- which(results_df$Term %in% possible_terms)
  
  if (length(idx) == 0) {
    stop(
      paste0(
        "Could not find ordered-logit interaction term in ", study_label, ".\nAvailable terms:\n",
        paste(results_df$Term, collapse = "\n")
      )
    )
  }
  
  results_df[idx[1], ]
}

ord_interaction_s1 <- get_ord_interaction(ord_results_s1, "Study 1")
ord_interaction_s2 <- get_ord_interaction(ord_results_s2, "Study 2")

ord_interaction_results <- bind_rows(ord_interaction_s1, ord_interaction_s2)
print(ord_interaction_results)

write.csv(ord_interaction_results, "ordered_logit_interaction_results.csv", row.names = FALSE)

# -------------------------
# 6. OLS comparison model
# Same focal specification for comparison
# -------------------------
ols_model_s1_simple <- lm(
  Jealousy_num ~ Attractive_condition * Gender,
  data = s1_ord
)

ols_model_s2_simple <- lm(
  Jealousy_num ~ Attractive_condition * Gender,
  data = s2_ord
)

ols_anova_s1_simple <- anova(ols_model_s1_simple)
ols_anova_s2_simple <- anova(ols_model_s2_simple)

print(ols_anova_s1_simple)
print(ols_anova_s2_simple)

write.csv(as.data.frame(ols_anova_s1_simple), "ols_simple_study1.csv", row.names = TRUE)
write.csv(as.data.frame(ols_anova_s2_simple), "ols_simple_study2.csv", row.names = TRUE)

# Pull out the same interaction from OLS ANOVA
get_ols_interaction <- function(anova_obj, study_label) {
  rn <- rownames(anova_obj)
  idx <- which(rn == "Attractive_condition:Gender")
  
  if (length(idx) == 0) {
    stop(
      paste0(
        "Could not find OLS interaction term in ", study_label, ".\nAvailable rows:\n",
        paste(rn, collapse = "\n")
      )
    )
  }
  
  data.frame(
    Study = study_label,
    Term = rn[idx],
    Df = anova_obj[idx, "Df"],
    Sum_Sq = anova_obj[idx, "Sum Sq"],
    Mean_Sq = anova_obj[idx, "Mean Sq"],
    F_value = anova_obj[idx, "F value"],
    p_value = anova_obj[idx, "Pr(>F)"],
    row.names = NULL
  )
}

ols_interaction_s1 <- get_ols_interaction(ols_anova_s1_simple, "Study 1")
ols_interaction_s2 <- get_ols_interaction(ols_anova_s2_simple, "Study 2")

ols_interaction_results <- bind_rows(ols_interaction_s1, ols_interaction_s2)
print(ols_interaction_results)

write.csv(ols_interaction_results, "ols_interaction_results.csv", row.names = FALSE)

# -------------------------
# 7. Predicted probabilities from ordered logit
# Probability of the highest jealousy category
# -------------------------
newdata_grid <- expand.grid(
  Attractive_condition = factor(
    c("Not Attractive", "Attractive"),
    levels = c("Not Attractive", "Attractive")
  ),
  Gender = factor(
    c("Men", "Women"),
    levels = c("Men", "Women")
  )
)

# Study 1
pred_probs_s1 <- as.data.frame(
  predict(ord_model_s1, newdata = newdata_grid, type = "probs")
)
highest_cat_s1 <- tail(colnames(pred_probs_s1), 1)

plot_s1 <- cbind(newdata_grid, pred_probs_s1) %>%
  transmute(
    Study = "Study 1",
    Attractive_condition,
    Gender,
    Prob_Highest_Jealousy = .data[[highest_cat_s1]]
  )

# Study 2
pred_probs_s2 <- as.data.frame(
  predict(ord_model_s2, newdata = newdata_grid, type = "probs")
)
highest_cat_s2 <- tail(colnames(pred_probs_s2), 1)

plot_s2 <- cbind(newdata_grid, pred_probs_s2) %>%
  transmute(
    Study = "Study 2",
    Attractive_condition,
    Gender,
    Prob_Highest_Jealousy = .data[[highest_cat_s2]]
  )

plot_data_ord <- bind_rows(plot_s1, plot_s2)

print(plot_data_ord)
write.csv(plot_data_ord, "ordered_logit_predicted_probabilities.csv", row.names = FALSE)

# -------------------------
# 8. Plot predicted probability of highest jealousy
# -------------------------
p_ord <- ggplot(
  plot_data_ord,
  aes(
    x = Attractive_condition,
    y = Prob_Highest_Jealousy,
    group = Gender,
    shape = Gender,
    linetype = Gender
  )
) +
  geom_line(position = position_dodge(width = 0.08)) +
  geom_point(size = 3, position = position_dodge(width = 0.08)) +
  facet_wrap(~ Study) +
  labs(
    title = "Predicted probability of the highest jealousy category",
    x = "Rival attractiveness condition",
    y = "Predicted probability"
  ) +
  theme_minimal(base_size = 12)

print(p_ord)

ggsave(
  filename = "ordered_logit_predicted_probability_plot.png",
  plot = p_ord,
  width = 8,
  height = 5,
  dpi = 300
)

# -------------------------
# 9. Build a comparison table: Ordered logit vs OLS
# -------------------------
comparison_table <- data.frame(
  Study = c("Study 1", "Study 2"),
  OLS_F_value = c(ols_interaction_s1$F_value, ols_interaction_s2$F_value),
  OLS_p_value = c(ols_interaction_s1$p_value, ols_interaction_s2$p_value),
  OrdLogit_Estimate = c(ord_interaction_s1$Estimate, ord_interaction_s2$Estimate),
  OrdLogit_p_value = c(ord_interaction_s1$p_value, ord_interaction_s2$p_value)
)

print(comparison_table)
write.csv(comparison_table, "ordered_logit_vs_ols_comparison.csv", row.names = FALSE)


