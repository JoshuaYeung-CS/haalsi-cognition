# Extra Q1 analyses for the presentation: bootstrap CIs for the intervention scenarios,
# healthy-user check, robustness to extra covariates, E-value, and slide charts.
# Run from this folder: R_LIBS_USER=$PWD/.rlib Rscript q1_extras.R

params <- list(data_path = "HAALSI_W1-3_Longitudinal_Dataset.dta")
purled <- tempfile(fileext = ".R")
knitr::purl("HAALSI_hackathon_joshua.Rmd", output = purled, quiet = TRUE)
code <- readLines(purled)
code <- code[seq_len(grep("^w1 \\|>$", code)[1] - 1)]   # stop before Question 2
invisible(capture.output(eval(parse(text = code))))
options(knitr.table.format = "pipe")
dir.create("slides_fig", showWarnings = FALSE)
set.seed(2026)

# 1. Bootstrap CIs for the scenario changes (percentage points) ----------------------------
boot_shift <- function(model, change, B = 500) {
  d <- model.frame(model) |> as.data.frame()
  f <- formula(model)
  replicate(B, {
    db <- d[sample(nrow(d), replace = TRUE), ]
    m <- glm(f, family = binomial, data = db)
    100 * (mean(predict(m, change(db), type = "response")) - mean(predict(m, db, type = "response")))
  })
}
sc <- list(
  "Cash transfer\n(assets +0.5 SD)" =
    list(m1a_low, function(d) mutate(d, asset_sd = asset_sd + 0.5)),
  "Hypertension care\n(treat all untreated)" =
    list(m1b_low, function(d) mutate(d, htn_group = replace(htn_group, htn_group == "Hypertension, untreated",
                                                            "Hypertension, treated"))),
  "Psychotherapy\n(CES-D -0.5 SD if symptomatic)" =
    list(m1c_low, function(d) mutate(d, cesd_z = ifelse(cesd_z > 0, cesd_z - 0.5, cesd_z)))
)
sc_tab <- lapply(names(sc), function(n) {
  pt <- 100 * diff(prev_shift(sc[[n]][[1]], sc[[n]][[2]]))
  bs <- boot_shift(sc[[n]][[1]], sc[[n]][[2]])
  data.frame(scenario = n, change = pt, lo = quantile(bs, 0.025), hi = quantile(bs, 0.975))
}) |> bind_rows()
sc_tab$base <- 100 * sapply(sc, function(s) prev_shift(s[[1]], s[[2]])["before"])
cat("\n## Scenario changes in low cognition (percentage points, bootstrap 95% CI, B = 500)\n")
print(knitr::kable(sc_tab |> mutate(scenario = gsub("\n", " ", scenario),
                                    across(where(is.numeric), ~ round(.x, 2))), row.names = FALSE))

# 2. Healthy-user check: who gets hypertension treatment? ---------------------------------
cat("\n## Hypertension groups at Wave 1\n")
w1 |> filter(!is.na(htn_group)) |> group_by(htn_group) |>
  summarise(n = n(), age = mean(age, na.rm = TRUE),
            any_schooling = mean(education_f != "No formal", na.rm = TRUE),
            richest_2_quintiles = mean(wealth_f %in% c("Q4", "Q5 (richest)"), na.rm = TRUE),
            depressive_z = mean(cesd_z, na.rm = TRUE)) |>
  mutate(across(where(is.numeric), ~ round(.x, 2))) |> knitr::kable() |> print()

# 3. Robustness of the wealth estimate ------------------------------------------------------
rob <- list(
  "Main (age, sex, education)" = low_cognition ~ asset_sd + age + sex_f + education_f,
  "+ marital status" = low_cognition ~ asset_sd + age + sex_f + education_f + marital_f,
  "+ household size" = low_cognition ~ asset_sd + age + sex_f + education_f + log(hhnum),
  "+ born in Mozambique" = low_cognition ~ asset_sd + age + sex_f + education_f + born_moz,
  "+ all three" = low_cognition ~ asset_sd + age + sex_f + education_f + marital_f + log(hhnum) + born_moz
)
rob_tab <- lapply(names(rob), function(n) {
  m <- try(glm(rob[[n]], family = binomial, data = w1), silent = TRUE)
  if (inherits(m, "try-error")) return(data.frame(model = n, OR = NA, lo = NA, hi = NA, n = NA))
  tidy(m, conf.int = TRUE, exponentiate = TRUE) |> filter(term == "asset_sd") |>
    transmute(model = n, OR = estimate, lo = conf.low, hi = conf.high, n = nobs(m))
}) |> bind_rows()
cat("\n## Wealth OR per SD for low cognition, with extra covariates\n")
print(knitr::kable(rob_tab |> mutate(across(c(OR, lo, hi), ~ round(.x, 3)))))

# 4. E-value for the wealth association (outcome ~20%, so convert OR to RR by sqrt) -------
evalue <- function(or) { rr <- 1 / sqrt(or); rr + sqrt(rr * (rr - 1)) }
or_main <- rob_tab$OR[1]; or_hi <- rob_tab$hi[1]
cat(sprintf("\n## E-value: point %.2f, CI limit %.2f\n", evalue(or_main), evalue(or_hi)))

# 5. Charts ---------------------------------------------------------------------------------
theme_slide <- theme_minimal(base_size = 20) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot")

g1 <- tidy(m1a_cat, conf.int = TRUE) |> filter(grepl("wealth", term)) |>
  mutate(q = sub("wealth_f", "", term)) |>
  bind_rows(data.frame(q = "Q1 (poorest)", estimate = 0, conf.low = 0, conf.high = 0)) |>
  mutate(q = factor(q, levels = c("Q1 (poorest)", "Q2", "Q3", "Q4", "Q5 (richest)")))
p1 <- ggplot(g1, aes(q, estimate)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), colour = "#1f5f8b", size = 1.1, linewidth = 1.3) +
  labs(x = "Household wealth quintile", y = "Cognition vs poorest (SD)",
       title = "Cognition rises with every step of household wealth",
       subtitle = "Wave 1, adjusted for age, sex, education; 95% CI") + theme_slide
ggsave("slides_fig/wealth_gradient.png", p1, width = 12, height = 6.75, dpi = 150, bg = "white")

p2 <- ggplot(sc_tab, aes(reorder(scenario, change), change)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_pointrange(aes(ymin = lo, ymax = hi), colour = "#b5442c", size = 1.1, linewidth = 1.3) +
  coord_flip() +
  labs(x = NULL, y = "Change in % with low cognition (percentage points)",
       title = "Modelled effect of each intervention",
       subtitle = sprintf("Baseline about %.0f%% low cognition; bootstrap 95%% CI", sc_tab$base[1])) + theme_slide
ggsave("slides_fig/scenarios.png", p2, width = 12, height = 6.75, dpi = 150, bg = "white")

g3 <- q1c |> mutate(outcome = recode(outcome, objective_cognition_z_std = "Test score\n(objective)",
                                     memory_std = "Self-rated memory", concentration_std = "Concentration\ndifficulty",
                                     learning_std = "Learning\ndifficulty"),
                    type = ifelse(grepl("objective", outcome), "Objective", "Self-reported"),
                    size = abs(estimate), lo = ifelse(estimate < 0, -conf.high, conf.low),
                    hi = ifelse(estimate < 0, -conf.low, conf.high))
p3 <- ggplot(g3, aes(reorder(outcome, size), size, colour = type)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), size = 1.1, linewidth = 1.3) + coord_flip() +
  scale_colour_manual(values = c(Objective = "#1f5f8b", `Self-reported` = "#b5442c")) +
  labs(x = NULL, y = "Size of association per 1 SD depressive symptoms (SD)", colour = NULL,
       title = "Depression shows up more in how people rate their memory",
       subtitle = "than in how they actually score; 95% CI") + theme_slide + theme(legend.position = "bottom")
ggsave("slides_fig/depression.png", p3, width = 12, height = 6.75, dpi = 150, bg = "white")

g4 <- tidy(m1b, conf.int = TRUE) |> filter(grepl("htn", term)) |>
  mutate(g = sub("htn_groupHypertension, ", "", term)) |>
  bind_rows(data.frame(g = "no hypertension", estimate = 0, conf.low = 0, conf.high = 0)) |>
  mutate(g = factor(g, levels = c("no hypertension", "untreated", "treated")))
p4 <- ggplot(g4, aes(g, estimate)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), colour = "#1f5f8b", size = 1.1, linewidth = 1.3) +
  labs(x = NULL, y = "Cognition vs no hypertension (SD)",
       title = "Untreated hypertension: no gap. Treated: a suspicious advantage",
       subtitle = "Wave 1, adjusted for age, sex, education, wealth; 95% CI") + theme_slide
ggsave("slides_fig/hypertension.png", p4, width = 12, height = 6.75, dpi = 150, bg = "white")
cat("\nCharts written to slides_fig/\n")
