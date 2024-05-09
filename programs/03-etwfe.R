# This a script to 
# run etwfe regressions

# Date: May 7th, 2024

### Load data
CPS_dates <- read_csv(file.path(datasets,"CPS_dates_county.csv")) |> 
    filter(year <= 2019)

CPS_dates  <- CPS_dates |>
  filter(county != 0)
### Run etwfe regressions: Event Study Table
library(etwfe)
library(marginaleffects)
options(datatable.optimize=1)

Hispanic_mod = etwfe(
    fml  = Hispanic ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    )
Hispanic_mod_es       = emfx(Hispanic_mod, type = "event", collapse = FALSE)

ggplot(Hispanic_mod_es, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "black") +  # Horizontal line in black
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +  # Error bars in black
  geom_point(size = 3, color = "red") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on Hispanic Identity") +
  theme_customs()

ggsave(path = figures_wd, filename = "hispanic_event_study.png", dpi = 300)

Hispanic_mod_avg       = emfx(Hispanic_mod, collapse = FALSE)

models = list(
  "Poor Health" = etwfe(
    fml  = poor_health ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    ),
    "School Lunch" = etwfe(
    fml  = school_lunch ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    ),
    "Log School Lunch" = etwfe(
    fml  = ln_schl_lunch ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    ),
    "SNAP" = etwfe(
    fml  = snap_status ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    ),
    "Log SNAP" = etwfe(
    fml  = ln_snap ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    )
    )

mod_es = list(
    "Poor Health"       = emfx(models$`Poor Health`, type = "event", collapse = FALSE),
    "School Lunch"      = emfx(models$`School Lunch`, type = "event", collapse = FALSE),
    "Log School Lunch"  = emfx(models$`Log School Lunch`, type = "event", collapse = FALSE),
    "SNAP"              = emfx(models$`SNAP`, type = "event", collapse = FALSE),
    "Log SNAP"          = emfx(models$`Log SNAP`, type = "event", collapse = FALSE)
    )

# Quick renaming function to replace ".Dtreat" with something more meaningful
rename_fn = function(old_names) {
  new_names = gsub(".Dtreat", "Years post treatment =", old_names)
  setNames(new_names, old_names)
}

f1 <- function(x) format(round(x, 3), big.mark=".")
f2 <- function(x) format(round(x, 0), big.mark=",")

gm <- list(
  list(raw = "nobs", clean = "Observations", fmt = f2),
  list(raw = "FE..first_treat", clean = "Cohort FE", fmt = 0),
  list(raw = "FE..year", clean = "Year FE", fmt = 0),
  list(raw = "std.error.type", clean = "Standard Errors", fmt = 0)
)
options(modelsummary_factory_latex = "kableExtra")
config_modelsummary(factory_default = "kableExtra")
regression_tab  <- modelsummary(
  mod_es, 
  fmt         = f1, 
  shape       = term:event:statistic ~ model,
  coef_rename = rename_fn,
  stars       = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  gof_map     = gm,
  escape      = F,
  output     = "latex",
  title       = "Extended Two Way Fixed Effects \\label{tab:etwfe}") |>
  kable_styling(latex_options = c("scale_down", "HOLD_position")) |> 
  footnote(number = c("\\\\footnotesize{Each column is the results of the extended two-way fixed effects estimation. 
                      Standard errors are clustered on the county level.}",
                      "\\\\footnotesize{The samples include first, second, third, and fourth+ generation Hispanic children ages 17 and below who live in intact families. 
                      A first-generation Hispanic child is one that is born in a Spanish-speaking country. 
                      A second-generation Hispanic child is one that is born in the United States with at least one parent born in a Spanish-speaking country.
                      Third-generation Hispanic immigrant children are native-born with native-born parents and at least one grandparent is born in a Spanish-speaking country.
                      country.
                      Fourth-generation+ are native born with native-born parents, all grandparents are born in the United States, and one parent self-reported Hispanic identity.}",
                      "\\\\footnotesize{Data source is the 1994-2019 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T)


regression_tab %>%
  save_kable(file.path(tables_wd,"tab01-etwfe.tex"))

regression_tab %>%
  save_kable(file.path(thesis_tabs,"tab01-etwfe.tex"))

### Plots
library(ggplot2)
theme_set(
  theme_minimal() + theme(panel.grid.minor = element_blank())
)

ggplot(mod_es$`Poor Health`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  labs(x = "Years post treatment", y = "Effect on poor health") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1, color = "red") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))
ggsave(path = figures_wd, filename = "poor_health_event_study.png", dpi = 300)

ggplot(mod_es$`Log School Lunch`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(col = "darkcyan") +
  labs(x = "Years post treatment", y = "Effect on log school lunch") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1, color = "red") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))
ggsave(path = figures_wd, filename = "ln_schl_lunch_event_study.png", dpi = 300)

ggplot(mod_es$`School Lunch`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(col = "darkcyan") +
  labs(x = "Years post treatment", y = "Effect on school lunch") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1, color = "red") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))
ggsave(path = figures_wd, filename = "schl_lunch_event_study.png", dpi = 300)

ggplot(mod_es$`Log SNAP`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(col = "darkcyan") +
  labs(x = "Years post treatment", y = "Effect on log SNAP") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1, color = "red") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))
ggsave(path = figures_wd, filename = "ln_snap_event_study.png", dpi = 300)

ggplot(mod_es$`SNAP`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_pointrange(col = "darkcyan") +
  labs(x = "Years post treatment", y = "Effect on SNAP") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), show.legend = FALSE, linetype= 1, size = 1.1, color = "red") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))
ggsave(path = figures_wd, filename = "snap_event_study.png", dpi = 300)

### Average effect
average_model  <- list(
    "Poor Health"       = emfx(models$`Poor Health`, collapse = FALSE),
    "School Lunch"  = emfx(models$`School Lunch`, collapse = FALSE),
    "Log School Lunch"  = emfx(models$`Log School Lunch`, collapse = FALSE),
    "Log SNAP"          = emfx(models$`Log SNAP`, collapse = FALSE),
    "SNAP"          = emfx(models$`SNAP`, collapse = FALSE)
    )
modelsummary(
  average_model,
  stars       = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  gof_omit    = "Adj|Within|IC|RMSE",
  title       = "Aggregate effect",
  notes       = "Std. errors are clustered at the county level"
)

regression_tab  <- modelsummary(
  average_model, 
  fmt         = f1, 
  stars       = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  gof_map     = gm,
  escape      = F,
  output     = "latex",
  title       = "Extended Two Way Fixed Effects: Aggregated Effects \\label{tab:etwfe-agg}") |>
  kable_styling(latex_options = c("scale_down", "HOLD_position")) |> 
  footnote(number = c("\\\\footnotesize{Each column is the results of the aggregat effects of extended two-way fixed effects estimation. 
                      Standard errors are clustered on the county level.}",
                      "\\\\footnotesize{The samples include first, second, third, and fourth+ generation Hispanic children ages 17 and below who live in intact families. 
                      A first-generation Hispanic child is one that is born in a Spanish-speaking country. 
                      A second-generation Hispanic child is one that is born in the United States with at least one parent born in a Spanish-speaking country.
                      Third-generation Hispanic immigrant children are native-born with native-born parents and at least one grandparent is born in a Spanish-speaking country.
                      country.
                      Fourth-generation+ are native born with native-born parents, all grandparents are born in the United States, and one parent self-reported Hispanic identity.}",
                      "\\\\footnotesize{Data source is the 1994-2019 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T)


regression_tab %>%
  save_kable(file.path(tables_wd,"tab02-etwfe-agg.tex"))

regression_tab %>%
  save_kable(file.path(thesis_tabs,"tab02-etwfe-agg.tex"))


mod_es_i = etwfe(
  ln_snap ~ 1, 
  tvar = year, 
  gvar = first_treat, 
  data = CPS_dates,
  ivar = county  # NEW: Use unit-level (county) FEs
  ) |>
  emfx("event", collapse = FALSE)

modelsummary(
  mod_es_i,
  shape             = term:event:statistic ~ model,
  stars             = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  coef_rename       = rename_fn,
  gof_omit          = "Adj|Within|IC|RMSE"
)
