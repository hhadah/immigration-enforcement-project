# This a script to 
# run etwfe regressions

# Date: May 7th, 2024

### Load data
CPS_dates <- read_csv(file.path(big_data_dir,"CPS_dates_county.csv")) |> 
    filter(year <= 2019)|> 
    filter(Type == "Third Generation")

CPS_dates  <- CPS_dates |>
  mutate(OneHispanic = case_when(Grandparent_Type == "HWWW" ~ 1,
                                 Grandparent_Type == "WHWW" ~ 1,
                                 Grandparent_Type == "WWHW" ~ 1,
                                 Grandparent_Type == "WWWH" ~ 1,
                                 TRUE ~ 0),
         TwoHispanic = case_when(Grandparent_Type == "HHWW" ~ 1,
                                 Grandparent_Type == "HWHW" ~ 1,
                                 Grandparent_Type == "HWWH" ~ 1,
                                 Grandparent_Type == "WHHW" ~ 1,
                                 Grandparent_Type == "WWHH" ~ 1,
                                 Grandparent_Type == "WHWH" ~ 1,
                                 TRUE ~ 0),
         ThreeHispanic = case_when(Grandparent_Type == "HHHW" ~ 1,
                                   Grandparent_Type == "HHWH" ~ 1,
                                   Grandparent_Type == "HWHH" ~ 1,
                                   Grandparent_Type == "WHHH" ~ 1,
                                 TRUE ~ 0),
         FourHispanic = case_when(Grandparent_Type == "HHHH" ~ 1,
                                 TRUE ~ 0))
CPS_dates  <- CPS_dates |>
  filter(FourHispanic == 1)
  
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
Hispanic_mod_avg       = emfx(Hispanic_mod, collapse = FALSE)

ggplot(Hispanic_mod_es, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "black") +  # Horizontal line in black
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on Hispanic Identity") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = Hispanic_mod_avg$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = Hispanic_mod_avg$estimate, label = sprintf("ETWFE Estimate = %.2f (%.2f)", Hispanic_mod_avg$estimate, Hispanic_mod_avg$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation

ggsave(path = figures_wd, filename = "thirdgen-four-hispanic_event_study.png", dpi = 300)

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
    ),
    "Poverty Line" = etwfe(
    fml  = poverty_line ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    ),
    "Food Insecure" = etwfe(
    fml  = food_insecure ~ 1, # outcome ~ controls
    tvar = year,            # time variable
    gvar = first_treat,     # group variable
    data = CPS_dates,       # dataset
    vcov = ~county,         # vcov adjustment (here: clustered)
    fe = "feo"              # fixed effects
    ),
    "Food Insecure Child" = etwfe(
    fml  = food_insecure_child ~ 1, # outcome ~ controls
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
    "Log SNAP"          = emfx(models$`Log SNAP`, type = "event", collapse = FALSE),
    "Poverty Line"      = emfx(models$`Poverty Line`, type = "event", collapse = FALSE),
    "Food Insecure"     = emfx(models$`Food Insecure`, type = "event", collapse = FALSE),
    "Food Insecure Child" = emfx(models$`Food Insecure Child`, type = "event", collapse = FALSE)
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
modelsummary(
  mod_es, 
  fmt         = f1, 
  shape       = term:event:statistic ~ model,
  coef_rename = rename_fn,
  stars       = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  gof_map     = gm,
  escape      = F,
  output     = "kableExtra",
  title       = "Extended Two Way Fixed Effects: Third Generation and Four Hispanic Grandparents \\label{tab:etwfe-thirdgen-four}") |>
  kable_styling(latex_options = c("scale_down", "HOLD_position")) |> 
  footnote(number = c("\\\\footnotesize{Each column is the results of the extended two-way fixed effects estimation. 
                      Standard errors are clustered on the county level.}",
                      "\\\\footnotesize{The samples include third-generation Hispanic children with four Spanish-born grandparents ages 17 and below who live in intact families. 
                      Third-generation Hispanic immigrant children are native-born with native-born parents and at least one grandparent is born in a Spanish-speaking country.
                      }",
                      "\\\\footnotesize{Data source is the 1994-2019 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T)
regression_tab  <- modelsummary(
  mod_es, 
  fmt         = f1, 
  shape       = term:event:statistic ~ model,
  coef_rename = rename_fn,
  stars       = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  gof_map     = gm,
  escape      = F,
  output     = "latex",
  title       = "Extended Two Way Fixed Effects: Third Generation and Four Hispanic Grandparents \\label{tab:etwfe-thirdgen-four}") |>
  kable_styling(latex_options = c("scale_down", "HOLD_position")) |> 
  footnote(number = c("\\\\footnotesize{Each column is the results of the extended two-way fixed effects estimation. 
                      Standard errors are clustered on the county level.}",
                      "\\\\footnotesize{The samples include third-generation Hispanic children with four Spanish-born grandparents ages 17 and below who live in intact families. 
                     Third-generation Hispanic immigrant children are native-born with native-born parents and at least one grandparent is born in a Spanish-speaking country."),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T)


regression_tab %>%
  save_kable(file.path(tables_wd,"tab21-etwfe-third-four.tex"))

regression_tab %>%
  save_kable(file.path(thesis_tabs,"tab21-etwfe-third-four.tex"))

### Average effect
average_model  <- list(
    "Poor Health"       = emfx(models$`Poor Health`, collapse = FALSE),
    "School Lunch"  = emfx(models$`School Lunch`, collapse = FALSE),
    "Log School Lunch"  = emfx(models$`Log School Lunch`, collapse = FALSE),
    "Log SNAP"          = emfx(models$`Log SNAP`, collapse = FALSE),
    "SNAP"          = emfx(models$`SNAP`, collapse = FALSE),
    "Poverty Line"      = emfx(models$`Poverty Line`, collapse = FALSE),
    "Food Insecure"     = emfx(models$`Food Insecure`, collapse = FALSE),
    "Food Insecure Child" = emfx(models$`Food Insecure Child`, collapse = FALSE)
    )
modelsummary(
  average_model,
  stars       = c('***' = 0.01, '**' = 0.05, '*' = 0.1),
  gof_map     = gm,
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
  title       = "Extended Two Way Fixed Effects: Aggregated Effects \\label{tab:etwfe-agg-thirdgen-four}") |>
  kable_styling(latex_options = c("scale_down", "HOLD_position")) |> 
  footnote(number = c("\\\\footnotesize{Each column is the results of the aggregat effects of extended two-way fixed effects estimation. 
                      Standard errors are clustered on the county level.}",
                      "\\\\footnotesize{The samples include third-generation Hispanic children with four Spanish-born grandparents ages 17 and below who live in intact families. 
                      Third-generation Hispanic immigrant children are native-born with native-born parents and at least one grandparent is born in a Spanish-speaking country.}",
                      "\\\\footnotesize{Data source is the 1994-2019 Current Population Survey.}"),
           footnote_as_chunk = F, title_format = c("italic"),
           escape = F, threeparttable = T, fixed_small_size = T)


regression_tab %>%
  save_kable(file.path(tables_wd,"tab22-etwfe-agg-third-four.tex"))

regression_tab %>%
  save_kable(file.path(thesis_tabs,"tab22-etwfe-agg-third-four.tex"))

### Plots
library(ggplot2)
theme_set(
  theme_minimal() + theme(panel.grid.minor = element_blank())
)

##################
# Event Study Plots
##################

## Poor Health
ggplot(mod_es$`Poor Health`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  labs(x = "Years post treatment", y = "Effect on poor health") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`Poor Health`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`Poor Health`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`Poor Health`$estimate, average_model$`Poor Health`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot81-poor_health_event_study-third-four.png", dpi = 300)

## Log School Lunch
ggplot(mod_es$`Log School Lunch`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on log school lunch") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`Log School Lunch`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`Log School Lunch`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`Log School Lunch`$estimate, average_model$`Log School Lunch`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot82-ln_schl_lunch_event_study-third-four.png", dpi = 300)

## School Lunch
ggplot(mod_es$`School Lunch`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on school lunch") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`School Lunch`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`School Lunch`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`School Lunch`$estimate, average_model$`School Lunch`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot83-schl_lunch_event_study-third-four.png", dpi = 300)

## Log SNAP
ggplot(mod_es$`Log SNAP`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on log SNAP") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`Log SNAP`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`Log SNAP`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`Log SNAP`$estimate, average_model$`Log SNAP`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot84-ln_snap_event_study-third-four.png", dpi = 300)

## SNAP
ggplot(mod_es$`SNAP`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on SNAP") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`SNAP`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`SNAP`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`SNAP`$estimate, average_model$`SNAP`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot85-snap_event_study-third-four.png", dpi = 300)

## Poverty Line
ggplot(mod_es$`Poverty Line`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on poverty line") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`Poverty Line`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`Poverty Line`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`Poverty Line`$estimate, average_model$`Poverty Line`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot86-poverty_line_event_study-third-four.png", dpi = 300)

## Food Insecure
ggplot(mod_es$`Food Insecure`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on food insecure") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`Food Insecure`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`Food Insecure`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`Food Insecure`$estimate, average_model$`Food Insecure`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot87-food_insecure_event_study-third-four.png", dpi = 300)

## Food Insecure Child
ggplot(mod_es$`Food Insecure Child`, aes(x = event, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Error bars in black
  geom_point(size = 3, color = "black") +  # Points in red
  labs(x = "Years post treatment", y = "Effect on food insecure child") +
  theme_customs()+  
  geom_hline(yintercept = 0, colour = "black", linetype = "dotted") +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.title = element_text(color = "black",  size = 9)) +
  theme(plot.title = ggtext::element_markdown(size = 9, color = "black", hjust = 0, lineheight = 1.2))+
  geom_hline(yintercept = average_model$`Food Insecure Child`$estimate, colour = "red", linetype = "dashed") +  # Dashed red line
  geom_text(aes(x = Inf, y = average_model$`Food Insecure Child`$estimate, label = sprintf("ETWFE Estimate = %.4f (%.4f)", average_model$`Food Insecure Child`$estimate, average_model$`Food Insecure Child`$std.error)),
            hjust = 1.1, vjust = -1, color = "black")  # Text annotation
ggsave(path = figures_wd, filename = "plot88-food_insecure_child_event_study-third-four.png", dpi = 300)
