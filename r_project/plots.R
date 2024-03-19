library(ggplot2)
library(PerformanceAnalytics)

# Supondo que seu dataframe se chama 'sol'
# e você tem as variáveis preditoras já definidas

################################################################################
# chart correlation
chart.Correlation(sol, histogram = TRUE)
################################################################################



################################################################################
# Primeiro, remodelar os dados para um formato longo
sol_longo <- reshape2::melt(sol, id.vars = "sol_close", 
                           measure.vars = c("last_day_btc_high", "last_day_btc_low", 
                                            "last_day_btc_close", "last_day_btc_marketcap"))

# Criar gráficos de dispersão
ggplot(sol_longo, aes(x = value, y = sol_close)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Relação entre Variáveis Preditoras e SOL Close", 
       x = "Valor da Variável Preditora", 
       y = "SOL Close") +
  theme_minimal()
################################################################################



################################################################################