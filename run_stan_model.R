library(tidyverse)
library(rstan)
library(tidybayes)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


###########################################################################
d <- readxl::read_excel("./data/fig1c.xlsx") %>% 
  mutate(
    Val  = as.numeric(Val),
    cpm  = as.numeric(cpm),
    low  = as.numeric(low),
    high = as.numeric(high),
    ymin = pmin(cpm, low, high, na.rm = TRUE),  # 原始数据有误，所以选择最小的那个当 ymin
    ymax = pmax(cpm, low, high, na.rm = TRUE)   # 同理
  )

d |> 
  ggplot(aes(x = Val, y = cpm)) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    width = 0.12,
    linewidth = 0.8
  ) +
  geom_point(size = 4.2) +
  scale_x_log10() 

###########################################################################




###########################################################################
x_new <- 10^seq(-3, 5, length.out = 500)

stan_data <- list(
  N = nrow(d),
  x = d$Val,
  y = d$cpm,
  
  N_new = length(x_new),
  x_new = x_new
)

stan_data
###########################################################################




###########################################################################
mod <- stan_model("./stan/four_pl_binding.stan")

fit <- mod %>% 
  sampling(
    data    = stan_data,
    chains  = 4,
    iter    = 4000,
    warmup  = 2000,
    seed    = 1024,
    control = list(adapt_delta = 0.99, max_treedepth = 12)
)
###########################################################################






###########################################################################
# 用stan模型结果，重新画出图形

fit %>% 
  tidybayes::gather_draws(mu_new[i]) %>% 
  ggdist::mean_qi() %>% 
  mutate(Val = x_new[i])


curve_df <- fit %>% 
  tidybayes::gather_draws(mu_new[i]) %>% 
  group_by(i) %>% 
  summarise(
    mean   = mean(.value),
    median = median(.value),
    q025   = quantile(.value, 0.025),
    q975   = quantile(.value, 0.975),
    .groups = "drop"
  ) %>% 
  mutate(Val = x_new[i])

head(curve_df)



ggplot() +
  geom_ribbon(
    data = curve_df,
    aes(x = Val, ymin = q025, ymax = q975),
    alpha = 0.15
  ) +
  geom_line(
    data = curve_df,
    aes(x = Val, y = median),
    linewidth = 0.9,
    color = "red"
  ) +
  geom_errorbar(
    data = d,
    aes(x = Val, ymin = ymin, ymax = ymax),
    width = 0.12,
    linewidth = 0.8,
    color = "black"
  ) +
  geom_point(
    data = d,
    aes(x = Val, y = cpm),
    size = 4.2,
    color = "black"
  ) +
  scale_x_log10(
    limits = c(1e-3, 1e5),
    breaks = c(1e-3, 1e-1, 1e1, 1e3, 1e5),
    labels = c("0.001", "0.1", "10", "1,000", "100,000")
  ) +
  scale_y_continuous(
    limits = c(-2000, 30000),
    breaks = c(0, 10000, 20000, 30000),
    labels = scales::comma
  ) +
  annotate(
    "text",
    x = 0.02,
    y = 28500,
    label = expression(italic(K)[d] == 1.95 ~ mu * M),
    parse = TRUE,
    size = 8
  ) +
  labs(
    x = "Valine (μM)",
    y = expression("["^3*"H]Val bound (c.p.m.)")
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.background   = element_rect(fill = "grey94", color = NA),
    panel.background  = element_rect(fill = "grey94", color = NA),
    axis.line         = element_line(linewidth = 1.1, color = "black"),
    axis.ticks        = element_line(linewidth = 1.1, color = "black"),
    axis.ticks.length = unit(0.28, "cm"),
    axis.text         = element_text(color = "black", size = 18),
    axis.title        = element_text(color = "black", size = 22),
    axis.title.x      = element_text(margin = margin(t = 18)),
    axis.title.y      = element_text(margin = margin(r = 18)),
    plot.margin       = margin(15, 30, 20, 20)
  )
###########################################################################