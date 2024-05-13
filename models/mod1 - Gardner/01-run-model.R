# Control script for Gardner model

library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
library(postjags)
library(broom.mixed)


# Read in data
mrc <- read_csv("models/mod1 - Gardner/moisture-release.csv") |>
  mutate(depthID = factor(depthID))

# Quick plot
mrc |>
  ggplot() +
  geom_point(aes(x = vwc, y = abs(wp),
                 color = depthID)) +
  geom_line(aes(x = vwc, y = abs(wp),
                group = depthID,
                lty = texture)) +
  theme_bw(base_size = 14)

# Check mean of vwc
vwc <- read_csv("data_clean/neon_swcdaily.csv")
# averaged by soil pits 1&2 or 3&4
# for depths 6, 16, 26, or 56 (deepest was ignored)
# Plots 1&2 averaged
vwc |> 
  ggplot() +
  geom_point(aes(x = date, y = m_p12_6, color = "12 - 6")) +
  geom_point(aes(x = date, y = m_p12_16, color = "12 - 16")) +
  geom_point(aes(x = date, y = m_p12_26, color = "12 - 26")) +
  geom_point(aes(x = date, y = m_p12_56, color = "12 - 56"))

# Plots 3&4 averaged
vwc |> 
  ggplot() +
  geom_point(aes(x = date, y = m_p34_6, color = "34 - 6")) +
  geom_point(aes(x = date, y = m_p34_16, color = "34 - 16")) +
  geom_point(aes(x = date, y = m_p34_26, color = "34 - 26")) +
  geom_hline(yintercept = 0.04)

tapply(mrc$vwc, mrc$texture, mean) # loamy sand: 0.0272; sandy loam: 0.0365
log(tapply(mrc$vwc, mrc$texture, mean)) # -3.6, -3.3
log(0.04) # -3.218

# Use log(0.04) to center data, no need to standardize as goal is prediction


# Plot for Gardner model
mrc |> 
  ggplot(aes(x = log(vwc), y = log(abs(wp)))) +
  geom_point(aes(col = depthID)) +
  geom_vline(xintercept = log(0.04)) +
  facet_wrap(~texture)

# Data list
data_list <- list(log.y = log(abs(mrc$wp)),
                  log.x = log(mrc$vwc) - log(0.045),
                  N = nrow(mrc),
                  texture = factor(mrc$texture,
                                   levels = c("loamy sand", "sandy loam"))) 

# Initials
inits <- function() {
  list(inv.b = rnorm(2, 0, 10),
       log.a = rnorm(2, 0, 10),
       tau = runif(1, 0, 1))
}
inits_list <- list(inits(), inits(), inits())

# Or start from saved state
load("models/mod1 - Gardner/inits/inits_texture.Rdata")
# Compile model
jm <- jags.model(file = "models/mod1 - Gardner/Gardner_texture.JAGS",
                 # inits = inits_list,
                 inits = saved_state[[2]],
                 # inits = list(saved_state[[2]][[2]],
                 #              saved_state[[2]][[3]],
                 #              saved_state[[2]][[3]]),
                 data = data_list,
                 n.chains = 3)
# update(jm, 10000)
dic.samples(jm, 1000)

# Sample posterior
params <- c("deviance", "Dsum", "R2",
             "inv.b", "log.a",
            "sig", "tau", 
            "b", "a")
coda_params <- coda.samples(jm, variable.names = params,
                            n.iter = 10000,
                            thin = 10)

# View posterior
mcmcplot(coda_params, parms = c("deviance", "Dsum", "R2",
                                "inv.b", "log.a",
                                "sig", "a", "b"))
caterplot(coda_params, parms = c("a", "b"), reorder = FALSE)
caterplot(coda_params, parms = "inv.b", reorder = FALSE)
caterplot(coda_params, parms = "log.a", reorder = FALSE)

# Check for convergence
gel <- gelman.diag(coda_params, multivariate = FALSE)

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("Dsum", rowname) | grepl("R2", rowname) | grepl("deviance", rowname) )

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^sig", rowname))

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^inv.b", rowname))

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^log.a", rowname))


# Save state
# final <- initfind(coda_params, OpenBUGS = FALSE)
# final[[1]]
# saved_state <- removevars(final, variables = c(1:4, 7))
# saved_state[[1]]
# save(saved_state, file = "models/mod1 - Gardner/inits/inits_texture.Rdata")

# ind <- which(colnames(coda_params[[2]]) == "Dsum")
# mean(coda_params[[1]][,ind])
# mean(coda_params[[2]][,ind])
# mean(coda_params[[3]][,ind])

save(coda_params, file = "models/mod1 - Gardner/coda/coda_params.Rdata")

# Sample residuals
coda_resid <- coda.samples(jm,
                           variable.names = c("resid"),
                           n.iter = 10000,
                           thin = 10)

resid_sum <- tidyMCMC(coda_resid,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

resids <- cbind.data.frame(mrc, resid_sum)

resids |> 
  ggplot() +
  geom_histogram(aes(x = pred.mean)) +
  facet_grid(rows = vars(texture),
             scales = "free_x",
             space = "free")

save(coda_resid, file = "models/mod1 - Gardner/coda/coda_resid.Rdata")

# Sample predicted values
coda_pred <- coda.samples(jm,
                          variable.names = c("log.y.rep"),
                          n.iter = 3000)

pred_sum <- tidyMCMC(coda_pred,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

preds <- cbind.data.frame(mrc, pred_sum)

save(coda_pred, file = "models/mod1 - Gardner/coda/coda_pred.Rdata")

# fit
sm1 <- summary(lm(pred.mean ~ log(abs(wp)), data = preds)) # R2 = 0.974
preds |> 
  ggplot() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_abline(slope = coef(sm1)[2,1], intercept = coef(sm1)[1,1],
              lty = "longdash") +
  geom_errorbar(aes(x = log(abs(wp)), 
                    ymin = pred.lower,
                    ymax = pred.upper), 
                alpha = 0.25) +
  geom_point(aes(x = log(abs(wp)), y = pred.mean)) +
  scale_x_continuous(expression(paste("Observed log(|", Psi, "|)"))) +
  scale_y_continuous(expression(paste("Predicted log(|", Psi, "|)"))) +
  theme_bw(base_size = 14)

# On original scales
preds |> 
  ggplot() +
  # geom_hline(yintercept = 2) +
  geom_errorbar(aes(x = vwc, 
                    ymin = exp(pred.lower),
                    ymax = exp(pred.upper)),
                alpha = 0.25) +
  geom_line(aes(x = vwc, y = exp(pred.mean),
                group = texture)) +
  geom_point(aes(x = vwc, y = exp(pred.mean),
                 group = texture)) +
  geom_point(aes(x = vwc, y = abs(wp), 
                 color = factor(depthID)),
             size = 2, alpha = 0.25) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)"))) +
  theme_bw(base_size = 14) +
  guides(color = "none")

# Create predictions from site-level params
load("models/mod1 - Gardner/coda/coda_params.Rdata")
param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)


text_p <- param_sum |> 
  filter(grepl("^inv.b\\[", term) |
           grepl("^log.a\\[", term))

# Create test data frame for depths 6 and 16 cm, 
# which is span both textures, x1 = loamy sand, x2 = sandy loam
x1 <- seq(log(min(vwc$m_p34_6, na.rm = TRUE)) - log(0.04),
         log(max(vwc$m_p34_6, na.rm = TRUE)) - log(0.04),
         by = 0.01)

x2 <- seq(log(min(vwc$m_p34_16, na.rm = TRUE)) - log(0.04),
          log(max(vwc$m_p34_16, na.rm = TRUE)) - log(0.04),
          by = 0.01)

invb1 <- text_p$pred.mean[1]
loga1 <- text_p$pred.mean[3]

invb2 <- text_p$pred.mean[2]
loga2 <- text_p$pred.mean[4]

y1 <- -1*invb1*(x1 - loga1)
y2 <- -1*invb2*(x2 - loga2)

pred_m <- data.frame(x = c(x1, x2),
                     y = c(y1, y2),
                     texture = c(rep("loamy sand", length(x1)),
                                 rep("sandy loam", length(x2)))) |> 
  mutate(swp = -1*exp(y),
         vwc = exp(x + log(0.04)))

ggplot() +
  geom_line(data = pred_m, 
             aes(x = vwc,
                 y = swp,
                 group = texture))

mrc |> 
  ggplot() +
  geom_point(aes(x = vwc, y = abs(wp), 
                 color = depthID),
             alpha = 0.5) +
  geom_line(aes(x = vwc, y = abs(wp), 
                 color = depthID),
             alpha = 0.5) +
  geom_line(data = pred_m, 
            aes(x = vwc,
                y = abs(swp),
                lty = texture)) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)")),
                     limits = c(0, 8)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")
