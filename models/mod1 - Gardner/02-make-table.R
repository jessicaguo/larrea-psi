# Invert Gardner model
# Run through with range of SWC and posterior parameter sets
# for _texture model version, with 2 sets of predictions
# loamy sand and sandy loam 
library(tidyverse)
library(coda)
library(udunits2)
library(broom.mixed)

# Define inverse function
# Input SWC is [theta.r, theta.s]
# Oputputu is SWP in MPa
VWC_to_SWP <- function(vwc, invb, loga) {
  if(sum(vwc < 1 & vwc > 0) == length(vwc)) {
    # Convert vwc to log and centered scale
    y = -1*invb*(log(vwc) - log(0.04) - loga)
    # Return soil water potential in MPa
    return(-1 * exp(y))
  } else {
    print("Make sure VWC is in units of cm^3 cm^-3")
  }
}

VWC_to_SWP_vec <- Vectorize(FUN = VWC_to_SWP, 
                            vectorize.args = c("invb", "loga"),
                            SIMPLIFY = TRUE)

# Load parameters
load("models/mod1 - Gardner/coda/coda_params.Rdata")

# Calculate posterior means
text_df <- tidyMCMC(coda_params, 
                    conf.int =  TRUE, 
                    conf.method = "HPDinterval",
                    conf.level = 0.95) |> 
  filter(grepl("inv.b\\[", term) | grepl("log.a\\[", term)) |> 
  mutate(term_original = term) |> 
  tidyr::separate(term, into = c("term", "term2", "type", "discard")) |> 
  mutate(term = paste0(term, ".", term2),
         texture = case_when(type == 1 ~ "loamy sand",
                          type == 2 ~ "sandy loam")) |> 
  select(-term2, -discard)

# Test single curve with posterior means
test <- data.frame(vwc_in = seq(0.015, 0.23, .0001))
foo <- VWC_to_SWP_vec(test$vwc_in, text_df$estimate[text_df$term == "inv.b"], text_df$estimate[text_df$term == "log.a"])
test2 <- cbind.data.frame(test, foo)
colnames(test2)[2:3] <- c("swp_out_1", "swp_out_2") # loamy sand and sandy loam

test2 |> 
  ggplot(aes(x = vwc_in)) +
  geom_point(aes(y = swp_out_1,
                 color = "loamy sand")) +
  geom_point(aes(y = swp_out_2,
                 color = "sandy loam")) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)"))) +
  theme_bw(base_size = 12)

# Apply for each of 3000 iterations
# Assemble texture-level parameters
coda_1 <- rbind.data.frame(coda_params[[1]], coda_params[[2]], coda_params[[3]]) %>%
  select(filter(text_df, type == "1")$term_original)

coda_2 <- rbind.data.frame(coda_params[[1]], coda_params[[2]], coda_params[[3]]) %>%
  select(filter(text_df, type == "2")$term_original)

# Custom functions
vwc_apply <- function(vec, vwc) { # vector of parameters c(inv.b, log.a)
    # Convert vwc to log and centered scale
  y = -1*vec[1]*(log(vwc) - log(0.04) - vec[2])
  # Return soil water potential in MPa
  return(-1 * exp(y))
}
cnt <- function(x) {sum(!is.na(x))}

# Calculate swp for all sets of parameters


out_1 <- apply(coda_1, MARGIN = 1, FUN = vwc_apply,
               vwc = seq(0.015, 0.45, .0001))

out_2 <- apply(coda_2, MARGIN = 1, FUN = vwc_apply,
               vwc = seq(0.015, 0.45, .0001))

# Summarize to number, median, and central 50th percentile
out_df <- cbind.data.frame(vwc  = seq(0.015, 0.45, .0001),
                           n = apply(out_1, 1, FUN = cnt),
                           `1_SWP_MPa_50` = apply(out_1, 1, FUN = median, na.rm = TRUE),
                           `1_SWP_MPa_25` = apply(out_1, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           `1_SWP_MPa_75` = apply(out_1, 1, FUN = quantile, probs = 0.75, na.rm = TRUE),
                           `2_SWP_MPa_50` = apply(out_2, 1, FUN = median, na.rm = TRUE),
                           `2_SWP_MPa_25` = apply(out_2, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           `2_SWP_MPa_75` = apply(out_2, 1, FUN = quantile, probs = 0.75, na.rm = TRUE)) %>%
  filter(vwc < 0.45)

# Plot with error for sanity check
out_df |> 
  filter(vwc < 0.04) %>%
  ggplot() +
  geom_errorbar(aes(x = vwc,
                    ymin = `1_SWP_MPa_25`,
                    ymax = `1_SWP_MPa_75`),
                width = 0,
                alpha = 0.15,
                color = "forestgreen") +
  geom_point(aes(x = vwc,
                 y = `1_SWP_MPa_50`),
             color = "forestgreen",
             size = 0.1) +
  geom_errorbar(aes(x = vwc,
                    ymin = `2_SWP_MPa_25`,
                    ymax = `2_SWP_MPa_75`),
                width = 0,
                alpha = 0.15,
                color = "coral") +
  geom_point(aes(x = vwc,
                 y = `2_SWP_MPa_50`),
             color = "coral",
             size = 0.1) +
  theme_bw(base_size = 14)

#### Create lookup table based on predictions all predictions
# Include the median and central 50th percentile

lookup <- out_df %>%
  filter(n > 1000) # all vwc's have values

# Save
save(lookup, file = "source/mrc_lookup.Rdata")

