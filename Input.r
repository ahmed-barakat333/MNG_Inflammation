# Set working directory
setwd("")

# Load libraries
library(tidyverse)
library(readxl)
library(openxlsx)

################################################################################
################################################################################

# Analyze indentation data
indentation_df <- read_excel("")  %>%
  mutate(
    UnitType = case_when(
      UnitType == "A-HTMR" ~ "UFN",
      UnitType %in% c("Field-LTMR", "SA-LTMR") ~ "A-LTMR",
      TRUE ~ UnitType
    )
  )

indentation_long <- indentation_df %>%
  select(-SubunitType) %>%
  mutate(UnitID = row_number()) %>% 
  pivot_longer(-c(UnitType, Condition, UnitID), names_to = "Stimulus", values_to = "SpikeCount") %>%
  mutate(
    Force_mN = as.numeric(gsub("F_|_mN", "", Stimulus)),
    ForceDomain = ifelse(Force_mN <= 100, "Low intensity", "High intensity")
  )

# Normalize by unit N to get mean spikes (Boada method)
indentation_norm <- indentation_long %>%
  group_by(UnitType, Condition, ForceDomain) %>%
  summarise(
    MeanSpikesPerUnit = sum(SpikeCount, na.rm=T) / n_distinct(UnitID), 
    .groups = "drop"
  ) %>%
  group_by(Condition, ForceDomain) %>%
  mutate(NormInput = MeanSpikesPerUnit / sum(MeanSpikesPerUnit)) %>%
  ungroup() %>%
  mutate(ForceDomain = factor(ForceDomain, levels = c("Low intensity", "High intensity")))

# Arrange unit types 
target_order <- c("A-LTMR", "C-LTMR", "C-HTMR", "UFN")

# Apply order
indentation_norm <- indentation_norm %>%
  mutate(UnitType = factor(UnitType, levels = target_order))

# Choose colors for the plot
sensory_colors <- c(
  "UFN"     = "#EE4B2B", # Bright Red (Pain )
  "C-HTMR"     = "#FFBF00", # Amber/Orange (Pain )
  "A-LTMR" = "#89CFF0", # Baby Blue (Touch)
  #  "SA-LTMR"    = "#0047AB", # Cobalt Blue (Touch)
  "C-LTMR"     = "#40B5AD"  # Seafoam/Teal (Touch)
)

# Plot normalized input
ggplot(indentation_norm, aes(x = Condition, y = NormInput * 100 , fill = UnitType)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
  
  # Add percentage labels
  # Only show labels if the percentage is > 5% to avoid cluttering small segments
  geom_text(aes(label = ifelse(NormInput > 0.05, paste0(round(NormInput * 100), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, 
            fontface = "plain",
            color = "white") + 
  
  # Styling and faceting
  facet_wrap(~ ForceDomain) +
  scale_fill_manual(values = sensory_colors) +
  labs(title = "Indentation", 
       y = "Percentage of mechanoreceptor input (spike count)", 
       x = "", 
       fill = "Afferent type") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(), 
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "plain"),
        # Rotated x-axis for cleaner look if Condition names are long
        axis.text.x = element_text(color = "black"))

ggsave("", width = 7, height = 7, dpi = 300)

################################################################################
################################################################################

# Analyze brush data

brush_df <- read_excel("") %>%
  mutate(
    UnitType = case_when(
      UnitType == "A-HTMR" ~ "UFN",
      UnitType %in% c("Field-LTMR", "SA-LTMR") ~ "A-LTMR",
      TRUE ~ UnitType
    )
  )

brush_long <- brush_df %>%
  select(-SubunitType) %>%
  mutate(UnitID = row_number()) %>% 
  pivot_longer(-c(UnitType, Condition, UnitID), names_to = "Stimulus", values_to = "SpikeCount") %>%
  mutate(
    ForceDomain = case_when(
      str_detect(Stimulus, "Soft") ~ "Low intensity",
      str_detect(Stimulus, "Rough") ~ "High intensity"
    )
  ) 

# Normalize by unit N to get mean spikes (Boada method)
brush_norm <- brush_long %>%
  group_by(UnitType, Condition, ForceDomain) %>%
  summarise(
    MeanSpikesPerUnit = sum(SpikeCount, na.rm=T) / n_distinct(UnitID),
    .groups = "drop"
  ) %>%
  group_by(Condition, ForceDomain) %>%
  mutate(NormInput = MeanSpikesPerUnit / sum(MeanSpikesPerUnit)) %>%
  ungroup() %>%
  mutate(ForceDomain = factor(ForceDomain, levels = c("Low intensity", "High intensity")))


# Apply order
brush_norm <- brush_norm %>%
  mutate(UnitType = factor(UnitType, levels = target_order))


# Plot normalized input
ggplot(brush_norm, aes(x = Condition, y = NormInput * 100, fill = UnitType)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
  
  # Add percentage labels
  # Only show labels if the percentage is > 5% to avoid cluttering small segments
  geom_text(aes(label = ifelse(NormInput > 0.05, paste0(round(NormInput * 100), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, 
            fontface = "plain",
            color = "white") + 
  
  # Styling and faceting
  facet_wrap(~ ForceDomain) +
  scale_fill_manual(values = sensory_colors) +
  labs(title = "Brush", 
       y = "Percentage of mechanoreceptor input (spike count)", 
       x = "", 
       fill = "Afferent type") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(), 
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "plain"),
        # Rotated x-axis for cleaner look if Condition names are long
        axis.text.x = element_text(color = "black"))


ggsave("", width = 7, height = 7, dpi = 300)

################################################################################
################################################################################

# Analyze hair pull data
hairpull_df <- read_excel("") %>%
  mutate(
    UnitType = case_when(
      UnitType == "A-HTMR" ~ "UFN",
      UnitType %in% c("Field-LTMR", "SA-LTMR") ~ "A-LTMR",
      TRUE ~ UnitType
    )
  )

hairpull_long <- hairpull_df %>%
  select(-SubunitType) %>%
  mutate(UnitID = row_number()) %>% 
  pivot_longer(-c(UnitType, Condition, UnitID), names_to = "Stimulus", values_to = "SpikeCount") %>%
  mutate(
    Force_mN = as.numeric(gsub("F_|_mN", "", Stimulus)),
    ForceDomain = ifelse(Force_mN <= 50, "Low intensity", "High intensity")
  )

# Normalize by unit N to get mean spikes (Boada method)
hairpull_norm <- hairpull_long %>%
  group_by(UnitType, Condition, ForceDomain) %>%
  summarise(
    MeanSpikesPerUnit = sum(SpikeCount, na.rm=T) / n_distinct(UnitID),
    .groups = "drop"
  ) %>%
  group_by(Condition, ForceDomain) %>%
  mutate(NormInput = MeanSpikesPerUnit / sum(MeanSpikesPerUnit)) %>%
  ungroup() %>%
  mutate(ForceDomain = factor(ForceDomain, levels = c("Low intensity", "High intensity")))

# Apply order
hairpull_norm <- hairpull_norm %>%
  mutate(UnitType = factor(UnitType, levels = target_order))


# Plot normalized input
ggplot(hairpull_norm, aes(x = Condition, y = NormInput * 100, fill = UnitType)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.3) +
  
  # Add percentage labels
  # Only show labels if the percentage is > 5% to avoid cluttering small segments
  geom_text(aes(label = ifelse(NormInput > 0.05, paste0(round(NormInput * 100), "%"), "")), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, 
            fontface = "plain",
            color = "white") + 
  
  # Styling and faceting
  facet_wrap(~ ForceDomain) +
  scale_fill_manual(values = sensory_colors) +
  labs(title = "Hair pull", 
       y = "Percentage of mechanoreceptor input (spike count)", 
       x = "", 
       fill = "Afferent type") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(), 
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "plain"),
        # Rotated x-axis for cleaner look if Condition names are long
        axis.text.x = element_text(color = "black"))


ggsave("", width = 7, height = 7, dpi = 300)


################################################################################
################################################################################


# Analyze proportion changes of UFNs between baseline and inflammation

library(betareg)
library(emmeans)

# Filter for just UFNs to see if their specific share of input changed
ufn_only <- combined_df %>% 
  filter(UnitType == "UFN", !Modality %in% c("Cooling", "Heating"))

# Beta regression model
fit_beta <- betareg(NormInput ~ Condition + Modality + ForceDomain, data = ufn_only)
summary(fit_beta)




# Calculate the overall effect of condition 
overall_preds <- emmeans(fit_beta, ~ Condition, type = "response") %>%
  as.data.frame()

if("response" %in% colnames(overall_preds)) overall_preds$prob <- overall_preds$response
if("emmean" %in% colnames(overall_preds)) overall_preds$prob <- overall_preds$emmean

if("asymp.LCL" %in% colnames(overall_preds)) overall_preds$low <- overall_preds$asymp.LCL
if("lower.CL" %in% colnames(overall_preds)) overall_preds$low <- overall_preds$lower.CL

if("asymp.UCL" %in% colnames(overall_preds)) overall_preds$high <- overall_preds$asymp.UCL
if("upper.CL" %in% colnames(overall_preds)) overall_preds$high <- overall_preds$upper.CL

# Create regression line plot
ggplot(overall_preds, aes(x = Condition, y = prob * 100, group = 1)) +
  geom_line(color = "black", linewidth = 1.2) +
  
  geom_ribbon(aes(ymin = low * 100, ymax = high * 100), alpha = 0.2) +
  
  geom_point(aes(color = Condition), size = 6) +
  
  annotate("text", x = 1.5, y = max(overall_preds$high * 110), 
           label = "p = 3.07e-11", size = 5, fontface = "italic") +
  
  scale_color_manual(values = c("Baseline" = "#89CFF0", "Inflammation" = "#EE4B2B")) +
  theme_classic(base_size = 16) +
  labs(title = "Effect of inflammation on UFN relative input",
       y = "Predicted contribution to total input (%)",
       x = "") +
  theme(legend.position = "none",
        plot.title = element_text(face = "plain"))

ggsave("", width = 6, height = 6, dpi = 300)

