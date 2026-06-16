# ==============================================================================
# Prepare data
# ==============================================================================

# Set working directory 
setwd("")

# Load libraries
library(tidyverse)
library(readxl)
library(umap)
library(patchwork)
library(ggforce)
library(ggtext)

# Import data and transform labels
raw_df <- read_excel("") %>%
  mutate(
    UnitType = case_when(
      UnitType == "A-HTMR" ~ "UFN",
      TRUE ~ UnitType
    ), 
    
    SubunitType = case_when(
      SubunitType == "Cool+" ~ "UFN A-PEP.KIT+",
      SubunitType == "Cool-" ~ "UFN KIT-",
      TRUE ~ SubunitType
    ),
    SensoryClass = ifelse(UnitType %in% c("UFN", "C-HTMR"), "Pain", "Touch")
  )

raw_df <- raw_df %>% mutate(
  UnitType = case_when(
    SubunitType == "UFN A-PEP.KIT+" ~ "UFN A-PEP.KIT+",
    SubunitType == "UFN KIT-" ~ "UFN KIT-",
    TRUE ~ UnitType
  )
)

# Impute hair pull missing values with column median within each Force and UnitType group
sc_cols <- colnames(raw_df)[grep("^HP_.*_SC$", colnames(raw_df))]

pf_cols <- colnames(raw_df)[grep("^HP_.*_PF$|^HP_F_.*_mN$", colnames(raw_df))] 

raw_df_imputed <- raw_df %>%
  group_by(UnitType, Condition) %>%
  mutate(across(all_of(c(sc_cols, pf_cols)), ~ {
    val <- .
    if (all(is.na(val))) {
      return(rep(0, length(val))) 
    } else {
      val[is.na(val)] <- median(val, na.rm = TRUE)
      return(val)
    }
  })) %>%
  ungroup()


# Define unit type shapes, ellipse transparency, and condition colors 
neuron_shapes <- c(
  "UFN A-PEP.KIT+" = 21,  # Circle (Fillable)
  "UFN KIT-" = 22,  # Square (Fillable)
  "C-HTMR"   = 3,   # Plus (Color only)
  "Field-LTMR" = 17, # Triangle (Solid)
  "SA-LTMR"  = 8,   # Asterisk (Color only)
  "C-LTMR"   = 23   # Diamond (Fillable)
)

condition_colors <- c("Baseline" = "#1f78b4", "Inflammation" = "#e31a1c")

hull_alpha_values <- c(
  "UFN A-PEP.KIT+.Baseline" = 0.1, "UFN A-PEP.KIT+.Inflammation" = 0.1, 
  "UFN KIT-.Baseline" = 0.1, "UFN KIT-.Inflammation" = 0.1, 
  "C-HTMR.Baseline" = 0.02,   "C-HTMR.Inflammation" = 0.02,
  "Field-LTMR.Baseline" = 0.1, "Field-LTMR.Inflammation" = 0.1,
  "SA-LTMR.Baseline" = 0.1, "SA-LTMR.Inflammation" = 0.1,
  "C-LTMR.Baseline" = 0.02,  "C-LTMR.Inflammation" = 0.02
)

neuron_labels <- c(
  "UFN A-PEP.KIT+" = "UFN<sup> A-PEP.KIT+</sup>",
  "UFN KIT-" = "UFN<sup> KIT-</sup>",
  "C-HTMR"   = "C-HTMR",
  "Field-LTMR" = "Field-LTMR",
  "SA-LTMR"  = "SA-LTMR",
  "C-LTMR"   = "C-LTMR"
)

# ==============================================================================
# Plot pain neurons (A-HTMR, C-HTMR)
# ==============================================================================

# Prepare pain data
pain_meta <- raw_df_imputed %>% filter(SensoryClass == "Pain")
pain_meta$UnitType <- factor(pain_meta$UnitType, 
                             levels = c("UFN A-PEP.KIT+", "UFN KIT-", "C-HTMR"))
pain_data <- pain_meta %>% select(where(is.numeric))

# Scale and PCA 
pain_data_scaled <- scale(pain_data[, apply(pain_data, 2, sd, na.rm = TRUE) != 0])
pca_pain <- prcomp(pain_data_scaled, center = FALSE, scale. = FALSE)

# Elbow Plot for PCA 
pc_var_pain <- pca_pain$sdev^2
pc_var_prop_pain <- pc_var_pain / sum(pc_var_pain)

plot_pain <- data.frame(
  PC = 1:length(pc_var_prop_pain),
  Variance = pc_var_prop_pain
) %>% slice(1:40) 

ggplot(plot_pain, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = 1:40) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "PCA Elbow plot - Pain",
    x = "Principal Component (PC)",
    y = "Variance explained"
  ) +
  theme_classic()


# UMAP 
set.seed(123)
umap_pain_res <- umap(
  pca_pain$x[, 1:min(15, ncol(pca_pain$x))], 
  n_neighbors = 11, min_dist = 0.8, spread = 1.1, metric = "euclidean"
)

df_pain_umap <- bind_cols(pain_meta, tibble(UMAP1 = umap_pain_res$layout[,1], UMAP2 = umap_pain_res$layout[,2]))

# Plot
p_pain_final <- ggplot(df_pain_umap, aes(x = UMAP1, y = UMAP2)) +
  geom_mark_hull(aes(group = interaction(UnitType, Condition), 
                     color = Condition, fill = Condition, 
                     alpha = interaction(UnitType, Condition)),
                 concavity = 10, expand = unit(3, "mm"), radius = unit(2, "mm"), 
                 linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  geom_point(aes(shape = UnitType, color = Condition, fill = Condition), 
             size = 4, stroke = 1) +
  scale_shape_manual(values = neuron_shapes,
                     labels = neuron_labels) +
  scale_color_manual(values = condition_colors) +
  scale_fill_manual(values = condition_colors) +
  scale_alpha_manual(values = hull_alpha_values) +
  coord_fixed(ratio = 1, clip = "off") +
  labs(title = "Pain-sensing neurons") +
  theme_void() +
  theme(
    legend.text = element_markdown(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    legend.position = "none",
    plot.margin = margin(10, 60, 40, -20) 
  )

# View plot
print(p_pain_final)

# ==============================================================================
# Plot touch neurons (C-LTMR, Field-LTMR, SA-LTMR)
# ==============================================================================

# Prepare touch data
touch_meta <- raw_df_imputed %>% filter(SensoryClass == "Touch")
touch_meta$UnitType <- factor(touch_meta$UnitType, 
                             levels = c("Field-LTMR", "SA-LTMR", "C-LTMR"))
touch_data <- touch_meta %>% select(where(is.numeric))

# Scale and PCA 
touch_data_scaled <- scale(touch_data[, apply(touch_data, 2, sd, na.rm = TRUE) != 0])
pca_touch <- prcomp(touch_data_scaled, center = FALSE, scale. = FALSE)

# Elbow Plot for PCA 
pc_var_touch <- pca_touch$sdev^2
pc_var_prop_touch <- pc_var_touch / sum(pc_var_touch)

plot_touch <- data.frame(
  PC = 1:length(pc_var_prop_touch),
  Variance = pc_var_prop_touch
) %>% slice(1:40) 

ggplot(plot_touch, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  scale_x_continuous(breaks = 1:40) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "PCA Elbow plot - Touch",
    x = "Principal Component (PC)",
    y = "Variance explained"
  ) +
  theme_classic()


# UMAP 
set.seed(123)
umap_touch_res <- umap(
  pca_touch$x[, 1:min(15, ncol(pca_touch$x))], 
  n_neighbors = 11, min_dist = 0.3, spread = 1, metric = "euclidean"
)

df_touch_umap <- bind_cols(touch_meta, tibble(UMAP1 = umap_touch_res$layout[,1], UMAP2 = umap_touch_res$layout[,2]))

# Plot
p_touch_final <- ggplot(df_touch_umap, aes(x = UMAP1, y = UMAP2)) +
  geom_mark_hull(aes(group = interaction(UnitType, Condition), 
                     color = Condition, fill = Condition, 
                     alpha = interaction(UnitType, Condition)),
                 concavity = 10, expand = unit(3, "mm"), radius = unit(2, "mm"), 
                 linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  geom_point(aes(shape = UnitType, color = Condition, fill = Condition), 
             size = 4, stroke = 1) +
  scale_shape_manual(values = neuron_shapes) +
  scale_color_manual(values = condition_colors) +
  scale_fill_manual(values = condition_colors) +
  scale_alpha_manual(values = hull_alpha_values) +
  coord_fixed(ratio = 1, clip = "off") +
  labs(title = "Touch-sensing neurons") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    legend.position = "none",
    plot.margin = margin(10, -20, 40, 60) 
  )

# View plot
print(p_touch_final)


# ==============================================================================
# Shared legend
# ==============================================================================

# Neuron type order
desired_order <- c("UFN A-PEP.KIT+", "UFN KIT-", "C-HTMR", "Field-LTMR", "SA-LTMR", "C-LTMR")

# Make a false plot
p_master_leg <- ggplot(raw_df_imputed, aes(x=1, y=1)) +
  geom_point(aes(shape = UnitType, color = Condition, fill = Condition), size = 4) +
  scale_shape_manual(values = neuron_shapes, name = "Neuron Type", 
                     labels = neuron_labels, 
                     breaks = desired_order) +
  scale_color_manual(values = condition_colors, name = "Condition") +
  scale_fill_manual(values = condition_colors) +
  theme_minimal() +
  theme(
    legend.text = element_markdown() 
  ) +
  guides(
    fill = "none", 
    shape = guide_legend(
      order = 1, 
      override.aes = list(color = "black", fill = "black")
    ),
    color = guide_legend(
      order = 2, 
      override.aes = list(shape = 15, size = 5)
    )
  )

# Extract the legend from the false plot
shared_legend <- cowplot::get_legend(p_master_leg)


# ==============================================================================
# UMAP axes
# ==============================================================================

# Plot
p_axis_final <- ggplot() +
  geom_segment(aes(x = 0, xend = 3, y = 0, yend = 0), 
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 3), 
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"), linewidth = 1) +
  annotate("text", x = 1.5, y = -1.2, label = "UMAP 1", fontface = "italic", size = 4) +
  annotate("text", x = -1.2, y = 1.5, label = "UMAP 2", fontface = "italic", size = 4, angle = 90) +
  coord_fixed(clip = "off") +
  theme_void() +
  xlim(-2, 4) + ylim(-2, 4) 

# View plot
print(p_axis_final)

# ==============================================================================
# Combine plots and save
# ==============================================================================

# Combine clusters
main_clusters <- (
  p_pain_final + theme(plot.margin = margin(10, -10, 10, -40)) + 
    p_touch_final + theme(plot.margin = margin(10, -10, 10, -10))
) + plot_layout(widths = c(1, 1))

# Combine clusters, axes, and legend
final_assembly <- (
  main_clusters / 
    (p_axis_final + plot_spacer()) + plot_layout(heights = c(1, 0.15))
) | shared_legend

# Save the plot
ggsave("UMAP.png", 
       final_assembly + plot_layout(widths = c(1, 0.08)), 
       width = 13.5, height = 7.5, dpi = 600)

