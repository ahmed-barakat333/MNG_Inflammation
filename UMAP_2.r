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

# Remove hair pull data
raw_df <- raw_df %>% 
  select(!starts_with("HP_"))

# Isolate only Pain neurons right away to keep the environment clean
pain_df <- raw_df %>% filter(SensoryClass == "Pain")


# Define unit type shapes, ellipse transparency, condition colors, and order
desired_order <- c("UFN A-PEP.KIT+", "UFN KIT-", "C-HTMR")

neuron_shapes <- c(
  "UFN A-PEP.KIT+" = 21,  # Circle (Fillable)
  "UFN KIT-" = 22,        # Square (Fillable)
  "C-HTMR"   = 3          # Plus (Color only)
)

condition_colors <- c(
  "Baseline" = "#1f78b4", 
  "Inflammation" = "#e31a1c", 
  "Diclofenac_Baseline" = "#33a02c", 
  "Diclofenac_Inflammation" = "#ff7f00"
)

condition_order <- c(
  "Baseline", 
  "Inflammation", 
  "Diclofenac_Baseline", 
  "Diclofenac_Inflammation"
)

hull_alpha_values <- c(
  "UFN A-PEP.KIT+.Baseline" = 0.1, "UFN A-PEP.KIT+.Inflammation" = 0.1, 
  "UFN KIT-.Baseline" = 0.1, "UFN KIT-.Inflammation" = 0.1, 
  "C-HTMR.Baseline" = 0.02,   "C-HTMR.Inflammation" = 0.02,
  
  "UFN A-PEP.KIT+.Diclofenac_Baseline" = 0.1, "UFN A-PEP.KIT+.Diclofenac_Inflammation" = 0.1,
  "UFN KIT-.Diclofenac_Baseline" = 0.1, "UFN KIT-.Diclofenac_Inflammation" = 0.1,
  "C-HTMR.Diclofenac_Baseline" = 0.02, "C-HTMR.Diclofenac_Inflammation" = 0.02
)

neuron_labels <- c(
  "UFN A-PEP.KIT+" = "UFN<sup> A-PEP.KIT+</sup>",
  "UFN KIT-" = "UFN<sup> KIT-</sup>",
  "C-HTMR"   = "C-HTMR"
)

# ==============================================================================
# Plot pain neurons (A-HTMR, C-HTMR)
# ==============================================================================

# Prepare pain data
pain_df$UnitType <- factor(pain_df$UnitType, levels = desired_order)
pain_data <- pain_df %>% select(where(is.numeric))

# Scale and PCA
pain_data_scaled <- scale(pain_data[, apply(pain_data, 2, sd, na.rm = TRUE) != 0])
pca_pain <- prcomp(pain_data_scaled, center = FALSE, scale. = FALSE)

# UMAP
set.seed(123)
umap_pain_res <- umap(
  pca_pain$x[, 1:min(15, ncol(pca_pain$x))], 
  n_neighbors = 7.5, min_dist = 0.9, spread = 1.1, metric = "euclidean"
)

df_pain_umap <- bind_cols(
  pain_df, 
  tibble(UMAP1 = umap_pain_res$layout[,1], UMAP2 = umap_pain_res$layout[,2])
)

# Plot
p_pain_final <- ggplot(df_pain_umap, aes(x = UMAP1, y = UMAP2)) +
  geom_mark_hull(aes(group = interaction(UnitType, Condition), 
                     color = Condition, fill = Condition, 
                     alpha = interaction(UnitType, Condition)),
                 concavity = 10, expand = unit(3, "mm"), radius = unit(2, "mm"), 
                 linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  geom_point(aes(shape = UnitType, color = Condition, fill = Condition), 
             size = 4, stroke = 1) +
  scale_shape_manual(values = neuron_shapes, labels = neuron_labels) +
  scale_color_manual(values = condition_colors) +
  scale_fill_manual(values = condition_colors) +
  scale_alpha_manual(values = hull_alpha_values) +
  coord_fixed(ratio = 1, clip = "off") +
  labs(title = "Nociceptors") +
  theme_void() +
  theme(
    legend.text = element_markdown(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "plain"),
    legend.position = "none",
    plot.margin = margin(10, 20, 40, 20) 
  )

print(p_pain_final)

# ==============================================================================
# Shared legend
# ==============================================================================

# Generate legend 
p_master_leg <- ggplot(pain_df, aes(x=1, y=1)) +
  geom_point(aes(shape = UnitType, color = Condition, fill = Condition), size = 4) +
  scale_shape_manual(values = neuron_shapes, name = "Neuron Type", 
                     labels = neuron_labels, breaks = desired_order) +
  scale_color_manual(values = condition_colors, name = "Condition", breaks = condition_order) +
  scale_fill_manual(values = condition_colors, breaks = condition_order) +
  theme_minimal() +
  theme(legend.text = element_markdown()) +
  guides(
    fill = "none", 
    shape = guide_legend(order = 1, override.aes = list(color = "black", fill = "black")),
    color = guide_legend(order = 2, override.aes = list(shape = 15, size = 5))
  )

shared_legend <- cowplot::get_legend(p_master_leg)

# ==============================================================================
# UMAP axes
# ==============================================================================

# Generate axes
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

# ==============================================================================
# Combine plots and save
# ==============================================================================

# Assemble using patchwork
final_assembly <- (
  p_pain_final / 
    (p_axis_final + plot_spacer()) + plot_layout(heights = c(1, 0.15))
) | shared_legend

print(final_assembly)

# Save the plot 
ggsave("UMAP_2.png", 
       final_assembly + plot_layout(widths = c(1, 0.15)), 
       width = 12, height = 7.5, dpi = 600)



