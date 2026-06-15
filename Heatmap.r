# Set working directory
setwd("")

# Load libraries
library(tidyverse) # For data manipulation
library(readxl) # For reading Excel files
library(ComplexHeatmap) # For heatmap generation
library(circlize) # For color mapping
library(pBrackets) # For arranging multiple plots
library(gridtext) # For text annotations

# Load data files and merge

allunits_df <- read_excel("") %>%
  mutate(
    UnitType = case_when(
      UnitType == "A-HTMR" ~ "UFN",
      TRUE ~ UnitType
    ), 
    
    SubunitType = case_when(
      SubunitType == "Cool+" ~ "UFN KIT+",
      SubunitType == "Cool-" ~ "UFN KIT-",
      TRUE ~ SubunitType
    )
  )


###############################################################################
###############################################################################
# Generate heatmaps

# A-HTMR

# Extract A-HTMR data
ahtmr_data <- allunits_df %>%
  filter(UnitType == "UFN") %>%
  group_by(Condition) %>%
  mutate(Unit_Index = row_number()) %>%
  ungroup()

# Hair pull

# Extract hair pull columns
ahtmr_hairpull_cols <- colnames(ahtmr_data)[
                      str_starts(colnames(ahtmr_data), "HP") & 
                        str_ends(colnames(ahtmr_data), "SC")
                    ]

# Create matrix for heatmap
ahtmr_hairpull_raw_data <- ahtmr_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(ahtmr_hairpull_cols)) %>%
  pivot_longer(cols = all_of(ahtmr_hairpull_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, SubunitType, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

ahtmr_subunits <- ahtmr_hairpull_raw_data$SubunitType
ahtmr_hairpull_raw_data <- as.matrix(ahtmr_hairpull_raw_data %>% select(-SubunitType))

# Normalization: Response intensity percentage

# Function to normalize the entire matrix by the single highest value (group max)
normalize_group_apply <- function(data_mat) {
  # 1. Calculate the max of the WHOLE matrix first
  group_total_max <- max(data_mat, na.rm = TRUE)
    norm_mat <- t(apply(data_mat, 1, function(x) {
    if (group_total_max > 0) {
      return((x / group_total_max) * 100)
    } else {
      return(x * 0) # Handle completely silent groups
    }
  }))
  
  return(norm_mat)
}

# Divide each cell by the maximum response across BOTH conditions for that unit

ahtmr_hairpull_percent_data <- normalize_group_apply(ahtmr_hairpull_raw_data)


colnames(ahtmr_hairpull_percent_data) <- colnames(ahtmr_hairpull_raw_data)

# Prepare annotation info
ahtmr_hairpull_col_info <- str_split_fixed(colnames(ahtmr_hairpull_percent_data), "__", 2)
ahtmr_hairpull_cond_split <- factor(ahtmr_hairpull_col_info[,2], levels = c("Baseline", "Inflammation"))




# Function to identify and move empty rows to the bottom of each subunit group
ahtmr_is_empty_row <- apply(
  ahtmr_hairpull_percent_data,
  1,
  function(x) all(is.na(x))
)


global_ahtmr_row_order <- unlist(
  tapply(
    seq_len(nrow(ahtmr_hairpull_percent_data)),
    ahtmr_subunits,
    function(idx) {
      idx[!ahtmr_is_empty_row[idx]] %>% c(idx[ahtmr_is_empty_row[idx]])
    }
  )
)



###############################################################################

# Mechanical Indentation  

# Extract indentation columns
ahtmr_indent_cols <- colnames(ahtmr_data)[
                                            str_starts(colnames(ahtmr_data), "Ind") & 
                                              str_ends(colnames(ahtmr_data), "SC")
                                          ]

# Create matrix for heatmap
ahtmr_indent_raw_data <- ahtmr_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(ahtmr_indent_cols)) %>%
  pivot_longer(cols = all_of(ahtmr_indent_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, SubunitType, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Store Subunit labels for the split, then remove from the numeric matrix
ahtmr_subunits <- ahtmr_indent_raw_data$SubunitType
ahtmr_indent_raw_data <- as.matrix(ahtmr_indent_raw_data %>% select(-SubunitType))

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
ahtmr_indent_percent_data <- normalize_group_apply(ahtmr_indent_raw_data)


colnames(ahtmr_indent_percent_data) <- colnames(ahtmr_indent_raw_data)


# Prepare annotation info
ahtmr_indent_col_info <- str_split_fixed(colnames(ahtmr_indent_percent_data), "__", 2)
ahtmr_indent_cond_split <- factor(ahtmr_indent_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Brush  

# Extract brush columns
ahtmr_brush_cols <- colnames(ahtmr_data)[
                              str_starts(colnames(ahtmr_data), "Brush") & 
                                str_ends(colnames(ahtmr_data), "SC")
                            ]
# Create matrix for heatmap
ahtmr_brush_raw_data <- ahtmr_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(ahtmr_brush_cols)) %>%
  pivot_longer(cols = all_of(ahtmr_brush_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, SubunitType, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Store Subunit labels for the split, then remove from the numeric matrix
ahtmr_subunits <- ahtmr_brush_raw_data$SubunitType
ahtmr_brush_raw_data <- as.matrix(ahtmr_brush_raw_data %>% select(-SubunitType))

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

ahtmr_brush_percent_data <- normalize_group_apply(ahtmr_brush_raw_data)



colnames(ahtmr_brush_percent_data) <- colnames(ahtmr_brush_raw_data)

# Prepare annotation info
ahtmr_brush_col_info <- str_split_fixed(colnames(ahtmr_brush_percent_data), "__", 2)
ahtmr_brush_cond_split <- factor(ahtmr_brush_col_info[,2], levels = c("Baseline", "Inflammation"))

# Top annotation: stimulus type labels
ahtmr_brush_top_labels <- ahtmr_brush_col_info[,1] %>%
  str_extract("Brush_Soft|Brush_Rough") %>% 
  str_replace("Brush_Soft", "Soft") %>%
  str_replace("Brush_Rough", "Coarse")


###############################################################################

# Heating

# Extract heating columns
ahtmr_heat_cols <- colnames(ahtmr_data)[
                                    str_starts(colnames(ahtmr_data), "Heat") & 
                                      str_ends(colnames(ahtmr_data), "SC")]

# Create matrix for heatmap
ahtmr_heat_raw_data <- ahtmr_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(ahtmr_heat_cols)) %>%
  pivot_longer(cols = all_of(ahtmr_heat_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, SubunitType, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")


# Store Subunit labels for the split, then remove from the numeric matrix
ahtmr_subunits <- ahtmr_heat_raw_data$SubunitType
ahtmr_heat_raw_data <- as.matrix(ahtmr_heat_raw_data %>% select(-SubunitType))

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
ahtmr_heat_percent_data <- normalize_group_apply(ahtmr_heat_raw_data)

colnames(ahtmr_heat_percent_data) <- colnames(ahtmr_heat_raw_data)

# Prepare annotation info
ahtmr_heat_col_info <- str_split_fixed(colnames(ahtmr_heat_percent_data), "__", 2)
ahtmr_heat_cond_split <- factor(ahtmr_heat_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Cooling

# Extract cooling columns
ahtmr_cool_cols <- colnames(ahtmr_data)[
                                          str_starts(colnames(ahtmr_data), "Cool") & 
                                            str_ends(colnames(ahtmr_data), "SC")]


# Create matrix for coolmap
ahtmr_cool_raw_data <- ahtmr_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(ahtmr_cool_cols)) %>%
  pivot_longer(cols = all_of(ahtmr_cool_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, SubunitType, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Store Subunit labels for the split, then remove from the numeric matrix
ahtmr_subunits <- ahtmr_cool_raw_data$SubunitType
ahtmr_cool_raw_data <- as.matrix(ahtmr_cool_raw_data %>% select(-SubunitType))

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
ahtmr_cool_percent_data <- normalize_group_apply(ahtmr_cool_raw_data)

colnames(ahtmr_cool_percent_data) <- colnames(ahtmr_cool_raw_data)

# Prepare annotation info
ahtmr_cool_col_info <- str_split_fixed(colnames(ahtmr_cool_percent_data), "__", 2)
ahtmr_cool_cond_split <- factor(ahtmr_cool_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################
###############################################################################

# C-HTMR

# Extract C-HTMR data
chtmr_data <- allunits_df %>%
  filter(UnitType == "C-HTMR") %>%
  group_by(Condition) %>%
  mutate(Unit_Index = row_number()) %>%
  ungroup()


# Hair pull

# Extract hair pull columns
chtmr_hairpull_cols <- colnames(chtmr_data)[
                          str_starts(colnames(chtmr_data), "HP") & 
                            str_ends(colnames(chtmr_data), "SC")
                        ]

# Create matrix for heatmap
chtmr_hairpull_raw_data <- chtmr_data %>%
  select(Unit_Index, Condition, all_of(chtmr_hairpull_cols)) %>%
  pivot_longer(cols = all_of(chtmr_hairpull_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
chtmr_hairpull_percent_data <- normalize_group_apply(chtmr_hairpull_raw_data)

colnames(chtmr_hairpull_percent_data) <- colnames(chtmr_hairpull_raw_data)

# Prepare annotation info
chtmr_hairpull_col_info <- str_split_fixed(colnames(chtmr_hairpull_percent_data), "__", 2)
chtmr_hairpull_cond_split <- factor(chtmr_hairpull_col_info[,2], levels = c("Baseline", "Inflammation"))


# Function to identify and move empty rows to the bottom
chtmr_is_empty_row <- apply(
  chtmr_hairpull_percent_data,
  1,
  function(x) all(is.na(x))
)


global_chtmr_row_order <- c(
  which(!chtmr_is_empty_row),
  which(chtmr_is_empty_row)
)

###############################################################################


# Mechanical Indentation  

# Extract indentation columns
chtmr_indent_cols <- colnames(chtmr_data)[
                      str_starts(colnames(chtmr_data), "Ind") & 
                        str_ends(colnames(chtmr_data), "SC")
                    ]

# Create matrix for heatmap
chtmr_indent_raw_data <- chtmr_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(chtmr_indent_cols)) %>%
  pivot_longer(cols = all_of(chtmr_indent_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")


# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
chtmr_indent_percent_data <- normalize_group_apply(chtmr_indent_raw_data)


colnames(chtmr_indent_percent_data) <- colnames(chtmr_indent_raw_data)


# Prepare annotation info
chtmr_indent_col_info <- str_split_fixed(colnames(chtmr_indent_percent_data), "__", 2)
chtmr_indent_cond_split <- factor(chtmr_indent_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Brush 

# Extract brush columns
chtmr_brush_cols <-  colnames(ahtmr_data)[
                      str_starts(colnames(ahtmr_data), "Brush") & 
                        str_ends(colnames(ahtmr_data), "SC")
                    ]

# Create matrix for heatmap
chtmr_brush_raw_data <- chtmr_data %>%
  select(Unit_Index, Condition, all_of(chtmr_brush_cols)) %>%
  pivot_longer(cols = all_of(chtmr_brush_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

chtmr_brush_percent_data <- normalize_group_apply(chtmr_brush_raw_data)


colnames(chtmr_brush_percent_data) <- colnames(chtmr_brush_raw_data)

# Prepare annotation info
chtmr_brush_col_info <- str_split_fixed(colnames(chtmr_brush_percent_data), "__", 2)
chtmr_brush_cond_split <- factor(chtmr_brush_col_info[,2], levels = c("Baseline", "Inflammation"))

# Top annotation: Stimulus type labels
chtmr_brush_top_labels <- chtmr_brush_col_info[,1] %>%
                          str_extract("Brush_Soft|Brush_Rough") %>% 
                          str_replace("Brush_Soft", "Soft") %>%
                          str_replace("Brush_Rough", "Coarse")

###############################################################################

# Heating

# Extract heating columns
chtmr_heat_cols <- colnames(chtmr_data)[
                      str_starts(colnames(chtmr_data), "Heat") & 
                        str_ends(colnames(chtmr_data), "SC")]

# Create matrix for heatmap
chtmr_heat_raw_data <- chtmr_data %>%
  select(Unit_Index, Condition, all_of(chtmr_heat_cols)) %>%
  pivot_longer(cols = all_of(chtmr_heat_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
chtmr_heat_percent_data <- normalize_group_apply(chtmr_heat_raw_data)


colnames(chtmr_heat_percent_data) <- colnames(chtmr_heat_raw_data)


# Prepare annotation info
chtmr_heat_col_info <- str_split_fixed(colnames(chtmr_heat_percent_data), "__", 2)
chtmr_heat_cond_split <- factor(chtmr_heat_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Cooling
# Extract cooling columns
chtmr_cool_cols <- colnames(chtmr_data)[
                    str_starts(colnames(chtmr_data), "Cool") & 
                      str_ends(colnames(chtmr_data), "SC")]


# Create matrix for coolmap
chtmr_cool_raw_data <- chtmr_data %>%
  select(Unit_Index, Condition, all_of(chtmr_cool_cols)) %>%
  pivot_longer(cols = all_of(chtmr_cool_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

chtmr_cool_percent_data <- normalize_group_apply(chtmr_cool_raw_data)


colnames(chtmr_cool_percent_data) <- colnames(chtmr_cool_raw_data)


# Prepare annotation info
chtmr_cool_col_info <- str_split_fixed(colnames(chtmr_cool_percent_data), "__", 2)
chtmr_cool_cond_split <- factor(chtmr_cool_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################
###############################################################################

# Field-LTMR

# Extract Field-LTMR data
field_data <- allunits_df %>%
  filter(UnitType == "Field-LTMR") %>%
  group_by(Condition) %>%
  mutate(Unit_Index = row_number()) %>%
  ungroup()


# Hair pull

# Extract hair pull columns
field_hairpull_cols <- colnames(field_data)[
  str_starts(colnames(field_data), "HP") & 
    str_ends(colnames(field_data), "SC")
]

# Create matrix for heatmap
field_hairpull_raw_data <- field_data %>%
  select(Unit_Index, Condition, all_of(field_hairpull_cols)) %>%
  pivot_longer(cols = all_of(field_hairpull_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
field_hairpull_percent_data <- normalize_group_apply(field_hairpull_raw_data)

colnames(field_hairpull_percent_data) <- colnames(field_hairpull_raw_data)

# Prepare annotation info
field_hairpull_col_info <- str_split_fixed(colnames(field_hairpull_percent_data), "__", 2)
field_hairpull_cond_split <- factor(field_hairpull_col_info[,2], levels = c("Baseline", "Inflammation"))


# Function to identify and move empty rows to the bottom
field_is_empty_row <- apply(
  field_hairpull_percent_data,
  1,
  function(x) all(is.na(x))
)


global_field_row_order <- c(
  which(!field_is_empty_row),
  which(field_is_empty_row)
)

###############################################################################


# Mechanical Indentation  

# Extract indentation columns
field_indent_cols <- colnames(field_data)[
  str_starts(colnames(field_data), "Ind") & 
    str_ends(colnames(field_data), "SC")
]

# Create matrix for heatmap
field_indent_raw_data <- field_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(field_indent_cols)) %>%
  pivot_longer(cols = all_of(field_indent_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")


# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
field_indent_percent_data <- normalize_group_apply(field_indent_raw_data)


colnames(field_indent_percent_data) <- colnames(field_indent_raw_data)


# Prepare annotation info
field_indent_col_info <- str_split_fixed(colnames(field_indent_percent_data), "__", 2)
field_indent_cond_split <- factor(field_indent_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Brush 

# Extract brush columns
field_brush_cols <-  colnames(ahtmr_data)[
  str_starts(colnames(ahtmr_data), "Brush") & 
    str_ends(colnames(ahtmr_data), "SC")
]

# Create matrix for heatmap
field_brush_raw_data <- field_data %>%
  select(Unit_Index, Condition, all_of(field_brush_cols)) %>%
  pivot_longer(cols = all_of(field_brush_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

field_brush_percent_data <- normalize_group_apply(field_brush_raw_data)


colnames(field_brush_percent_data) <- colnames(field_brush_raw_data)

# Prepare annotation info
field_brush_col_info <- str_split_fixed(colnames(field_brush_percent_data), "__", 2)
field_brush_cond_split <- factor(field_brush_col_info[,2], levels = c("Baseline", "Inflammation"))

# Top annotation: Stimulus type labels
field_brush_top_labels <- field_brush_col_info[,1] %>%
  str_extract("Brush_Soft|Brush_Rough") %>% 
  str_replace("Brush_Soft", "Soft") %>%
  str_replace("Brush_Rough", "Coarse")

###############################################################################

# Heating

# Extract heating columns
field_heat_cols <- colnames(field_data)[
  str_starts(colnames(field_data), "Heat") & 
    str_ends(colnames(field_data), "SC")]

# Create matrix for heatmap
field_heat_raw_data <- field_data %>%
  select(Unit_Index, Condition, all_of(field_heat_cols)) %>%
  pivot_longer(cols = all_of(field_heat_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
field_heat_percent_data <- normalize_group_apply(field_heat_raw_data)


colnames(field_heat_percent_data) <- colnames(field_heat_raw_data)


# Prepare annotation info
field_heat_col_info <- str_split_fixed(colnames(field_heat_percent_data), "__", 2)
field_heat_cond_split <- factor(field_heat_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Cooling
# Extract cooling columns
field_cool_cols <- colnames(field_data)[
  str_starts(colnames(field_data), "Cool") & 
    str_ends(colnames(field_data), "SC")]


# Create matrix for coolmap
field_cool_raw_data <- field_data %>%
  select(Unit_Index, Condition, all_of(field_cool_cols)) %>%
  pivot_longer(cols = all_of(field_cool_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

field_cool_percent_data <- normalize_group_apply(field_cool_raw_data)


colnames(field_cool_percent_data) <- colnames(field_cool_raw_data)


# Prepare annotation info
field_cool_col_info <- str_split_fixed(colnames(field_cool_percent_data), "__", 2)
field_cool_cond_split <- factor(field_cool_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################
###############################################################################

# SA-LTMR

# Extract SA-LTMR data
sa_data <- allunits_df %>%
  filter(UnitType == "SA-LTMR") %>%
  group_by(Condition) %>%
  mutate(Unit_Index = row_number()) %>%
  ungroup()


# Hair pull

# Extract hair pull columns
sa_hairpull_cols <- colnames(sa_data)[
  str_starts(colnames(sa_data), "HP") & 
    str_ends(colnames(sa_data), "SC")
]

# Create matrix for heatmap
sa_hairpull_raw_data <- sa_data %>%
  select(Unit_Index, Condition, all_of(sa_hairpull_cols)) %>%
  pivot_longer(cols = all_of(sa_hairpull_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
sa_hairpull_percent_data <- normalize_group_apply(sa_hairpull_raw_data)

colnames(sa_hairpull_percent_data) <- colnames(sa_hairpull_raw_data)

# Prepare annotation info
sa_hairpull_col_info <- str_split_fixed(colnames(sa_hairpull_percent_data), "__", 2)
sa_hairpull_cond_split <- factor(sa_hairpull_col_info[,2], levels = c("Baseline", "Inflammation"))


# Function to identify and move empty rows to the bottom
sa_is_empty_row <- apply(
  sa_hairpull_percent_data,
  1,
  function(x) all(is.na(x))
)


global_sa_row_order <- c(
  which(!sa_is_empty_row),
  which(sa_is_empty_row)
)

###############################################################################


# Mechanical Indentation  

# Extract indentation columns
sa_indent_cols <- colnames(sa_data)[
  str_starts(colnames(sa_data), "Ind") & 
    str_ends(colnames(sa_data), "SC")
]

# Create matrix for heatmap
sa_indent_raw_data <- sa_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(sa_indent_cols)) %>%
  pivot_longer(cols = all_of(sa_indent_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")


# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
sa_indent_percent_data <- normalize_group_apply(sa_indent_raw_data)


colnames(sa_indent_percent_data) <- colnames(sa_indent_raw_data)


# Prepare annotation info
sa_indent_col_info <- str_split_fixed(colnames(sa_indent_percent_data), "__", 2)
sa_indent_cond_split <- factor(sa_indent_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Brush 

# Extract brush columns
sa_brush_cols <-  colnames(ahtmr_data)[
  str_starts(colnames(ahtmr_data), "Brush") & 
    str_ends(colnames(ahtmr_data), "SC")
]

# Create matrix for heatmap
sa_brush_raw_data <- sa_data %>%
  select(Unit_Index, Condition, all_of(sa_brush_cols)) %>%
  pivot_longer(cols = all_of(sa_brush_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

sa_brush_percent_data <- normalize_group_apply(sa_brush_raw_data)


colnames(sa_brush_percent_data) <- colnames(sa_brush_raw_data)

# Prepare annotation info
sa_brush_col_info <- str_split_fixed(colnames(sa_brush_percent_data), "__", 2)
sa_brush_cond_split <- factor(sa_brush_col_info[,2], levels = c("Baseline", "Inflammation"))

# Top annotation: Stimulus type labels
sa_brush_top_labels <- sa_brush_col_info[,1] %>%
  str_extract("Brush_Soft|Brush_Rough") %>% 
  str_replace("Brush_Soft", "Soft") %>%
  str_replace("Brush_Rough", "Coarse")

###############################################################################

# Heating

# Extract heating columns
sa_heat_cols <- colnames(sa_data)[
  str_starts(colnames(sa_data), "Heat") & 
    str_ends(colnames(sa_data), "SC")]

# Create matrix for heatmap
sa_heat_raw_data <- sa_data %>%
  select(Unit_Index, Condition, all_of(sa_heat_cols)) %>%
  pivot_longer(cols = all_of(sa_heat_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
sa_heat_percent_data <- normalize_group_apply(sa_heat_raw_data)


colnames(sa_heat_percent_data) <- colnames(sa_heat_raw_data)


# Prepare annotation info
sa_heat_col_info <- str_split_fixed(colnames(sa_heat_percent_data), "__", 2)
sa_heat_cond_split <- factor(sa_heat_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Cooling
# Extract cooling columns
sa_cool_cols <- colnames(sa_data)[
  str_starts(colnames(sa_data), "Cool") & 
    str_ends(colnames(sa_data), "SC")]


# Create matrix for coolmap
sa_cool_raw_data <- sa_data %>%
  select(Unit_Index, Condition, all_of(sa_cool_cols)) %>%
  pivot_longer(cols = all_of(sa_cool_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

sa_cool_percent_data <- normalize_group_apply(sa_cool_raw_data)


colnames(sa_cool_percent_data) <- colnames(sa_cool_raw_data)


# Prepare annotation info
sa_cool_col_info <- str_split_fixed(colnames(sa_cool_percent_data), "__", 2)
sa_cool_cond_split <- factor(sa_cool_col_info[,2], levels = c("Baseline", "Inflammation"))



###############################################################################
###############################################################################

# C-LTMR

# Extract C-LTMR data
cltmr_data <- allunits_df %>%
  filter(UnitType == "C-LTMR") %>%
  group_by(Condition) %>%
  mutate(Unit_Index = row_number()) %>%
  ungroup()


# Hair pull

# Extract hair pull columns
cltmr_hairpull_cols <- colnames(cltmr_data)[
  str_starts(colnames(cltmr_data), "HP") & 
    str_ends(colnames(cltmr_data), "SC")
]

# Create matrix for heatmap
cltmr_hairpull_raw_data <- cltmr_data %>%
  select(Unit_Index, Condition, all_of(cltmr_hairpull_cols)) %>%
  pivot_longer(cols = all_of(cltmr_hairpull_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
cltmr_hairpull_percent_data <- normalize_group_apply(cltmr_hairpull_raw_data)

colnames(cltmr_hairpull_percent_data) <- colnames(cltmr_hairpull_raw_data)

# Prepare annotation info
cltmr_hairpull_col_info <- str_split_fixed(colnames(cltmr_hairpull_percent_data), "__", 2)
cltmr_hairpull_cond_split <- factor(cltmr_hairpull_col_info[,2], levels = c("Baseline", "Inflammation"))


# Function to identify and move empty rows to the bottom
cltmr_is_empty_row <- apply(
  cltmr_hairpull_percent_data,
  1,
  function(x) all(is.na(x))
)


global_cltmr_row_order <- c(
  which(!cltmr_is_empty_row),
  which(cltmr_is_empty_row)
)

###############################################################################


# Mechanical Indentation  

# Extract indentation columns
cltmr_indent_cols <- colnames(cltmr_data)[
  str_starts(colnames(cltmr_data), "Ind") & 
    str_ends(colnames(cltmr_data), "SC")
]

# Create matrix for heatmap
cltmr_indent_raw_data <- cltmr_data %>%
  select(Unit_Index, Condition, SubunitType, all_of(cltmr_indent_cols)) %>%
  pivot_longer(cols = all_of(cltmr_indent_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")


# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
cltmr_indent_percent_data <- normalize_group_apply(cltmr_indent_raw_data)


colnames(cltmr_indent_percent_data) <- colnames(cltmr_indent_raw_data)


# Prepare annotation info
cltmr_indent_col_info <- str_split_fixed(colnames(cltmr_indent_percent_data), "__", 2)
cltmr_indent_cond_split <- factor(cltmr_indent_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Brush 

# Extract brush columns
cltmr_brush_cols <-  colnames(ahtmr_data)[
  str_starts(colnames(ahtmr_data), "Brush") & 
    str_ends(colnames(ahtmr_data), "SC")
]

# Create matrix for heatmap
cltmr_brush_raw_data <- cltmr_data %>%
  select(Unit_Index, Condition, all_of(cltmr_brush_cols)) %>%
  pivot_longer(cols = all_of(cltmr_brush_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

cltmr_brush_percent_data <- normalize_group_apply(cltmr_brush_raw_data)


colnames(cltmr_brush_percent_data) <- colnames(cltmr_brush_raw_data)

# Prepare annotation info
cltmr_brush_col_info <- str_split_fixed(colnames(cltmr_brush_percent_data), "__", 2)
cltmr_brush_cond_split <- factor(cltmr_brush_col_info[,2], levels = c("Baseline", "Inflammation"))

# Top annotation: Stimulus type labels
cltmr_brush_top_labels <- cltmr_brush_col_info[,1] %>%
  str_extract("Brush_Soft|Brush_Rough") %>% 
  str_replace("Brush_Soft", "Soft") %>%
  str_replace("Brush_Rough", "Coarse")

###############################################################################

# Heating

# Extract heating columns
cltmr_heat_cols <- colnames(cltmr_data)[
  str_starts(colnames(cltmr_data), "Heat") & 
    str_ends(colnames(cltmr_data), "SC")]

# Create matrix for heatmap
cltmr_heat_raw_data <- cltmr_data %>%
  select(Unit_Index, Condition, all_of(cltmr_heat_cols)) %>%
  pivot_longer(cols = all_of(cltmr_heat_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit
cltmr_heat_percent_data <- normalize_group_apply(cltmr_heat_raw_data)


colnames(cltmr_heat_percent_data) <- colnames(cltmr_heat_raw_data)


# Prepare annotation info
cltmr_heat_col_info <- str_split_fixed(colnames(cltmr_heat_percent_data), "__", 2)
cltmr_heat_cond_split <- factor(cltmr_heat_col_info[,2], levels = c("Baseline", "Inflammation"))


###############################################################################

# Cooling
# Extract cooling columns
cltmr_cool_cols <- colnames(cltmr_data)[
  str_starts(colnames(cltmr_data), "Cool") & 
    str_ends(colnames(cltmr_data), "SC")]


# Create matrix for coolmap
cltmr_cool_raw_data <- cltmr_data %>%
  select(Unit_Index, Condition, all_of(cltmr_cool_cols)) %>%
  pivot_longer(cols = all_of(cltmr_cool_cols), names_to = "Stim", values_to = "Val") %>%
  mutate(FullCol = paste(Stim, Condition, sep = "__")) %>%
  select(Unit_Index, FullCol, Val) %>%
  pivot_wider(names_from = FullCol, values_from = Val) %>%
  column_to_rownames("Unit_Index")

# Normalization: Response intensity percentage

# Divide each cell by the maximum response across BOTH conditions for that unit

cltmr_cool_percent_data <- normalize_group_apply(cltmr_cool_raw_data)


colnames(cltmr_cool_percent_data) <- colnames(cltmr_cool_raw_data)


# Prepare annotation info
cltmr_cool_col_info <- str_split_fixed(colnames(cltmr_cool_percent_data), "__", 2)
cltmr_cool_cond_split <- factor(cltmr_cool_col_info[,2], levels = c("Baseline", "Inflammation"))



###############################################################################
###############################################################################

# Combine stimulus morality data
all_brush_data    <- rbind(ahtmr_brush_percent_data, chtmr_brush_percent_data, 
                           field_brush_percent_data, sa_brush_percent_data, cltmr_brush_percent_data)
all_indent_data   <- rbind(ahtmr_indent_percent_data, chtmr_indent_percent_data, 
                           field_indent_percent_data, sa_indent_percent_data, cltmr_indent_percent_data)
all_hairpull_data <- rbind(ahtmr_hairpull_percent_data, chtmr_hairpull_percent_data, 
                           field_hairpull_percent_data, sa_hairpull_percent_data, cltmr_hairpull_percent_data)
all_heat_data     <- rbind(ahtmr_heat_percent_data, chtmr_heat_percent_data, 
                           field_heat_percent_data, sa_heat_percent_data, cltmr_heat_percent_data)
all_cool_data     <- rbind(ahtmr_cool_percent_data, chtmr_cool_percent_data, 
                           field_cool_percent_data, sa_cool_percent_data, cltmr_cool_percent_data)

# Define plot level

# levels used for grouping
split_levels <- c("UFN KIT+", "UFN KIT-", "C-HTMR", "Field-LTMR", "SA-LTMR", "C-LTMR")

# Factoring levels for row splitting (grouping)
row_split_factor <- factor(
  c(as.character(ahtmr_subunits), 
    rep("C-HTMR", nrow(chtmr_brush_percent_data)),
    rep("Field-LTMR", nrow(field_brush_percent_data)),
    rep("SA-LTMR", nrow(sa_brush_percent_data)),
    rep("C-LTMR", nrow(cltmr_brush_percent_data))),
  levels = split_levels
)

# Labels 
row_split_titles <- gt_render(c(
  "UFN<sup> KIT+</sup>", 
  "UFN<sup> KIT-</sup>", 
  "C-HTMR", 
  "Field-LTMR", 
  "SA-LTMR", 
  "C-LTMR"
))

# Update row order and gaps
ufn_count    <- nrow(ahtmr_brush_percent_data)
chtmr_count  <- nrow(chtmr_brush_percent_data)
field_count  <- nrow(field_brush_percent_data)
sa_count     <- nrow(sa_brush_percent_data)

combined_row_order <- c(
  global_ahtmr_row_order, 
  global_chtmr_row_order + ufn_count,
  global_field_row_order + ufn_count + chtmr_count,
  global_sa_row_order    + ufn_count + chtmr_count + field_count,
  global_cltmr_row_order + ufn_count + chtmr_count + field_count + sa_count
)

# Add more gap for the new group
row_gaps <- unit(c(2, 5, 5, 5, 5), "mm")


# Prepare annotation and colors for the heatmaps

# Color scheme
# 0 (Black), 0.05 (Dark Red), 0.2 (Orange), 0.5 (Yellow), 1.0 (White)
# 0.0 -> Black
# 0.05 -> Deep Red
# 0.25 -> Bright Red/Orange
# 0.6 -> Orange-Yellow
# 1.0 -> Bright Yellow 
col_fun_pct = colorRamp2(c(0, 5, 25, 75, 100), 
                         c("black", "#700000", "#FF4500", "#FFD700", "#FFFF00"))


# Define fixed unit sizes and padding
unit_height_mm = 6  # Fixed height for each unit
unit_width_mm  = 6 # Fixed width for columns
pad_h_in <- 2.0   # Top/Bottom space for labels (inches)
pad_w_in <- 2.5   # Side space for legend/text (inches)


# Function to create modality heatmaps
make_modality_ht <- function(data, title, col_labels, cond_split, col_gap, show_legend = FALSE, base_fontsize = 18) {
  
  # Define the annotation INSIDE the function so we can control font size per modality
  local_b_anno = HeatmapAnnotation(
    Condition = anno_block(
      gp = gpar(fill = NA, col = NA), 
      labels = c("Baseline", "Inflammation"),
      labels_gp = gpar(col = c("blue", "red"), fontface = "plain", fontsize = base_fontsize)
    ),
    show_annotation_name = FALSE
  )
  
  Heatmap(data, 
          name = title,           
          column_title = title,
          col = col_fun_pct,
          heatmap_legend_param = list(title = "Intensity %"),
          
          row_split = row_split_factor,
          row_title = row_split_titles,
          row_gap = row_gaps,
          row_title_rot = 0,
          row_title_gp = gpar(fontsize = 15),
          
          cluster_rows = FALSE, 
          show_row_dend = FALSE,        
          cluster_columns = FALSE,
          show_column_dend = FALSE,     
          
          row_order = combined_row_order,
          show_row_names = FALSE,
          
          width = unit(ncol(data) * unit_width_mm, "mm"),
          height = unit(nrow(data) * unit_height_mm, "mm"),
          column_split = cond_split,
          column_gap = col_gap,
          
          bottom_annotation = local_b_anno,
          
          column_labels = col_labels,
          column_names_side = "top",     
          column_names_rot = 45,
          column_names_gp = gpar(fontsize = 12),
          column_title_gp = gpar(fontsize = 18, fontface = "plain"),
          
          show_heatmap_legend = show_legend,
          rect_gp = gpar(col = "white", lwd = 0.01),
          border = "white",
          border_gp = gpar(lwd = 0.01),
          na_col = "white")
}

# Create heatmaps for each modality 
ht_brush    <- make_modality_ht(all_brush_data, "Brush", ahtmr_brush_top_labels, ahtmr_brush_cond_split, unit(6, "mm"), TRUE, base_fontsize = 18)
ht_indent   <- make_modality_ht(all_indent_data, "Mechanical indentation (mN)", str_extract(ahtmr_indent_col_info[,1], "\\d+"), ahtmr_indent_cond_split, unit(6, "mm"), base_fontsize = 18)
ht_hairpull <- make_modality_ht(all_hairpull_data, "Hair pull (mN)", str_extract(ahtmr_hairpull_col_info[,1], "\\d+"), ahtmr_hairpull_cond_split, unit(6, "mm"), base_fontsize = 18)
ht_heat     <- make_modality_ht(all_heat_data, "Heating (°C)", str_extract(ahtmr_heat_col_info[,1], "\\d+"), ahtmr_heat_cond_split, unit(6, "mm"), base_fontsize = 18)
ht_cool     <- make_modality_ht(all_cool_data, "Cooling (°C)", str_extract(ahtmr_cool_col_info[,1], "\\d+"), ahtmr_cool_cond_split, unit(6, "mm"), base_fontsize = 18)

# Combine heatmaps into a single list
final_ht_list <- ht_brush + ht_indent + ht_hairpull + ht_heat + ht_cool


# plot the legend 
intensity_legend = Legend(
  title = "Intensity %", 
  col_fun = col_fun_pct, 
  at = c(0, 25, 50, 75, 100), 
  labels = c("0", "25", "50", "75", "100"),
  direction = "vertical",
  title_position = "topcenter",
  legend_width = unit(6, "cm")
)

# plot 
png("Heatmap.png", width = 29, height = 20, units = "in", res = 300)

ht_draw <- draw(final_ht_list, 
                ht_gap = unit(8, "mm"), 
                show_heatmap_legend = FALSE, # Turn off auto-legend
                padding = unit(c(10, 90, 30, 10), "mm"))

seekViewport("global")


draw(intensity_legend, x = unit(70, "cm"), y = unit(3, "cm"), just = c("right", "bottom"))

# add brackets
# Pain-sensing neurons
grid.brackets(x1 = unit(9.0, "cm"), y1 = unit(40.0, "cm"), 
              x2 = unit(9.0, "cm"), y2 = unit(23.0, "cm"), 
              h = unit(-1.2, "cm"), lwd = 3, col = "black")

grid.text("Pain-sensing\nneurons", x = unit(6.5, "cm"), y = unit(32.0, "cm"), 
          rot = 90, gp = gpar(fontsize = 28, fontface = "bold", col = "#CC6677"))

# Touch-sensing neurons
grid.brackets(x1 = unit(9.0, "cm"), y1 = unit(20.5, "cm"), 
              x2 = unit(9.0, "cm"), y2 = unit(2.5, "cm"), 
              h = unit(-1.2, "cm"), lwd = 3, col = "black")

grid.text("Touch-sensing\nneurons", x = unit(6.5, "cm"), y = unit(12.0, "cm"), 
          rot = 90, gp = gpar(fontsize = 28, fontface = "bold", col = "#117733"))



dev.off()





