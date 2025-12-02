
library(tidyverse)
library(uwot)
library(ggbeeswarm)
library(readxl)
library(RColorBrewer)

### modify path to helper script if necessary
# source("R/standalone_utils.R")

source("./standalone_utils.R")

### modify input and output paths if necessary
# dir_in <- "data/input/"
# dir_out <- "data/output/"

dir_in <- Sys.getenv("INPUT_DIR")
print(dir_in)
dir_out <- Sys.getenv("OUTPUT_DIR")


########################################################
### Part I: wrangle historical metadata and features ###
########################################################

files <- list.files(dir_in)
print(files)

matches_metadata <- files[grepl("metadata_MDIPA", files)]

metadata_file <- if (length(matches_metadata) > 0) matches_metadata[1] else stop("No files in the directory contain the historical metadata.")
print(metadata_file)

matches_features <- files[grepl("feat_MDIPA", files)]

features_file <- if (length(matches_features) > 0) matches_features[1] else stop("No files in the directory contain the historical metadata.")
print(features_file)


metadata_historic <- read_excel(paste0(dir_in, metadata_file)) %>%
  filter(!grepl("VIP|Wherry", Study)) %>% ### remove studies we don't want represented on the map
  mutate(file=New_Filename %>% str_remove(".fcs") %>% str_remove("_Processed") %>% str_remove("_Normalized"),
         Subject = LV_PartID,
         VisitDate = ymd(LV_VisDate)) %>% ### standardize dates and filenames
  mutate(Age = as.numeric(Age)) ### convert age to numeric (will get NA if non-numeric characters present)

features_historic <- read_csv(paste0(dir_in, features_file)) %>%
  select(!matches("out of| CD16| CD38lo")) ### remove unstable features like B cell CD38lo or neutrophil CD16lo

feat_names <- names(features_historic %>% select(where(is.numeric)))


### join immune features with metadata
### remove PBMC samples to make IH map for whole blood
### make file, study, etc first columns for convenience
feat_meta_historic <- left_join(features_historic, metadata_historic) %>%
  filter(!grepl("PBMC", file)) %>% 
  relocate(file, Study, Diagnosis_L1, Diagnosis_L2, L1_Treatment)
n <- nrow(feat_meta_historic)



### streamline Diagnosis_L2 column
# there's only one thyroid cancer, pool together with skin cancer
# COVID vs non-COVID infection is not that interesting;
# better to group by Acute vs Long vs Recovering
feat_meta_historic <- feat_meta_historic %>%
  mutate(Diagnosis_L2 = case_when(grepl( "Skin Cancer|Thyroid Cancer", Diagnosis_L2) ~ "Solid tumor", 
                                  grepl("Infection", Diagnosis_L2) ~ Diagnosis_L1,
                                  TRUE ~ Diagnosis_L2))


##########################################################
### Part II: features and metadata for current samples ###
# Ideally metadata would be already included in historical
# But in practice we often need the report before
##########################################################

### Change experiment name and file path if necessary
# this_study <- "Stability"
# features_current <- read_csv(paste0(dir_in,"feat_stability.csv")) %>%
#   select(all_of(c("file", feat_names)))
this_study <- Sys.getenv("TARGET_STUDY")

target_diagnosis <- Sys.getenv("TARGET_DIAGNOSIS")

reference_studies <- Sys.getenv("REFERENCE_STUDIES")

reference_diagnoses <- Sys.getenv("REFERENCE_DIAGNOSES")

### Add some metadata on the fly: study name; Donor and Day by parsing file name
### This will vary with experiment
# feat_meta_current <- features_current %>%
#  mutate(Study = this_study,
#         Donor = file %>% str_split("_") %>% sapply("[",1),
#         Day = file %>% str_remove("_2X") %>% str_split("_") %>% sapply("[",3))

feat_meta <- feat_meta_historic
                      

### Concatenate historical and current
### Use rbind.fill, to preserve columns present in only one table
### (Will fill with NA for the other table)
# feat_meta <- plyr::rbind.fill(feat_meta_historic, feat_meta_current)


### Quick sanity check to see that metadata was parsed correctly
# feat_meta %>% filter(Study=="Stability") %>%
#  select(file, Study, Donor, Day)



#############################################
### Part III: Create map and design plots ###
#############################################

### read from command line some variables: Study name; other studies to compare to; a metadata column to highlight

### Create UMAP
set.seed(0)
df_um <- get_umap_uniq_subj(feat_meta, feat_names)

### Add a "Highlight" or column for grouping/plotting
reference_vec <- unlist(strsplit(reference_studies, "\\|"))
diagnosis_vec <- unlist(strsplit(reference_diagnoses, "\\|"))

df_highlight <- df_um %>%
  mutate(Highlight = case_when(grepl(this_study, Study, ignore.case=TRUE) #Study==this_study # check if current study or not
                               ~ this_study, # Could be individual file or any other metadata column
                               grepl(reference_studies, Study, ignore.case=TRUE) ~ Study,
                               TRUE ~ "Other"
            ), # catch-all "Other" category for everything else
           Diagnosis_Highlight = case_when(
             target_diagnosis == Primary.Diagnosis ~ target_diagnosis,
             grepl(reference_diagnoses, Primary.Diagnosis, ignore.case=TRUE) ~ Primary.Diagnosis,
             TRUE ~ "Other"
           )
         )


### save CSV for use by other software
write_csv(df_highlight, paste0(dir_out, "features_with_umap_", this_study, ".csv"))


ref_colors <- brewer.pal(max(3, length(reference_vec)), "Set2")[seq_along(reference_vec)]
names(ref_colors) <- reference_vec

pal_highlight <- c(
  this_study = RColorBrewer::brewer.pal(3, "Reds")[3],  # Deep red for AILC
  ref_colors,
  "Other" = "gray"                                  # Gray for Other
)

names(pal_highlight) <- sort(unique(df_highlight$Highlight))

# Choose shapes; 0–25 are commonly safe ggplot shapes
# (adjust as needed or if you have many diagnosis categories)
shape_vals <- c(16, 17, 15, 3, 8, 4)  # circle, triangle, square, cross, star, X

diag_levels <- sort(unique(df_highlight$Diagnosis_Highlight))

# recycle shapes if needed
shape_palette <- setNames(rep(shape_vals, length.out = length(diag_levels)), diag_levels)

### UMAP plot
ggplot(df_highlight, aes(x=umap1, y=umap2, color=Highlight, shape=Diagnosis_Highlight,
                               size=Study!=this_study)) + ## make dots smaller for other studies
  geom_point() +
  scale_color_manual(values=pal_highlight, name="Sample") +
  scale_shape_manual(values = shape_palette, name = "Diagnosis") +
  scale_size_manual(values=setNames(c(1,3), c("TRUE", "FALSE"))) +
  guides(size="none") +
  # ggtitle("Immune Landscape") +
  theme_bw(base_size=16) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
ggsave(filename=paste0(dir_out, "umap_messi.png"), width=9, height=7)



### Manually set color and shape scales for all L2 diagnoses
val_sh <- c(rep(seq(15,18), 4),15,16,17)
names(val_sh) <- c(sort(unique(df_highlight$Diagnosis_L2)), "NA")
val_col <- c("#33A02C", "#1F78B4", "black", "black", "black",
             "#B2DF8A", "#B15928", "#A6CEE3", "#FB9A99", "#E31A1C",
             "#66A61E", "#CAB2D6", "#FDBF6F", "#FDBF6F", "#6A3D9A", 
             "#1F78B4", "#FF7F00", "#FFFF33", "gray")
names(val_col) <- names(val_sh)

ggplot(df_highlight %>% mutate(Diagnosis_L2 = replace_na(Diagnosis_L2, "NA")), 
            aes(x=umap1, y=umap2, color=Diagnosis_L2, shape=Diagnosis_L2)) +
  geom_point() +
  scale_color_manual(values=val_col, name="Diagnosis") +
  scale_shape_manual(values=val_sh, name="Diagnosis") +
  theme_bw(base_size=16)+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
ggsave(filename=paste0(dir_out, "umap_diagnosis_l2.png"), width=10.5, height=7)


ggplot(df_highlight, aes(x=umap1, y=umap2, color=Age)) +
  geom_point() +
  scale_color_viridis_c() +
  # ggtitle("Immune Landscape with subject age highlighted") +
  theme_bw(base_size=16)+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
ggsave(filename=paste0(dir_out, "umap_age.png"), width=9, height=7)





df_highlight_tall <- df_highlight %>%
   pivot_longer(all_of(feat_names), names_to="Feature", values_to="Value")

## Facet plot for individual features
## Need to fine-tune on a case to case basis
## Remove geom_line and add geom_boxplot etc
 ggplot(df_highlight_tall %>% filter(Highlight!="Other"), 
             aes(y=Value, x=Primary.Diagnosis, color=Highlight)) +
   geom_boxplot() +
  # geom_line(aes(group=Donor), color="black") +
  scale_color_manual(values=pal_highlight, name="Sample") +
  xlab("Dilution") +
  ylab("Abundance (relative to parent)") +
  expand_limits(y=0) +
  facet_wrap(~Feature, scales="free_y") +
  ggtitle("Distribution of cell type abundance") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        legend.position = "top",
        strip.background = element_blank())
ggsave(filename=paste0(dir_out, "box_features_stability.png"), width=16, height=12)








