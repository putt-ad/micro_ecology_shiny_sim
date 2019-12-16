#####
# qiime2 file reader
# the top part of this code should be run in a conda shell, within the qiime2 environment

# 1 Export OTU table from qiime as .biom
# --input-path = output of qiime dada2 denoise-paired \
# --output-path phyloseq_export/feature-table
qiime tools export \
--input-path notrunc_shortrun_dada2table.qza \
--output-path phyloseq/feature-table

# 2 OTU tables exports as feature-table.biom so convert to .tsv
# -i feature-table.biom
# -o feature-table.tsv
biom convert \
-i phyloseq/feature-table/feature-table.biom \
-o phyloseq/feature-table/feature-table.tsv \
--to-tsv

# 2.1 replace "Feature ID" with "OTUID" this can be done in the terminal or in R using step 6.1
# final argument "feature-table.tsv is your file output from step 2.0
sed -i  's/#OTU ID/OTUID/g' feature-table.tsv

# 3.0 Export taxonomy table
# --input-path = output of qiime feature-classifier classify-sklearn \
# --output-path taxonomy.tsv
qiime tools export \
--input-path notrunc_taxonomy_umb_shortrun_dada2.qza \
--output-path phyloseq/feature-table/taxonomy

# 3.1 replace "Feature ID" with "OTUID" this can be done in the terminal or in R using step 6.1
# final argument "taxonomy.tsv is your file output from step 3.0
sed -i  's/Feature ID/OTUID/g' taxonomy.tsv

# 4 Export phylogenetic tree
# --input-path output qiime phylogeny align-to-tree-mafft-fasttree
# --output-path tree.nwk
qiime tools export \
--input-path unrooted-tree_notrunc_shortrun_dada2.qza \
--output-path phyloseq/feature-table/tree

##### perform in R #####
library(tidyverse)
library(tidyr)

# 5 import OTU
OTU <- read.csv ("data/feature-table1-1.tsv", sep = "\t", header = FALSE)
View(OTU)

# 5.1 WHOA THERE!!! if you did not do step 2.1 in terminal you must do this step. (if you did 2.1, skip to 5.2)
# replace the default OTUID column title "#OTU ID" with "OTUID" so this OTU file can be merged with the taxonomy file
OTU_sub <- data.frame(lapply(OTU, function(x) {
  gsub("#OTU ID", "OTUID", x)}))
#View(OTU_sub)
OTU <- OTU_sub

# 5.2 remove "# constructed from biom" false header
OTU <- OTU[-1,]
View(OTU)

# 5.3  set sample IDs as column names
colnames(OTU) <- as.character(unlist(OTU[1,]))
OTU = OTU[-1, ]
View(OTU)


# 6  import TAX (taxonomy)
TAX <- read.csv ("data/taxonomy1.tsv", sep = "\t", header = FALSE)
View(TAX)

# 6.1 WHOA THERE!!! If you did not do step 3.1 you must do this step. (if you did 3.1 skip to 6.2)
# replace the default OTUID column title "feature ID" with "OTUID" so this oTU file can be merged with the taxonomy file
TAX_sub <- data.frame(lapply(TAX, function(x) {
  gsub("Feature ID", "OTUID", x)}))
#View(TAX_sub)
TAX <- TAX_sub

#6.2  set sample IDs as column names so you can merge
colnames(TAX) <- as.character(unlist(TAX[1,]))
TAX = TAX[-1, ]
View(TAX)

# 7 merge OTU and taxonomy drops empty taxonomy for OTUs not present in the sample.
# If this step errors make sure that TAX[1,1] and OTU[1,1] both read "OTUID" and that
merged_file <- merge(OTU, TAX, by.x = c("OTUID"), by.y = c("OTUID"))
head(merged_file)
View(merged_file)

# 8 make otu_table
remove_taxa <- c("Taxon", "Confidence")
otu_table <- merged_file[ , !(names(merged_file) %in% remove_taxa)]
head(otu_table)

# 9 make a taxonomy file that phyloseq can read.
taxonomy <- merged_file %>% select(OTUID, Taxon)
View(taxonomy)

# 10 seperate out taxonomy
ranks = c("kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
taxonomy <- separate(taxonomy, 2, ranks,sep = ";")
View(otu_table)
View(taxonomy)
write.csv(taxonomy, "taxonomy.csv", row.names = FALSE, col.names = FALSE)
write.csv(otu_table, "otu_table.csv")

# 11  change dataframes to matrix
taxonomy <- read.csv("taxonomy.csv", sep = ",", row.names = 1, header = TRUE)
View(taxonomy)
otu_table <- as.matrix(otu_table, row.names = 1)
taxonomy <- as.matrix(taxonomy)
head(otu_table)

# 12  read metadata (mapping file)
metadata <- read.table("data/micro_geochem_data_adp_2019.11.27.csv", row.names = 1, sep = ",")
View(metadata)

# 13  read qiime export tree (from step 4)
phy_tree <- read_tree("data/tree.nwk")

# 14  import as phyloseq objects
OTU <- otu_table(otu_table, taxa_are_rows = TRUE)
