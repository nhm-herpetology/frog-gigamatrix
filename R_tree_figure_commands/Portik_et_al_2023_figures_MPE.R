#R commands for making tree figures in Portik DP, Streicher JW, Wiens JJ. 2023. Frog phylogeny: A time-calibrated, species-level tree based on hundreds of loci and 5,242 species. Molecular Phylogenetics and Evolution 188: 107907.

#Load required R packages

library(ape)
library(ggtree)
library(treeio)
library(cowplot)

#Figure 3: Maximum-likelihood phylogeny reduced to one tip per anuran family

phy <-read.tree("RAxML_Partitioned_Best_Tree_Bootstraps.tre")

to_keep <- c("Ascaphus_truei", "Leiopelma_archeyi", "Bombina_bombina", "Alytes_obstetricans", "Rhinophrynus_dorsalis", "Xenopus_laevis", "Scaphiopus_holbrookii", "Pelodytes_ibericus", "Pelobates_cultripes", "Brachytarsophrys_carinense", "Heleophryne_regis", "Nasikabatrachus_sahyadrensis", "Sooglossus_sechellensis", "Gastrophryne_olivacea", "Hemisus_marmoratus", "Breviceps_macrops", "Arthroleptis_stenodactylus", "Hyperolius_ruvuensis", "Micrixalus_fuscus", "Conraua_goliath", "Petropedetes_perreti", "Pyxicephalus_adspersus", "Odontobatrachus_natator", "Ptychadena_cooperi", "Phrynobatrachus_krefftii", "Nyctibatrachus_major", "Cornufer_guppyi", "Quasipaa_spinosa", "Indirana_beddomii", "Amolops_torrentis", "Mantella_viridis", "Polypedates_leucomystax", "Calyptocephalella_gayi", "Myobatrachus_gouldii", "Rhinoderma_darwinii", "Batrachyla_leptopus", "Alsodes_nodosus", "Hylodes_ornatus", "Cycloramphus_carvalhoi", "Telmatobius_culeus", "Ceratophrys_ornata", "Hemiphractus_proboscideus", "Hyla_arborea", "Dendrobates_auratus", "Ceuthomantis_smaragdinus", "Eleutherodactylus_longipes", "Brachycephalus_ephippium", "Craugastor_podiciferus", "Allophryne_ruthveni", "Centrolene_muelleri", "Leptodactylus_fuscus", "Odontophrynus_americanus", "Bufo_bufo")

phy2 <-keep.tip(phy, to_keep)

Family_tiplabels <- c("Leiopelmatidae", "Ascaphidae", "Bombinatoridae", "Alytidae", "Rhinophrynidae", "Pipidae", "Scaphiopodidae", "Pelodytidae", "Pelobatidae", "Megophryidae", "Heleophrynidae", "Rhinodermatidae", "Brachycephalidae", "Craugastoridae", "Eleutherodactylidae", "Ceuthomantidae", "Dendrobatidae", "Odontophrynidae", "Bufonidae", "Allophrynidae", "Centrolenidae", "Leptodactylidae", "Hylidae", "Ceratophryidae", "Hemiphractidae", "Telmatobiidae", "Alsodidae", "Batrachylidae", "Hylodidae", "Cycloramphidae", "Myobatrachidae", "Calyptocephalellidae", "Nasikabatrachidae", "Sooglossidae", "Microhylidae", "Hemisotidae", "Brevicepitidae", "Hyperoliidae", "Arthroleptidae", "Micrixalidae", "Ptychadenidae", "Phrynobatrachidae", "Odontobatrachidae", "Pyxicephalidae", "Petropedetidae", "Conrauidae", "Ceratobatrachidae", "Nyctibatrachidae", "Ranixalidae", "Dicroglossidae", "Rhacophoridae", "Mantellidae", "Ranidae")

phy2$tip.label <- Family_tiplabels

Fig_3 <-ggtree(phy2) + geom_tiplab(size = 3, align=FALSE) + geom_treescale() + geom_rootedge(rootedge = 0.1) + xlim(0,1.5) 

Fig_3

pdf("Fig_3.pdf", width=7, height=7)

Fig_3

dev.off()

#Figure 4: Time-calibrated phylogeny based on partitioned maximum-likelihood analysis reduced to one tip per family

phy3 <-read.tree("Amphibia_treePL_output_Tetrapod.tre")

phy4 <-keep.tip(phy3, to_keep)

phy4$tip.label <- Family_tiplabels

phy5 <-ggtree(phy4) + geom_tiplab(size = 3, align=FALSE) + theme_tree2()

phy6 <-revts(phy5)

Fig_4 <-phy6 + xlim(-200, 35)

Fig_4

pdf("Fig_4.pdf", width=7, height=7)

Fig_4

dev.off()


#Figure 5: Legend for detailed phylogeny figures

tree_code <-source("partitioned_tree_legend.R")

Fig_5 <-tree_code

Fig_5

pdf("Fig_5.pdf", width=13.15, height=14.89)

Fig_5

dev.off()

#Figures 6-60: Detailed phylogeny figures

Support_values <- phy$node.label

phy3$node.label <- Support_values

phy3$tip.label<-gsub("_"," ",phy3$tip.label)

#Other Archaeobatrachia [Part 1 = Fig. 6]

F1 <-tree_subset(phy3, "Rhinophrynus dorsalis", levels_back=4)

F1a <-ggtree(F1) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=3.5) + theme_tree2()

F1b <- collapse(F1a, node=5309) + geom_point2(aes(subset=(node==5309)), shape=21, size=5, fill='black')

F1c <-revts(F1b)

F1d <-F1c + xlim(-200, 60)

Fig_6 <-plot_grid(F1d, nrow = 1, rel_widths = c(0.4,1))

Fig_6

pdf("Fig_6.pdf", width=10, height=15)

Fig_6

dev.off()

#Megophryidae I and Pelobatoidea [Part 2 = Fig. 7]

F2 <-tree_subset(phy3, "Scaphiopus couchii", levels_back=3)

F2a <-ggtree(F2) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=3.5) + theme_tree2()

F2b <- collapse(F2a, node=352) + geom_point2(aes(subset=(node==352)), shape=21, size=5, fill='black')

F2c <-revts(F2b)

F2d <-F2c + xlim(-135, 30)

Fig_7 <-plot_grid(F2d, nrow = 1, rel_widths = c(0.4,1))

Fig_7

pdf("Fig_7.pdf", width=10, height=15)

Fig_7

dev.off()

#Megophryidae II [Part 3 = Fig. 8]

F3 <-tree_subset(phy3, "Leptobrachium leucops", levels_back=7)

F3a <-ggtree(F3) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F3b <-revts(F3a)

F3c <-F3b + xlim(-55, 12)

Fig_8 <-plot_grid(F3c, nrow = 1, rel_widths = c(0.4,1))

Fig_8

pdf("Fig_8.pdf", width=10, height=15)

Fig_8

dev.off()

#Sooglossidae, Heleophrynidae, Nasikabtrachidae, Microhylidae I [Part 4 = Fig. 9]

F4 <-tree_subset(phy3, "Sooglossus sechellensis", levels_back=6)

F4a <-ggtree(F4) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=3.5) + theme_tree2()

F4b <- collapse(F4a, node=4940) + geom_point2(aes(subset=(node==4940)), shape=21, size=5, fill='black')

F4c <- collapse(F4b, node=8148) + geom_point2(aes(subset=(node==8148)), shape=21, size=5, fill='black')

F4d <- collapse(F4c, node=7756) + geom_point2(aes(subset=(node==7756)), shape=21, size=5, fill='black')

F4e <- collapse(F4d, node=7948) + geom_point2(aes(subset=(node==7948)), shape=21, size=5, fill='black')

F4f <-revts(F4e)

F4g <-F4f + xlim(-130, 40)

Fig_9 <-plot_grid(F4g, nrow = 1, rel_widths = c(0.4,1))

Fig_9

pdf("Fig_9.pdf", width=10, height=15)

Fig_9

dev.off()

#Microhylidae II [Part 5 = Fig. 10]

F5 <-tree_subset(phy3, "Cophyla berara", levels_back=10)

F5a <-ggtree(F5) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F5b <-revts(F5a)

F5c <-F5b + xlim(-40, 10)

Fig_10 <-plot_grid(F5c, nrow = 1, rel_widths = c(0.4,1))

Fig_10

pdf("Fig_10.pdf", width=10, height=15)

Fig_10

dev.off()

#Microhylidae III [Part 7 = Fig. 11]

F6 <-tree_subset(phy3, "Microhyla minuta", levels_back=16)

F6a <-ggtree(F6) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F6b <- collapse(F6a, node=288) + geom_point2(aes(subset=(node==288)), shape=21, size=5, fill='black')

F6c <-revts(F6b)

F6d<-F6c + xlim(-50, 20)

Fig_11 <-plot_grid(F6d, nrow = 1, rel_widths = c(0.4,1))

Fig_11

pdf("Fig_11.pdf", width=10, height=15)

Fig_11

dev.off()

#Microhylidae IV [Part 7 = Fig. 12]

F7 <-tree_subset(phy3, "Xenorhina oxycephala", levels_back=9)

F7a <-ggtree(F7) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F7b <-revts(F7a)

F7c <-F7b + xlim(-40, 15)

Fig_12 <-plot_grid(F7c, nrow = 1, rel_widths = c(0.4,1))

Fig_12

pdf("Fig_12.pdf", width=10, height=15)

Fig_12

dev.off()

#Brevicipitidae and Hemisotidae [Part 8 = Fig. 13]

F8 <-tree_subset(phy3, "Hemisus guttatus", levels_back=2)

F8a <-ggtree(F8) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F8b <-revts(F8a)

F8c <-F8b + xlim(-60, 20)

Fig_13 <-plot_grid(F8c, nrow = 1, rel_widths = c(0.4,1))

Fig_13

pdf("Fig_13.pdf", width=10, height=15)

Fig_13

dev.off()

#Arthroleptidae [Part 9 = Fig. 14]

F9 <-tree_subset(phy3, "Leptopelis anebos", levels_back=16)

F9a <-ggtree(F9) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F9b <-revts(F9a)

F9c <-F9b + xlim(-50, 15)

Fig_14 <-plot_grid(F9c, nrow = 1, rel_widths = c(0.4,1))

Fig_14

pdf("Fig_14.pdf", width=10, height=15)

Fig_14

dev.off()

#Hyperoliidae I [Part 10 = Fig. 15]

F10 <-tree_subset(phy3, "Kassina fusca", levels_back=8)

F10a <-ggtree(F10) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F10b <- collapse(F10a, node=206) + geom_point2(aes(subset=(node==206)), shape=21, size=5, fill='black')

F10c <-revts(F10b)

F10d <-F10c + xlim(-45, 15)

Fig_15 <-plot_grid(F10d, nrow = 1, rel_widths = c(0.4,1))

Fig_15

pdf("Fig_15.pdf", width=10, height=15)

Fig_15

dev.off()

#Hyperoliidae II [Part 11 = Fig. 16]

F11 <-tree_subset(phy3, "Hyperolius fusciventris", levels_back=6)

F11a <-ggtree(F11) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F11b <-revts(F11a)

F11c <-F11b + xlim(-25, 10)

Fig_16 <-plot_grid(F11c, nrow = 1, rel_widths = c(0.4,1))

Fig_16

pdf("Fig_16.pdf", width=10, height=15)

Fig_16

dev.off()

#Micrixalidae [Part 12 = Fig. 17]

F12 <-tree_subset(phy3, "Micrixalus adonis", levels_back=10)

F12a <-ggtree(F12) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F12b <-revts(F12a)

F12c <-F12b + xlim(-35, 15)

Fig_17 <-plot_grid(F12c, nrow = 1, rel_widths = c(0.4,1))

Fig_17

pdf("Fig_17.pdf", width=10, height=15)

Fig_17

dev.off()

#Pyxicephalidae, Petropedetidae and Conrauidae [Part 13 = Fig. 18]

F13 <-tree_subset(phy3, "Aubria subsigillata", levels_back=4)

F13a <-ggtree(F13) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F13b <-revts(F13a)

F13c <-F13b + xlim(-60, 20)

Fig_18 <-plot_grid(F13c, nrow = 1, rel_widths = c(0.4,1))

Fig_18

pdf("Fig_18.pdf", width=10, height=15)

Fig_18

dev.off()

#Phrynobatrachidae, Ptychadenidae and Odonotobatrachidae [Part 14 = Fig. 19]

F14 <-tree_subset(phy3, "Phrynobatrachus dispar", levels_back=10)

F14a <-ggtree(F14) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F14b <-revts(F14a)

F14c <-F14b + xlim(-65, 15)

Fig_19 <-plot_grid(F14c, nrow = 1, rel_widths = c(0.4,1))

Fig_19

pdf("Fig_19.pdf", width=10, height=15)

Fig_19

dev.off()

#Ceratobatrachidae and Nyctibatrachidae [Part 15 = Fig. 20]

F15 <-tree_subset(phy3, "Cornufer guppyi", levels_back=9)

F15a <-ggtree(F15) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F15b <-revts(F15a)

F15c <-F15b + xlim(-60, 25)

Fig_20 <-plot_grid(F15c, nrow = 1, rel_widths = c(0.4,1))

Fig_20

pdf("Fig_20.pdf", width=10, height=15)

Fig_20

dev.off()

#Ranixalidae and Dicroglossidae I [Part 16 = Fig. 21]

F16 <-tree_subset(phy3, "Nanorana parkeri", levels_back=8)

F16a <-ggtree(F16) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F16b <- collapse(F16a, node=259) + geom_point2(aes(subset=(node==259)), shape=21, size=5, fill='black')

F16c <-revts(F16b)

F16d <-F16c + xlim(-60, 20)

Fig_21 <-plot_grid(F16d, nrow = 1, rel_widths = c(0.4,1))

Fig_21

pdf("Fig_21.pdf", width=10, height=15)

Fig_21r

dev.off()

#Dicroglossidae II [Part 17 = Fig. 22]

F17<-tree_subset(phy3, "Limnonectes kong", levels_back=7)

F17a <-ggtree(F17) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F17b <-revts(F17a)

F17c <-F17b + xlim(-35, 15)

Fig_22 <-plot_grid(F17c, nrow = 1, rel_widths = c(0.4,1))

Fig_22

pdf("Fig_22.pdf", width=10, height=15)

Fig_22

dev.off()

#Ranidae I [Part 18 = Fig. 23]

F18 <-tree_subset(phy3, "Amolops afghanus", levels_back=11)

F18a <-ggtree(F18) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F18b <- collapse(F18a, node=467) + geom_point2(aes(subset=(node==467)), shape=21, size=5, fill='black')

F18c <-revts(F18b)

F18d <-F18c + xlim(-45, 20)

Fig_23 <-plot_grid(F18d, nrow = 1, rel_widths = c(0.4,1))

Fig_23

pdf("Fig_23.pdf", width=10, height=15)

Fig_23

dev.off()

#Ranidae II [Part 19 = Fig. 24]

F19 <-tree_subset(phy3, "Amnirana amnicola", levels_back=13)

F19a <-ggtree(F19) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F19b <-revts(F19a)

F19c <-F19b + xlim(-35, 15)

Fig_24 <-plot_grid(F19c, nrow = 1, rel_widths = c(0.4,1))

Fig_24

pdf("Fig_24.pdf", width=10, height=15)

Fig_24

dev.off()

#Ranidae III [Part 20 = Fig. 25]

F20 <-tree_subset(phy3, "Rana temporaria", levels_back=11)

F20a <-ggtree(F20) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F20b <-revts(F20a)

F20c <-F20b + xlim(-30, 10)

Fig_25 <-plot_grid(F20c, nrow = 1, rel_widths = c(0.4,1))

Fig_25

pdf("Fig_25.pdf", width=10, height=15)

Fig_25

dev.off()

#Mantellidae I [Part 21 = Fig. 26]

F21 <-tree_subset(phy3, "Boophis luteus", levels_back=12)

F21a <-ggtree(F21) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F21b <- collapse(F21a, node=219) + geom_point2(aes(subset=(node==219)), shape=21, size=5, fill='black')

F21c <-revts(F21b)

F21d <-F21c + xlim(-45, 20)

Fig_26 <-plot_grid(F21d, nrow = 1, rel_widths = c(0.4,1))

Fig_26

pdf("Fig_26.pdf", width=10, height=15)

Fig_26

dev.off()

#Mantellidae II [Part 22 = Fig. 27]

F22 <-tree_subset(phy3, "Mantidactylus bellyi", levels_back=14)

F22a <-ggtree(F22) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F22b <-revts(F22a)

F22c <-F22b + xlim(-40, 20)

Fig_27 <-plot_grid(F22c, nrow = 1, rel_widths = c(0.4,1))

Fig_27

pdf("Fig_27.pdf", width=10, height=15)

Fig_27

dev.off()

#Rhacophoridae I [Part 23 = Fig. 28]

F23 <-tree_subset(phy3, "Buergeria otai", levels_back=3)

F23a <-ggtree(F23) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F23b <- collapse(F23a, node=336) + geom_point2(aes(subset=(node==336)), shape=21, size=5, fill='black')

F23c <-revts(F23b)

F23d <-F23c + xlim(-50, 20)

Fig_28 <-plot_grid(F23d, nrow = 1, rel_widths = c(0.4,1))

Fig_28

pdf("Fig_28.pdf", width=10, height=15)

Fig_28

dev.off()

#Rhacophoridae II [Part 24 = Fig. 29]

F24 <-tree_subset(phy3, "Rhacophorus kio", levels_back=8)

F24a <-ggtree(F24) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F24b <-revts(F24a)

F24c <-F24b + xlim(-40, 20)

Fig_29 <-plot_grid(F24c, nrow = 1, rel_widths = c(0.4,1))

Fig_29

pdf("Fig_29.pdf", width=10, height=15)

Fig_29

dev.off()

#Rhacophoridae III [Part 25 = Fig. 30]

F25 <-tree_subset(phy3, "Philautus acutus", levels_back=9)

F25a <-ggtree(F25) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F25b <- collapse(F25a, node=218) + geom_point2(aes(subset=(node==218)), shape=21, size=5, fill='black')

F25c <-revts(F25b)

F25d <-F25c + xlim(-40, 20)

Fig_30 <-plot_grid(F25d, nrow = 1, rel_widths = c(0.4,1))

Fig_30

pdf("Fig_30.pdf", width=10, height=15)

Fig_30

dev.off()

#Rhacophoridae IV [Part 26 = Fig. 31]

F26 <-tree_subset(phy3, "Raorchestes luteolus", levels_back=9)

F26a <-ggtree(F26) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=1.3) + theme_tree2()

F26b <-revts(F26a)

F26c <-F26b + xlim(-30, 15)

Fig_31 <-plot_grid(F26c, nrow = 1, rel_widths = c(0.4,1))

Fig_31

pdf("Fig_31.pdf", width=10, height=15)

Fig_31

dev.off()

#Myobatrachoidea [Part 27 = Fig. 32]

F27 <-tree_subset(phy3, "Myobatrachus gouldii", levels_back=10)

F27a <-ggtree(F27) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=1.3) + theme_tree2()

F27b <-revts(F27a)

F27c <-F27b + xlim(-100, 30)

Fig_32 <-plot_grid(F27c, nrow = 1, rel_widths = c(0.4,1))

Fig_32

pdf("Fig_32.pdf", width=10, height=15)

Fig_32

dev.off()

#Rhinodermatidae and Neoaustrarana [Part 28 = Fig. 33]

F28 <-tree_subset(phy3, "Rhinoderma darwinii", levels_back=2)

F28a <-ggtree(F28) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F28b <- collapse(F28a, node=2691) + geom_point2(aes(subset=(node==2691)), shape=21, size=5, fill='black')

F28c <-revts(F28b)

F28d <-F28c + xlim(-70, 25)

Fig_33 <-plot_grid(F28d, nrow = 1, rel_widths = c(0.4,1))

Fig_33

pdf("Fig_33.pdf", width=10, height=15)

Fig_33

dev.off()

#Telmatobiidae [Part 29 = Fig. 34]

F29 <-tree_subset(phy3, "Telmatobius vellardi", levels_back=2)

F29a <-ggtree(F29) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.2) + theme_tree2()

F29b <-revts(F29a)

F29c <-F29b + xlim(-8, 3)

Fig_34 <-plot_grid(F29c, nrow = 1, rel_widths = c(0.4,1))

Fig_34

pdf("Fig_34.pdf", width=10, height=15)

Fig_34

dev.off()

#Hemiphractidae and Ceratophryidae [Part 30 = Fig. 35]

F30 <-tree_subset(phy3, "Gastrotheca gracilis", levels_back=17)

F30a <-ggtree(F30) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F30b <-revts(F30a)

F30c <-F30b + xlim(-60, 20)

Fig_35 <-plot_grid(F30c, nrow = 1, rel_widths = c(0.4,1))

Fig_35

pdf("Fig_35.pdf", width=10, height=15)

Fig_35

dev.off()

#Hylidae I [Part 31 = Fig. 36]

F31 <-tree_subset(phy3, "Agalychnis lemur", levels_back=6)

F31a <-ggtree(F31) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F31b <-revts(F31a)

F31c <-F31b + xlim(-30, 15)

Fig_36 <-plot_grid(F31c, nrow = 1, rel_widths = c(0.4,1))

Fig_36

pdf("Fig_36.pdf", width=10, height=15)

Fig_36

dev.off()

#Hylidae II [Part 32 = Fig. 37]

F32 <-tree_subset(phy3, "Litoria iris", levels_back=15)

F32a <-ggtree(F32) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F32b <-revts(F32a)

F32c <-F32b + xlim(-40, 15)

Fig_37 <-plot_grid(F32c, nrow = 1, rel_widths = c(0.4,1))

Fig_37

pdf("Fig_37.pdf", width=10, height=15)

Fig_37

dev.off()

#Hylidae III [Part 33 = Fig. 38]

F33 <-tree_subset(phy3, "Boana pardalis", levels_back=13)

F33a <-ggtree(F33) + geom_tiplab(size = 2, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F33b <-revts(F33a)

F33c <-F33b + xlim(-45, 15)

Fig_38 <-plot_grid(F33c, nrow = 1, rel_widths = c(0.4,1))

Fig_38

pdf("Fig_38.pdf", width=10, height=15)

Fig_38

dev.off()

#Hylidae IV [Part 34 = Fig. 39]

F34 <-tree_subset(phy3, "Scinax staufferi", levels_back=11)

F34a <-ggtree(F34) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F34b <-revts(F34a)

F34c <-F34b + xlim(-45, 20)

Fig_39 <-plot_grid(F34c, nrow = 1, rel_widths = c(0.4,1))

Fig_39

pdf("Fig_39.pdf", width=10, height=15)

Fig_39

dev.off()

#Hylidae V [Part 35 = Fig. 40]

F35 <-tree_subset(phy3, "Osteocephalus taurinus", levels_back=9)

F35a <-ggtree(F35) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F35b <-revts(F35a)

F35c <-F35b + xlim(-35, 20)

Fig_40 <-plot_grid(F35c, nrow = 1, rel_widths = c(0.4,1))

Fig_40

pdf("Fig_40.pdf", width=10, height=15)

Fig_40

dev.off()

#Hylidae VI [Part 36 = Fig. 41]

F36 <-tree_subset(phy3, "Pseudis paradoxa", levels_back=6)

F36a <-ggtree(F36) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F36b <-revts(F36a)

F36c <-F36b + xlim(-50, 20)

Fig_41 <-plot_grid(F36c, nrow = 1, rel_widths = c(0.4,1))

Fig_41

pdf("Fig_41.pdf", width=10, height=15)

Fig_41

dev.off()

#Hylidae VII [Part 37 = Fig. 42]

F37 <-tree_subset(phy3, "Hyla arborea", levels_back=10)

F37a <-ggtree(F37) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F37b <-revts(F37a)

F37c <-F37b + xlim(-35, 15)

Fig_42 <-plot_grid(F37c, nrow = 1, rel_widths = c(0.4,1))

Fig_42

pdf("Fig_42.pdf", width=10, height=15)

Fig_42

dev.off()

#Ceuthomantidae + Eleutherodactylidae I [Part 38 = Fig. 43]

F38 <-tree_subset(phy3, "Eleutherodactylus varians", levels_back=14)

F38a <-ggtree(F38) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F38b <- collapse(F38a, node=770) + geom_point2(aes(subset=(node==770)), shape=21, size=5, fill='black')

F38c <- collapse(F38b, node=1405) + geom_point2(aes(subset=(node==1405)), shape=21, size=5, fill='black')

F38d <-revts(F38c)

F38e <-F38d + xlim(-60, 20)

Fig_43 <-plot_grid(F38e, nrow = 1, rel_widths = c(0.4,1))

Fig_43

pdf("Fig_43.pdf", width=10, height=15)

Fig_43

dev.off()

#Eleutherodactylidae II [Part 39 = Fig. 44]

F39 <-tree_subset(phy3, "Eleutherodactylus goini", levels_back=11)

F39a <-ggtree(F39) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F39b <-revts(F39a)

F39c <-F39b + xlim(-35, 20)

Fig_44 <-plot_grid(F39c, nrow = 1, rel_widths = c(0.4,1))

Fig_44

pdf("Fig_44.pdf", width=10, height=15)

Fig_44

dev.off()

#Brachycephalidae [Part 40 = Fig. 45]

F40 <-tree_subset(phy3, "Brachycephalus didactylus", levels_back=5)

F40a <-ggtree(F40) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F40b <-revts(F40a)

F40c <-F40b + xlim(-55, 20)

Fig_45 <-plot_grid(F40c, nrow = 1, rel_widths = c(0.4,1))

Fig_45

pdf("Fig_45.pdf", width=10, height=15)

Fig_45

dev.off()

#Craugastoridae I [Part 41 = Fig. 46]

F41 <-tree_subset(phy3, "Craugastor augusti", levels_back=7)

F41a <-ggtree(F41) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F41b <- collapse(F41a, node=514) + geom_point2(aes(subset=(node==514)), shape=21, size=5, fill='black')

F41c <-revts(F41b)

F41d <-F41c + xlim(-60, 20)

Fig_46 <-plot_grid(F41d, nrow = 1, rel_widths = c(0.4,1))

Fig_46

pdf("Fig_46.pdf", width=10, height=15)

Fig_46

dev.off()

#Craugastoridae II [Part 42 = Fig. 47]

F42 <-tree_subset(phy3, "Yunganastes fraudator", levels_back=7)

F42a <-ggtree(F42) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F42b <- collapse(F42a, node=501) + geom_point2(aes(subset=(node==501)), shape=21, size=5, fill='black')

F42c <-revts(F42b)

F42d <-F42c + xlim(-50, 20)

Fig_47 <-plot_grid(F42d, nrow = 1, rel_widths = c(0.4,1))

Fig_47

pdf("Fig_47.pdf", width=10, height=15)

Fig_47

dev.off()

#Craugastoridae III [Part 43 = Fig. 48]

F43 <-tree_subset(phy3, "Pristimantis elegans", levels_back=20)

F43a <-ggtree(F43) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F43b <- collapse(F43a, node=435) + geom_point2(aes(subset=(node==435)), shape=21, size=5, fill='black')

F43c <-revts(F43b)

F43d <-F43c + xlim(-45, 20)

Fig_48 <-plot_grid(F43d, nrow = 1, rel_widths = c(0.4,1))

Fig_48

pdf("Fig_48.pdf", width=10, height=15)

Fig_48

dev.off()

#Craugastoridae IV [Part 44 = Fig. 49]

F44 <-tree_subset(phy3, "Pristimantis elegans", levels_back=17)

F44a <-ggtree(F44) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F44b <- collapse(F44a, node=221) + geom_point2(aes(subset=(node==221)), shape=21, size=5, fill='black')

F44c <-revts(F44b)

F44d <-F44c + xlim(-30, 15)

Fig_49 <-plot_grid(F44d, nrow = 1, rel_widths = c(0.4,1))

pdf("Fig_49.pdf", width=10, height=15)

Fig_49

dev.off()

#Craugastoridae V [Part 45 = Fig. 50]

F45 <-tree_subset(phy3, "Pristimantis elegans", levels_back=13)

F45a <-ggtree(F45) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F45b <-revts(F45a)

F45c <-F45b + xlim(-20, 10)

Fig_50 <-plot_grid(F45c, nrow = 1, rel_widths = c(0.4,1))

Fig_50

pdf("Fig_50.pdf", width=10, height=15)

Fig_50

dev.off()

#Dendrobatidae I [Part 46 = Fig. 51]

F46 <-tree_subset(phy3, "Allobates ornatus", levels_back=13)

F46a <-ggtree(F46) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F46b <-revts(F46a)

F46c <-F46b + xlim(-40, 20)

Fig_51 <-plot_grid(F46c, nrow = 1, rel_widths = c(0.4,1))

Fig_51

pdf("Fig_51.pdf", width=10, height=15)

Fig_51

dev.off()

#Dendrobatidae II [Part 47 = Fig. 52]

F47 <-tree_subset(phy3, "Ameerega parvula", levels_back=8)

F47a <-ggtree(F47) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F47b <-revts(F47a)

F47c <-F47b + xlim(-43, 15)

Fig_52 <-plot_grid(F47c, nrow = 1, rel_widths = c(0.4,1))

Fig_52

pdf("Fig_52.pdf", width=10, height=15)

Fig_52

dev.off()

#Centrolenidae and Allophrynidae [Part 48 = Fig. 53]

F48 <-tree_subset(phy3, "Espadarana prosoblepon", levels_back=12)

F48a <-ggtree(F48) + geom_tiplab(size = 2.5, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F48b <-revts(F48a)

F48c <-F48b + xlim(-45, 20)

Fig_53 <-plot_grid(F48c, nrow = 1, rel_widths = c(0.4,1))

Fig_53

pdf("Fig_53.pdf", width=10, height=15)

Fig_53

dev.off()

#Leptodactylidae I [Part 49 = Fig. 54]

F49 <-tree_subset(phy3, "Pleurodema bibroni", levels_back=8)

F49a <-ggtree(F49) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F49b <-revts(F49a)

F49c <-F49b + xlim(-55, 25)

Fig_54 <-plot_grid(F49c, nrow = 1, rel_widths = c(0.4,1))

Fig_54

pdf("Fig_54.pdf", width=10, height=15)

Fig_54

dev.off()

#Leptodactylidae II [Part 50 = Fig. 55]

F50 <-tree_subset(phy3, "Leptodactylus fuscus", levels_back=8)

F50a <-ggtree(F50) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F50b <-revts(F50a)

F50c <-F50b + xlim(-55, 20)

Fig_55 <-plot_grid(F50c, nrow = 1, rel_widths = c(0.4,1))

Fig_55

pdf("Fig_55.pdf", width=10, height=15)

Fig_55

dev.off()

#Odonotphrynidae [Part 51 = Fig. 56]

F51 <-tree_subset(phy3, "Proceratophrys cristiceps", levels_back=4)

F51a <-ggtree(F51) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F51b <-revts(F51a)

F51c <-F51b + xlim(-35, 15)

Fig_56 <-plot_grid(F51c, nrow = 1, rel_widths = c(0.4,1))

Fig_56

pdf("Fig_56.pdf", width=10, height=15)

Fig_56

dev.off()

#Bufonidae I [Part 52 = Fig. 57]

F52 <-tree_subset(phy3, "Peltophryne dunni", levels_back=12)

F52a <-ggtree(F52) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.8) + theme_tree2()

F52b <- collapse(F52a, node=453) + geom_point2(aes(subset=(node==453)), shape=21, size=5, fill='black')

F52c <-revts(F52b)

F52d <-F52c + xlim(-60, 20)

Fig_57 <-plot_grid(F52d, nrow = 1, rel_widths = c(0.4,1))

Fig_57

pdf("Fig_57.pdf", width=10, height=15)

Fig_57

dev.off()

#Bufonidae II [Part 53 = Fig. 58]

F53<-tree_subset(phy3, "Rhinella marina", levels_back=12)

F53a <-ggtree(F53) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.5) + theme_tree2()

F53b <-revts(F53a)

F53c <-F53b + xlim(-30, 10)

Fig_58 <-plot_grid(F53c, nrow = 1, rel_widths = c(0.4,1))

Fig_58

pdf("Fig_58.pdf", width=10, height=15)

Fig_58

dev.off()

#Bufonidae III [Part 54 = Fig. 59]

F54<-tree_subset(phy3, "Bufo bufo", levels_back=11)

F54a <-ggtree(F54) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.5) + theme_tree2()

F54b <- collapse(F54a, node=248) + geom_point2(aes(subset=(node==248)), shape=21, size=5, fill='black')

F54c <-revts(F54b)

F54d <-F54c + xlim(-30, 10)

Fig_59 <-plot_grid(F54d, nrow = 1, rel_widths = c(0.4,1))

Fig_59

pdf("Fig_59.pdf", width=10, height=15)

Fig_59

dev.off()

#Bufonidae IV [Part 55 = Fig. 60]

F60<-tree_subset(phy3, "Bufo bufo", levels_back=8)

F60a <-ggtree(F60) + geom_tiplab(size = 3, align=FALSE, fontface='italic') + geom_nodelab(size=2.5, nudge_x=0.5) + theme_tree2()

F60b <-revts(F60a)

F60c <-F60b + xlim(-30, 15)

Fig_60 <-plot_grid(F60c, nrow = 1, rel_widths = c(0.4,1))

Fig_60

pdf("Fig_60.pdf", width=10, height=15)

Fig_60

dev.off()

#Supplementary file S6 Densitree plot of bootstrap trees

phyX <-read.tree("bs-100-combined.tre")

phyY <-lapply(phyX, keep.tip, to_keep)

tiporder <- as.character(c("Leiopelma_archeyi", "Ascaphus_truei", "Bombina_bombina", "Alytes_obstetricans", "Rhinophrynus_dorsalis", "Xenopus_laevis", "Scaphiopus_holbrookii", "Pelodytes_ibericus", "Pelobates_cultripes", "Brachytarsophrys_carinense", "Heleophryne_regis", "Myobatrachus_gouldii", "Calyptocephalella_gayi", "Rhinoderma_darwinii", "Alsodes_nodosus", "Batrachyla_leptopus", "Hylodes_ornatus", "Cycloramphus_carvalhoi", "Telmatobius_culeus", "Hyla_arborea", "Ceratophrys_ornata", "Hemiphractus_proboscideus", "Ceuthomantis_smaragdinus", "Eleutherodactylus_longipes", "Brachycephalus_ephippium", "Craugastor_podiciferus", "Dendrobates_auratus", "Odontophrynus_americanus", "Bufo_bufo", "Leptodactylus_fuscus", "Allophryne_ruthveni", "Centrolene_muelleri", "Nasikabatrachus_sahyadrensis", "Sooglossus_sechellensis", "Gastrophryne_olivacea", "Hemisus_marmoratus", "Breviceps_macrops","Hyperolius_ruvuensis", "Arthroleptis_stenodactylus",  "Micrixalus_fuscus", "Odontobatrachus_natator", "Ptychadena_cooperi", "Phrynobatrachus_krefftii", "Pyxicephalus_adspersus", "Petropedetes_perreti", "Conraua_goliath", "Cornufer_guppyi", "Nyctibatrachus_major", "Indirana_beddomii", "Quasipaa_spinosa", "Amolops_torrentis", "Polypedates_leucomystax", "Mantella_viridis"))      

Fig_S6 <-ggdensitree(phyY, type = "cladogram", alpha=0.05, colour='purple', align.tips = TRUE, tip.order = tiporder) + geom_tiplab(size=3) + xlim(-210, 35) + theme_tree2()

Fig_S6

pdf("Fig_S6.pdf", width=7, height=7)

Fig_S6

dev.off()


