#Geom_signif code
geom_signif(comparisons = list(c("Paleocene","PETM")),
  map_signif_level=TRUE)+
geom_signif(comparisons = list(c("PETM","Eocene")),
  map_signif_level=TRUE)+
geom_signif(comparisons = list(c("Paleocene","Eocene")),
  map_signif_level=TRUE)