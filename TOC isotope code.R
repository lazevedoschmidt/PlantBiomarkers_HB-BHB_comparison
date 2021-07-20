# # Basic box plot
# #d13C TOC
# TOC_plot <- d13C_alk %>%
#   mutate(name = fct_relevel(Epoch, 
#                             "Paleocene", "PETM", "Eocene")) %>%
#   ggplot(aes(x = Epoch, y = d13C.TOC, 
#              fill = Basin)) + 
#   geom_boxplot(stat = "boxplot", data = d13C_alk, outlier.colour="red", 
#                outlier.shape=8,
#                outlier.size=4,
#                show.legend = TRUE)+
#   geom_jitter(width = 0.2, shape=d13C_alk$Basin, na.rm = TRUE)+
#   scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2")) +
#   scale_x_discrete(limits=c("Paleocene","PETM","Eocene"))+
#   labs(x="", y="d13C TOC")+
#   theme(legend.position = "none")+
#   ggtitle("Carbon changes across PETM boundary")+
#   geom_signif(comparisons = list(c("Paleocene","PETM")),
#               map_signif_level=TRUE)+
#   geom_signif(comparisons = list(c("PETM","Eocene")),
#               map_signif_level=TRUE)+
#   geom_signif(comparisons = list(c("Paleocene","Eocene")),
#               map_signif_level=TRUE)+
#   scale_y_continuous(expand = expansion(mult = c(0.05,0.1)))
# TOC_plot
# 
# #Adding pvalues 
# df_pair_TOC <- pairwiseComparisons::pairwise_comparisons(
#   data = d13C_alk,
#   x = Epoch,
#   y = d13C.TOC, 
#   type = "parametric",
#   var.equal = TRUE,
#   paired = FALSE,
#   p.adjust.method = "bonferroni")
# 
# # adding a geom for pairwise comparisons
# TOC_plot<- ggstatsplot:::ggsignif_adder(
#   plot = TOC_plot,
#   data =d13C_alk,
#   x = Epoch,
#   y = d13C.TOC,
#   df_pairwise = df_pair_TOC)
# TOC_plot
# TOC_plot <- TOC_plot +stat_compare_means(aes(group = Basin), label.y = -22.5, 
#                                          method = "anova") 
# TOC_plot