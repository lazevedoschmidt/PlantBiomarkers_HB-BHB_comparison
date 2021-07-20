#new figure configuration 04.23.21

#Making n-alkane super plot
bottom <- plot_grid(avg_terp_error,
                    nrow = 1,
                    ncol = 1, 
                    labels = c("B"))
N_alkanes <- plot_grid(oddplot, bottom, 
                       ncol = 1, 
                       #rel_widths = c(1, 2),
                       labels = c("A", ""))
N_alkanes

#Oxidation figures
bottomrow <- plot_grid(hopplot, bacplot,
                       nrow = 1, 
                       ncol = 2, 
                       labels = c("C","D"))
bottomrow
Oxfig <- plot_grid(lith_Pr.PhvsTAR,
                   paq_plot,
                   bottomrow,
                   labels = c('A', 'B',""),
                   ncol = 1)
Oxfig

#saving figures:
ggsave("Figures/N_alkanes_super.pdf", N_alkanes,height = 12, width = 9)
ggsave("Figures/Oxfig_PAQ.pdf", Oxfig, height = 14, width = 10)
