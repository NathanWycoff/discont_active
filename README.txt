## Information.

This repository accompanies the article "Surrogate Active Subspaces for Jump-Discontinuous Functions" available on arXiv https://arxiv.org/abs/2310.10907

It contains R scripts and Flee simulator output needed to reproduce the figures in that article.

The following R packages need to be installed:
install.packages(c("hetGP","activegp","akima","plot3D","xtable"))

## Figure 1
Rscript R/figure_1.R

## Figure 2
Rscript R/figure_2.R

## Figure 3
./run_toy.sh
(You may need to give run_toy.sh permission to execute first)

## Figure 4
Rscript R/flee_bakeoff.R
Rscript R/flee_run_asm.R
Rscript R/figure_4_top.R
Rscript R/figure_4_mid.R
Rscript R/figure_4_bot.R

