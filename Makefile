all: mathbio2011.pdf dropbox

mathbio2011.pdf: mathbio2011.tex Figs/errors_graphically_new.pdf Figs/gve_dist.pdf Figs/xchr_fig.pdf Figs/gve_scheme.pdf Figs/gve.pdf Figs/insulin_lod.pdf
	pdflatex mathbio2011

Figs/errors_graphically_new.pdf: R/plot_plates_new.R
	cd R;R CMD BATCH plot_plates_new.R

Figs/xchr_fig.pdf: R/xchr_fig.R
	cd R;R CMD BATCH xchr_fig.R

Figs/gve.pdf: R/gve.R
	cd R;R CMD BATCH gve.R

Figs/gve_dist.pdf: R/gve_dist.R
	cd R;R CMD BATCH gve_dist.R

Figs/gve_scheme.pdf: R/gve_scheme.R
	cd R;R CMD BATCH gve_scheme.R

Figs/insulin_lod.pdf: R/insulin_fig.R
	cd R;R CMD BATCH insulin_fig.R

dropbox: ~/DropBox/Talks/mathbio2011.pdf 

~/DropBox/Talks/mathbio2011.pdf : mathbio2011.pdf
	\cp mathbio2011.pdf ~/DropBox/Talks/mathbio2011.pdf

tar: mathbio2011.pdf
	tar czvf mathbio2011.tgz mathbio2011.tex Figs/* R/* Makefile beamerthemeconfposter.sty mathbio2011.pdf
