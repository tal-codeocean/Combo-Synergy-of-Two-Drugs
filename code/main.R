require(RSQLite)
require(plotly)
 

make.plot<-function(drugA, drugB, cell){
	query<-paste('select drugA_name, drugA_conc, drugB_name, drugB_conc, x2x0, HSA, Bliss from combo where drugA_name in ("', 
		drugA, '","', drugB, '") and drugB_name in ("', drugA, '","', drugB, '") and cell_line="', 
		cell, '" order by drugA_conc, drugB_conc', sep='')

	data<-dbGetQuery(db,query)

	if (dim(data)[1]>0) {
		drugA_conc<-unique(data[,"drugA_conc"])
		drugB_conc<-unique(data[,"drugB_conc"])
		a.conc<-rep(c(0,1,2,3),each=4)
		b.conc<-rep(c(0,1,2,3),4)

		observed.data<-matrix(data[,"x2x0"], nrow=4, ncol=4, byrow=TRUE)
		hsa<-matrix(data[,"HSA"], nrow=4, ncol=4, byrow=TRUE)
		bliss<-matrix(data[,"Bliss"], nrow=4, ncol=4, byrow=TRUE)

		p<-plot_ly(z=observed.data, type="surface", colors="Blue", opacity=0.4, , showscale=FALSE) %>%
		add_trace(x=b.conc, y=a.conc, z=data$x2x0, type="scatter3d", marker=list(color="rgb(5, 10, 172)"), opacity=0.99, mode="markers", name="Observed Data") %>%
		add_trace(z=hsa, type="surface", colors="Red", opacity=0.4) %>%
		add_trace(x=b.conc, y=a.conc, z=data$HSA, type="scatter3d", marker=list(color="rgb(255, 0, 0)"), opacity=0.99, mode="markers", name="Predicted (HSA)") %>%
		add_trace(z=bliss, type="surface", colors="Green", opacity=0.4) %>%
		add_trace(x=b.conc, y=a.conc, z=data$Bliss, type="scatter3d", marker=list(color="rgb(78, 190, 0)"), opacity=0.99, mode="markers", name="Predicted (Bliss)") %>%
			layout(title = paste(drugA, "&", drugB, "Combination"),
				scene = list(
					zaxis = list(title = "viability", nticks=10),
					xaxis = list(title = "drugA concentration", tickvals=c(0,1,2,3), ticktext=drugA_conc), 
					yaxis = list(title = "drugB concentration", tickvals=c(0,1,2,3), ticktext=drugB_conc), 
					aspectratio=list(x=1, y=1, z=1)))
	
		htmlwidgets::saveWidget(as_widget(p), "/output/plot.html")

			}
	else {
	    write("    Sorry, This combination does not exist. Try different Drug cominations", "/output/plot.html")
		cat("This combination does not exist")
	    }		
	}
	
sqlite    <- dbDriver("SQLite")
db <- dbConnect(sqlite,"/input/DrugCombo.db")

# get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    drugA<-args[1]
    drugB<-args[2]
    cell<-args[3]
    make.plot(drugA, drugB, cell)
}else{
    make.plot("5-FU", "ABT-888", "A2058")
}
#make.plot("5-FU", "ABT-888", "A2058")
#make.plot("5-FU", "Erlotinib", "A2058")

dbDisconnect(db)