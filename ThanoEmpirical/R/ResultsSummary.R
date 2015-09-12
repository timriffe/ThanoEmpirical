# Deprecated

#if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
#	# if I'm on the laptop
#	setwd("/home/tim/git/ThanoEmpirical/ThanoEmpirical")
#} else {
#	# in that case I'm on Berkeley system, and other people in the dept can run this too
#	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/ThanoEmpirical/ThanoEmpirical"))
#}
#library(ggplot2)
#
#Meta 			<- read.csv( "Data/PercentThano.csv",stringsAsFactors=FALSE)
#Meta$Mean 		<- rowMeans(Meta[,c("Male","Female")])
#a 				<- boxplot(Mean~Group,data=Meta, plot=FALSE)
#Meta$Group 		<- factor(Meta$Group, levels = a$names[order(a$stats[3,])],ordered = TRUE)
#
#Meta$ThermoM 	<- NULL
#Meta$ThermoF 	<- NULL
#Male         	<- Female <- Meta
#Male$Female  	<- NULL
#Female$Male  	<- NULL
#colnames(Male)[colnames(Male) == "Male"] 		<- "percent"
#colnames(Female)[colnames(Female) == "Female"] 	<- "percent"
#Male$Sex 		<- "male"
#Female$Sex 		<- "female"
#Meta2 			<- rbind(Male,Female)
#
#
#a <- ggplot(Meta2, aes(x=Group, y=percent, fill=Sex)) + geom_boxplot() + 
#		theme(axis.text.x = element_text(size = 12, colour = "black"),
#				legend.text=element_text(size=12, colour = "black"))
#ggsave("Figures/ResultsBoxplot.pdf",a,width=9,height=5)
