## reading Detection history
DHist <- list.files(pattern="*.csv")
## defining the occassion length
	# OL <- 5 # one occasion is 5 days 
	OL <- c(3,5,7,13) ## if you want different length of occassions
	
for (l in 1:length(DHist)){
	assign(DHist[l], read.csv(DHist[l]))
	DH <- read.csv(DHist[l])
	name <- strsplit(DHist[l], ".csv")
	Species <- name[1] # Name of your species if the filename has been named with species

	# number of cameratrap days
	IN_Occ <- ncol(DH) -1
	
	## getting the new Occasion length
	Occ_no <- IN_Occ%/%OL # getting the collapsed occasion length
	Occ_no
	N <- nrow(DH)
	head(DH)

	## Now since we got different length of occassions we run a loop
	for(k in 1:length(Occ_no)){
	## create a new matrix to keep the new DH
	DH2 <-  matrix(nrow = N, ncol = Occ_no[k])
	# now doing the loop to get the maximum value from within occasion days
		for(i in 1:N){
			for(j in 1:Occ_no[k]){
				DH2[i, j] <- max(DH[i,(((j-1)*OL[k])+2):((OL[k]*j)+1)]) ## 2 here because the first column is trapsite
				}}
			## Making it neat
			colnames(DH2) <- paste("Occ", as.character(1:Occ_no[k]))
			rownames(DH2) <- DH[,1]
		# creating a csv file
		name <- paste(Species, "_Occ", as.character(OL[k]),".csv")
		name <- gsub(" ", "", name) 
		write.csv(DH2, name)
	}
}