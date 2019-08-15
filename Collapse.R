## reading Detection history
DH <- read.csv("originaldh.csv")

# number of cameratrap days
IN_Occ <- ncol(DH) -1

## defining the occassion length
OL <- 5 # one occasion is 5 days 

## getting the new Occasion length
Occ_no <- IN_Occ%/%OL # getting the collapsed occasion length
Occ_no
N <- nrow(DH)
head(DH)

## create a new matrix to keep the new DH
DH2 <-  matrix(nrow = N, ncol = Occ_no)

# now doing the loop to get the maximum value from within occasion days
for(i in 1:N){
for(j in 1:Occ_no){
DH2[i, j] <- max(DH[i,(((j-1)*OL)+2):((OL*j)+2)]) ## 2 here because the first column is trapsite
}}

## Making it neat
colnames(DH2) <- paste("Occ", as.character(1:Occ_no))
rownames(DH2) <- DH[,1]
DH2

# creating a csv file
write.csv(DH2, "DH.csv")
