#### After Image manipulation using ImageMagick : conversion of images to text format ####
#### Prerequisites ####
setwd("directory/of/text/files")
source("https://www.researchgate.net/profile/Julien-Claude/publication/350439729_Rfunctions1_R_functions_for_geometric_morphometrics_developed_in_Claude_J_2008_Morphometrics_with_R/data/605f5625a6fdccbfea0f436a/Rfunctions1.txt")
library(MASS)
library(Momocs)

#### 1 - Import image in text format to R ####

files <- list.files()[grep(".txt", list.files())]

wh <- gsub("mm.*","", files)
wh <- gsub(".*\\(","", wh)
wh <- gsub(" ","", wh)
wh <- gsub("wh","", wh)
wh <- as.numeric(wh)

list_mat <- list() #Empty list to store matrices to be analysed

for (j in 1:length(files)) { #Start of loop filling list

a <- scan(files[j], what="character") #Import text file
print(j)

a <- a[-c(1:(grep("0,0:", a)[1]-1))]

coord <- a[grep(":", a)]

#### 2 - Extract pixel data to data frame ####

X <- Y <- rep(NA, length(a)/4) #Create empty vectors
v1 <- unlist(strsplit(coord, ","))

g <- a[grep("gray", a)]
g <- gsub("gray\\(", "", g)
g <- gsub("graya\\(", "", g)
g <- gsub("\\)", "", g)
g <- gsub(",.*", "", g)

for (i in 1:(length(a)/4)) {
   X[i] <- v1[i*2-1]
   Y[i] <- v1[i*2]
} #Fill vectors with X,Y coordinates and gray value

Y <- as.numeric(gsub(":", "", Y))
X <- as.numeric(X)
g <- as.numeric(g) #Make everything numeric

data <- data.frame(X=X, Y=Y, gray=g) #Make it a dataframe

#### 3 - Create matrix representing image (0 / 255 values) with rows = Y coordinates and columns = X coordinates ####

mat <- matrix(0, ncol=length(unique(data[,1]))-1, nrow=length(unique(data[,2]))-1)
for (i in 1:dim(data)[1]) {
	X <- data[i,1]
	Y <- data[i,2]
	mat[Y,X] <- data[i,3]
	}

list_mat[[j]] <- mat

} #End of loop filling list

