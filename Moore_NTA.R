#### 4 - Adapted version of Moore's Neighborhood to trace the lower edge of suture line (goes from left to right of image) ####

mooren <- function(P) {
	P1 <- c(P[1]-1, P[2]-1)
	P2 <- c(P[1], P[2]-1)
	P3 <- c(P[1]+1, P[2]-1)
	P4 <- c(P[1]+1, P[2])
	P5 <- c(P[1]+1, P[2]+1)
	P6 <- c(P[1], P[2]+1)
	P7 <- c(P[1]-1, P[2]+1)
	P8 <- c(P[1]-1, P[2])
	Moore <- rbind(P1,P2,P3,P4,P5,P6,P7,P8)
	} #Internal function to compute Moore's Neighborhood for each pixel, necessary for next function to work

Moore.alg <- function(mat, NLM=200, progress=T) {

if (progress==T) {image(mat)} #Having a look at the algo working. Much master if progress=F

y <- length(mat[,1])
while (mat[y,1] == 255) {y <- y-1} #Find first black px starting from bottom left corner

print(paste("Found first black pixel at", y, 1)) #Just checking

B <- list() #Make empty list
B[[1]] <- s <- P <- c(2,y) #Starting px, x=2 because we will add a column to the matrix later
Moore <- mooren(P) #Compute Moore neighborhood of starting px using previously defined function
matt <- cbind(rep(255, length(mat[,1])), mat) #Add one black column before matrix to avoid the loop from stopping due to absence of pixels (cutoff of Moore Neighborhood on left side of matrix)
b <- Moore[6,] #Backtrack px since we came from bottom
k <- Moore[7,] #Next clockwise px from the backtrack px
i <- 1 #Initiate index to fill the list in the following loop

while (P[1] < dim(matt)[2]) { #Stopping criterion is maximum x (e.g. when we reach right side of line)

if (progress==T) {points(P[2]/dim(matt)[1], P[1]/dim(matt)[2], cex=2, pch=20)} #To have a look at the process

	if (matt[k[2], k[1]] == 0) {
		i <- i+1
		B[[i]] <- k
		P <- k
		Moore <- mooren(P)
		k <- b
		} else {
			b <- k
			if (which(k[1]==Moore[,1] & k[2]==Moore[,2])!=8) {
			k <- Moore[which(k[1]==Moore[,1] & k[2]==Moore[,2])+1,]
			} else {k <- Moore[1,]}
		}
	}
o <- matrix(unlist(B), ncol=2, byrow=T)
subo <- o[round(seq(from=1, to=dim(o)[1], length.out=NLM)),]
list(o=o, subo=subo)
}


#### 5 - Example run function on imported matrices (in array form) ####

NLM <- 200 #Define number of landmarks
array_lines <- array(NA, dim=c(NLM, 2, length(list_mat)))

for (i in 1:length(list_mat)) {
	print(i)
	oo <- Moore.alg(list_mat[[i]], NLM=NLM, progress=T)
	array_lines[,,i] <- oo$subo
	}
	
