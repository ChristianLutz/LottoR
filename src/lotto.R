#
#
# @author: Christian Lutz
# @version: 0.2.0
#
# Beschreibung:
#
#
#
#

subs <- function(x,y) {
	for( i in y ) {
		if( any(x==i) ) {
			
		}
		else {
			return(0);
		}
	}
	return(1);
}

# Fakultät berechnen.
fak <- function(n) {
	p <- 1;
	for ( i in 1:n )
		p <- p * i;
	return(p)
}


# Berechne N über K Matrix
# dabei ist ein Vector, welcher alle N möglichen Element beinhaltet
# und k gibt an wie viele Elemente gezogen werden sollen.
# 
# return Matrix (NÜberK)xK
nokIdxMatrix <- function(x,y) {

	#prüfe ob x vector ist -> Todo
	#prüfe ob y kleiner length(x) ist -> Todo

	# sort vector x <- apply(x,1,sort);

	n <- fak(length(x));	#n!
	k <- fak(y);	#k!
	nk <- fak(abs(length(x)-y)); # (n-k)!
	
	nrNK <- n/(nk*k);
	
	
	# create result matrix
	res <- matrix(0,nrNK,y);
	revIdx <- seq( y, 1, -1 );
	idxStack <- c(1:y) # init fill
	
	for( row in 1:nrNK ) {
		
		# kopiere Inhalt basierend auf dem Index vom Eingabe Vector in die Matrix
		res[row,] <- idxStack;
		
		# erzeuge nächsten Index Eintrag
		for( feIdx in revIdx ) {
			if( idxStack[feIdx] <= (length(x) - revIdx[feIdx]) ) {
				idxStack[feIdx] <- idxStack[feIdx] + 1;
				if( feIdx < y ) {
					for( retIdx in (feIdx+1):y ) {
						idxStack[retIdx] <- idxStack[retIdx-1]+1;
					}
				}
				break;
			}
		}
	}
	
	return(res)
}


#
# x = index vector
# y = component vector
#
# take each element of the index vector and concatenate a return vector with the
# component at the giben index
#
mapIdxToVector <- function(x,y) {
	ret <- c(x);
	for( i in 1:length(x) ) {
		ret[i] <- y[x[i]];
	}
	return(ret);
}


lotto <- function() {
	
	pfadUndDateiName <- "/Users/christian/Documents/Workspace/Scripte/LottoS/lottozahlen2009.txt"
	dataset <- read.csv(pfadUndDateiName, TRUE, sep=",", quote="");	zahlen <- c(1:49);
	
	#
	#### PREPROCESSING ########
	
	# entferne Datum
	dataset <- subset(dataset, select=c(2:8));
	# zu einer Matrix konvertieren
	dataset <- data.matrix(dataset); 
	# die ersten sechs zahlen sortieren 
	data <- apply(dataset[,1:6],1,sort);
	dataset <- cbind( t(data), dataset[,7] );
	# sortiere Reihenfolge einer jeder Zeile, wichtig für das hashen bei den Kombinationen
	sortedDataSet <- t(apply(dataset,1,sort));
	# lottoNr
	lottoNrs <- c(1:49);
	
	
	#
	#### AUSWERTUNG BEGINNEN ########
	
	#
	# Infos:
	#
	
	print('Anzahl Ziehungen: ')
	print(length(dataset[,1]))
	print('')
	
	#
	# berechne Verteilung der einzelner Kugeln
	#
	
	print('Berechne Verteilung ...')

	verteilung <- vector('integer',49);
	for ( i in lottoNrs ) {
		verteilung[i] <- length( which( dataset==i ));
	}
#	barplot(verteilung)
	
	
	#	
	# berechne durchschnittlichen Distanz zwischen erster und letzer Zahl
	#
	
	print('Berechne Distanz ...')
	
	dif <- vector('integer', length(dataset[,1]) );
	for ( i in 1:length(dataset[,1]) ) {
		dif[i] <- max(dataset[i,1:6]) - min(dataset[i,1:6]);
	}
	
	print(max(dif));				# maximale Distanz
	print(min(dif));				# minimale Distanz
	print(sum(dif) / length(dif)); 	# mean Distanz
#	boxplot(dif)					# Box Plot Grafik mit Median etc.
	
	
	
	#
	# Kombinationen die bereits mehrmals gezogen wurden
	#
	
	print('Berechne doppelte Kombinationen ...')
	
	print( dataset[duplicated(dataset),] );
	
	
	
	#
	# Haeufigste Superzahl
	#
	
	print('Berechne Superzahlverteilung ...');
	numSZ <- table( dataset[,7] ); # beinhaltet für jede Zahl wie oft diese als Superzahl gezogen wurde.
									# dabei entspricht der Index der entsprechenden Kugel
	print.table( numSZ );
	
	
	
	#
	# haeufigste Zweierkombination ( 1128 Moeglichkeiten )
	#
	
	print('-------------------------------------------------------------------');
	print('Berechne haeufigsten zweier Kombinationen ...')
	
	a <- Sys.time();
	nrKombinationen <- 21;	#Anzahl Kombinationen, bei 2 aus 7
	kombination <- matrix(0,1176,3); 	#Columns: x1, x2, Anzahl der Matches
	index <- 1;
	for ( i in 1:49 ) {
		j <- i+1;
		while ( j < 50 ) {
			kombination[index,1] <- i;
			kombination[index,2] <- j;
			index <- index + 1;
			j <- j + 1;
		}	
	}
	
	b <- Sys.time();
	print(difftime(b,a));
	
	for ( j in 1:length(kombination[,1]) ) {
		kombination[j,3] <- sum(apply(dataset, 1, subs, kombination[j,1:2] ));
	}
	
	c <- Sys.time();
	print(difftime(c,b));

	kombination <- kombination[order(kombination[,3], na.last=TRUE, decreasing=TRUE),];
	print( kombination[1,])
	print( kombination[2,])
	print( kombination[3,])
	
	

	#
	# haeufigste Dreierkombination ( 18424 Moeglichkeiten )
	#
	
	print('-------------------------------------------------------------------');
	print('Berechne haeufigsten dreier Kombinationen ...')
	
	

	a <- Sys.time()
	nrKombinationen <- 35; 				#Anzahl Kombination die es bei 3 aus 7 geben kann
	hash <- list();

	
	for ( i in 1:49 ) {
		j <- i+1;
		while ( j < 50 ) {
			k <- j+1;
			while( k < 50 ) {
				hash[[paste(c(i,j,k), collapse = '.')]] <- 0;
				k <- k + 1;
			}
			j <- j + 1;
		}	
	}
	indexMX <- matrix(0,35,3); 	#Columns: x1, x2, x3, Anzahl der Matches
	index <- 1;
	for ( i in 1:7 ) {
		j <- i+1;
		while ( j < 8 ) {
			k <- j+1;
			while( k < 8 ) {
				indexMX[index,1] <- i;
				indexMX[index,2] <- j;
				indexMX[index,3] <- k;
				index <- index + 1;
				k <- k + 1;
			}
			j <- j + 1;
		}	
	}
#	print(indexMX);
	
	
	b <- Sys.time();
	print(difftime(b,a));
	
	# suche und zähle jede Kombination
	for( i in 1:nrow(dataset) ) {
		for ( j in 1:nrow(indexMX) ) {
			fi <- c(sortedDataSet[i, indexMX[j,1]], sortedDataSet[i, indexMX[j,2]], sortedDataSet[i, indexMX[j,3]]);
			hash[[paste(fi, collapse = '.')]] <- hash[[paste(fi, collapse = '.')]] + 1;
		}
	}
	
	c <- Sys.time();
	print(difftime(c,b));
	
	kombination <- matrix(0,18424,4); 	#Columns: x1, x2, x3, Anzahl der Matches
	index <- 1;
	for ( i in 1:49 ) {
		j <- i+1;
		while ( j < 50 ) {
			k <- j+1;
			while( k < 50 ) {
				kombination[index,1] <- i;
				kombination[index,2] <- j;
				kombination[index,3] <- k;
				kombination[index,4] <- hash[[paste(c(i,j,k), collapse = '.')]];
				index <- index + 1;
				k <- k + 1;
			}
			j <- j + 1;
		}	
	}
	kombination <- kombination[order(kombination[,4], na.last=TRUE, decreasing=TRUE),];
	print( kombination[1,])
	print( kombination[2,])
	print( kombination[3,])
	
	
	
	#
	# haeufigste Viererkombination ( 211876 Moeglichkeiten )
	#
	
	print('-------------------------------------------------------------------');
	print('Berechne haeufigsten vierer Kombinationen ...')

	a <- Sys.time();
	
	#prepare hash()
	hash <- list();
	
	hashIdx <- nokIdxMatrix( lottoNrs, 4);
	mappedV <- t(apply(hashIdx, 1, mapIdxToVector, lottoNrs)); # alle 211876 möglichen vierer Kombinationen
	for( i in 1:nrow(mappedV) ) {
		hash[[paste(mappedV[i,], collapse = '.')]] <- 0; # Wert initialisieren
	}
	
	# Index matrix für jede Ziehung, damit sind die Indizes für alle 35 Kombinationen abgelegt.
	indexMX <- nokIdxMatrix( c(1:7),4); #Columns: x1, x2, x3, x4, Anzahl der Matches
	
	b <- Sys.time();
	print(difftime(b,a));
	
	# suche und zähle jede Kombination
	for( i in 1:nrow(sortedDataSet) ) {
		mappedZiehung <- t(apply(indexMX, 1, mapIdxToVector, sortedDataSet[i,]));
		for ( j in 1:nrow(mappedZiehung) ) {
			hash[[paste(mappedZiehung[j,], collapse = '.')]] <- hash[[paste(mappedZiehung[j,], collapse = '.')]] + 1;
		}
	}
	
	mappedV <- cbind( mappedV, matrix(0,nrow(mappedV),1) ); #Zusätzliche Spalte für die Anzahl der Matches
	for( i in 1:nrow(mappedV) ) {
		mappedV[i,5] <- hash[[paste(mappedV[i,1:4], collapse = '.')]];
	}
	
	c <- Sys.time();
	print(difftime(c,b));
	
	mappedV <- mappedV[order(mappedV[,5], na.last=TRUE, decreasing=TRUE),];
	print( mappedV[1,]);
	print( mappedV[2,]);
	print( mappedV[3,]);

	
	#
	# Berechne durchschnittlicher Abstand bis eine Zahl in der nächsten Ziehung wieder gelost wird.
	#
	
	
}
