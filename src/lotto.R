#
#
# @author: Christian Lutz
# @version: 0.1.3
#
# Beschreibung:
#
#
#
#

subs <- function(x,y) {
	#print(y %in% x * 1)
	#if( any(x == y[1]) && any(x == y[2]) ) {
	for( i in y ) {
		if( any(x==i) ) {
			
		}
		else {
			return(0);
		}
	}
	return(1);
	
	#if( sum( y %in% x * 1 ) == length(y) ) {
	#	return(1);
	#}
	#return(0);
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
	for ( i in 1:49 ) {
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
	
	b <- Sys.time()
	print(difftime(b,a))
	for ( j in 1:length(kombination[,1]) ) {
		kombination[j,3] <- sum(apply(dataset, 1, subs, kombination[j,1:2] ));
	}
	c <- Sys.time()
	print(difftime(c,b))
	
	
#	b <- Sys.time();
#	print(difftime(b,a));
	
#	for ( i in 1:length(dataset[,1]) ) {
#		gfk <- 0; # gefundene Kombinationen
#		for ( j in 1:length(kombination[,1]) ) {
#			
#			if( any(dataset[i,] == kombination[j,1]) &
#				any(dataset[i,] == kombination[j,2]) ) {
#					
#				kombination[j,3] <- kombination[j,3] + 1;
#				gfk <- gfk + 1;
#			}
#		
#			# wenn alle Kombination gefunden wurde kann mit dem naechsten Ziehung angefangen werden
#			if( gfk == nrKombinationen ) { break; }	
#		}
#	}
#	c <- Sys.time();
#	print(difftime(c,b));

	kombination <- kombination[order(kombination[,3], na.last=TRUE, decreasing=TRUE),];
	print( kombination[1,])
	print( kombination[2,])
	print( kombination[3,])
	
	
	#
	# haeufigste Dreierkombination ( 18424 Moeglichkeiten )
	#
	
	print('Berechne haeufigsten dreier Kombinationen ...')
	
	a <- Sys.time()
	nrKombinationen <- 35; 				#Anzahl Kombination die es bei 3 aus 7 geben kann
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
				index <- index + 1;
				k <- k + 1;
			}
			j <- j + 1;
		}	
	}
	
	b <- Sys.time();
	print(difftime(b,a));
	
	for ( j in 1:length(kombination[,1]) ) {
		kombination[j,4] <- sum(apply(dataset, 1, subs, kombination[j,1:3] ));
	}
	c <- Sys.time()
	print(difftime(c,b))
	kombination <- kombination[order(kombination[,4], na.last=TRUE, decreasing=TRUE),];
	print( kombination[1,])
	print( kombination[2,])
	print( kombination[3,])
	
	a <- Sys.time()
	nrKombinationen <- 35; 				#Anzahl Kombination die es bei 3 aus 7 geben kann
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
				index <- index + 1;
				k <- k + 1;
			}
			j <- j + 1;
		}	
	}
	
	b <- Sys.time();
	print(difftime(b,a));
	ccc <- 0;
	for ( i in 1:length(dataset[,1]) ) {
		gfk <- 0 # gefundene Kombinationen
		for ( j in 1:length(kombination[,1]) ) {
			
#			if( any(dataset[i,] == kombination[j,1]) &
#				any(dataset[i,] == kombination[j,2]) &
#				any(dataset[i,] == kombination[j,3]) ) {
			if( subs( dataset[i,], kombination[j,1:3] ) == 1 ) {
				kombination[j,4] <- kombination[j,4] + 1;
				gfk <- gfk + 1;
			}
		
			# wenn alle Kombination gefunden wurde kann mit dem naechsten Ziehung angefangen werden
			if( gfk == nrKombinationen ) { 
				print( j );
				print( '-------' );
				ccc <- ccc + 1;
				break;
			}	
		}
	}
	c <- Sys.time()
	print(difftime(c,b))
	print(ccc);
	
	kombination <- kombination[order(kombination[,4], na.last=TRUE, decreasing=TRUE),];
	print( kombination[1,])
	print( kombination[2,])
	print( kombination[3,])
	
	
	#
	# haeufigste Viererkombination ( 211876 Moeglichkeiten )
	#
	
#	print('Berechne haeufigsten vierer Kombinationen ...')
#
#	a <- Sys.time()
#	nrKombinationen <- 35;
#	kombination <- matrix(0,211876,5); #Columns: x1, x2, x3, x4, Anzahl der Matches
#	index <- 1;
#	for ( i in 1:49 ) {
#		j <- i+1;
#		while ( j < 50 ) {
#			k <- j+1;
#			while( k < 50 ) {
#				l <- k+1;
#				while( l < 50) {
#					kombination[index,1] <- i;
#					kombination[index,2] <- j;
#					kombination[index,3] <- k;
#					kombination[index,4] <- l;
#					index <- index + 1;
#					l <- l + 1;
#				}
#				k <- k + 1;
#			}
#			j <- j + 1;
#		}	
#	}
#	b <- Sys.time()
#	print(difftime(b,a))
#	
#	for ( i in 1:length(dataset[,1]) ) {
#		gfk <- 0 # gefundene Kombinationen
#		for ( j in 1:length(kombination[,1]) ) {
#			
#			if( any(dataset[i,] == kombination[j,1]) &
#				any(dataset[i,] == kombination[j,2]) &
#				any(dataset[i,] == kombination[j,3]) &
#				any(dataset[i,] == kombination[j,4])) {
#					
#					kombination[j,5] <- kombination[j,5] + 1;
#					gfk <- gfk + 1;
#			}
#		
#			# wenn alle Kombination gefunden wurde kann mit dem naechsten Ziehung angefangen werden
#			if( gfk == nrKombinationen ) { break; }	
#		}
#	}
#	c <- Sys.time()
#	print(difftime(c,b))
#	
#	kombination <- kombination[order(kombination[,5], na.last=TRUE, decreasing=TRUE),];
#	print( kombination[1,])
#	print( kombination[2,])
#	print( kombination[3,])
	
	
	#
	# Berechne durchschnittlicher Abstand bis eine Zahl in der nächsten Ziehung wieder gelost wird.
	#
	
	
}
