#
#
# @author: Christian Lutz
#
# Description: This is nonsense and just for me playing around with R
#              If you ever think it is possible to calc the next lotto numbers take a statistic course :)
#



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
# data = each row of the matrix is already sorted
# baseNumbers = vector with all Numbers within the matrix
# comboNo = Länge der zu suchenden Kombinationen
#
# Diese Funktion berechnet alle wie häufig eine Kombination mit einer bestimmten Anzahl
# an Elementen in der Matrix vorkommen. Dabei wird Zeile für Zeile jede Kombination
# geprüft.
#
calcNoOfCombinations <- function( data, baseNumbers, combNo ) {

	hashIdx <- nokIdxMatrix( baseNumbers, combNo);
	mappedV <- t(apply(hashIdx, 1, function(x,y) y[x], baseNumbers)); # alle möglichen comboNo Kombinationen
	#prepare hash()
	#Option1: hash <- list();
	#         length(hash) <- nrow(mappedV);
	#Option2: hash <- vector(mode="list", nrow(mappedV) );
	#Option3: hash <- new.env(hash=TRUE) - very fast hash
	hash <- new.env(hash=TRUE)
	for( i in 1:nrow(mappedV) ) {
		hash[[paste(mappedV[i,], collapse = '.')]] <- 0; # Wert initialisieren
	}
	
	# Index matrix für jede Ziehung, damit sind die Indizes für alle 35 Kombinationen abgelegt.
	indexMX <- nokIdxMatrix( c(1:ncol(data)),combNo); #Columns: x1, x2, x3, x4, Anzahl der Matches
	
	# suche und zähle jede Kombination
	for( i in 1:nrow(data) ) {
		for ( j in 1:nrow(indexMX) ) {
			fi <- paste(data[i,indexMX[j,]], collapse = '.')
			hash[[fi]] <- hash[[fi]] + 1;
		}
	}
	
	mappedV <- cbind( mappedV, matrix(0,nrow(mappedV),1) ); #Zusätzliche Spalte für die Anzahl der Matches
	for( i in 1:nrow(mappedV) ) {
		mappedV[i,(combNo+1)] <- hash[[paste(mappedV[i,1:combNo], collapse = '.')]];
	}
	
	mappedV <- mappedV[order(mappedV[,(combNo+1)], na.last=TRUE, decreasing=TRUE),];
	return( mappedV );
}



#
# data = each row of the matrix is already sorted
# baseNumbers = vector with all Numbers within the matrix
#
# Calculate the average number of 'Ziehungen' until the same number appears
#
avgDistanceForEachNumber <- function(data, lottoNrs) {

	distances <- c();

	for( i in lottoNrs ) {
		foundNoX <- apply(data, 1, function(x) any(x==i));
		lastFoundIdx <- c();
		for( j in 1:length(foundNoX)) {
			if( foundNoX[j] ) {
				if( is.null(lastFoundIdx) ) {
					lastFoundIdx <- c(j);
				}
				else {
					distances <- c(distances, j - lastFoundIdx[1]);
					lastFoundIdx <- c(j);
				}
			}
		}
	}
	
	
	# Verteilung, wie haeufig kam eine bestimmte Distanz vor.
	#print('Berechne Distanz Verteilung ...')
	#distribution <- table(distances);
	#print(distribution);
	#barplot(distribution)
	
	# Durchschnitt
	print('Durchschnittliche Distanz ...')
	avg <- sum(distances) / length(distances);
	print(avg);
	
	# Anzahl Distanzen
	# print("Anzahl Distanzen ...")
	# print(length(distances));
	
	return(avg);
}


#
# Berechne die Häufigkeit und die Verteilung von 10 Kombinationen größer drei
# Bsp.: 13,17,18 oder 42,45,49
#
#
findDecimalCombination <- function(data) {
	
}


#
#
# prüft ob ein vector bereits als Zeile in einem bestehenden Datensatz vorkommt.
# stimmen die Anzahl der Spalten und die Länge des Vectors nicht überein wird
# immer FALSE zurück geliefert.
#
# return TRUE oder FALSE
isDuplicate <- function(orgData, newNumbers) {

	sortedNewNumbers <- sort(newNumbers)
	if( ncol(orgData) == length(newNumbers) ) {
		
		tmpData <- unique(orgData);
		tmpData <- rbind(tmpData,sortedNewNumbers);		
		return(anyDuplicated(tmpData) > 0);
	}
	
	return(FALSE);
}

#
# wann wurden welche Lotte Zahlen zum letzten Mal gespielt
# Für jede Lottozahl gibt es eine Nummer welche an gibt, wie viele Ziehungen es
# her ist das diese Nummer gezogen wurde.
#
lastTimeNumbersPlayed <- function(orgData) {

	lastPlayed <- matrix(0,1,49);
	idx <- 0;
	for (rowNo in nrow(orgData):1) {
		idx <- idx + 1
		for (colmnNo in 1:6) {
			currentNumber <- orgData[rowNo, colmnNo]
			if (lastPlayed[1,currentNumber] == 0) {
				lastPlayed[1,currentNumber] <- idx;
			} 
		}
	}
	
	return(lastPlayed)
}

#
# berechne eine neue Wahrscheinlichkeitsverteilung basierende auf den letzten Ziehungen
# diese Verteilung ist humbug und soll nur zum Spass ausprobiert werden.
# Die zu erwartenden Gewinne dürfen nicht höher sein als die durch gleichmäßige Zufallszahlen.
#
calculateProbabilityDistribution <- function(orgData) {

	lowestPossibleProbability <- 30;
	avgDistance <- avgDistanceForEachNumber(orgData, c(1:49));
	
	probability <- matrix(49,1,49);
	numbersPlayed <- lastTimeNumbersPlayed(orgData);
	numbersPlayed <- apply(numbersPlayed, c(1,2), function(x) if(x < avgDistance) 49 else 49 - (x / 3));
	
	return(apply(numbersPlayed, c(1,2), function(x) 1/x));
}

isNumberAlreadyUsed <- function(currentNumbers, newNumber) {
	if (nrow(currentNumbers)==0) {
		return(FALSE);
	}
	if (is.na(any(match(newNumber, currentNumbers)))) {
		return(FALSE);
	}
	return(TRUE);
}

findNextNumbers <- function(orgData, amountOfNewNumber, avgNumberDistanz) {

	newNumbers <- matrix(,0,6);
	newProbability <- calculateProbabilityDistribution(orgData);
	hist(sample(1:49,1000000,replace=T, prob=newProbability))
	
	while (nrow(newNumbers) != amountOfNewNumber) {
		randomNumbers <- sample(1:49,6,replace=F, prob=newProbability);
		if (! isDuplicate(orgData, randomNumbers)) {
			if (! isNumberAlreadyUsed(newNumbers, randomNumbers)) {
				newNumbers <- rbind(newNumbers, randomNumbers);
			}
		} 
	}
	
	newNumbers <- t(apply(newNumbers,1,sort));
	return(newNumbers);
}


zweidreivier <- function() {
	#
	# haeufigste Zweierkombination ( 1128 Moeglichkeiten )
	#
	print('-------------------------------------------------------------------');
	print('Berechne haeufigsten zweier Kombinationen ...')
	
	a <- Sys.time()
	zweierKombination <- calcNoOfCombinations( data, lottoNrs, 2 )
	b <- Sys.time();
	print(difftime(b,a));
	
	print( zweierKombination[1,])
	print( zweierKombination[2,])
	print( zweierKombination[3,])
	
	
	#
	# haeufigste Dreierkombination ( 18424 Moeglichkeiten )
	#
	print('-------------------------------------------------------------------');
	print('Berechne haeufigsten dreier Kombinationen ...')
	
	a <- Sys.time()
	dreierKombination <- calcNoOfCombinations( data, lottoNrs, 3 )
	b <- Sys.time();
	print(difftime(b,a));
	
	print( dreierKombination[1,])
	print( dreierKombination[2,])
	print( dreierKombination[3,])
	
	
	
	#
	# haeufigste Viererkombination ( 211876 Moeglichkeiten )
	#
	print('-------------------------------------------------------------------');
	print('Berechne haeufigsten vierer Kombinationen ...')

	a <- Sys.time()
	viererKombination <- calcNoOfCombinations( data, lottoNrs, 4 )
	b <- Sys.time();
	print(difftime(b,a));
	
	print( viererKombination[1,])
	print( viererKombination[2,])
	print( viererKombination[3,])
	
	
	#
	# haeufigste Fünferkombination ( xx Moeglichkeiten )
	#
	print('-------------------------------------------------------------------');
	print('Berechne haeufigsten fünfer Kombinationen ...')

	a <- Sys.time()
	fuenferKombination <- calcNoOfCombinations( data, lottoNrs, 5 )
	b <- Sys.time();
	print(difftime(b,a));
	
	print( fuenferKombination[1,])
	print( fuenferKombination[2,])
	print( fuenferKombination[3,])
}



lotto <- function() {
	
	pfadUndDateiName <- "/Users/christian/Documents/Workspace/Scripte/LottoS/lottozahlen.csv"
	importedData <- read.csv(pfadUndDateiName, TRUE, sep=";", quote="");	
	zahlen <- c(1:49);
	
	#
	#### PREPROCESSING ########
	
	# entferne Datum, Zusatzzahl und Superzahl
	dataset <- subset(importedData, select=c(2:7));
	# zu einer Matrix konvertieren
	dataset <- data.matrix(dataset); 
	# die ersten sechs zahlen sortieren 
	data <- t(apply(dataset,1,sort));
	# dataset <- cbind( data, dataset[,7] ); # füge Zusatzzahl dazu, gibt es aktuell nicht mehr, es gibt nur noch eine Zusatzzahl.
	# sortiere Reihenfolge einer jeder Zeile, wichtig für das hashen bei den Kombinationen
	data <- t(apply(data,1,sort));
	# lottoNr
	lottoNrs <- c(1:49);
	
	
	#
	#### AUSWERTUNG BEGINNEN ########
	
	#
	# Infos:
	#
	
	print('Anzahl Ziehungen: ')
	print(nrow(data))
	print('')
	
	#
	# berechne Verteilung der einzelner Kugeln
	#
	print('Berechne Verteilung ...')
	verteilung <- vector('integer',49);
	for ( i in lottoNrs ) {
		verteilung[i] <- length( which( data==i ));
	}
	print(verteilung);
	barplot(verteilung)
	
	
	#	
	# berechne durchschnittlichen Distanz zwischen erster und letzer Zahl
	#
	
	print('Berechne Distanz ...')
	
	dif <- vector('integer', nrow(data) );
	for ( i in 1:nrow(data) ) {
		dif[i] <- max(data[i,]) - min(data[i,]);
	}
	
	print(max(dif));				# maximale Distanz
	print(min(dif));				# minimale Distanz
	print(sum(dif) / length(dif)); 	# mean Distanz
	#boxplot(dif)					# Box Plot Grafik mit Median etc.
	
	
	
	#
	# Kombinationen die bereits mehrmals gezogen wurden
	#
	print('-------------------------------------------------------------------');
	print('Berechne doppelte Kombinationen ...');
	print('- ohne Zusatzzahl und Superzahl');
	print( data[duplicated(data),] );
	
	
	#
	# Haeufigste Superzahl
	#
	#print('Berechne Zusatzzahl Verteilung ...');
	#numSZ <- table( dataset[,7] ); # beinhaltet für jede Zahl wie oft diese als Superzahl gezogen wurde.
									# dabei entspricht der Index der entsprechenden Kugel
	#print.table( numSZ );
	
	
	#
	# Berechne durchschnittlicher Abstand bis eine Zahl in der nächsten Ziehung wieder gelost wird.
	#
	#print('-------------------------------------------------------------------');
	#print('Berechne abstand zwischen den Ziehungen ...')
	#avgNumberDistance <- avgDistanceForEachNumber(data, lottoNrs);
	
	
	#
	# Berechne die Häufigkeit und die Verteilung von 10 Kombinationen größer drei
	# Bsp.: 13,17,18 oder 42,45,49
	#
	# TODO
	
	
	# 
	# Wann wurden die nummer das letzte Mal gezogen
	print('-------------------------------------------------------------------');
	print('Wann wurden die Zahlen zu letzt gezogen ...')
	print(lastTimeNumbersPlayed(data));
	
	
	#
	# Berechne die nächsten Lottozahlen :))))
	#
	# Beachte:
	#   - Durchschnittliche Distanz
	#	- Dauer bis eine Zahl wieder gezogen wurde
	#   - Roland meine es gibt immer eine Zehner Kombi 21,23,29 oder 13,15,16
	# 	- Zahl darf noch nie gezogen worden sein.
	#	- Die letzten Ziehungen berücksichtigen.
	#	- Als Parameter die nächsten X-Zahlen die alle Kriterien entsprechen.
	print('-------------------------------------------------------------------');
	print('Berechne neue zufällige Lottozahlen ...')
	newNumbers <- findNextNumbers(data, 6, 0.0)
	print(newNumbers)
}
