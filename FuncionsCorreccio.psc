// **************** INICI codi per la correccio de Funcions ****************
Proceso FuncionsCorreccio
	// **************** Definicio de variables ****************
	Definir TOTAL_NOTES Como Real;				// Real per indicar la quatitat de notes de l'activitat
	Definir TOT_A_ZERO Como Entero;			// Enter per indicar el mode a inicialitzar un Vector => tot a zero
	
	Definir posicio Como Entero; 				// Enter per controlar la posicio dins de qualsevol vector
	Definir posicioNotes Como Entero; 			// Enter per controlar la posicio dins del vector vNotes
	Definir qtatFuncions Como Entero; 			// Enter per controlar la quantitat de funcions a avaluar
	Definir notaFinal Como Real; 				// Real per controlar la nota final de l'alumne
	Definir resultat Como Real;				// Real per controlar el resutlat que es rebi de cada funcio
	Definir respHaFetLaFuncio Como Caracter;	// Caracter per controlar la resposta del l'alumne si ha fet o no un exercici
	
	Definir vTextCorregir Como Cadena;			// Vector de cadenas per saber quins exercicis cal avaluar a l'alumne
	Definir vCorregir Como Entero;				// Vector de caracters (S=0/N=1) per saber quins exercicis cal avaluar a l'alumne
	Definir vNotes Como Real;					// Vector d'enters per dur el control de les notes de l'alumne
	Definir vValorsEntrats Como Entero;		// Vector d'enters per controlar els valor que rebrà el programa (11)
	Definir vCodiExercicis Como Entero;		// Vector d'enters per controlar els codis dels exercicis
	Definir matTextExercicis Como Cadena; 		// Matriu de cadenas per saber quins exercicis cal avaluar a l'alumne
	
	Definir qtatNumMitjArit Como Entero;		// Enter per controlar la quantitat de nombres per avaluar la funció obteMitArit
	Definir vNumMitjArit, pos Como Entero;		// Vector d'enters per controlar el nomnbres per avaluar la funció obteMitArit
	
	// **************** Inicialitzacio de les variables ****************
	TOTAL_NOTES <- 7;
	qtatFuncions <- 4;
	TOT_A_ZERO <- 0;
	
	Dimension vNotes[7]; 				// Dimension vNotes[TOTAL_NOTES];
	Dimension vCorregir[7]; 			// Dimension vCorregir[TOTAL_NOTES];
	Dimension vTextCorregir[7]; 		// Dimension vTextCorregir[TOTAL_NOTES];
	Dimension vCodiExercicis[7];		// Dimension vCodiExercicis[TOTAL_NOTES];
	Dimension matTextExercicis[7,2];	// Dimension matTextExercicis[TOTAL_NOTES,2];
	Dimension vValorsEntrats[11];
	Dimension vNumMitjArit[5];
	
	inicialitzaVectorEnters(vNotes,TOTAL_NOTES,TOT_A_ZERO);
	inicialitzaVectorEnters(vCorregir,TOTAL_NOTES,TOT_A_ZERO);
	inicialitzaVectorEnters(vCodiExercicis,TOTAL_NOTES,1); 
	inicialitzaVectorCadenes(vTextCorregir,qtatFuncions,0); 
	inicialitzaMatriu(matTextExercicis);
	// **************** Inici del Programa ****************
	Para posicio <- 0 hasta (qtatFuncions - 1) Hacer
		Repetir
			Escribir Sin Saltar "Has fet la funció => " , vTextCorregir[posicio] , " => (S/N) : ";
			Leer respHaFetLaFuncio;
			respHaFetLaFuncio <- Mayusculas(respHaFetLaFuncio);
			si ((respHaFetLaFuncio != 'S') & (respHaFetLaFuncio != 'N')) Entonces
				Escribir "ERROR, només pots entrar S o N!, si us plau toran a intentar-ho!";
			SiNo
				Si (respHaFetLaFuncio !='N') Entonces
					vCorregir[posicio] <- 1;
				FinSi
				
			FinSi
		Hasta Que (!((respHaFetLaFuncio != 'S') & (respHaFetLaFuncio != 'N'))) 
		//Funcion resutlat <- operacio(numero1,numero2,operador)
		//Funcion distancia <- obteDistancia (puntACoorX, puntAOrdeY, puntBCoorX, puntBOrdeY)
		//Funcion mitjanaAritmerica <- obteMitArit (vector, midaVector)
	FinPara 
	
	netejaPantalla(); // Limpiar Pantalla;
	
	notaFinal <- 0;
	posicioNotes <- 0;
	qtatNumMitjArit <- 5;
	
	// INICI comprobació de la funció operació les operacions +,-, *, /
	Si (vCorregir[0] = 1) Entonces
		pintaCapsalera("COMPROVACIO funcio operacio");
		pintaTitol("Prova 1: cap dels numeros llegits es Zero!");
		Repetir
			Escribir Sin Saltar "Entra el primer dels nombres: ";
			Leer vValorsEntrats[0];
			Escribir Sin Saltar "Entra el segon dels nombres : ";
			Leer vValorsEntrats[1];
			Si ((vValorsEntrats[0]==0) | (vValorsEntrats[1]==0)) Entonces
				Escribir "ERROR, algún del dos valors entrats es zero! Si us plau toran a intentar-ho!";
			FinSi
		Hasta Que ((vValorsEntrats[0]!=0) & (vValorsEntrats[1]!=0))
		pintaLinia('=',16,Verdadero);
		
		Para posicio <-0 Hasta 4 Con Paso 1 Hacer
			// resultat <- operacio(vValorsEntrats[0], vValorsEntrats[1], posicio);
			Segun posicio Hacer
				1: // suma
					resultat <- operacio(vValorsEntrats[0], vValorsEntrats[1], '+');
					Escribir vValorsEntrats[0], " + " , vValorsEntrats[1] , " = " , resultat;
					vNotes[posicioNotes] <- puntuar(vNotes, posicioNotes, vCodiExercicis[0], vValorsEntrats, resultat, matTextExercicis);
				2: // resta
					resultat <- operacio(vValorsEntrats[0], vValorsEntrats[1], '-');
					Escribir vValorsEntrats[0], " - " , vValorsEntrats[1] , " = " , resultat;
					vNotes[posicioNotes] <- puntuar(vNotes, posicioNotes, vCodiExercicis[1], vValorsEntrats, resultat, matTextExercicis);
				3:
					resultat <- operacio(vValorsEntrats[0], vValorsEntrats[1], '*');
					Escribir vValorsEntrats[0], " * " , vValorsEntrats[1] , " = " , resultat;
					vNotes[posicioNotes] <- puntuar(vNotes, posicioNotes, vCodiExercicis[2], vValorsEntrats, resultat, matTextExercicis);
				4:
					resultat <- operacio(vValorsEntrats[0], vValorsEntrats[1], '/'); 
					Escribir vValorsEntrats[0], " / " , vValorsEntrats[1] , " = " , resultat;
					vNotes[posicioNotes] <- puntuar(vNotes, posicioNotes, vCodiExercicis[3], vValorsEntrats, resultat, matTextExercicis);
			FinSegun
		FinPara
		salta();
	SiNo
		Escribir "L usuari no ha fet " , vTextCorregir[0];
		Para posicio <-1 Hasta 4 Con Paso 1 Hacer
			vNotes[posicio] <- 0;
		FinPara
	FinSi
	// FINAL comprobació de la funció operació les operacions +,-, *, /
	mostraResultats(vNotes,matTextExercicis,4,TOTAL_NOTES);
	pausa();
	netejaPantalla();
	
	// INICI comprobació de la funció operació amb dividir per zero
	Si (vCorregir[1] = 1) Entonces
		pintaTitol("Prova 2: dividir per zero");
		vValorsEntrats[1] <- 0;
		resultat <- operacio(vValorsEntrats[0], vValorsEntrats[1], '/');
		Escribir vValorsEntrats[0], " / " , vValorsEntrats[1] , " = " , resultat;
		vNotes[posicioNotes] <- puntuar(vNotes, posicioNotes, vCodiExercicis[4], vValorsEntrats, resultat, matTextExercicis);
		pintaLinia('=',16,Verdadero);
		salta();
	SiNo
		Escribir "L usuari no ha fet " , vTextCorregir[1];
		vNotes[4] <- 0;
	FinSi
	// FINAL comprobació de la funció operació amb dividir per zero
	
	// INICI comprobació de la funci obteDistancia
	Si (vCorregir[2] = 1) Entonces
		pintaCapsalera("COMPROVACIO funcio obteDistancia");
		Escribir Sin Saltar "Entra la coordenada del punt A (Ax) : ";
		Leer vValorsEntrats[3];
		
		Escribir Sin Saltar "Entra la ordenada del punt A   (Ay) : ";
		Leer vValorsEntrats[4];
		
		Escribir Sin Saltar "Entra la coordenada del punt B (Bx) : ";
		Leer vValorsEntrats[5];
		
		Escribir Sin Saltar "Entra la ordenada del punt B   (By) : ";
		Leer vValorsEntrats[6];
		
		pintaLinia('=',16,Verdadero);
		resultat <- obteDistancia(vValorsEntrats[3],vValorsEntrats[4],vValorsEntrats[5],vValorsEntrats[6]);
		Escribir "Segon els valors dels dos punt:";
		Escribir "* punt A=(", puntACoorX , "," ,puntAOrdeY ,") i";
		Escribir "* punt B=(", puntBCoorX , "," ,puntBOrdeY ,")";
		Escribir "La distancia entre el puntA i el puntB es de ", resultat;
		vNotes[posicioNotes] <- puntuar(vNotes, posicioNotes, vCodiExercicis[5], vValorsEntrats, resultat, matTextExercicis);
		salta();
		pintaLinia('=',16,Verdadero);
		salta();
	SiNo
		Escribir "L usuari no ha fet " , vTextCorregir[2];
		vNotes[5] <- 0;
	FinSi
	// FINAL comprobació de la funci obteDistancia
	
	// INICI comprobació de la funcio obteMitArit
	Si (vCorregir[2] = 1) Entonces
		pintaCapsalera("COMPROVACIO funcio obteMitArit");
		Para pos <- 0 Hasta qtatNumMitjArit -1 Hacer
			Repetir
				Escribir Sin Saltar "Entra el valor ", pos+1, " de " , qtatNumMitjArit, ": ";
				Leer vNumMitjArit[pos];
				vValorsEntrats[pos + 5] <- vNumMitjArit[pos];
			Hasta Que ( (vNumMitjArit[pos]>=0) & (vNumMitjArit[pos]<=10) )
		FinPara
		vValorsEntrats[10] <- qtatNumMitjArit;
		resultat <- obteMitArit(vNumMitjArit, 5);
		pintaLinia('=',16,Verdadero);
		Escribir "La mitjana aritmetica de les notes es : ", resultat;
		pintaLinia('=',16,Verdadero);
		vNotes[posicioNotes] <- puntuar(vNotes, posicioNotes, vCodiExercicis[6], vValorsEntrats, resultat, matTextExercicis);
	SiNo
		Escribir "L usuari no ha fet " , vTextCorregir[1];
		vNotes[6] <- 0;
	FinSi
	// FINAL comprobació de la funcio obteMitArit

FinAlgoritmo

// **************** INICI procediments i funcions professor per la correccio de Funcions ****************
SubProceso netejaPantalla()
	Limpiar Pantalla;
FinSubProceso

SubProceso pausa()
	Definir teclaRebuda Como Caracter;
	Repetir
		Escribir Sin Saltar "Pitja la tecla A per continuar ...  ";
		Leer teclaRebuda;
		teclaRebuda <- Mayusculas(teclaRebuda);
	Hasta Que (teclaRebuda = "A")
FinSubProceso


SubProceso mostraResultats(vNotesRebudes Por Referencia, matTextExercicisRebuda , finsLaNota, totalNotes) 
	
	
	// INICI mostra resultats
	Definir posicio, notaFinal Como Entero;
	notaFinal <- 0;
	Para posicio <- 0 Hasta (finsLaNota -1) Con Paso 1 Hacer
		Si (vNotesRebudes[posicio] = -2000) Entonces
			Escribir " -- Exercici ", matTextExercicisRebuda[posicio,1] , " no realitzat! -- ";
			vNotesRebudes[posicio] <- 0;
		FinSi
		Escribir "Nota de ", matTextExercicisRebuda[posicio,1] , " = " , vNotesRebudes[posicio];
		notaFinal <- notaFinal + vNotesRebudes[posicio];
	FinPara
	Si (finsLaNota = totalNotes) Entonces
		Escribir "Nota FINAL ", notaFinal;
	sino
		Escribir "Nota PARCIAL ", notaFinal;
	FinSi
	
	// FINAL mostra resultats
FinSubProceso

SubProceso inicialitzaMatriu(matriu Por Referencia)
	// Si codiMatriuAIni = 0, cal inicialitzar tots els elements de la matriu matTextExercicis[i,0]
	// Si codiMatriuAIni = 1, cal inicialitzar tots els elements de la matriu matTextExercicis[i,1]
	matriu[0,0] <- "Operació part Suma ................";
	matriu[1,0] <- "Operació part Resta................";
	matriu[2,0] <- "Operació part Producte.............";
	matriu[3,0] <- "Operació part Divisió sense zero!..";
	matriu[4,0] <- "Operació part Divisió nun2 = zero!.";
	matriu[5,0] <- "Distancia..........................";
	matriu[6,0] <- "Mitjana aritmetica.................";
	matriu[0,1] <- "suma";
	matriu[1,1] <- "resta";
	matriu[2,1] <- "producte";
	matriu[3,1] <- "divisio";
	matriu[4,1] <- "divisioZero";
	matriu[5,1] <- "distancia";
	matriu[6,1] <- "mitjArit";
	
FinSubProceso

SubProceso inicialitzaVectorCadenes(vector Por Referencia, midaVector, codiVectorAIni)
	Si ((codiVectorAIni = 0) & (midaVector=4)) Entonces  // vTextCorregir
		vector[0] <- "Funcion resutlat <- operacio(numero1,numero2,operador)";
		vector[1] <- "a la Funcion resutlat <- operacio(numero1,numero2,operador) has controlat que divideixi per zero";
		vector[2] <- "Funcion distancia <- obteDistancia (puntACoorX, puntAOrdeY, puntBCoorX, puntBOrdeY)";
		vector[3] <- "Funcion mitjanaAritmerica <- obteMitArit (vector, midaVector)";
	FinSi
FinSubProceso

SubProceso inicialitzaVectorEnters(vector Por Referencia, midaVector, codiVectorAIni)
	Definir posicio Como Real;
	// Si codiVectorAIni = 0, cal inicialitzar tots els elements del vector amb el valor 0 -> vNotes, vCorregir
	// Si codiVectorAIni = 1, cal inicialitzar tots els elements  del vector vCodiExercicis amb el valor (pos - 1) -> vCodiExercicis
	// Si codiVectorAIni = 2, cal inicialitzar tots els elements  del vector vTextCorregir;
	Si ((codiVectorAIni = 0) | (codiVectorAIni = 1)) Entonces
		Para posicio <- 0 hasta (midaVector - 1) Hacer
			Si (codiVectorAIni = 0) Entonces // -> vNotes, vCorregir
				vector[posicio] <-.0;
			SiNo
				Si (codiVectorAIni = 1) Entonces // vCodiExercicis
					vector[0] <- 1;
					vector[1] <- 2;
					vector[2] <- 3;
					vector[3] <- 4;
					vector[4] <- 5;
					vector[5] <- 6;
					vector[6] <- 7;
				FinSi
			FinSi
		FinPara
	FinSi
FinSubProceso

SubProceso pintaTitol(titol)
	Definir mida Como Entero;
	mida <- Longitud(titol);
	Escribir titol;
	pintaLinia('-',mida,Verdadero);
FinSubProceso 

SubProceso pintaCapsalera(textCapsalera)
	Definir mida Como Entero;
	mida <- Longitud(textCapsalera);
	pintaLinia('-',mida + 6,Verdadero);
	Escribir '|  ', textCapsalera, '  |';
	Escribir Sin Saltar '|  ';
	pintaLinia('=',mida ,Falso);
	Escribir '  |';
	pintaLinia('-',mida + 6,Verdadero);
FinSubProceso 

SubProceso pintaLinia(caracterAPintar, mida, ambSalt)
	Repetir
		Escribir Sin Saltar caracterAPintar;
		mida <- mida - 1;
	Hasta Que mida = 0;
	si (ambSalt) Entonces
		salta();
	FinSi
FinSubProceso

SubProceso salta()
	Escribir "";
FinSubProceso

Funcion nota <- puntuar(notes Por Referencia, posicioNotes Por Referencia, codiExercici, vValorsEntrats, resultat, matTextExercicisRebuda)
	Definir nota como Real;
	Definir operador Como Caracter;
	operador <- "X";
	nota <- 0;
	// matTextExercicis[i,1]
	Si (resultat = -2000) Entonces
		nota <- -2000;
	SiNo
		Segun codiExercici Hacer
			1: 	// "suma" : // suma = 1
				operador <- "+";
				Si (resultat = (vValorsEntrats[0] + vValorsEntrats[1])) Entonces
					nota <- 1;
				FinSi
			2: //"resta" : // resta = 2
				operador <- "-";
				Si (resultat = (vValorsEntrats[0] - vValorsEntrats[1])) Entonces
					nota <- 1;
				FinSi
			3: //"producte": // producte = 3
				operador <- "*";
				Si (resultat = (vValorsEntrats[0] * vValorsEntrats[1])) Entonces
					nota <- 1;
				FinSi
			4: //"divisio": // divisio "normal" = 4
				operador <- "/";
				Si (resultat = (vValorsEntrats[0] / vValorsEntrats[1])) Entonces
					nota <- 1;
				FinSi
			5: //"divisioZero": // divisio entre zero = 5
				operador <- "/";
				Si (vValorsEntrats[1]=0) Entonces
					Escribir "Cal escriure un missatge del tipus -> No es pot divir per zero! (codi -2001)";
					nota <- 2;
				FinSi
			6: //"distancia": // distancia 3->ax 4->ay 5->bx 6->by // RAIZ((puntAOrdeY-puntBOrdeY)^2 + (puntACoorX-puntBCoorX)^2)
				resultat <- RAIZ((vValorsEntrats[5]-vValorsEntrats[3])^2 + (vValorsEntrats[6]-vValorsEntrats[4])^2);
				Si (resultat = 5) Entonces
					nota <- 2;
				FinSi	
			7: //"mitjArit": // mitjana aritmetica
				Definir pos Como Entero;
				Definir mitArit Como Real;
				mitArit <- 0;
				para pos <- 5 Hasta 9 Con Paso 1 Hacer
					mitArit <- mitArit + vValorsEntrats[pos];
				FinPara
				mitArit <- mitArit / vValorsEntrats[10];
				Si (resultat = mitArit) Entonces
					nota <- 2;
				FinSi		
		FinSegun
	FinSi
	Escribir Sin Saltar "La funció ", matTextExercicisRebuda[codiExercici-1,1] , " sera correcte, si ";
	Escribir vValorsEntrats[0], " ",  operador, " ", vValorsEntrats[1], " es igual a ", resultat, ".";
	Escribir "La nota de ", matTextExercicisRebuda[codiExercici-1,1] , " es: ", nota;
	Escribir "";
	
	notes[posicioNotes] <- nota;
	posicioNotes <- posicioNotes + 1;
FinFuncion

// **************** FINAL procediments i funcions professor per la correccio de Funcions ****************

//// Funcio operacio per si l alumne no te la funcio.
//Funcion resutlat <- operacio(numero1,numero2,operador)
//	Definir resutlat Como Entero;
//	resultat <- -2000;
//FinFuncion
//

//// Funcio obteDistancia per si l alumne no te la funcio.
//Funcion distancia <- obteDistancia (puntACoorX, puntAOrdeY, puntBCoorX, puntBOrdeY)
//	Definir distancia Como Real;
//	distancia <- -2000;
//FinFuncion

//// Funcio mitjanaAritmerica per si l alumne no te la funcio.
//Funcion mitjanaAritmerica <- obteMitArit (vector, midaVector)
//	Definir mitjanaAritmerica Como Real;
//	mitjanaAritmerica <- -2000;
//FinFuncion

// **************** FINAL codi per la correccio de Funcions ****************


// **************** INICI codi de l'alumne per l'activitat de Funcions ****************

/// Funció operacio(numRebut1, numRebut2, operador)
/// Parametres d'entrada
///    numRebut1 (enter)    		->  primer nombre per fer una operació
///    numRebut2 (enter)    		->  segon nombre per fer una operació
///    operador (enter)    	->  codi que indica quina opearció cal fer.
///    	Si = 1 -> suma      -> numRebut1 + numRebut2
///    	Si = 2 -> resta     -> numRebut1 - numRebut2
///    	Si = 3 -> producte  -> numRebut1 * numRebut2
///    	Si = 4 -> divisió		
///    					Si numRebut2 != 0 -> numRebut1 / numRebut2
///    					Si numRebut2 = 0  -> -1 (més mostrar cadena indicant que no es pot fer l'operació)
/// Parametres de sortida
///    resultatOperacio (real)  -> resultat de la opearció feta amb els nombres rebuts
Funcion resultatOperacio <- operacio(numRebut1, numRebut2, operadorRebut)
	Definir resultatOperacio Como Real;
	Definir operador Como Real;
	Si (operadorRebut = '+') Entonces
		operador <- 1;
	SiNo
		Si (operadorRebut = '-') Entonces
			operador <- 2;
		SiNo
			Si (operadorRebut = '*') Entonces
				operador <- 3;
			SiNo
				operador <- 4;
			FinSi
		FinSi
	FinSi
	resultatOperacio <- 0;
	
	Segun operador Hacer
		1:
			resultatOperacio <- numRebut1 + numRebut2;
		2:
			resultatOperacio <- numRebut1 - numRebut2;
		3:
			resultatOperacio <- numRebut1 * numRebut2;
		4:
			si (numRebut2=0) Entonces
				Escribir "No es pot dividir per zero! (Codi = 2001)";
				resultatOperacio <- -2001;
			SiNo
				resultatOperacio <- numRebut1 / numRebut2;
			FinSi
	FinSegun
FinFuncion

Funcion distancia <- obteDistancia(puntACoorX, puntAOrdeY, puntBCoorX, puntBOrdeY)
	Definir distancia Como Real;
	distancia <- RAIZ((puntAOrdeY-puntBOrdeY)^2 + (puntACoorX-puntBCoorX)^2);
FinFuncion

Funcion mitjanaAritmerica <- obteMitArit(vector, midaVector)
	Definir mitjanaAritmerica Como Real;
	mitjanaAritmerica <- 0;
	Definir posicio Como Entero;
	para posicio <- 0 Hasta (midaVector - 1) Con Paso 1 Hacer
		mitjanaAritmerica <- mitjanaAritmerica + vector[posicio];
	FinPara
	mitjanaAritmerica <- (mitjanaAritmerica / midaVector);
FinFuncion

// **************** FINAL codi de l'alumne per l'activitat de Funcions ****************