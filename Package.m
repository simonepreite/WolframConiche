(* ::Package:: *)

(* ::Code::Initialization::Plain:: *)
(* PACKAGE.M
 * Progetto d'esame di Matematica Computazionale + Calcolo Numerico e Software Didattico
 * Corsi di laurea magistrale in Informatica e Matematica
 * Anno accademico 2016/2017
 * 
 * Autori:
 *   Federico Giubaldo, Cristina Stamegna, Domenica Stilo, Mattia Venturini
 *
 * Versione di sviluppo e testing: Wolfram Mathematica 11.1
 *)

BeginPackage[ "Progetto`"];

Unprotect["Progetto`*"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["Progetto`*"];

(* Usage prima del Private *)

(* Fase 1 *)
drawSystem::usage = "disegna un sistema di 2 equazioni (non gestisce casi particolari)";

ShowLine::usage = "ShowLine[] Mostra una retta, con la possibilit\[AGrave] di modificare i parametri della forma implicita";
ShowCircle::usage = "ShowCircle[] Mostra una circonferenza, con la possibilit\[AGrave] di spostare un punto su di essa";
ShowParabola::usage = "ShowParabola[] Mostra una parabola, con la possibilit\[AGrave] di spostare un punto su di essa";
ShowEllipse::usage = "ShowEllipse[] Mostra un'ellisse, con la possibilit\[AGrave] di spostare un punto su di essa";
ShowHyperbole::usage = "ShowHyperbole[] Mostra un'iperbole, con la possibilit\[AGrave] di spostare un punto su di essa";

HyperboleTheory::usage = "Mostra un grafico con un'iperbole e le sue componenti";
ParabolaTheory::usage = "Mostra un grafico con un'ellisse e le sue componenti";

(* Fase 2 *)
checkCondition::usage = "verifica le condizioni di esistenza per una figura";
systemManipulate::usage = "Permette di creare e risolvere sistemi in due equazioni";

(* Fase 3 *)
randomFigures::usage = "genera figure a caso tra le 5 tipologie di coniche";
mainPhase3::usage = "Gioco interattivo di tre tipologie";

(* servono ad evitare problemi nella print delle equazioni *)
x::usage = "";
y::usage = "";

Begin["`Private`"]; (* Comincia spazio privato *)

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

(* 
 * disegna un sistema di 2 equazioni (non gestisce casi particolari)
 * @param eq1, eq2: equazioni come espressioni booleane, con variabili libere x e y
 *)
drawSystem[eq1_,eq2_]:=
	Module[{sol,intersections},
		sol =Solve[Rationalize[eq1&&eq2],{x,y},Reals]; (* trova le intersezioni delle 2 equazioni *)
		intersections={Red,PointSize[Large],Point[{x,y}/.sol]}; (* crea i punti che rappresentano le intersezioni *)
		Show[ (* disegno il grafico *)
			Plot[y/.Solve[eq1],{x,-20,20},PlotRange->{{-10,10},{-10,10}},ImageSize->Large,AspectRatio->1,PlotStyle->{Purple,Thick}], (* disegno eq1 *)
			Plot[y/.Solve[eq2],{x,-20,20},PlotRange->{{-10,10},{-10,10}},ImageSize->Large, AspectRatio->1,PlotStyle->{Blue,Thick}], (* disegno eq2 *)
			Graphics[{intersections}]  (* disegno le intersezioni *)
		]
	];

(* Mostra una retta, con la possibilit\[AGrave] di modificare i parametri della forma implicita *)
ShowLine[]:=
  DynamicModule[{},
	Manipulate[ (* permette all'utente di modificare i parametri *)
		Row[{
			StringForm["Equazione : ``x ``y ``", NumberForm[a,3], NumberForm[b,3,NumberSigns->{"-","+"}], NumberForm[c,2,NumberSigns->{"-","+"}]], (* mostro l'equazione *)
			(*controllo quali coefficienti dell'eq. della retta sono uguali a 0 per calcolare il coeff. angolare*)
			If[b==0, 
				(* THEN 1 *)
				If[a==0, 
					(* THEN 2 *)
					StringForm["m = indeterminato"], (* Se A\[Equal]0 e B\[Equal]0 allora il coeff. angolare \[EGrave] indeterminato*) 
					(* ELSE 2 *)
					StringForm["m = indefinito"] (* Se B\[Equal]0 e A\[NotEqual]0 allora il coeff. angolare \[EGrave] uguale a 0*) 
				],
				(* ELSE 1 *)
				StringForm["m = ``", NumberForm[-(a/b),3,NumberSigns->{"-","+"}]] (*Se B\[NotEqual]0 allora il coeff. angolare \[EGrave] dato da -(a/b). Il valori viene mostrato con 3 cifre significative*)
			],
			(*controllo quali coefficienti dell'eq. della retta sono uguali a 0 per disegnare la retta*)
			If[b==0, 
				(* THEN 1 *)
				If[a==0, 
					(* THEN 2 *)
					Graphics[Axes -> True, ImageSize -> Large], (* Se A\[Equal]0 e B\[Equal]0 allora mostro solo gli assi*) 
					(* ELSE 2 *)
					Plot[,{x,-20,20},PlotRange->{{-20,20},{-20,20}},AspectRatio->1, GridLines->{{-(c/a)},{}},GridLinesStyle->Directive[Red,Thick], ImageSize->Large](* Se B\[Equal]0 e A\[NotEqual]0 allora mostra la retta parallela all'asse y*) 
				],
				(* ELSE 1 *)
				Plot[-(a/b) x - (c/b),{x,-20,20},PlotRange -> {{-20, 20}, {-20, 20}},ImageSize->Large, PlotStyle->{Red,Thick}] (*Se B\[NotEqual]0 allora plotto la retta attraverso la Plot *)
			],
		}, "\n"],
		(* parametri modificabili: i 3 coefficenti della retta *)
		{{a,1,"a"}, -20, 20},
		{{b,1,"b"}, -20, 20},
		{{c,1,"c"}, -20, 20}
	]
  ];

(* Mostra una circonferenza, con la possibilit\[AGrave] di spostare un punto su di essa *)
ShowCircle[]:=
  DynamicModule[{a = 0, b = 0, r = 1, xp, yp, y},
	Manipulate[
		(* coordinate del punto P sulla circonferenza *)
		xp=Cos[ang];
		yp =Sin[ang];
		Row[{
			StringForm["P(``,``)",NumberForm[xp,3],NumberForm[yp,3]],  (* mostro coordinate di P *)
			StringForm["distanza dal centro: ``", NumberForm[Sqrt[xp^2+yp^2],3]], (* mostro distanza tra P e il centro della circonferenza (non cambia) *)
			Show[
				Plot[y/.Solve[(x-a)^2+(y-b)^2==r^2],{x,-2,2},AspectRatio->Automatic, ImageSize->Large, PlotStyle->{RGBColor[0,0.6,0],Thick}], (* Cerchio come soluzione di x^2+y^2 = 1 *)
				Graphics[{PointSize[0.025],Point[{xp,yp}], (* Punto P a partire dall'angolo a *)
					Line[{{0,0},{xp,yp}}],    (* Segmento che rappresenta il raggio *)
					Dashing[0.01], (* specifica che le prossime linee sono tratteggiate *)
					Line[{{0,yp},{xp,yp}}], (* linea da P all'asse y *)
					Line[{{xp,0},{xp,yp}}], (* linea da P all'asse x *)
					Text["P",{xp+0.1,yp+0.1}]
				}] (* end Graphics *)
			]
		}, "\n"],  (* separatore tra ogni elemento della Row *)(* end ROW *)
		{{ang,1.,"Muovi P"},0,2Pi, 0.05}, (* variabile manipolabile per spostare P *)
		ControlPlacement->Left (* comandi a sinistra, grafico a destra *)
	]
  ];

(* Mostra una parabola, con la possibilit\[AGrave] di spostare un punto su di essa *)
ShowParabola[]:=
  DynamicModule[{a = 1, b = 0, c = 0, delta, F},
    delta = b^2-4 a c;
	F = {0,(1-delta)/4a};  (* Fuoco *)
	Manipulate[
		Row[{
			StringForm["P(``,``)",NumberForm[x0,3],NumberForm[x0^2,3]], (* mostro coordinate del punto *)
			StringForm["Distanza dal fuoco: ``",NumberForm[Sqrt[(x0^2-F[[2]])^2 + (x0-F[[1]])^2],3]], (* mostro distanza tra P e F *)
			StringForm["Distanza dalla direttrice: ``",NumberForm[x0^2+(1+delta)/4a,3]], (* mostro distanza tra P e la direttrice *)
			(* le 2 distanze saranno uguali *)
			Show[ (* mostro nel grafico *)
				Plot[x^2,{x,-2,2},AspectRatio->1, ImageSize->Large, PlotRange->{-1,2},PlotStyle->{RGBColor[0,0,0.6],Thick}], (* Parabola come equazione di secondo grado *)
				Plot[-(1+delta)/4a,{x,-2,2},AspectRatio->1, ImageSize->Large, PlotRange->{-1,2},PlotStyle->{Black}], (* disegna la direttrice *)
				Graphics[{
					PointSize[0.025],
					Point[{x0,x0^2}], (* Punto P a partire dall'angolo a *)
					Point[F], (* disegno il fuoco *)
					Dashing[0.01],
					Line[{F,{x0,x0^2}}], (* linea tra P e F *)
					Line[{{x0,-(1+delta)/4a},{x0,x0^2}}], (* linea da P alla direttrice *)
					Text["P",{x0+0.1,x0^2+0.1}],
					Text["F",{0.1,(1-delta)/4a+0.1}],
					Text["direttrice",{1,-(1+delta)/4a-0.1}]
				}] (* end Graphics *)
			]
		}, "\n"], (* separatore tra ogni elemento della Row *)
		{{x0,0.,"Muovi P"}, -1.4, 1.4,  0.1}, (* coordinata x di P \[EGrave] manipolabile *)
		ControlPlacement->Left (* comandi a sinistra, grafico a destra *)
	]
  ];

(* Mostra un'ellisse, con la possibilit\[AGrave] di spostare un punto su di essa *)
ShowEllipse[]:=
  DynamicModule[{a, b, c, F1, F2}, (* definisco variabili locali alla funzione *)
    a = 1.6; b = 1; (* parametri dell'equazione dell'ellisse *)
    c = Sqrt[a^2-b^2];
	F1={-c,0};  (* Fuochi *)
	F2= {c,0};
	Manipulate[
		(* coordinate di P *)
		xp = a*Cos[ang];
		yp = b*Sin[ang];
		
		(* distanze di P dai due fuochi *)
		distF1=Sqrt[(xp+c)^2+(yp)^2];
		distF2=Sqrt[(xp-c)^2+(yp)^2];
		Row[{
			StringForm["P(``,``)",NumberForm[xp,3],NumberForm[yp,3]], (* mostro le coordinate del punto P *)
			StringForm["Distanza di P da F1: ``",NumberForm[distF1,3]], (* mostro le distanze di P dai fuochi *)
			StringForm["Distanza di P da F2: ``",NumberForm[distF2,3]],
			StringForm["Somma delle distanze: ``",NumberForm[distF1+distF2,3]], (* mostro la somma delle distanze (resta costante) *)
			Show[ (* grafico *)
				Plot[y  /.Solve[(x/a)^2+(y/b)^2==1], {x,-2,2},AspectRatio->Automatic, ImageSize->Large, PlotStyle->{Orange,Thick}], (* Parabola come equazione di secondo grado *)
				Graphics[{PointSize[0.025],Point[{xp,yp}], (* Punto P a partire dall'angolo ang *)
					Point[F1], (* Mostro i fuochi *)
					Point[F2],
					Dashing[0.01], (* linee tratteggiate *)
					Line[{F1,{xp,yp}}], (* segmenti da P ai fuochi *)
					Line[{F2,{xp,yp}}],
					Text["P",{xp+0.1,yp+0.1}], (* etichette per P, F1 e F2 *)
					Text["F1",{-c,0.1}],
					Text["F2",{c,0.1}]
				}] (* end Graphics *)
			]
		}, "\n"], (* separatore tra ogni elemento della Row *)
		{{ang,1.,"Muovi P"},0,2Pi, 0.1}, (* parametro manipolabile: angolo che indica il punto P *)
		ControlPlacement->Left (* comandi a sinistra, grafico a destra *)
	]
  ];
  
  
(* Mostra un'iperbole, con la possibilit\[AGrave] di spostare un punto su di essa *)
ShowHyperbole[]:=
  DynamicModule[{a = 1, b = 0.8, c, F1, F2, xp, yp, distF1, distF2, y},
    c = Sqrt[a^2+b^2];
    F1={c,0};  (* Fuochi *)
	F2= {-c,0};
	Manipulate[
		(* coordinate di P *)
		xp=a*Cosh[ang];
		yp=b*Sinh[ang];
		
		(* distanze di P dai due fuochi *)
		distF1=Sqrt[(xp+c)^2+(yp)^2];
		distF2=Sqrt[(xp-c)^2+(yp)^2];
		Row[{
			StringForm["P(``,``)",NumberForm[xp,3],NumberForm[yp,3]],
			StringForm["Distanza di P da F1: ``",NumberForm[distF1,3]],
			StringForm["Distanza di P da F2: ``",NumberForm[distF2,3]],
			StringForm["Differenza delle distanze: ``",NumberForm[Abs[distF1-distF2],3]],
			Show[
				Plot[y  /.Solve[(x/a)^2-(y/b)^2==1], {x,-4,4},AspectRatio->1, ImageSize->Large,PlotStyle->{RGBColor[1,0,1],Thick}], (* Iperbole come equazione di secondo grado *)
				Graphics[{PointSize[0.025],Point[{xp,yp}], (* Punto P a partire dall'angolo a *)
					Point[F1],
					Point[F2],
					Dashing[0.01],
					Line[{F1,{xp,yp}}],
					Line[{F2,{xp,yp}}],
					Text["P",{xp+0.2,yp+0.1}],
					Text["F1",{-c,0.2}],
					Text["F2",{c,0.2}]
				}] (* end Graphics *)
			]
		}, "\n"], (* separatore tra ogni elemento della Row *)
		{{ang,1.1,"Muovi P"},-2,2, 0.1}, (* parametro manipolabile: angolo che indica il punto P *)
		ControlPlacement->Left (* comandi a sinistra, grafico a destra *)
	]
];


(* Mostra un grafico con un'iperbole e le sue componenti*)
HyperboleTheory[]:=
	Module[{a = 1, b = 0.8, c, F1, F2, y},
		c = Sqrt[a^2+b^2]; (* terzo parametro ricavato dai primi *)
		F1={c,0};  (* Fuochi *)
		F2= {-c,0};
		Row[{
			Show[
				Plot[{y  /.Solve[(x/a)^2-(y/b)^2==1]}, {x,-3,3},AspectRatio->1, ImageSize->Large, PlotStyle->{RGBColor[1,0,1],Thick}], (* Iperbole come equazione di secondo grado *)
				Plot[{-b/a x, b/a x},{x,-3,3},AspectRatio->1, ImageSize->Large, PlotStyle->{Gray}],
				Graphics[{PointSize[0.025], (* Punto P a partire dall'angolo a *)
					Point[F1], (* punti che indicano i fuochi *)
					Point[F2],
					Arrowheads[{-.03,.03}], (* impostazioni delle frecce *)
					Arrow[{ {-a,1.1},{-0,1.1}}], (* cosa rappresenta b in termini grafici *)
					Arrow[{ {-1.8,0},{-1.8,b}}],(* cosa rappresenta a in termini grafici *)
					Arrow[{ {0,-0.2},{c,-0.2}}],(* cosa rappresenta c in termini grafici *)
					
					(* quadato che evidenzia i semiassi *)
					Line[{ {-a,b},{a,b}}],
					Line[{ {-a,-b},{a,-b}}],
					Line[{ {-a,-b},{-a,b}}],
					Line[{ {a,-b},{a,b}}],
					
					Dashing[0.01], (* imposta linee tratteggiate *)
					Text["F1",{-c,0.2}], (* etichette dai fuochi *)
					Text["F2",{c,0.2}],
					Style[Text["semiasse\nnon\ntrasverso\n(b)",{-2.2,0.5}],FontSize->14],  (* etichette sui semiassi *)
					Style[Text["semiasse\ntrasverso\n(a)",{-0.5,1.5}],FontSize->14],
					Style[Text["semidistanza\nfocale\n(a)",{0.7,-0.6}],FontSize->14],  (* etichetta sulla semidistanza focale *)
					Style[Text["asintoto\n",{2.2,2.1}],FontSize->14] (* etichetta su uno degli asintoti *)
				}] (* end Graphics *)
			] (* end Show *)
		}, "\n"] (* end Row *)
    ];
  
  
 (* Mostra un grafico con una parabola, in cui il coefficente a \[EGrave] manipolabile *)
 ParabolaTheory[]:=
     Manipulate[
		Row[{
			StringForm["y = `` x^2 + 1",NumberForm[a,3]], (* mostra equazione completa *)
			Plot[a x^2+1,{x,-2,2}, AspectRatio->1, ImageSize->Large, PlotRange->{-2,4},PlotStyle->{RGBColor[0,0,0.6],Thick}], (* mostra grafico *)
		}, "\n"],
		{{a,1,"a"},-20,20} (* coefficente di x^2 manipolabile *)
	];
	

(* FASE 2 --------------------------------------------------------------------------------- *)

(* 
 * verifica le condizioni di esistenza per una figura
 * @param figura: stringa che indica il tipo di figura da verificare (Line, Parabole, Ellipse, Circle, Hyperbole o None)
 * @param a,b,c: coefficenti della figura
 * Ritorna al chiamante una stringa con l'errore riscontrato o la stringa "OK" in caso di successo
 *)
checkCondition[figura_,a_,b_,c_]:= (
	If[figura=="None", (* figura non definita: \[EGrave] un errore in quanto vogliamo avere 2 figure nel sistema *)
		Return["Inserire due equazioni"]
	];
	If[a == Null || b==Null || c==Null, (* errore generico: campi dei coefficenti vuoti *)
		Return["Errore: hai lasciato alcuni campi vuoti"]
	];
	If[figura == "Line"&&a==0&&b==0, (* Retta: almeno uno tra a e b deve essere diverso da 0 *)
		Return["Retta: valori dei coefficienti non validi"]
	];
	If[figura == "Parabola"&&a==0, (* Parabola: a \[NotEqual] 0 *)
		Return["Parabola: valore 'a' deve essere diverso da 0, altrimenti ottieni una retta"]
	];
	If[figura == "Ellipse"&&(a<=0||b<=0), (* Ellisse: coefficenti non negativi *)
		Return["Ellisse: i valori dei coefficienti devono essere positivi"]
	];
	If[figura == "Ellipse"&&(a==b), (* Ellisse: coefficenti diversi tra loro (senn\[OGrave] collassa a circonferenza) *)
		Return["Ellisse: devi avere 'a' diverso da 'b', altrimenti ottieni una circonferenza"]
	];
	If[figura == "Circle"&&(a==0&&b==0&&c==0), (* Circonferenza: almeno uno dei coefficenti deve essere non nullo *)
		Return["Circonferenza: valori non ammessi, l'equazione rappresenta le rette bisettrici"]
	];
	If[figura=="Circle" && (-a/2)^2+(-b/2)^2-c<0, (* equazione di esistenza della Circonferenza *)
		Return["Circonferenza: valori non ammessi, bisogna rispettare la condizione descritta nella teoria"]
	];
	If[figura == "Hyperbole"&&(a<=0||b<=0)|| figura == "Ellisse"&&a<=b, (* Iperbole: coefficienti non negativi *)
		Return["Iperbole: i valori dei coefficienti devono essere positivi"]
	];
	Return ["OK"]; (* Nessun errore *)
);

(* 
 * Cuore di fase 2: permette di creare un sistema di 2 equazioni e mostrarne il grafico
 *)
systemManipulate[]:= 
	Module[{fun1="None", fun2="None", eq1, eq2, a1Edit=3, b1Edit=1, c1Edit=1, a2Edit=1, b2Edit=1, c2Edit=1, a1, b1, c1, a2, b2, c2, errorMsg="",
		ris1,ris2, case=-1, sol={}, print=False, intersections,color1,color2,speakImg = Import[".\\img\\megafono.png"],sistResult="",fontSize,fontSize2}, (* variabili locali *)
		(* Le variabili che terminano con "Edit" rappresentano i coefficienti delle figure durante l'input da parte dell'utente.
		 * Non appena l'utente preme il pulsare per risolvere il sistema e non vi sono problemi nei coefficienti, il contenuto di queste variabili 
		 * vengono copiate nelle variabili a1,b1,c1,a2,b2,c2 che verranno usate per mostrare il grafico 
		 * La variabile PRINT ha sempre valore false tranne quando l'utente vuole mostrare a grafico le eq. che ha inserito ed i coefficienti sono corretti*)
		
		Row[{ (* Row che racchiude tutta il contenuto della fase 2 *)
		
			(* funzione ausiliaria che verifica le condizioni di esistenza e, se rispettate, setta la variabile PRINT a true, senn\[OGrave] a false *)
			solveSystem[]:=(
				(* verifico le condizioni sui parametri *)
				ris1 = checkCondition[fun1,a1Edit,b1Edit,c1Edit]; (*chiamo "checkCondition" con la figura(retta,circ.,iperbole,etc) e i relativi coefficienti*)
				ris2 = checkCondition[fun2,a2Edit,b2Edit,c2Edit]; (*chiamo "checkCondition" con la figura(retta,circ.,iperbole,etc) e i relativi coefficienti*)
				If[ris1=="OK",
					(* THEN 1 *)
					If[ris2=="OK",
						(* THEN 2 *)
						(* tutto bene: metto le variabili di edit in quelle da usare nella plot *)
						a1=a1Edit;b1=b1Edit;c1=c1Edit;a2=a2Edit;b2=b2Edit;c2=c2Edit; errorMsg=""; print=True,
						(* ELSE 2 *)
						errorMsg=ris2; print=False (*vi sono problemi nei coefficienti della figura due; assegno l'errore alla variabile ERRORMSG; metto la variabile print a false*)
					],
					(* ELSE 1 *)
					errorMsg=ris1; print=False (*vi sono problemi nei coefficienti della figura due; assegno l'errore alla variabile ERRORMSG; metto la variabile print a false*)
				];
			);
	
			(* LEFT ---------------- *)
			Column[{ (* Column che contiene i comandi per inserire le equazioni *)
				(* Pulsanti per l'inserimento dinamico di funzioni nel sistema *)
				Button["Retta"Import[".\\img\\retta.png"],
					If[fun1=="None",
						(* THEN *)(*se la fun1 \[EGrave] "none" allora gli assegno l'eq. della figura corrente(retta in questo caso) e setto le variabili "Edit" con i valori default dei coefficienti*)
						print = False;fun1="Line";a1Edit=1;b1Edit=1;c1Edit=1; eq1:=a1 x+b1 y+c1==0;,
						(* ELSE *)(*se la fun1 NON \[EGrave] "none" allora assegno l'eq. della figura corrente(retta in questo caso) alla varibile fun2 *)
						print = False;fun2="Line";a2Edit=2;b2Edit=1;c2Edit=2; eq2:=a2 x+b2 y+c2==0;
					],
					BaseStyle->{18} (* definisce il font *)
				],
				Button["Parabola"Import[".\\img\\Parabola.png"],
					If[fun1=="None",
						print = False; fun1="Parabola"; a1Edit=1;b1Edit=1;c1Edit=1; eq1:=y ==a1 x^2+b1 x+c1;,
						print = False; fun2="Parabola"; a2Edit=2;b2Edit=1;c2Edit=1; eq2:=y ==a2 x^2+b2 x+c2;
					],
					BaseStyle->{18} (* definisce il font *)
				],
				Button["Circonferenza"Import[".\\img\\circonferenza.png"],
					If[fun1=="None",
						print = False;fun1="Circle"; a1Edit=2;b1Edit=1;c1Edit=1; eq1:=x^2+y^2+a1 x+b1 y+c1==0;,
						print = False;fun2="Circle"; a2Edit=4;b2Edit=1;c2Edit=1; eq2:=x^2+y^2+a2 x+b2 y+c2==0;
					],
					BaseStyle->{18}
				],
				Button["Ellisse"Import[".\\img\\ellisse.png"],
					If[fun1=="None",
						print = False;fun1="Ellipse";a1Edit=4;b1Edit=1;c1Edit=1;eq1 := (x/a1)^2+(y/b1)^2==1;,
						print = False;fun2="Ellipse";a2Edit=2;b2Edit=1;c2Edit=1;eq2 := (x/a2)^2+(y/b2)^2==1;
					],
					BaseStyle->{18}
				],
				Button["Iperbole"Import[".\\img\\iperbole.png"],
					If[fun1=="None",
						print = False;fun1="Hyperbole"; a1Edit=4;b1Edit=1;c1Edit=1; eq1 := (x/a1)^2-(y/b1)^2==1;,
						print = False;fun2="Hyperbole"; a2Edit=2;b2Edit=1;c2Edit=1; eq2 := (x/a2)^2-(y/b2)^2==1;
					],
					BaseStyle->{18}
				],
				
				Row[{  (* Row che contiene il sistema di equazioni scelte dall'utente e permette di editare i coefficienti *)
					Column[{
						Style["{",FontSize->100](* parentesi graffa che rappresenta il sistema *)
					}],
		
					Column[{  (* insieme di equazioni con coefficienti editabili *)
						(* il contenuto delle due Row seguenti dipende dal tipo di equazione selezionata dall'utente *)
						fontSize = 3; (*dimensioni delle inputBox che permettono di inserire i valori dei coefficienti*)
						fontSize2 = 15; (*font delle equazioni mostrate dentro la parentesi graffa*)
						Dynamic[
							Text[Style[
								EventHandler[ (* tutto ci\[OGrave] che \[EGrave] racchiuso qui, ha associato un evento sul tasto Invio che rischiama la funz. solveSystem *)
									Switch[fun1,
										"Line",
										Row[{InputField[Dynamic[a1Edit],Number, FieldSize->fontSize]," x + ", InputField[Dynamic[b1Edit],Number ,FieldSize->fontSize]," y + ",InputField[Dynamic[c1Edit],Number, FieldSize->fontSize], " = 0"},ImageMargins->5],
										
										"Parabola",
										Row[{"y = ",InputField[Dynamic[a1Edit],Number, FieldSize->fontSize]," x "^2//TraditionalForm," + ", InputField[Dynamic[b1Edit],Number ,FieldSize->fontSize]," x + ",InputField[Dynamic[c1Edit],Number, FieldSize->fontSize]},ImageMargins->5],
										
										"Circle",
										Row[{" x "^2//TraditionalForm," + "," y "^2//TraditionalForm ," + ",InputField[Dynamic[a1Edit],Number, FieldSize->fontSize]," x + ",InputField[Dynamic[b1Edit],Number, FieldSize->fontSize], " y + ",InputField[Dynamic[c1Edit],Number, FieldSize->fontSize], " = 0"},ImageMargins->5],
										
										"Ellipse",
										Row[{" x "^2/InputField[Dynamic[a1Edit],Number, FieldSize->fontSize]^2//TraditionalForm," + "," y "^2/ InputField[Dynamic[b1Edit],Number ,FieldSize->fontSize]^2//TraditionalForm," = 1"},ImageMargins->5],
										
										"Hyperbole",
										Row[{" x "^2/InputField[Dynamic[a1Edit],Number, FieldSize->fontSize]^2//TraditionalForm," - "," y "^2/ InputField[Dynamic[b1Edit],Number ,FieldSize->fontSize]^2//TraditionalForm," = 1"},ImageMargins->5],
										
										_, "" (* valore di default: non fa niente *)
									],{"ReturnKeyDown":> solveSystem[]}],(* fine EventHandler*)
								fontSize2]
							]
						],
						
						Dynamic[
							Text[Style[
								EventHandler[ (* tutto ci\[OGrave] che \[EGrave] racchiuso qui ha associato un evento sul tasto Invio *)
									Switch[fun2,
										"Line",
										Row[{InputField[Dynamic[a2Edit],Number, FieldSize->fontSize]," x + ", InputField[Dynamic[b2Edit],Number ,FieldSize->fontSize]," y + ",InputField[Dynamic[c2Edit],Number, FieldSize->fontSize], " = 0"},ImageMargins->5],
										
										"Parabola",
										Row[{"y = ",InputField[Dynamic[a2Edit],Number, FieldSize->fontSize]," x "^2//TraditionalForm," + ", InputField[Dynamic[b2Edit],Number ,FieldSize->fontSize]," x + ",InputField[Dynamic[c2Edit],Number, FieldSize->fontSize]},ImageMargins->5],
										
										"Circle",
										Row[{" x "^2//TraditionalForm," + "," y "^2//TraditionalForm ," + ",InputField[Dynamic[a2Edit],Number, FieldSize->fontSize]," x + ",InputField[Dynamic[b2Edit],Number, FieldSize->fontSize], " y + ",InputField[Dynamic[c2Edit],Number, FieldSize->fontSize], " = 0"},ImageMargins->5],
										
										"Ellipse",
										Row[{" x "^2/InputField[Dynamic[a2Edit],Number, FieldSize->fontSize]^2//TraditionalForm," + "," y "^2/ InputField[Dynamic[b2Edit],Number ,FieldSize->fontSize]^2//TraditionalForm," = 1"},ImageMargins->5],
										
										"Hyperbole",
										Row[{" x "^2/InputField[Dynamic[a2Edit],Number, FieldSize->fontSize]^2//TraditionalForm," - "," y "^2/ InputField[Dynamic[b2Edit],Number ,FieldSize->fontSize]^2//TraditionalForm," = 1"},ImageMargins->5],
										
										_, "" (* valore di default: non fa niente *)
									],{"ReturnKeyDown":> solveSystem[]}],(* fine EventHandler*)
								fontSize2]
							]
						]
					}],(* end COLUMN che visualizza il sistema *)
					
					(* tasto Invio risolve il sistema (senza bisogno di cliccare sul pulsante) *)
					
					Column[{ (* pulsanti per la rimozione e l'ascolto di un'equazione *)
					
						(* Pulsanti relativi alla PRIMA funzione del sistema *)
						Dynamic[
							If[fun1!="None",
								(* THEN 1 *)
								Row[{
									(* pulsante per la rimozione di una funzione *)
									Style[
										Button[
											"X",
											(* sposto la funzione 2 nella prima *)
											print=False;fun1=fun2;a1Edit=a2Edit;b1Edit=b2Edit;c1Edit=c2Edit;fun2="None";
											Switch[fun1, 
												"Line", eq1:=a1 x+b1 y+c1==0;,
												"Parabola", eq1:=y ==a1 x^2+b1 x+c1;,
												"Circle", eq1:=x^2+y^2+a1 x+b1 y+c1==0;,
												"Ellipse", eq1 := (x/a1)^2+(y/b1)^2==1;,
												"Hyperbole", eq1 := (x/a1)^2-(y/b1)^2==1;,
												_,
											];,
											ImageSize->{30,30}
										],
									Red,Bold],
									
									(* pulsante per lo Speak *)
									Style[Button[speakImg,
										Switch[fun1, 
											"Line", eqToSpeak = SpokenString[a1Edit "x" + b1Edit "y" + c1Edit == 0];, (*assegno alla variabile eqToSpeak l'equazioni da riprodurre con la sintesi vocale*)
											"Parabola", eqToSpeak = SpokenString["y" == a1Edit "x"^2+b1Edit "x"+c1Edit];,
											"Circle", eqToSpeak = SpokenString["x"^2+"y"^2+a1Edit "x"+b1Edit "y"+c1Edit == 0];,
											"Ellipse", eqToSpeak = SpokenString[("x"/a1Edit)^2+("y"/b1Edit)^2 == 1];,
											"Hyperbole", eqToSpeak = SpokenString[("x"/a1Edit)^2-("y"/b1Edit)^2 == 1];,
											_,
										];
										(*Azione del button Speak *)
										(* StringSplit divide l'equazione assegnata a eqToSpeak in unita semplici(stringhe);
									     * sostituiamo alcune stringhe della sintesi con altre stringhe per rendere il tutto pi\[UGrave] comprensibile
									     * StringRiffle concatena le stringhe 
									     * Speak inizia la riproduzione dell'intera stringa *)
										StringSplit[eqToSpeak]/.{"quote"->"","plus"->"pi\[UGrave]", "equals"->"uguale","times"->"per","minus"->"meno","squared"->"al quadrato"}//StringRiffle//Speak;,
										(*Aspetto del button*)
										ImageSize->{35,35}
									]]
								}],
								(* ELSE 1 *)
								"" (* non stampa nulla in quando nessuna funz. \[EGrave] stata selezionata *)
							]
						],
					
						(*Pulsanti relativi alla SECONDA funzione del sistema*)
						Dynamic[
							If[fun2!="None",
								(* THEN 1 *)
								Row[{
									(* pulsante per la rimozione di una funzione *)
									Style[Button[
										"X",
										fun2="None"; print=False;,
										ImageSize->{30,30}
									],(*end buttom*)
									Red,Bold],
									
									(* pulsante per lo Speak *)
									Style[Button[speakImg,
										Switch[fun2, 
											"Line", eqToSpeak = SpokenString[a2Edit "x" + b2Edit "y" + c2Edit == 0];, (*assegno alla variabile eqToSpeak l'equazioni da riprodurre con la sintesi vocale*)
											"Parabola", eqToSpeak = SpokenString["y" == a2Edit "x"^2+b2Edit "x"+c2Edit];,
											"Circle", eqToSpeak = SpokenString["x"^2+"y"^2+a2Edit "x"+b2Edit "y"+c2Edit == 0];,
											"Ellipse", eqToSpeak = SpokenString[("x"/a2Edit)^2+("y"/b2Edit)^2 == 1];,
											"Hyperbole", eqToSpeak = SpokenString[("x"/a2Edit)^2-("y"/b2Edit)^2 == 1];,
											_,	
										];
										(*Azione del button Speak*)
										(* StringSplit divide l'equazione assegnata a eqToSpeak in unita semplici(stringhe);
								         * sostituiamo alcune stringhe della sintesi con altre stringhe per rendere il tutto pi\[UGrave] comprensibile
								         * StringRiffle concatena le stringhe 
								         * Speak inizia la riproduzione dell'intera stringa*)
								         StringSplit[eqToSpeak]/.{"quote"->"","plus"->"pi\[UGrave]", "equals"->"uguale","times"->"per","minus"->"meno","squared"->"al quadrato"}//StringRiffle//Speak;,
										(*Aspetto del button*)
										ImageSize->{35,35}
									]]
								}],
								(* ELSE 1 *)
								"" (* non stampa nulla in quando nessuna funz. \[EGrave] stata selezionata *)
							]
						]
					}] (* end COLUMN che contiene pulsanti per la rimozione e l'ascolto di un'equazione *)
				}], (* end ROW che contiene il sistema di equazioni scelte dall'utente e permette di editare i coefficienti *)
				
				(* Pulsante risolvi: passa le variabili di editing ai parametri dei grafici*)
				Button["Risolvi",
					solveSystem[],
					BaseStyle->{18}  (* dimensione del font *)
				]
			},ItemSize->{20, 2.4}], (* end left COLUMN che contiene i comandi per inserire le equazioni *)
	  
	  
			"\t", (* SPAZIO tra comandi e grafici *)
	  
	  	  (* RIGHT --------------- *)
	  	  Dynamic[Column[{ (* COLUMN che contiene Grafico, soluzioni e messaggi d'errore *)
	  	
				(*Se la variabile PRINT \[EGrave] true vuol dire che posso plottare le figure che sono memorizzate nelle variabili fun1 e fun2 con i rispettivi
				 * parametri che sono memorizzati nelle variabili a1,b1,c2,a2,b2,c2
				 *)
				If[print==True,
					(* THEN: visualizza grafico ed eventuali soluzioni *)
					Column[{ (* COLUMN che contiene tutto il grafico ed eventuali risultati *)
					
						(* sequenza di IF che controlla se tra le figure da plottare vi sono rette e se i loro coefficienti B sono uguali a zero;
						 * A seconda, quindi, delle figure da plottare e dei loro parametri sar\[OGrave] in un caso diverso che memorizzo nella variabile CASE;
						 * Questo \[EGrave] stato fatto perch\[EGrave] la plot non riesce a disegnare una retta parallela all'asse y e quindi devo utilizzare la funz. Line;
						 * A seconda delle figure da plottare, inoltre, definisco i colori delle figure.
						 *)
						If[fun1=="Line"&&b1==0,
							(* THEN 1 *)
							If[fun2=="Line" &&b2==0,
								(* THEN 2 *)
								case=1; (* CASO 1: 2 rette con parallele all'asse y *)
								sol =Solve[Rationalize[x==-(c1/a1)&&x==-(c2/a2)],{x,y},Reals]; 
								color1=Red; 
								color2=Red,
								(* ELSE 2 *)
								case=2; (* CASO 2: prima equazione \[EGrave] una retta parallela all'asse y *)
								sol =Solve[Rationalize[x==-(c1/a1)&&eq2],{x,y},Reals]; 
								color1 = Red;
								Switch[fun2, 
									"Line", color2 = Red;,
									"Parabola", color2 = Blue;,
									"Circle", color2 = Green;,
									"Ellipse", color2 = Orange;,
									"Hyperbole", color2 = Purple;,
									_,
								]
							];,
							(* ELSE 1 *)
							If[fun2=="Line" &&b2==0,
								(* THEN 3 *)
								case=3; (* CASO 3: seconda equazione \[EGrave] una retta parallela all'asse y *)
								sol =Solve[Rationalize[eq1&&x==-(c2/a2)],{x,y},Reals];
								color2 = Red;
								Switch[fun1, 
									"Line", color1 = Red;,
									"Parabola", color1 = Blue;,
									"Circle", color1 = Green;,
									"Ellipse", color1 = Orange;,
									"Hyperbole", color1 = Purple;,
									_,
								],
								(* ELSE 3 *)
								case=4;(* CASO 4: caso generico risolvibile con una plot semplice *)
								sol =Solve[Rationalize[eq1&&eq2],{x,y},Reals];
								Switch[fun1, 
									"Line", color1 = Red;,
									"Parabola", color1 = Blue;,
									"Circle", color1 = Green;,
									"Ellipse", color1 = Orange;,
									"Hyperbole", color1 = Purple;,
									_,
								];
								Switch[fun2, 
									"Line", color2 = Red;,
									"Parabola", color2 = Blue;,
									"Circle", color2 = Green;,
									"Ellipse", color2 = Orange;,
									"Hyperbole", color2 = Purple;,
									_,
								]
							];
						];
						
						(*controllo se il sistema formato dalle due equazioni ha delle soluzioni e, in caso positivo, definisco i punti di intersezioni per disegnarli in seguito *)
						If[Length[sol]==0,
							intersections={};, (*then*)
							If[Length[sol[[1]]]==1,  (*else*)
								intersections={};,  (*then*)
								intersections={Red,PointSize[Large],Point[{x,y}/.sol]}  (*else*)
							]
						];
						
						(*A seconda del caso in cui sono, definito nella parte iniziale, plotto con funzioni differenti le figure date in input *)
						If[case==1, (* CASO 1: 2 rette con parallele all'asse y *)
							Show[
								Plot[,{x,-20,20},PlotRange->{{-20,20},{-20,20}},ImageSize->Large, AspectRatio->1], (* grafico vuoto *)
								(* rette disegnate con primitive Line *)
								Graphics[{ color1, Thick, Line[{ {-(c1/a1),-100},{-(c1/a1),100}}],Line[{ {-(c2/a2),-100},{-(c2/a2),100} }]}],
								Graphics[{intersections}] (* stampa punti di intersezione (eventualmente nessuno) *)
							],
							If[case==2,(* CASO 2: prima equazione \[EGrave] una retta parallela all'asse y *)
								Show[
									(* disegno eq2 *)
									Plot[y/.Solve[eq2],{x,-20,20},PlotRange->{{-20,20},{-20,20}},PlotStyle->{color2}, ImageSize->Large, AspectRatio->1],
									(* eq1 disegnata con primitiva Line *)
									Graphics[{ color1, Thick,Line[{ {-(c1/a1),-100},{-(c1/a1),100} }]}],
									Graphics[{intersections}] (* stampa punti di intersezione (eventualmente nessuno) *)
								],
								If[case==3, (* CASO 3: seconda equazione \[EGrave] una retta parallela all'asse y *)
									Show[
										(* disegno eq1 *)
										Plot[y/.Solve[eq1],{x,-20,20},PlotRange->{{-20,20},{-20,20}},PlotStyle->{color1},ImageSize->Large, AspectRatio->1],
										(* eq2 disegnata con primitiva Line *)
										Graphics[{ color2,Thick, Line[{ {-(c2/a2),-100},{-(c2/a2),100} }]}],
										Graphics[{intersections}] (* stampa punti di intersezione (eventualmente nessuno) *)
									],
									If[case==4, (* CASO 4: caso generico risolvibile con una plot semplice *)
										Show[
											(* disegno eq1 e eq2 *)
											Plot[y/.Solve[eq1],{x,-20,20},PlotRange->{{-20,20},{-20,20}},PlotStyle->{color1},ImageSize->Large,AspectRatio->1],
											Plot[y/.Solve[eq2],{x,-20,20},PlotRange->{{-20,20},{-20,20}},PlotStyle->{color2},ImageSize->Large, AspectRatio->1],
											Graphics[{intersections}] (* stampa punti di intersezione (eventualmente nessuno) *)
										]
									]
								]
							]
						],
						
						(* Controllo se il sistema \[EGrave] impossibile, determinato o ha infinite soluzioni;
						 * Stampa, in caso, i punti di intersezione e assegna alla variabile SISTRESULT la stringa che sar\[AGrave] letta dal sintetizzatore vocale 
						 *)
						If [Length[sol]==0,
							(* THEN 1: nessuna soluzione *)
							sistResult="Il sistema \[EGrave] impossibile";
							StringForm["Il sistema \[EGrave] impossibile"],
							(* ELSE 1 *)
							If[Length[sol[[1]]]==1,
								(* THEN 2: soluzioni definite da una regola della forma y\[Rule]f(x) *)
								sistResult="Il sistema ha infinite soluzioni";
								StringForm["Il sistema ha infinite soluzioni"],
								(* ELSE 2: soluzioni definite come punti, nella forma x\[Rule]v1, y\[Rule]v2 *)
								sistResult="Il sistema \[EGrave] determinato";
								Row[{
									StringForm["Il sistema \[EGrave] determinato\nLe soluzioni sono : "],
									TableForm[ N[sol,3],TableHeadings->{{},{"X","Y"}}] (* soluzioni in forma ti tabella *)
								}]
							]
						],
						(* Button che permette all'utente di ascoltare la soluzione, nel caso non ci siano errori *)
						Style[Button[speakImg,
							Speak[sistResult];,
							ImageSize->{35,35}
						]]
					}], (* fine COLUMN che contiene tutto il grafico ed eventuali risultati o errori*)
					
					(* ELSE: non visualizzare grafico *)
					(* controlla se ci sono messaggi di errore relativi ai coefficienti inseriti ed in caso li mostra *)
					If[errorMsg=="",
						(* THEN: non ci sono problemi, \[EGrave] il caso iniziale (nessuna funzione inserita) *)
						Column[{
							Plot[,{x,-20,20},PlotRange->{{-20,20},{-20,20}},ImageSize->Large, AspectRatio->1], (* grafico vuoto *)
						}],
						(* ELSE: ci sono errori *)
						Column[{
							Plot[,{x,-20,20},PlotRange->{{-20,20},{-20,20}},ImageSize->Large, AspectRatio->1], (* grafico vuoto *)
							 (* messaggio d'errore con icona grafica *)
							Style[ImageResize[Import[".\\img\\error.png"],20]StringForm[" "<>errorMsg],Red],
							(* Button che permette all'utente di ascoltare l'errore riscontrato *)
							Style[Button[speakImg,
								Speak[errorMsg],
								ImageSize->{35,35}
							]]
						}]
					](* fine IF che controlla se ci sono messaggi di errore ed in caso li mostra*)
				](* fine if che controlla se PRINT \[EGrave] uguale a 1 *)

			}, ItemSize->{28, Automatic}]] (* end COLUMN che contiene Grafico, soluzioni e messaggi d'errore *) (* end DYNAMIC*)
		}, Background->White](* end ROW che racchiude tutta il contenuto della fase 2 *)
	] (* chiudo DYNAMIC MODULE *);
  
(* FASE 3 ---------------------------------------------------------------- *)
  
(*
 * genera figure a caso tra le 5 tipologie di coniche
 * @param exceptionFun1, exceptionFun2: indica una coppia di figure non valida
 *  (per quando dobbiamo generare figure sbagliate, diverse da quelle giuste gi\[AGrave] generate)
 * @return {fun1, eq1, fun2, eq2}
 *)
randomFigures[exceptionFun1_: "None", exceptionFun2_: "None"] :=
  Module[{fun1, fun2, a1, b1, c1, a2, b2, c2, eq1, eq2, min1, min2},
	(* scelgo 2 figure a caso tra le 5 alternative *)
	fun1 = RandomChoice[{"Line", "Parabola", "Circle", "Ellipse", "Hyperbole"}];
	fun2 = RandomChoice[{"Line", "Parabola", "Circle", "Ellipse", "Hyperbole"}];
   
	(* ripete la scelta se ha preso una coppia che non andava bene *)
	While[(fun1 == exceptionFun1 && fun2 == exceptionFun2) || (fun1 == exceptionFun2 && fun2 == exceptionFun1),
		fun1 = RandomChoice[{"Line", "Parabola", "Circle", "Ellipse", "Hyperbole"}];
		fun2 = RandomChoice[{"Line", "Parabola", "Circle", "Ellipse", "Hyperbole"}];
    ];
    
    (* Iperbole ed Ellisse hanno casi particolari nella scelta dei coefficienti *)
	If[fun1 == "Hyperbole" || fun1 == "Ellipse",
		min1 = 1, (* THEN: i coefficenti generati devono essere positivi *)
		min1 = -10 (* ELSE: prendo il range dei coefficienti tra -10 e 10 *)
	];
	(* stessi vincoli sulla seconda funzione *)
	If[fun2 == "Hyperbole" || fun2 == "Ellipse",
		min2 = 1, (* THEN *)
		min2 = -10 (* ELSE *)
    ];
	
	(* genero casualmente i 3 coefficienti della prima funzione *)
	a1 = RandomInteger[{min1, 10}];
	b1 = RandomInteger[{min1, 10}];
	c1 = RandomInteger[{-10, 10}];
   
	(* verifico se i valori presi rispettano le condizioni di esistenza della figura 1 *)
	While[checkCondition[fun1, a1, b1, c1] != "OK" || (fun1 == "Line" && b1 == 0),
		(* in caso contrario li riprendo a caso *)
		a1 = RandomInteger[{min1, 10}];
		b1 = RandomInteger[{min1, 10}];
		c1 = RandomInteger[{-10, 10}];
    ];
    
    (* genero casualmente i 3 coefficienti della prima funzione *)
	a2 = RandomInteger[{min2, 10}];
	b2 = RandomInteger[{min2, 10}];
	c2 = RandomInteger[{-10, 10}];
	
	(* verifico se i valori presi rispettano le condizioni di esistenza della figura 2 *)
	While[checkCondition[fun2, a2, b2, c2] != "OK" || (fun2 == "Line" && b2 == 0),
		(* in caso contrario li riprendo a caso *)
		a2 = RandomInteger[{min2, 10}];
		b2 = RandomInteger[{min2, 10}];
		c2 = RandomInteger[{-10, 10}];
	];
	
	(* associa l'equazione corrispondente alla figura 1, come espressione booleana *)
	Switch[fun1, 
		"Line", eq1 := a1 x + b1 y + c1 == 0;,
		"Parabola", eq1 := y == a1 x^2 + b1 x + c1;,
		"Circle", eq1 := x^2 + y^2 + a1 x + b1 y + c1 == 0;,
		"Ellipse", eq1 := (x/a1)^2 + (y/b1)^2 == 1;,
		"Hyperbole", eq1 := (x/a1)^2 - (y/b1)^2 == 1;,
		_,
	];
   
	(* If Tipo1 generare con una piccola probabilit\[AGrave] la stessa figura di prima *)
	If[exceptionFun1 == "None" && exceptionFun2 == "None" && RandomInteger[{1, 10}] > 9, (* probabilit\[AGrave] = 1/10 *)
		fun2 = fun1; a2 = a1; b2 = b1; c2 = c1;
		,(* else niente *)
    ];
	
	(* associa l'equazione corrispondente alla figura 2, come espressione booleana *)
	Switch[fun2, 
    	"Line", eq2 := a2 x + b2 y + c2 == 0;,
    	"Parabola", eq2 := y == a2 x^2 + b2 x + c2;,
    	"Circle", eq2 := x^2 + y^2 + a2 x + b2 y + c2 == 0;,
		"Ellipse", eq2 := (x/a2)^2 + (y/b2)^2 == 1;,
    	"Hyperbole", eq2 := (x/a2)^2 - (y/b2)^2 == 1;,
    	_,
    ];
	
	Return[{fun1, eq1, fun2, eq2}] (* restituisco una tupla di figure ed equazioni *)
];


(*
 * cuore di fase 3: gioco formato da una serie di esercizi di 3 tipi diversi (generati casualmente)
 * @param limit: intero che indica quanti esercizi fare prima di terminare
 *)
mainPhase3[limit_Integer:5] :=
	(* ad ogni passo genera un esercizio casuale tra 3 tipi diversi, e da pi\[UGrave] alternative tra cui scegliere
	 * se la risposta \[EGrave] corretta incrementa il punteggio; in ogni caso cambia esercizio;
	 * ad ogni passo decrementa remaining, e quando arriva a 0 mostra il totale di risposte esatte rispetto al numero di domande
	 *)
	DynamicModule[{ret, alts, score = 0, sol, type, fun1, fun2, eq1, eq2, color1, color2, rightChoise, exerciseType1, exerciseType2, exerciseType3,
		message = "", intersections, minX, maxX, minY, maxY, rispArray, NewExercise, showModalMessage, SoundGood, SoundBad, figura1, figura2, fontSize = 18, fontSize2, altFunz1, altFunz2 , altFig1, altFig2, 
		imageLine, imageParabola, imageCircle, imageEllipse, imageHyperbole, remaining=limit},
		
		(* 3 risposte possibili per tipo 1 *)
		rispArray = {"Il sistema \[EGrave] determinato", "Il sistema ha infinite soluzioni", "Il sistema \[EGrave] impossibile"};
		
		(* immagini importate, una per ogni figura, da inserire nei pulsanti delle risposte per il tipo 3 *)
		imageLine = Import[".\\img\\retta.png"];
		imageParabola = Import[".\\img\\Parabola.png"];
		imageCircle = Import[".\\img\\circonferenza.png"];
		imageEllipse = Import[".\\img\\ellisse.png"];
		imageHyperbole = Import[".\\img\\iperbole.png"];
  
		(* FUNZIONI AUSILIARIE PER INIZIALIZZARE LE VARIABILI PER L'ESERCIZIO *)
		
		(* TIPO 1: all'utente viene mostrato un grafico che rappresenta un sistema, deve indicare se \[EGrave] determinato, indeterminato e impossibile *)
		exerciseType1[] := (
			Module[{solutionX, solutionY, i},
				(* Breve messaggio da mostrare all'utente = consegna dell'esercizio *)
				message = "Il sistema rappresentato \[EGrave] determinato, indeterminato o impossibile?";
				
				(* in RET metto le informazioni relative alle due eq. casuali scelte *)
				ret = randomFigures[];
				(* la struttura ritornata dalla funzione "randomFigures" \[EGrave] la seguente : { figura_1, equazione_figura_1, figura_2, equazione_figura_2 }*)
				eq1 = ret[[2]];
				eq2 = ret[[4]];
				fun1 = ret[[1]];
				fun2 = ret[[3]];
				(*inizializzo la variabile SOL che conterr\[AGrave] le soluzioni del sistema, ovvero i punti di intersezione tra le figure *)
				sol = {};
				(* la variabile RIGHTCHOISE serve a tenere memoria della risposta corretta. 
				 * Quindi potr\[AGrave] avere un valore tra 1 e 3 compresi che rappresentano le tre risposte possibili *)
				rightChoise = 0;
			
				(* risolvo il sistema di due equazioni e assegno i giusti colori per poi disegnarle*)
				sol = Solve[Rationalize[eq1 && eq2], {x, y}, Reals];
				Switch[fun1, 
					"Line", color1 = Red;,
					"Parabola", color1 = Blue;,
					"Circle", color1 = Green;,
					"Ellipse", color1 = Orange;,
					"Hyperbole", color1 = Purple;,
					_,
				];
				Switch[fun2, 
					"Line", color2 = Red;,
					"Parabola", color2 = Blue;,
					"Circle", color2 = Green;,
					"Ellipse", color2 = Orange;,
					"Hyperbole", color2 = Purple;,
					_,
				];
				
				
				If[Length[sol] == 0,
					(* THEN *)
					intersections = {};, 
					(* ELSE *)
					If[Length[sol[[1]]] == 1,  
						(* THEN *)
						intersections = {};,
						(* ELSE *) (*se vi sono soluzioni, definisco i valori delle x e delle y relative ai punti di intersezione per plottare con il focus in essi *)
						(* Trovo valori X minimi e massimi *)
						i = 0;
						solutionX = {};
						While[i < Length[sol],
							i = i + 1; 
							AppendTo[solutionX, N[sol[[i]][[1]][[2]]]]
							(* prendo la soluzione i-esima { x\[Rule]NUMERO, y\[Rule]NUMERO } , il primo elemento { x\[Rule]NUMERO }, la seconda componente { NUMERO } *)
						];
						(* prendo il valore minimo e il valore massimo delle Y dall'array SOLUTION_X e aggiungo un margine di 10 unita *)
						minX = Min[solutionX] - 10;
						maxX = Max[solutionX] + 10;
						
						(* Trovo valori Y minimi e massimi*)
						i = 0;
						solutionY = {};
						While[i < Length[sol],
							i = i + 1; 
							AppendTo[solutionY, N[sol[[i]][[2]][[2]]]]
							(* prendo la soluzione i-esima { x\[Rule]NUMERO, y\[Rule]NUMERO } , il secondo elemento { y\[Rule]NUMERO }, la seconda componente { NUMERO } *)
						];
						(* prendo il valore minimo e il valore massimo delle Y dall'array SOLUTION_Y e aggiungo un margine di 10 unita *)
						minY = Min[solutionY] - 10;
						maxY = Max[solutionY] + 10;
						
						(* definisco i punti di intersezione come pallini di colore rosso da plottare insieme alle figure *)
						intersections = {Red, PointSize[Large], Point[{x, y} /. sol]}
					]
				];
				(* a seconda del numero di soluzioni definisco la risposta corretta.
				 * soluzioni \[GreaterEqual] 1 con due valori (x,y) allora il sistema \[EGrave] determinato
				 * soluzioni \[Equal] 1 con un solo valore ( x o y) allora il sistema ha infinite soluzioni poich\[EGrave] le figure solo sovrapposte 
				 * solizioni < 1 allora il sistema \[EGrave] impossibile *)
				If [Length[sol] == 0,
					(* THEN *)
					rightChoise = 3;, (*Il sistema \[EGrave] impossibile*)
					(* ELSE *)
					If[Length[sol[[1]]] == 1,
						(* THEN *)
						rightChoise = 2;, (*Il sistema ha infinite soluzioni*)
						(* ELSE *)
        				rightChoise = 1;(*Il sistema \[EGrave] determinato*)
        			];
				];
			]
		);
  
	(* TIPO 2: all'utente viene mostrato un grafico di 2 equazioni e deve indicare il sistema che rappresenta *)
	exerciseType2[] := (
		(* Breve messaggio da mostrare all'utente = consegna dell'esercizio *)
		message = "Quale sistema rappresenta il grafico seguente?";
		
		ret = randomFigures[]; (* genero 2 figure a caso da mostrare*)
		
		(* valori delle figure memorizzati in apposite variabili *)
		fun1 = ret[[1]];
		eq1 = ret[[2]];
		fun2 = ret[[3]];
		eq2 = ret[[4]];
		
		alts = {}; (* contenitore per le risposte alternative *)
		
		rightChoise = RandomInteger[{1, 4}]; (* indica la risposta esatta tra le 4 possibili *)
		
		(* Cambia il colore delle figure nel grafico *)
		Switch[fun1, 
			"Line", color1 = Red;,
			"Parabola", color1 = Blue;,
			"Circle", color1 = Green;,
			"Ellipse", color1 = Orange;,
			"Hyperbole", color1 = Purple;,
			_,
		];
		Switch[fun2, 
			"Line", color2 = Red;,
			"Parabola", color2 = Blue;,
			"Circle", color2 = Green;,
			"Ellipse", color2 = Orange;,
			"Hyperbole", color2 = Purple;,
			_,
		];
    );
  
	(* TIPO 3: viene mostrato un sistema di 2 equazioni e l'utente deve dire quali sono le figure mostrate *)
	exerciseType3[] := (
		(* Breve messaggio da mostrare all'utente = consegna dell'esercizio *)
		message = "Quali figure sono presenti in questo sistema?";
		
		(* genero 2 figure a caso di cui mostrare le equazioni *)
		ret = randomFigures[];
		eq1 = ret[[2]];
		eq2 = ret[[4]];
		fun1 = ret[[1]];
		fun2 = ret[[3]];
		
		alts = {}; (* contenitore per le risposte *)
		
		rightChoise = RandomInteger[{1, 4}]; (* indica la risposta esatta tra le 4 possibili *)
		
		(* Cambia le immagini da associare al pulsante, in base alle figure *)
		Switch[fun1, 
			"Line", figura1 = imageLine;,
     		"Parabola", figura1 = imageParabola;,
     		"Circle", figura1 = imageCircle;,
     		"Ellipse", figura1 = imageEllipse;,
     		"Hyperbole", figura1 = imageHyperbole;,
     		_,
     	];
		Switch[fun2, 
     		"Line", figura2 = imageLine;,
     		"Parabola", figura2 = imageParabola;,
     		"Circle", figura2 = imageCircle;,
     		"Ellipse", figura2 = imageEllipse;,
     		"Hyperbole", figura2 = imageHyperbole;,
     		_,
     	];
    );
  
	(* instanzia un nuovo esercizio casuale
	 * @param rightAnswer: indica se la risposta precedente era giusta
	 *)
	NewExercise[rightAnswer_: False] := (
		 (* se la risposta era giusta bisogna incrementare il punteggio *)
		If[rightAnswer,
			score = score + 1,
		];
		
		(* se ci sono ancora da fare degli esercizi *)
		If[remaining>0,
			(* THEN: nuovo esercizio di tipo casuale *)
			remaining = remaining-1;
			type = RandomInteger[{1, 3}]; (* genero un tipo casuale tre 1, 2 e 3 *)
			(* nuovo esercizio casuale *)
			Switch[type,
				1, exerciseType1[];,
				2, exerciseType2[];,
				3, exerciseType3[];,
				_, (* caso default: nulla (non dovrebbe capitare) *)
			];,
			(* ELSE: fine del gioco, mostro il punteggio rispetto al numero di domande *)
			(* creo finestra modale *)
			CreateDialog[
				Column[{
					Style[StringForm["  FINE!  \n\n  Risposte corrette:\n `` su ``  ",score,limit], FontSize->fontSize], (* messaggio testuale *)
					
					DefaultButton[Style["Ricomincia",FontSize -> fontSize], (* pulsante che fa ricominciare la partita *)
						remaining = limit; (* rimette al massimo gli esercizi rimanenti *)
						score = 0; (* e rimette a 0 il punteggio*)
						NewExercise[]; (* nuovo esercizio *)
						DialogReturn[]; (* chiude il dialog *)
					]
				},ItemSize->20], (* end Column della dialog*)
				Modal->True,  (* indica di aprire il dialog come schermata modale (pop-up) *)
				NotebookEventActions->{"WindowClose":>(
												remaining = limit; (* rimette al massimo gli esercizi rimanenti *)
												score = 0; (* e rimette a 0 il punteggio*)
												NewExercise[]; (* nuovo esercizio *)
												DialogReturn[]; (* chiude il diaog *)
											)
										} (* evento di chiusura del dialog *)
			]; (* end DIALOG *)
		] (* end If *)
    );
    
    (*
     * Mostra un dialog che indica se l'utente ha dato la risposta giusta oppure no
     * @param rightAnswer: indica se dare un messaggio di risposta corretta o risposta sbagliata
     *)
    showModalMessage[rightAnswer_:False]:=
		(* crea una finestra modale da mostrare all'utente *)
        CreateDialog[
			Column[{
				If[rightAnswer,
					(* THEN: messaggio di risposta corretta *)
					Style["  Risposta esatta!  ", FontSize->fontSize],
					(* THEN: messaggio di risposta sbagliata, e indico anche qual era quella giusta *)
					Style[StringForm["  Risposta sbagliata...  \n  Quella giusta era la ``  ",rightChoise], FontSize->fontSize]
				],
				(* pulsante di chiusura del modale *)
				DefaultButton[Style["Prossimo",FontSize -> fontSize],
					NewExercise[rightAnswer]; (* apre un nuovo esercizio *)
					DialogReturn[]; (* chiude il dialog *)
				]
			},ItemSize->20], (* end Column della dialog*)
			Modal->True,  (* indica di aprire il dialog come schermata modale (pop-up) *)
			NotebookEventActions->{"WindowClose":>(NewExercise[rightAnswer])} (* evento di chiusura del dialog *)
		]; (* end DIALOG *)
	
	(* funzioni che generano un suono di successo o di errore *)
	SoundGood[] := EmitSound[Sound`AudioToSound[Audio[File["./Audio/suono_successo.mp3"]]]];
	SoundBad[] := EmitSound[Sound`AudioToSound[Audio[File["./Audio/suono_errore.mp3"]]]];
  
	(* CODE ENTRY / MAIN *)
	
	NewExercise[False];
  
	(* OUTPUT GRAFICO *)
	Column[{
		Dynamic[Style[StringForm["Punteggio: ``", score], FontSize -> fontSize]], (* mostro il punteggio attuale *)
		Dynamic[Style[message, FontSize -> fontSize]], (* mostro il compito dell'utente per l'esercizio attuale *)
		
		Row[{ (* ROW gigante *)
		
			(* contenuto del quesito *)
			Dynamic[
				Switch[type,
					1, (* TIPO 1 *)
					If[intersections == {}, (* se non ci sono intersezioni *)
						(* THEN mostro le due figure con assi tra -20 e 20*)
						Show[ 
							Plot[y /. Solve[eq1], {x, -20, 20}, PlotRange -> {{-20, 20}, {-20, 20}}, PlotStyle -> {color1},ImageSize -> Large, AspectRatio -> 1],
							Plot[y /. Solve[eq2], {x, -20, 20}, PlotStyle -> {color2}, ImageSize -> Large, AspectRatio -> 1]
						],
						(* ELSE mostro le due figure con assi che mostrano le intersezioni*)
						Show[ 
							Plot[y /. Solve[eq1], {x, minX, maxX}, PlotRange -> {{minX, maxX}, {minY, maxY}}, PlotStyle -> {color1}, ImageSize -> Large, AspectRatio -> 1],
							Plot[y /. Solve[eq2], {x, minX, maxX}, PlotRange -> {{minX, maxX}, {minY, maxY}}, PlotStyle -> {color2}, ImageSize -> Large, AspectRatio -> 1],
							Graphics[{intersections}]
						]
					],
					
					2, (* TIPO 2 *)
					Show[ (* mostro il grafico delle 2 figure (le intersezioni non sono rilevanti ai fini dell'esercizio) *)
						Plot[y /. Solve[eq1], {x, -20, 20}, PlotRange -> {{-20, 20}, {-20, 20}}, PlotStyle -> {color1}, ImageSize -> Large, AspectRatio -> 1],
						Plot[y /. Solve[eq2], {x, -20, 20}, PlotRange -> {{-20, 20}, {-20, 20}}, PlotStyle -> {color2}, ImageSize -> Large, AspectRatio -> 1]
					],
        
					3, (* TIPO 3 *)
					(* mostro un sistema di 2 equazioni *)
					Row[{
						Column[{
							Style["{", FontSize -> 100](* parentesi graffa che rappresenta il sistema *)
						}],
						Column[{ (* stampa delle equazioni *)
							Text[Style[eq1, FontSize -> fontSize]],
							Text[Style[eq2, FontSize -> fontSize]]
						}]
					}]
				]
			],
      
			"\t\t\t", (* SPAZIO *)
      
      
			Dynamic[
				Column[ (* parte delle risposte *)
					Switch[type,
					
						1,(* TIPO 1 *)
						Table[ (* genero un numero fissato di risposte di cui una sola \[EGrave] quella giusta *)
							Row[{ (* spazio di una risposta *)
								If[i == rightChoise,
									(* THEN: risposta esatta *)
									Button[Style[rispArray[[i]], FontSize -> fontSize],
										SoundGood[]; (* feedback sonoro positivo *)
										showModalMessage[True]; (* messaggio modale *)
									],
									(* ELSE: risposta errata *)
									Button[Style[rispArray[[i]], FontSize -> fontSize],
										SoundBad[]; (* feedback sonoro negativo *)
										showModalMessage[False]; (* messaggio modale *)
									]
								]
							}], (* end Row *)
							{i, 1, 3} (* variabile su cui iterare con la Table *)
						](* end Table *),
         
						2,(* TIPO 2 *)
						Table[ (* genero un numero fissato di risposte di cui una sola \[EGrave] quella giusta *)
							Row[{ (* spazio di una risposta: pulsante per confermare e sistema*)
								If[i == rightChoise,
							
									(* THEN: pulsante della risposta ESATTA *)
									Button[Style[StringForm[" `` ", i], FontSize -> fontSize],
										SoundGood[]; (* feedback sonoro positivo *)
										showModalMessage[True]; (* messaggio modale *)
									],(* end BUTTON *)
							
									(* ELSE: i \[NotEqual] rightChoice *)
									(* pulsante di una risposta sbagliata *)
									Button[Style[StringForm[" `` ", i], FontSize -> fontSize],
										SoundBad[]; (* feedback sonoro negativo *)
										showModalMessage[False]; (* messaggio modale *)
									], (* end BUTTON *)
								],(* end IF *)
					
								Style["{", FontSize -> 80],(* parentesi graffa che rappresenta il sistema *)
            
								fontSize2 = 22; (* opzioni grafiche *)
						
								If[i == rightChoise,
									(* THEN: inserisco le equazioni giuste *)
									AppendTo[alts, ret];
						
									Column[{  (* insieme di equazioni *)
										(* il contenuto delle 2 Row seguenti dipende dal tipo di equazione selezionata dall'utente *)
										Text[Style[eq1, fontSize2]],
										Text[Style[eq2, fontSize2]],
										"\n"
									}],
									(* ELSE: genero altre 2 funzioni a caso *)
									AppendTo[alts, randomFigures[fun1, fun2]];
									(* le inserisco in una lista per poi visualizzarla, per evitare conflitti tra le varie istanze della table *)
             
									Column[{  (* insieme di equazioni *)
										(* il contenuto delle 2 Row seguenti dipende dalle equazioni generate a caso *)
										Text[Style[alts[[i]][[2]], FontSize -> fontSize2]],
										Text[Style[alts[[i]][[4]], FontSize -> fontSize2]],
										"\n"
									}]
								] (* end if *) 
							}], (* end Row *)
							{i, 1, 4} (* variabile su cui iterare *)
						],(* end Table *)
         
						3, (* TIPO 3*)
						Table[ (* genero un numero fissato di risposte di cui una sola \[EGrave] quella giusta *)
							Row[{ (* spazio di una risposta *)
								If[i == rightChoise,
									AppendTo[alts, {fun1, fun2, figura1, figura2}];
									(* THEN: pulsante della risposta ESATTA *)
									Button[ (* contiene nome ed immagine di 2 figure *)
										Row[{
											Column[{
												Row[{fun1}],
													Row[{ figura1}
												]}, Alignment -> Left],
											Column[{
												Row[{fun2}],
												Row[{ figura2}
											]}, Alignment -> Right]
										}],
										SoundGood[]; (* feedback sonoro positivo *)
										showModalMessage[True];
									],
									
									(* ELSE: i \[NotEqual] rightChoice *)
									(* pulsante della risposta ERRATA *)
									ret = randomFigures[fun1, fun2]; (* genero risposte alternative sbagliate *)
					
									altFunz1 = ret[[1]];
									altFunz2 = ret[[3]];
									(* associo le immagini (gi\[AGrave] importate) alle figure generate *)
									Switch[altFunz1, 
										"Line", altFig1 = imageLine;,
										"Parabola", altFig1 = imageParabola;,
										"Circle", altFig1 = imageCircle;,
										"Ellipse", altFig1 = imageEllipse;,
										"Hyperbole", altFig1 = imageHyperbole;,
										_,
									];
									Switch[altFunz2, 
										"Line", altFig2 = imageLine;,
										"Parabola", altFig2 = imageParabola;,
										"Circle", altFig2 = imageCircle;,
										"Ellipse", altFig2 = imageEllipse;,
										"Hyperbole", altFig2 = imageHyperbole;,
										_,
									];
									(* metto le risposte in lista per evitare conflitti tra le varie instaze della table *)
									AppendTo[alts, {altFunz1, altFunz2, altFig1, altFig2}];
						
									Button[
										(* contiene nome ed immagine di 2 figure *)
										Row[{
											Column[{
												Row[{alts[[i]][[1]]}],
												Row[{ alts[[i]][[3]]}
											]}, Alignment -> Left],
											Column[{
												Row[{alts[[i]][[2]]}],
												Row[{ alts[[i]][[4]]}
											]}, Alignment -> Right]
										}],
										SoundBad[]; (* Messaggio di feedback negativo prima di creare un altro esercizio *)
										showModalMessage[False];
									] (* end Button *)
									
								] (* end IF *)
							}], (* end Row *)
							{i, 1, 4} (* variabile su cui iterare *)
						](* end Table *)
				
					] (* end SWITCH *)
				](* end column*) 
			] (* end DYNAMIC *)
		}] (* end BIG ROW *)
	}, Background->White] (* end BIG COLUMN *)
]
End[]; (* Fine spazio privato *)
Protect["Progetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)
