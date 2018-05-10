(* ::Package:: *)

BeginPackage["PackageProgetto`"]

Unprotect["PackageProgetto`*"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["PackageProgetto`*"];
(*USAGE*)
(*Here are the names and usage messages for the functions and parameters that will be publicly exported from the package.*)

recognizeShape::usage = "Checks which kind of Shape is defined by the equations parameter";
color::usage = "Assigns a color to the shape";
buildGraphicConicalEquation::usage = "Builds an interactive Panel with the plots of the Conical equation";
buildGraphicPlaneCone::usage = "Builds an interactive Panel with the equation of a Plane and a Cone, highligthing te intersection and plotting it inside another plot";
buildGraphicEccentricity::usage = "Builds an interactive Panel with the equation of a curve base on its eccentricity";
buildAnimation::usage = "Creates the animation of a Line rotating generating a Cone";
printExercise::usage="Prints the text, equation of an exercise, with the possible solutions in a radio button bar and tests if the answer given is correct";
rFile::usage="Print all exercices into a passed file";
NotebookOpeners::usage="Displays different buttons, each one opens a different .nb file";
buildLabeledGraphic::usage="builds an interactive Panel with the equation of a cone and prints a table with the deatils of the components.Undertanding how to limit the rotation of the Plot is important";

Begin["Private`"];

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

(*Checks which kind of Shape is defined by the equations parameter*)
recognizeShape[a_,b_,c_, d_,e_,f_]:=
(If[d^2/4a+e^2/4c-f<0,"Non \[EGrave] una conica!",flag=b^2-(4*a*c);Which[flag==0,If[a==b && b==c && c==0, "Retta","Parabola"] , LessThan[0][flag], If[a==c && b==0,"Circonferenza","Ellisse"], GreaterThan[0][flag], If[a+c==0,"Iperbole Equilatera","Iperbole"]]]);

(*Assigns a color to the shape*)
(*AAA: We could choose better colors by using RGB[]*)
(*AAA: We need to add some words color association here for the graphicPlaneCone*)
color[text_]:=(Switch[text,"Circonferenza",Brown,"Ellisse",Purple,"Iperbole",Blue ,"Parabola", Yellow, "Retta", Black, "Iperbole Equilatera", Blue, "Non \[EGrave] una conica!", Brown]);

(*Builds an interactive Panel with the plots of the Conical equation*)
(*TODO: Trovare un modo di colorare la Grid*)
buildGraphicConicalEquation:=(
DynamicModule[{text, clr},
Manipulate[ 
Grid[{{
Column[{
	text=recognizeShape[a,b,c,d,e,f];
	clr=color[text];
	Row[{
	Text["La funzone sta disegnando: "<>text],Spacer[20], "Equazione: ",(*Outputs the name of shape*)
	With[{a=a, b=b, c=c, d=d, e=e, f=f},HoldForm[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)+f==0]]
	}],
	Row[{
	ContourPlot[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)+f==0, {x,-20, 20}, {y,-20,20},(*2D Plot of the equation*)
	ImageSize->Medium (*Defines the size of the Plot*)
	],Spacer[40],
	ContourPlot3D[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)+f==0, {x,-20, 20}, {y, -20, 20}, {z, -20, 20},(*3D Plot of the equation *)
	ImageSize->Medium, 
	Mesh-> None,(*Defines the properties of the grid in the plot*)
	ContourStyle->clr(*Defines the color of the shape based on the parameters of the equation*)
	]}]
}]},{
Column[{
	Row[{
	Grid[
	{{"Ellisse","Delta < 0", "a = c, b = 0"},
	{"Parabola","Delta = 0"},
	{"Circonferenza","Delta = 0"},
	{"Iperbole","Delta > 0"},
	{"Iperbole Equilatera","Delta > 0", "a + c = 0"},
	{"Retta","Delta = 0", "a = b = c = 0"},
	{"Non \[EGrave] una conica", "Condizione di esistenza non soddisfatta!"}},
	Frame->All,
	Background->{Null,bgColorsTwo[text,clr] }]
	}]
	}]}
}, Alignment->{Left, Right}, Frame->All],
{{a,0, "a"},-10,10,1,  Appearance->"Labeled"}, 
{{b,0, "b"},-10,10,1,  Appearance->"Labeled"},
{{c,0, "c"},-10,10,1,  Appearance->"Labeled"}, 
{{d,0, "d"},-10,10,1,  Appearance->"Labeled"},
{{e,0, "e"},-10,10,1, Appearance->"Labeled"},
{{f,0, "f"},-10,10,1, Appearance->"Labeled"},
ControlPlacement->Top (*Places the Sliders on the top of the Panel*)
]]);

checkFigure[alfa_,beta_, g_]:=(Which[beta==90, "Circonferenza",beta<90 && alfa < beta, "Ellisse",beta<90 && alfa < beta && g==0, "Punto", beta==alfa, "Parabola",beta==alfa && g==0, "Retta",beta<alfa,"Iperbole",beta<alfa && g==0,"Rette Incidenti"]);

aux[namez_, colorz_,xz_]:=(If[StringMatchQ[namez,xz],colorz,Null]);

bgColors[name_,color_ ]:=(aux[name, color,#]&/@{"Ellisse","Parabola", "Circonferenza","Iperbole","Rette Incidenti", "Retta","Punto" });

bgColorsTwo[name_,color_ ]:=(aux[name, color,#]&/@{"Ellisse","Parabola", "Circonferenza","Iperbole","Iperbole Equilatera", "Retta","Non \[EGrave] una conica" });

(*Builds an interactive Panel with the equation of a Plane and a Cone, highligthing te intersection and plotting it inside another plot*)
(*TODO:individuare il tipo di conica che si va a formare*)

buildGraphicPlaneCone:=(
Manipulate[
beta=N[ArcSin[Abs[4*f]/(4*Sqrt[(d^2) + (e^2)+ (f^2)])]/Degree];alfa= 45;
Grid[{{
Row[{Text["L'angolo tra il piano e l'asse del cono, Beta, \[EGrave]: "],beta}]},
{Row[{Text["L'angolo tra l'asse del cono e la generatrice, Alfa, \[EGrave]: "],alfa}]},
{
Dynamic[
Show[
ContourPlot3D[{(d*x)+(e*y)+(f*z)+g==0, (x^2)+(y^2)-(z^2)==0}, {x,-2,2},{y,-2,2},{z,-2,2} , ContourStyle->{Automatic,Opacity[0.8]},
ImageSize->Medium,
Mesh->None,
BoundaryStyle->{{1,2}->{Red,Thick}},
AxesOrigin-> True
],
Graphics3D[{Red,Thick, Tooltip[Line[{{0,0,-2}, {0,0,2}}], "Asse di rotazione"]}]
]
],
Dynamic[ Show[{ContourPlot[-((d*x)+(e*y)+g)/f==Sqrt[((x^2)/1 )+(( y^2)/1)], {x,-2,2},{y,-2,2}, ImageSize->Medium]},{ContourPlot[-((d*x)+(e*y)+g)/f==-Sqrt[((x^2)/1 )+(( y^2)/1)], {x,-2,2},{y,-2,2}, ImageSize->Medium]}]]
}, {
Grid[
	{{"Ellisse","Alfa < Beta < 90\[Degree]"},
	{"Parabola","Alfa = Beta"},
	{"Circonferenza","Beta = 90\[Degree]"},
	{"Iperbole","Beta < 90\[Degree]"},
	{"Rette Incidenti","Beta parallelo all'asse", "il piano passa per il vertice"},
	{"Retta","Beta = Alfa", "Il piano passa per il vertice del cono"},
	{"Punto", "Alfa < Beta <= 90\[Degree]", "Il piano passa per il vertice del cono"}},
	Frame->All,
	Background->{Null, bgColors[checkFigure[alfa,beta,g], color[checkFigure[alfa,beta,g]]]}
]
}
}],
{{d,1,"Coefficiente x Piano:"},0,10,0.1,Appearance->"Labeled"},
{{e,1,"Coefficiente y Piano:"},0,10,0.1,Appearance->"Labeled"},
{{f,1,"Coefficiente z Piano:"},0,10,0.1,Appearance->"Labeled"},
{{g,1,"Profondit\[AGrave] del piano:"},0,10,0.1,Appearance->"Labeled"}
]);

(*Builds an interactive Panel with the equation of a curve base on its eccentricity*)
(*Individuare il tipo di conica che si va a formare*)
buildGraphicEccentricity:=(Manipulate[
Column[{
	Row[{
	Text["Equazione dell'ellisse: "],With[{e=e},HoldForm[((1-e^2)*x^2)+(y^2) -2*(1+e)*x==0]]
	}],Row[{
	Text["Retta generata: "<>Which[e>=1,"Parabola",e<=-1,"Iperbole", -1<e<1,"Ellisse"]],
	}],
	Row[{
	Dynamic[ContourPlot[(1-e^2)*x^2+y^2 -(1+e)*x==0,{x,-10,10},{y,-10,10},ImageSize->Medium, Axes->True]]
	}]
}],
{{e,0,"Eccentricit\[AGrave]"},-3,3,0.000001,Appearance->"Labeled"}
]);

(*Costruisce un grafico interattivo che mostra come, per una conica, sia possibile mantenere costante l'eccentricit\[AGrave] intesa come rapporto di distanze*)
buildGraphicMovablePoint:=(DynamicModule[{a = 1, b = 0, c = 0, delta, F},
    delta = b^2-4 a c;
	F = {0,(1-delta)/4a};  (* Fuoco *)
	Manipulate[
Column[{
		Row[{
			StringForm["P(``,``)",NumberForm[x0,3],NumberForm[x0^2,3]], (* mostro coordinate del punto *)
			StringForm["Distanza dal fuoco, PF: ``",NumberForm[Sqrt[(x0^2-F[[2]])^2 + (x0-F[[1]])^2],3]], (* mostro distanza tra P e F *)
			StringForm["Distanza dalla direttrice, PH: ``",NumberForm[x0^2+(1+delta)/4a,3]],
 (* mostro distanza tra P e la direttrice *)
		Row[{StringForm["Eccentricit\[AGrave],"],HoldForm[PF/PH],StringForm[": "],NumberForm[(Sqrt[(x0^2-F[[2]])^2 + (x0-F[[1]])^2])/(x0^2+(1+delta)/4a)]}]
}, "\n"]
,Row[{
			(* le 2 distanze saranno uguali *)
			Show[ (* mostro nel grafico *)
				Plot[x^2,{x,-2,2},AspectRatio->1, ImageSize->Medium, PlotRange->{-1,2},PlotStyle->{RGBColor[0,0,0.6],Thick}], (* Parabola come equazione di secondo grado *)
				Plot[-(1+delta)/4a,{x,-2,2},AspectRatio->1, ImageSize->Medium, PlotRange->{-1,2},PlotStyle->{Black}], (* disegna la direttrice *)
				Graphics[{
					PointSize[0.02],
					Red,Point[{x0,x0^2}], (* Punto P a partire dall'angolo a *)
					Green,Point[F], (* disegno il fuoco *)
					Dashing[0.02],
					Blue,Line[{F,{x0,x0^2}}], (* linea tra P e F *)
					Yellow,Line[{{x0,-(1+delta)/4a},{x0,x0^2}}], (* linea da P alla direttrice *)
					Text["P",{x0+0.1,x0^2+0.1}],
					Text["F",{0.1,(1-delta)/4a+0.1}],
					Text["direttrice",{1,-(1+delta)/4a-0.1}]
				}] (* end Graphics *)
			]
		}, "\n"]}], (* separatore tra ogni elemento della Row *)
		{{x0,0.,"Muovi P"}, -1.4, 1.4,  0.1}(* coordinata x di P \[EGrave] manipolabile *)
	]
  ]);

(*builds an interactive Panel with the equation of a cone and prints a table with the deatils of the components*)
(*Undertanding how to limit the rotation of the Plot is important*)
buildLabeledGraphic:=(
c1=Graphics3D[{Yellow, Opacity[0.8],Cone[{{0, 0, 5}, {0,0,0}}, 2]}];
c2=Graphics3D[{Yellow,Opacity[0.8], Cone[{{0,0,-5}, {0,0,0}}, 2]}];
c3=Graphics3D[{Black,PointSize->0.05, Tooltip[Point[{0,0,0}],"Vertice del Cono"]}];
l1=Graphics3D[{Red,Thick, Tooltip[Line[{{0,0,-8}, {0,0,8}}], "Asse di rotazione"]}];
l2=Graphics3D[{Blue,Thick, Tooltip[Line[{{2,0,-5}, {-2,0,5}}], "Generatrice del Cono"]}];
l3=Graphics3D[{Blue,Thick, Tooltip[Line[{{0,-2,-5},{0,0,0}, {0,-2,5}}], "Generatrice del Cono"]}];
Show[c1,c2,c3, l1,l2, l3, Axes->True, RotationAction->"Fit", Background->White]
);

(*Creates the animation of a Line rotating generating a Cone, the animation stars stopped and must be started by hand*)
buildAnimation:=(Animate[RevolutionPlot3D[{{t,t},{-t,-t}},{t,0,4 Pi},{b,0,theta}],{theta,0.,2*Pi}, DefaultDuration->20, AnimationRunning->False]);

(*Prints the text, equation of an exercise, with the possible solutions in a radio button bar and tests if the answer given is correct*)
printExercise[expr_, text_, values_, answer_]:=(
DynamicModule[{z=1, txt="Ancora da valutare"},
	Column[{
	Row[{Text[text]}],
	Row[{ToExpression[expr]} ],
	"",
	Row[{
	RadioButtonBar[Dynamic[z],{1->HoldForm[Evaluate[values[[1]]]],2->HoldForm[Evaluate[values[[2]]]],3->HoldForm[Evaluate[values[[3]]]],4-> HoldForm[Evaluate[values[[4]]]], 5->HoldForm[Evaluate[values[[5]]]]}, Appearance->"Vertical"]
	}],
	"",
	Row[{
		Button["Clicca per controllare il risultato",Dynamic[If[Equal[z,ToExpression[answer]], txt="Corretto", txt="Sbagliato!"]]],
		Spacer[20],
		Dynamic[Text["Risultato:"<>txt]] 
	}]
	}]
	]
);

(*Reads the text of the exercises from a file*)
rFile[filename_]:=(
	exerc=ReadList[filename, String];
	For[i=1, i<=Length[exerc], i++,
		rowl=StringSplit[exerc[[i]],";"];
		val3=StringSplit[rowl[[3]]," "];
		Print[printExercise[rowl[[2]],rowl[[1]],val3,rowl[[4]]]]
	]
);

(*Displays different buttons, each one opens a different .nb file*)
(*We could impreove the display of the buttons and put the output of this function into a ChoiceDialog*)
NotebookOpeners[]:=(
bList={};
For[i=1, i<=3, i++,
			(*text=Evaluate[Directory[]<>"/notebooks/app"<>ToString[i]<>".nb"];*)
			Which[i==1,appr="Grafico";img=Import["img/approccioGrafico.png"],i==2,appr="Matematico";img=Import["img/approccioMatematico.png"], i==3,appr="Eccentricit\[AGrave]";img=Import["img/approccioEccentric.png"]];
			bList=Append[bList, DynamicModule[{},Button[img,NotebookOpen[Evaluate[Directory[]<>"/notebooks/app"<>ToString[i]<>".nb"]]]
			(*Row[{Button[img,NotebookOpen[text]]    Button["Approccio "<>appr,NotebookOpen[text]]}]*)
	]]];
	Grid[{
	{bList[[1]],"",""},
	{"",bList[[2]],""},
	{"","",bList[[3]]}
	},
	ItemSize->Fit, Frame->None, Alignment->{Left,Center, Right},Spacings->3
	]
	);

ShowEllisse:=(Manipulate[
		ContourPlot[{y==(b/a)*Sqrt[(a^2)-(x^2)],y==-(b/a)*Sqrt[(a^2)-(x^2)]},{x,-10,10},{y,-10,10},ImageSize->Medium, Axes->True],
		{{a,1,"a"},1,10,1,Appearance->"Labeled"},
		{{b,1,"b"},1,10,1,Appearance->"Labeled"}
	]);
	
ShowCirconferenza:=(
Manipulate[
ContourPlot[{y==Sqrt[(r^2)-(x^2)],y==-Sqrt[(r^2)-(x^2)]},{x,-10,10},{y,-10,10},
	ImageSize->Medium, Axes->True],
{{r,1,"raggio"},1,10,1,Appearance->"Labeled"}
	]);
	
ShowIperbole:=(
Manipulate[
ContourPlot[{y==(b/a)*Sqrt[-(a^2)+(x^2)],y==-(b/a)*Sqrt[-(a^2)+(x^2)]},{x,-10,10},{y,-10,10},
	ImageSize->Medium, Axes->True],
{{a,1,"a"},1,10,1,Appearance->"Labeled"},
{{b,1,"b"},1,10,1,Appearance->"Labeled"}
]);	
	
End[]; (* Fine spazio privato *)
Protect["PackageProgetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)






