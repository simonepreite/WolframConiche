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
ShowEllisse
ShowCirconferenza
ShowIperbole
Begin["Private`"];

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

(*Checks which kind of Shape is defined by the equations parameter*)
recognizeShape[a_,b_,c_, d_,e_,f_]:=

((*If[d^2/4a+e^2/4c-f<0,"Non \[EGrave] una conica!",*)flag=b^2-(4*a*c);Which[flag==0,If[a==b && b==c && c==0, "Retta","Parabola"] , LessThan[0][flag], If[a==c && b==0,"Circonferenza","Ellisse"], GreaterThan[0][flag], If[a+c==0,"Iperbole Equilatera","Iperbole"]](*]*));
(*Assigns a color to the shape*)
(*AAA: We could choose better colors by using RGB[]*)
(*AAA: We need to add some words color association here for the graphicPlaneCone*)
color[text_]:=(Switch[text,"Circonferenza",Brown,"Ellisse",Purple,"Iperbole",Blue ,"Parabola", Yellow, "Retta", Black, "Iperbole Equilatera", Blue, "Non \[EGrave] una conica!", Brown, "Punto",Pink, "Rette Incidenti",Green]);

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

checkFigure[alfa_,beta_, g_]:=(Which[beta<=90 && alfa < beta && g==0, "Punto",beta<alfa && g==0,"Rette Incidenti",beta==alfa && g==0, "Retta",beta<90 && alfa < beta, "Ellisse", beta==alfa, "Parabola",beta<alfa,"Iperbole",beta==90, "Circonferenza"]);

aux[namez_, colorz_,xz_]:=(If[StringMatchQ[namez,xz],colorz,Null]);

bgColors[name_,color_ ]:=(aux[name, color,#]&/@{"Ellisse","Parabola", "Circonferenza","Iperbole","Rette Incidenti", "Retta","Punto" });

bgColorsTwo[name_,color_ ]:=(aux[name, color,#]&/@{"Ellisse","Parabola", "Circonferenza","Iperbole","Iperbole Equilatera", "Retta","Non \[EGrave] una conica" });

(*Builds an interactive Panel with the equation of a Plane and a Cone, highligthing te intersection and plotting it inside another plot*)
(*TODO:individuare il tipo di conica che si va a formare*)
buildGraphicPlaneCone:=(
Manipulate[
beta=IntegerPart[N[ArcSin[Abs[4*f]/(4*Sqrt[(d^2) + (e^2)+ (f^2)])]/Degree]];alfa= 45;
Grid[{{
Row[{Text["L'angolo tra il piano e l'asse del cono, Beta, \[EGrave]: "],beta}]},
{Row[{Text["L'angolo tra l'asse del cono e la generatrice, Alfa, \[EGrave]: "],alfa}]},
{Row[{Text["La figura disegnata \[EGrave]: "],checkFigure[alfa,beta, g]}]},
{
Dynamic[
Show[
ContourPlot3D[{(d*x)+(e*y)+(f*z)+g==0, ((x^2))+((y^2))-((z^2))==0}, {x,-2,2},{y,-2,2},{z,-2,2} , ContourStyle->{Automatic,Opacity[0.8]},
ImageSize->Medium,
Mesh->None,
BoundaryStyle->{{1,2}->{Red,Thick}},
AxesOrigin-> True
],
Graphics3D[{Red,Thick, Tooltip[Line[{{0,0,-2}, {0,0,2}}], "Asse di rotazione"]}]
]
],
Dynamic[ Show[{ ContourPlot[{-((d*x)+(e*y)+g)/f==Sqrt[((x^2)/1 )+(( y^2)/1)],-((d*x)+(e*y)+g)/f==-Sqrt[((x^2)/1 )+(( y^2)/1)]}, {x,-2,2},{y,-2,2}, ImageSize->Medium, ContourStyle->color[checkFigure[alfa,beta,g]]]}]]
}, {
Grid[
	{{"Ellisse","Alfa < Beta < 90\[Degree]"},
	{"Parabola","Alfa = Beta"},
	{"Circonferenza","Beta = 90\[Degree]"},
	{"Iperbole","Beta < 90\[Degree]"},
	{"Rette Incidenti","Beta parallelo all'asse", "il piano passa per il vertice"},
	{"Retta (due rette coincidenti)","Beta = Alfa", "Il piano passa per il vertice del cono"},
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

eccentricity[x_, e_]:=(
result=-1+4 e^2+2 x-4 e^2 x-x^2+e^2 x^2;
If[result>=0, {{x,Sqrt [result]},{x, -Sqrt [result]}},{} ]
);

checkE[e_]:=(
If [e<-1,"Iperbole",If[e==-1,"Parabola",If[-1<e<0,"Ellisse",If[e==0,"Circonferenza",If[0<e<1,"Ellisse",If[e==1,"Parabola",If[e>1,"Iperbole"]]]]]]]
);

(*c \[EGrave] la x del Fuoco, e \[EGrave] l'eccentricit\[AGrave]*)
buildGraphicEccentricity:=(
DynamicModule[{elems},
Manipulate[
elems=eccentricity[aaa,e];
Column[{
Row[
If[elems!={},
{Text["Eccentricit\[AGrave] "], HoldForm[PF/Pd],Text[": "],EuclideanDistance[{1,0},elems[[1]]]/EuclideanDistance[{2,elems[[1]][[2]]},elems[[1]]],Text["PF: "],EuclideanDistance[{1,0},elems[[1]]], Text["PD: "],EuclideanDistance[{2,elems[[1]][[2]]},elems[[1]]]},{If[e==0,"Non hai disegnato alcuna figura!", "Il punto non appartiene alla figura!"]}
]
],
Row[{
Show[
ContourPlot[(1-e^2)*x^2 + y^2-2*(1-2*e^2)*x + 1-4*e^2==0, {x,-12,12},{y,-6,6}],
Graphics[{ 
Join[
{Red, PointSize->0.025,Point[{1,0}],Text["F",{1.2,0.5}],Black,Line[{{2,-12}, {2,12}}], Text["Direttrice, y=2",{4, 3}] },
If[elems!={},
{Brown, PointSize->0.025,Point[elems[[1]]],Point[elems[[2]]], Text["P",(1+#)&/@elems[[1]]], Text["P",(1+#)&/@elems[[2]]]},
 {}],
If[elems!={},
{Line[{{1,0},elems[[1]]}], Line[{{2,elems[[1]][[2]]},elems[[1]]}],Point[{2,elems[[1]][[2]]}],Text["d",{3,elems[[1]][[2]]}]},{} ]]}
],
 Axes->{True, True},
ImageSize->Medium
]
}],
Row[{
Grid[
	{{"Ellisse","0<|e|<1"},
	{"Parabola","|e|\[Equal]1"},
	{Tooltip["Punto","Circonferenza di raggio 0"],"|e| = 0"},
	{"Iperbole","|e| > 1"}},
	Frame->All,
	Background->{Null, bgColors[checkE[e], color[checkE[e]]]
}]
}]
}],
{{e,0,"Eccentricit\[AGrave]:"},-1.5,1.5,0.1,Appearance->"Labeled"},
{{aaa,0,"Px:"},-12,12,0.1,Appearance->"Labeled"}
]]); 

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



