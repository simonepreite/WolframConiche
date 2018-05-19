(* ::Package:: *)

(* PACKAGE.M
 * Progetto d'esame di Matematica Computazionale + Calcolo Numerico e Software Didattico
 * Corsi di laurea magistrale in Informatica e Matematica
 * Anno accademico 2017/2018
 * 
 * Autori:
 *   Carlo, Simone, Yuan, Aliona, Loredana
 *
 * Versione di sviluppo e testing: Wolfram Mathematica 11.2
 *)

BeginPackage["PackageProgetto`"]

Unprotect["PackageProgetto`*"] (* toglie temporaneamente la protezione per ridefinire le funzioni *)
ClearAll["PackageProgetto`*"];
(*USAGE*)
(*Here are the names and usage messages for the functions and parameters that will be publicly exported from the package.*)

recognizeShape::usage = "Checks which kind of shape is defined by the equations parameter";
color::usage = "Returns a color depending from the shape given in input";
buildGraphicConicalEquation::usage = "Builds an interactive Panel with the plots of the Conical equation";
buildGraphicPlaneCone::usage = "Builds an interactive Panel with the equation of a Plane and a Cone and the plot of their intersection";
buildGraphicEccentricity::usage = "Builds an interactive Panel with the equation of a curve based on its eccentricity";
buildAnimation::usage = "Creates the animation of a Line rotating, which generates a Cone";
printExercise::usage="Prints the text, equation of an exercise, with the possible solutions in a radio button bar and tests if the answer given is correct";
rFile::usage="Print all exercises into a passed file";
NotebookOpeners::usage="Displays different buttons, each one is a link to a different portion of the presentation";
buildLabeledGraphic::usage="builds an interactive Panel with the equation of a cone with Tooltips detailing the components";
ShowEllisse::usage="Display a simple Ellipse plot, with its equation labeled";
ShowCirconferenza::usage="Display a simple Circle Plot,  with its equation labeled";
ShowIperbole::usage="Display a simple Hyperbole Plot, with its equation labeled";
ShowParabola::usage="Display a simple Parabole Plot";
ShowExamples::usage="Display an interactive Panel with the progressive evolution of a numerical example";
ShowButton::usage="Dislplay the buttons of the navigation bar at the bottom of the page";
observationButton::usage="Toggle cell with graphics and observation";

Begin["Private`"];

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

(*
*Checks which kind of Shape is defined by the equations parameter
*@a,@b, @c,@d, @e, @f are the conical equation's parameters
*)
recognizeShape[a_,b_,c_, d_,e_,f_]:=
(flag=b^2-(4*a*c);Which[flag==0,If[a==b && b==c && c==0, "Retta","Parabola"] , LessThan[0][flag], If[a==c && b==0,"Circonferenza","Ellisse"], GreaterThan[0][flag], If[a+c==0,"Iperbole Equilatera","Iperbole"]]);

(*
*Checks which kind of Shape is defined by the intersection of a plane and a cone based on the angles of the intersection
*@alfa is the angle between the cone axis and the plane
*@beta is the angle between the cone generating line and its axis
*@g is the y axis of the plane, when g==0 the plane intersects the cone in its vertex  
*)
checkFigure[alfa_,beta_, g_]:=(Which[beta<=90 && alfa < beta && g==0, "Punto",beta<alfa && g==0,"Rette Incidenti",beta==alfa && g==0, "Retta",beta<90 && alfa < beta, "Ellisse", beta==alfa, "Parabola",beta<alfa,"Iperbole",beta==90, "Circonferenza"]);

(*
*Checks which kind of Shape is defined by its eccentricity
*@e is the eccentricity of the curve
*)
checkE[e_]:=(
If [e<-1,"Iperbole",If[e==-1,"Parabola",If[-1<e<0,"Ellisse",If[e==0,"Circonferenza",If[0<e<1,"Ellisse",If[e==1,"Parabola",If[e>1,"Iperbole"]]]]]]]
);

(*
*Assigns a color to the shape
*@text is the result of a function such as checkFigure or recognizeShape
*)
color[text_]:=(Switch[text,"Circonferenza",RGBColor[1,0.74,0.04](*Brown,*),"Ellisse",RGBColor[1,0.74,0.04](*Purple*),"Iperbole",RGBColor[1,0,0.43](*Blue*) ,"Parabola", RGBColor[0.98,0.34,0.02](*Yellow*), "Retta", RGBColor[0.98,0.34,0.02](*Black*), "Iperbole Equilatera", RGBColor[1,0,0.43](*Blue*), "Non \[EGrave] una conica!", RGBColor[0.98,0.34,0.02](*Brown*), "Punto",RGBColor[1,0.74,0.04](*Pink*), "Rette Incidenti",RGBColor[1,0,0.43](*Green*)]);

(*
*Auxiliary function used in bgColors. 
*@name is the result of a function such as checkFigure or recognizeShape
*@color is a color, resulting from the color function
*@xz is the name of a shape
*)
aux[name_, color_,xz_]:=(If[StringMatchQ[name,xz],color,Null]);

(*
*Creates a list of colors to highlight the Background of a grid
*@name is the result of a function such as checkFigure or recognizeShape
*@color is a color, resulting from the color function
*)
bgColors[name_,color_ ]:=(aux[name, color,#]&/@{"Ellisse","Parabola", "Circonferenza","Iperbole","Rette Incidenti", "Retta","Punto" });

(*
*Creates a list of colors to highlight the Background of a grid
*@name is the result of a function such as checkFigure or recognizeShape
*@color is a color, resulting from the color function
*)
bgColorsTwo[name_,color_ ]:=(aux[name, color,#]&/@{"Ellisse","Parabola", "Circonferenza","Iperbole","Iperbole Equilatera", "Retta","Non \[EGrave] una conica" });

(*
*Returns the points of the equation, calculated from the ordinate and eccentricity
*@x is the ordinate of the points
*@e is the eccentricity of the curve
*)
eccentricity[x_, e_]:=(
result=-1+4 e^2+2 x-4 e^2 x-x^2+e^2 x^2;
If[result>=0, {{x,Sqrt [result]},{x, -Sqrt [result]}},{} ]
);

(*Builds an interactive Panel with the plots of the Conical equation*)
buildGraphicConicalEquation:=(
DynamicModule[{text, clr}, (*text and clr are local variables*)
Manipulate[ (*Creates a panel and automatically deploy the controls described at the end*)
Grid[{{ (*The frame displays a Grid with three rows and one column*)
Column[{
	text=recognizeShape[a,b,c,d,e,f]; (*Checks which shape is defined*)
	clr=color[text]; (*calculates the color of the shape*)
	Row[{
	Text["Equazione: "],(*Outputs the name of shape*)
	With[{a=a, b=b, c=c, d=d, e=e, f=f, xs="x", ys="y"},HoldForm[(a*xs^2) +(2b*xs*ys) +(c*ys^2) +(2d*xs) + (2e*ys)+f==0]] (*Displays the equation*)
	}],
	Row[{Text["  \[CapitalDelta] = b^2-4ac = "<>ToString[Evaluate[b^2-4a*c]]]}],
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
	{"Retta","Delta = 0", "a = b = c = 0"}},
	Frame->All,
	Background->{Null,bgColorsTwo[text,clr] }] (*creates the list of colors for the grid background*)
	}]
	}]}
}, Alignment->{Left, Right}, Frame->All],
{{a,0, "a"},-10,10,1,  Appearance->"Labeled"}, 
{{b,0, "b"},-10,10,1,  Appearance->"Labeled"},
{{c,0, "c"},-10,10,1,  Appearance->"Labeled"}, 
{{d,0, "d"},-10,10,1,  Appearance->"Labeled"},
{{e,0, "e"},-10,10,1, Appearance->"Labeled"},
{{f,0, "f"},-10,10,1, Appearance->"Labeled"},
ControlPlacement->Top, (*Places the Sliders on the top of the Panel*)
LabelStyle->{Medium,Bold, Black}
]]);

(*Builds an interactive Panel with the equation of a Plane and a Cone, highligthing te intersection and plotting it inside another plot*)
buildGraphicPlaneCone:=(
Manipulate[
beta=IntegerPart[N[ArcSin[Abs[4*f]/(4*Sqrt[(d^2) + (e^2)+ (f^2)])]/Degree]]; (*beta is the angle between the cone generating line and its axis*)
alfa= 45; (*alfa is the angle between the cone axis and the plane*)
Grid[{ (*The frame displays a Grid with three rows and one column*)
   {Row[{Text["L'angolo tra il piano e l'asse del cono, \[Beta], \[EGrave]: "],beta,Text["  L'angolo tra l'asse del cono e la generatrice, \[Alpha], \[EGrave]: "],alfa},Frame->All]},
{
Row[{
Dynamic[
Show[
ContourPlot3D[{(d*x)+(e*y)+(f*z)+g==0, ((x^2))+((y^2))-((z^2))==0}, {x,-2,2},{y,-2,2},{z,-2,2} , ContourStyle->{Automatic,Opacity[0.8]}, (*3D Plot of the plane and cone*)
ImageSize->Medium,
Mesh->None,
BoundaryStyle->{{1,2}->{Red,Thick}}, (*Makes the boundary of the equation Red and Thick, so that the Intersection is highlighted*)
AxesOrigin-> True
],
Graphics3D[{Red,Thick, Tooltip[Line[{{0,0,-2}, {0,0,2}}], "Asse di rotazione", TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}]}] (*Cone Axis added at the graphic*)
]
],Spacer[20],Dynamic[ Show[Join[{ 
ContourPlot[{-((d*x)+(e*y)+g)/f==Sqrt[((x^2)/1 )+(( y^2)/1)],-((d*x)+(e*y)+g)/f==-Sqrt[((x^2)/1 )+(( y^2)/1)]}, {x,-2,2},{y,-2,2}, ImageSize->Medium, ContourStyle->color[checkFigure[alfa,beta,g]]]},{
If[g==0 && alfa==beta, {Graphics[Yellow,Point[{0,0}]]},{}]}], PointSize->0.5]] (*NON FUNZIONA*)
}]}, {
Grid[
	{{"Ellisse","\[Alpha] < \[Beta] < 90\[Degree]"},
	{"Parabola","\[Alpha] = \[Beta]"},
	{"Circonferenza","\[Beta]= 90\[Degree]"},
	{"Iperbole","\[Beta] < 90\[Degree]"},
	{"Rette Incidenti","\[Beta] parallelo all'asse", "il piano passa per il vertice"},
	{Tooltip ["Retta","Due Rette Coincidenti",TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}],"\[Beta] = \[Alpha]", "Il piano passa per il vertice del cono"},
	{Tooltip["Punto","Circonferenza di raggio 0"TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}], "\[Alpha] < \[Beta] <= 90\[Degree]", "Il piano passa per il vertice del cono"}},
	Frame->All,
	Background->{Null, bgColors[checkFigure[alfa,beta,g], color[checkFigure[alfa,beta,g]]]} (*generates a list of colors for the grid background*)
]
}
}, Frame->All],
{{d,1,"Coefficiente x Piano:"},0,10,0.1,Appearance->"Labeled"},
{{e,1,"Coefficiente y Piano:"},0,10,0.1,Appearance->"Labeled"},
{{f,1,"Coefficiente z Piano:"},1,10,0.1,Appearance->"Labeled"},
{{g,1,"Profondit\[AGrave] del piano:"},0,10,0.1,Appearance->"Labeled"},
ControlPlacement->Top,
LabelStyle->{Medium,Bold, Black}
]);

(* Builds a interactive panel with the conical equation parametrised on the eccentricity *)
buildGraphicEccentricity[]:=(
DynamicModule[{elems}, (*elems, pf and pd are local*)
Manipulate[
elems=eccentricity[aaa,e]; (*calculates the point in which the equation is satisfied based on the ordinate of the point and the eccentricity*)
Column[{ (*the content is displayed in a column with three rows*)
Row[
If[elems!={}, (*if the equation is solvable depending on x and e, display the distances and the eccentricity, otherwise display a error message*)
{Text["Eccentricit\[AGrave] "], TraditionalForm["PF"/"Pd"],Text[": "],EuclideanDistance[{1,0},elems[[1]]]/EuclideanDistance[{2,elems[[1]][[2]]},elems[[1]]],Text["  PF: "],EuclideanDistance[{1,0},elems[[1]]], Text["  PD: "],EuclideanDistance[{2,elems[[1]][[2]]},elems[[1]]]},{If[e==0,"Non hai disegnato alcuna figura!", "Il punto non appartiene alla figura!"]}
]
],
Row[{
Show[
ContourPlot[(1-e^2)*x^2 + y^2-2*(1-2*e^2)*x + 1-4*e^2==0, {x,-12,12},{y,-6,6}], (*2D plot of the equation*)
Graphics[{ 
Join[
{Red, PointSize->0.025,Point[{1,0}],Text["F",{1.2,0.5}],Black,Line[{{2,-12}, {2,12}}], Text["Direttrice, y=2",{4, 3}] }, (*draw a line and the fire of the conical*)
If[elems!={},
{Brown, PointSize->0.025,Point[elems[[1]]],Point[elems[[2]]], Text["P",(1+#)&/@elems[[1]]], Text["P",(1+#)&/@elems[[2]]]}, (*if the point exists, add them to the graphic*)
 {}],
If[elems!={},
{Line[{{1,0},elems[[1]]}], Line[{{2,elems[[1]][[2]]},elems[[1]]}],Point[{2,elems[[1]][[2]]}],Text["d",{3,elems[[1]][[2]]}]},{} ]]}(*draw lines from the point to other stuff*)
],
 Axes->{True, True},
ImageSize->Medium
]
}],
Row[{
Grid[
	{{"Ellisse","0<|e|<1"},
	{"Parabola","|e|\[Equal]1"},
	{Tooltip["Punto","Circonferenza di raggio 0", TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}],"|e| = 0"},
	{"Iperbole","|e| > 1"}},
	Frame->All,
	Background->{Null, bgColors[checkE[e], color[checkE[e]]] (*creates the background colors for the grid*)
}]
}]
}],
{{e,0,"Eccentricit\[AGrave]:"},-1.5,1.5,0.1,Appearance->"Labeled"},
{{aaa,0,"Px:"},-12,12,0.1,Appearance->"Labeled"},
LabelStyle->{Medium,Bold, Black}
]]);

(*builds an interactive Panel with the equation of a cone and prints a table with the deatils of the components*)
buildLabeledGraphic:=(
c1=Graphics3D[{Yellow, Opacity[0.8],Cone[{{0, 0, 5}, {0,0,0}}, 2]}];
c2=Graphics3D[{Yellow,Opacity[0.8], Cone[{{0,0,-5}, {0,0,0}}, 2]}];
c3=Graphics3D[{Black,PointSize->0.15, Text["Vertice del Cono",{0,0,0}(*, TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}*)]}];
l1=Graphics3D[{Red,Thick, Tooltip[Line[{{0,0,-8}, {0,0,8}}], "Asse di rotazione",TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}]}];
l2=Graphics3D[{Blue,Thick, Tooltip[Line[{{2,0,-5}, {-2,0,5}}], "Generatrice del Cono",TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}]}];
l3=Graphics3D[{Blue,Thick, Tooltip[Line[{{0,-2,-5},{0,0,0}, {0,-2,5}}], "Generatrice del Cono",TooltipStyle->{Background -> LightRed, CellFrame -> 3, FontSize->Medium}]}];
Show[c1,c2,c3, l1,l2, l3, Axes->True, RotationAction->"Fit", Background->White, ImageSize->Medium]
);

(*Creates the animation of a Line rotating generating a Cone, the animation starts paused and must be started by hand*)
buildAnimation:=(Animate[
	RevolutionPlot3D[{{t,t},{-t,-t}},{t,0,4 Pi},{b,0,theta}], (*Revolution plot of a line of 45\[Degree] angle*)
	{{theta,0.5, "Angolo"}, 0.5, 2*Pi}, 
	DefaultDuration->20,
	AnimationRunning->False]
);

(*
*Prints the text and equation of an exercise, with the possible solutions in a radio button bar and tests if the answer given is correct
*@expr is the expression object of the exercise
*@text is the text of the exercise
*@values are the possible values for the radiobutton bar, contained in a list
*@answer is the number reprtesenting the correct answer
*)
printExercise[i_,expr_, text_, values_, answer_, comment_]:=(
Panel[
DynamicModule[{z=1, txt="Ancora da valutare", t=False}, (*z, t and txt are local variables*)
	Dynamic[
	Column[ (*The content is displayed in a column with several rows*)
	Join[{Dynamic[Button[If[t,"Nascondi Esercizio ","Mostra Esercizio "]<>ToString[i], t=!t]]},
	If[t,{
	Row[{Text[text]}],
	If[expr!="",Row[{ToExpression[expr]},""]],
	"",
	Row[{
	RadioButtonBar[Dynamic[z],{1->HoldForm[Evaluate[values[[1]]]],2->HoldForm[Evaluate[values[[2]]]],3->HoldForm[Evaluate[values[[3]]]],4-> HoldForm[Evaluate[values[[4]]]]}, Appearance->"Vertical"]
	}], (*radio button bar provides a closed-choice menu for the answer*)
	"",
	Row[{
		Button["Clicca per controllare il risultato",Dynamic[If[Equal[z,ToExpression[answer]], txt="Corretto", txt="Sbagliato!"]]], (*Button to evaluate the answer*)
		Spacer[20],
		Dynamic[Style[Text["Risultato:"<>txt], FontColor->If[txt=="Corretto", Green, Red]]]
	}],
	Row[{Dynamic[If[txt!="Ancora da valutare",Text["Soluzione: "<>comment],"" ]]}]
	(*Display the solution if the user evaluated its answer*)
	},{}]
	],Frame->True]]]]
);

(*
*Reads the text of the exercises from a file
*@filename is the path of the exercise file
*)
rFile[filename_]:=(
	list={};
	exerc=ReadList[filename, String]; (*get a list of the elements in the specified file*)
	For[i=1, i<=Length[exerc], i++, (*foreach line in the specified file*)
		rowl=StringSplit[exerc[[i]],";"]; (*Split the String when a ";" is encountered*)
		val3=StringSplit[rowl[[3]],"/"]; (*Split the Solutions when a "/" is encountered*)
		list=Join[list,{printExercise[i,rowl[[2]],rowl[[1]],val3,rowl[[4]],rowl[[5]]]}]; (*call to printExercise*)
	];
	Return[list]
);

(*
*Display an example in a column, adding a step of its resolution everytime a button is pressed 
*@list is the content of the example
*@title is the title of the example
*@exp is the expression to plot 
*)
(*
*Display an example in a column, adding a step of its resolution everytime a button is pressed 
*@list is the content of the example
*@title is the title of the example
*)
ShowExamples[list_,title_,text_]:=(
DynamicModule[{i=1, t=False, elems={}, current={}, g=False ,plot}, (*i is a local variable, representing the number of column to display*)
Panel[
Column[{
Dynamic[Button[Style[If[t,"Nascondi ","Mostra "]<>title,Medium,Bold, Black], t=!t]],
Dynamic[If[t, Grid[{ (*the content is displayed in a grid with two rows*)
{Style[title,FontSize->Medium,FontWeight->Bold]},
{Style[text,FontSize->Medium,FontWeight->Bold]},
{Row[{
Dynamic[
Row[{
Column[{If[current!={},Grid[current,Frame->All,Alignment->Center]]}], (*this column displays the first i elements from the list*)
Column[{If[g,Show[plot,Graphics[If[elems!={},elems, {}]], PointSize->0.25]]}]
}]
],"  ",
Dynamic[Column[{ (*This column displays the button if there is more output to be shown, a disabled button otherwise followed by a plot of the expression*)
If[i>=Length[list],Button[Style["Esempio Terminato!",Medium,Bold, Black],Enabled->False],Button[Style["Avanti",Medium,Bold],
Dynamic[
i++;
current=Join[current,{Take[list[[i]],2]}];
If[Length[list[[i]]]==3,
If[!g, g=True;plot=list[[i]][[3]],
elems=Join[elems,list[[i]][[3]]] ]
]],Appearance->"FramedPalette"]]
}]]
}]
}
},
Background->White,Frame->All],""], ""]
}, Alignment->Center, ItemSize->Fit],""
]
]);

(*Displays different buttons, each one opens a different .nb file*)
NotebookOpeners[]:=(
bList={};
For[i=1, i<=3, i++,
			(*text=Evaluate[Directory[]<>"/notebooks/app"<>ToString[i]<>".nb"];*)
			Which[i==1,appr="Grafico";img=Import["img/approccioGrafico.png", ImageSize->1000, Appearance->{None}],
			i==2,appr="Matematico";img=Import["img/approccioMatematico.png", ImageSize->1000, Appearance->{None}], 
			i==3,appr="Eccentric";img=Import["img/approccioEccentric.png", ImageSize->1000, Appearance->{None}]];
			bList=Append[bList, DynamicModule[{},Hyperlink[Button[img],{"slides.nb",appr}]]]
	];
	Grid[{
	{bList[[1]],Style["Approccio Sintetico", FontFamily->"EB Garamond",FontSize->50, RGBColor[0,0,0]]},
	{bList[[2]],Style["Approccio Analitico", FontFamily->"EB Garamond",FontSize->50, RGBColor[0,0,0]]},
	{bList[[3]],Style["Approccio Eccentricit\[AGrave]", FontFamily->"EB Garamond",FontSize->50, RGBColor[0,0,0]]}
	},
	ItemSize->{20,10}, Frame->None, Alignment->{Left,Center, Right},FrameStyle->{RGBColor[1,1, 1]}, Spacings->{20,0}
	]
	);

(*
*Display a simple plot of a ellipse, with controllers to modify the equation parameters
*)	
ShowEllisse[]:=(Manipulate[
	Show[ (*Plots the equations that form the ellipse, adds a PlotLabel with the equations*)
		ContourPlot[{y==(b/a)*Sqrt[(a^2)-(x^2)],y==-(b/a)*Sqrt[(a^2)-(x^2)]},{x,-10,10},{y,-10,10},ImageSize->Medium, Axes->True,ContourLabels->None,
		 PlotLabel->{Style[StandardForm["y"==(b/a)*Sqrt[(a^2)-("x"^2)]],FontColor->Red],Style[StandardForm["y"==-(b/a)*Sqrt[(a^2)-("x"^2)]],FontColor->Blue]}, ContourStyle->{Red, Blue, Thick}]
		],
		{{a,1,"a"},1,10,1,Appearance->"Labeled"},
		{{b,1,"b"},1,10,1,Appearance->"Labeled"},
		LabelStyle->{Medium,Bold, Black}
	]);

(*
*Display a simple plot of a circle, with controllers to modify the radius
*)	
ShowCirconferenza[]:=(
Manipulate[
Show[  (*Plots the equations that form the circle, adds a PlotLabel with the equations*)
ContourPlot[{y==Sqrt[(r^2)-(x^2)], y==-Sqrt[(r^2)-(x^2)]},{x,-10,10},{y,-10,10},ImageSize->Medium, Axes->True, ContourLabels->None,
 PlotLabel->{Style[StandardForm["y"==Sqrt[(r^2)-("x"^2)]],FontColor->Red],Style[StandardForm["y"==-Sqrt[(r^2)-("x"^2)]],FontColor->Blue]}
	, ContourStyle->{Red, Blue, Thick}]
],
{{r,1,"raggio"},1,10,1,Appearance->"Labeled"},
LabelStyle->{Medium,Bold, Black}
	]);

(*
*Display a simple plot of a Hyperbole, with controllers to modify the equation parameters
*)	
ShowIperbole[]:=(
Manipulate[
Show[ (*Plots the equations that form the hyperbole, adds a PlotLabel with the equations*)
ContourPlot[{y==(b/a)*Sqrt[-(a^2)+(x^2)], y==-(b/a)*Sqrt[-(a^2)+(x^2)]},{x,-10,10},{y,-10,10},
	ImageSize->Medium, Axes->True, ContourStyle->{Red, Blue, Thick}],ContourLabels->None,
	PlotLabel->{Style[StandardForm["y"==(b/a)*Sqrt[-(a^2)+("x"^2)]],FontColor->Red],Style[StandardForm["y"==-(b/a)*Sqrt[-(a^2)+("x"^2)]],FontColor->Blue]}
],
{{a,1,"a"},1,10,1,Appearance->"Labeled"},
{{b,1,"b"},1,10,1,Appearance->"Labeled"},
LabelStyle->{Medium,Bold, Black}
]);

(*
*Display a simple plot of a Parabole with the axis parallel to the ordinate
*and controllers to modify the equation parameters
*)	
ShowParabola[]:=(
Manipulate[ (*Plots the equation that form the parabole, adds a PlotLabel with the equation*)
Show[
ContourPlot[x==(a*y^2)+b*y,{x,-10,10},{y,-10,10}, ImageSize->Medium, Axes->True, ContourStyle->{Red, Blue, Thick}],
	PlotLabel->{Style[StandardForm["x">0],FontColor->Red],Style[StandardForm["z"<0\[Section]],FontColor->Blue]}],
{{a,0,"a"},0,10,1,Appearance->"Labeled"},
{{b,0,"b"},0,10,1,Appearance->"Labeled"},
LabelStyle->{Medium,Bold, Black}
]
);

(*
*Creates the buttons placed at the bottom of the presentation's slides
*@prec pointer to the precedent slide
*@next pointer to the next slide
*)

ShowButton[prec_, next_]:=(
forwardButton1 = Hyperlink[Button[Import["img/pulsanti/next-button-green.png"], ImageSize->{80,80}, Appearance->{None}],{"slides.nb",next}];
forwardButton2 = Hyperlink[Button[Import["img/pulsanti/back-button-green.png"], ImageSize->{80,80}, Appearance->{None}],{"slides.nb",prec}];
forwardButton3 = Hyperlink[Button[Import["img/pulsanti/house-outline-green.png"], ImageSize->{80,80}, Appearance->{None}],{"slides.nb","Home"}];
Grid[{{forwardButton3,forwardButton2 forwardButton1}},Alignment->{{Left,Right,Right}}, ItemSize->Fit, Frame->None, FrameStyle->RGBColor[0.94,0.94,0.94], Spacings->{10,3}]
);

observationButton[content_, show_]:=(DynamicModule[{f=False },

Button[
Style[show,FontSize->30,RGBColor[0,0.6,0.14]],
Dynamic[f=!f]]
Dynamic[Grid[
If[f,
content,
 {}]

]]]);

End[]; (* Fine spazio privato *)
Protect["PackageProgetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)









