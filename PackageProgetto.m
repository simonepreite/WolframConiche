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
color[text_]:=(Switch[text,"Cerchio",Brown,"Ellisse",Purple,"Iperbole",Blue ,"Parabola", Yellow, "Retta", Black, "Iperbole Equilatera", Blue, "Non \[EGrave] una conica!", Brown]);

(*Builds an interactive Panel with the plots of the Conical equation*)
buildGraphicConicalEquation:=(Manipulate[ 
	Column[{text=recognizeShape[a,b,c,d,e,f];,
	Row[{
	Text["La funzone sta disegnando: "<>text],Spacer[20], "Equazione: ",
	With[{a=a, b=b, c=c, d=d, e=e, f=f},HoldForm[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)+f==0]]
	}],
	Row[{
	ContourPlot[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)+f==0, {x,-20, 20}, {y,-20,20},(*2D Plot of the equation*)
	ImageSize->Medium (*Defines the size of the Plot*)
	],
	ContourPlot3D[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)+f==0, {x,-20, 20}, {y, -20, 20}, {z, -20, 20},(*3D Plot of the equation *)
	ImageSize->Medium, 
	Mesh-> None,(*Defines the properties of the grid in the plot*)
	ContourStyle->color[text](*Defines the color of the shape based on the parameters of the equation*)
	]
	}]	
	}],(*Outputs the name of shape*)
{{a,0, "a"},-10,10,1,  Appearance->"Labeled"}, 
{{b,0, "b"},-10,10,1,  Appearance->"Labeled"},
{{c,0, "c"},-10,10,1,  Appearance->"Labeled"}, 
{{d,0, "d"},-10,10,1,  Appearance->"Labeled"},
{{e,0, "e"},-10,10,1, Appearance->"Labeled"},
{{f,0, "f"},-10,10,1, Appearance->"Labeled"},
ControlPlacement->Top (*Places the Sliders on the left hand side of the Panel*)
]);

(*Builds an interactive Panel with the equation of a Plane and a Cone, highligthing te intersection and plotting it inside another plot*)
(*TODO:individuare il tipo di conica che si va a formare*)
buildGraphicPlaneCone:=(Manipulate[
Dynamic[
ContourPlot3D[{(d*x)+(e*y)+(f*z)+g==0, (x^2)+(y^2)-(z^2)==0 }, {x,-2,2},{y,-2,2},{z,-2,2} , ContourStyle->{Automatic,Opacity[0.8]},
ImageSize->Medium,
Mesh->None,
BoundaryStyle->{2->None,{1,2}->{Red,Thick}}]
]Dynamic[ Show[{ContourPlot[-((d*x)+(e*y)+g)/f==Sqrt[((x^2)/1 )+(( y^2)/1)], {x,-2,2},{y,-2,2}, ImageSize->Medium]},{ContourPlot[-((d*x)+(e*y)+g)/f==-Sqrt[((x^2)/1 )+(( y^2)/1)], {x,-2,2},{y,-2,2}, ImageSize->Medium]}]],
{{d,1,"Coefficiente x Piano:"},1,10,0.1,Appearance->"Labeled"},
{{e,1,"Coefficiente y Piano:"},1,10,0.1,Appearance->"Labeled"},
{{f,1,"Coefficiente z Piano:"},1,10,0.1,Appearance->"Labeled"},
{{g,1,"Profondit\[AGrave] del piano:"},1,10,0.1,Appearance->"Labeled"}
]);

(*Builds an interactive Panel with the equation of a curve base on its eccentricity*)
buildGraphicEccentricity:=(Manipulate[
Column[{
	Row[{
	Text["Equazione dell'ellisse: "],With[{e=e},HoldForm[((1-e^2)*x^2)+(y^2) -2*(1+e)*x==0]]
	}],
	Row[{
	Dynamic[ContourPlot[(1-e^2)*x^2+y^2 -(1+e)*x==0,{x,-10,10},{y,-10,10},ImageSize->Medium, Axes->True]]
	}]
}],
{{e,0,"Eccentricit\[AGrave]"},0,3,0.000001,Appearance->"Labeled"}
]);

(*builds an interactive Panel with the equation of a cone and prints a table with the deatils of the components*)
(*Undertanding how to limit the rotation of the Plot is important*)
buildLabeledGraphic:=(
c1=Graphics3D[{Yellow, Opacity[0.8],Cone[{{0, 0, 5}, {0,0,0}}, 2]}];
c2=Graphics3D[{Yellow,Opacity[0.8], Cone[{{0,0,-5}, {0,0,0}}, 2]}];
l1=Graphics3D[{Red,Thick, Tooltip[Line[{{0,0,-8}, {0,0,8}}], "Asse di rotazione"]}];
l2=Graphics3D[{Blue,Thick, Tooltip[Line[{{2,0,-5}, {-2,0,5}}], "Generatrice del Cono"]}];
l3=Graphics3D[{Blue,Thick, Tooltip[Line[{{0,-2,-5},{0,0,0}, {0,-2,5}}], "Generatrice del Cono"]}];
Show[c1,c2, l1,l2, l3]
);

(*Creates the animation of a Line rotating generating a Cone*)
buildAnimation:=(Animate[RevolutionPlot3D[{{t,t},{-t,-t}},{t,0,2 Pi},{b,0,theta}],{theta,0.,2*Pi}]);

(*Prints the text, equation of an exercise, with the possible solutions in a radio button bar and tests if the answer given is correct*)
(*Needs a default value for the RadioButtonBar, and the interface needs to be improved*)
(*TODO:Non effettua bene la valutazione del risultato*)
printExercise[expr_, text_, values_, answer_]:=(
DynamicModule[{z=10, txt="Ancora da valutare"},
	Column[{
	Row[{Text[text]}],
	Row[{ToExpression[expr]} ],
	"",
	Row[{
	RadioButtonBar[Dynamic[z],{1->HoldForm[Evaluate[values[[1]]]],2->HoldForm[Evaluate[values[[2]]]],3->HoldForm[Evaluate[values[[3]]]],4-> HoldForm[Evaluate[values[[4]]]], 5->HoldForm[Evaluate[values[[5]]]]}, Appearance->"Vertical"]
	}],
	"",
	Row[{Button["Clicca per controllare il risultato", If[z==answer,txt="Giusto",txt="Sbagliato!" ]],Spacer[20],Dynamic[Text["Risultato:"<>txt]] }]
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
For[i=1, i<=3, i++,
		Print[
		DynamicModule[{text=Evaluate["notebooks/app"<>ToString[i]<>".nb"]},
			Row[{Button["Approccio"<>ToString[i],NotebookOpen[text]]}]
	]
		];
	]);
	
End[]; (* Fine spazio privato *)
Protect["PackageProgetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)
