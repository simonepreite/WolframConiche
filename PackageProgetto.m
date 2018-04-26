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
Begin["Private`"];

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

(*Checks which kind of Shape is defined by the equations parameter*)
recognizeShape[a_,b_,c_]:=(
	flag=b^2-(4*a*c);
	Which[flag==0,If[a==b && b==c && c==0, "Retta","Parabola"] ,
		LessThan[0][flag], If[a==c && b==0,"Circonferenza","Ellisse"], 
		GreaterThan[0][flag], If[a+c==0,"Iperbole Equilatera","Iperbole"]
		]);

(*Assigns a color to the shape*)
(*AAA: We could choose better colors by using RGB[]*)
color[text_]:=(Switch[text,"Cerchio",Brown,"Ellisse",Purple,"Iperbole",Blue ,"Parabola", Yellow, "Retta", Black, "Iperbole Equilatera", Blue]);

(*Builds an interactive Panel with the plots of the Conical equation*)
buildGraphicConicalEquation:=(
Manipulate[ 
	Dynamic[ 
		text=recognizeShape[a,b,c];
		ContourPlot[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)==0, {x,-20, 20}, {y,-20,20},(*2D Plot of the equation*)
			ImageSize->Medium (*Defines the size of the Plot*)
		]
		ContourPlot3D[(a*x^2) +(2b*x*y) +(c*y^2) +(2d*x) + (2e*y)==0, {x,-20, 20}, {y, -20, 20}, {z, -20, 20},(*3D Plot of the equation *)
			ImageSize->Medium, 
			Mesh-> None,(*Defines the properties of the grid in the plot*)
			ContourStyle->color[text](*Defines the color of the shape based on the parameters of the equation*)
		]
		Text["La funzone sta disegnando:"<>text](*Outputs the name of shape*)
	],
{{a,0, "a"},-10,10,1,Appearance->"Labeled"}, 
{{b,0, "b"},-10,10,1,Appearance->"Labeled"},
{{c,0, "c"},-10,10,1,Appearance->"Labeled"}, 
{{d,0, "d"},-10,10,1,Appearance->"Labeled"},
{{e,0, "e"},-10,10,1,Appearance->"Labeled"},
ControlPlacement->Left (*Places the Sliders on the left hand side of the Panel*)
]);

(*Builds an interactive Panel with the equation of a Plane and a Cone, highligthing te intersection and plotting it inside another plot*)
buildGraphicPlaneCone:=(
Manipulate[
	Dynamic[
		ContourPlot3D[{(d*x)+(e*y)+(f*z)+g==0, ((x^2)/1 )+(( y^2)/1)-((z^2)/1)==0 }, {x,-2,2},{y,-2,2},{z,-2,2} , ContourStyle->{Automatic,Opacity[0.8]},
		ImageSize->Medium,
		Mesh->None,
		BoundaryStyle->{2->None,{1,2}->{Red,Thick}}]
	]
Dynamic[
	Show[
	{ContourPlot[-((d*x)+(e*y)+g)/f==Sqrt[((x^2)/1 )+(( y^2)/1)], {x,-2,2},{y,-2,2}, ImageSize->Medium]},
	{ContourPlot[-((d*x)+(e*y)+g)/f==-Sqrt[((x^2)/1 )+(( y^2)/1)], {x,-2,2},{y,-2,2}, ImageSize->Medium]}
	]
	],
{{d,1,"Coefficiente x Piano:"},1,10,0.1,Appearance->"Labeled"},
{{e,1,"Coefficiente y Piano:"},1,10,0.1,Appearance->"Labeled"},
{{f,1,"Coefficiente z Piano:"},1,10,0.1,Appearance->"Labeled"},
{{g,1,"Profondit\[AGrave] del piano:"},1,10,0.1,Appearance->"Labeled"}
]);

(*Builds an interactive Panel with the equation of a curve base on its eccentricity*)
buildGraphicEccentricity:=(
	Manipulate[
		Dynamic[
			ContourPlot[(1-e^2)*x^2+y^2 -2*(1+e)*x==0,{x,-10,20},{y,-10,20},ImageSize->Medium],
			]
	Text[((1-e^2)*x^2)+(y^2) -2*(1+e)*x==0],
	{{e,0,"eccentricit\[AGrave]"},0,3,0.1,Appearance->"Labeled"}
	]);
	
(*Creates the animation of a Line rotating generating a Cone*)
buildAnimation:=(Animate[RevolutionPlot3D[{{t,t},{-t,-t}},{t,0,2 Pi},{b,0,theta}],{theta,0.,2*Pi}]);

(*Prints the text, equation of an exercise, with the possible solutions in a radio button bar and tests if the answer given is correct*)
printExercise[expr_, text_, values_, answer_ ]:=(
Module[{z},
	Column[{
	Row[{Text[text]}],
	Row[{ToExpression[expr]} ],
	"",
	Row[{
	RadioButtonBar[Dynamic[z],{1->HoldForm[Evaluate[values[[1]]]],2->HoldForm[Evaluate[values[[2]]]],3->HoldForm[Evaluate[values[[3]]]],4-> HoldForm[Evaluate[values[[4]]]], 5->HoldForm[Evaluate[values[[5]]]]}]
	}],"",
	Row[{Dynamic[If[z==answer,True, False]]}]
	}]
	]
);
(* Open exercices file, parse row by row the exercices and call to printExercise to show it in the slideshow *)
rFile[filename_]:=(
	exerc=ReadList[filename, String];
	Print[exerc]
	Length[exerc]
	For[i=1, i<=Length[exerc], i++, 
		Print[exerc[[i]]]
		(*rowl=3 StringSplit[exerc[[i]],";"]*)
		
		ciao=5
		For[j=1, j<=ciao,j++, Print[j]]
	]
);

rFile["exercices/first.txt"]

End[]; (* Fine spazio privato *)
Protect["PackageProgetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)



