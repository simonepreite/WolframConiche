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

Begin["Private`"];

(* disabilito alcuni warning *)
Off[Solve::svars]; (* avvisa quando la Solve non riesce a risolvere, nel caso di sistema ad infinite soluzione *)
Off[General::shdw]; (* warning di definizioni oscurate *)
SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)

(*Checks which kind of Shape is defined by the equations parameter*)
(*AAA: We could rewrite this function as a Switch statement, so that we can further refine the recognition*)
recognizeShape[a_, b_, c_]:=( If[b^2 -(4*a*c)<0 && a==c && b==0, "Cerchio", If[b^2-(4*a*c)==0,"Parabola",If[b^2 -(4*a*c)<0 && a!= c && b!=0,"Ellisse",If[b^2 -(4*a*c)>0,"Iperbole", If[b^2 -(4*a*c)>0 && a+c==0, text="Iperbole Equilatera",If[a==b && b==c && c==0, "Retta"]]]]]] );

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

End[]; (* Fine spazio privato *)
Protect["PackageProgetto`*"] (* protegge i nomi del package *)
EndPackage[]; (* Fine del Package *)



