(* ::Package:: *)

printExercise[expr_, text_, values_, answer_]:=(
DynamicModule[{z=0},
	Column[{
	Row[{Text[text]}],
	Row[{ToExpression[expr]} ],
	"",
	Row[{
	RadioButtonBar[z,{1->HoldForm[Evaluate[values[[1]]]],2->HoldForm[Evaluate[values[[2]]]],3->HoldForm[Evaluate[values[[3]]]],4-> HoldForm[Evaluate[values[[4]]]], 5->HoldForm[Evaluate[values[[5]]]]}]
	}],"",
	(*Row[{Evaluate[z\[Equal]answer]}]*)
	}]
	]
);
rFile[filename_]:=(
	exerc=ReadList[filename, String];
	For[i=1, i<=Length[exerc], i++,
		rowl=StringSplit[exerc[[i]],";"];
		val3=StringSplit[rowl[[3]]," "];
		Print[printExercise[rowl[[1]],rowl[[2]],val3,rowl[[4]]]];
	]
);
SetDirectory[NotebookDirectory[]];
rFile["exercises/first.txt"]



