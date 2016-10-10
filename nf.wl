(* ::Package:: *)

(* ::Title:: *)
(*Sensors work*)


(* ::Chapter:: *)
(*Written by Leanne Friedrich at Rush University in April-July 2015*)


(* ::Text:: *)
(*parseFileName collects parameter data from the names of files in the DFT double layer database*)
(*Input: string fileName*)
(*Output: 4x1 vector of numbers {cation valence, cation size in pm, log10 of concentration of cations, surface charge in C/m^2}*)


parseFileName[fileName_]:=Module[{filesplit, valence, size, concCat, sc},
Catch[
	filesplit = StringSplit[fileName, "_"]; (*break up the file name by _ *)
	valence= StringTake[filesplit[[1]], {2}];
	size = StringTake[filesplit[[1]], {5;;7}][[1]];
	concCat = ToExpression[filesplit[[2]]];
	If[concCat>-1, concCat = Log10[concCat]]; (*this adjusts for the file naming system*)
	sc = filesplit[[7]];
Return[ToExpression[{valence, size, concCat, sc}]];
]]


(* ::Text:: *)
(*collectVo collects data from a file for plotting concentration, surface charge, and surface potential*)
(*Input: string fileName*)
(*Output: 3x1 vector of numbers {log10 of cation concentration, surface charge in C/m^2, surface potential}*)


collectVo[file_]:=Module[{vo, valence, size, concCat, sc},
vo = ToExpression[StringSplit[Import[file, {"Lines", 2}], "\t"][[4]]]; (*take in the first line*)
{valence, size, concCat, sc} = parseFileName[file]; (*get parameters from file name*)
Return[{concCat, sc, vo}]
(*to understand the strange phrasing of vo, run these three lines to compare timing
Timing[Import[proFiles[[1]], {"Data",1,4}]]
Timing[Import[proFiles[[1]], "tsv"][[1,4]]]
Timing[ToExpression[StringSplit[Import[proFiles[[1]], {"Lines", 1}], "\t"][[4]]]]*)
]


(* ::Text:: *)
(*interface selectors allows the user to set variables for use with graphMaker*)
(*interfaceSelectors is a panel that defines global variables*)


interfaceSelectors:=Module[{},
	export = d3 = False;
	redRange = noZeros = contour = labels = True;
	minZ = -50; maxZ = 50; contours = 10;
	autoRange = True;
	cf = "ThermometerColors";
	flist = {"V"};
Print[Panel[Column[{
		SetterBar[Dynamic[fileIndex],(#->vFiles[[#]])&/@Range[12]],
		SetterBar[Dynamic[cf],(#->ColorData[#, "Image"])&/@
							{"DarkRainbow", "LightTemperatureMap", "ThermometerColors", "RedBlueTones"}, 
					Appearance->"Vertical"->{Automatic, 4}],
		Panel[Row[{"Reduce range to 0.1 C/m^2 ", Checkbox[Dynamic[redRange]], "\t Eliminate zeros ", Checkbox[Dynamic[noZeros]],  
			"\t Plot contour ",   Checkbox[Dynamic[contour]],  "\t Plot 3D ",   Checkbox[Dynamic[d3]], "\t Labels ",   Checkbox[Dynamic[labels]], 
			  "\t Export ",   Checkbox[Dynamic[export]]}]],
		Panel[TogglerBar[Dynamic[flist], {"V"->"Voltage (\[Rho], \[Sigma]) ", "C"->"Capacitance(\[Rho], \[Sigma])  ", "dCd\[Sigma]"->"dC/d\[Sigma](\[Rho], \[Sigma]) ", "dVd\[Sigma]"->"dV/d\[Sigma](\[Rho], \[Sigma])", "dVd\[Rho]"->"dV/d\[Rho](\[Rho], \[Sigma])  ", 
									"dVd\[Rho]RsL"-> "dV/d\[Rho](\[Rho], \[Sigma]) real log","dVd\[Rho]Rs"-> "dV/d\[Rho](\[Rho], \[Sigma]) real ", "dVd\[Rho]d\[Sigma]"-> " dV/d\[Rho]/d\[Sigma](\[Rho], \[Sigma])  ", "d\[Sigma]dV"->" d\[Sigma]/dV(\[Rho], \[Sigma])  ", "\[CapitalDelta]V"-> " \[CapitalDelta]V(\[Rho], \[Sigma])  ", 
									"\[CapitalDelta]VV"-> "\[CapitalDelta]V/V(\[Rho], \[Sigma]) ", "\[Sigma]"-> "\[Sigma](\[Rho], V) ", "CV"-> "C(\[Rho], V)  ", "dVd\[Sigma]V"-> "dV/d\[Sigma](\[Rho], V)  ", "d\[Sigma]dVV"-> "d\[Sigma]/dV(\[Rho], V)  "
									,"VR"->" V (\[Rho], \[Sigma]) unscaled", "dVd\[Rho]R"-> "dV/d\[Rho](\[Rho], \[Sigma]) real unscaled "
				}, Appearance->"Horizontal"->{Automatic, 4}] (* ADD FUNCTIONS HERE *)
		],
		Panel[Row[{"Auto ", Checkbox[Dynamic[autoRange]],
			Dynamic[If[!autoRange, Row[{"\tMin ", InputField[Dynamic[minZ], FieldSize->10], 
				"\tMax ", InputField[Dynamic[maxZ], FieldSize->10], 
				"\tContours ", InputField[Dynamic[contours], FieldSize->10]}], ""]]
				}]]

}]]];
]


(* ::Text:: *)
(*makeGrids is for bulk grid production*)


makeGrids[redRange_, noZeros_, files_, flist_,contour_,d3_, pr_, contours_, cf_, labels_, export_, autoRange_]:=Module[{graphTable, stringList},
Catch[
graphTable = graphMaker[redRange, noZeros, #, flist,contour,d3, pr, contours, cf, labels, autoRange]&/@files;
If[export,
	Export[StringJoin[ToString[#], ".tiff"], Show[graphTable[[#]], ImageSize->Full, ImageResolution->200]]&/@Range[12]]; 
graphTable = Transpose[graphTable];
stringList = list2String[flist];
Print[Style[stringList[[#]], Large], "\n", Grid[Partition[graphTable[[#]],4]]]&/@Range[Length[graphTable]];
]];


(* ::Text:: *)
(*graphMaker makes graphs for 0 or more functions*)


(* ::Text:: *)
(*to add a new function to be plotted, insert new variables wherever (* ADD FUNCTIONS HERE *) is written. this is in InterfaceSelectors, listTranslate, list2String, and graphMaker*)
(*in graphMaker, you will need to determine which other functions your new functions depends on, and make sure you add that dependency in the lists of If[ifV || ifC], etc.*)
(**)


(* ::Text:: *)
(*listTranslate takes in global variables that are defined in interfaceSelector and then passed into graphMaker*)


listTranslate[list_]:=Module[{ifV,ifC,ifdCd\[Sigma],ifdVd\[Sigma],ifdVd\[Rho],ifdVd\[Rho]RsL,ifdVd\[Rho]Rs,ifdVd\[Rho]d\[Sigma],ifd\[Sigma]dV,if\[CapitalDelta]V,if\[CapitalDelta]VV,if\[Sigma],ifCV,ifdVd\[Sigma]V,ifd\[Sigma]dVV,ifVR, ifdVd\[Rho]R}, (* ADD FUNCTIONS HERE *)
Catch[
	ifV = ifC = ifdCd\[Sigma] = ifdVd\[Sigma] = ifdVd\[Rho] = ifdVd\[Rho]RsL = ifdVd\[Rho]Rs = ifdVd\[Rho]d\[Sigma] = ifd\[Sigma]dV = if\[CapitalDelta]V = if\[CapitalDelta]VV = if\[Sigma] = ifCV = ifdVd\[Sigma]V = ifd\[Sigma]dVV = ifVR = ifdVd\[Rho]R = False; (* ADD FUNCTIONS HERE *)

		If[MemberQ[list, #[[1]]], ReleaseHold[#[[2]]]]&/@		
			{{"V", Hold[ifV= True]},
			{"C", Hold[ifC= True]},
			{"dCd\[Sigma]", Hold[ifdCd\[Sigma]= True]},
			{"dVd\[Sigma]", Hold[ifdVd\[Sigma]= True]},
			{"dVd\[Rho]",Hold[ ifdVd\[Rho]= True] },
			{"dVd\[Rho]RsL",Hold[ ifdVd\[Rho]RsL= True] },
			{"dVd\[Rho]Rs",Hold[ ifdVd\[Rho]Rs= True] },
			{"dVd\[Rho]d\[Sigma]", Hold[ifdVd\[Rho]d\[Sigma]= True] },
			{"d\[Sigma]dV",Hold[ ifd\[Sigma]dV = True]},
			{"\[CapitalDelta]V", Hold[if\[CapitalDelta]V= True] },
			{"\[CapitalDelta]VV", Hold[if\[CapitalDelta]VV = True]},
			{"\[Sigma]", Hold[if\[Sigma]= True] },
			{"CV", Hold[ifCV = True]},
			{"dVd\[Sigma]V", Hold[ifdVd\[Sigma]V= True] },
			{"d\[Sigma]dVV",Hold[ ifd\[Sigma]dVV= True] },
			{"VR", Hold[ifVR= True] },
			{"dVd\[Rho]R",Hold[ ifdVd\[Rho]R= True] }
		}; (* ADD FUNCTIONS HERE *)

	Return[{ifV,ifC,ifdCd\[Sigma],ifdVd\[Sigma],ifdVd\[Rho],ifdVd\[Rho]RsL,ifdVd\[Rho]Rs,ifdVd\[Rho]d\[Sigma],ifd\[Sigma]dV,if\[CapitalDelta]V,if\[CapitalDelta]VV,if\[Sigma],ifCV,ifdVd\[Sigma]V,ifd\[Sigma]dVV, ifVR, ifdVd\[Rho]R}] (* ADD FUNCTIONS HERE *)
]]


(* ::Text:: *)
(*list2String is a graphical aid used in bulk production of graphs*)


list2String[list_]:=Module[{},
Catch[
		Return[Reap[If[MemberQ[list, #[[1]]], Sow[#[[2]]]]&/@		
			{{"V", " Voltage [kT/e]"},
			{"C", " Capacitance"},
			{"dCd\[Sigma]",  " dC/d\[Sigma]"},
			{"dVd\[Sigma]", " dV/d\[Sigma] [kTm^2/Ce]"},
			{"dVd\[Rho]"," dV/d\[Rho] [kT/Me]"},
			{"dVd\[Rho]RsL"," Log(dV/d\[Rho]) [kT/Me]"},
			{"dVd\[Rho]Rs"," dV/d\[Rho] [kT/Me]"},
			{"dVd\[Rho]d\[Sigma]", " dV/d\[Rho]/d\[Sigma] [kTm^2/MeC]"},
			{"d\[Sigma]dV"," d\[Sigma]/dV [kTm^2/Ce]"},
			{"\[CapitalDelta]V", " \[CapitalDelta]Voltage [kT/e]"},
			{"\[CapitalDelta]VV", " \[CapitalDelta]Voltage/Voltage"},
			{"\[Sigma]", " Surface charge [C/m^2]"},
			{"CV", " Capacitance"},
			{"dVd\[Sigma]V", " d\[Sigma]/dV"},
			{"d\[Sigma]dVV"," dV/d\[Sigma]" },
			{"VR", " Voltage [kT/e]" },
			{"dVd\[Rho]R"," dV/d\[Rho] [kT/Me]"}
		}][[2,1]]] (* ADD FUNCTIONS HERE *)
]]


graphMaker[redRange_, noZeros_, file_, flist_, contour_,d3_, pr_, contours_, cf_, labels_, auto_]:=
Module[{ifV,ifC,ifdCd\[Sigma],ifdVd\[Sigma],ifdVd\[Rho],ifdVd\[Rho]R,ifdVd\[Rho]d\[Sigma],ifd\[Sigma]dV,if\[CapitalDelta]V,if\[CapitalDelta]VV,if\[Sigma],ifCV,ifdVd\[Sigma]V,ifd\[Sigma]dVV, functions, fCV, functions2, plots2,ifVR,ifdVd\[Rho]Rs,ifdVd\[Rho]RsL, minV, maxV,
			min\[Sigma], max\[Sigma], Varray, fV, fC,fdCd\[Sigma] ,fdVd\[Sigma],f\[CapitalDelta]V,fd\[Sigma]dV,f\[Sigma],fd\[Sigma]dVV,fdVd\[Sigma]V,plotV, name,globalOptionsContour,plotdVd\[Rho]d\[Sigma], plotdVd\[Rho]d\[Sigma]3d,plotdVd\[Rho]R,
		plotV3d,plotC,plotC3d,plotdCd\[Sigma],plotdCd\[Sigma]3d,plotdVd\[Sigma],plotdVd\[Sigma]3d,plotd\[Sigma]dV,plotd\[Sigma]dV3d,plot\[CapitalDelta]V,plot\[CapitalDelta]V3d,plot\[CapitalDelta]VV,plot\[CapitalDelta]VV3d,fVR,fdVd\[Rho]R,plotdVd\[Rho]3dR, plots3,
		f\[CapitalDelta]VV ,plot\[Sigma],plot\[Sigma]3d,plotCV,plotCV3d,plotdVd\[Sigma]V,plotdVd\[Sigma]V3d,plotd\[Sigma]dVV,plotd\[Sigma]dVV3d, plots,fdVd\[Rho], plotdVd\[Rho], plotdVd\[Rho]3d,  fdVd\[Rho]d\[Sigma], fdVd\[Rho]Rs, fdVd\[Rho]RsL, functions3}, (* ADD FUNCTIONS HERE *)
Catch[
If[pr[[1]]>=pr[[2]],Throw[{"Failed in line 2 of graphMaker. Min must be less than max"}]];
Quiet[
(*------get list of functions-------*)
(*ifV is a bool that indicates if you need function fV*)
	{ifV,ifC,ifdCd\[Sigma],ifdVd\[Sigma],ifdVd\[Rho],ifdVd\[Rho]RsL,ifdVd\[Rho]Rs,ifdVd\[Rho]d\[Sigma],ifd\[Sigma]dV,if\[CapitalDelta]V,if\[CapitalDelta]VV,if\[Sigma],ifCV,ifdVd\[Sigma]V,ifd\[Sigma]dVV, ifVR, ifdVd\[Rho]R} = listTranslate[flist]; (* ADD FUNCTIONS HERE *)
	If[Fold[And, Not/@{ifV,ifC,ifdCd\[Sigma],ifdVd\[Sigma],ifdVd\[Rho],ifdVd\[Rho]RsL,ifdVd\[Rho]Rs,ifdVd\[Rho]d\[Sigma],ifd\[Sigma]dV,if\[CapitalDelta]V,if\[CapitalDelta]VV,if\[Sigma],ifCV,ifdVd\[Sigma]V,ifd\[Sigma]dVV, ifVR, ifdVd\[Rho]R}], Throw[{"Failed in line 4 of graphMaker. No functions requested."}]];
	(*note- this order must match the order that is returned in the listTranslate function. Otherwise, you will get graphs you didn't ask for.*)

(*----set options------*)
	If[redRange, min\[Sigma] = -0.1, min\[Sigma] = -0.5]; (*reduce range to low charge*)
	If[noZeros, max\[Sigma] = -0.005, max\[Sigma] = 0]; (*eliminate zero values of charge*)
	Varray = Import[file, "tsv"]; (*import file*)
	Varray = (Varray/.x_/;StringQ[x]->0); (*fixes import errors from exponents*)
	If[noZeros, Varray = Select[Varray, #[[2]]<0&];]; (*eliminate zero values of charge*)
	name = StringTake[file, 7]; (*get a title for plot labels*)

(*-------calculate functions--------*) (* ADD FUNCTIONS HERE *)

(*--functions3 is in the non-log concentration domain--*)
functions3 = Reap[
	If[ifdVd\[Rho]Rs || ifVR || ifdVd\[Rho]R || ifdVd\[Rho]RsL, 
		fVR = Interpolation[{{10^(#[[1]]), #[[2]]}, #[[3]]}&/@Sort[Varray], InterpolationOrder->3, Method->"Spline"][\[Rho], \[Sigma]];
		If[ifVR, 
			Sow[{fVR, " V [kT/e]"}];
		];
		If[ifdVd\[Rho]R || ifdVd\[Rho]Rs|| ifdVd\[Rho]RsL, 
			fdVd\[Rho]R = D[fVR, \[Rho]];
			If[ifdVd\[Rho]R,
				Sow[{fdVd\[Rho]R, " dV/d\[Rho] [kT/Me]"}];
			];
		];
	];
];

(*---functions: log(concentration) v. surface charge----*)
functions = Reap[
	If[ifV || if\[CapitalDelta]V||if\[CapitalDelta]VV||ifdVd\[Sigma] || ifdVd\[Rho] || ifdVd\[Rho]d\[Sigma] || ifC || ifdCd\[Sigma], 
		fV = Interpolation[{{#[[1]], #[[2]]}, #[[3]]}&/@Sort[Varray], InterpolationOrder->3, Method->"Spline"][\[Rho], \[Sigma]];
		If[ifV, 
			Sow[{fV, " Voltage [kT/e]"}];
		];
		If[ifdVd\[Rho] || ifdVd\[Rho]d\[Sigma],
			fdVd\[Rho] = D[fV, \[Rho]];
			If[ifdVd\[Rho], 
				Sow[{fdVd\[Rho], " dV/d\[Rho] [kT/Me]"}];
			];
			If[ifdVd\[Rho]d\[Sigma],
				fdVd\[Rho]d\[Sigma] = D[fdVd\[Rho], \[Sigma]];
				Sow[{fdVd\[Rho]d\[Sigma]," dV/d\[Rho]/d\[Sigma] [kTm^2/MeC]"}];
			];
		];
		If[if\[CapitalDelta]V||if\[CapitalDelta]VV||ifdVd\[Sigma]||ifd\[Sigma]dV, 
			fdVd\[Sigma] = D[fV, \[Sigma]];
			If[ifdVd\[Sigma], 
				Sow[{fdVd\[Sigma], " dV/d\[Sigma] [kTm^2/Ce]"}];
			];
			If[ifd\[Sigma]dV,
				fd\[Sigma]dV = 1/fdVd\[Sigma];
				Sow[{fd\[Sigma]dV, " d\[Sigma]/dV [kTm^2/Ce]"}];
			];
			If[if\[CapitalDelta]V||if\[CapitalDelta]VV, 
				f\[CapitalDelta]V = -1*\[Sigma]*fdVd\[Sigma];
				If[if\[CapitalDelta]V, Sow[{f\[CapitalDelta]V, " \[CapitalDelta]Voltage [kT/e]"}]];
				If[if\[CapitalDelta]VV,
					f\[CapitalDelta]VV = -f\[CapitalDelta]V/fV;
					Sow[{f\[CapitalDelta]VV, " \[CapitalDelta]Voltage/Voltage"}]];
			];		
		];
		If[ifC || ifdCd\[Sigma], 
			fC = \[Sigma]/fV;
			If[ifC, 
				Sow[{fC, " Capacitance"}];
			];
			If[ifdCd\[Sigma],
				fdCd\[Sigma] = D[fC, \[Sigma]];
				Sow[{fdCd\[Sigma],  " dC/d\[Sigma]"}];
			];
		];
	];	
	If[ifdVd\[Rho]RsL || ifdVd\[Rho]Rs, 
		fdVd\[Rho]Rs = fdVd\[Rho]R /.{\[Rho]->10^\[Rho]};
		If[ifdVd\[Rho]Rs, Sow[{fdVd\[Rho]Rs, " dV/d\[Rho] [kT/Me]"}]];
		If[ifdVd\[Rho]RsL ,
			fdVd\[Rho]RsL = Log10[fdVd\[Rho]Rs];
			Sow[{fdVd\[Rho]RsL," Log(dV/d\[Rho]) [kT/Me]" } ]];
	];
];


(*functions2: log concentration v. V*)
functions2 = Reap[
minV = Min[Varray[[;;,3]]];
maxV = Max[Varray[[;;,3]]];
	If[ifCV, 
		fCV = Interpolation[{{#[[1]], #[[3]]}, #[[2]]/#[[3]]}&/@Sort[Varray], InterpolationOrder->1][\[Rho], V];
		Sow[{fCV," Capacitance"}]];
	If[if\[Sigma] || ifdVd\[Sigma]V||ifd\[Sigma]dVV, 
		f\[Sigma] = Interpolation[{{#[[1]], #[[3]]}, #[[2]]}&/@Sort[Varray], InterpolationOrder->1][\[Rho], V];
		If[if\[Sigma], 
			Sow[{f\[Sigma]," Surface charge [C/m^2]"}]
		];
		If[ifdVd\[Sigma]V||ifd\[Sigma]dVV,
			fd\[Sigma]dVV = D[f\[Sigma], V];
			If[ifd\[Sigma]dVV, 
				Sow[{fd\[Sigma]dVV, " d\[Sigma]/dV"}];
			];
			If[ifdVd\[Sigma]V,
				fdVd\[Sigma]V = 1/fd\[Sigma]dVV;
				Sow[{fdVd\[Sigma]V, " dV/d\[Sigma]"}];
			];
		];
	];	
];




(*------make plots---------*)

If[contour, SetOptions[ContourPlot, {ImageSize->Medium, ExclusionsStyle->Gray}]];
If[d3, SetOptions[Plot3D,{ViewPoint->{-1,1, 4}, ViewVertical->{0,1,0}, ImageSize->Medium}]];
If[auto,
	If[contour, SetOptions[ContourPlot, {PlotRange->Automatic, ContourLabels->All, Contours->10, ColorFunctionScaling->True, ColorFunction->cf}]];
	If[d3, SetOptions[Plot3D, {PlotRange->Automatic, ColorFunction->cf }]];
,
	If[contour, SetOptions[ContourPlot, {PlotRangePadding->Scaled[0.02], PlotRange->pr, ContourLabels->If[labels, All, None], Contours->contours,  ColorFunctionScaling->False}]];
	If[d3, SetOptions[Plot3D, {PlotRange->pr}]];
];

If[Length[functions3[[2]]]>0, 
	functions3 = functions3[[2,1]];
	plots3 = Reap[Do[
		If[contour, 
			If[auto, 
					Sow[ContourPlot[functions3[[i,1]], {\[Rho], 10^-6, 1}, {\[Sigma],min\[Sigma] , max\[Sigma]},PlotLabel->StringJoin[name , functions3[[i, 2]]], 
								 FrameLabel->{"Concentration [M]", "Surface charge [C/m^2]"}]];
					,
					Sow[ContourPlot[functions3[[i,1]], {\[Rho], 10^-6, 1}, {\[Sigma],min\[Sigma] , max\[Sigma]},PlotLabel->StringJoin[name , functions3[[i, 2]]], 
								 FrameLabel->{"Concentration [M]", "Surface charge [C/m^2]"},
							ColorFunction->(ColorData[cf][Rescale[#, pr, {0,1}]]&)]];
			];
		];
		If[d3,   Sow[Plot3D[functions3[[i,1]], {\[Rho], 10^-6, 1}, {\[Sigma],min\[Sigma] , max\[Sigma]},PlotLabel->StringJoin[name , functions3[[i, 2]]], 
								AxesLabel->{"Log(\[Rho])", "\[Sigma]", "V"}]];
		];
	, {i, Length[functions3]}]];
	,
	plots3 = {};
];

If[Length[functions[[2]]]>0, 
	functions = functions[[2,1]];
	plots = Reap[Do[
		If[contour, 
			If[auto, 
					Sow[ContourPlot[functions[[i,1]], {\[Rho], -6, 0}, {\[Sigma],min\[Sigma] , max\[Sigma]},PlotLabel->StringJoin[name , functions[[i, 2]]], 
								 FrameLabel->{"Log(Concentration [M])", "Surface charge [C/m^2]"}]];
					,
					Sow[ContourPlot[functions[[i,1]], {\[Rho], -6, 0}, {\[Sigma],min\[Sigma] , max\[Sigma]},PlotLabel->StringJoin[name , functions[[i, 2]]], 
								 FrameLabel->{"Log(Concentration [M])", "Surface charge [C/m^2]"},
							ColorFunction->(ColorData[cf][Rescale[#, pr, {0,1}]]&)

					]];
			];
		];
		If[d3,   Sow[Plot3D[functions[[i,1]], {\[Rho], -6, 0}, {\[Sigma],min\[Sigma] , max\[Sigma]},PlotLabel->StringJoin[name , functions[[i, 2]]], 
								AxesLabel->{"Log(\[Rho])", "\[Sigma]", "V"}]];
		];
	, {i, Length[functions]}]];
	,
	plots = {};
];

If[Length[functions2[[2]]]>0, 
	functions2 = functions2[[2,1]];
	plots2 = Reap[Do[
		If[contour, 
			If[auto,
					Sow[ContourPlot[functions2[[i,1]], {\[Rho], -6, 0}, {V,minV , maxV},PlotLabel->StringJoin[name , functions2[[i, 2]]], 
								 FrameLabel->{"Log(Concentration [M])", "Voltage"}]];		
					,
					Sow[ContourPlot[functions2[[i,1]], {\[Rho], -6, 0}, {V,minV , maxV},PlotLabel->StringJoin[name , functions2[[i, 2]]], 
								 FrameLabel->{"Log(Concentration [M])", "Voltage"},
							ColorFunction->(ColorData[cf][Rescale[#, pr, {0,1}]]&)]];
			];
		];
		If[d3,   Sow[Plot3D[functions2[[i,1]], {\[Rho], -6, 0}, {V,minV , maxV},PlotLabel->StringJoin[name , functions2[[i, 2]]], 
								AxesLabel->{"Log(\[Rho])","V", "C"}]];
		];
	, {i, Length[functions2]}]];
	,
	plots2 = {};
];];


If[Length[plots]>0, plots = plots[[2,1]]];
If[Length[plots2]>0, plots2 = plots2[[2,1]]];
If[Length[plots3]>0, plots3 = plots3[[2,1]]];
Return[Join[plots, plots2, plots3]];

]]
