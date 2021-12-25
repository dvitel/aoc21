module d13

import IO;
import List;
import String;
import Set;
import Relation;

loc file = |project://aoc2021/inputs/d13|;

set[tuple[int,int]] doFold(dots, <"fold along x", xF>) = 
	{ <(x <= xF) ? x : (2*xF - x), y> | <x, y> <- dots, (x <= xF) || (2*xF - x >= 0)};
	
set[tuple[int,int]] doFold(dots, <"fold along y", yF>) = 
	{ <x, (y <= yF) ? y : (2*yF - y)> | <x, y> <- dots, (y <= yF) || (2*yF - y >= 0)};	

value p1() {
	res = {};
	if ([dotS, foldS] := split("\n\n", readFile(file))) {
		set[tuple[int,int]] dots = { <toInt(nS[0]), toInt(nS[1])> | l <- split("\n", dotS), nS := split(",", l)};
		list[tuple[str,int]] folds = [ <nS[0], toInt(nS[1])> | l <- split("\n", foldS), nS := split("=", l)];	
		println("<dots> <folds>");
		res = doFold(dots, folds[0]);
	}		
	return size(res);
}

value p2() {
	res = {};
	if ([dotS, foldS] := split("\n\n", readFile(file))) {
		set[tuple[int,int]] dots = { <toInt(nS[0]), toInt(nS[1])> | l <- split("\n", dotS), nS := split(",", l)};
		list[tuple[str,int]] folds = [ <nS[0], toInt(nS[1])> | l <- split("\n", foldS), nS := split("=", l)];
		res = dots;
		for (f <- folds) {
			res = doFold(res, f);
		}
	}	
	//println("<res>");
	coords = unzip(toList(res));
	mX = max(coords[0]);
	mY = max(coords[1]);
	dr = ("" | it + ("" | it + ((<j, i> in res) ? "*" : " ") | j <- [0..mX+1]) + "\n" | i <- [0..mY+1]);
	println("<dr>");
	return size(res);
}
