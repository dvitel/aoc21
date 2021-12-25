module d5

import IO; 
import String;
import List;
import Map;
import Set;
import Relation;
import ParseTree;
import util::Math;

loc file = |project://aoc2021/inputs/d5|;
list[str] input = readFileLines(file);

list[tuple[int, int]] vLineDots() = [ <toInt(x1), y> 
	| line <- input, /<x1:\d+>,<y1:\d+>\s-\>\s<x2:\d+>,<y2:\d+>/ := line, x1 == x2,
	yi1 := toInt(y1), yi2 := toInt(y2), s := min([yi1, yi2]), e := max([yi1, yi2]),
	y <- [s..(e + 1)]];
	
list[tuple[int, int]] hLineDots() = [ <x, toInt(y1)> 
	| line <- input, /<x1:\d+>,<y1:\d+>\s-\>\s<x2:\d+>,<y2:\d+>/ := line, y1 == y2,
	xi1 := toInt(x1), xi2 := toInt(x2), s := min([xi1, xi2]), e := max([xi1, xi2]),
	x <- [s..(e + 1)]];

list[tuple[int, int]] diagLines() = [ <xi1 + xstep * i, yi1 + ystep * i> 
	| line <- input, /<x1:\d+>,<y1:\d+>\s-\>\s<x2:\d+>,<y2:\d+>/ := line, x1 != x2, y1 != y2,
	xi1 := toInt(x1), xi2 := toInt(x2), xstep := (((xi2 - xi1) >= 0) ? 1 : -1),
	yi1 := toInt(y1), yi2 := toInt(y2), ystep := (((yi2 - yi1) >= 0) ? 1 : -1),
	i <- [0..(abs(xi1 - xi2) + 1)] ];
		
value p1() = size({ p | <p, n> <- toList(distribution(hLineDots() + vLineDots())), n > 1 });

value p2() = size({ p | <p, n> <- toList(distribution(hLineDots() + vLineDots() + diagLines())), n > 1 });

void main() {
	println("<p1()>");
}