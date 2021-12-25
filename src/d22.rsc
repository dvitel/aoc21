module d22

import IO;
import List;
import String;
import Set;
import Relation;
import util::Math;

loc file = |project://aoc2021/inputs/d22|;
list[str] lines = readFileLines(file);

data CUBE = cube(bool on, tuple[tuple[int, int], tuple[int, int], tuple[int, int]] range, list[CUBE] children);

list[CUBE] cubeList = [ cube(s == "on", <<toInt(xs), toInt(xe)>, <toInt(ys), toInt(ye)>, <toInt(zs), toInt(ze)>>, []) | l <- lines, /<s:on|off>\sx=<xs:-?\d+>\.\.<xe:-?\d+>,y=<ys:-?\d+>\.\.<ye:-?\d+>,z=<zs:-?\d+>\.\.<ze:-?\d+>/ := l];

bool isZero(<<x1, x2>,<y1, y2>,<z1, z2>>) = x1 > x2 || y1 > y2 || z1 > z2;
bool isZero(cube(_, r, _)) = isZero(r);

tuple[tuple[int,int],tuple[int,int],tuple[int,int]] intersect(<x1, y1, z1>, <x2, y2, z2>) = <intersect(x1,x2), intersect(y1, y2), intersect(z1, z2)>;	
tuple[int,int] intersect(<a1, b1>, <a2, b2>) = <max([a1, a2]), min([b1, b2])>;
CUBE intersect(cube(s, r1, ch), r) = cube(s, intersect(r1, r), [ i | c <- ch, i := intersect(c, r), !isZero(i)]);
	
CUBE negate(cube(s, r, ch)) = cube(!s, r, [ negate(c) | c <- ch]);
bool isIn(CUBE a, CUBE b) = intersect(a, b.range).range == a.range; //checks if a in b

CUBE combine(root:cube(s, r, cs), c1:cube(s, r1, ch1)) { //same status s, invariant: c1 should be inside root on call   
	newCh = [ isZero(i) ? c : combine(c, i) | c <- cs, i := intersect(cube(s, r1, []), c.range), i.range != c.range ];
	return combine(cube(s, r, newCh), ch1);
}
CUBE combine(root:cube(s, r, cs), c1:cube(s1, r1, ch1)) { //s != s1 here and cs have same status as c1
	newCh = [ isZero(i) ? c : combine(c, i) | c <- cs, i := intersect(cube(s, r1, []), c.range), i.range != c.range];
	return cube(s, r, newCh + c1);
}
CUBE combine(c, [c1, *other]) = combine(combine(c, c1), other);
CUBE combine(c, []) = c;
	 	
int count(<<x1,x2>,<y1,y2>,<z1,z2>>) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1);	 	
int count(cube(s, r, ch)) = (s ? 1 : -1) * count(r) + count(ch);
int count([*ch]) = (0 | it + count(c) | c <- ch);
value p1() {
	root = cube(false, <<-50,50>,<-50,50>,<-50,50>>, []);
	cb = combine(root, [c | c <- cubeList, isIn(c, root)]);
	c = count(cb.children);
	println("<cb>");
	return c;
}

value p2() {
	x = (<0, 0> | <min([it[0], c.range[0][0]]), max([it[1], c.range[0][1]])> | c <- cubeList);
	y = (<0, 0> | <min([it[0], c.range[1][0]]), max([it[1], c.range[1][1]])> | c <- cubeList);
	z = (<0, 0> | <min([it[0], c.range[2][0]]), max([it[1], c.range[2][1]])> | c <- cubeList);
	println("<x> <y> <z>");
	root = cube(false, <x,y,z>, []);
	cb = combine(root, [c | c <- cubeList, isIn(c, root)]);
	c = count(cb.children);
	println("<cb>");
	return c;
}

