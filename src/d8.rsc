module d8

import IO; 
import String;
import List;
import Set;
import Map;
import Relation;

loc file = |project://aoc2021/inputs/d8|;
list[str] input = readFileLines(file);

list[str] n1 = ["c", "f"];
list[str] n7 = ["a", "c", "f" ];
list[str] n4 = ["b", "c", "d", "f"];
list[str] n2 = ["a", "c", "d", "e", "g"];
list[str] n3 = ["a", "c", "d", "f", "g"];
list[str] n5 = ["a", "b", "d", "f", "g"];
list[str] n6 = ["a", "b", "d", "e", "f", "g"];
list[str] n0 = ["a", "b", "c", "e", "f", "g"];
list[str] n9 = ["a", "b", "c", "d", "f", "g" ];
list[str] n8 = ["a", "b", "c", "d", "e", "f", "g" ];
list[list[str]] ns = [ n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 ];
map[list[str], int] nmap = (n0:0,n1:1,n2:2,n3:3,n4:4,n5:5,n6:6,n7:7,n8:8,n9:9);

rel[str, str] getMapping(mapping, [], []) = mapping;
rel[str, str] getMapping(mapping, [], _) = {};
rel[str, str] getMapping(mapping, _, []) = {};
default rel[str, str] getMapping(rel[str, str] mapping, [cdg, *otherCdgs], nms) {
	fnms = [n | n <- nms, size(n) == size(cdg)];
	for (pm <- permutations(cdg), n <- fnms) {
		new_mapping = zip(pm, n); //new binding
		if (!any(<s1, s2> <- new_mapping, size(mapping[s1]) == 1, {s2} != mapping[s1])) { //no binding conflict
			resMapping = getMapping(mapping + toSet(new_mapping), otherCdgs, nms - [n]);
			if (resMapping != {}) {
				return resMapping;
			}
		}
	}
	return {};
}

//str entry = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
value p2() {
	allOutput = for (entry <- input) {
		if ([ dgts, cd ] := split(" | ", entry)) {
			cdgts = sort([ split("", s) | s <- split(" ", dgts)], bool(a, b) { return size(a) < size(b); });
			println("cdgts <cdgts>");
			mapping = getMapping({}, cdgts, ns);	
			output = [ nmap[sort([ tc  | c <- split("", s), tc <- mapping[c]])] | s <- split(" ", cd)];	
			res = output[0] * 1000 + output[1] * 100 + output[2] * 10 + output[3];
			println("output: <res>");
			append res;
		}
	}	
	return (0 | it + i | i <- allOutput);
}