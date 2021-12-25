module d10

import IO; 
import String;
import List;
import Set;
import Map;
import Relation;
import ParseTree;
import Exception;
import analysis::grammars::Ambiguity;
		
start syntax Chunk = "(" Chunk* ")"?; // | "[" Chunk* "]"? | "{" Chunk* "}"? | "\<" Chunk* "\>"?;
		
loc file = |project://aoc2021/inputs/d10|;
list[str] input = readFileLines(file);

map[str, int] errors = (")": 3, "]":57, "}": 1197, "\>":25137);

//(((
// IChunk(IChunk, IChunk) or IChunk(IChunk(IChunk))
value p1() {
	int sum = 0;
	input = ["()", "(()", "(())", "((("];
	//list[loc] input = [ |project://aoc2021/inputs/d10t| ];
	for (i <- index(input)) {
		line = input[i];
		try {
			println("Line <i> <line>");
			res = parse(#Chunk, line, |project://aoc2021/inputs/d10t|, allowAmbiguity=true);
			//diagnose(res);
			if (/amb(st) := res) 
			  println("alternatives: <[a.prod | a <- st]>");			
			println("Parsed res");
		} catch ParseError(loc l): {
			//if (l.offset < size(line) && line[l.offset] in errors) {
			println("Line <i> error at <l.offset>");
			//sum += errors[line[l.offset]];
			//}
		}
	}
	return sum;
} 

map[str, str] pairedSymRev = (")":"(", "}":"{", "]":"[", "\>":"\<");

map[str, int] completion = ("(": 1, "[":2, "{": 3, "\<": 4);

value p2() {
	scores = for (i <- index(input)) {
		line = input[i];
		score = 0;
		while (true) {
			//println("Line <i> <line>");
			try {
				res = parse(#Chunk2, line);
				println("Parsed <res>");
				break;
			} catch ParseError(loc l): {
				if (l.offset == size(line)) { //do not know what to try - search for symbol
					foundBracket = "}";
					for (tryBracket <- ["}", "]", ")", "\>"]) {
						possibleLine = line + tryBracket;
						try {
							res = parse(#Chunk, possibleLine);
							foundBracket = tryBracket;
						} catch ParseError(loc l2): {
							if (l2.offset < size(possibleLine) && possibleLine[l2.offset] in errors) {
								continue;
							}
							foundBracket = tryBracket;
							break;
						}
					} 
					println("Line <i> error at end <l.offset> found <foundBracket>");
					score = score * 5 + completion[pairedSymRev[foundBracket]];
					line = line + foundBracket;
					continue;
				} else if (line[l.offset] notin errors) {
					println("Line <i> error at <l.offset> char <line[l.offset]>");
					line = line[l.offset..];
					continue;
				}
				break;
			}
		}
		if (score > 0) append score;
	};	
	scores = sort(scores);
	print("Scores <scores>");
	return scores[size(scores)/2];	
}

