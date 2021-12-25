module d4

import IO; 
import String;
import List;
import Set;
import Relation;
import ParseTree;

loc file = |project://aoc2021/inputs/d4|;
list[str] input = readFileLines(file);

list[list[int]] parseBoard(list[str] boardLines) = 
	[ [ toInt(numStr) | numStr <- split(" ", line), numStr != ""] | line <- boardLines ];
	
list[list[list[int]]] parseBoards(acc, []) = acc;
list[list[list[int]]] parseBoards(acc, ["", *other]) = parseBoards(acc,other);
list[list[list[int]]] parseBoards(acc, [*other, "", *otherBoards]) = 
	parseBoards([*acc, parseBoard(other)], otherBoards);
default list[list[list[int]]] parseBoards(acc, lastBoard) = [*acc, parseBoard(lastBoard)]; 

tuple[list[list[int]], list[list[int]]] boardToRowsCols(board) = 
	<board, [ [ board[j][i] | j <- index(board[i])] | i <- index(board) ] >;
	
value playBingo(winners, [], processedBoards, boards) = winners;

value playBingo(winners, [n, *ns], [<rows:[*_, [], *_], _>, *pbds], bds) = 
	playBingo([ <n, rows>, *winners ], [n, *ns], pbds, bds);
	
value playBingo(winners, [n, *ns], [<_, cols:[*_, [], *_]>, *pbds], bds) = 
	playBingo([ <n, cols>, *winners ], [n, *ns], pbds, bds);
		
value playBingo(winners, [n, *ns], processedBoards, [<rows, cols>, *boards]) =
	playBingo(winners, [n, *ns], [<[ r - n | r <- rows ], [ c - n | c <- cols ]>, *processedBoards], boards);

default value playBingo(winners, [n, *ns], processedBoards, []) = playBingo(winners, ns, [], processedBoards);
	
value d4() {
	if ([numsStr, *other] := input) {
		nums = [ toInt(numStr) | numStr <- split(",", numsStr) ];
		boards = [ boardToRowsCols(b) | b <- parseBoards([], other) ];
		winners = playBingo([], nums, [], boards);
		for ([ *winnersList ] := winners,  winner <- winnersList)
			println("winner = <winner>\n");
		if ([ <winnerN, winnerBoard>, *_] := winners){	
			println("winner = <sum(concat(winnerBoard))*winnerN>");
		}
	}
	return 0;
}

int main() {
	d4();
	return 0;
}