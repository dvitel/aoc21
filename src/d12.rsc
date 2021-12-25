module d12

import IO;
import List;
import String;
import Set;
import Relation;

str input = "we-NX
ys-px
ys-we
px-end
yq-NX
px-NX
yq-px
qk-yq
pr-NX
wq-EY
pr-oe
wq-pr
ys-end
start-we
ys-start
oe-DW
EY-oe
end-oe
pr-yq
pr-we
wq-start
oe-NX
yq-EY
ys-wq
ys-pr";
rel[str, str] cavesOnesided = { <a[0], a[1]> | l <- split("\n", input), a := split("-", l) };

rel[str, str] allCaves() = cavesOnesided + invert(cavesOnesided);

set[list[str]] buildP(path:[*_, "end"], _, _) = {path};
set[list[str]] buildP(path, {}, _) = {};		
default set[list[str]] buildP([*prevN, lastN], caves, visitedOnce) =
	(lastN == toLowerCase(lastN))
		? ((visitedOnce || (lastN == "start")) 
			? ({} | it + buildP([*prevN, lastN, nextN], caves - rangeR(caves, { lastN }), visitedOnce) | nextN <- caves[lastN])
			: ({} | it + buildP([*prevN, lastN, nextN], caves, true) + buildP([*prevN, lastN, nextN], caves - rangeR(caves, { lastN }), false) | nextN <- caves[lastN]))
		: ({} | it + buildP([*prevN, lastN, nextN], caves, visitedOnce) | nextN <- caves[lastN]);

value p() {
	c = allCaves();
	pths = buildP(["start"], c, false);
	println("<pths>");
	return size(pths);
}
