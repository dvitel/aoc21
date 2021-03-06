module d18

import IO;
import List;
import String;
import Set;
import Relation;

list[value] input1 = 
[
<<<<2,5>,4>,<<1,0>,<8,3>>>,<<2,<2,4>>,<1,<3,3>>>>,
<<<2,2>,<<4,3>,3>>,<<<8,6>,3>,<3,7>>>,
<<<9,<4,1>>,<9,0>>,<6,<6,0>>>,
<<<3,9>,<<4,4>,<2,5>>>,<<9,<8,4>>,8>>,
<<<<0,0>,9>,<<9,3>,<8,2>>>,<2,<1,3>>>,
<<<8,4>,6>,<<5,1>,<3,6>>>,
<<<6,<7,6>>,<<2,6>,5>>,<<6,4>,2>>,
<<1,<9,7>>,<<<5,9>,<9,5>>,<<7,0>,1>>>,
<<<<5,8>,<9,4>>,<<9,3>,<7,8>>>,8>,
<<<0,9>,<<6,0>,7>>,<<<7,7>,6>,<<9,7>,<0,4>>>>,
<<<<4,3>,<9,5>>,<7,<7,3>>>,<<<2,8>,9>,4>>,
<<7,5>,<8,1>>,
<<4,6>,<<<0,6>,6>,<7,4>>>,
<<<1,8>,<<1,4>,<1,6>>>,<3,4>>,
<<<6,5>,<4,<7,3>>>,<<<0,1>,<8,4>>,<4,8>>>,
<<5,1>,<9,<9,<3,3>>>>,
<<<<7,0>,<2,5>>,1>,<9,<<2,7>,<4,4>>>>,
<<<<5,8>,8>,0>,<8,<1,<2,5>>>>,
<8,<<5,4>,7>>,
<<<9,8>,<6,7>>,<<2,<2,6>>,<9,6>>>,
<<<<2,3>,7>,6>,<<8,6>,3>>,
<<<8,<7,2>>,3>,<<<3,9>,4>,<6,8>>>,
<9,<<<6,7>,<6,0>>,<<3,9>,8>>>,
<<<7,7>,<4,7>>,<<<9,8>,9>,<9,<2,4>>>>,
<<<<5,0>,1>,<4,<4,8>>>,<9,<6,7>>>,
<<<<9,2>,5>,<1,<5,8>>>,<<9,<0,1>>,<3,8>>>,
<<<5,<2,5>>,8>,<2,<0,<9,3>>>>,
<<7,<<8,4>,<8,4>>>,4>,
<<<<3,3>,4>,<<0,0>,<5,5>>>,<4,5>>,
<<<<9,3>,<9,3>>,2>,<5,3>>,
<<<9,5>,<1,4>>,<<7,1>,<3,<6,5>>>>,
<8,<<<1,1>,<0,1>>,<9,<3,6>>>>,
<<<<4,4>,7>,<0,3>>,<1,5>>,
<<<3,<0,8>>,8>,<5,<7,5>>>,
<<<<9,6>,2>,7>,<<5,<3,7>>,0>>,
<4,9>,
<<<5,<1,3>>,<<9,5>,6>>,<<<7,9>,5>,3>>,
<<<<3,9>,<7,2>>,<5,<8,8>>>,<1,9>>,
<<<<7,8>,8>,<<9,0>,<5,1>>>,<6,<<1,0>,<3,3>>>>,
<<<<5,8>,1>,<<8,6>,<2,9>>>,<<5,1>,6>>,
<<1,7>,<<5,<3,2>>,4>>,
<<<<3,1>,2>,<0,8>>,<3,<4,6>>>,
<<9,6>,<0,<<5,2>,<1,1>>>>,
<<<<1,8>,8>,<<9,0>,3>>,<<6,<2,8>>,<<6,4>,<6,0>>>>,
<<7,<<3,2>,<9,0>>>,<<<3,2>,<2,8>>,<<5,5>,<9,2>>>>,
<<<<2,5>,<3,1>>,<7,<9,6>>>,<<<7,0>,7>,<2,<9,1>>>>,
<<<<1,6>,9>,<1,<6,5>>>,<<8,<4,1>>,6>>,
<<<7,<4,6>>,<<2,7>,<6,6>>>,<8,0>>,
<<9,7>,<<<0,7>,5>,<<1,4>,<1,3>>>>,
<<<1,<8,2>>,<<0,6>,<9,0>>>,8>,
<<<4,0>,<7,<3,3>>>,<9,6>>,
<0,<<<6,9>,7>,<<0,6>,1>>>,
<5,<<4,3>,<<8,3>,<5,7>>>>,
<<9,0>,<0,<<7,8>,<1,8>>>>,
<<<<4,3>,<5,6>>,2>,<<2,3>,1>>,
<4,<<9,9>,<<1,8>,<9,2>>>>,
<<<<6,9>,5>,1>,<<<7,4>,<8,1>>,3>>,
<<8,<5,<2,6>>>,<<<2,7>,6>,<6,0>>>,
<<<<6,8>,8>,6>,<<<5,7>,2>,<<6,5>,<3,0>>>>,
<<<1,<2,5>>,3>,<5,<4,<6,6>>>>,
<<<<4,9>,8>,1>,<9,0>>,
<<1,<0,<5,7>>>,<<1,<5,9>>,<<3,2>,<1,7>>>>,
<<<<2,9>,<2,7>>,<<4,2>,5>>,<<<9,1>,<7,2>>,<2,<7,5>>>>,
<<<<5,7>,<8,9>>,<5,<7,9>>>,<<7,<6,6>>,<7,<8,0>>>>,
<<<<6,6>,<4,6>>,<4,<7,8>>>,<1,<<5,5>,<1,9>>>>,
<<<<4,3>,8>,2>,<<9,<4,0>>,<8,<7,0>>>>,
<<2,<7,5>>,<<<0,1>,1>,<8,<3,5>>>>,
<<<4,<4,2>>,<<0,4>,9>>,<1,4>>,
<<<5,5>,<5,6>>,<<0,<4,2>>,<<7,8>,<5,6>>>>,
<2,<<0,<9,1>>,<<1,7>,<0,0>>>>,
<<<5,<4,8>>,1>,9>,
<8,<<2,1>,<3,0>>>,
<<<<6,5>,<1,1>>,7>,<<<7,5>,3>,<0,1>>>,
<<<<0,3>,7>,7>,<<<4,8>,<6,1>>,<<6,1>,9>>>,
<<<<4,8>,9>,<1,0>>,<6,<4,<4,8>>>>,
<<<<8,0>,<5,1>>,6>,1>,
<<<<6,6>,<7,7>>,<<4,3>,<2,6>>>,<<3,5>,<<7,0>,<7,3>>>>,
<<1,<5,8>>,<<<3,7>,<9,6>>,<<4,8>,<3,4>>>>,
<<<1,5>,<8,2>>,<<<3,1>,5>,<4,1>>>,
<<<<6,3>,5>,8>,<<9,<3,6>>,<<3,5>,<6,9>>>>,
<<<7,<5,4>>,<0,<6,0>>>,<<<7,7>,<1,1>>,<<5,1>,7>>>,
<<<1,5>,<<8,6>,0>>,5>,
<<<<0,8>,<6,0>>,<<3,0>,9>>,<<<7,1>,2>,<4,2>>>,
<<<6,<8,7>>,<2,<2,0>>>,<9,<7,<6,6>>>>,
<3,<<7,<4,5>>,<<8,5>,4>>>,
<<<<8,0>,<8,3>>,<<5,4>,<1,6>>>,<<0,<8,5>>,3>>,
<<<7,2>,1>,<9,<<3,8>,4>>>,
<<4,<7,<9,9>>>,<3,8>>,
<<<<7,1>,9>,<<6,9>,<9,6>>>,<2,0>>,
<<<<6,2>,9>,<3,<3,9>>>,<<8,<3,4>>,<3,7>>>,
<<4,9>,<8,<5,<9,8>>>>,
<3,<<9,<9,7>>,4>>,
<<<<5,9>,6>,<1,<3,1>>>,<4,<1,<3,8>>>>,
<<<<7,6>,2>,3>,<<0,<1,8>>,<<4,9>,<4,3>>>>,
<<3,<<8,1>,<3,8>>>,<<<2,0>,<0,8>>,<<7,0>,9>>>,
<<<<9,7>,<9,3>>,<<5,8>,6>>,<<<6,2>,0>,<2,4>>>,
<<<8,<9,7>>,<<5,1>,<1,4>>>,3>,
<<7,<<5,6>,<2,7>>>,<<<7,3>,0>,<1,<0,6>>>>,
<<2,<<5,5>,2>>,<<3,<7,2>>,<<7,1>,8>>>,
<<<<2,4>,<6,8>>,<0,<7,5>>>,<<3,<2,5>>,<7,7>>>
];

//<<<<5,9>,<16,0>>,<<11,0>,<<3,4>,2>>>,<<<5,<2,8>>,4>,<5,<<9,9>,0>>>>
//<p1,<<<p4,<2,8>>,s3>,s2>>
//<<<<5,9>,<16,0>>,<<11,0>,<<3,4>,2>>>,<<<7,0>,12>,<5,<<9,9>,0>>>>
//<<p2,<p3,<<3,4>,s4>>>,s1>

value ex = [
<<<0,<5,8>>,<<1,7>,<9,6>>>,<<4,<1,2>>,<<1,4>,2>>>,
<<<5,<2,8>>,4>,<5,<<9,9>,0>>>,
<6,<<<6,2>,<5,6>>,<<7,6>,<4,7>>>>,
<<<6,<0,7>>,<0,9>>,<4,<9,<9,0>>>>,
<<<7,<6,4>>,<3,<1,3>>>,<<<5,5>,1>,9>>,
<<6,<<7,3>,<3,2>>>,<<<3,8>,<5,7>>,4>>,
<<<<5,4>,<7,7>>,8>,<<8,3>,8>>,
<<9,3>,<<9,9>,<6,<4,9>>>>,
<<2,<<7,7>,7>>,<<5,8>,<<9,3>,<0,2>>>>,
<<<<5,2>,5>,<8,<3,7>>>,<<5,<7,5>>,<4,4>>>
];

value ex2 = [
<<<0,<4,5>>,<0,0>>,<<<4,5>,<2,6>>,<9,5>>>,
<7,<<<3,7>,<4,3>>,<<6,3>,<8,8>>>>,
<<2,<<0,8>,<3,4>>>,<<<6,7>,1>,<7,<1,6>>>>,
<<<<2,4>,7>,<6,<0,5>>>,<<<6,8>,<2,8>>,<<2,1>,<4,5>>>>,
<7,<5,<<3,8>,<1,4>>>>,
<<2,<2,2>>,<8,<8,1>>>,
<2,9>,
<1,<<<9,3>,9>,<<9,0>,<0,7>>>>,
<<<5,<7,4>>,7>,1>,
<<<<4,2>,2>,6>,<8,7>>
];

value addLists(acc, []) = acc;
value addLists([], [l1, *other]) = addLists(l1, other);
value addLists(acc, [l1, *other]) = addLists(reduceList(<acc, l1>), other);

value add(n, <n1, n2>) = <add(n, n1), n2>;
value add(<n1, n2>, n) = <n1, add(n2, n)>;
value add(n1, n2) = n1 + n2;

value reduceList(l) { println("Reducing <l>"); fail; }
 
value reduceList(<<<<<l, r>, s4>, s3>, s2>, s1>) = reduceList(<<<<0, add(r, s4)>, s3>, s2>, s1>);
value reduceList(<<<<p4, <l, r>>, s3>, s2>, s1>) = reduceList(<<<<add(p4, l), 0>, add(r, s3)>, s2>, s1>);

value reduceList(<<<p3, <<l, r>, s4>>, s2>, s1>) = reduceList(<<<add(p3, l), <0, add(r, s4)>>, s2>, s1>);
value reduceList(<<<p3, <p4, <l, r>>>, s2>, s1>) = reduceList(<<<p3, <add(p4, l), 0>>, add(r, s2)>, s1>);

value reduceList(<<p2, <<<l, r>, s4>, s3>>, s1>) = reduceList(<<add(p2, l), <<0, add(r, s4)>, s3>>, s1>);
value reduceList(<<p2, <<p4, <l, r>>, s3>>, s1>) = reduceList(<<p2, <<add(p4, l), 0>, add(r, s3)>>, s1>);
value reduceList(<<p2, <p3, <<l, r>, s4>>>, s1>) = reduceList(<<p2, <add(p3, l), <0, add(r, s4)>>>, s1>);
value reduceList(<<p2, <p3, <p4, <l, r>>>>, s1>) = reduceList(<<p2, <p3, <add(p4, l), 0>>>, add(r, s1)>);

value reduceList(<p1, <<<<l, r>, s4>, s3>, s2>>) = reduceList(<add(p1, l), <<<0, add(r, s4)>, s3>, s2>>);
value reduceList(<p1, <<<p4, <l, r>>, s3>, s2>>) = reduceList(<p1, <<<add(p4, l), 0>, add(r, s3)>, s2>>);
value reduceList(<p1, <<p3, <<l, r>, s4>>, s2>>) = reduceList(<p1, <<add(p3, l), <0, add(r, s4)>>, s2>>);
value reduceList(<p1, <<p3, <p4, <l, r>>>, s2>>) = reduceList(<p1, <<p3, <add(p4, l), 0>>, add(r, s2)>>);
value reduceList(<p1, <p2, <<<l, r>, s4>, s3>>>) = reduceList(<p1, <add(p2, l), <<0, add(r, s4)>, s3>>>);
value reduceList(<p1, <p2, <<p4, <l, r>>, s3>>>) = reduceList(<p1, <p2, <<add(p4, l), 0>, add(r, s3)>>>);
value reduceList(<p1, <p2, <p3, <<l, r>, s4>>>>) = reduceList(<p1, <p2, <add(p3, l), <0, add(r, s4)>>>>);
value reduceList(<p1, <p2, <p3, <p4, <l, r>>>>>) = reduceList(<p1, <p2, <p3, <add(p4, l), 0>>>>);

tuple[bool, value] split(<l, r>) {
	<lsplitted, ln> = split(l);
	if (lsplitted) return <true, <ln, r>>;
	<rsplitted, rn> = split(r);
	return <rsplitted, <l, rn>>;
}
tuple[bool, value] split(n) = (n >= 10) ? <true, <n / 2, n / 2 + n % 2>> : <false, n>;

default value reduceList(l) {
	if (<true, ln> := split(l)) {
		//println("Splitted <l> to <ln>");
		return reduceList(ln);
	}
	return l;
}

int magnitude(<l, r>) = 3 * magnitude(l) + 2 * magnitude(r);
int magnitude(n) = n;

void p1() = println(magnitude(reduceList(addLists([], input1))));

void p2() {
	magnitudes = [ magnitude(reduceList(<input1[i], input1[j]>)) | i <- index(input1), j <- index(input1) ];
	m = max(magnitudes);
	println("<m>");
}
