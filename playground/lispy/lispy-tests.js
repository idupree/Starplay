
// Copyright Isaac Dupree, MIT-licensed.

// This file contains tests for lispy.js.
// Include it after including lispy.js, then run lispy.test
// and it will throw an exception if any test fails.

(function() {
"use strict";


lispy.test = function() {
  var errString = "";

  // REP: read, eval, print
  function testREP(exprStr) {
    var expr = lispy.parseSexp(exprStr);
    var result = lispy.evaluate(expr, lispy.builtinsEnv);
    var resultStr = lispy.printSexpNonWhitespacePreserving(result);
    return resultStr;
  }

  function testEval(exprStr, desiredResultStr) {
    try {
      var resultStr = testREP(exprStr);
    } catch(e) {
      errString += "test error: broken:"+
        "\n      expr: "+exprStr+
        "\n  actually "+e+
        "\n  expected: "+desiredResultStr+
        "\n";
      return;
    }
    if(resultStr !== desiredResultStr) {
      errString += "test error: mismatch:"+
        "\n      expr: "+exprStr+
        "\n    actual: "+resultStr+
        "\n  expected: "+desiredResultStr+
        "\n";
    }
  }

  function testBreak(exprStr) {
    try {
      var resultStr = testREP(exprStr);
    } catch(e) {
      return;
    }
    errString += "test error: did not break:"+
      "\n      expr: "+exprStr+
      "\n    actual: "+resultStr+
      "\n  expected an exception."+
      "\n";
  }

  function test(jsFn, desiredResultStr) {
    try {
      var resultStr = ""+jsFn();
    } catch(e) {
      errString += "test error:"+
        "\n        JS:\n"+jsFn+
        "\n   actually "+e+
        "\n  expected: "+desiredResultStr+
        "\n";
      return;
    }
    if(resultStr !== desiredResultStr) {
      errString += "test error:"+
        "\n        JS:\n"+jsFn+
        "\n    actual: "+resultStr+
        "\n  expected: "+desiredResultStr+
        "\n";
    }
  }

  testEval('1', '1');
  testEval('12', '12');
  testEval('-2', '-2');
  testEval('-002', '-2');
  testEval('0.5', '0.5');
  testEval('-0', '0');

  testBreak('(');
  testBreak(')');
  testBreak('1 2');
  testBreak('1\n2');
  testEval('  1  ', '1');
  testBreak('- 1');

  testBreak('"');
  testEval('"foo bar"', '"foo bar"');
  // multi line string literals are allowed:
  testEval('"foo\nbar"', '"foo\nbar"');
  // escaping:
  testEval('"foo\\"and\\\\bar"', '"foo\\"and\\\\bar"');

  testEval('(+ 2 3)', '5');
  testEval('(+ 13 27)', '40');
  testEval('(- 13 27)', '-14');
  testEval('(- 3 2)', '1');
  testEval('(* 3 2)', '6');
  testEval('(/ 3 2)', '1.5');
  testEval('(mod 5 3)', '2');
  testEval('(mod -5 3)', '1');
  testBreak('(mod 5 -3)');
  testBreak('(mod -5 -3)');
  testEval('(negate 3)', '-3');
  testEval('(negate -3)', '3');
  testEval('(negate 0)', '0');
  testEval('(negate -0)', '0');

  testEval('true', 'true');
  testEval('false', 'false');
  testEval('(and  true false)', 'false');
  testEval('(and  true  true)', 'true');
  testEval('(and false false)', 'false');
  testEval('(and  false true)', 'false');
  testEval('(or  true false)', 'true');
  testEval('(or  true  true)', 'true');
  testEval('(or false false)', 'false');
  testEval('(or  false true)', 'true');
  testEval('(not false)', 'true');
  testEval('(not true)', 'false');

  // Extra whitespace doesn't stop evaluation:
  testEval('  (  not  true  )  ', 'false');
  // Test nested evaluation:
  testEval('(+ (+ 1 2) 4)', '7');

  // We currently choose not to have weakly typed boolean ops:
  testBreak('(and true 3)');
  testBreak('(or false 3)');
  testBreak('(not 3)');
  // but allow boolean short-circuiting to short-circuit type safety:
  testEval('(and false 3)', 'false');
  testEval('(or true 3)', 'true');

  testEval('(= 1 2)', 'false');
  testEval('(not= 1 2)', 'true');
  testEval('(= 1 1)', 'true');
  testEval('(not= 1 1)', 'false');
  testEval('(= -1 -001)', 'true');
  testEval('(not= -1 -001)', 'false');

  testEval('(= true true)', 'true');
  testEval('(not= true true)', 'false');
  testEval('(= true false)', 'false');
  testEval('(not= true false)', 'true');

  // Cross-type equality comparison is currently acceptable:
  testEval('(= true 17)', 'false');

  // Equality on non-atoms is horribly defined presently:
  // it should be forbidden unless it gets a good definition:
  testBreak('(= (array) (array))');
  testBreak('(not= (array) (array))');

  // Non-numbers can't be compared for ordering at present.
  testBreak('(< false true)');
  testBreak('(< false 3)');
  testBreak('(< 3 false)');
  testBreak('(> false true)');
  testBreak('(>= false true)');
  testBreak('(<= false true)');

  testEval('(> 1 2)', 'false');
  testEval('(>= 1 2)', 'false');
  testEval('(< 1 2)', 'true');
  testEval('(<= 1 2)', 'true');
  testEval('(> 1 1)', 'false');
  testEval('(>= 1 1)', 'true');
  testEval('(< 1 1)', 'false');
  testEval('(<= 1 1)', 'true');

  testBreak('(if 1 2 3)');
  testEval('(if true 2 3)', '2');
  testEval('(if false 2 3)', '3');
  testEval('(if (= 1 1) 2 3)', '2');
  testEval('(if (= 1 3) 2 3)', '3');
  // short-circuiting:
  testEval('(if (= 1 1) 2 (not 17))', '2');
  testEval('(if (= 1 2) (not 17) 3)', '3');
  testBreak('(if (= 1 2) 2 (not 17))');
  testBreak('(if (= 1 1) (not 17) 3)');

  testEval('(array 1 4)', '(array 1 4)');
  testEval('(array false)', '(array false)');
  testEval('(array 1 4 true)', '(array 1 4 true)');
  testEval('(array)', '(array)');
  testEval('(  array  )', '(array)');
  testEval('(array 1 (+ 3 6))', '(array 1 9)');

  testEval('a', '#unbound-variable');

  testEval('((fn () 3))', '3');
  testEval('(fn () 3)', '(fn () 3)');
  // A more thorough canonicalization would alpha-rename
  // bound vars to reverse de Bruijn notation or such. Hmm.
  testEval('(fn (a) 3)', '(fn (a) 3)');
  testEval('(fn (a b) a)', '(fn (a b) a)');
  testEval('((fn (a b) a) 3 4)', '3');
  // exact numbers of arguments must be given:
  testBreak('((fn (a b) a) 3)');
  testBreak('((fn (a b) a) 3 4 5)');
  // functions can do complicated things with their arguments:
  testEval('((fn (b v) (if b (* v v) (+ v v))) true 7)', '49');
  testEval('((fn (b v) (if b (* v v) (+ v v))) false 7)', '14');
  // higher-order functions, scope, closures:
  testEval('((fn (f) (f)) (fn () 3))', '3');
  testEval('((fn (f) (f 3)) (fn (f) f))', '3');
  testEval('((fn (f x) (f 3)) (fn (x) x) 4)', '3');
  testEval('((fn (f x) (f 3)) (fn (y) x) 4)', '#unbound-variable');
  testEval('((fn (f x y) (f (+ y y))) (fn (x) x) 4 3)', '6');
  testEval('((fn (f x y) (f (+ y y))) (fn (y) x) 4 3)', '#unbound-variable');

  testEval('((if true + -) 7 3)', '10');

  test(function() {
    return _.keys(lispy.freeVarsIn(lispy.parseSexp('(fn (y) (+ y y))'))
                 ).sort().join(' ');
  }, '+');
  test(function() {
    return _.keys(lispy.freeVarsIn(lispy.parseSexp('(fn (y) (+ y y))'),
                                   lispy.builtinsAsLispyThings)
                 ).sort().join(' ');
  }, '');

  testEval('(((fn (x) (fn () x)) 3))', '3');
  testEval('(((fn (x) (fn (y) (+ x y))) 3) 4)', '7');
  testEval('((fn (x) (fn (y) (+ x y))) 3)', '(fn (y) (+ 3 y))');
  testEval('((fn (x) (fn (y) (not-actually-+ x y))) 3)', '(fn (y) (#unbound-variable 3 y))');

  testEval('(letrec (fac (fn (n) (if (= n 0) 1 (* n (fac (- n 1)))))) (fac 4))', '24');

  // fn
  // (fn (x x)) ??
  // y combinator


  if(errString !== "") {
    throw errString;
  }
};





}());
