
// Copyright Isaac Dupree, MIT-licensed.

// This file contains tests for lispy.js.
// Include it after including lispy.js, then run lispy.test
// and it will throw an exception if any test fails.

(function() {
"use strict";


lispy.test = function() {
  var errString = "";

  function testEval(exprStr, desiredResultStr) {
    try {
      var expr = lispy.parseSexp(exprStr);
      var result = lispy.evaluate(expr, lispy.builtinsAsLispyThings);
      var resultStr = lispy.printSexpNonWhitespacePreserving(result);
    } catch(e) {
      errString += "test error: broken:"+
        "\n      expr: "+exprStr+
        "\n  actually "+e+
        "\n  expected: "+desiredResultStr+
        "\n";
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
      var expr = lispy.parseSexp(exprStr);
      var result = lispy.evaluate(expr, lispy.builtinsAsLispyThings);
      var resultStr = lispy.printSexpNonWhitespacePreserving(result);
    } catch(e) {
      return;
    }
    errString += "test error: did not break:"+
      "\n      expr: "+exprStr+
      "\n    actual: "+resultStr+
      "\n  expected an exception."+
      "\n";
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

  // We currently choose not to have weakly typed boolean ops:
  testBreak('(and true 3)');
  testBreak('(or false 3)');
  testBreak('(not 3)');
  // but allow boolean short-circuiting to short-circuit type safety:
  testEval('(and false 3)', 'false');
  testEval('(or true 3)', 'true');

  testEval('a', 'unbound-variable');

  if(errString !== "") {
    throw errString;
  }
};





}());
