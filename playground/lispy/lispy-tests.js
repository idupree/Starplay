
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
  testEval('a', 'under');

  if(errString !== "") {
    throw errString;
  }
};





}());
