
(function() {
"use strict";

var lispy = (window || exports).lispy = {};

var assert = function(b, str) {
  if(!b) {
    throw ("assert failure!  " + (""+str));
  }
};

var types = {  //strs easier for debugging, objs maybe faster
  // token types
  openParen: "openParen",//{},
  closeParen: "closeParen",//{},
  number: "number",//{},
  identifier: "identifier",//{},
  boolean: "boolean",//{},
  comment: "comment",//{}, //TODO
  string: "string",//{},
  void: "void",//{},
  EOF: "EOF",//{}

  // composite types
  list: "list",//{},
  program: "program",//{}
  imperative: "imperative",//{} //function bodies become this

  // other types
  unboundVariable: "unboundVariable",//{} //hmm
  builtinFunction: "builtinFunction"//{},
};


var mkvoid = lispy.mkvoid = function(n) {
  return { type: types.void, string: "void" };
}
function mknum(n) {
  return { type: types.number, value: n, string: (""+n) };
}
function mkbool(b) {
  if(b) {
    return { type: types.boolean, value: true, string: "true" };
  } else {
    return { type: types.boolean, value: false, string: "false" };
  }
}
function mkidentifier(s) {
  return { type: types.identifier, string: s };
}
function mkUnboundVariable() {
  return { type: types.unboundVariable, string: 'unbound-variable' };
}
function mkfn(f) {
  return { type: types.builtinFunction, value: f, string: (""+f) };
}
function mkstr(s) {
  return { type: types.string, value: s,
           string: s.replace(/\\/g, '\\\\').replace(/"/g, '\\"') };
}
lispy.wrapJSVal = function(v) {
  if(_.isNumber(v)) {
    return mknum(v);
  }
  else if(_.isBoolean(v)) {
    return mkbool(v);
  }
  else if(_.isFunction(v)) {
    return mkfn(v);
  }
  else if(_.isString(v)) {
    return mkstr(v);
  }
  else {
    throw 'wrapJSVal: not implemented yet: ' + v;
  }
};
var english_numbering_names = ['first', 'second', 'third'];
function evaluate_to_number(tree, env) {
  var evaled = lispy.evaluate(tree, env);
  assert(evaled.type === types.number, lispy.crappyRender(tree) + " is not a number");
  return evaled;
}
function evaluate_to_bool(tree, env) {
  var evaled = lispy.evaluate(tree, env);
  assert(evaled.type === types.boolean, lispy.crappyRender(tree) + " is not a boolean");
  return evaled;
}
function modulo(num, mod) {
  assert(mod > 0, "modulo: non-positive divisor " + mod);
  var result = num % mod;
  if(result < 0) {
    result += mod;
  }
  return result;
}
var builtins = {
  // just binary ops currently, not the lisp pattern..
  '+': function(tree, env) {
    //TODO we could combine all src info till we had like everything it came from involved here?
    //TODO type/argnum checking? outsidely visible argument count?
 //   assert(tree.type === types.list);
 //   assert(tree[0].type === types.identifier);
 //   assert(tree[0].string === '+');
    //those were generic. now,
//    assert(tree.length === 3);
//    assert(tree[1].type === types.number);
//    assert(tree[2].type === types.number);
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    //console.log(tree);
    //floating point math?
    return mknum(evaluate_to_number(tree[1], env).value + evaluate_to_number(tree[2], env).value);
  },
  '-': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mknum(evaluate_to_number(tree[1], env).value - evaluate_to_number(tree[2], env).value);
  },
  '*': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mknum(evaluate_to_number(tree[1], env).value * evaluate_to_number(tree[2], env).value);
  },
  '/': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mknum(evaluate_to_number(tree[1], env).value / evaluate_to_number(tree[2], env).value);
  },
  'mod': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mknum(modulo(evaluate_to_number(tree[1], env).value, evaluate_to_number(tree[2], env).value));
  },
  'negate': function(tree, env) {
    assert(tree.length === 2, lispy.crappyRender(tree) + " arg count");
    return mknum(-evaluate_to_number(tree[1], env).value);
  },
  //should and/or use the "return the first/last valid value" thing and have all this implicit boolean convertability?
  //These implementations do not evaluate the second argument if the first
  //one shows we don't need to know its value (intentionally) (due to the JS
  //short circuiting behavior here).
  'and': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(evaluate_to_bool(tree[1], env).value && evaluate_to_bool(tree[2], env).value);
  },
  'or': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(evaluate_to_bool(tree[1], env).value || evaluate_to_bool(tree[2], env).value);
  },
  'not': function(tree, env) {
    assert(tree.length === 2, lispy.crappyRender(tree) + " arg count");
    return mkbool(!evaluate_to_bool(tree[1], env).value);
  },
  //equality/lessthan ?
  // THIS IS NOT A VERY GOOD IMPLEMENTATION, TODO
  '=': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(lispy.evaluate(tree[1], env).value === lispy.evaluate(tree[2], env).value);
  },
  'not=': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(lispy.evaluate(tree[1], env).value !== lispy.evaluate(tree[2], env).value);
  },
  // CURRENTLY ONLY ARE A THING FOR NUMBERS:
  '<': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(evaluate_to_number(tree[1], env).value < evaluate_to_number(tree[2], env).value);
  },
  '>': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(evaluate_to_number(tree[1], env).value > evaluate_to_number(tree[2], env).value);
  },
  '>=': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(evaluate_to_number(tree[1], env).value >= evaluate_to_number(tree[2], env).value);
  },
  '<=': function(tree, env) {
    assert(tree.length === 3, lispy.crappyRender(tree) + " arg count");
    return mkbool(evaluate_to_number(tree[1], env).value <= evaluate_to_number(tree[2], env).value);
  },
  // IIRC 'if' needs to be a builtin in strictly evaluated languages
  'if': function(tree, env) {
    assert(tree.length === 4, lispy.crappyRender(tree) + " arg count");
    var b = evaluate_to_bool(tree[1], env).value;
    return (b ? lispy.evaluate(tree[2], env) : lispy.evaluate(tree[3], env));
  }
};
//what if all composite types (fn, list, assoc) got names
//let's see
//i'll have to set up indirection for:
//beta-reduce
//any builtins that operate on list, assoc

// following Scheme (not very accurately):
// even [:alnum:] doesn't work in JS regexps
var identifierChar = /[\-!$%&*+.\/:<=>?@\^_~0-9a-zA-Z]/;
// following Haskell:
var tabwidthForColumnCount = 8;
// following the most common conventions:
var initialLine = 1;
var initialColumn = 0;

// tokenize : string -> array of token details objects
function tokenize(str) {
  var result = [];
  var pos = 0;
  var line = initialLine;
  var column = initialColumn;
  var token = function(details, len) {
    details.pos = pos;
    details.line = line;
    details.column = column;
    details.endLine = line; // TODO multi line tokens (strings?)
    details.endColumn = column + len - 1;
    details.len = len;
    details.string = str.slice(pos, pos + len);
    result.push(details);
    pos += len;
    // assumes tokens don't contain \n
    column += len;
  };
  while(pos < str.length) {
    if(/[ \t\n\r]/.test(str[pos])) {
      if(str[pos] === ' ') {
        column += 1;
      } else if(str[pos] === '\t') {
        column += tabwidthForColumnCount;
        column -= (column % tabwidthForColumnCount);
      } else if(str[pos] === '\n') {
        column = initialColumn;
        line += 1;
      }
      // else assume any \r is in a \r\n combination and ignore it.
      pos += 1;
    } else if(str[pos] === '"') {
      var strlen = 1;
      assert(pos + strlen < str.length, "unterminated string literal");
      while((str[pos + strlen - 1] === '\\' || str[pos + strlen] !== '"')) {
        ++strlen;
        assert(pos + strlen < str.length, "unterminated string literal");
      }
      ++strlen;
      var strstr = str.slice(pos + 1, pos + strlen - 1);
      strstr = strstr.replace(/\\"/g, '"').replace(/\\\\/g, '\\');
      token({type: types.string, value: strstr}, strlen);
    } else if(str[pos] === '(') {
      token({type: types.openParen}, 1);
    } else if(str[pos] === ')') {
      token({type: types.closeParen}, 1);
    } else if(/^-?\.?[0-9]/.test(str.slice(pos, pos+3))) {
      var numlen = 1;
      while(pos + numlen < str.length && /[\-0-9a-zA-z_,.]/.test(str[pos + numlen])) {
        ++numlen;
      }
      // TODO use our own not JS's hex/oct/dec number syntax?
      // TODO int vs non integer numbers?
      //var numval = parseInt(str.slice(pos, pos + numlen));
      var numstr = str.slice(pos, pos + numlen);
      var numval = +numstr;
      assert(!isNaN(numval), "number interpretation of '" + numstr +
              "' failed at line " + line + " column " + column + "!");
      // TODO forbid 12&& being a number token followed by an
      // identifier token without any spaces?  Ah by eating up
      // a whole identifier and then if it begins number-like
      // then make it a number or fail.
      token({type: types.number, value: numval}, numlen);
    } else if(identifierChar.test(str[pos])) {
      var idlen = 1;
      while(pos + idlen < str.length && identifierChar.test(str[pos + idlen])) {
        ++idlen;
      }
      // are true/false keywords? why? why not immutable globals? why not #t / #f?
      var idstr = str.slice(pos, pos + idlen);
      if(idstr === 'true') {
        token({type: types.boolean, value: true}, idlen);
      } else if(idstr === 'false') {
        token({type: types.boolean, value: false}, idlen);
      } else {
        token({type: types.identifier}, idlen);
      }
    } else {
      throw ("tokenizer fail at line " + line + " column " + column + "!");
    }
  }
  token({type: types.EOF}, 0);
  //console.log(result);
  return result;
}
var builtinsAsLispyThings = {};
_.each(builtins, function(val, key) {
  builtinsAsLispyThings[key] = lispy.wrapJSVal(val);
});
lispy.builtins = builtins;
lispy.builtinsAsLispyThings = builtinsAsLispyThings;

function isLiteralValueToken(tok) {
  return tok.type === types.number ||
    tok.type === types.identifier ||
    tok.type === types.string ||
    tok.type === types.boolean;
}

// Returns { parsed: list of sub-lists or tokens, endPos: n } with endPos one-after-end
function parseList(toks, pos, type) {
  var ourNest = [];
  ourNest.type = type;
  while(true) {
    if(toks[pos].type === types.openParen) {
      var result = parseList(toks, pos + 1, types.list);
      ourNest.push(result.parsed);
      pos = result.endPos;
    } else if(isLiteralValueToken(toks[pos])) {
      ourNest.push(toks[pos]);
      pos += 1;
    } else if((toks[pos].type === types.closeParen && type === types.list) ||
              (toks[pos].type === types.EOF && type === types.program)) {
      pos += 1;
      return { parsed: ourNest, endPos: pos };
    } else {
      //TODO maybe different error handling?
      throw ("parse failure at line " + toks[pos].line + " column " + toks[pos].column);
    }
  }
}

// parseProgram : string -> list/token structure
lispy.parseProgram = function(str) {
  return parseList(tokenize(str), 0, types.program).parsed;
};

// parseProgram ""
// parseProgram "()"
// parseProgram "(1)"

// TODO next:
//  (fn (a b c) (+ b c))
//  fn and application and +  ...and do/sequencing ?
//  application: eval each argument (incl function) and then enter function.
//    or rather: eval function, look at its strictness 

//lispy.renameProgram //well we could use a de Bruijn form, we could use ptrs,
// we could just turn all the strings into symbols/atoms (numbers)


//This should probably (be used when) eval args first, not duplicate them like that
//Or name the args (give them wacky names)
//beta-reduce depth first deeper first left-to-right
//ALSO bug beware http://foldoc.org/name+capture
//if args are not in NORMAL FORM with all their non bound vars
//already substituted (locally non bound ones; & globally non bound ones as errors or undef)
// http://foldoc.org/Weak+Head+Normal+Form
// http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form
// http://en.wikipedia.org/wiki/Beta_normal_form

function keepMetaDataFrom(tree, arrayTree) {
  for(var key in tree) {
    if(_.has(tree, key) && !/^[0-9]+$/.test(key)) {
      arrayTree[key] = tree[key];
    }
  }
  return arrayTree;
}

//...larger arguments get (randomly)named, perhaps
lispy.betaReduceO_N = function(tree) {
  // pattern-match ((fn (params...) body...) args...)
  assert(lispy.isHeadBetaReducible(tree), "beta beta");
  //assert(tree.type === types.list);
  //assert(tree.length >= 1);
  var fn = tree[0];
  var args = tree.slice(1);
  //assert(fn.length > 2);
  //assert(fn[0].type === types.identifier);
  //assert(fn[0].string === 'fn');
  //assert(fn[1].type === types.list);
  var params = fn[1];
  var body = keepMetaDataFrom(fn, fn.slice(2));
  body.type = types.imperative;

  var substitutions = {};
  assert(params.length === args.length, lispy.crappyRender(tree) + " equal params length"); //no silly stuff!
  for(var i = 0; i !== params.length; ++i) {
    assert(params[i].type === types.identifier, "params are identifiers");
    substitutions[params[i].string] = args[i];
  }

  // We need to substitute it everywhere except where it's
  // named in a let or lambda
  // [and except where it's quoted / part of a macro]
  return lispy.substitute(substitutions, body);
};

// evaluates all beta-redexes not within a lambda (?)
// evaluates until we have a int or lambda?
// prevents list literals from being partly evaled?

lispy.isLambdaLiteral = function(tree) {
  return tree.type === types.list &&
    tree.length > 2 &&
    tree[0].type === types.identifier &&
    tree[0].string === 'fn' &&
    tree[1].type === types.list;
};

lispy.isHeadBetaReducible = function(tree) {
  return tree.type === types.list &&
    tree.length >= 1 && lispy.isLambdaLiteral(tree[0]);
};

lispy.rep = lispy.readEvalPrint = function(str) {
  return lispy.crappyRender(lispy.evaluate(lispy.parseProgram(str), builtinsAsLispyThings));
};
$(function() {
  var $code = $('#code');
  var $out = $('#out');
  $code.on('change keyup', function() {
    var code = $code.val();
    var out;
    try {
      out = lispy.rep(code);
    }
    catch(e) {
      out = (""+e);
    }
    $out.text(out);
  });
  $code.val("((fn (x) x) 3)\n(+ 3 4)\n45\n\n((fn (x)\n  (if (= (mod x 2) 0)\n      (/ x 2)\n      (+ (* x 3) 1)))\n3)");
  $code.trigger('change');
});

// lispy.rep("((fn (x) (x x)) (fn (x) (x x)))")
// lispy.crappyRender(lispy.evaluate(lispy.parseProgram("((fn (x y) y (x y)) (fn (x) x) 34)")))
// lispy.crappyRender(lispy.evaluate(lispy.parseProgram("((fn (x y) y) 23 34)")[0]))

//env is a {} from identifier (as plain string) to tree.
//It is used to look up free variables.
//(Strictly evaluated substitution e.g. betaReduceO_N
// cannot implement recursion or letrec,
// so env is necessary, not just conventional.
// It can be represented with a (let (...) ...)
// around the to-be-evaluated tree if necessary.)
//BUG TODO- members of 'env' cannot have any free variables.
//I *think* pre-substituting them is fine but having a (env (...) ...)
//primitive would be fine (like 'let' but clears the scope) (or let for
//all the free variables also works fine) (assuming they *can* be referenced
//thus).
//
//returns the evaluated version of the tree.
lispy.evaluate = function(tree, env) {
  if(arguments.length === 1) {
    env = {};
  }
  while(true) {
    if(tree.type === types.identifier && _.has(env, tree.string)) {
      // substitute plain identifiers from env:
      //   ident
      tree = env[tree.string];
    }
    else if(tree.type === types.list && tree.length >= 1 &&
        tree[0].type === types.identifier && _.has(env, tree[0].string)) {
      // substitute function names that are about to be called from env:
      //   (ident ...)
      var headEvaledTree = shallowCopyArray(tree);
      headEvaledTree[0] = env[tree[0].string];
      tree = headEvaledTree;
    }
    else if(lispy.isHeadBetaReducible(tree)) {
      // call lambda:
      //   ((fn (...) ...) ...)
      tree = lispy.strictBetaReduceO_N(tree);
    }
    else if(tree.type === types.program) {
      // evaluate programs in sequence:
      //   (+ 1 2)
      //   (+ 3 4)
      tree = keepMetaDataFrom(tree, _.map(tree, function(subtree) {
        return lispy.evaluate(subtree, env);
      }));
      break;
    }
    else if(tree.type === types.imperative) {
      // evaluate function-bodies in sequence:
      //   (+ 1 2) (+ 3 4)
      assert(tree.length !== 0, "imperative things have to return something");// (TODO make a nil return default maybe?)");
      var result = null;
      _.each(tree, function(subtree) {
        result = lispy.evaluate(subtree, env);
      });
      tree = result;
      break;
    }
    else if(tree.type === types.list &&
        tree[0].type === types.builtinFunction) {
      // evaluate builtins:
      //   (+ 1 2)
      tree = tree[0].value(tree, env);
      break;
    }
    else if(tree.type === types.list &&
        tree[0].type === types.list) {
      // attempt to evaluate the function part:
      //   ((if true + -) 7 3)
      var headEvaled = lispy.evaluate(tree[0], env);
      // TODO identity-based equality comparison is
      // fragile here? (to prevent infinite loop
      // trying to reduce something that we can't
      // reduce anymore)
      // TODO if function-calling called evaluate
      // once on its body then there would be no
      // infinite loop/recursion worries, right?
      // (aside from legitimately nonterminating
      // computations, of course).
      if(headEvaled === tree[0]) {
        break;
      }
      else {
        var headEvaledTree = shallowCopyArray(tree);
        headEvaledTree[0] = headEvaled;
        tree = headEvaledTree;
      }
    }
    else {
      // No reductions found: we must be done.
      break;
    }
  }
  tree = lispy.bindFreeVars(tree, env);
  return tree;
};

function shallowCopyArray(arr) {
  return keepMetaDataFrom(arr, _.map(arr, _.identity));
}

lispy.strictBetaReduceO_N = function(tree, env) {
  assert(lispy.isHeadBetaReducible(tree), "strictBeta beta");
  var argsEvaledTree = shallowCopyArray(tree);
  for(var i = 1; i !== tree.length; ++i) {
    argsEvaledTree[i] = lispy.evaluate(tree[i], env);
  }
  return lispy.betaReduceO_N(argsEvaledTree);
};

/*
  substitute({
    'foo': { type: types.number, value: 3, string: "3", ... },
    'bar': { type: types.identifier, string: "something", ... },
  },
  [ { type: types.identifier, string: "+", ... },
    { type: types.identifier, string: "foo", ... },
    { type: types.number, value: 7, string: "7", ... }
  ])

wait, does it mutate or return a new tree? return a new tree. there might be sharing
of some tokens - we intend that they are never mutated by any code

can 'fn' be bound? that is not guarded against.
*/
lispy.substitute = function(varsToTreesMap, tree) {
  if(tree.type === types.list || tree.type === types.program || tree.type === types.imperative) {
    if(lispy.isLambdaLiteral(tree)) {
      var bindings = _.pluck(tree[1], 'string');
      var subMap = _.omit(varsToTreesMap, bindings);
      return keepMetaDataFrom(tree, _.map(tree, function(sub) {
        return lispy.substitute(subMap, sub);
      }));
    }
    else {
      return keepMetaDataFrom(tree, _.map(tree, function(sub) {
        return lispy.substitute(varsToTreesMap, sub);
      }));
    }
  }
  else if(tree.type === types.identifier && _.has(varsToTreesMap, tree.string)) {
    return varsToTreesMap[tree.string];
  }
  else { //other token
    return tree;
  }
};

// returns a set { var: true, ... } of the free vars in tree
// Asymptotic complexity is currently suboptimal.
lispy.freeVarsIn = function(tree, boundVars) {
  if(boundVars === undefined) { boundVars = {}; }
  var freeVars = {};
  if(tree.type === types.list || tree.type === types.program || tree.type === types.imperative) {
    if(lispy.isLambdaLiteral(tree)) {
      var bindings = _.pluck(tree[1], 'string');
      _.each(tree.slice(2), function(sub) {
        _.extend(freeVars, lispy.freeVarsIn(sub, _.extend({}, bindings, boundVars)));
      });
    }
    else {
      _.each(tree, function(sub) {
        _.extend(freeVars, lispy.freeVarsIn(sub, boundVars));
      });
    }
  }
  else if(tree.type === types.identifier && !_.has(boundVars, tree.string)) {
    freeVars[tree.string] = true;
  }
  return freeVars;
};

lispy.bindFreeVars = function(tree, env) {
  // We sort this for determinacy's sake.
  var varsToBind = _.keys(lispy.freeVarsIn(tree)).sort();

  if(varsToBind.length === 0) {
    return tree;
  }
  else {
    var bindings = _.map(varsToBind, function(v) {
      if(_.has(env, v)) {
        return v;
      }
      else {
        // hmm
        return mkUnboundVariable();
        // hmm could use a dummy variable here of type unbound_free_var or such
      //  throw "Unbound free var " + v + " in " + lispy.crappyRender(tree);
      }
    });
    //TODO implement 'let' as syntactic sugar for such immediately-applied-function
    var paramsTree = _.map(varsToBind, function(v) { return mkidentifier(v); });
    paramsTree.type = types.list;
    var lambdaTree = [mkidentifier('fn'), paramsTree, tree];
    lambdaTree.type = types.list;
    var applyTree = [lambdaTree].concat(bindings);
    applyTree.type = types.list;
    return applyTree;
  }
};

lispy.crappyRender = function(tree) {
  var result;
  if(tree.type === types.program || tree.type === types.imperative) {
    result = '';
    _.each(tree, function(subtree) {
      result += lispy.crappyRender(subtree);
      result += '\n';
    });
    return result;
  }
  else if(tree.type === types.list) {
    result = '(';
    _.each(tree, function(subtree, index) {
      if(index !== 0) {
        result += ' ';
      }
      result += lispy.crappyRender(subtree);
    });
    result += ')';
    return result;
  }
  else {
    return tree.string;
  }
};

//lispy.betaReduceO_1

}());
