
// Copyright Isaac Dupree, MIT-licensed.

// This file implements a simple lisp-like language
// with symbolic evaluation.

(function() {
"use strict";

var lispy = (window || exports).lispy = {};

var assert = function(b, str) {
  if(!b) {
    throw ("assert failure!  " + (""+str));
  }
};

// Tokenizing and parsing create abstract-syntax-tree objects
// (S-expressions, or sexps) which are the main internal representation
// of the program.
//
// There is no bytecode or intermediate language.
// This is partly intentional to make it simple to retain
// source-level info (e.g. line, column, source text) while computing,
// to make it easier to show the user what's been going on with their code.
var types = {  //strs easier for debugging, objs maybe faster
  // token types
  openParen: "openParen",//{},   // (
  closeParen: "closeParen",//{}, // )
  number: "number",//{},         // 123
  identifier: "identifier",//{}, // abc
  boolean: "boolean",//{},       // true
  comment: "comment",//{}, //TODO
  string: "string",//{},         // "Hi there."
  // (In JS, void is a keyword,
  // so we use _void for simplicity)
  _void: "_void",//{},           // lack of result, e.g. from ((fn () ))
  EOF: "EOF",//{}                // end-of-file (used for parsing)

  // composite types
  // code
  sexp: "sexp",//{},             // S-expressions, e.g. (f a b), (f (a b) c),
                                 // (arg1 arg2) in a fn, etc.
  program: "program",//{}        // 1
                                 // 23
                                 // 456
                                 // (the sequence of S-expressions that
                                 //  make up a file.)
  imperative: "imperative",//{}  // Function bodies, which are a sequence
                                 // of S-expressions, become this
                                 // during evaluation.

  // data
  // (I am not using/exposing lisp's homeomorphic abilities presently.)
  // (There is no way to write a literal array or dict presently;
  //  create them with builtin functions (array ...) or (TODO) (dict ...).
  array: "array",//{},           // A runtime sequence
  dict: "dict",//{}, //SOMEWHAT TODO: a runtime assoc

  // other types
  unboundVariable: "unboundVariable",//{}
                                 // When an unbound variable is evaluated,
                                 // it becomes this.
  builtinFunction: "builtinFunction"//{},
                                 // Builtin functions (i.e. ones that simply
                                 // contain a JS function(){}) are this.
};

// Data in lispy-land are represented by a JS object
// with { 'type': types.something } and other fields
// depending on what the type is.  These functions
// provide ways to create basic lispy-land data:
// mk*() for specific types, and wrapJSVal() for
// any type that occurs both in JS values and lispy-land.
var mkvoid = lispy.mkvoid = function() {
  return { type: types._void, string: "void" };
};
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
// TODO: how to detect WHETHER an 'object' is a lispy object:
// i should put a field in.
// *should* arrays have a string rep? PROBABLY NOT
function mkarray(a) {
  return { type: types.array, value: a };
}
function mkdict(d) {
  return { type: types.dict, value: d };
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

// == Builtin functions ==
// Consider a JS var 'sexp' representing '(+ 2 3)';
// sexp.type will be types.sexp.  '+' is sexp[0].
// If the '+' in scope is
//   {type: types.builtinFunction, value: f}
// then the eval loop will call 'f(sexp, env)' where
// env represents the variables in scope;
// env is a JS object mapping identifier strings to sexps that
// might be unevaluated but definitely contain no free variables.
//
// Builtin functions also cannot count on their arguments being
// evaluated, and must do so if they wish to e.g. do math on their
// arguments.  As a benefit, it is simple to write (if) as a builtin
// function that does not evaluate both the true and false branches.

// These are convenience functions for use writing builtins.
function evaluate_to_number(sexp, env) {
  var evaled = lispy.evaluate(sexp, env);
  assert(evaled.type === types.number, lispy.crappyRender(sexp) + " is not a number");
  return evaled.value;
}
function evaluate_to_bool(sexp, env) {
  var evaled = lispy.evaluate(sexp, env);
  assert(evaled.type === types.boolean, lispy.crappyRender(sexp) + " is not a boolean");
  return evaled.value;
}
function modulo(num, mod) {
  assert(mod > 0, "modulo: non-positive divisor " + mod);
  var result = num % mod;
  if(result < 0) {
    result += mod;
  }
  return result;
}

// These are the typical set of builtins.
// You can make them available to lispy programs by passing
// builtinsAsLispyThings as the env parameter to the lispy code you evaluate.
var builtins = {
  // We just offer binary ops currently, not the lisp pattern of
  // monoidal ops like '+', '*', 'or', 'and' accepting any number of
  // arguments.

  // TODO: don't rely on floating-point math.
  // (This TODO will not be done in the prototype implementation.)
  '+': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mknum(evaluate_to_number(sexp[1], env) + evaluate_to_number(sexp[2], env));
  },
  '-': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mknum(evaluate_to_number(sexp[1], env) - evaluate_to_number(sexp[2], env));
  },
  '*': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mknum(evaluate_to_number(sexp[1], env) * evaluate_to_number(sexp[2], env));
  },
  '/': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mknum(evaluate_to_number(sexp[1], env) / evaluate_to_number(sexp[2], env));
  },
  'mod': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mknum(modulo(evaluate_to_number(sexp[1], env), evaluate_to_number(sexp[2], env)));
  },
  'negate': function(sexp, env) {
    assert(sexp.length === 2, lispy.crappyRender(sexp) + " arg count");
    return mknum(-evaluate_to_number(sexp[1], env));
  },
  //should and/or use the "return the first/last valid value" thing and have all this implicit boolean convertability?
  //These implementations do not evaluate the second argument if the first
  //one shows we don't need to know its value (intentionally) (due to the JS
  //short circuiting behavior here).
  'and': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(evaluate_to_bool(sexp[1], env) && evaluate_to_bool(sexp[2], env));
  },
  'or': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(evaluate_to_bool(sexp[1], env) || evaluate_to_bool(sexp[2], env));
  },
  'not': function(sexp, env) {
    assert(sexp.length === 2, lispy.crappyRender(sexp) + " arg count");
    return mkbool(!evaluate_to_bool(sexp[1], env));
  },
  //equality/lessthan ?
  // THIS IS NOT A VERY GOOD IMPLEMENTATION, TODO
  '=': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(lispy.evaluate(sexp[1], env).value === lispy.evaluate(sexp[2], env).value);
  },
  'not=': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(lispy.evaluate(sexp[1], env).value !== lispy.evaluate(sexp[2], env).value);
  },
  // CURRENTLY ONLY ARE A THING FOR NUMBERS:
  '<': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(evaluate_to_number(sexp[1], env) < evaluate_to_number(sexp[2], env));
  },
  '>': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(evaluate_to_number(sexp[1], env) > evaluate_to_number(sexp[2], env));
  },
  '>=': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(evaluate_to_number(sexp[1], env) >= evaluate_to_number(sexp[2], env));
  },
  '<=': function(sexp, env) {
    assert(sexp.length === 3, lispy.crappyRender(sexp) + " arg count");
    return mkbool(evaluate_to_number(sexp[1], env) <= evaluate_to_number(sexp[2], env));
  },
  // IIRC 'if' needs to be a builtin in strictly evaluated languages
  'if': function(sexp, env) {
    assert(sexp.length === 4, lispy.crappyRender(sexp) + " arg count");
    var b = evaluate_to_bool(sexp[1], env);
    return (b ? lispy.evaluate(sexp[2], env) : lispy.evaluate(sexp[3], env));
  },
  // these create
  //'array'
  //'dict'
  //(mutable? then they'd need a name and stuff)
  'array': function(sexp, env) {
    var result = [];
    _.each(sexp.slice(1), function(v) {
      result.push(lispy.evaluate(v, env));
    });
    return mkarray(result);
  }
  /*'dict': function(sexp, env) {
    assert(sexp.length % 2 === 1, lispy.crappyRender(sexp) + " arg count is even");
    var result = {};
    var key = null;
    _.each(sexp, function(v) {
      if(key) {
        result[key]//...wait JS only has strings for keys. hmm.
      }
      result.push(lispy.evaluate(v, env));
    });
    return mkarray(result);
  }*/
};
var builtinsAsLispyThings = {};
_.each(builtins, function(val, key) {
  builtinsAsLispyThings[key] = lispy.wrapJSVal(val);
});
lispy.builtins = builtins;
lispy.builtinsAsLispyThings = builtinsAsLispyThings;

// TODO consider:
//what if all composite types (fn, list, assoc) got names
//let's see
//i'll have to set up indirection for:
//beta-reduce
//any builtins that operate on list, assoc

// Legal characters in lispy identifiers are the same as Scheme allows
// (mostly; even [:alnum:] doesn't work in JS regexps; this code ought to
// allow non-ASCII letters but it's too hard to do in browser JS to justify
// doing it in this language-prototype).
var identifierChar = /[\-!$%&*+.\/:<=>?@\^_~0-9a-zA-Z]/;
// following Haskell, pretend there are tabstops every 8 characters
// for the sake of defining what column a character is at:
var tabwidthForColumnCount = 8;
// following the most common conventions for line and column numbering:
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
      var result = parseList(toks, pos + 1, types.sexp);
      ourNest.push(result.parsed);
      pos = result.endPos;
    } else if(isLiteralValueToken(toks[pos])) {
      ourNest.push(toks[pos]);
      pos += 1;
    } else if((toks[pos].type === types.closeParen && type === types.sexp) ||
              (toks[pos].type === types.EOF && type === types.program)) {
      pos += 1;
      return { parsed: ourNest, endPos: pos };
    } else {
      //TODO maybe different error handling?
      throw ("parse failure at line " + toks[pos].line + " column " + toks[pos].column);
    }
  }
}

// parseProgram : string -> sexp
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


// copies all non-numeric own keys from orig to dest
function keepMetaDataFrom(orig, dest) {
  for(var key in orig) {
    if(_.has(orig, key) && !/^[0-9]+$/.test(key)) {
      dest[key] = orig[key];
    }
  }
  return dest;
}

//...larger arguments get (randomly)named, perhaps
lispy.betaReduceO_N = function(sexp) {
  // pattern-match ((fn (params...) body...) args...)
  assert(lispy.isHeadBetaReducible(sexp), "beta beta");
  //assert(sexp.type === types.sexp);
  //assert(sexp.length >= 1);
  var fn = sexp[0];
  var args = sexp.slice(1);
  //assert(fn.length > 2);
  //assert(fn[0].type === types.identifier);
  //assert(fn[0].string === 'fn');
  //assert(fn[1].type === types.sexp);
  var params = fn[1];
  var body = keepMetaDataFrom(fn, fn.slice(2));
  body.type = types.imperative;

  var substitutions = {};
  assert(params.length === args.length, lispy.crappyRender(sexp) + " equal params length"); //no silly stuff!
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

// TODO make fn be scoped identifier, or
// make the parser recognize it specially.
lispy.isLambdaLiteral = function(sexp) {
  return sexp.type === types.sexp &&
    sexp.length > 1 &&
    sexp[0].type === types.identifier &&
    sexp[0].string === 'fn' &&
    sexp[1].type === types.sexp;
};

lispy.isHeadBetaReducible = function(sexp) {
  return sexp.type === types.sexp &&
    sexp.length >= 1 && lispy.isLambdaLiteral(sexp[0]);
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

//env is a {} from identifier (as plain string) to sexp.
//It is used to look up free variables.
//(Strictly evaluated substitution e.g. betaReduceO_N
// cannot implement recursion or letrec,
// so env is necessary, not just conventional.
// It can be represented with a (let (...) ...)
// around the to-be-evaluated sexp if necessary.)
//BUG TODO- members of 'env' cannot have any free variables.
//I *think* pre-substituting them is fine but having a (env (...) ...)
//primitive would be fine (like 'let' but clears the scope) (or let for
//all the free variables also works fine) (assuming they *can* be referenced
//thus).
//
//returns the evaluated version of the sexp.
lispy.evaluate = function(sexp, env) {
  if(arguments.length === 1) {
    env = {};
  }
  while(true) {
    if(sexp.type === types.identifier && _.has(env, sexp.string)) {
      // substitute plain identifiers from env:
      //   ident
      sexp = env[sexp.string];
    }
    else if(sexp.type === types.sexp && sexp.length >= 1 &&
        sexp[0].type === types.identifier && _.has(env, sexp[0].string)) {
      // substitute function names that are about to be called from env:
      //   (ident ...)
      var headEvaledSexp = shallowCopyArray(sexp);
      headEvaledSexp[0] = env[sexp[0].string];
      sexp = headEvaledSexp;
    }
    else if(lispy.isHeadBetaReducible(sexp)) {
      // call lambda:
      //   ((fn (...) ...) ...)
      sexp = lispy.strictBetaReduceO_N(sexp, env);
    }
    else if(sexp.type === types.program) {
      // evaluate programs in sequence:
      //   (+ 1 2)
      //   (+ 3 4)
      sexp = keepMetaDataFrom(sexp, _.map(sexp, function(subsexp) {
        return lispy.evaluate(subsexp, env);
      }));
      break;
    }
    else if(sexp.type === types.imperative) {
      // evaluate function-bodies in sequence:
      //   (+ 1 2) (+ 3 4)
      var result = mkvoid();
      _.each(sexp, function(subsexp) {
        result = lispy.evaluate(subsexp, env);
      });
      sexp = result;
      break;
    }
    else if(sexp.type === types.sexp &&
        sexp[0].type === types.builtinFunction) {
      // evaluate builtins:
      //   (+ 1 2)
      sexp = sexp[0].value(sexp, env);
      break;
    }
    else if(sexp.type === types.sexp &&
        sexp[0].type === types.sexp) {
      // attempt to evaluate the function part:
      //   ((if true + -) 7 3)
      var headEvaled = lispy.evaluate(sexp[0], env);
      // TODO identity-based equality comparison is
      // fragile here? (to prevent infinite loop
      // trying to reduce something that we can't
      // reduce anymore)
      // TODO if function-calling called evaluate
      // once on its body then there would be no
      // infinite loop/recursion worries, right?
      // (aside from legitimately nonterminating
      // computations, of course).
      if(headEvaled === sexp[0]) {
        break;
      }
      else {
        var headEvaledSexp = shallowCopyArray(sexp);
        headEvaledSexp[0] = headEvaled;
        sexp = headEvaledSexp;
      }
    }
    else {
      // No reductions found: we must be done.
      break;
    }
  }
  sexp = lispy.bindFreeVars(sexp, env);
  return sexp;
};

function shallowCopyArray(arr) {
  return keepMetaDataFrom(arr, _.map(arr, _.identity));
}

lispy.strictBetaReduceO_N = function(sexp, env) {
  assert(lispy.isHeadBetaReducible(sexp), "strictBeta beta");
  var argsEvaledSexp = shallowCopyArray(sexp);
  for(var i = 1; i !== sexp.length; ++i) {
    argsEvaledSexp[i] = lispy.evaluate(sexp[i], env);
  }
  return lispy.betaReduceO_N(argsEvaledSexp);
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

wait, does it mutate or return a new sexp? return a new sexp. there might be sharing
of some tokens - we intend that they are never mutated by any code

can 'fn' be bound? that is not guarded against.
*/
lispy.substitute = function(varsToSexpsMap, sexp) {
  if(sexp.type === types.sexp || sexp.type === types.program || sexp.type === types.imperative) {
    if(lispy.isLambdaLiteral(sexp)) {
      var bindings = _.pluck(sexp[1], 'string');
      var subMap = _.omit(varsToSexpsMap, bindings);
      return keepMetaDataFrom(sexp, _.map(sexp, function(sub) {
        return lispy.substitute(subMap, sub);
      }));
    }
    else {
      return keepMetaDataFrom(sexp, _.map(sexp, function(sub) {
        return lispy.substitute(varsToSexpsMap, sub);
      }));
    }
  }
  else if(sexp.type === types.identifier && _.has(varsToSexpsMap, sexp.string)) {
    return varsToSexpsMap[sexp.string];
  }
  else { //other token
    return sexp;
  }
};

// returns a set { var: true, ... } of the free vars in sexp
// Asymptotic complexity is currently suboptimal.
lispy.freeVarsIn = function(sexp, boundVars) {
  if(boundVars === undefined) { boundVars = {}; }
  var freeVars = {};
  if(sexp.type === types.sexp || sexp.type === types.program || sexp.type === types.imperative) {
    if(lispy.isLambdaLiteral(sexp)) {
      var bindings = _.pluck(sexp[1], 'string');
      _.each(sexp.slice(2), function(sub) {
        _.extend(freeVars, lispy.freeVarsIn(sub, _.extend({}, bindings, boundVars)));
      });
    }
    else {
      _.each(sexp, function(sub) {
        _.extend(freeVars, lispy.freeVarsIn(sub, boundVars));
      });
    }
  }
  else if(sexp.type === types.identifier && !_.has(boundVars, sexp.string)) {
    freeVars[sexp.string] = true;
  }
  return freeVars;
};

lispy.bindFreeVars = function(sexp, env) {
  // We sort this for determinacy's sake.
  var varsToBind = _.keys(lispy.freeVarsIn(sexp)).sort();

  if(varsToBind.length === 0) {
    return sexp;
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
      //  throw "Unbound free var " + v + " in " + lispy.crappyRender(sexp);
      }
    });
    //TODO implement 'let' as syntactic sugar for such immediately-applied-function
    var paramsSexp = _.map(varsToBind, function(v) { return mkidentifier(v); });
    paramsSexp.type = types.sexp;
    var lambdaSexp = [mkidentifier('fn'), paramsSexp, sexp];
    lambdaSexp.type = types.sexp;
    var applySexp = [lambdaSexp].concat(bindings);
    applySexp.type = types.sexp;
    return applySexp;
  }
};

lispy.crappyRender = function(sexp) {
  var result;
  if(sexp.type === types.program || sexp.type === types.imperative) {
    result = '';
    _.each(sexp, function(subsexp) {
      result += lispy.crappyRender(subsexp);
      result += '\n';
    });
    return result;
  }
  else if(sexp.type === types.sexp) {
    result = '(';
    _.each(sexp, function(subsexp, index) {
      if(index !== 0) {
        result += ' ';
      }
      result += lispy.crappyRender(subsexp);
    });
    result += ')';
    return result;
  }
  else if(sexp.type === types.array) {
    result = '(array';
    _.each(sexp.value, function(subsexp) {
      result += ' ';
      result += lispy.crappyRender(subsexp);
    });
    result += ')';
    return result;
  }
  else if(sexp.type === types.dict) {
    result = '(dict';
    _.each(sexp.value, function(subsexpVal, subsexpKey) {
      result += ' ';
      result += lispy.crappyRender(subsexpKey);
      result += ' ';
      result += lispy.crappyRender(subsexpVal);
    });
    result += ')';
    return result;
  }
  else {
    return sexp.string;
  }
};

//lispy.betaReduceO_1

}());
