
(function() {
"use strict";

var lispy = (window || exports).lispy = {};

var assert = function(b, str) {
  if(!b) {
    throw ("assert failure! " + (""+str));
  }
};

var tokenType = {  //strs easier for debugging, objs maybe faster
  openParen: "openParen",//{},
  closeParen: "closeParen",//{},
  number: "number",//{},
  identifier: "identifier",//{},
  comment: "comment",//{}, //TODO
  string: "string",//{}, //TODO
  EOF: "EOF"//{}
};

var compositeType = {
  list: "list",//{},
  program: "program"//{}
};

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
    } else if(str[pos] === '(') {
      token({type: tokenType.openParen}, 1);
    } else if(str[pos] === ')') {
      token({type: tokenType.closeParen}, 1);
    } else if(/[\-.]?[0-9]/.test(str[pos])) {
      var numlen = 1;
      while(pos + numlen < str.length && /[\-0-9a-zA-z_,.]/.test(str[pos + numlen])) {
        ++numlen;
      }
      // TODO use our own not JS's hex/oct/dec number syntax?
      // TODO int vs non integer numbers?
      var numval = parseInt(str.slice(pos, numlen));
      // TODO forbid 12&& being a number token followed by an
      // identifier token without any spaces?  Ah by eating up
      // a whole identifier and then if it begins number-like
      // then make it a number or fail.
      token({type: tokenType.number, value: numval}, numlen);
    } else if(identifierChar.test(str[pos])) {
      var idlen = 1;
      while(pos + idlen < str.length && identifierChar.test(str[pos + idlen])) {
        ++idlen;
      }
      token({type: tokenType.identifier}, idlen);
    } else {
      throw ("tokenizer fail at line " + line + " column " + column + "!");
    }
  }
  token({type: tokenType.EOF}, 0);
  //console.log(result);
  return result;
}


// Returns { parsed: list of sub-lists or tokens, endPos: n } with endPos one-after-end
function parseList(toks, pos, type) {
  var ourNest = [];
  ourNest.type = type;
  while(true) {
    if(toks[pos].type === tokenType.openParen) {
      var result = parseList(toks, pos + 1, compositeType.list);
      ourNest.push(result.parsed);
      pos = result.endPos;
    } else if(toks[pos].type === tokenType.number || toks[pos].type === tokenType.identifier) {
      ourNest.push(toks[pos]);
      pos += 1;
    } else if((toks[pos].type === tokenType.closeParen && type === compositeType.list) ||
              (toks[pos].type === tokenType.EOF && type === compositeType.program)) {
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
  return parseList(tokenize(str), 0, compositeType.program).parsed;
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
  //assert(tree.type === compositeType.list);
  //assert(tree.length >= 1);
  var fn = tree[0];
  var args = tree.slice(1);
  //assert(fn.length > 2);
  //assert(fn[0].type === tokenType.identifier);
  //assert(fn[0].string === 'fn');
  //assert(fn[1].type === compositeType.list);
  var params = fn[1];
  var body = keepMetaDataFrom(fn, fn.slice(2));
  body.type = compositeType.program;

  var substitutions = {};
  assert(params.length === args.length, "equal params length"); //no silly stuff!
  for(var i = 0; i !== params.length; ++i) {
    assert(params[i].type === tokenType.identifier, "params are identifiers");
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
lispy.isHeadBetaReducible = function(tree) {
  return tree.type === compositeType.list &&
    tree.length >= 1 && tree[0].type === compositeType.list &&
    tree[0].length > 2 &&
    tree[0][0].type === tokenType.identifier &&
    tree[0][0].string === 'fn' &&
    tree[0][1].type === compositeType.list;
};


// lispy.rep("((fn (x) (x x)) (fn (x) (x x)))")
// lispy.crappyRender(lispy.evaluate(lispy.parseProgram("((fn (x y) y (x y)) (fn (x) x) 34)")))
// lispy.crappyRender(lispy.evaluate(lispy.parseProgram("((fn (x y) y) 23 34)")[0]))
lispy.evaluate = function(tree) {
  while(true) {
    if(lispy.isHeadBetaReducible(tree)) {
      tree = lispy.strictBetaReduceO_N(tree);
    }
    else if(tree.type === compositeType.program) {
      return keepMetaDataFrom(tree, _.map(tree, lispy.evaluate));
    }
    else {
      return tree;
    }
  }
};

function shallowCopyArray(arr) {
  return keepMetaDataFrom(arr, _.map(arr, _.identity));
}

lispy.strictBetaReduceO_N = function(tree) {
  assert(lispy.isHeadBetaReducible(tree), "strictBeta beta");
  var argsEvaledTree = shallowCopyArray(tree);
  for(var i = 1; i !== tree.length; ++i) {
    argsEvaledTree[i] = lispy.evaluate(tree[i])
  }
  return lispy.betaReduceO_N(argsEvaledTree);
};

/*
  substitute({
    'foo': { type: tokenType.number, value: 3, string: "3", ... },
    'bar': { type: tokenType.identifier, string: "something", ... },
  },
  [ { type: tokenType.identifier, string: "+", ... },
    { type: tokenType.identifier, string: "foo", ... },
    { type: tokenType.number, value: 7, string: "7", ... }
  ])

wait, does it mutate or return a new tree? return a new tree. there might be sharing
of some tokens - we intend that they are never mutated by any code

can 'fn' be bound? that is not guarded against.
*/
lispy.substitute = function(varsToTreesMap, tree) {
  if(tree.type === compositeType.list || tree.type === compositeType.program) {
    if(tree[0].type === tokenType.identifier && tree[0].string === 'fn') {
      var fn = tree;
      assert(fn.length > 2, "substitute: fn binding is fn, 1");
      assert(fn[1].type === compositeType.list, "substitute: fn binding is fn, 2");
      var bindings = _.pluck(fn[1], 'string');
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
  else if(tree.type === tokenType.identifier && _.has(varsToTreesMap, tree.string)) {
    return varsToTreesMap[tree.string];
  }
  else { //other token
    return tree;
  }
};

lispy.crappyRender = function(tree) {
  var result;
  if(tree.type === compositeType.program) {
    result = '';
    _.each(tree, function(subtree) {
      result += lispy.crappyRender(subtree);
      result += '\n';
    });
    return result;
  }
  else if(tree.type === compositeType.list) {
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
