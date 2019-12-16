// node.js unix "filter" which 
//   takes a input a one line containing a JSrealB expression (must be at least one non-space char long)
//   returns its English realisation
// call: node filter-enfr.js

//////// 
//  load JSrealB
var fs = require('fs');
var jsrealb=require('/Users/lapalme/Documents/GitHub/jsRealB/dist/jsRealB-node.min.js');
// eval exports 
for (var v in jsrealb){
    eval("var "+v+"=jsrealb."+v);
}

loadEn();

// add a few English words used for the unit testing of Gophi
addToLexicon("arms",{"N":{"tab":["n6"]}});
addToLexicon("football",{"N":{"tab":["n1"]}});
addToLexicon("giggle",{"N":{"tab":["n1"]},"V":{"tab":"v3"}});
addToLexicon("number",{"N":{"tab":["n1"]},"V":{"tab":"v1"}});
addToLexicon("tsunami",{"N":{"tab":["n1"]}});
addToLexicon("web",{"N":{"tab":["n1"]}});
addToLexicon("whistle",{"N":{"tab":["n1"]},"V":{"tab":"v3"}});


var readline = require('readline');
var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

rl.on('line', function(line){
    line=line.trim();
    if (line.length>0)
        console.log(eval(line).toString());
})

// try this as input
// S(NP(D("the"),N("man")),VP(V("love")))
// it should return
// The man loves.