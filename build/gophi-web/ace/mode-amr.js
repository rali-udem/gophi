/* 
   Ace mode for AMRs (by adapting the lisp-mode)
   Guy Lapalme 
*/


define("ace/mode/amr_highlight_rules",["require","exports","module","ace/lib/oop","ace/mode/text_highlight_rules"],function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var AmrHighlightRules = function() {
    this.$rules = 
        {
    "start": [
        {
            token : "comment",
            regex : ";.*$"
        },
        {
            token: ["variable","space","slash","space","concept"],
            regex: "([a-z][a-z0-9]?)(\\s*)(/)(\\s*)([-\\w\\d]+)"
        },
        {
            token: "role",
            regex: ":[\\w-]+"
        },
        {
            token: "variable",
            regex: "[a-z][a-z0-9]?"
        },
        {
            token : "number", // float
            regex : "[+-]?\\d+(?:(?:\\.\\d*)?(?:[eE][+-]?\\d+)?)?\\b"
        },
        {
            token : "string",
            regex : '"(?=.)',
            next  : "qqstring"
        }
    ],
    "qqstring": [
        {
            token: "constant.character.escape.amr",
            regex: "\\\\."
        },
        {
            token : "string",
            regex : '[^"\\\\]+'
        }, {
            token : "string",
            regex : "\\\\$",
            next  : "qqstring"
        }, {
            token : "string",
            regex : '"|$',
            next  : "start"
        }
    ]
};

};

oop.inherits(AmrHighlightRules, TextHighlightRules);

exports.AmrHighlightRules = AmrHighlightRules;
});

define("ace/mode/amr",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/amr_highlight_rules"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var AmrHighlightRules = require("./amr_highlight_rules").AmrHighlightRules;

var Mode = function() {
    this.HighlightRules = AmrHighlightRules;
    this.$behaviour = this.$defaultBehaviour;
};
oop.inherits(Mode, TextMode);

(function() {
    this.lineCommentStart = ";";
    this.$id = "ace/mode/amr";
}).call(Mode.prototype);

exports.Mode = Mode;
});
                (function() {
                    window.require(["ace/mode/amr"], function(m) {
                        if (typeof module == "object" && typeof exports == "object" && module) {
                            module.exports = m;
                        }
                    });
                })();
