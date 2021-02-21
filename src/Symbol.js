"use strict";

exports.symbol = description => Symbol(description);

exports.eqCompare = a => b => a === b;

exports.getDescription = symbol => symbol.description;
