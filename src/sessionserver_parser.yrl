Nonterminals 
line param.

Terminals
var create check delete version.

Rootsymbol line.

line -> create param param : {create, '$2', '$3'}.
line -> delete param : {delete, '$2'}.
line -> check param : {check, '$2'}.
line -> version param : {version, '$2'}.

param -> var : unwrap('$1').

Erlang code.

unwrap({_, _, V}) -> V.