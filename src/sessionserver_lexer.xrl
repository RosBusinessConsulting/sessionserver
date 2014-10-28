Definitions.

VAR = ([a-zA-Z0-9!@#$%^&*()_+\-=\[\]{};':"\\|,.<>\/?])
WS  = ([\000-\s]|%.*)

Rules.

CREATE_SESSION  : {token, {create, TokenLine, list_to_atom(TokenChars)}}.
DELETE_SESSION  : {token, {delete, TokenLine, list_to_atom(TokenChars)}}.
CHECK_SESSION   : {token, {check, TokenLine, list_to_atom(TokenChars)}}.
VERSION         : {token, {version, TokenLine, list_to_atom(TokenChars)}}.

{VAR}+          : {token, {var, TokenLine, list_to_atom(TokenChars)}}.

{WS}+           : skip_token.

Erlang code.