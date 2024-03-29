%%

begin            "BEGIN"
end              "END"
read             "READ"
write            "WRITE"
if               "IF"
then             "THEN"
else             "ELSE"
endif            "ENDIF"
do               "DO"
while            "WHILE"
endwhile         "ENDWHILE"
continue         "CONTINUE"
break            "BREAK"
repeat           "REPEAT"
until            "UNTIL"
decl             "DECL"
enddecl          "ENDDECL"
type             "TYPE"
endtype          "ENDTYPE"
class            "CLASS"
endclass         "ENDCLASS"
extends          "EXTENDS"
new              "NEW"
delete           "DELETE"
self             "SELF"
int              "INT_T"
str              "STRING_T"
main             "MAIN"
return           "RETURN"
alloc            "ALLOC"
free             "FREE"
initialize       "INIT"
null             "NULL"
exposcall        "EXPOS"

\+               "+"
\-               "-"
\*               "*"
/                "/"
%                "%"
=                "="
==               "=="
!=               "!="
<                "<"
>                ">"
<=               "<="
>=               ">="
&                "&"
&&               "&&"
\|\|             "||"
\.               "."

\(               "("
\)               ")"
\[               "["
\]               "]"
\{               "{"
\}               "}"
,                ","
;                ";"

[\pL_]\w*       "VAR"
[0-9]+          "NUM"
["][^\n]*?["]   "STRING_C"

//[^\n]*        ;
[\s]+           ;

.               "UNMATCHED"
