/*defintion*/
%option c++
%option yylineno

%{
    enum token_type {
        eof = 0,
        none,
        label,
        directive,
        mnemonic,
	pseudo_mnemonic,
        identifier,
        hex_integer,
        decimal_integer,
        binary_integer,
        character,
        reg,
        string,
        newline,
        lbracket,
        rbracket,
        comma,
	reloc,
	integer_label,
	integer_identifier,
	add,
	macro_identifier,
	lsft
  };
%}

DIGIT [0-9]
BIT [0-1]
STRING \"([^\\\"]|\\.)*\"
HEX_DIGIT [0-9A-F]|[0-9a-f]
ALPHA_NUM [a-zA-Z0-9]
ALPHA [a-zA-Z]
IDENTIFIER (_|\.|{ALPHA})(_|\.|{ALPHA_NUM})*
LABEL {IDENTIFIER}:
INTEGER \-?{DIGIT}+
INTEGER_LABEL {DIGIT}+:
INTEGER_IDENTIFIER {DIGIT}+(b|f)
MACRO_IDENTIFIER \\{IDENTIFIER}
HEX_INTEGER \-?0x{HEX_DIGIT}+
BIN_INTEGER \-?0b{BIT}+
LBRACKET \(
RBRACKET \)
COMMA ,
CHAR \-?\'([^\\\']|\\.)*\'
ADD \+
LSHFT <<

PARAMETER <{DIGIT}+>

REG (x[0-9]|x1[0-9]|x2[0-9]|x3[0-1]|zero|ra|sp|gp|tp|t[0-6]|fp|s[0-9]|s1[0-1]|a[0-7])

RELOC(\%pcrel_hi|\%pcrel_lo|\%hi|\%lo)


%%

(\/\/.*) {return none;}
(\;\;.*) {return none;}
(#.*) {return none;}
{ADD} {return add;}
{STRING} {return string;}
{REG} {return reg;}
{INTEGER} {return decimal_integer;}
{HEX_INTEGER} {return hex_integer;}
{BIN_INTEGER} {return binary_integer;}
{RELOC} {return reloc;}
{LABEL} {return label;}
{INTEGER_LABEL} {return integer_label;}
{IDENTIFIER} {return identifier;}
{INTEGER_IDENTIFIER} {return integer_identifier;}
{MACRO_IDENTIFIER} {return macro_identifier;}
{CHAR} {return character;}
{COMMA} {return comma;}
{LBRACKET} {return lbracket;}
{RBRACKET} {return rbracket;}
{LSHFT} {return lsft;}
<<EOF>> {return eof;}
[\n]+ {return newline;}
[\t]+ {return none;}
. {return none;}

%%

int yyFlexLexer::yywrap() {return 0;}
