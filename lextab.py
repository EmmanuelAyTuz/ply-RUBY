# lextab.py. This file automatically created by PLY (version 3.11). Don't edit!
_tabversion   = '3.10'
_lextokens    = set(('AND', 'ANDEQUAL', 'ARROW', 'CHARACTER', 'COLON', 'COMMA', 'DECREMENT', 'DIVEQUAL', 'DIVIDE', 'ELLIPSIS', 'EQ', 'EQUALS', 'FLOAT', 'GE', 'GT', 'ID', 'INCREMENT', 'INTEGER', 'LAND', 'LBRACE', 'LBRACKET', 'LE', 'LNOT', 'LOR', 'LPAREN', 'LSHIFT', 'LSHIFTEQUAL', 'LT', 'MINUS', 'MINUSEQUAL', 'MODEQUAL', 'MODULO', 'NE', 'NOT', 'OR', 'OREQUAL', 'PERIOD', 'PLUS', 'PLUSEQUAL', 'RBRACE', 'RBRACKET', 'RESERVEDWORD_BEGIN', 'RESERVEDWORD_ENCODING', 'RESERVEDWORD_END', 'RESERVEDWORD_FILE', 'RESERVEDWORD_LINE', 'RESERVEDWORD_alias', 'RESERVEDWORD_and', 'RESERVEDWORD_begin', 'RESERVEDWORD_break', 'RESERVEDWORD_case', 'RESERVEDWORD_class', 'RESERVEDWORD_def', 'RESERVEDWORD_defined', 'RESERVEDWORD_do', 'RESERVEDWORD_else', 'RESERVEDWORD_elsif', 'RESERVEDWORD_end', 'RESERVEDWORD_ensure', 'RESERVEDWORD_false', 'RESERVEDWORD_for', 'RESERVEDWORD_if', 'RESERVEDWORD_in', 'RESERVEDWORD_module', 'RESERVEDWORD_next', 'RESERVEDWORD_nil', 'RESERVEDWORD_not', 'RESERVEDWORD_or', 'RESERVEDWORD_redo', 'RESERVEDWORD_rescue', 'RESERVEDWORD_retry', 'RESERVEDWORD_return', 'RESERVEDWORD_self', 'RESERVEDWORD_super', 'RESERVEDWORD_then', 'RESERVEDWORD_true', 'RESERVEDWORD_undef', 'RESERVEDWORD_unless', 'RESERVEDWORD_until', 'RESERVEDWORD_when', 'RESERVEDWORD_while', 'RESERVEDWORD_yield', 'RPAREN', 'RSHIFT', 'RSHIFTEQUAL', 'SEMI', 'STRING', 'TERNARY', 'TIMES', 'TIMESEQUAL', 'TYPEID', 'XOR', 'XOREQUAL'))
_lexreflags   = 64
_lexliterals  = ''
_lexstateinfo = {'INITIAL': 'inclusive'}
_lexstatere   = {'INITIAL': [('(?P<t_newline>\\r\\n+)|(?P<t_ID>[a-zA-Z_][a-zA-Z_0-9]*)|(?P<t_FLOAT>((\\d+)(\\.\\d+)(e(\\+|-)?(\\d+))? | (\\d+)e(\\+|-)?(\\d+))([lL]|[fF])?)|(?P<t_INTEGER>\\d+([uU]|[lL]|[uU][lL]|[lL][uU])?)|(?P<t_CHARACTER>(L)?\\\'([^\\\\\\n]|(\\\\.))*?\\\')|(?P<t_STRING>\\"([^\\\\\\n]|(\\\\.))*?\\")|(?P<t_ELLIPSIS>\\.\\.\\.)|(?P<t_LOR>\\|\\|)|(?P<t_INCREMENT>\\+\\+)|(?P<t_ignore_COMMENT>\\#.*)|(?P<t_TIMESEQUAL>\\*=)|(?P<t_PLUSEQUAL>\\+=)|(?P<t_LSHIFTEQUAL><<=)|(?P<t_RSHIFTEQUAL>>>=)|(?P<t_OREQUAL>\\|=)|(?P<t_XOREQUAL>\\^=)|(?P<t_TERNARY>\\?)|(?P<t_PLUS>\\+)|(?P<t_TIMES>\\*)|(?P<t_OR>\\|)|(?P<t_XOR>\\^)|(?P<t_LSHIFT><<)|(?P<t_RSHIFT>>>)|(?P<t_LAND>&&)|(?P<t_LE><=)|(?P<t_GE>>=)|(?P<t_EQ>==)|(?P<t_NE>!=)|(?P<t_DIVEQUAL>/=)|(?P<t_MODEQUAL>%=)|(?P<t_MINUSEQUAL>-=)|(?P<t_ANDEQUAL>&=)|(?P<t_DECREMENT>--)|(?P<t_ARROW>->)|(?P<t_LPAREN>\\()|(?P<t_RPAREN>\\))|(?P<t_LBRACKET>\\[)|(?P<t_RBRACKET>\\])|(?P<t_LBRACE>\\{)|(?P<t_RBRACE>\\})|(?P<t_PERIOD>\\.)|(?P<t_MINUS>-)|(?P<t_DIVIDE>/)|(?P<t_MODULO>%)|(?P<t_AND>&)|(?P<t_NOT>~)|(?P<t_LNOT>!)|(?P<t_LT><)|(?P<t_GT>>)|(?P<t_EQUALS>=)|(?P<t_COMMA>,)|(?P<t_SEMI>;)|(?P<t_COLON>:)', [None, ('t_newline', 'newline'), ('t_ID', 'ID'), (None, 'FLOAT'), None, None, None, None, None, None, None, None, None, None, (None, 'INTEGER'), None, (None, 'CHARACTER'), None, None, None, (None, 'STRING'), None, None, (None, 'ELLIPSIS'), (None, 'LOR'), (None, 'INCREMENT'), (None, None), (None, 'TIMESEQUAL'), (None, 'PLUSEQUAL'), (None, 'LSHIFTEQUAL'), (None, 'RSHIFTEQUAL'), (None, 'OREQUAL'), (None, 'XOREQUAL'), (None, 'TERNARY'), (None, 'PLUS'), (None, 'TIMES'), (None, 'OR'), (None, 'XOR'), (None, 'LSHIFT'), (None, 'RSHIFT'), (None, 'LAND'), (None, 'LE'), (None, 'GE'), (None, 'EQ'), (None, 'NE'), (None, 'DIVEQUAL'), (None, 'MODEQUAL'), (None, 'MINUSEQUAL'), (None, 'ANDEQUAL'), (None, 'DECREMENT'), (None, 'ARROW'), (None, 'LPAREN'), (None, 'RPAREN'), (None, 'LBRACKET'), (None, 'RBRACKET'), (None, 'LBRACE'), (None, 'RBRACE'), (None, 'PERIOD'), (None, 'MINUS'), (None, 'DIVIDE'), (None, 'MODULO'), (None, 'AND'), (None, 'NOT'), (None, 'LNOT'), (None, 'LT'), (None, 'GT'), (None, 'EQUALS'), (None, 'COMMA'), (None, 'SEMI'), (None, 'COLON')])]}
_lexstateignore = {'INITIAL': ' \t'}
_lexstateerrorf = {'INITIAL': 't_error'}
_lexstateeoff = {}
