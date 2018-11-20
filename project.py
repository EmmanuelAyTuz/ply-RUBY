import ply.lex as lex
import re
import codecs
import os
import sys

# Lista de tokens
tokens = [
    # Variables
    'VARLOCAL','VAR_GLOBAL','VAR_INSTANT', 'VAR_CLASS',
    
    # Literales (Entero, Flotante, Cadena, Caracter)
    'INTEGER', 'FLOAT', 'STRING', 'CHARACTER',

    # Operadores (+,-,*,/,%,|,&,~,^,<<,>>, ||, &&, !, <, <=, >, >=, ==, !=)
    'PLUS', 'MINUS', 'MULTI', 'DIVIDE', 'MODULO',
    'OR', 'AND', 'NOT', 'XOR', 'LSHIFT', 'RSHIFT',
    'LOR', 'LAND', 'LNOT',
    'LESS_THAN', 'LESS_EQUAL', 'GREATER_THAN', 'GREATER_EQUAL', 'EQUAL_TO', 'NEGATIVE',

    # Asignacion (=, *=, /=, %=, +=, -=, <<=, >>=, &=, ^=, |=)
    'EQUALS', 'TIMESEQUAL', 'DIVEQUAL', 'MODEQUAL', 'PLUSEQUAL', 'MINUSEQUAL',
    'LSHIFTEQUAL','RSHIFTEQUAL', 'ANDEQUAL', 'XOREQUAL', 'OREQUAL',

    # Incremento/decrement0 (++,--)
    'INCREMENT', 'DECREMENT',

    # Desreferencia de estructura (->)
    'ARROW',

    # Operador ternario (?)
    'TERNARY',

    # Delimitadores ( ) [ ] { } , . ; :
    'LEFT_PAREN', 'RIGTH_PAREN',
    'LEFT_BRACKET', 'RIGTH_BRACKET',
    'LEFT_BRACE', 'RIGTH_BRACE',
    'COMMA', 'POINT', 'SEMICOLON', 'COLON',

    # Elipsis (...)
    'ELLIPSIS',

    # Palabras reservadas
    'BEGIN', 'END', 'alias', 'and', 'begin',
    'break', 'case', 'class', 'def', 'defined',
    'do', 'else', 'elsif', 'end', 'ensure',
    'false', 'for', 'if', 'in', 'module',
    'next', 'nil', 'not', 'or', 'redo',
    'rescue', 'retry', 'return', 'self', 'super',
    'then', 'true', 'undef', 'unless', 'until',
    'when', 'while', 'yield',

    # Más
    'FUNC'
]

# Enteros
t_INTEGER = r'\d+([uU]|[lL]|[uU][lL]|[lL][uU])?'

# Flotantes
t_FLOAT = r'((\d+)(\.\d+)(e(\+|-)?(\d+))? | (\d+)e(\+|-)?(\d+))([lL]|[fF])?'

# Cadena
t_STRING = r'\"([^\\\n]|(\\.))*?\"'

# Caracteres
t_CHARACTER = r'(L)?\'([^\\\n]|(\\.))*?\''

# Funciones
t_FUNC = r'gets|puts'

# Palabras reservadas de RUBY
t_BEGIN = r'\bBEGIN\b'
t_END = r'\bEND\b'
t_alias = r'\balias\b'
t_and = r'\band\b'
t_begin = r'\bbegin\b'

t_break = r'\bbreak\b'
t_case = r'\bcase\b'
t_class = r'\bclass\b'
t_def = r'\bdef\b'
t_defined = r'\bdefined?\b'

t_do = r'\bdo\b'
t_else = r'\belse\b'
t_elsif = r'\belsif\b'
t_end = r'\bend\b'
t_ensure = r'\bensure\b'

t_false = r'\bfalse\b'
t_for = r'\bfor\b'
t_if = r'\bif\b'
t_in = r'\bin\b'
t_module = r'\bmodule\b'

t_next = r'\bnext\b'
t_nil = r'\bnil\b'
t_not = r'\bnot\b'
t_or = r'\bor\b'
t_redo = r'\redo\b'

t_rescue = r'\brescue\b'
t_retry = r'\bretry\b'
t_return = r'\breturn\b'
t_self = r'\bself\b'
t_super = r'\bsuper\b'

t_then = r'\bthen\b'
t_true = r'\btrue\b'
t_undef = r'\bundef\b'
t_unless = r'\bunless\b'
t_until = r'\buntil\b'

t_when = r'\bwhen\b'
t_while = r'\bwhile\b'
t_yield = r'\byield\b'

# Variables Local, Global, Intantanea y Clase
#t_VARLOCAL = r'\b[a-z]+[a-zA-Z0-9]*(\s|\S)\='
t_VARLOCAL = r'_[a-z][a-zA-Z0-9]+'
t_VAR_GLOBAL = r'\$[\w]+'
t_VAR_INSTANT = r'\@[\w]+'
t_VAR_CLASS = r'\@@[\w]+'

# Operador ?
t_TERNARY          = r'\?'

# Operadores
t_PLUS             = r'\+'
t_MINUS            = r'-'
t_MULTI            = r'\*'
t_DIVIDE           = r'/'
t_MODULO           = r'%'
t_OR               = r'\|'
t_AND              = r'&'
t_NOT              = r'~'
t_XOR              = r'\^'
t_LSHIFT           = r'<<'
t_RSHIFT           = r'>>'
t_LOR              = r'\|\|'
t_LAND             = r'&&'
t_LNOT             = r'!'
t_LESS_THAN        = r'<'
t_GREATER_THAN     = r'>'
t_LESS_EQUAL       = r'<='
t_GREATER_EQUAL    = r'>='
t_EQUAL_TO         = r'=='
t_NEGATIVE         = r'!='

# Operadores de asignacion
t_EQUALS           = r'='
t_TIMESEQUAL       = r'\*='
t_DIVEQUAL         = r'/='
t_MODEQUAL         = r'%='
t_PLUSEQUAL        = r'\+='
t_MINUSEQUAL       = r'-='
t_LSHIFTEQUAL      = r'<<='
t_RSHIFTEQUAL      = r'>>='
t_ANDEQUAL         = r'&='
t_OREQUAL          = r'\|='
t_XOREQUAL         = r'\^='

# Incremento/decremento
t_INCREMENT        = r'\+\+'
t_DECREMENT        = r'--'

# ->
t_ARROW            = r'->'

# Delimitadores
t_LEFT_PAREN = r'\('
t_RIGTH_PAREN = r'\)'
t_LEFT_BRACKET = r'\['
t_RIGTH_BRACKET = r'\]'
t_LEFT_BRACE = r'\{'
t_RIGTH_BRACE = r'\}'
t_COMMA = r','
t_POINT = r'\.'
t_SEMICOLON = r';'
t_COLON = r':'
t_ELLIPSIS = r'\.\.\.'

# Ignorar espacios y tab
t_ignore = ' \t'

# Ignorar Comentarios
t_ignore_COMMENT = r'\#.*'

# Ignorar Saltos de linea
def t_newline(t):
   r'\r\n+'
   t.lexer.lineno += len(t.value)

# Captura de errores
def t_error(t):
    print ("Caracter ilegal: '%s'" %t.value[0])
    t.lexer.skip(1)
    
# Build the lexer
    def build(self,**kwargs):
        self.lexer = lex.lex(module=self, **kwargs)

# Seleccionar archivo
def searchFile(d):
    ficher = []
    numberFile = ''
    answer = False
    cont = 1
    print ("Lista de archivos:")
    for base, dirs, files in os.walk(d):
        ficher.append(files)
        
    for file in files:
        print (str(cont) + ". " + file)
        cont = cont + 1
        
    while answer == False:
       numberFile = input('\nArchivo: ')
       for file in files:
           if file == files[int(numberFile)-1]:
               answer = True
               break
    print ("Has escogido \"%s\" \n" %files[int(numberFile)-1])
    return files[int(numberFile)-1]

# Ubicacion
directory = 'test/' # Ruta
file = searchFile(directory)
test = directory + file
fp = codecs.open(test, "r", "ANSI")# Se puede cambiar por otro tipo de codificacion
cadena = fp.read()
fp.close()

analyzer = lex.lex()
analyzer.input(cadena)
print ("Codigo: ")
print (cadena)
print ("\n")

# Imprimir
while True:
    tok = analyzer.token()
    if not tok : break
    #print (tok)
    print ("###################" ,"\n  Tipo: %s" %tok.type , "\n  Valor: %s" %tok.value , "\n  Linea: %s" %tok.lineno, "\n  Posicion: %s" %tok.lexpos)
