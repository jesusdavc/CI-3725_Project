
# ------------------------------------------------------------
# lexer.py
# Analizador lexicografico para el lenguaje GCL
# ------------------------------------------------------------
import sys

if ".." not in sys.path: sys.path.insert(0,"..")
import ply.lex as lex

# Lista con los token de las palabras reservadas para GCL
reserved = {
   'declare' : 'TkDeclare',
   'if' : 'TkIf',
   'fi' : 'TkFi',
   'do' : 'TkDo',
   'od' : 'TkOd',
   'for' : 'TkFor',
   'rof' : 'TkOrf',
   'int' : 'TkInt',
   'bool' : 'TkBool',
   'array' : 'TkArray',
   'skip' : 'TkSkip',
   'print' : 'TkPrint',
   'ERROR' : 'TkError'
}

# Lista de los nombres de los tokens. Siempre es requerido
tokens = tuple(reserved.values()) + (
   'TkId',
   'TkNum',
   'TkString',
   'TkOBlock',
   'TkCBlock',
   'TkSoForth',
   'TkComma',
   'TkOpenPar',
   'TkColsePar',
   'TkAsig',
   'TKSemicolon',
   'TkArrow',
   'TkGuard',
   'TkPlus',
   'TkMinus',
   'TkMult',
   'TkOr',
   'TkAnd',
   'TkNot',
   'TkLess',
   'TkLeq',
   'TkGeq',
   'TkGreater',
   'TkEqual',
   'TkNEqual',
   'TkOBracket',
   'TkCBracket',
   'TkTwoPoints',
   'TkConcat',
) 

# Reglas de las expresiones regulares para cada token

t_TkString = r'".*"'
t_TkOBlock = r'[|]\['
t_TkCBlock = r'\][|]'
t_TkSoForth = r'\.\.'
t_TkComma = r','
t_TkOpenPar = r'\('
t_TkColsePar = r'\)'
t_TkAsig = r':='
t_TKSemicolon = r';'
t_TkArrow = r'-->'
t_TkGuard = r'\[\]'
t_TkPlus = r'\+'
t_TkMinus = r'-'
t_TkMult = r'\*'
t_TkOr = r'\\/'
t_TkAnd = r'/\\'
t_TkNot = r'\!'
t_TkLess = r'<'
t_TkLeq = r'<='
t_TkGeq = r'>='
t_TkGreater = r'>'
t_TkEqual = r'=='
t_TkNEqual = r'\!='
t_TkOBracket = r'\['
t_TkCBracket = r'\]'
t_TkTwoPoints = r'\:'
t_TkConcat = r'\.'

# Expresiones regulares que poseeen alguna acción extra


def t_TkId(t):
    r'[a-zA-Z][a-zA-Z0-9]*(_[a-zA-Z0-9]+)*' # Identifica si es una variable o una palabra reservada
    t.type = reserved.get(t.value,'TkId')
    return t

def t_TkNum(t):
    r'[0-9][0-9]*[a-zA-Z0-9_]*' # Identifica si es un numero
    try:
        t.value = int(t.value) # En caso de serlo conviertelo a tipo int
        return t
    except:
        #t.type = 'ERROR'        # En caso contrario sitia el tipo del token como ERROR
        t_error(t)
        return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")   # Cuenta las filas del archivo a leer

# Manejador de errores
def t_error(t):
    t.type = 'ERROR'
    print (f"Error: Unexpected character \"{t.value[0]}\" in row {t.lineno}, column {t.lexpos+1}")
    t.lexer.skip(1)

def t_Tabulador(t):
    r'\t+ | \s'
    pass

def t_Coment(t):
    r'//.*'
    pass

# Construye el lexer
lexer = lex.lex(optimize=1, lextab= "compilador")

# Abre el archivo
try:
    f = open(sys.argv[1], "r")
    for x in f:
        error = ''
        lexer.input(x) # Crear tokens
        while True:
            tok = lexer.token() # Tomar un token
            if not tok: 
                break      # Se acabo la linea
            
            if tok.type == "ERROR":
                error = tok.type
            
            if error != 'ERROR':
                if tok.type == ('TkId' or 'TkNum' or 'TkString'):
                    print(f"{tok.type} ({tok.value}) {tok.lineno} {tok.lexpos}")
                else:
                    print(f"{tok.type} {tok.lineno} {tok.lexpos}")     
        
    f.close()
except:
    # Caso donde no se consiguio el archivo o no lo indico
    print("Archivo no encontrado, indique un archivo para analizar")