# ------------------------------------------------------------
# Universidad Simón Bolívar
# CI-3725 Traductores e Interpretadores
# Proyecto. Etapa 1. Analisis Lexicográfico
# Estudiantes:
# Jesús Prieto 19-10211
# Jesús Cuéllar 15-10345
# lexer.py
# Analizador lexicografico para el lenguaje GCL
<<<<<<< HEAD
# Carnet: 15-10345 y 19-10211
=======
# Se contruye una analizador lexicográfico para GCL en el 
# lenguaje Python para GCL implementando PLY
# Documentación PLY: http://www.dabeaz.com/ply/ 
>>>>>>> 16866ddaa7b7d97ec8e02c41b3e26beca9a8145f
# ------------------------------------------------------------
import sys
if ".." not in sys.path: sys.path.insert(0,"..")
import ply.lex as lex
import ply.yacc as yacc

# Lista con los token de las palabras reservadas para GCL
reserved = {
   'declare' : 'TkDeclare',
   'if' : 'TkIf',
   'fi' : 'TkFi',
   'do' : 'TkDo',
   'od' : 'TkOd',
   'for' : 'TkFor',
   'rof' : 'TkRof',
   'int' : 'TkInt',
   'bool' : 'TkBool',
   'array' : 'TkArray',
   'skip' : 'TkSkip',
   'print' : 'TkPrint',
   'ERROR' : 'TkError',
   'false' : 'TkFalse',
   'true'  : 'TkTrue',
   'in'    : 'TkIn',
   'to'    : 'TkTo'
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
   'TkClosePar',
   'TkAsig',
   'TkSemicolon',
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
   'TkComent'
) 

# Reglas de las expresiones regulares para cada token

t_TkString = r'\"(\\\"|\\\\|\\n|[^\\\n])*?\"'
t_TkOBlock = r'[|]\['
t_TkCBlock = r'\][|]'
t_TkSoForth = r'\.\.'
t_TkComma = r','
t_TkOpenPar = r'\('
t_TkClosePar = r'\)'
t_TkAsig = r':='
t_TkSemicolon = r';'
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
t_TkConcat = r'[.]'

# Expresiones regulares que poseeen alguna acción extra

# Función t_TkId. Indentifica si es una variable o una palabra reservada. 
def t_TkId(t):
    r'([a-zA-Z] | _)[a-zA-Z0-9]*(_[a-zA-Z0-9]+)*[_]*' 
    t.type = reserved.get(t.value,'TkId')
    return t
#Función t_TkNum. Indentifica si es número y si lo es lo pasa a tipo int
# En caso contrario sitia el tipo del token como ERROR
def t_TkNum(t):
    r'[0-9][0-9]*[a-zA-Z0-9_]*'
    try:
        t.value = int(t.value)
        return t
    except:
        t_error(t)         

# Función t_newline
# Cuenta las filas del archivo a leer
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")   

def t_TkComent(t):
    r'//.*'     # Ignora las oraciones que empiezan con //
    pass

# Manejador de errores
def t_error(t):
    print (f"Error: Unexpected character \"{t.value[0]}\" in row {t.lineno}, column {t.lexpos+1}")
    error.sum()
    t.lexer.skip(1)

<<<<<<< HEAD
# Ignora el tabulador  
t_ignore = ' \t'
=======
# Ignora tabulaciones y espacios
t_ignore = ' \t' 

# Ignora Comentarios
def t_Coment(t):
    r'//.*'
    pass
>>>>>>> 16866ddaa7b7d97ec8e02c41b3e26beca9a8145f

# Crear clase que permita contar los errores encontrados
class Error_Counter: 
    def __init__(self, error = 0): 
         self._error = error 
      
    # metodo getter
    def get_value(self): 
        return self._error
    # metodo para aumentar en 1
    def sum(self): 
        self._error = self.get_value() +1
 
# Construye el lexer

lexer = lex.lex(optimize=1, lextab= "compilador")

# Abre el archivo

error = Error_Counter()
'''
try:
<<<<<<< HEAD
    f = open(sys.argv[1], "r")   
    assert f.name.endswith('.gcl') # Verifica que sea un .gcl
    content = f.readlines()
    f.close()
    # Primer analisis, se buscan posibles errores
=======
    error = Error_Counter()
    f = open(sys.argv[1], "r")
    assert f.name.endswith('.gcl') #assert que verifica la extensión
    content = f.readlines()
    f.close()
    lexer.lineno = 1
    # Se buscan errores en un primer recorrido.
>>>>>>> 16866ddaa7b7d97ec8e02c41b3e26beca9a8145f
    for x in content:
        lexer.input(x) # Crear tokens
        while True:
            tok = lexer.token() # Tomar un token
            if not tok: 
                break      # Se acabo la linea
<<<<<<< HEAD

    # En caso de no tener error procedemos a imprimir cada token

=======
    #Si no hay errores se hace otro recorrido e imprime los tokens
>>>>>>> 16866ddaa7b7d97ec8e02c41b3e26beca9a8145f
    if error.get_value() == 0:
        lexer.lineno = 1
        for x in content:
            lexer.input(x) # Crear tokens
            while True:
                tok = lexer.token() # Tomar un token
                if not tok: 
                    break      # Se acabo la linea
                else:
                    traduccion_lexer.append(tok.type)
                    traduccion_lexer.append(tok.value)
                    if tok.type == 'TkId':
                        print(f"{tok.type}(\"{tok.value}\") {tok.lineno} {tok.lexpos+1}")
                    elif tok.type == 'TkNum' or tok.type == 'TkString':
                        print(f"{tok.type}({tok.value}) {tok.lineno} {tok.lexpos+1}")
                    else:
                        print(f"{tok.type} {tok.lineno} {tok.lexpos+1}")  
except:
    # Caso donde no se consiguio el archivo o no lo indico
    print("Archivo no encontrado o no es de extensión .gcl, indique un archivo para analizar")
'''