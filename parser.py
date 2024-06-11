# ------------------------------------------------------------
# parser.py
# Generador de arbol AST para el lenguaje GCL
# Carnet: 15-10345 y 19-10211
# ------------------------------------------------------------
import ply.lex as lex
import ply.yacc as yacc
import sys
from lexer import tokens, Error_Counter, reserved
from collections import deque
if ".." not in sys.path: sys.path.insert(0,"..") # type: ignore

# Precedencia de algunas expresiones de GCL
precedence = (
    ('rigth', 'TkSemicolon'),
    ('left', 'TkTwoPoints'),
    ('left', 'TkComma'),
)

# Produccion para detectar un programa en GCL
def p_program(p):
    "program : TkOBlock expresion TkCBlock"
    p[0] = Node("block", p[2])
    print("block: "+ p[2].type)

# Produccion para detectar la expresion terminal declare
def p_expresion_declare(p):
    "expresion : TkDeclare expresion"
    p[0] = Node("declare", p[2])
    print("declare: "+ p[1] + p[2].type )

# Produccion para detectar la expresion terminal TwoPoints
def p_expresion_comma(p):
    '''expresion : expresion TkComma expresion
                 | number TkComma number
                 | expresion TkComma number
                 | number TkComma expresion'''
    p[0] = Node("Coma", p[1], p[3])
    print("coma nivel: "+ str(p[1].type) + "," + str(p[3].type))

# Produccion para detectar la expresion las secuencias del programa
def p_expresion_semicolon(p):
    '''expresion : expresion TkSemicolon expresion
                 | reserved TkSemicolon reserved
                 | expresion TkSemicolon reserved
                 | reserved TkSemicolon expresion'''
    p[0] = Node("Secuencia", p[1], p[3])
    print("semicolon nivel: "+p[1].type + ";" + p[3].type)

# Produccion para detectar la expresion no terminal TwoPoints
def p_expresion_two_point(p):
    '''expresion : expresion TkTwoPoints expresion
                 | expresion TkTwoPoints reserved
                 | reserved TkTwoPoints reserved
                 | reserved TkTwoPoints expresion
                 | expresion TkTwoPoints number
                 | number TkTwoPoints expresion
                 | number TkTwoPoints number'''
    p[0] = Node("Dos puntos", p[1], p[3])
    print("dos puntos nivel: "+p[1].type +","+ p[3].type) 

# Produccion para detectar la expresion asignacion
def p_asignation(p):
    '''expresion : expresion TkAsig expresion
                 | expresion TkAsig number'''
    p[0] = Node("Asignacion", p[1], p[3])
    print("asignacion: "+p[1].type+","+str(p[3].type))

# Produccion para detectar la expresion terminales reservadas
def p_reserved(p):
    '''reserved : TkIf
                | TkFi
                | TkDo
                | TkOd
                | TkFor
                | TkRof
                | TkInt
                | TkBool
                | TkArray rango
                | TkArray rango expresion
                | TkSkip
                | TkPrint
                | TkError
                | TkFalse
                | TkTrue
                | TkIn
                | TkTo'''
    if (p[1] == "array" and p[3] == None):
        p[0] = Node(p[1], p[2])
        print("Reservado Array: "+ p[1] +","+ p[2].type)
    elif (p[1] == "array" and p[3] != None):
        p[0] = Node(p[1], p[2], p[3])
        print("Reservado: "+ p[1] +":"+ p[2].type +" "+ p[3].type)
    else:
        p[0] = Node(p[1])
        print("Reservado: "+ p[1])

# Produccion para detectar el rango de un array
def p_rango(p):
    '''rango : TkOBracket number TkSoForth number TkCBracket'''
    p[0] = Node("rango", p[2], p[4])
    print("rango: "+ str(p[2].type) +".."+ str(p[4].type))

# Produccion para detectar la expresion terminal de un numero
def p_number(p):
    '''number : TkNum'''
    p[0] = Node(p[1])
    print("Numero: "+  str(p[1]))

# Produccion para detectar la expresion terminal de un identificador
def p_expresion_id(p):
    '''expresion : TkId'''
    p[0] = Node(p[1])
    print("Id: "+ p[1])

# Produccion para detectar la expresion terminal vacia
def p_expresion_empty(p):
    'expresion :'
    
    p[0] = Node("vacio")
    print("Vacio: "+ p[0].type) 

# Produccion para detectar la expresion comentario
def p_coment(p):
    '''expresion : TkComent'''
    pass

# Manejador de errores 
def p_error(p):
    print("Syntax error in input!")
    pass

#Inicializador del parser
parser = yacc.yacc()

#Clase para la creacion de nodos, con el fin de generar el arbol AST
class Node:
    def __init__(self,type,left=None,rigth=None,level=None):
        self.type = type
        self.children = []
        self.children_type = []
        if left:
            self.left = left
            self.children.append(left)
            self.children_type.append(left.type)
        if rigth:
            self.rigth = rigth 
            self.children.append(rigth) 
            self.children_type.append(rigth.type) 

        self.level = level

# Funcion para recorrer usando BFS
def bfs_recursivo(cola, visitados):
    if not cola:
        return visitados
    
    if all(x.type == cola[0].type for x in cola):
        nodo = cola.pop()
    else:
        nodo = cola.popleft()
    '''print("--------------------")
    print("Padre: "+ str(nodo.type))
    print("Hijos: "+ str(nodo.children_type))
    print("Nivel: "+ str(nodo.level))
    print("--------------------")  
    '''
    print("-"* nodo.level + str(nodo.type))
    
    if nodo not in visitados:
        visitados.add(nodo)

        for child in nodo.children:
            child.level = nodo.level + 1
            if child not in visitados:
                cola.append(child)
    return bfs_recursivo(cola, visitados)

def bfs(nodo_inicio):
    visitados = set()
    cola = deque([nodo_inicio])
    nodo_inicio.level = 0
    bfs_recursivo(cola, visitados)

while True:
    f = open(sys.argv[1], "r")   
    assert f.name.endswith('.gcl') # Verifica que sea un .gcl
    content = ' '.join(f.readlines())
    f.close()
    print(content)
    result = parser.parse(content)
    print(f"Estos son los resultados:\n {result}")
    bfs(result)
    break   