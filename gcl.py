# ------------------------------------------------------------
# parser.py
# Generador de arbol AST para el lenguaje GCL
# Carnet: 15-10345 y 19-10211
# ------------------------------------------------------------
import ply.lex as lex
import ply.yacc as yacc
import sys
from lexer import tokens, reserved, Error_Counter
from collections import deque
if ".." not in sys.path: sys.path.insert(0,"..") # type: ignore

# Precedencia de algunas expresiones de GCL
precedence = (
    ('left', 'TkSemicolon'),
    ('left', 'TkAsig'),
    ('left', 'TkTwoPoints'),
    ('left', 'TkMinus'),
    ('left', 'TkPlus'),
    ('left', 'TkMult'),
    ('left', 'UMINUS'),
    ('left', 'TkComma'),
    ('left', 'TkFor'),
    ('left', 'TkDo'),
    ('left', 'TkIf'),
    ('left', 'TkGuard'),
    ('left', 'TkArrow'),
    ('left', 'TkIn'),
    ('left', 'TkTo'),
    ('left', 'TkOr'),
    ('left', 'TkAnd'),
    ('left', 'TkNot'),
    ('left', 'TkEqual', 'TkNEqual'),
    ('left', 'TkLess', 'TkLeq', 'TkGeq', 'TkGreater'),
    ('left', 'TkOBracket', 'TkCBracket'),
    ('left', 'TkOpenPar', 'TkClosePar'),
    ('left', 'TkSoForth'),
    ('left', 'TkPrint'),
    ('left', 'TkConcat'),
    ('left', 'TkString'),
    ('left', 'TkId', 'TkNum'),
)


# Produccion para detectar un programa en GCL
def p_program(p):
    '''program : TkOBlock declare TkCBlock 
               | TkOBlock expresion TkCBlock'''

    table = SymbolTable()
    tables.append(table)
    p[0] = Block("Block", p[2], 0)


# Produccion para detectar la expresion terminal declare
def p_expresion_declare(p):
    '''declare : TkDeclare expresion'''

    p[0] = Declare("Symbol Table", p[2])


# Produccion para detectar la expresion las secuencias del programa
def p_expresion(p):
    '''expresion : semicolon
                 | asig
                 | negative
                 | twoPoints
                 | aritmetic
                 | comma
                 | space
                 | for
                 | do
                 | if
                 | print
                 | word
                 | proposition
                 | readArray
                 | writeArray
                 | soForth
                 | program
                 | empty'''
    
    p[0] = p[1]

def p_expresion_space_empty(p):
    '''space : twoPoints expresion
             | twoPoints empty
             | readArray expresion
             | twoPoints print
             | readArray print'''
    if p[1].type == "ReadArray":
        p[0] = Space_Declare("SDeclare", p[1], p[2])

    else:
        p[0] = Space_Declare("SDeclare", p[1], p[2])

def p_expresion_semicolon(p):
    '''semicolon : expresion TkSemicolon expresion''' 
    
    p[0] = Secuencia("Secuencia", p[1],p[3])

def p_expresion_asig(p):
    '''asig : expresion TkAsig expresion
            | expresion TkAsig reserved'''
    
    p[0] = Asignation("Asignacion: ", p[1], p[3])


# Produccion para detectar un valor negativo
def p_expresion_uminus(p):
    '''negative : TkMinus number %prec UMINUS
                | TkMinus readArray %prec UMINUS
                | TkMinus word %prec UMINUS
                | TkMinus TkOpenPar aritmetic TkClosePar %prec UMINUS'''
    p[0] = Aritmetic("UMINUS",p[2])

# Produccion para detectar aritmetica
def p_expresion_aritmetic(p):
    '''aritmetic : aritmetic TkPlus aritmetic
                | aritmetic TkMinus aritmetic
                | aritmetic TkMult aritmetic
                | TkOpenPar aritmetic TkClosePar
                | negative
                | number
                | readArray
                | word '''
               #| word TkMinus aritmetic
    if(len(p) > 2 and p[1] != '('):
        if(p[2] == '+'):
            p[0] = Aritmetic("Plus", p[1], p[3])
 
        elif(p[2] == '-'):
            p[0] = Aritmetic("Minus", p[1], p[3])

        elif(p[2] == '*'):
            p[0] = Aritmetic("Mult", p[1], p[3])
    elif p[1] == '(':
            p[0] = p[2]
    else: 
        p[0] = p[1]

# Produccion para detectar la expresion no terminal TwoPoints
def p_expresion_two_point(p):
    '''twoPoints : expresion TkTwoPoints expresion
                 | expresion TkTwoPoints reserved
                 | number TkTwoPoints expresion'''
    if (p[3].type == "int" or p[3].type == "ReadArray" or p[3].type == "bool"):
        p[0] = TwoPoints("Tpdeclare", p[1], p[3], 'declare')
    else:
        p[0] = TwoPoints("TwoPoints", p[1], p[3])

    

# Produccion para detectar la expresion no terminal Comma
def p_expresion_comma(p):
    "comma : expresion TkComma expresion"
    p[0] = Comma("Comma", p[1], p[3])

def p_reserved(p):
    '''reserved : TkInt
                | TkBool
                | TkTrue
                | TkFalse
                | TkArray
                | TkSkip'''
    if (p[1] == 'array'):
        p[0] = Reserved("array", value = p[1])
    elif (p[1] == 'int'):
        p[0] = Reserved("int", value = p[1])
    elif (p[1] == 'true'):
        p[0] = Reserved("Literal:", value = p[1])
    elif (p[1] == 'false'):
        p[0] = Reserved("Literal:", value = p[1])
    elif (p[1] == 'bool'):
        p[0] = Reserved("bool", value = p[1])
    else: 
        p[0] = Reserved("skip", value = p[1])

# Produccion del ! 
def p_not(p):
    '''not : TkNot not
           | TkNot word
           | TkNot proposition
           | TkNot TkOpenPar proposition TkClosePar'''
    
    if (p[1] == '!'):
        p[0] = Not("Not", p[2])
    else:
        p[0] = Not("Not", p[3])

# Produccion para detectar condiciones bool
def p_proposition(p):
    '''proposition : proposition TkAnd      proposition
                   | proposition TkOr       proposition
                   | proposition TkLess     proposition
                   | proposition TkLeq      proposition
                   | proposition TkGeq      proposition
                   | proposition TkGreater  proposition
                   | proposition TkEqual    proposition
                   | proposition TkNEqual   proposition
                   | TkOpenPar proposition TkClosePar
                   | not
                   | aritmetic
                   | number
                   | readArray
                   | reserved
                   | word'''
    
    if(len(p) > 2):
        if (p[2] == '/\\'):
            p[0] = Condition("And", p[1], p[3])
        elif (p[2] == '\\/'):
            p[0] = Condition("Or", p[1], p[3])
        elif (p[2] == '<'):
            p[0] = Condition("Less", p[1], p[3])
        elif (p[2] == '<='):
            p[0] = Condition("Leq", p[1], p[3])
        elif (p[2] == '>='):
            p[0] = Condition("Geq", p[1], p[3])
        elif (p[2] == '>'):
            p[0] = Condition("Greater", p[1], p[3])
        elif (p[2] == '=='):
            p[0] = Condition("Equal", p[1], p[3])
        elif (p[2] == '!='):
            p[0] = Condition("NEqual", p[1], p[3])
        elif(p[1] == '('):
            p[0] = p[2]
    else:
        p[0] = p[1]

# Produccion para leer un array
def p_read_array(p):
    '''readArray : reserved TkOBracket soForth TkCBracket
                 | reserved TkOBracket number TkCBracket
                 | reserved TkOBracket negative TkCBracket
                 | word TkOBracket word TkCBracket
                 | word TkOBracket number TkCBracket
                 | word TkOBracket negative TkCBracket
                 | word TkOBracket aritmetic TkCBracket
                 | writeArray TkOBracket number TkCBracket
                 | writeArray TkOBracket word TkCBracket
                 | writeArray TkOBracket aritmetic TkCBracket
                 | writeArray TkOBracket negative TkCBracket
                 | readArray TkOBracket number TkCBracket
                 | readArray TkOBracket word TkCBracket
                 | readArray TkOBracket aritmetic TkCBracket
                 | readArray TkOBracket negative TkCBracket'''
    if (p[3].type == 'TkSoForth'):
        p[0] = ReadArray("ReadArray", p[1], p[3])
    else:
        p[0] = ReadArray("ReadArray", p[1], p[3])

# Produccion para escribir un array
def p_write_array(p):
    '''writeArray : word TkOpenPar expresion TkClosePar
                  | writeArray TkOpenPar expresion TkClosePar
                  | TkOpenPar expresion TkClosePar'''
    
    if (len(p) > 4):
        p[0] = WriteArray("WriteArray", p[1], p[3])
    else:
        p[0] = WriteArray("WriteArray", p[2])

# Produccion para detectar la expresion no terminal SoForth
def p_expresion_so_forth(p):
    '''soForth : number TkSoForth number
               | word TkSoForth word
               | number TkSoForth word
               | word TkSoForth number
               | number TkSoForth negative
               | word TkSoForth negative
               | negative TkSoForth number
               | negative TkSoForth word
               | negative TkSoForth negative'''

    p[0] = TwoSoFort("TkSoForth", p[1], p[3])

# Produccion para detectar el bucle for   
def p_for(p):
    "for : TkFor in TkArrow expresion TkRof"
    p[0] = Loop_For("For: ", p[2], p[4])

# Produccion para detectar condicion In
def p_expresion_in(p):
    '''in : number TkIn to
          | negative TkIn to
          | word TkIn to
          | readArray TkIn to '''

    p[0] = Loop_For("In", p[1], p[3])

# Produccion para detectar condicion To
def p_expresion_to(p):
    '''to :  expresion TkTo expresion'''
    
    p[0] = Loop_For("To", p[1], p[3])
    
# Produccion para detectar el bucle do
def p_do(p):
    "do : TkDo guard TkOd"
    p[0] = Loop_Do("Do: ", p[2])

# Produccion para detectar el if
def p_if(p):
    "if : TkIf guard TkFi"
    print(p)
    p[0] = Condition_If("If", p[2])


# Produccion que detecta las guardas
def p_guard(p):
    ''' guard : arrow TkGuard guard
              | arrow'''
    if(len(p) > 2):
        p[0] = Guard("Guard", p[1], p[3])
    else:
        p[0] = p[1]

# Produccion para detectar -->
def p_arrow(p):
    '''arrow : proposition TkArrow expresion
             | reserved TkArrow expresion'''

    p[0] = Arrow("Then", p[1], p[3])

# Produccion para detectar un print
def p_expresion_print(p):
    '''print : TkPrint sentence'''

    p[0] = Print("Print", p[2])

# Produccion para detectar la expresion terminal de un identificador
def p_expresion_sentence(p):
    '''sentence : sentence TkConcat sentence
                | readArray
                | writeArray
                | word
                | number
                | string'''
    if len(p) > 2:
        p[0] = Concat("Concat: ", p[1], p[3])
    else:
        p[0] = Transicion("", p[1])

# Produccion para detectar la expresion terminal una palabra
def p_string(p):
    "string : TkString"
    p[0] = Atom("String: ",p[1])


# Produccion para detectar la expresion terminal de un numero
def p_number(p):
    "number : TkNum"
    p[0] = Atom("Number: ",p[1])


# Produccion para detectar la expresion terminal de un identificador
def p_expresion_id(p):
    "word : TkId"
    p[0] = Atom("Ident: ", p[1])


# Produccion para detectar la expresion terminal vacia o letra
def p_expresion_empty(p):
    "empty :" 
    p[0] = Atom("Empty" )


# Produccion para detectar la expresion comentario
def p_coment(p):
    '''expresion : TkComent'''
    pass

def find_column(input,token):
  ultimoSalto = input.rfind('\n',0,token.lexpos)
  if ultimoSalto < 0:
    ultimoSalto = 0
  column = (token.lexpos - ultimoSalto) + 1
  return column
# Manejador de errores 
def p_error(p):
    print(f"Error de sintaxis en la linea {p.lineno -1} columna:  + {str(find_column(p.lexer.lexdata,p))} : token inesperado: {p.value}")
    yacc.restart()


#Inicializador del parser
parser = yacc.yacc()


# Tabla de simbolos: Aqui se almacenaran todas las tablas creadas
tables = []

#Clase para la creacion de numeros, palabras y variables.
# Esto ccon el fin de generar el arbol AST
class Atom:
    #Constructor de la clase
    def __init__(self, type,value=None, context = None):
        self.type = type
        self.value = str(value)
        self.context = context
       
    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block=0):
        if (self.type == "Empty"):
            AST =""
        elif(self.value == None):
            AST = "-"*level + self.type
        else:
            AST = "-"*level + self.type +self.value + " | "+ "type: "+ self.context
        print(AST)
      
    #Metodo para imprimir el arbol AST en una pila y poder 
    # obtener los valores a imprimir o recorre
    def print_AST_DQ(self, level=0):
        pila = deque()
        pila.append(self.value)
        return pila
    
    #Metodo para agregar contexto a las variables
    def add_context(self, block=0):
        #Verifica si es la produccion vacia o una palabra
        if self.type == "Empty" or self.type == "String: ":
            self.context = ""
            return None
        elif es_entero(self.value):
            # Si es un entero, el contexto sera int
            self.context = "int"
            return "int"
        else:
            # Si es una variable, se busca en la tablas de simbolos
            while block > -1 :
                
                # Se busca en la tabla de simbolos
                value = tables[block].get(self.value)
                # Si no se encuentra en la tabla de simbolos, se busca en el aparta loop
                # esto con el fin de comprobar el caso donde la variable es parte de un for
                if value is None:
                    value = tables[block].get_loop(self.value)

                # Si no se encuentra en la tabla de simbolos ni loop, buscamos en la tabla anterior
                if value is None:
                    # El valor obtenido es None, puedes realizar acciones específicas aquí
                    block -= 1
                else:
                    # El valor obtenido no es None
                    self.context = value
                    return value
            # Caso donde la variable no pertenece a ninguna tabla
            print("Variable no pertenece a ninguna tabla")
        # Si no se encuentra en ninguna tabla, se imprime un error y cierra
        sys.exit(1)

    def print_PAPP(self, level=0, block=0):
        if (self.type == "Empty"):
            AST =""
        elif(self.value == None):
            AST = ""
        elif(self.type == "String: "):
            AST = ""
        elif es_entero(self.value):
            AST = create_numbers(int(self.value))
        else:
            AST = tables[block].esp[self.value]
        return AST
    
    def print_PAPP_DQ(self, level=0):
        pila = deque()
        pila.append(self.value)
        return pila 
    
class Reserved:
    #Constructor de la clase
    def __init__(self, type, left=None, right=None, value=None, context=None):
        self.type = type
        self.value = value
        self.left = left
        self.right = right
        # Comprueba que el valor sea true o false, para indicar que es bool
        if self.value == "true" or self.value == "false":
            self.context = "bool"
        else:
            self.context = type

    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block=0):
        if(self.value == None):
            AST = "-"*level + self.type
        else:
            AST = "-"*level + self.type +self.value +" | "+ "type: "+ self.context
        print(AST)

    #Metodo para imprimir el arbol AST en una pila y poder
    # obtener los valores a imprimir o recorrer
    def print_AST_DQ(self):
        pila = deque()
        pila.append(self.value)
        return pila
    
    #Metodo para agregar contexto a las variables
    def add_context(self, block=0):
        if(self.value == None):
            return None
        else:
            return self.context
        
    def print_PAPP(self, level=0, block=0):
        # CLASIFICAR LOS RESERVED
        if(self.value == None):
            AST = ""
        elif self.value == "skip":
            AST = ""
        elif self.value == "true":
            AST = "c_{8}"
        elif self.value == "false":
            AST = "c_{9}"
        return AST 
    
    def print_PAPP_DQ(self, level=0):
        pila = deque()
        pila.append(self.value)
        return pila        
#Clase para la creacion de nodos para punto y comas(;)
class Secuencia:

    #Constructor de la clase
    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    #Metodo para imprimir el arbol AST  
    def print_AST(self, level=0, block =0):
        pila = deque()
        if not self.left.type == "Tpdeclare":
            ret = "-"*level + self.type 
            print(ret)
        pila.append(self.left)

        if(self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)

        while(len(pila)>0):
            x = pila.popleft()
            if x is None:
                continue
            elif x.type == "Tpdeclare":
                # Caso para evitar la impresion de un declare
                continue
            x.print_AST(level+1, block)

    #Metodo para imprimir el arbol AST en una pila     
    def print_AST_DQ(self,level=0, block=0):
        pila = deque()
        ret = "-"*level + self.type 
        if block == 0:
            print(ret)
        pila.append(self.left)
    
        if (self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        return pila
    
    #Metodo para agregar contexto a las variables
    def add_context(self, block=0):
        #Crea la pila
        pila = deque()
       
        pila.append(self.left)
        # Agrega el hijo izquierdo
        # Verifica si el hijo derecho es una secuencia
        if(self.right.type == "Secuencia"):
            # En caso de serlo se agrega a la pila los hijos de ese
            pila += self.right.print_AST_DQ()
        else:
            # En caso de no serlo se agrega a la pila el hijo derecho
            pila.append(self.right)
        # Recorre la pila
        while(len(pila)>0):
            x = pila.popleft()
            if x is None:
                continue
            if (x.type == 'Block'):
                # Si el hijo es un bloque, se suma uno para indicar que se 
                # esta usando otra tabla
                x.add_context(block+1)
            else:
                # Si no es un bloque, se agrega el contexto de los hijos de la pila
                x.add_context(block)

    def print_PAPP(self, level=0, block =0):
        ret = ""
        pila = deque()
        #print("AQUI EN SECUENCIA")
        #print(self.left)
        #print(self.right)
        if(self.left.type == "Secuencia"):
            #print("check secuencia")
            pila += self.left.print_AST_DQ(level+1,1)
        else:
            pila.append(self.left)
        #print(pila)
        if(self.right.type == "Secuencia"):
            #print("check secuencia")
            pila += self.right.print_AST_DQ(level+1,1)
        else:
            pila.append(self.right)
        #print("Estoy en secuencia")
        #print(pila)
        if (pila[-1].type == "Secuencia"):
            pila.pop()
        i = 3
        while(i>0):
            check = False
            for x in pila:
                #print(x)
                #print(x.type)
                #print(x.right.type)
                if x is None:
                    continue
                elif x.type == "TwoPoints":
                    # Caso para evitar la impresion de un declare
                    #print("1")
                    if x.right.type == "SDeclare":
                        #print("w")
                        pila.remove(x)
                        p = x.print_AST_DQ(level+1)
                        pila.appendleft(p[-1])
                        check = True
                        #print(pila)
                        break
                    else:
                        pila.remove(x)
                        check = True
                        break
                elif x.type == "Tpdeclare":   
                    # Caso para evitar la impresion de un declare
                    #print("1")
                    pila.remove(x)
                    check = True
                    break
                elif x.type == "SDeclare":
                    pila.remove(x)
                    pila = x.print_AST_DQ(level+1) + pila
                    check = True
                    break
                elif x.type == "Secuencia":
                    #print("loop s")
                    pila.remove(x)
                    pila = x.print_AST_DQ(level+1,1) + pila
                    check = True
                    break
                    #p = x.print_AST_DQ()
                    #x.print_AST()
                    #print(p)
            if not check:
                i -= 1
                #x.print_AST()
                #ret += x.print_PAPP(level+1, block)
        #print(pila)
        if (pila[-1].type == "Empty"):
            pila.pop()
        #print(pila)
        #range = len(pila) -1
        ret += concat_secuencia(pila, block)
        #print(ret)
        return ret
# Clase para la creacion de nodos para la declaracion de producciones con dos puntos   
class TwoPoints:
    #Constructor de la clase
    def __init__(self, type, left=None, right=None, context=None, block=0):
        self.type = type
        self.left = left
        self.right = right
    
    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block =0):
        # Verificamos si es una clase Space_Declare o una clase Reserved
        if (self.right.type == "Space" or self.right.type == "SDeclare"):
            if self.right.right.type == "SDeclare":
                self.right.right.print_AST(0, block)
            else:
                self.right.right.print_AST(level, block)
        elif (self.right.type == "ReadArray" or self.right.value == "int" or self.right.value == "bool" 
            or self.right.value == "array"): 

            pila = deque()
            AST = "-"*level
            pila += self.left.print_AST_DQ()
            pila.append(":")
            pila += self.right.print_AST_DQ()
            AST += " ".join(pila)
            print(AST)
        else:

            AST = "-"*level+self.type
            print(AST)
            self.left.print_AST(level+1, block)
            self.right.print_AST(level+1, block)

    #Metodo para imprimir el arbol AST en una pila
    def print_AST_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append(":")
        pila += self.right.print_AST_DQ()
        return pila
    #Metodo para agregar contexto a las variables
    def add_context(self, block=0):
        if (self.right.type == "Space" or self.right.type == "SDeclare"):
            # Verifica si el hijo derecho es una clase Space_Declare
            pila_left = self.left.print_AST_DQ()
            pila = self.right.print_AST_DQ()
            pila_right = pila.popleft().print_AST_DQ()
            long = None
            # Verifica si el hijo derecho es una clase reservada
            if (self.right.type == "ReadArray" or self.right.type == "array" 
                or self.right.type == "Space" or self.right.type == "SDeclare"):
                # Aqui buscamos la longitud de los array
                # Buscamos la pila del array, separamos los numeros y comparamos
                cleaned = []
                negative = ""
                # Recorremos la pila del array para hallar los numeros
                for element in pila_right:
                    if element == "-": 
                        negative = "-"
                        continue
                    if element.isdigit() and negative == "-":
                        element = negative+element
                        cleaned.append(element)
                        negative = ""
                    elif element.isdigit():
                        cleaned.append(element)
                # Comparamos los numeros
                if not (int(cleaned[0])) <= int(cleaned[1]):
                    # Si no es valido, se imprime un error y se cierra
                    print("Rango del array declarado es invalido")
                    sys.exit(1)
                else:
                    # Si es valido, se calcula la longitud
                    long = int(cleaned[1]) -(int(cleaned[0]))+1
            # Se crea el contexto
            context = "".join(pila_right)

            # Se recorre la pila de la izquierda, con el fin de
            # ver asignar los nombres de las variables
            for element in pila_left:
                # Verifica si la variable ya fue declarada
                if not tables[block].lookup(element):
                    # Si no ha sido declarada, se agrega a la tabla de simbolos
                    tables[block].add(element, context)
                    if long != None:
                        # Si la longitud no es None, se agrega el tamaño a la seccion de loop
                        tables[block].add_long(element, long)
                elif tables[block].lookup(element):
                    # Si la variable ya fue declarada, se imprime un error y se cierra
                    print("La variable ya fue declarada anteriormente")
                    sys.exit(1)
            # Se recorre la pila  con el fin de seguir agregando mas contexto
            # Debido a que estamos en Space se debe hacer esto, sino ocurre 
            # un recorrido incompleto
            while len(pila) > 0:
                element = pila.popleft()
                # Verifica si el elemento es un bloque
                if element.type == 'Block':
                    # Si es un bloque, se suma uno al bloque para indicar que se esta
                    # usando otra tabla
                    element.add_context(block+1)
                elif (element.type != 'Empty'):
                    element.add_context()

        elif (self.right.type == "ReadArray" or self.right.value == "int" or self.right.value == "bool" 
            or self.right.value == "array"): 
            # Verifica si el hijo derecho es una clase reservada
            pila_left = self.left.print_AST_DQ()
            pila_right = self.right.print_AST_DQ()
            long = None
            if (self.right.type == "ReadArray" or self.right.type == "array"):
                # Aqui buscamos la longitud de los array
                # Buscamos la pila del array, separamos los numeros y comparamos
                cleaned = []
                negative = ""
                for element in pila_right:
                    if element == "-": 
                        negative = "-"
                        continue
                    if element.isdigit() and negative == "-":
                        element = negative+element
                        cleaned.append(element)
                        negative = ""
                    elif element.isdigit():
                        cleaned.append(element)
                if not (int(cleaned[0])) <= int(cleaned[1]):
                    print("Rango del array declarado es invalido")
                    sys.exit(1)
                else:
                    long = int(cleaned[1]) -(int(cleaned[0]))+1
            # Se crea el contexto
            context = "".join(pila_right)
            for element in pila_left:
                if not tables[block].lookup(element):
                    tables[block].add(element, context)
                    if long != None:
                        tables[block].add_long(element, long)
                elif tables[block].lookup(element):
                    # Si la variable ya fue declarada, se imprime un error y se cierra
                    print("La variable ya fue declarada anteriormente")
                    sys.exit(1)
        else:
            # Si no es ninguna de las anteriores, se agrega contexto a los hijos
            # Caso donde es la lectura de un arreglo
            context_left = self.left.add_context(block)
            context_right = self.right.add_context(block) 

            if context_left == context_right:
                return context_left
            else:
                # Ambos hijos no coinciden con la clase
                print("Hubo un error con el contexto entre elementos")
                sys.exit(1)

    def print_PAPP(self, level=0, block =0):
        # Verificamos si es una clase Space_Declare o una clase Reserved
        AST = ""
        #print(self.left)
        #print(self.right)
        AST+= "("+self.right.print_PAPP(level+1, block)+")"
        AST+= "("+self.left.print_PAPP(level+1, block)+")"
        #print(AST)
        return AST
#Clase para la creacion de nodos para la declaracion de producciones con asignacion
class Asignation:

    #Constructor de la clase
    def __init__(self, type, left=None, right=None, value=0):
        self.type = type
        self.left = left
        self.right = right
        self.value = value

    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block = 0):
        AST = "-"*level + self.type
        print(AST)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    #Metodo para imprimir el arbol AST en una pila
    def print_AST_DQ(self, level=0):
        #status(self)
        pila = deque()
        pila.append(self.left)
        if (self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        return pila
    
    #Metodo para agregar contexto a las variables
    def add_context(self, block=0):
        #Agrega contexto al hijo izquierdo
        context_left = self.left.add_context(block)
        if self.left.type == "ReadArray":
            print("Error el valor de la asignacion izquierda es una lectura de arreglo")
            sys.exit(1)

        if self.right.type == "Comma" and ("array" in context_left 
            or "ReadArray" in context_left):
            # Verifica si el hijo derecho es una coma
            # Si es una coma, se busca el nivel para saber el tamaño que 
            # se va declarar el array.Ejem: A:=1,2,3, tamaño 3
            level = self.right.level()
            # Se obtiene el valor de la variable
            item = self.left.value
            # Se obtiene el tamaño de la variable de la tabla de simbolos
            long = tables[block].get_long(item) - level
            if long < 0:
                # Si el tamaño es menor a 0, se imprime un error y se cierra
                print("Se excedio el tamaño de la asignacion de valores del array")
                sys.exit(1)
            # Se agrega el contexto del hijo derecho
            context_right = self.right.add_context(block,level)
        else:
            context_right = self.right.add_context(block)

        #Casos array:= Array[x] y Array[x] := int
        if "array" in context_left and self.right.type == "ReadArray":
            print("Error por la izquierda en la asignacion")
            sys.exit(1)
        elif "array" in context_right and self.right.type == "Ident: ":
            print("Error por la derecha en la asignacion")
            sys.exit(1)

        # Verifica si el contexto de los hijos es igual
        #print(context_left)
        #print(context_right)
        if not (("array" in context_left) or ("array" in context_right) or 
                context_left == context_right):
            print("Error con el tipo de asignacion")
            sys.exit(1)

    def print_PAPP(self, level=0, block = 0):
        AST = ""
        #print("ASIG PAPP")
        AST+= get_asignation(self,block)
        #AST+= self.right.print_AST(level+1, block)
        return AST
#Clase para la creacion de nodos para la declaracion de producciones con Space
class Space_Declare:
    #Constructor de la clase
    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right
    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block = 0):
        
        if not self.type == "SDeclare":
            self.left.print_AST(level)
        if (self.right.type == "Secuencia"):
            self.right.print_AST(level+1, block)
        else:
            self.right.print_AST(level, block)

    #Metodo para imprimir el arbol AST en una pila
    def print_AST_DQ(self,level=0):
        pila = deque()
        pila.append(self.left)
        if (self.right.type == "Secuencia"):
            pila += self.right.print_AST_DQ(level+1)
        pila.append(self.right)
        return pila
    
    #Metodo para agregar contexto a las variables
    def add_context(self, block=0):
        self.left.add_context(block)
        if (self.right.type == "Block"):
            self.right.add_context(block+1)
        else:
            self.right.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        ret = ""
        if (self.right.type == "Secuencia"):
            ret += self.right.print_PAPP(level+1, block)
        elif(self.right.type == "Block"):
            ret += self.right.print_PAPP(level, block+1)
        else:
            ret+= self.right.print_PAPP(level, block)
        return ret
#Clase para la creacion de nodos para la declaracion de producciones con coma
class Comma:
    #Constructor de la clase
    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = context

    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block = 0):
        AST = "-"*level + self.type+ " | "+ "type: "+ self.context
        print(AST)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    #Metodo para imprimir el arbol AST en una pila
    def print_AST_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila += self.right.print_AST_DQ()
        return pila  
    
    #Metodo para agregar contexto a las variables
    def add_context(self, block=0, long=0):
        # Comprueba si el hijo izquierdo es una coma
        if (self.left.type == "Comma"):
            # Si es una coma, se agrega el contexto al hijo izquierdopero con 
            # un array menor
            context_left=self.left.add_context(block, long-1)
        else:
            context_left=self.left.add_context(block)

        # Se agrega el contexto al hijo derecho
        context_right=self.right.add_context(block) 
        
        if context_left == context_right or (("array" in context_left) 
            or ("array" in context_right)):
            # Verifica si el contexto de los hijos es igual
            self.context = "array with long "+str(long)
            return self.context
        
    #Metodo para obtener el nivel de la coma
    def level(self):
        level = 1
        if self.left.type == "Comma":
            level += self.left.level()
        else:
            level += 1
        return level

#Clase para la creacion de nodos para la declaracion de producciones con concatenacion    
class Concat:
    #Constructor de la clase
    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block = 0):
        AST = "-"*level + self.type
        print(AST)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    #Metodo para imprimir el arbol AST en una pila
    def print_AST_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ(level)
        pila.append(".")
        pila += self.right.print_AST_DQ(level)
        return pila
    
    # Metodo para agregar contexto a las variables   
    def add_context(self, block=0):
        self.left.add_context(block)
        self.right.add_context(block) 


#Clase para la creacion de nodos para la declaracion de producciones con 
# operaciones aritmeticas   
class Aritmetic:
    #Constructor de la clase
    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = "int"
    
    #Metodo para imprimir el arbol AST
    def print_AST(self, level=0, block = 0):
        if (self.type == "UMINUS"):
            AST = "-"*level + 'Minus'
            print(AST)
            self.left.print_AST(level+1, block)
        else:
            AST = "-"*level + self.type+ " | "+ "type: "+ self.context
            print(AST)
            self.left.print_AST(level+1, block)
            self.right.print_AST(level+1, block)

    #Metodo para imprimir el arbol AST en una pila
    def print_AST_DQ(self, level=0):

        if (self.type == "UMINUS"):
            pila = deque()
            pila.append("-")
            pila += self.left.print_AST_DQ()
            return pila
        else:
            pila = deque()
            pila += self.left.print_AST_DQ()
            pila.append(",")
            pila += self.right.print_AST_DQ()
            return pila

    #Metodo para agregar contexto a las variables  
    def add_context(self, block=0):
        # Caso donde el numero es negativo
        if (self.type == "UMINUS"):
            context = self.left.add_context(block)
            if self.left.type == "Ident: " and ("array" in context):
                print("Error con el tipo de contexto al asignar UMINUS")
                sys.exit(1)
            else:
                return "int"
        else:
            # Caso donde es una operacion aritmetica
            context_left = self.left.add_context(block)
            context_right = self.right.add_context(block)

            # Comprueba que ambos lados sean iguales,es decir, si es un variable
            # que no sea un arreglo, esto para evitar casos donde a:=d+1, y d es un arreglo
            if self.left.type == "Ident: " and "array" in context_left:
                print("Error con el contexto de la operacion binaria a la izquierda")
                sys.exit(1)
            elif self.right.type == "Ident: " and "array" in context_right:
                print("Error con el contexto de la operacion binaria a la derecha")
                sys.exit(1)

            #Comprueba que sean iguales, se este asignado un array
            if (context_left == context_right or ("array" in context_left and self.left.type == "ReadArray") 
                or ("array" in context_right and self.right.type == "ReadArray")):
                return self.context
            else:
                print("Error con el contexto de la operacion ")
                sys.exit(1)

    def print_PAPP(self, level=0, block = 0):
        if (self.type == "UMINUS"):
            AST = "c_{64}"
            AST+="("+ self.left.print_PAPP(level, block)+")"
        else:
            AST = ""
            #print(AST)
            if (self.type == "Plus"):
                AST += "c_{55}"
            elif (self.type == "Minus"):
                AST += "c_{56}"
            else:
                AST += "c_{57}"
            AST+="("+self.right.print_PAPP(level+1, block)+")"
            AST+= "("+self.left.print_PAPP(level+1, block)+")"
        return AST
        
    def print_PAPP_DQ(self, level=0):

        if (self.type == "UMINUS"):
            pila = deque()
            pila.append("-")
            pila += self.left.print_PAPP_DQ()
            return pila
        else:
            pila = deque()
            pila += self.left.print_PAPP_DQ()
            if (self.type == "Plus"):
                pila.append("+")
            elif (self.type == "Minus"):
                pila.append("-")
            else:
                pila.append("*")
            pila += self.right.print_PAPP_DQ()
            return pila

# Clase para la gramatica print
class Print:  
    # Construtor
    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right
    # Impresion de arbol
    def print_AST(self, level=0, block = 0):
        if self is not None:

            if(self.right is not None):
                pila = deque()
                pila += self.left.print_AST_DQ(level+1)
                level+= len(pila)
                AST = "-"*level + self.type
                print(AST)
                print(pila)
                while(len(pila)>0):
                    x = pila.popleft()
                    x.print_AST(level+1, block)
            else:
                AST = "-"*level + self.type
                print(AST)
                self.left.print_AST(level+1, block)
    # Impresion de pila
    def print_AST_DQ(self, level=0):
        pila = deque()
        if self.left is not None:
            pila += self.left.print_AST_DQ(level)
        if self.right is not None:
            pila += self.right.print_AST_DQ(level)
        return pila 
    
    # Ajustador de contexto
    def add_context(self, block=0):
        if self is not None:
            if(self.right is not None):
                pila = deque()
                pila += self.left.print_AST_DQ()
                print(pila)
                while(len(pila)>0):
                    x = pila.popleft()
                    x.add_context()
            else:
                self.left.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        ret = "EXPRESION"
        '''if self is not None:
            if(self.right is not None):
                pila = deque()
                pila += self.left.print_AST_DQ(level+1)
                level+= len(pila)

                while(len(pila)>0):
                    x = pila.popleft()
                    ret+=x.print_PAPP(level+1, block)
            else:
                ret+= self.left.print_PAPP(level+1, block)'''
        return ret
    
# Clase para la lectura de array  
class ReadArray:
    # Construtor
    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = context
    # Imprimir arbol
    def print_AST(self, level=0, block = 0):
        if (self.left.type == "TwoPoints"):
            ret = "-"*level
            ret+= " ".join(self.print_AST_DQ())
            print(ret)
        else:
            ret = "-"*level + self.type
            print(ret)
            self.left.print_AST(level+1, block)
            self.right.print_AST(level+1, block)
    # Obtener pila
    def print_AST_DQ(self, level=0):

        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("[")
        pila += self.right.print_AST_DQ()
        pila.append("]")

        return pila
    # Contexto
    def add_context(self, block=0):
        context_left = self.left.add_context(block)
        contexto_right = self.right.add_context(block)
        # Caso int := array[booleano]
        if "array" in context_left or context_left == "int":
            if (contexto_right == "int"):
                self.context = "int"
                return context_left
            print("El contexto derecho de la lectura del array es incorrecto")
            sys.exit(1)
        print("El contexto izquierdo de la lectura del array es incorrecto")
        sys.exit(1) 

    def print_PAPP(self, level=0, block = 0):
            ret = ""
            ret+= "("+self.left.print_PAPP(level+1, block)+ ") "
            ret+= self.right.print_PAPP(level+1, block)
            #print(ret)
            return ret
    
    def print_PAPP_DQ(self, level=0):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("[")
        pila += self.right.print_AST_DQ()
        pila.append("]")
        return pila
    
# Clase para lectura de array
class WriteArray:
    # Construtor
    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    # Impresion del arbol
    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    # Obtencion de pila
    def print_AST_DQ(self, level=0):
        pila = deque()
        pila.append("(")
        pila += self.left.print_AST_DQ(level)
        pila.append(")")
        return pila
    
    # Introduccion de contexto
    def add_context(self, block=0):
        context_left = self.left.add_context(block)
        contexto_right = self.right.add_context(block)
        #Caso analogo al readArray
        if "array" in context_left or context_left == "int":
            if (contexto_right == "int"):
                self.context = "int"
                return context_left
            print("El contexto derecho de la lectura del array es incorrecto")
            sys.exit(1)
        print("El contexto derecho de la escritura del array es incorrecto")
        sys.exit(1)

    def print_PAPP(self, level=0, block = 0):
        #print("ESTOY EN WAPAPP")
        #print(self.left)
        #print(self.right)
        ret = "c_{58}"
        ret+= self.right.print_PAPP(level+1, block)
        #print(ret)
        ret+= "("+self.left.print_PAPP(level+1, block)+")"
        #print(ret)
        return ret
    
    # Obtencion de pila
    def print_PAPP_DQ(self, level=0):
        #print("Aqui")
        pila = deque()
        pila.append("(")
        pila += self.left.print_AST_DQ(level)
        pila.append(")")
        return pila
    
class Not:

    def __init__(self, type, children):
        self.type = type
        self.children = children

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type
        print(ret)
        self.children.print_AST(level+1, block)

    def add_context(self, block=0):
        context = self.children.add_context(block)
        return context
    
    def print_PAPP(self, level=0, block = 0):
        #print("ESTOY en NOT PA")
        ret = "c_{7}"
        #print(self.children)
        ret += "("+self.children.print_PAPP(level+1, block)+")" 
        return ret 
     
    def print_PAPP_DQ(self, level=0, block = 0):
        #print("ESTOY en NOT DQ")
        ret = "c_{7}"
        ret+="("+self.children.print_PAPP(level+1, block)+")"
        return ret      
     
class Condition_If:

    def __init__(self,type, children = None,level = 0 ):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type 
        print(ret)
        self.children.print_AST(level+1, block)
    
    def add_context(self, block=0):
        print(self.children.type)
        self.children.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        ret = ""
        ret+= self.children.print_AST(level+1, block)   
        return ret   
class Guard:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        pila.append(self.left)
        if(self.right.type == "Guard"):
            pila += self.right.print_AST_DQ(level+1, block)
        else:
            pila.append(self.right)
            
        while(len(pila)>0):
            x = pila.popleft()
            x.print_AST(level+1, block)

    def print_AST_DQ(self,level=0):
        pila = deque()
        ret = "-"*level + self.type 
        print(ret)
        pila.append(self.left)
        if (self.right.type == "Guard"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
        return pila
    
    def add_context(self, block=0):
        pila = deque()
        pila.append(self.left)
        if(self.right.type == "Guard"):
            pila += self.right.print_AST_DQ()
        else:
            pila.append(self.right)
            
        while(len(pila)>0):
            x = pila.popleft()
            if(x.type == 'Block'):
                x.add_context(block+1)
            else:
                x.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        pila = deque()
        #print("Estoy en if")
        ret = ""
        pila.append(self.left)
        if(self.right.type == "Guard"):
            pila += self.right.print_AST_DQ(level+1)
        else:
            pila.append(self.right)
            
        while(len(pila)>0):
            x = pila.popleft()
            ret += x.print_PAPP(level+1, block)
        return ret
    
class Arrow:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def add_context(self, block=0):
        context_left = self.left.add_context(block)
        if context_left != "bool":
            print(context_left)
            print("Error en el contexto del lado izquierdo de la flechaf")
            sys.exit(1)
        self.right.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        ret = ""
        ret += self.left.print_PAPP(level+1, block)
        if (self.right.type == "Block"):
            ret+= self.right.print_PAPP(level+1, block+1)
        else:
            ret+= self.right.print_PAPP(level+1, block)
        return ret
class Condition:

    def __init__(self, type, left=None, right=None, context=None):
        self.type = type
        self.left = left
        self.right = right
        self.context = context

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def add_context(self, block=0):
        context_left = self.left.add_context(block)
        context_right = self.right.add_context(block)
        #print("------")
        #print(self.left)
        #print(self.left.print_AST_DQ())
        #print(self.right)
        #print(self.right.print_AST_DQ())
        #print(context_left)
        #print(context_right)
        if context_left == context_right:
            self.context = "bool"
            return self.context
        else:
            #print("cai en error*")
            return "ERROR"
        
    def print_PAPP(self, level=0, block = 0):
        ret = ""
        if (self.type == "And"):
            ret+= "c_{5}"
        elif (self.type == "Or"):
            ret+= "c_{4}"
        elif (self.type == "Less"):
            ret+= "c_{65}"
        elif (self.type == "Leq"):
            ret+= "c_{66}"
        elif (self.type == "Geq"):
            ret+= "c_{68}"
        elif (self.type == "Greater"):
            ret+= "c_{67}"
        elif (self.type == "Equal"):
            ret+= "c_{15}"
        elif (self.type == "NEqual"):
            ret+= "c_{59}"
        ret+="("+self.right.print_PAPP(level+1, block)+")"
        ret+= "("+self.left.print_PAPP(level+1, block)+")"
        return ret
    
    def print_PAPP_DQ(self, level=0, block = 0):
        #print("Aqui en condition")
        pila = deque()
        pila+= self.left.print_PAPP_DQ()
        #print(pila)
        if (self.type == "And"):
            pila.append("/\\")
        elif (self.type == "Or"):
            pila.append("\\/")
        elif (self.type == "Less"):
            pila.append("<")
        elif (self.type == "Leq"):
            pila.append("<=")
        elif (self.type == "Geq"):
            pila.append(">=")
        elif (self.type == "Greater"):
            pila.append(">")
        elif (self.type == "Equal"):
            pila.append("==")
        elif (self.type == "NEqual"):
            pila.append("!=")
        pila+= self.right.print_PAPP_DQ()
        #print(pila)
        return pila
class Loop_For:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def add_context(self, block=0):
        if self.type == "In":
            if not self.left.type == "Ident: ":
                x = self.left.print_AST_DQ()
                x = "".join(x)
                print(x)
                if not tables[block].lookup(x):
                    tables[block].add_loop(x)    
                    context_right = self.right.add_context(block)
                    if not "int" == context_right:
                        print("Error en la asignacion de la variable a iterar en loop for(IN)")
                        sys.exit(1)
                    self.left.context = "int"
            else :
                print("Variable para asignar con iterador en el for ya fue declarada")
                sys.exit(1)
        elif self.type == "To":  
            context_left = self.left.add_context(block)
            context_right = self.right.add_context(block)
            if not context_left and context_right == "int":
                print("Error en la asignacion de la cotas para la iteracion for")
                sys.exit(1)
            else :
                return "int"
        else:
            self.left.add_context(block)
            self.right.add_context(block)
            tables[block].loop.clear()

class Loop_Do:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type
        print(ret)
        self.left.print_AST(level+1, block)

    def add_context(self, block=0):
        self.left.add_context(block)
class TwoSoFort:

    def __init__(self, type, left=None, right=None):
        self.type = type
        self.left = left
        self.right = right

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type+":"
        print(ret)
        self.left.print_AST(level+1, block)
        self.right.print_AST(level+1, block)

    def print_AST_DQ(self):
        pila = deque()
        pila += self.left.print_AST_DQ()
        pila.append("..")
        pila += self.right.print_AST_DQ()
        return pila
    
    def add_context(self, block=0):
        self.left.add_context(block)
        self.right.add_context(block)


class Declare:

    def __init__(self, type, children = None,level = 0):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0, block = 0):
        tables[block].print_AST(level)
        self.children.print_AST(level+1, block) 

    def add_context(self, block=0):
        self.children.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        ret= ""
        #print("en declare")
        ret+= self.children.print_PAPP(level+1, block)
        return ret
class Block:
    
    def __init__(self,type, children = None,level = 0):
        self.type = type
        self.level = level
        self.children = children

    def print_AST(self, level=0, block = 0):
        ret = "-"*level + self.type 
        print(ret)
        self.children.print_AST(level+1, block)

    def add_context(self, block=0):
        self.children.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        ret = ""
        #print("BLOCK")
        ret+= self.children.print_PAPP(level, block)
        return ret
class Transicion:

    def __init__(self, type, children = None,level = 0, value=0):
        self.type = type
        self.level = level
        self.children = children
        self.value = value

    def print_AST(self, level=0, block = 0):
        if(self.type == "Print"):
            AST = "-"*level + self.type
            print(AST)

            self.children.print_AST(level+1, block)
        else:
            self.children.print_AST(level, block)

    def print_AST_DQ(self):
        return self.children.print_AST_DQ()
    
    def add_context(self, block=0):
        self.children.add_context(block)

    def print_PAPP(self, level=0, block = 0):
        if(self.type == "Print"):
            AST = ""
            AST+= self.children.print_PAPP(level+1, block)
        else:
            AST+= self.children.print_PAPP(level, block)
        return AST
#Clase para manejo de errores.     
class SyntaxErrorException(Exception):
    def __init__(self, message, lineno):
        super().__init__(message)
        self.lineno = lineno

    def __str__(self):
        return f"{self.args[0]} (line {self.lineno})"
class SymbolTable:
    
    def __init__(self):
        self.symbols = {}
        self.long ={}
        self.loop = {}
        self.esp = {}
        self.ESP = ""

    def add(self, name, type):
        if name in self.symbols:
            raise Exception(f"Error: Redeclaration of variable '{name}'")
        self.symbols[name] = type

    def add_long(self, name, long):
        if name in self.long:
            raise Exception(f"Error: Redeclaration long of variable '{name}'")
        self.long[name] = long

    def add_loop(self, name):
        self.loop[name] = "int"

    def lookup(self, name):
        if name in self.symbols or name in self.loop:
            return True
        else:
            False

    def lookup_loop(self, name):
        if name in self.loop:
            return True
        else:
            False
    def get(self, name):
        return self.symbols.get(name)
    
    def get_long(self, name):
        return self.long.get(name)
    
    def get_loop(self, name):
        return self.loop.get(name) 
             
    def print_AST(self, level=0):
        for name, type in self.symbols.items():
            print("-"*(level) + f"Variable: {name} | type: {type}")
    
    def create_ESP(self):
        i = 1
        for name in self.symbols.keys():
            self.esp[name] = "x_{"+str(i)+"}"
            i+=1
        tp = list(self.symbols.values()) 
        self.ESP = concat_set(tp)
#Funcion para verificar si un string es un entero
def es_entero(s):
    try:
        int(s)  # Intenta convertir el str a int
        return True  # La conversión fue exitosa
    except ValueError:
        return False  # La conversión falló, s no es un int 
    
# Funcion para determinar numeros
def create_numbers(s):
    numbers = ""
    if s > 9:
        pila = separar_numero(s)
        #print(pila)
        numbers = concat_numbers(pila)
        #print(numbers)
    elif s < 0:
        numbers += "c_{63}"
        s = s*(-1)
        numbers += "("+create_numbers(s)+")"
    else:
        numbers = get_number(s)
        #print(numbers)
    return numbers

def concat_numbers(p):
    concat = ""
    range = len(p)-1
    #print(p)
    if (range != 0):
        concat += "c_{54}"
        n = p.pop()
        id = get_number(n)
        concat += "("+concat_numbers(p)+")"
        concat += id
        return concat
    else:
        n = p.pop(0)
        id = get_number(n)
        concat += id
        return concat
    
def concat_set(p):
    concat = ""
    range = len(p)-1
    if (range != 0):
        concat += "c_{32}"
        item = p.pop()
        type = get_set(item)
        concat += "(" + type + ")"
        concat += "("+concat_set(p)+")"
        return concat
    else:
        item = p.pop()
        type = get_set(item)
        concat += type
        return concat 
     
def concat_secuencia(p, block):
    concat = ""
    range = len(p)-1
    #print(p)
    if (range != 0):
        concat += "c_{34}"
        item = p.pop()
        type = "noting"
        if item.type == "Asignacion: ":
            type = get_asignation(item, block)
        elif item.type == "If":
            type = get_if(item, block)
        concat += "(" + type + ") "
        concat += "("+concat_secuencia(p, block)+")"
        return concat
    else:
        item = p.pop()
        type = "noting"
        if item.type == "Asignacion: ":
            type = get_asignation(item, block)
        elif item.type == "If":
            type = get_if(item, block)
        concat += type
        return concat   
# Funcion que determina el codigo del numero
def get_number(s):
    numbers = ""
    if s == 0:
        numbers = "c_{42}"
    elif s == 1:
        numbers = "c_{43}"
    elif s == 2:
        numbers = "c_{44}"
    elif s == 3:
        numbers = "c_{45}"
    elif s == 4:  
       numbers =  "c_{46}"
    elif s == 5:
        numbers = "c_{47}"
    elif s == 6:
        numbers = "c_{48}"
    elif s == 7:
        numbers = "c_{49}"
    elif s == 8:
        numbers = "c_{50}"
    else:
        numbers = "c_{51}"
    return numbers

# Funcion que determina tipo del conjunto
def get_set(type):
    esp = ""
    if "array" in type:
        type = type.replace("array","")
        type = type.replace("[","")
        type = type.replace("]","")
        type = type.replace("..",",")
        pila = type.split(",")
        #print(pila)
        n1 = "("+ create_numbers(int(pila[0])) + ")"
        n2 = "("+ create_numbers(int(pila[-1])) + ")"
        exponente = f"c_{{63}}{n2}{n1}"
        esp = f"c_{{38}}({exponente})(c_{{36}})"
    elif type == "int":
        esp = "c_{36}"
    else:
        esp = "c_{37}"
    return esp

def get_asignation(item, block):
    esp = ""
    #print(item.left.type)
    #print(item.right.type)
    #2
    x_1 = item.left.print_PAPP()
    #print(x_1)
    second = f"{item.right.print_PAPP()}" # Expresion
    #print(second)
    pila = item.left.print_PAPP_DQ()
    #print(pila)
    pila.append(":=")
    pila += item.right.print_PAPP_DQ()
    #print(pila)
    #1
    incog = list(tables[block].esp.values())
    '''
    i = 0
    for x in pila:
        if x in tables[block].esp:
            value = tables[block].esp[x]
            incog.append(value)
            pila[i] = value
        i+=1'''
    #2.1
    pila = list(pila)  
    while(True):
        i = pila[0]
        #print(i)
        if not i == ":=":
            pila.pop(0)
        else:
            pila.pop(0)
            break
    # Supongamos que queremos eliminar elementos que contengan el término "termino"
    termino = "x"
    # Usando comprensión de lista para filtrar elementos
    #expresion_filtrada = [elemento for elemento in pila if termino in elemento]
    #expresion_filtrada = list(expresion_filtrada)
    #expresion_filtrada.insert(0, second)
    
    extra = incog.copy()
    extra2 = incog.copy()
    #print(expresion_filtrada)
    #print(pila)
    #print(incog)
    #print(x_1)

    first = f"c_{{33}}({get_comma(incog)})"
    #print("termino first")
    #print(second)
    #if second is None:
    #    print("T")
    #else:
    #    print("F")
    second = f"c_{{33}}({get_comma2(extra, second, x_1)})"
    #print(extra)
    
    predicado = recursive_incog(extra2, first, second, block)
    #print("###########################")
    #print(first)
    #print("###########################")
    #print(second)
    #print("###########################")
    #print(predicado)
    #print("###########################")
    asignation = f"c_{{19}}(\\lambda x_{{120}}.{predicado})(\\lambda x_{{120}}.c_{{32}}({tables[block].ESP})({tables[block].ESP}))"
    #print(asignation)
    esp = f"c_{{24}}(c_{{20}}(c_{{31}}(c_{{40}})(c_{{40}})))({asignation})" #abort U
    #esp += "("+get_asignation_PAPP(pila, block)+")"
    return esp

def get_if(item, block):
    #print("ESTOY EN GET_IF")
    item = item.children
    #print(item)
    esp = ""
    if(item.type == "Guard"):
        pila = item.print_AST_DQ()
    else:
        pila = deque()
        pila.append(item)

    #print(pila)
    instrucciones = deque()
    condiciones = deque()
    for arrow  in pila:
        condiciones.append(arrow.left)
        instrucciones.append(arrow.right)
    #print(condiciones)
    #print(instrucciones)
    list_T = []
    list_S = []
    list_id = []
    list_first = []
    #creacion de condicones/Ti
    for x in condiciones:
        predicado = x.print_PAPP()
        conjunto = f"c_{{19}} ({predicado}) (\\lambda x_{{120}}. ({tables[block].ESP}))"
        #print(conjunto)
        list_T.append(conjunto)
    #print(list_T)

    # Creacion de instruciiones sem<inst>
    for x in instrucciones:
        sem = x.print_PAPP()
        #print(sem)
        list_S.append(sem)

    # creacion de id_Ti
    for x in list_T:
        id = f"c_{{39}} ({x})"
        list_id.append(id)
    i=0
    # creacion de sem<instruccion > o id
    for x in list_S:
        element = f"c_{{34}}({list_id[i]}) ({x})"
        list_first.append(element)
        i+=1
    # Union de todos los sem<instruccion > o id
    first = f"c_{{33}} ({get_union(list_first)})"
    # Union de Ti
    second_left = f"c_{{33}}({get_union(list_T)})"
    second_left = f"c_{{41}}({second_left}))"
    second = f"c_{{33}}(c_{{32}}(c_{{20}}c_{{40}})({second_left}))"
    esp = f"c_{{24}}({second})({first})"
    #print(list_T)
    #print(list_S)    
    #print("Aqui")
    return esp

def recursive_incog(incog, first, second, block, i=0):
    esp = ""
    range = len(incog)-(i+1)
    #print("aqui en lam")
    #print(incog)
    x = incog[i]
    #print(x)
    if not range == 0:
        i+=1
        a3 = recursive_incog(incog, first, second, block,i)
        esp+= f"c_{{62}}c_{{4}}(\\lambda {x}.c_{{8}})(\\lambda {x}.{a3})"
        #print(incog)
    else :
        par_ordenado = f"c_{{31}}({second})({first})"
        esp+= f"c_{{62}}c_{{4}}(\\lambda {x}.c_{{8}})(\\lambda {x}.c_{{15}}({par_ordenado})(x_{{120}}))"
    return esp

def get_comma(incog):
    comma = ""
    range = len(incog)-1
    #print(range)
    if (range != 0):
        comma += "c_{21}"
        item = incog.pop()
        comma += "(" + item + ")"
        comma += "("+get_comma(incog)+")"
        return comma
    else:
        item = incog.pop()
        comma += item
        return comma
    
def get_comma2(incog, second, stop):
    comma = ""
    range = len(incog)-1
    #print(range)
    #print(stop)
    if (range != 0):
        comma += "c_{21}"
        item = incog.pop()
        #print(item)
        if item == stop:
            comma += "(" + second + ")"
        else:
            comma += "("+item+")"
        #print(comma)
        comma += "("+get_comma2(incog, second, stop)+")"
        return comma
    else:
        item = incog.pop()
        if item == stop:
            comma += second
        else:
            comma += item
        return comma   
     
def get_union(pila):
    union = ""
    range = len(pila)-1
    if (range != 0):
        union += "c_{24}"
        item = pila.pop()
        union += "(" + item + ")"
        union += "("+get_union(pila)+")"
        return union
    else:
        item = pila.pop()
        union += item
        return union

def separar_numero(numero):
    # Convertir el número a string para iterar sobre cada dígito
    numero_str = str(numero)
    # Convertir cada dígito de vuelta a entero y almacenar en una lista
    digitos = [int(digito) for digito in numero_str]
    return digitos  
  
def find_column(input,token):
  ultimoSalto = input.rfind('\n',0,token.lexpos)
  if ultimoSalto < 0:
    ultimoSalto = 0
  column = (token.lexpos - ultimoSalto) + 1
  return column  
if __name__ == "__main__":
    try:
        if len(sys.argv) != 2:
            raise ValueError("Debe proporcionar el nombre del archivo a analizar.")
        nombre_archivo = sys.argv[1]
        with open(nombre_archivo, "r") as f:
            assert f.name.endswith('.gcl') # Verifica que sea un .gcl
            contenido = ' '.join(f.readlines())
        result = parser.parse(contenido)
        result.add_context()
        #result.print_AST()
        for i in range(len(tables)):
            tables[i].create_ESP()
        sem = result.print_PAPP()
        print(sem)
    except:
        if f.name.endswith('.gcl') == False: # Verifica que sea un .gcl
            print("Archivo no encontrado o no es de extensión .gcl, indique un archivo para analizar")
        else:
            pass