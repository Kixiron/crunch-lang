import random, string

def indent(indentlev: int) -> str:
    s = ""
    for _ in range(0, indentlev):
        s += "    "
    return s

def rand_ident(length: int) -> str:
    keywords = [
        "in", "for", "fn", "break", 
        "continue", "type", "enum", 
        "trait", "while", "if", "else", 
        "then", "let", "or", "and", "as", 
        "end", "lib", "pkg", "inf", "NaN",
        "loop",
    ]
    
    letters = string.ascii_lowercase
    ident = ''.join(random.choice(letters) for i in range(length))
    
    if ident in keywords or ident.startswith("br"):
        return rand_ident(length)
   
    return ident

def rand_ty() -> str:
    types = ["unit", "bool", "int", "str", "float", ""]
    
    ty = random.choice(types)
    
    if ty == "":
        ty = rand_ident(random.randint(1, 10))
        
    return ty

def rand_params() -> str:
    params = ""
    
    for _ in range(0, random.randint(0, 10)):
        params += f" {rand_ident(random.randint(5, 10))} : {rand_ty()} , "
        
    return params

def rand_call_params() -> str:
    params = " "
    
    for _ in range(0, random.randint(0, 10)):
        params += rand_ident(random.randint(5, 10))
        params += " , "
        
    return params

def expr(paren: bool) -> str:
    exprs = []
    if paren:
        exprs = ["func", "binop", "var", "paren"]
    else:
        exprs = ["func", "binop", "var"]
    binops = ["+", "-", "/", "*", "&", "**", "^", "%", "|"]
    
    exp = random.choice(exprs)
    
    if exp == "func":
        exp = rand_ident(random.randint(2, 10))
        exp += f"({rand_call_params()}) "
    
    elif exp == "var":
        exp = rand_ident(random.randint(2, 10))
        exp += " "
        
    elif exp == "paren":
        exp = "( "
        exp += expr(True)
        exp += " )"
        exp += " "
    
    elif exp == "binop":
        exp = rand_ident(random.randint(2, 10))
        exp += f" {random.choice(binops)} "
        exp += expr(True)
        exp += " "
        
    return exp

def statement(indentlev: int, recurse: int) -> str:
    stmts = ["if", "let", "func", "return", "match"]
    # "match", "for", "while", "loop", 
    
    if recurse > 5:
        stmts = ["let", "func", "return"]

    stmt = random.choice(stmts)
    
    if stmt == "return":
        s = indent(indentlev)
        s += stmt
        s += "\n"
        return s
    
    elif stmt == "if":
        stmt = indent(indentlev)
        stmt += "if "
        
        stmt += expr(True)
        stmt += " \n"
        indentlev += 1
        
        for _ in range(0, random.randint(1, 5)):
            stmt += statement(indentlev, recurse + 1)
        
        indentlev -= 1
        stmt += indent(indentlev)
        stmt += "end\n"
        
        return stmt
    
    elif stmt == "let":
        stmt = indent(indentlev)
        stmt += "let "
        stmt += rand_ident(random.randint(2, 5))
        stmt += " = "
        stmt += expr(True)
        stmt += " "
        stmt += "\n"
        
        return stmt
    
    elif stmt == "func":
        stmt = indent(indentlev)
        stmt += rand_ident(random.randint(2, 5))
        stmt += "( "
        stmt += rand_call_params()
        stmt += " )\n"
        
        return stmt
    
    elif stmt == "match":
        stmt = indent(indentlev)
        stmt += "match "
        stmt += rand_ident(random.randint(2, 6))
        stmt += "\n"
        
        indentlev += 1
        for _ in range(0, random.randint(1, 5)):
            stmt += indent(indentlev)
            stmt += rand_ident(random.randint(2, 6))
            stmt += " =>\n"
            
            indentlev += 1
            for _ in range(0, random.randint(1, 2)):
                stmt += statement(indentlev, recurse + 1)
                
            indentlev -= 1
            stmt += indent(indentlev)
            stmt += "end\n"

        indentlev -= 1
        stmt += indent(indentlev)
        stmt += "end\n"
        
        return stmt
                

def function(indentlev: int) -> str:
    func = f"fn {rand_ident(random.randint(5, 6))}({rand_params()}) -> {rand_ty()}\n"
    indentlev += 1
    
    for _ in range(0, random.randint(10, 100)):
        func += statement(indentlev, 0)
    
    indentlev -= 1
    func += "end\n\n"
    return func

def generate() -> str:
    output = ":: Note: This will probably overflow in debug mode\n\n"
    indentlev = 0
    
    for _ in range(0, 200):
        output += function(indentlev)
        
    return output
        

def main():
    with open("test.crunch", "w+") as f:
        f.truncate(0)
        f.write(generate())

if __name__ == "__main__":
    main()