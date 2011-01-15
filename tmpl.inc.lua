local setmetatable = setmetatable
local assert = assert
local print = print

local dump = dump
local dofile = dofile
local error = error
local type = type
local string = string
local ipairs = ipairs
local pairs = pairs
local io = io
local table = table
local setmetatable = setmetatable
local getmetatable = getmetatable
local tostring = tostring
local tonumber = tonumber

local debug = debug
local collectgarbage = collectgarbage

module("parser")



-- primary_expression

function primary_expression_1(identifier)
  --print('in primary_expression_1 ( IDENTIFIER )')
  return expr_meta.gen_id(identifier)
end

function primary_expression_2(constant)
  --print('in primary_expression_2 ( constant )')
  return constant
end

function primary_expression_3()
  print('in primary_expression_3 ( STRING_LITERAL )')

end

function primary_expression_4(expression)
  print('in primary_expression_4 ( L_PAREN expression R_PAREN )')

end

--   ;

-- constant

function constant_1(literal)
  --print('in constant_1 ( STRING_CONSTANT )')
  return expr_meta.gen_constant("string", literal)
end

function constant_2(literal)
  --print('in constant_2 ( CHAR_CONSTANT )')
  return expr_meta.gen_constant("char", literal)
end

function constant_3(literal)
  --print('in constant_3 ( OCT_INT_CONSTANT )')
  return expr_meta.gen_constant("oct", literal)
end

function constant_4(literal)
  --print('in constant_4 ( DEC_INT_CONSTANT )')
  return expr_meta.gen_constant("dec", literal)
end

function constant_5(literal)
  --print('in constant_5 ( HEX_INT_CONSTANT )')
  return expr_meta.gen_constant("hex", literal)
end

function constant_6(literal)
  --print('in constant_6 ( FLOAT_CONSTANT )')
  return expr_meta.gen_constant("float", literal)
end

--   ;

-- postfix_expression

function postfix_expression_1(primary_expression)
  --print('in postfix_expression_1 ( primary_expression )')
  return primary_expression
end

function postfix_expression_2(postfix_expression, expression)
  print('in postfix_expression_2 ( postfix_expression L_BRACKET expression R_BRACKET )')

end

function postfix_expression_3(postfix_expression)
  print('in postfix_expression_3 ( postfix_expression L_PAREN R_PAREN )')

end

function postfix_expression_4(postfix_expression, argument_expression_list)
  print('in postfix_expression_4 ( postfix_expression L_PAREN argument_expression_list R_PAREN )')

end

function postfix_expression_5(postfix_expression, identifier)
  print('in postfix_expression_5 ( postfix_expression DOT IDENTIFIER )')

end

function postfix_expression_6(postfix_expression, identifier)
  print('in postfix_expression_6 ( postfix_expression PTR_OP IDENTIFIER )')

end

function postfix_expression_7(postfix_expression)
  print('in postfix_expression_7 ( postfix_expression INC_OP )')

end

function postfix_expression_8(postfix_expression)
  print('in postfix_expression_8 ( postfix_expression DEC_OP )')

end

function postfix_expression_9(type_name, initializer_list)
  print('in postfix_expression_9 ( L_PAREN type_name R_PAREN L_BRACE initializer_list R_BRACE )')

end

function postfix_expression_10(type_name, initializer_list)
  print('in postfix_expression_10 ( L_PAREN type_name R_PAREN L_BRACE initializer_list COMMA R_BRACE )')

end

--   ;

-- argument_expression_list

function argument_expression_list_1(assignment_expression)
  print('in argument_expression_list_1 ( assignment_expression )')

end

function argument_expression_list_2(argument_expression_list, assignment_expression)
  print('in argument_expression_list_2 ( argument_expression_list COMMA assignment_expression )')

end

--   ;

-- unary_expression

function unary_expression_1(postfix_expression)
  --print('in unary_expression_1 ( postfix_expression )')
  return postfix_expression
end

function unary_expression_2(unary_expression)
  --print('in unary_expression_2 ( INC_OP unary_expression )')
  return cast_expression:uinc()
end

function unary_expression_3(unary_expression)
  --print('in unary_expression_3 ( DEC_OP unary_expression )')
  return cast_expression:udec()
end

function unary_expression_4(cast_expression)
  --print('in unary_expression_4 ( AMPERSAND cast_expression )')
  return cast_expression:ref()
end

function unary_expression_5(cast_expression)
  --print('in unary_expression_5 ( STAR cast_expression )')
  return cast_expression:ptr()
end

function unary_expression_6(cast_expression)
  --print('in unary_expression_6 ( PLUS cast_expression )')
  return cast_expression:unp()
end

function unary_expression_7(cast_expression)
  --print('in unary_expression_7 ( MINUS cast_expression )')
  return -cast_expression
end

function unary_expression_8(cast_expression)
  --print('in unary_expression_8 ( TILDE cast_expression )')
  return cast_expression:bnot()
end

function unary_expression_9(cast_expression)
  --print('in unary_expression_9 ( EXCLAMATION cast_expression )')
  return cast_expression:lnot()
end

function unary_expression_10(unary_expression)
  --print('in unary_expression_10 ( SIZEOF unary_expression )')

end

function unary_expression_11(type_name)
  --print('in unary_expression_11 ( SIZEOF L_PAREN type_name R_PAREN )')
  return cast_expression:sizeof()
end

--   ;

-- cast_expression

function cast_expression_1(unary_expression)
  --print('in cast_expression_1 ( unary_expression )')
  return unary_expression
end

function cast_expression_2(type_name, cast_expression)
  print('in cast_expression_2 ( L_PAREN type_name R_PAREN cast_expression )')

end

--   ;

-- multiplicative_expression

function multiplicative_expression_1(cast_expression)
  --print('in multiplicative_expression_1 ( cast_expression )')
  return cast_expression
end

function multiplicative_expression_2(multiplicative_expression, cast_expression)
  --print('in multiplicative_expression_2 ( multiplicative_expression STAR cast_expression )')
  return multiplicative_expression * cast_expression
end

function multiplicative_expression_3(multiplicative_expression, cast_expression)
  --print('in multiplicative_expression_3 ( multiplicative_expression SLASH cast_expression )')
  return multiplicative_expression / cast_expression
end

function multiplicative_expression_4(multiplicative_expression, cast_expression)
  --print('in multiplicative_expression_4 ( multiplicative_expression PERCENT cast_expression )')
  return multiplicative_expression % cast_expression
end

--   ;

-- additive_expression

function additive_expression_1(multiplicative_expression)
  --print('in additive_expression_1 ( multiplicative_expression )')
  return multiplicative_expression
end

function additive_expression_2(additive_expression, multiplicative_expression)
  --print('in additive_expression_2 ( additive_expression PLUS multiplicative_expression )')
  return additive_expression + multiplicative_expression
end

function additive_expression_3(additive_expression, multiplicative_expression)
  --print('in additive_expression_3 ( additive_expression MINUS multiplicative_expression )')
  return additive_expression - multiplicative_expression
end

--   ;

-- shift_expression

function shift_expression_1(additive_expression)
  --print('in shift_expression_1 ( additive_expression )')
  return additive_expression
end

function shift_expression_2(shift_expression, additive_expression)
  print('in shift_expression_2 ( shift_expression LEFT_OP additive_expression )')

end

function shift_expression_3(shift_expression, additive_expression)
  print('in shift_expression_3 ( shift_expression RIGHT_OP additive_expression )')

end

--   ;

-- relational_expression

function relational_expression_1(shift_expression)
  --print('in relational_expression_1 ( shift_expression )')
  return shift_expression
end

function relational_expression_2(relational_expression, shift_expression)
  --print('in relational_expression_2 ( relational_expression LESS shift_expression )')
  return relational_expression:ls(shift_expression)
end

function relational_expression_3(relational_expression, shift_expression)
  --print('in relational_expression_3 ( relational_expression GREATER shift_expression )')
  return relational_expression:gt(shift_expression)
end

function relational_expression_4(relational_expression, shift_expression)
  --print('in relational_expression_4 ( relational_expression LE_OP shift_expression )')
  return relational_expression:le(shift_expression)
end

function relational_expression_5(relational_expression, shift_expression)
  --print('in relational_expression_5 ( relational_expression GE_OP shift_expression )')
  return relational_expression:ge(shift_expression)
end

--   ;

-- equality_expression

function equality_expression_1(relational_expression)
  --print('in equality_expression_1 ( relational_expression )')
  return relational_expression
end

function equality_expression_2(equality_expression, relational_expression)
  --print('in equality_expression_2 ( equality_expression EQ_OP relational_expression )')
  return equality_expression:eq(relational_expression)
end

function equality_expression_3(equality_expression, relational_expression)
  --print('in equality_expression_3 ( equality_expression NE_OP relational_expression )')
  return equality_expression:ne(relational_expression)
end

--   ;

-- and_expression

function and_expression_1(equality_expression)
  --print('in and_expression_1 ( equality_expression )')
  return equality_expression
end

function and_expression_2(and_expression, equality_expression)
  --print('in and_expression_2 ( and_expression AMPERSAND equality_expression )')
  return and_expression:band(equality_expression)
end

--   ;

-- exclusive_or_expression

function exclusive_or_expression_1(and_expression)
  --print('in exclusive_or_expression_1 ( and_expression )')
  return and_expression
end

function exclusive_or_expression_2(exclusive_or_expression, and_expression)
  --print('in exclusive_or_expression_2 ( exclusive_or_expression CIRCUMFLEX and_expression )')
  return exclusive_or_expression:bxor(and_expression)
end

--   ;

-- inclusive_or_expression

function inclusive_or_expression_1(exclusive_or_expression)
  --print('in inclusive_or_expression_1 ( exclusive_or_expression )')
  return exclusive_or_expression
end

function inclusive_or_expression_2(inclusive_or_expression, exclusive_or_expression)
  --print('in inclusive_or_expression_2 ( inclusive_or_expression PIPE exclusive_or_expression )')
  return inclusive_or_expression:bor(exclusive_or_expression)
end

--   ;

-- logical_and_expression

function logical_and_expression_1(inclusive_or_expression)
  --print('in logical_and_expression_1 ( inclusive_or_expression )')
  return inclusive_or_expression
end

function logical_and_expression_2(logical_and_expression, inclusive_or_expression)
  --print('in logical_and_expression_2 ( logical_and_expression AND_OP inclusive_or_expression )')
  return logical_and_expression:land(inclusive_or_expression)
end

--   ;

-- logical_or_expression

function logical_or_expression_1(logical_and_expression)
  --print('in logical_or_expression_1 ( logical_and_expression )')
  return logical_and_expression
end

function logical_or_expression_2(logical_or_expression, logical_and_expression)
  --print('in logical_or_expression_2 ( logical_or_expression OR_OP logical_and_expression )')
  return logical_or_expression:lor(logical_and_expression)
end

--   ;

-- conditional_expression

function conditional_expression_1(logical_or_expression)
  --print('in conditional_expression_1 ( logical_or_expression )')
  return logical_or_expression
end

function conditional_expression_2(logical_or_expression, expression, conditional_expression)
  print('in conditional_expression_2 ( logical_or_expression QUESTION expression COLON conditional_expression )')

end

--   ;

-- assignment_expression

function assignment_expression_1(conditional_expression)
  --print('in assignment_expression_1 ( conditional_expression )')
  return conditional_expression
end

function assignment_expression_2(unary_expression, assignment_expression)
  --print('in assignment_expression_2 ( unary_expression ASSIGN assignment_expression )')
  return unary_expression:asgn(assignment_expression)
end

function assignment_expression_3(unary_expression, assignment_expression)
  --print('in assignment_expression_3 ( unary_expression MUL_ASSIGN assignment_expression )')
  return unary_expression:mul_asgn(assignment_expression)
end

function assignment_expression_4(unary_expression, assignment_expression)
  --print('in assignment_expression_4 ( unary_expression DIV_ASSIGN assignment_expression )')
  return unary_expression:div_asgn(assignment_expression)
end

function assignment_expression_5(unary_expression, assignment_expression)
  --print('in assignment_expression_5 ( unary_expression MOD_ASSIGN assignment_expression )')
  return unary_expression:mod_asgn(assignment_expression)
end

function assignment_expression_6(unary_expression, assignment_expression)
  --print('in assignment_expression_6 ( unary_expression ADD_ASSIGN assignment_expression )')
  return unary_expression:add_asgn(assignment_expression)
end

function assignment_expression_7(unary_expression, assignment_expression)
  --print('in assignment_expression_7 ( unary_expression SUB_ASSIGN assignment_expression )')
  return unary_expression:sub_asgn(assignment_expression)
end

function assignment_expression_8(unary_expression, assignment_expression)
  --print('in assignment_expression_8 ( unary_expression LEFT_ASSIGN assignment_expression )')
  return unary_expression:shl_asgn(assignment_expression)
end

function assignment_expression_9(unary_expression, assignment_expression)
  --print('in assignment_expression_9 ( unary_expression RIGHT_ASSIGN assignment_expression )')
  return unary_expression:shr_asgn(assignment_expression)
end

function assignment_expression_10(unary_expression, assignment_expression)
  --print('in assignment_expression_10 ( unary_expression AND_ASSIGN assignment_expression )')
  return unary_expression:band_asgn(assignment_expression)
end

function assignment_expression_11(unary_expression, assignment_expression)
  --print('in assignment_expression_11 ( unary_expression XOR_ASSIGN assignment_expression )')
  return unary_expression:bxor_asgn(assignment_expression)
end

function assignment_expression_12(unary_expression, assignment_expression)
  --print('in assignment_expression_12 ( unary_expression OR_ASSIGN assignment_expression )')
  return unary_expression:bor_asgn(assignment_expression)
end

--   ;

-- expression

function expression_1(assignment_expression)
  --print('in expression_1 ( assignment_expression )')
  return assignment_expression:finalize()
end

function expression_2(expression, assignment_expression)
  print('in expression_2 ( expression COMMA assignment_expression )')

end

--   ;

-- constant_expression

function constant_expression_1(conditional_expression)
  --print('in constant_expression_1 ( conditional_expression )')
  return conditional_expression:finalize()
end

--   ;

-- declaration

function declaration_1(type_name, init_declarator_list)
  --print('in declaration_1 ( type_name init_declarator_list SEMICOLON )')
  return init_declarator_list:add_type(type_name):finalize()
end

function declaration_2(type_name, identifier)
  print('in declaration_2 ( TYPEDEF type_name IDENTIFIER SEMICOLON )')

end

function declaration_3(storage_class_specifier, type_name, init_declarator_list)
  print('in declaration_3 ( storage_class_specifier type_name init_declarator_list SEMICOLON )')

end

function declaration_4(struct_or_union_definition)
  --print('in declaration_4 ( struct_or_union_declarator SEMICOLON )')
  return struct_or_union_definition
end

--   ;

-- struct_or_union_declarator

function struct_or_union_definition_1(struct_or_union, identifier, struct_declaration_list)
  --print('in struct_or_union_declarator_1 ( struct_or_union IDENTIFIER L_BRACE struct_declaration_list R_BRACE )')
  local s = struct_declaration_list
  s:add_kind(struct_or_union)
  s:add_id(identifier)
  return s:finalize()
end

--   ;

-- init_declarator_list

function init_declarator_list_1(init_declarator)
  --print('in init_declarator_list_1 ( init_declarator )')
  return init_declarator
end

function init_declarator_list_2(init_declarator_list, init_declarator)
  --print('in init_declarator_list_2 ( init_declarator_list COMMA init_declarator )')
  return init_declarator_list + init_declarator
end

--   ;

-- init_declarator

function init_declarator_1(identifier)
  --print('in init_declarator_1 ( IDENTIFIER )')
  return decl_meta():add_id(identifier)
end

function init_declarator_2(identifier, initializer)
  print('in init_declarator_2 ( IDENTIFIER ASSIGN initializer )')

end

--   ;

-- storage_class_specifier

function storage_class_specifier_1()
  print('in storage_class_specifier_1 ( EXTERN )')

end

function storage_class_specifier_2()
  print('in storage_class_specifier_2 ( STATIC )')

end

function storage_class_specifier_3()
  print('in storage_class_specifier_3 ( AUTO )')

end

function storage_class_specifier_4()
  print('in storage_class_specifier_4 ( REGISTER )')

end

--   ;

-- type_specifier

function type_specifier_1()
  --print('in type_specifier_1 ( VOID )')
  return type_meta():add_type_specifier("void")
end

function type_specifier_2()
  --print('in type_specifier_2 ( CHAR )')
  return type_meta():add_type_specifier("char")
end

function type_specifier_3()
  --print('in type_specifier_3 ( SHORT )')
  return type_meta():add_type_specifier("short")
end

function type_specifier_4()
  --print('in type_specifier_4 ( INT )')
  return type_meta():add_type_specifier("int")
end

function type_specifier_5()
  --print('in type_specifier_5 ( LONG )')
  return type_meta():add_type_specifier("long")
end

function type_specifier_6()
  --print('in type_specifier_6 ( FLOAT )')
  return type_meta():add_type_specifier("float")
end

function type_specifier_7()
  --print('in type_specifier_7 ( DOUBLE )')
  return type_meta():add_type_specifier("double")
end

function type_specifier_8()
  --print('in type_specifier_8 ( SIGNED )')
  return type_meta():add_type_specifier("signed")
end

function type_specifier_9()
  --print('in type_specifier_9 ( UNSIGNED )')
  return type_meta():add_type_specifier("unsigned")
end

function type_specifier_10()
  --print('in type_specifier_10 ( BOOL )')
  return type_meta():add_type_specifier("bool")
end

function type_specifier_11()
  --print('in type_specifier_11 ( COMPLEX )')
  return type_meta():add_type_specifier("complex")
end

function type_specifier_12()
  --print('in type_specifier_12 ( IMAGINARY )')
  return type_meta():add_type_specifier("imaginary")
end

function type_specifier_13(struct_or_union_specifier)
  --print('in type_specifier_13 ( struct_or_union_specifier )')
  return struct_or_union_specifier;
end

function type_specifier_14(enum_specifier)
  print('in type_specifier_14 ( enum_specifier )')

end

function type_specifier_15(tn)
  print('in type_specifier_15 ( TYPE_NAME )')

end

function type_specifier_16()
  print('in type_specifier_16 ( INT8 )')

end

function type_specifier_17()
  print('in type_specifier_17 ( INT16 )')

end

function type_specifier_18()
  print('in type_specifier_18 ( INT32 )')

end

function type_specifier_19()
  print('in type_specifier_19 ( INT64 )')

end

function type_specifier_20()
  print('in type_specifier_20 ( UINT8 )')

end

function type_specifier_21()
  print('in type_specifier_21 ( UINT16 )')

end

function type_specifier_22()
  print('in type_specifier_22 ( UINT32 )')

end

function type_specifier_23()
  print('in type_specifier_23 ( UINT64 )')

end

--   ;

-- struct_or_union_specifier

function struct_or_union_specifier_1(struct_or_union, identifier)
  print('in struct_or_union_specifier_1 ( struct_or_union IDENTIFIER )')
  
end

function struct_or_union_specifier_2(struct_or_union, struct_declaration_list)
  --print('in struct_or_union_specifier_2 ( struct_or_union L_BRACE struct_declaration_list R_BRACE )')
  return struct_declaration_list:add_kind(struct_or_union)
end

--   ;

-- struct_or_union

function struct_or_union_1()
  --print('in struct_or_union_1 ( STRUCT )')
  return "struct"
end

function struct_or_union_2()
  --print('in struct_or_union_2 ( UNION )')
  return "union"
end

--   ;

-- struct_declaration_list

function struct_declaration_list_1(struct_declaration)
  --print('in struct_declaration_list_1 ( struct_declaration )')
  return struct_meta():add_field(struct_declaration)
end

function struct_declaration_list_2(struct_declaration_list, struct_declaration)
  --print('in struct_declaration_list_2 ( struct_declaration_list struct_declaration )')
  return struct_declaration_list + struct_meta():add_field(struct_declaration)
end

--   ;

-- struct_declaration

function struct_declaration_1(type_name, struct_declarator_list)
  --print('in struct_declaration_1 ( type_name struct_declarator_list SEMICOLON )')
  return struct_declarator_list:add_type(type_name):finalize()
end

--   ;

-- specifier_qualifier_list

function specifier_qualifier_list_1(type_specifier, specifier_qualifier_list)
  print('in specifier_qualifier_list_1 ( type_specifier specifier_qualifier_list )')

end

function specifier_qualifier_list_2(type_specifier)
  --print('in specifier_qualifier_list_2 ( type_specifier )')
  return type_specifier
end

function specifier_qualifier_list_3(type_qualifier, specifier_qualifier_list)
  print('in specifier_qualifier_list_3 ( type_qualifier specifier_qualifier_list )')

end

function specifier_qualifier_list_4(type_qualifier)
  print('in specifier_qualifier_list_4 ( type_qualifier )')

end

--   ;

-- struct_declarator_list

function struct_declarator_list_1(struct_declarator)
  --print('in struct_declarator_list_1 ( struct_declarator )')
  return struct_declarator
end

function struct_declarator_list_2(struct_declarator_list, struct_declarator)
  --print('in struct_declarator_list_2 ( struct_declarator_list COMMA struct_declarator )')
  return struct_declarator_list + struct_declarator
end

--   ;

-- struct_declarator

function struct_declarator_1(identifier)
  --print('in struct_declarator_1 ( IDENTIFIER )')
  return struct_decl_meta():add_id(identifier)
end

function struct_declarator_2(identifier, constant_expression)
  --print('in struct_declarator_2 ( IDENTIFIER COLON constant_expression )')
  local n = constant_expression:finalize():tonumber()
  assert(n)
  return struct_decl_meta():add_id(identifier, n)
end

--   ;

-- enum_specifier

function enum_specifier_1(enumerator_list)
  print('in enum_specifier_1 ( ENUM L_BRACE enumerator_list R_BRACE )')

end

function enum_specifier_2(identifier, enumerator_list)
  print('in enum_specifier_2 ( ENUM IDENTIFIER L_BRACE enumerator_list R_BRACE )')

end

function enum_specifier_3(enumerator_list)
  print('in enum_specifier_3 ( ENUM L_BRACE enumerator_list COMMA R_BRACE )')

end

function enum_specifier_4(identifier, enumerator_list)
  print('in enum_specifier_4 ( ENUM IDENTIFIER L_BRACE enumerator_list COMMA R_BRACE )')

end

function enum_specifier_5(identifier)
  print('in enum_specifier_5 ( ENUM IDENTIFIER )')

end

--   ;

-- enumerator_list

function enumerator_list_1(enumerator)
  print('in enumerator_list_1 ( enumerator )')

end

function enumerator_list_2(enumerator_list, enumerator)
  print('in enumerator_list_2 ( enumerator_list COMMA enumerator )')

end

--   ;

-- enumerator

function enumerator_1(identifier)
  print('in enumerator_1 ( IDENTIFIER )')

end

function enumerator_2(identifier, constant_expression)
  print('in enumerator_2 ( IDENTIFIER ASSIGN constant_expression )')

end

--   ;

-- type_qualifier

function type_qualifier_1()
  print('in type_qualifier_1 ( CONST )')

end

function type_qualifier_2()
  print('in type_qualifier_2 ( RESTRICT )')

end

function type_qualifier_3()
  print('in type_qualifier_3 ( VOLATILE )')

end

--   ;

-- function_specifier

function function_specifier_1()
  print('in function_specifier_1 ( INLINE )')

end

--   ;

-- pointer

function pointer_1()
  --print('in pointer_1 ( STAR )')
  return type_meta():add_ref("pointer")
end

function pointer_2(type_qualifier_list)
  print('in pointer_2 ( STAR type_qualifier_list )')

end

function pointer_3(pointer)
  --print('in pointer_3 ( STAR pointer )')
  return pointer:add_ref("pointer")
end

function pointer_4(type_qualifier_list, pointer)
  print('in pointer_4 ( STAR type_qualifier_list pointer )')

end

--   ;

-- type_qualifier_list

function type_qualifier_list_1(type_qualifier)
  print('in type_qualifier_list_1 ( type_qualifier )')

end

function type_qualifier_list_2(type_qualifier_list, type_qualifier)
  print('in type_qualifier_list_2 ( type_qualifier_list type_qualifier )')

end

--   ;

-- parameter_type_list

function parameter_type_list_1(parameter_list)
  --print('in parameter_type_list_1 ( parameter_list )')
  return parameter_list:finalize()
end

function parameter_type_list_2(parameter_list)
  print('in parameter_type_list_2 ( parameter_list COMMA ELLIPSIS )')

end

--   ;

-- parameter_list

function parameter_list_1(parameter_declaration)
  --print('in parameter_list_1 ( parameter_declaration )')
  return parameter_declaration
end

function parameter_list_2(parameter_list, parameter_declaration)
  --print('in parameter_list_2 ( parameter_list COMMA parameter_declaration )')
  return parameter_list + parameter_declaration
end

--   ;

-- parameter_declaration

function parameter_declaration_1(type_name, identifier)
  --print('in parameter_declaration_1 ( type_name IDENTIFIER )')
  return param_meta():add_decl(type_name, identifier)
end

--   ;

-- type_name

function type_name_1(specifier_qualifier_list)
  --print('in type_name_1 ( specifier_qualifier_list )')
  return specifier_qualifier_list:finalize()
end

function type_name_2(specifier_qualifier_list, abstract_declarator)
  --print('in type_name_2 ( specifier_qualifier_list abstract_declarator )')
  return (specifier_qualifier_list + abstract_declarator):finalize()
end

--   ;

-- abstract_declarator

function abstract_declarator_1(pointer)
  --print('in abstract_declarator_1 ( pointer )')
  return pointer
end

function abstract_declarator_2(direct_abstract_declarator)
  --print('in abstract_declarator_2 ( direct_abstract_declarator )')
  return direct_abstract_declarator
end

function abstract_declarator_3(pointer, direct_abstract_declarator)
  --print('in abstract_declarator_3 ( pointer direct_abstract_declarator )')
  return pointer + direct_abstract_declarator
end

--   ;

-- direct_abstract_declarator

function direct_abstract_declarator_1(abstract_declarator)
  print('in direct_abstract_declarator_1 ( L_PAREN abstract_declarator R_PAREN )')

end

function direct_abstract_declarator_2()
  --print('in direct_abstract_declarator_2 ( L_BRACKET R_BRACKET )')
  return type_meta():add_vararray()
end

function direct_abstract_declarator_3(assignment_expression)
  --print('in direct_abstract_declarator_3 ( L_BRACKET assignment_expression R_BRACKET )')
  local n = assignment_expression:finalize():tonumber()
  return type_meta():add_array(n)
end

function direct_abstract_declarator_4(direct_abstract_declarator)
  print('in direct_abstract_declarator_4 ( direct_abstract_declarator L_BRACKET R_BRACKET )')

end

function direct_abstract_declarator_5(direct_abstract_declarator, assignment_expression)
  --print('in direct_abstract_declarator_5 ( direct_abstract_declarator L_BRACKET assignment_expression R_BRACKET )')
  local n = assignment_expression:finalize():tonumber()
  return direct_abstract_declarator + type_meta():add_array(n)
end

function direct_abstract_declarator_6()
  print('in direct_abstract_declarator_6 ( L_BRACKET STAR R_BRACKET )')

end

function direct_abstract_declarator_7(direct_abstract_declarator)
  print('in direct_abstract_declarator_7 ( direct_abstract_declarator L_BRACKET STAR R_BRACKET )')

end

function direct_abstract_declarator_8()
  print('in direct_abstract_declarator_8 ( L_PAREN R_PAREN )')

end

function direct_abstract_declarator_9(parameter_type_list)
  print('in direct_abstract_declarator_9 ( L_PAREN parameter_type_list R_PAREN )')

end

function direct_abstract_declarator_10(direct_abstract_declarator)
  print('in direct_abstract_declarator_10 ( direct_abstract_declarator L_PAREN R_PAREN )')

end

function direct_abstract_declarator_11(direct_abstract_declarator, parameter_type_list)
  print('in direct_abstract_declarator_11 ( direct_abstract_declarator L_PAREN parameter_type_list R_PAREN )')

end

--   ;

-- initializer

function initializer_1(assignment_expression)
  print('in initializer_1 ( assignment_expression )')

end

function initializer_2(initializer_list)
  print('in initializer_2 ( L_BRACE initializer_list R_BRACE )')

end

function initializer_3(initializer_list)
  print('in initializer_3 ( L_BRACE initializer_list COMMA R_BRACE )')

end

--   ;

-- initializer_list

function initializer_list_1(initializer)
  print('in initializer_list_1 ( initializer )')

end

function initializer_list_2(designation, initializer)
  print('in initializer_list_2 ( designation initializer )')

end

function initializer_list_3(initializer_list, initializer)
  print('in initializer_list_3 ( initializer_list COMMA initializer )')

end

function initializer_list_4(initializer_list, designation, initializer)
  print('in initializer_list_4 ( initializer_list COMMA designation initializer )')

end

--   ;

-- designation

function designation_1(designator_list)
  print('in designation_1 ( designator_list ASSIGN )')

end

--   ;

-- designator_list

function designator_list_1(designator)
  print('in designator_list_1 ( designator )')

end

function designator_list_2(designator_list, designator)
  print('in designator_list_2 ( designator_list designator )')

end

--   ;

-- designator

function designator_1(constant_expression)
  print('in designator_1 ( L_BRACKET constant_expression R_BRACKET )')

end

function designator_2(identifier)
  print('in designator_2 ( DOT IDENTIFIER )')

end

--   ;

-- statement

function statement_1(closed_statement)
  --print('in statement_1 ( closed_statement )')
  return closed_statement:finalize()
end

function statement_2(opened_statement)
  print('in statement_2 ( opened_statement )')

end

--   ;

-- if_clause

function if_clause_1(expression)
  print('in if_clause_1 ( IF L_PAREN expression R_PAREN )')

end

--   ;

-- do_clause

function do_clause_1()
  print('in do_clause_1 ( DO )')

end

--   ;

-- for_clause

function for_clause_1(expression_statement, expression_statement)
  print('in for_clause_1 ( FOR L_PAREN expression_statement expression_statement R_PAREN )')

end

function for_clause_2(expression_statement, expression_statement, expression)
  print('in for_clause_2 ( FOR L_PAREN expression_statement expression_statement expression R_PAREN )')

end

function for_clause_3(declaration, expression_statement)
  print('in for_clause_3 ( FOR L_PAREN declaration expression_statement R_PAREN )')

end

function for_clause_4(declaration, expression_statement, expression)
  print('in for_clause_4 ( FOR L_PAREN declaration expression_statement expression R_PAREN )')

end

--   ;

-- while_clause

function while_clause_1(expression)
  print('in while_clause_1 ( WHILE L_PAREN expression R_PAREN )')

end

--   ;

-- label

function label_1(identifier)
  print('in label_1 ( IDENTIFIER COLON )')

end

function label_2(constant_expression)
  print('in label_2 ( CASE constant_expression COLON )')

end

function label_3()
  print('in label_3 ( DEFAULT COLON )')

end

--   ;

-- switch_clause

function switch_clause_1(expression)
  print('in switch_clause_1 ( SWITCH L_PAREN expression R_PAREN )')

end

--   ;

-- opened_statement

function opened_statement_1(if_clause, statement)
  print('in opened_statement_1 ( if_clause statement )')

end

function opened_statement_2(if_clause, closed_statement, opened_statement)
  print('in opened_statement_2 ( if_clause closed_statement ELSE opened_statement )')

end

function opened_statement_3(do_clause, opened_statement, while_clause)
  print('in opened_statement_3 ( do_clause opened_statement while_clause SEMICOLON )')

end

function opened_statement_4(for_clause, opened_statement)
  print('in opened_statement_4 ( for_clause opened_statement )')

end

function opened_statement_5(while_clause, opened_statement)
  print('in opened_statement_5 ( while_clause opened_statement )')

end

function opened_statement_6(label, opened_statement)
  print('in opened_statement_6 ( label opened_statement )')

end

--   ;

-- closed_statement

function closed_statement_1(if_clause, closed_statement, closed_statement)
  print('in closed_statement_1 ( if_clause closed_statement ELSE closed_statement )')

end

function closed_statement_2(do_clause, closed_statement, while_clause)
  print('in closed_statement_2 ( do_clause closed_statement while_clause SEMICOLON )')

end

function closed_statement_3(for_clause, closed_statement)
  print('in closed_statement_3 ( for_clause closed_statement )')

end

function closed_statement_4(while_clause, closed_statement)
  print('in closed_statement_4 ( while_clause closed_statement )')

end

function closed_statement_5(label, closed_statement)
  print('in closed_statement_5 ( label closed_statement )')

end

function closed_statement_6(expression_statement)
  --print('in closed_statement_6 ( expression_statement )')
  return expression_statement
end

function closed_statement_7(jump_statement)
  print('in closed_statement_7 ( jump_statement )')

end

function closed_statement_8(switch_clause, compound_statement)
  print('in closed_statement_8 ( switch_clause compound_statement )')

end

function closed_statement_9(compound_statement)
  print('in closed_statement_9 ( compound_statement )')

end

--   ;

-- compound_statement

function compound_statement_1()
  print('in compound_statement_1 ( L_BRACE R_BRACE )')
  return block_meta():finalize()
end

function compound_statement_2(block_item_list)
  --print('in compound_statement_2 ( L_BRACE block_item_list R_BRACE )')
  return block_item_list:finalize()
end

--   ;

-- block_item_list

function block_item_list_1(block_item)
  --print('in block_item_list_1 ( block_item )')
  return block_item
end

function block_item_list_2(block_item_list, block_item)
  --print('in block_item_list_2 ( block_item_list block_item )')
  return block_item_list + block_item
end

--   ;

-- block_item

function block_item_1(declaration)
  --print('in block_item_1 ( declaration )')
  return block_meta():add_decl(declaration)
end

function block_item_2(statement)
  --print('in block_item_2 ( statement )')
  return block_meta():add_stmt(statement)
end

--   ;

-- expression_statement

function expression_statement_1()
  print('in expression_statement_1 ( SEMICOLON )')

end

function expression_statement_2(expression)
  --print('in expression_statement_2 ( expression SEMICOLON )')
  return stmt_meta():add_expr(expression)
end

function expression_statement_3(compound_statement)
  print('in expression_statement_3 ( L_PAREN compound_statement R_PAREN SEMICOLON )')

end

--   ;

-- jump_statement

function jump_statement_1(identifier)
  print('in jump_statement_1 ( GOTO IDENTIFIER SEMICOLON )')

end

function jump_statement_2()
  print('in jump_statement_2 ( CONTINUE SEMICOLON )')

end

function jump_statement_3()
  print('in jump_statement_3 ( BREAK SEMICOLON )')

end

function jump_statement_4()
  print('in jump_statement_4 ( RETURN SEMICOLON )')

end

function jump_statement_5(expression)
  print('in jump_statement_5 ( RETURN expression SEMICOLON )')

end

--   ;

-- source

function source_1(translation_unit)
  --print('in source_1 ( translation_unit )')
  return translation_unit:finalize()
end

--   ;

-- translation_unit

function translation_unit_1(external_declaration)
  --print('in translation_unit_1 ( external_declaration )')
  return external_declaration
end

function translation_unit_2(translation_unit, external_declaration)
  --print('in translation_unit_2 ( translation_unit external_declaration )')
  return translation_unit + external_declaration
end

--   ;

-- external_declaration

function external_declaration_1(function_definition)
  --print('in external_declaration_1 ( function_definition )')
  return trans_meta():add_fun(function_definition);
end

function external_declaration_2(declaration)
  --print('in external_declaration_2 ( declaration )')
  return trans_meta():add_decl(declaration);
end

--   ;

-- function_definition

function function_definition_1(type_name, identifier, parameter_type_list, compound_statement)
  --print('in function_definition_1 ( type_name IDENTIFIER L_PAREN parameter_type_list R_PAREN compound_statement )')
  local f = func_meta()
  f:add_ret(type_name)
  f:add_id(identifier)
  f:add_param(parameter_type_list)
  f:add_comp(compound_statement)
  return f:finalize()
end

function function_definition_2(function_specifier, type_name, identifier, parameter_type_list, compound_statement)
  print('in function_definition_2 ( function_specifier type_name IDENTIFIER L_PAREN parameter_type_list R_PAREN compound_statement )')

end

--   ;

