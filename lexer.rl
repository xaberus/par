
#include "parser.inc.h"
#include "parser.h"

%%{
	machine clex;
	
	alphtype unsigned char;

	access lexer->;
	variable p lexer->p;
	variable pe lexer->pe;
	variable eof lexer->eof;
	variable stack lexer->stack;
	variable top lexer->top;
	variable act lexer->act;
	variable ts lexer->ts;
	variable te lexer->te;

	action on_eof {
		lexer->done = 1;
		lexer->token_type = END;
	}

	# "http://www.unicode.org/versions/Unicode5.0.0/ch03.pdf#G7404"
	utf8_char =	(any)
							#(ascii)
							#|	(0xC2..0xDF	0x80..0xBF)
							#|	(0xE0				0xA0..0xBF	0x80..0xBF)
							#|	(0xE1..0xEC	0x80..0xBF	0x80..0xBF)
							#|	(0xED				0x80..0x9F	0x80..0xBF)
							#|	(0xEE..0xEF	0x80..0xBF	0x80..0xBF)
							#|	(0xF0				0x90..0xBF	0x80..0xBF	0x80..0xBF)
							#|	(0xF1..0xF3	0x80..0xBF	0x80..0xBF	0x80..0xBF)
							#|	(0xF4				0x80..0x8F	0x80..0xBF	0x80..0xBF)
							;

	comment_char = utf8_char -- (0x00..0x07 | 0x14..0x1F | 0x7f);
	string_char = utf8_char -- (0x00..0x1F | 0x7f);

	oct_digit = ('0'..'7');
	dec_digit = ('0'..'9');
	hex_digit = ('0'..'9'|'a'..'f'|'A'..'F');

	int_postfix = [lL]?[uU]?;
	float_postfix = [fF]?[lL]?;

	escape =	('\\'[0abfnrtv\\])
						| ('\\x' hex_digit hex_digit)
						| ('\\o' ('0'..'3') oct_digit oct_digit) # right?
						| ('\\u' hex_digit hex_digit hex_digit hex_digit);
	
	newline = '\n' @{ lexer->line++; lexer->line_start=lexer->ts; };
	whitespace = (' ' | '\a' | '\b' | '\f' | newline | '\r' | '\t' | '\v');

	alpha_u = (alpha | '_');
	alnum_u = (alnum | '_');

	string_literal =
		(
			'"' ((string_char -- ('"'|'\\'|"\\\"")) | escape | "\\\"")* '"'
		) %{ 
					lexer->token_type = STRING_CONSTANT;
			};

	char_literal =
		(
			"'" ((string_char -- ("'" | "\\\'")) | escape | '\\"' |"\\\'")? "'"
		) %{
				lexer->token_type = CHAR_CONSTANT;
			};
	
	oct_literal =
		(
			'0' oct_digit+
		) %{
				lexer->token_type = OCT_INT_CONSTANT;
			};

	dec_literal =
		(
			('0')
			|	(dec_digit -- '0') dec_digit*
		) %{
				lexer->token_type = DEC_INT_CONSTANT;
			};

	hex_literal =
		(
			'0' [xX] hex_digit+
		) %{
				lexer->token_type = HEX_INT_CONSTANT;
			};

	float_literal =
		(
			(
				((dec_digit -- '0') dec_digit* '.' dec_digit+)
				| ('.' dec_digit+)
				| ((dec_digit -- '0') dec_digit* '.')
				| ( '0.' dec_digit*)
			) ([eE]dec_digit+)?
		) %{
				lexer->token_type = FLOAT_CONSTANT;
			};
	
	identifier = 
		(
			(alpha_u alnum_u* )
		) %{
				/* TODO type registry */
        lexer->token_type = TYPE_NAME;
			};

	keyword = (
			"auto" %{ lexer->token_type = AUTO; }
		|	"break" %{ lexer->token_type = BREAK; }
		| "case" %{ lexer->token_type = CASE; }
		| "char" %{ lexer->token_type = CHAR; }
		| "const" %{ lexer->token_type = CONST; }
		| "continue" %{ lexer->token_type = CONTINUE; }
		| "default" %{ lexer->token_type = DEFAULT; }
		| "do" %{ lexer->token_type = DO; }
		| "double" %{ lexer->token_type = DOUBLE; }
		| "else" %{ lexer->token_type = ELSE; }
		| "enum" %{ lexer->token_type = ENUM; }
		| "extern" %{ lexer->token_type = EXTERN; }
		| "float" %{ lexer->token_type = FLOAT; }
		| "for" %{ lexer->token_type = FOR; }
		| "goto" %{ lexer->token_type = GOTO; }
		| "if" %{ lexer->token_type = IF; }
		| "int" %{ lexer->token_type = INT; }
		| "long" %{ lexer->token_type = LONG; }
		| "register" %{ lexer->token_type = REGISTER; }
		| "return" %{ lexer->token_type = RETURN; }
		| "short" %{ lexer->token_type = SHORT; }
		| "signed" %{ lexer->token_type = SIGNED; }
		| "sizeof" %{ lexer->token_type = SIZEOF; }
		| "static" %{ lexer->token_type = STATIC; }
		| "struct" %{ lexer->token_type = STRUCT; }
		| "switch" %{ lexer->token_type = SWITCH; }
		| "typedef" %{ lexer->token_type = TYPEDEF; }
		| "union" %{ lexer->token_type = UNION; }
		| "unsigned" %{ lexer->token_type = UNSIGNED; }
		| "void" %{ lexer->token_type = VOID; }
		| "volatile" %{ lexer->token_type = VOLATILE; }
		| "while" %{ lexer->token_type = WHILE; }

		| "int8" %{ lexer->token_type = INT8; }
		| "int16" %{ lexer->token_type = INT16; }
		| "int32" %{ lexer->token_type = INT32; }
		| "int64" %{ lexer->token_type = INT64; }
		
		| "uint8" %{ lexer->token_type = UINT8; }
		| "uint16" %{ lexer->token_type = UINT16; }
		| "uint32" %{ lexer->token_type = UINT32; }
		| "uint64" %{ lexer->token_type = UINT64; }

		| "bool" %{ lexer->token_type = BOOL; }
		| "complex" %{ lexer->token_type = COMPLEX; }
		| "imaginary" %{ lexer->token_type = IMAGINARY; }
		| "restrict" %{ lexer->token_type = RESTRICT; }
		| "inline" %{ lexer->token_type = INLINE; }

	) (
			(^alnum_u)
			%{fhold;}
		);

	operator = (
			("..." %{ lexer->token_type = ELLIPSIS; })
		|	(">>=" %{ lexer->token_type = RIGHT_ASSIGN; })
		|	("<<=" %{ lexer->token_type = LEFT_ASSIGN; })
		|	("+=" %{ lexer->token_type = ADD_ASSIGN; })
		|	("-=" %{ lexer->token_type = SUB_ASSIGN; })
		|	("*=" %{ lexer->token_type = MUL_ASSIGN; })
		|	("/=" %{ lexer->token_type = DIV_ASSIGN; })
		|	("%=" %{ lexer->token_type = MOD_ASSIGN; })
		|	("&=" %{ lexer->token_type = AND_ASSIGN; })
		|	("^=" %{ lexer->token_type = XOR_ASSIGN; })
		|	("|=" %{ lexer->token_type = OR_ASSIGN; })
		|	(">>" %{ lexer->token_type = RIGHT_OP; })
		|	("<<" %{ lexer->token_type = LEFT_OP; })
		|	("++" %{ lexer->token_type = INC_OP; })
		|	("--" %{ lexer->token_type = DEC_OP; })
		|	("->" %{ lexer->token_type = PTR_OP; })
		|	("&&" %{ lexer->token_type = AND_OP; })
		|	("||" %{ lexer->token_type = OR_OP; })
		|	("<=" %{ lexer->token_type = LE_OP; })
		|	(">=" %{ lexer->token_type = GE_OP; })
		|	("==" %{ lexer->token_type = EQ_OP; })
		|	("!=" %{ lexer->token_type = NE_OP; })
		|	(";" %{ lexer->token_type = SEMICOLON; })
		|	("{" %{ lexer->token_type = L_BRACE; })
		|	("}" %{ lexer->token_type = R_BRACE; })
		|	("," %{ lexer->token_type = COMMA; })
		|	(":" %{ lexer->token_type = COLON; })
		|	("=" %{ lexer->token_type = ASSIGN; })
		|	("(" %{ lexer->token_type = L_PAREN; })
		|	(")" %{ lexer->token_type = R_PAREN; })
		|	("[" %{ lexer->token_type = L_BRACKET; })
		|	("]" %{ lexer->token_type = R_BRACKET; })
		|	("." %{ lexer->token_type = DOT; })
		|	("&" %{ lexer->token_type = AMPERSAND; })
		|	("!" %{ lexer->token_type = EXCLAMATION; })
		|	("~" %{ lexer->token_type = TILDE; })
		|	("-" %{ lexer->token_type = MINUS; })
		|	("+" %{ lexer->token_type = PLUS; })
		|	("*" %{ lexer->token_type = STAR; })
		|	("/" %{ lexer->token_type = SLASH; })
		|	("%" %{ lexer->token_type = PERCENT; })
		|	("<" %{ lexer->token_type = LESS; })
		|	(">" %{ lexer->token_type = GREATER; })
		|	("^" %{ lexer->token_type = CIRCUMFLEX; })
		|	("|" %{ lexer->token_type = PIPE; })
		|	("?" %{ lexer->token_type = QUESTION; })
		
	);

	cpp_comment_literal =
		(
			"//" (comment_char -- newline)* newline
		) %{
				lexer->token_type = LINE_COMMENT;
			};

	c_comment_literal =
		(
			"/*"
				(
					(comment_char -- ("*/" | "/*")) | newline
				)* :>>
			"*/"
		) %{
				lexer->token_type = MULTI_LINE_COMMENT;
			};


	literal =
		(
			((oct_literal | dec_literal | hex_literal) int_postfix)
			| (float_literal float_postfix)
			| string_literal
			| char_literal
		);

	cpp =
		(
			'#' ((comment_char -- ('\\' | newline)) | "\\" newline )+ newline
		);

	action on_inval {
		lexer->token_type = ERROR;
	}

	action on_preserve {
		fhold;
	}

	main := |*
		(((keyword) ((any -- alnum_u ) %on_preserve)) | identifier | operator | literal )
			>(on_inval)
			$eof(on_eof)

			{ fbreak; };
		
		cpp_comment_literal $eof on_eof;
		c_comment_literal $eof on_eof;

		cpp;
		(whitespace*) $eof on_eof;

	*|;
}%%

%% write data nofinal;

err_t lexer_init(lexer_t *lexer,
		unsigned char *source, size_t length)
{
	lexer->source=source;
	lexer->p=source;
	lexer->pe=source+length;
	lexer->eof=source+length;
	lexer->line=1;
	lexer->line_start=lexer->p;

	lexer->done=0;

	%% write init;
	return err_construct(0,0,0);
}

err_t lexer_next(lexer_t *lexer, lua_State * L)
{
	if (lexer->p < lexer->pe) {
		%% write exec;
    
    if (lexer->token_type == TYPE_NAME) {
      lua_getfield(L, LUA_GLOBALSINDEX, "typedefs");
      /*fprintf(stderr, "is type? %.*s\n",
          (int)(lexer->te - lexer->ts), (const char *)lexer->ts);*/
      lua_pushlstring(L, (const char *)lexer->ts, lexer->te - lexer->ts);
      lua_rawget(L, -2);
      if (lua_istable(L, -1)) {
        lexer->token_type = TYPE_NAME;
        lexer->type_ref = lua_ref(L, LUA_REGISTRYINDEX);
        /*fprintf(stderr, "DEF!\n");*/
      } else {
        lua_pop(L, 1);
        lexer->token_type = IDENTIFIER;
      }
      lua_pop(L, 1);
    }

		//fprintf(stderr, "p %p, pe: %p\n", lexer->p, lexer->pe);

		if (lexer->cs==clex_error) {
			const unsigned char *end=NULL;
			if (lexer->line_start
					&& lexer->line_start<lexer->pe) {
				for (const unsigned char *p=lexer->line_start;
					p<lexer->pe && p && *p; p++) {
					if (*p=='\n') {	
						end=p; break;
					}
				}
			}
			
			if (end) {
				fprintf(stderr, "Syntax error at line %u: '%.*s'\n",
					lexer->line,
					(int)(end-lexer->line_start), lexer->line_start);
			} else {
				fprintf(stderr, "Syntax error at line %u\n", lexer->line);
			}
				lexer->done=1;
			}
	} else {
		fprintf(stderr, "EOF\n");
		lexer->done=1;
	}
	return err_construct(0,0,0);
}

err_t lexer_deinit(lexer_t *lexer) {
	//%% write exit;
  /* unsused */
  (void)lexer;
	return err_construct(0,0,0);
}



