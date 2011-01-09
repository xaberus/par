#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <err.h>

typedef struct lexer {
  int cs; /* current state */
  unsigned char *source; /* pointer to source */
  const unsigned char *p; /* pointer to source */
  const unsigned char *pe; /* pointer to source end + 1 */
  const unsigned char *eof; /* end-of-file indicator */

  int act;
  const unsigned char *ts; /* token start */
  const unsigned char *te; /* token end */

  unsigned int line;
  const unsigned char *line_start;

  int token_type;
  
  int type_ref;

  int done;
} lexer_t;

err_t lexer_init(lexer_t *lexer, unsigned char *source, size_t length);
err_t lexer_next(lexer_t *lexer, lua_State * L);
err_t lexer_deinit(lexer_t *lexer);

typedef struct node {
  int type;
} node_t;

typedef struct token {
  int type;
  const unsigned char * start;
  const unsigned char * end;
  unsigned int line;
} token_t;

typedef struct parser {
  lexer_t lexer;
  err_t   last_err;

  lua_State * L;
} parser_t;


#define YYDEBUG 1
#define YYTOKEN_TABLE 1

