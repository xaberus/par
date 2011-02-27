#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <err.h>

typedef struct parser {
  err_t   last_err;

  lua_State * L;
  int ref;
  int lexref;
  int envref;
  int astref;

  int tokref;
} parser_t;



