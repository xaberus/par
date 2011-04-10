#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

typedef struct parser {
  lua_State * L;
  int ref;
  int lexref;
  int envref;
  int astref;

  int tokref;
} parser_t;



