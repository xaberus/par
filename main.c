#include "parser.inc.h"
#include "parser.h"

#include <malloc.h>

parser_t * parser_init(parser_t *parser)
{
  if (!parser)
    return NULL;
  
  parser->L = lua_open();
  if (!parser->L)
    return NULL;
  
  luaL_openlibs(parser->L);
  
  int e = luaL_dostring(parser->L,
      "require 'dump'\n"
      "require 'parser'\n"
  );
  if (e != 0) {
    fprintf(stderr, "error %s\n", lua_tostring(parser->L, -1));
  }
  
  lua_newtable(parser->L);
  lua_setfield(parser->L, LUA_GLOBALSINDEX, "typedefs");

  return parser;
}

void parser_clear(parser_t *parser)
{
  if (!parser)
    return;

  lua_close(parser->L);
  
}

extern int yydebug;

int main(int argc, char * argv[], char * envv[])
{
  //yydebug=1;
  if (argc > 1) {
    FILE *fp=fopen(argv[1], "rb");
    if (fp) {
      unsigned char *source;
      size_t length;

      if (fseek(fp, 0,SEEK_END)==0) {
        length=ftell(fp);

        source=malloc(length+1);

        if (source) {
          if (fseek(fp, 0, SEEK_SET)==0) {
            if (fread(source, length, 1, fp) == 1) {
              source[length]=0;

              parser_t parser;
              if (parser_init(&parser)) {
                /*lexer_init(&parser->lexer, source, length);

                while(!parser.lexer.done) {
                cs_lexer_next(&parser.lexer);
                }*/

                lexer_init(&parser.lexer, source, length);

                yyparse(&parser);
                
                int e = luaL_dostring(parser.L,
                  //"dump(tree,nil,nil)\n"
                  "parser.source(tree)\n"
                  //"dump(typedefs, 0, 0)\n"
                  //"print('done')"
                );
                if (e != 0) {
                  fprintf(stderr, "error %s\n", lua_tostring(parser.L, -1));
                }

                lexer_deinit(&parser.lexer);

                parser_clear(&parser);
              }
            }
          }
          free(source);
        }
      }
      fclose(fp);
    }
  }

  return 0;
}
