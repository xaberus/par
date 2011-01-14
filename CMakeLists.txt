cmake_minimum_required (VERSION 2.6)
project(PAR)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/Modules)

find_package(RAGEL)
find_package(BISON)

include_directories(
  /repos/butcher/
  /home/xa/workspace/nih/
)

set(CMAKE_C_FLAGS "-std=gnu99 -Wall -Wextra -pedantic -g -ggdb -DTEST -DNPOOL -fPIC")
set(CMAKE_SHARED_LIBRARY_C_FLAGS "-Wl,--no-undefined")
#-Werror -fprofile-arcs -ftest-coverage

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/parser.y
  COMMAND /usr/bin/lua tr.lua  ${PAR_SOURCE_DIR}/parser.in > ${PAR_SOURCE_DIR}/parser.y
  DEPENDS parser.in tr.lua generator.lua
)
add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/parser.lua
  COMMAND /usr/bin/lua ta.lua  ${PAR_SOURCE_DIR}/parser.in > ${PAR_SOURCE_DIR}/parser.lua
  DEPENDS parser.in ta.lua generator.lua handlers.inc.lua tmpl.inc.lua
)

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test
  COMMAND cp test.in test
  DEPENDS parser.in ta.lua generator.lua handlers.inc.lua tmpl.inc.lua test.in
)

RAGEL_TARGET(lexer lexer.rl ${CMAKE_CURRENT_BINARY_DIR}/lexer.c)
BISON_TARGET(
	parser parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.c
	COMPILE_FLAGS -Wall
)

include_directories(${libnih_SOURCE_DIR})

add_executable(par
  ${BISON_parser_OUTPUTS}
  ${RAGEL_lexer_OUTPUTS}
  main.c
  parser.lua
  test
)

target_link_libraries(par m lua)

