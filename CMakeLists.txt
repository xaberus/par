cmake_minimum_required (VERSION 2.6)
project(PAR)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/Modules)

find_package(BISON)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu99")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wextra")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -pedantic")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -ggdb")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0")
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror")
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fprofile-arcs")
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ftest-coverage")

set(CMAKE_SHARED_LIBRARY_C_FLAGS "-Wl,--no-undefined")


add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.lua
  COMMAND /usr/bin/luajit generator.lua  ${PAR_SOURCE_DIR}/parser.in
  DEPENDS parser.in generator.lua
)

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test
  COMMAND cp test.in test
  DEPENDS parser.in generator.lua test.in
)

BISON_TARGET(
  parser parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.c
  COMPILE_FLAGS "-Wall -r all"
)

include_directories(${libnih_SOURCE_DIR})

add_library(clex_parse SHARED
  ${BISON_parser_OUTPUTS}
)

target_link_libraries(clex_parse m luajit-5.1)
