/*
** interface.c for interface.c in /home/empty/Projets/hg/c_event/
**
** Made by baptiste careil
** Login   <careil_b@epitech.net>
**
** Started on  Wed Aug 31 15:40:53 2011 baptiste careil
** Last update Thu Sep  1 11:43:54 2011 baptiste careil
*/

#include <curses.h>
#include <stdlib.h>
#include "interface.h"
#include "utils.h"
#include "main.h"

t_interface	*create_interface() {
  t_interface	*this;

  this = xmalloc(sizeof(*this));
  this->parent.tick = interface_tick;
  this->parent.handle_key = interface_handle_key;
  this->parent.draw = interface_draw;
  this->parent.dtor = destroy_interface;
  this->menu = create_menu("Jouer\nTest\nQuitter\n");
  this->game = create_game();
  this->test = create_test();
  this->parent.child = &this->menu->parent;
  this->first_draw = 1;
  this->ticked = 0;
  this->paused = 0;

  menu_set_handler(this->menu, 0, interface_on_play, &this->parent);
  menu_set_handler(this->menu, 1, interface_on_test, &this->parent);
  menu_set_handler(this->menu, 2, interface_on_quit, &this->parent);
  return this;
}

void		destroy_interface(t_child *me) {
  t_interface	*this;

  this = (t_interface *)me;
  this->game->parent.dtor(&this->game->parent);
  this->menu->parent.dtor(&this->menu->parent);
  this->test->parent.dtor(&this->test->parent);
  free(this);
}

void		interface_tick(t_child *me) {
  t_interface	*this;

  this = (t_interface *)me;
  this->ticked++;
  if (this->paused == 0 && this->parent.child) {
    this->parent.child->tick(this->parent.child);
  }
}

void		interface_draw(t_child *me) {
  t_interface	*this;

  this = (t_interface *)me;
  if (this->first_draw) {
    mvprintw(0, 0, "Interface: ticked = %i", this->ticked);
    mvprintw(0, 30, "P has been pressed : %s", (this->paused ? "yes" : "no "));
    this->first_draw = 0;
  }
  else {
    mvprintw(0, 20, "%i", this->ticked);
    mvaddstr(0, 51, (this->paused ? "yes" : "no "));
  }
  if (this->parent.child) {
    this->parent.child->draw(this->parent.child);
  }
}

void		interface_handle_key(t_child *me, int key) {
  t_interface	*this;

  this = (t_interface *)me;
  if (key == 'P') this->paused = !this->paused;
  else if (key == 'Q') glb_loop = 0;
  else if (key == '0') {
    if (this->parent.child == &this->menu->parent) return;
    this->first_draw = 1;
    this->parent.child = &this->menu->parent;
    menu_reset(this->menu);
    erase();
  }
  else if (this->parent.child) {
    this->parent.child->handle_key(this->parent.child, key);
  }
}

void		interface_on_play(t_child *me) {
  t_interface	*this;

  this = (t_interface *)me;
  this->parent.child = &this->game->parent;
  game_reset(this->game);
  this->first_draw = 1;
  erase();
}

void		interface_on_test(t_child *me) {
  t_interface	*this;

  this = (t_interface *)me;
  this->parent.child = &this->test->parent;
  test_reset(this->test);
  this->first_draw = 1;
  erase();
}

void		interface_on_quit(t_child *me) {
  (void) me;
  glb_loop = 0;
}
