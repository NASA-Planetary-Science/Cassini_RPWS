struct sym_tab_entry
{
  int x;
  int y;
  int index;
};

#ifndef _SYMBOL
extern struct sym_tab_entry *symbol[128];
extern int symbol_read (void);
extern int symbol_value (float *, float *, char *, float);
extern struct sym_tab_entry *symbol_entry (int);
#endif
