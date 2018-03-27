  /*
   *    Example of getting files from ARCHIVES...
   *
   *    user_program -find t950101.r01
   *
   *    (fg_flag* are command line parsing routines)
   */

fg_flags (argc, argv);                  /* init parser */

if (fg_flag ("find")) {                 /* look for -find or +find *//* fg_flagc returns char string following */
  input = UTIL_find_open (fg_flagc ("find"), "rb");
  if (input)
    fprintf (stderr, "find: %s\n", UTIL_find_name ());
  else {
    fprintf (stderr, "file not found: %s\n", fg_flagc ("find"));
    exit (0);
  }
}
