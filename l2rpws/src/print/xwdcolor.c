#include <stdlib.h>
static char command[] = { "/usr/bin/X11/xwd | "
    "remsh etamin -l wtr "
    "'(cat > /opt/project/cassini/data/xwd/xwd_temp.xwd ; "
    "xprpjet -dither -hires -scale %s "
    "-output /opt/project/cassini/data/xwd/xwd_temp.pcl "
    "/opt/project/cassini/data/xwd/xwd_temp.xwd ; "
    "lpr -P pcxterm3_hp /opt/project/cassini/data/xwd/xwd_temp.pcl ; "
    "rm /opt/project/cassini/data/xwd/xwd_temp.xwd ; "
    "rm /opt/project/cassini/data/xwd/xwd_temp.pcl)' "
};
int main (int argc, char *argv[])
{
  int status;
  char temp[8192];

  sprintf (temp, command, argv[1]);
  status = system (temp);
  exit (status);
}
