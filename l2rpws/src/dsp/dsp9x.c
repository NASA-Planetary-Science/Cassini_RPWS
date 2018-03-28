#include <stdio.h>

#include "webutil.h"

char *dsp9x_title = { "(dsp9x) 1.4" };

int dsp9x (char *Version, int flag)
{
  if (flag)
    fprintf (stdout, "<br>");
  fprintf (stdout, "dsp9x(\"%s\", \"%s\");\n", Version, dsp9x_title);
  if (!flag)
    fprintf (stdout, "     HELP SCREEN is routed to stdout\n");
  fprintf (stdout, "\n");
  if (flag)
    fprintf (stdout, "<H2>");
  fprintf (stdout, "  TIME\n");
  if (flag)
    fprintf (stdout, "</H2>");
  fprintf (stdout, "      When SCLK/SCET information is available it will\n");
  fprintf (stdout, "      be used to display the time of the housekeeping\n");
  fprintf (stdout,
           "      record.  The difference between CDS-SCLK and CHDO-SCLK\n");
  fprintf (stdout,
           "      is applied to the CHDO-SCET and then used to display the\n");
  fprintf (stdout,
           "      time field.  When this 'more or less' accurate time is\n");
  fprintf (stdout,
           "      avaliable, the time field displays as yyyy-dddThh:mm:ss.mmm\n");
  fprintf (stdout, "\n");
  if (flag)
    fprintf (stdout, "<H2>");
  fprintf (stdout, "  Usage\n");
  if (flag)
    fprintf (stdout, "</H2>");
  fprintf (stdout,
           "      Housekeeping is now displayed and the 30 second update\n");
  fprintf (stdout,
           "      seems to work with at least Netscape.  The source of the\n");
  fprintf (stdout,
           "      housekeeping is based on the FORM/POST/INPUT/TYPE HTML tags.\n");
  if (flag)
    fprintf (stdout, "<dl type=\"A\">");
  if (flag)
    fprintf (stdout, "<dt>");
  fprintf (stdout, "      dsp9        ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "suppress housekeeping (recent data)\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      dsp9a* -h   ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "Help page in plain text\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      dsp9a       ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "Housekeeping display (stdin arguments, web style)");
  if (flag)
    fprintf (stdout, "</dd><pre><ul>");
  if (flag)
    fprintf (stdout, "<li>");
  fprintf (stdout, "            name=TEST value=VC0 \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        sciop1\n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=TEST value=VC1 \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        \n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=TEST value=ALL \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        \n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=PUSH value=VC0 \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        casrpws1 (no repeat)\n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=PUSH value=VC1 \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        \n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=PUSH value=ALL \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        \n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=NERT value=ALL \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout,
           "                        Not currently collecting VC0/VC1 separatly.\n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=NERT value=TEMP \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        Look at a temporary file\n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=DATA_EM value=ALL \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        flight spare\n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=DATA value=DSP \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        recent data\n");
  if (flag)
    fprintf (stdout, "</i><li>");
  fprintf (stdout, "            name=HELP value=HSK \n");
  if (flag)
    fprintf (stdout, "</li><spacer type=horizontal size=25><i>");
  fprintf (stdout, "                        help file\n");
  if (flag)
    fprintf (stdout, "</i></ul></pre>\n");

  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      dsp9a_vc0   ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "Real-Time VC0 data\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      dsp9a_vc1   ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "Real-Time VC1 data\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      dsp9a_all   ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "Real-Time VC0&VC1 data\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      dsp9a_nert  ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "Post-Pass VC0&VC1 data\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      dsp9a_em    ");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "Flight Spare (BENCH) data\n");

  if (flag)
    fprintf (stdout, "</dl><p>\n");
  fprintf (stdout, "      \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "    Preferences\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "      Your browser preferences may be used to select the font and\n");
  fprintf (stdout,
           "      character size you would like to see displayed.  The housekeeping\n");
  fprintf (stdout,
           "      display should be displayed as a fixed spacing font while the\n");
  fprintf (stdout,
           "      recent data at the bottom would typically show up as a mix of\n");
  fprintf (stdout, "      fixed and proportional fonts.\n");
  if (flag)
    fprintf (stdout, "<p>");
  fprintf (stdout,
           "      The author prefers to set both fixed and proportional fonts\n");
  fprintf (stdout,
           "      to 16 or 18 to make the display easy to read.  The help file\n");
  fprintf (stdout,
           "      headings remain more graduated when a font size of 14 is\n");
  fprintf (stdout, "      selected.\n");
  fprintf (stdout, "      \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "    Printing\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "      You should be able to produce hardcopy using the browsers\n");
  fprintf (stdout,
           "      print button.  Color should come through (such as Red Alarms,\n");
  fprintf (stdout,
           "      Yellow Alarms, Blue titles, etc.) as with any other web page.\n");
  fprintf (stdout,
           "      Since this a simple web page, it should honor the font selections\n");
  fprintf (stdout,
           "      you have set up in the preferences secion for the browser.\n");
  if (flag)
    fprintf (stdout, "<p>");
  fprintf (stdout,
           "      One side-effect of using this simple web design being that\n");
  fprintf (stdout,
           "      text based browsers (such a LYNX) may be used to monitor\n");
  fprintf (stdout,
           "      housekeeping.  This opens up the possibility of using a\n");
  fprintf (stdout,
           "      simple dial-up to access an internet connected machine and\n");
  fprintf (stdout,
           "      using a text based browser to monitor housekeeping.\n");
  fprintf (stdout, "      \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "    Sound\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "      There are provisions for rudimentary sound in <i>dsp9a</i>\n");
  fprintf (stdout,
           "      although this feature is currently disabled.  It was intended\n");
  fprintf (stdout,
           "      to allow a display to <i>scream</i> for attention when a\n");
  fprintf (stdout,
           "      red/yellow alarm occurs.  Unfortunately this scheme rather\n");
  fprintf (stdout,
           "      falls apart if there is an ongoing condition that reuqired attention.\n");
  if (flag)
    fprintf (stdout, "<p>");
  fprintf (stdout,
           "      Perhaps we will resolve things in a future release\n");
  if (flag)
    fprintf (stdout, "<H1>");
  fprintf (stdout, "Housekeeping Display Format Notes\n");
  if (flag)
    fprintf (stdout, "</H1>");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  Timetags\n");
  if (flag)
    fprintf (stdout, "</H3>");
  if (flag)
    fprintf (stdout, "<dl><dt>");
  fprintf (stdout, "      UT");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "        UT is obtained from the workstation as the\n");
  fprintf (stdout, "        display is formatted.  Since the workstation\n");
  fprintf (stdout,
           "        is typically making use of NTP, time should be\n");
  fprintf (stdout, "        good to the mS level.\n");
  if (flag)
    fprintf (stdout, "</dd><p><dd>");
  fprintf (stdout, "      Also appearing on this line are two additional\n");
  fprintf (stdout, "        times, the first being the difference between\n");
  fprintf (stdout,
           "        UT and ERT, representing the processing and delivery\n");
  fprintf (stdout,
           "        delay.  The second number is the update period, in seconds,\n");
  fprintf (stdout,
           "        how frequently your browser should requery for new housekeeping.\n");
  if (flag)
    fprintf (stdout, "</dd><p><dl>");
  fprintf (stdout,
           "    The first three time lines are color coded as a visual aid:\n");
  if (flag)
    fprintf (stdout, "<p><dt>");
  fprintf (stdout, "      Black");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        indicates that the ground delay is less than one minute.\n");
  fprintf (stdout,
           "        It seems like we see a delay on the order of 5 to 10 seconds\n");
  fprintf (stdout,
           "        when receiving real-time data (This display page update rate\n");
  fprintf (stdout,
           "        may make the number here appear high by up to 30 seconds).\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      Red");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        indicates that the ground delay is in excess of one minute\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      Yellow");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        indicates that the ground delay is in excess of one hour\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      Brown");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        indicates that the ground delay is in excess of one day\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd></dl><p>");
  if (flag)
    fprintf (stdout, "<dt>");
  fprintf (stdout, "      UT-ERT");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "        The UT-ERT field is not present when \n");
  fprintf (stdout,
           "        the delay is longer than 24 hours.  Note that this\n");
  fprintf (stdout,
           "        field is not explicitly labeled, it simply appears\n");
  fprintf (stdout, "        on the UT line when needed.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      ERT");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "        ERT is placed in a CHDO record that \n");
  fprintf (stdout,
           "        is appended to the data at JPL.  Earth Receive Time.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      RCT");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "        RCT is placed in a CHDO record that \n");
  fprintf (stdout,
           "        is appended to the data at JPL.  Record Creation Time.  \n");
  fprintf (stdout,
           "        This should be close to ERT unless the record was recovered\n");
  fprintf (stdout, "        during post-pass processing.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<p>");
  fprintf (stdout,
           "        This field is color coded to indicate something about\n");
  fprintf (stdout,
           "        when the record was processed.  It seems that the\n");
  fprintf (stdout,
           "        data is processed within 1 day for the most part.\n");
  fprintf (stdout,
           "        We estimate the delay in processing using RCT - ERT.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<p><dl><dt>");
  fprintf (stdout, "        No change in color\n");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "           indicates the record has been processed within 1 day of the\n");
  fprintf (stdout, "           records reception.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "        Yellow\n");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "           indicates the record has been processed more than 1 day after\n");
  fprintf (stdout, "           the record was received.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "        Red\n");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "           indicates the record has been processed more than 2 days after\n");
  fprintf (stdout, "           the record was received.\n");
  fprintf (stdout, "           \n");
  if (flag)
    fprintf (stdout, "</dd></dl><p>");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      SCET");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout, "        SCET is calculated from the spacecraft clock \n");
  fprintf (stdout,
           "        on the ground.  It is based on the latest estimates and \n");
  fprintf (stdout, "        measurements of how SCLK relates to UT.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      SCLK");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        SCLK is the Spacecraft Clock placed in the housekeeping\n");
  fprintf (stdout,
           "        the housekeeping record by RPWS.  The SCLK is deliveres \n");
  fprintf (stdout, "        (by CDS) to RPWS once per second.\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      RTI-16");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        RTI-16 is the lower 13 bits of seconds and the 3 bit RTI \n");
  fprintf (stdout,
           "        field.  This matches the value that appears in mini-packets \n");
  fprintf (stdout,
           "        and micro-packets (in particular, the micro-packets that \n");
  fprintf (stdout, "        appear later in the page).\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      OWLT");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        OWLT (One Way Light Time) will also appear on \n");
  fprintf (stdout,
           "        this line when the calculated delay is less than 3 hours.  \n");
  fprintf (stdout, "        This is the difference between SCET and ERT.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "      CCSDS");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "        CCSDS is the CCSDS header, the first six words\n");
  fprintf (stdout, "        (12 bytes) of the housekeeping packet\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "</dd></dl>");
  if (flag)
    fprintf (stdout, "<br>");
  fprintf (stdout, "      Packet Sequence is taken from the CCSDS header.\n");
  if (flag)
    fprintf (stdout, "<br>");
  if (flag)
    fprintf (stdout, "<br>");
  fprintf (stdout,
           "      VC is the virtual channel number.  This indicates real-time\n");
  fprintf (stdout, "        data (VC 0) and playback data (VC 1).\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  Processor Health Monitor\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        This section provides status information on the health of \n");
  fprintf (stdout,
           "        the Low Rate Processor.  Nominally, we are operating Science\n");
  fprintf (stdout,
           "        software with about 20%% utilization of the processor.\n");
  fprintf (stdout, "        BIU counters should be zero.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "    Operating Software:\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "      Science Software\n");
  if (flag)
    fprintf (stdout, "<br>");
  fprintf (stdout, "      ROM Software\n");
  if (flag)
    fprintf (stdout, "<br>");
  fprintf (stdout,
           "        Indicates when the Science Software is loaded.\n");
  fprintf (stdout, "        You should only see ROM following a reset.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      HALT\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        indicates the percentage of time the LRP is idle\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      BIU_rst\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        Count of the number of times that LRP has\n");
  fprintf (stdout, "        sent a reset signal to the BIU.  Should be 0.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      BIU_RTI\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Count of the number of times that RPWS has NOT\n");
  fprintf (stdout,
           "        received the RTI signal (this signal is generated\n");
  fprintf (stdout, "        internally when S/C fails to send it.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  Deploy Mechanism Monitor\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        As the antenna elements were successfully extended within a\n");
  fprintf (stdout,
           "        few days of launch, this display should indicate that all\n");
  fprintf (stdout, "        3 elements are extended.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Antenna I\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Shows the analog channel that measures the current\n");
  fprintf (stdout,
           "        through the antenna deploy motor.  The deploy motor is\n");
  fprintf (stdout,
           "        a 3-phase AC motor, so this is not particularly useful\n");
  fprintf (stdout,
           "        as a stall current monitor.  This reading should hover\n");
  fprintf (stdout, "        at ZERO.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Antenna Limit\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        Port C0 IN.\n");
  fprintf (stdout, "        Shows the current status of the \n");
  fprintf (stdout,
           "        limit switches in the deploy mechanism.  All 3\n");
  fprintf (stdout,
           "        extend limit switches should indicate they are set.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Antenna Position\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        As with the motor current, the position pots are\n");
  fprintf (stdout,
           "        not calibrated.  In this case, however, the readings\n");
  fprintf (stdout, "        should be, more or less, constant.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  Temperature Monitors\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        With only the search coil exposed to space, most of the\n");
  fprintf (stdout,
           "        temperature indications should be around 20 C.  The search \n");
  fprintf (stdout, "        coil runs near -60 C.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      P/S\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Power Supply Thermistor.  Measures the temperature\n");
  fprintf (stdout,
           "        in the HFR electronics package (i.e. within the power supply)\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      S/C\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Search Coil Thermistor.  The Search Coil is on a bracket\n");
  fprintf (stdout,
           "        that supports the HGA, exposed to space.  An RHU is used\n");
  fprintf (stdout, "        used to provide a small ammount of heating.\n");
  fprintf (stdout, "        This is a very cold point (i.e. -60C)\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Motors\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        Deploy mechanism motor temperatures.\n");
  fprintf (stdout,
           "        These points are still within the thermal blankets\n");
  fprintf (stdout,
           "        in the vicinity of the HFR (i.e. on the antenna,\n");
  fprintf (stdout, "        bracket.  Normally near room temperature.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  Power Supply Monitor\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        Voltage and Current measurements from the power supply.  None of\n");
  fprintf (stdout,
           "        these channels are integrated, so sample timing can affect the\n");
  fprintf (stdout,
           "        reading (HFR and BIU exibit mode dependant current variations.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      HFR\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        voltages and current\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      LRP\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        voltages and current\n");
  fprintf (stdout,
           "        The BIU current appears as part of the LRP current.\n");
  fprintf (stdout,
           "        As there is no integrator, this is an instantaenous\n");
  fprintf (stdout,
           "        measurement.  The timing of events may be such that\n");
  fprintf (stdout, "        we do not happen to catch the BIU peak.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      MFR\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        voltages and current for MFR, WBR, and WFR (DUST and LFDR\n");
  fprintf (stdout, "        are derivative instruments)\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      LP\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        voltages and current for Langmuir Probe Analog Electronics.\n");
  fprintf (stdout, "        Digitial power is supplied through LRP/HRP\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  Command Counters\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        This status is updated by the 1st. level command decoder.\n");
  fprintf (stdout,
           "        Commands supplied by the IEB handler are injected between the\n");
  fprintf (stdout,
           "        incoming byte counter and the parity validation (valid/invalid\n");
  fprintf (stdout, "        counters).\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Cmd Count\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Count of command correctly processed by 1st level \n");
  fprintf (stdout,
           "        command decoder.  This is a 16bit field that counts\n");
  fprintf (stdout,
           "        the number of OCTETS.  Does NOT count internal commands.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Valid\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Number of valid commands processed by the 1st. level\n");
  fprintf (stdout, "        command decoder.  Includes internal commands.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Invalid\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Number of invalid commands encountered by the 1st. level\n");
  fprintf (stdout, "        command decoder.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      IEB ID\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        IEB handler read-back.  Shows the bit pattern of the currently\n");
  fprintf (stdout,
           "        executing IEB.  No indication of static/cyclic, this shows\n");
  fprintf (stdout, "        the last 73IEB_TRIGGER received.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Bad-f[n]\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        IEB Bad command count\n");
  fprintf (stdout, "        Status/Count of BAD 73IEB_LOAD commands\n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Good-f[n]\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        IEB Good command count\n");
  fprintf (stdout, "        Status/Count of good 73IEB_LOAD commands\n");
  fprintf (stdout, "        \n");
  fprintf (stdout, "        The good/bad counter consist of a status\n");
  fprintf (stdout, "        bit combined with a 7 bit counter.  These\n");
  fprintf (stdout,
           "        are broken up for display with the f indicative\n");
  fprintf (stdout,
           "        of the status flag and the n indicative of the counter.\n");
  fprintf (stdout,
           "        Bothe flags may be set, indicating a successful load.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      IEB Status\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        The status flags have 4 combinations:\n");
  if (flag)
    fprintf (stdout, "<dl><dt>");
  fprintf (stdout, "            IEB Memory Empty\n");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "                    Nothing has been loaded into IEB memory.\n");
  fprintf (stdout, "                    (IEM memory is all ZERO).\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "            IEB Load Successful\n");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "                    IEB image loaded and checksum verified.\n");
  fprintf (stdout,
           "                    (73IEB_TRIGGER commands will execute).\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "            IEB Load Failed\n");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "                    IEB image not loaded because checksum failed.\n");
  fprintf (stdout,
           "                    (73IEB_TRIGGER commands will NOT execute).\n");
  if (flag)
    fprintf (stdout, "</dd><dt>");
  fprintf (stdout, "            IEB Load Successful, following a bad load\n");
  if (flag)
    fprintf (stdout, "</dt><dd>");
  fprintf (stdout,
           "                    IEB image loaded and checksum verified.\n");
  fprintf (stdout,
           "                    (73IEB_TRIGGER commands will execute).\n");
  fprintf (stdout,
           "                    This can occur when sending multiple images\n");
  fprintf (stdout, "                    (during real-time commanding).\n");
  if (flag)
    fprintf (stdout, "</dd></dl>");
  fprintf (stdout,
           "        These 4 indications are displayed as indicated\n");
  fprintf (stdout, "        by the status field.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  LRP Digital Status Monitor\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        Several of the I/O ports are save in houskeeping and are\n");
  fprintf (stdout,
           "        presented here.  As with all housekeeping, these are simply\n");
  fprintf (stdout, "        the last available patterns.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      BIU Disc\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        Port E0 IN\n");
  fprintf (stdout,
           "        The BIU Discrete command bits presented to the LRP.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Pwr Cntl\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "      Port E8 OUT\n");
  fprintf (stdout,
           "        The HFR/Power control port.  Used to switch the 3\n");
  fprintf (stdout, "        sections of the power supply.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Status\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        Port F0 OUT\n");
  fprintf (stdout, "        Status presented to the BIU discrete bits.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      Misc\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        Port D8 IN\n");
  fprintf (stdout,
           "        Hardware generated status bit that are presented\n");
  fprintf (stdout,
           "        to the BIU (these appear as BIU discrete bits).\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  HRP Digital Status Monitor\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        The Langmuir Probe always biases the spherical probe and\n");
  fprintf (stdout,
           "        may bias the Ex antenna (also referred to as the cylindrical \n");
  fprintf (stdout,
           "        probe).  In order to determine the bias voltage on the probes\n");
  fprintf (stdout,
           "        we need to inspect a L/P density packet to determine the most\n");
  fprintf (stdout, "        recent relay configuration.\n");
  fprintf (stdout, "        \n");
  fprintf (stdout, "        \n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      P8155\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        8155 Port C\n");
  fprintf (stdout,
           "        This is the bit pattern that appears in the 8155 port C\n");
  fprintf (stdout,
           "        register.  These bits control the Langmuir Probe.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      LP MUX\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Multiplexor control register in the L/P electronics.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      DAC-0\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Bit pattern in the L/P D-to-A convertor connected to the\n");
  fprintf (stdout, "        spherical probe (i.e. the Langmuir Probe).\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      DAC-1\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Bit pattern in the L/P D-to-A convertor connected to the\n");
  fprintf (stdout, "        cylindrical probe (i.e. the Ex antenna)\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H3>");
  fprintf (stdout, "  Micro Packet Monitor\n");
  if (flag)
    fprintf (stdout, "</H3>");
  fprintf (stdout,
           "        The trailing 128 byte of the housekeeping record is not dedicated\n");
  fprintf (stdout,
           "        to any specific status fields but contains packetized housekeeping\n");
  fprintf (stdout,
           "        products.  As of V2.7 the following status products are generated.\n");
  fprintf (stdout,
           "        The micro packet area of the housekeeping page is treated as a push\n");
  fprintf (stdout,
           "        down stack with a typical length of 16 bytes.  By convention the \n");
  fprintf (stdout,
           "        micropackets are a multiple of 16 bytes in length.  The MRO packets,\n");
  fprintf (stdout,
           "        as an example, can be easily produced with a length other than 16 bytes.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      DST\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        DUST analysis status micro packet.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      BFD\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        WBR/WFR command status micro packet.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      IPC\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "        Inter Processor Communications link counters micro packet. \n");
  fprintf (stdout, "        This is an MRO packet from address 1170.\n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      MRO\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "        Memory Read Out.  10 byte memory dump packet.\n");
  fprintf (stdout, "        \n");
  fprintf (stdout, "        \n");
  if (flag)
    fprintf (stdout, "<br>");
  if (flag)
    fprintf (stdout, "<br>");
  if (flag)
    fprintf (stdout, "<H2>");
  fprintf (stdout, "  Version Notes\n");
  if (flag)
    fprintf (stdout, "</H2>");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      (dsp9a) 1.7\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "            L/P voltage limits changed slightly\n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      (dsp9a) 1.5\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout,
           "            Reduced the overhead of fetching the latest\n");
  fprintf (stdout, "          datafile from the database file.  This only\n");
  fprintf (stdout,
           "          affects access to the database file.  For high\n");
  fprintf (stdout,
           "          rate periods, accessing the data file to find the\n");
  fprintf (stdout,
           "          time of the latest data can be time consuming.\n");
  if (flag)
    fprintf (stdout, "<p>");
  fprintf (stdout,
           "            Unfortunatly, we can't simply look at the last\n");
  fprintf (stdout,
           "          record in the data file (although it is trivial to\n");
  fprintf (stdout,
           "          scan backwards through the file), as it typically is\n");
  fprintf (stdout,
           "          not in time order due to playback data from the SSR.\n");
  fprintf (stdout, "\n");
  if (flag)
    fprintf (stdout, "<H4>");
  fprintf (stdout, "      (dsp9a) 1.4\n");
  if (flag)
    fprintf (stdout, "</H4>");
  fprintf (stdout, "          Added access to \n");
  fprintf (stdout,
           "            $RPWS_DATA/temp/recent_hsk.dat\n");
  fprintf (stdout,
           "          to allow visibility into data being written by\n");
  fprintf (stdout, "          a fast tape archive program.\n");
  fprintf (stdout, "\n");
  if (flag)
    fprintf (stdout, "<br>");
  if (flag)
    fprintf (stdout, "<br>");
  fprintf (stdout, "\n");
  return 0;
}
