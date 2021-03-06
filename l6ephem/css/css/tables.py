"""Static tables for various periods in the Cassini Mission"""

# $ID$
# 2017-02-09 cssOrbit updated, cssTitan updated, cssEnceladus updated  LJG

MissionPhaseName=(
  ("LAUNCH",                        "1997-288",  "1997-290"),
  ("TCM 1",                         "1997-291",  "1997-318"),
  ("INTERPLANETARY CRUISE",         "1997-318",  "1999-311"),
  ("VENUS 1 CRUISE",                "1997-318",  "1998-256"),
  ("VENUS 1 ENCOUNTER",             "1998-116",  "1998-116"),
  ("INSTRUMENT CHECKOUT 1",         "1998-257",  "1999-073"),
  ("VENUS 2 - EARTH CRUISE",        "1999-074",  "1999-311"),
  ("VENUS 2 ENCOUNTER",             "1999-175",  "1999-175"),
  ("EARTH ENCOUNTER",               "1999-230",  "1999-230"),
  ("OUTER CRUISE",                  "1999-312",  "2002-188"),
  ("HIGH GAIN ANTENNA TRANSITION",  "1999-312",  "2000-127"),
  ("INSTRUMENT CHECKOUT 2",         "2000-127",  "2000-310"),
  ("JUPITER CRUISE",                "2000-310",  "2001-120"),
  ("JUPITER ENCOUNTER",             "2000-365",  "2000-365"),
  ("QUIET CRUISE",                  "2001-120",  "2002-189"),
  ("SCIENCE CRUISE",                "2002-189",  "2004-162"),
  ("SPACE SCIENCE",                 "2002-189",  "2004-011"),
  ("APPROACH SCIENCE",              "2004-012",  "2004-162"),
  ("TOUR PRE-HUYGENS",              "2004-163",  "2004-359"),
  ("PHOEBE ENCOUNTER",              "2004-163",  "2004-163"),
  ("SATURN ORBIT INSERTION",        "2004-183",  "2004-183"),
  ("TITAN A ENCOUNTER",             "2004-300",  "2004-300"),
  ("TITAN B ENCOUNTER",             "2004-348",  "2004-348"),
  ("HUYGENS DESCENT",               "2004-359",  "2005-014"),
  ("HUYGENS PROBE SEPARATION",      "2004-359",  "2004-359"),
  ("TITAN C HUYGENS",               "2005-014",  "2005-014"),
  ("TOUR",                          "2005-014",  "2008-182"),
  ("EXTENDED MISSION",              "2008-183",  "2010-272"),
  ("EXTENDED-EXTENDED MISSION",     "2010-273",  "2017-258"),
  ("END OF MISSION",                "2017-258",  "2027-001"),
  ("","",""))

# tuple of Cassini orbit number and begin SCET
cssOrbit=(
  ("A", "2004-240T08:56"),
  ("B", "2004-326T08:41"),
  ("C", "2004-366T07:02"),
  ("3", "2005-032T03:25"),
  ("4", "2005-058T06:19"),
  ("5", "2005-078T17:34"),
  ("6", "2005-096T23:32"),
  ("7", "2005-113T23:30"),
  ("8", "2005-132T03:52"),
  ("9", "2005-150T08:21"),
  ("10", "2005-168T13:11"),
  ("11", "2005-186T18:57"),
  ("12", "2005-205T01:57"),
  ("13", "2005-223T08:29"),
  ("14", "2005-240T12:01"),
  ("15", "2005-257T16:49"),
  ("16", "2005-275T23:33"),
  ("17", "2005-293T23:59"),
  ("18", "2005-317T04:57"),
  ("19", "2005-345T04:26"),
  ("20", "2006-005T14:08"),
  ("21", "2006-036T20:57"),
  ("22", "2006-068T03:34"),
  ("23", "2006-099T10:05"),
  ("24", "2006-130T16:30"),
  ("25", "2006-161T23:07"),
  ("26", "2006-193T05:18"),
  ("27", "2006-216T21:21"),
  ("28", "2006-240T19:15"),
  ("29", "2006-260T17:10"),
  ("30", "2006-276T19:03"),
  ("31", "2006-292T22:17"),
  ("32", "2006-307T00:04"),
  ("33", "2006-318T23:30"),
  ("34", "2006-330T22:19"),
  ("35", "2006-342T21:01"),
  ("36", "2006-356T23:09"),
  ("37", "2007-008T04:38"),
  ("38", "2007-024T12:19"),
  ("39", "2007-041T10:53"),
  ("40", "2007-058T07:46"),
  ("41", "2007-074T12:36"),
  ("42", "2007-090T17:01"),
  ("43", "2007-106T20:40"),
  ("44", "2007-122T23:23"),
  ("45", "2007-139T01:03"),
  ("46", "2007-155T01:16"),
  ("47", "2007-171T01:09"),
  ("48", "2007-190T10:07"),
  ("49", "2007-221T16:46"),
  ("50", "2007-257T11:37"),
  ("51", "2007-285T09:51"),
  ("52", "2007-309T07:53"),
  ("53", "2007-329T07:54"),
  ("54", "2007-345T05:33"),
  ("55", "2007-361T01:28"),
  ("56", "2008-009T22:55"),
  ("57", "2008-021T21:42"),
  ("58", "2008-033T20:08"),
  ("59", "2008-045T19:23"),
  ("60", "2008-056T22:03"),
  ("61", "2008-067T13:00"),
  ("62", "2008-078T04:33"),
  ("63", "2008-088T01:22"),
  ("64", "2008-097T15:14"),
  ("65", "2008-107T04:56"),
  ("66", "2008-116T18:16"),
  ("67", "2008-126T07:31"),
  ("68", "2008-135T00:19"),
  ("69", "2008-142T23:31"),
  ("70", "2008-150T09:16"),
  ("71", "2008-157T12:30"),
  ("72", "2008-164T15:44"),
  ("73", "2008-171T18:47"),
  ("74", "2008-178T20:44"),
  ("75", "2008-185T21:33"),
  ("76", "2008-192T22:21"),
  ("77", "2008-199T23:12"),
  ("78", "2008-207T00:08"),
  ("79", "2008-213T19:51"),
  ("80", "2008-221T05:08"),
  ("81", "2008-228T14:08"),
  ("82", "2008-235T22:42"),
  ("83", "2008-243T07:40"),
  ("84", "2008-250T16:32"),
  ("85", "2008-258T01:23"),
  ("86", "2008-265T10:12"),
  ("87", "2008-272T19:02"),
  ("88", "2008-280T03:34"),
  ("89", "2008-287T11:41"),
  ("90", "2008-294T19:07"),
  ("91", "2008-302T02:15"),
  ("92", "2008-309T23:28"),
  ("93", "2008-317T22:29"),
  ("94", "2008-325T10:16"),
  ("95", "2008-333T09:31"),
  ("96", "2008-340T18:22"),
  ("97", "2008-348T17:47"),
  ("98", "2008-356T13:04"),
  ("99", "2008-366T00:09"),
  ("100", "2009-009T13:56"),
  ("101", "2009-019T03:41"),
  ("102", "2009-028T17:05"),
  ("103", "2009-039T02:28"),
  ("104", "2009-051T01:00"),
  ("105", "2009-062T23:45"),
  ("106", "2009-074T22:49"),
  ("107", "2009-086T04:51"),
  ("108", "2009-094T01:29"),
  ("109", "2009-106T23:06"),
  ("110", "2009-122T01:38"),
  ("111", "2009-137T13:47"),
  ("112", "2009-153T00:47"),
  ("113", "2009-168T14:09"),
  ("114", "2009-184T05:05"),
  ("115", "2009-199T21:29"),
  ("116", "2009-215T15:23"),
  ("117", "2009-231T11:08"),
  ("118", "2009-251T10:11"),
  ("119", "2009-275T08:02"),
  ("120", "2009-296T16:58"),
  ("121", "2009-315T16:57"),
  ("122", "2009-334T17:36"),
  ("123", "2009-352T05:55"),
  ("124", "2010-003T03:49"),
  ("125", "2010-019T04:18"),
  ("126", "2010-035T22:18"),
  ("127", "2010-053T11:27"),
  ("128", "2010-071T01:40"),
  ("129", "2010-088T17:01"),
  ("130", "2010-107T17:20"),
  ("131", "2010-128T03:04"),
  ("132", "2010-146T08:04"),
  ("133", "2010-162T07:28"),
  ("134", "2010-178T05:26"),
  ("135", "2010-196T04:18"),
  ("136", "2010-216T02:12"),
  ("137", "2010-236T01:24"),
  ("138", "2010-256T00:46"),
  ("139", "2010-277T20:48"),
  ("140", "2010-301T18:06"),
  ("141", "2010-324T01:57"),
  ("142", "2010-344T16:15"),
  ("143", "2010-365T07:30"),
  ("144", "2011-020T21:28"),
  ("145", "2011-041T08:06"),
  ("146", "2011-065T12:39"),
  ("147", "2011-093T10:51"),
  ("148", "2011-119T03:06"),
  ("149", "2011-150T10:15"),
  ("150", "2011-180T19:55"),
  ("151", "2011-202T12:10"),
  ("152", "2011-224T06:01"),
  ("153", "2011-246T01:47"),
  ("154", "2011-265T19:58"),
  ("155", "2011-283T14:40"),
  ("156", "2011-301T10:10"),
  ("157", "2011-319T06:12"),
  ("158", "2011-337T03:13"),
  ("159", "2011-357T19:13"),
  ("160", "2012-016T15:46"),
  ("161", "2012-040T12:16"),
  ("162", "2012-061T03:51"),
  ("163", "2012-078T23:54"),
  ("164", "2012-096T19:17"),
  ("165", "2012-114T14:46"),
  ("166", "2012-132T09:28"),
  ("167", "2012-149T05:15"),
  ("168", "2012-169T01:46"),
  ("169", "2012-192T23:45"),
  ("170", "2012-215T10:51"),
  ("171", "2012-236T16:36"),
  ("172", "2012-257T23:16"),
  ("173", "2012-280T10:18"),
  ("174", "2012-304T08:16"),
  ("175", "2012-324T02:54"),
  ("176", "2012-338T10:58"),
  ("177", "2012-351T17:22"),
  ("178", "2012-364T23:50"),
  ("179", "2013-012T06:43"),
  ("180", "2013-025T13:55"),
  ("181", "2013-038T21:11"),
  ("182", "2013-051T05:01"),
  ("183", "2013-063T04:01"),
  ("184", "2013-075T03:13"),
  ("185", "2013-087T02:16"),
  ("186", "2013-097T17:49"),
  ("187", "2013-107T07:26"),
  ("188", "2013-116T21:08"),
  ("189", "2013-126T10:48"),
  ("190", "2013-136T00:30"),
  ("191", "2013-146T07:38"),
  ("192", "2013-158T06:29"),
  ("193", "2013-170T05:34"),
  ("194", "2013-182T04:39"),
  ("195", "2013-196T06:15"),
  ("196", "2013-217T16:37"),
  ("197", "2013-241T14:47"),
  ("198", "2013-270T15:02"),
  ("199", "2013-311T06:30"),
  ("200", "2013-351T21:23"),
  ("201", "2014-019T10:10"),
  ("202", "2014-051T16:45"),
  ("203", "2014-083T18:01"),
  ("204", "2014-117T13:47"),
  ("205", "2014-151T08:36"),
  ("206", "2014-183T06:52"),
  ("207", "2014-215T06:57"),
  ("208", "2014-247T01:17"),
  ("209", "2014-278T22:42"),
  ("210", "2014-318T23:04"),
  ("211", "2014-358T19:16"),
  ("212", "2015-025T18:42"),
  ("213", "2015-057T17:14"),
  ("214", "2015-087T16:40"),
  ("215", "2015-115T17:46"),
  ("216", "2015-139T04:31"),
  ("217", "2015-158T01:49"),
  ("218", "2015-176T23:56"),
  ("219", "2015-197T08:19"),
  ("220", "2015-219T03:41"),
  ("221", "2015-240T23:28"),
  ("222", "2015-262T19:28"),
  ("223", "2015-280T15:12"),
  ("224", "2015-294T13:00"),
  ("225", "2015-308T10:42"),
  ("226", "2015-321T18:22"),
  ("227", "2015-334T11:52"),
  ("228", "2015-347T05:22"),
  ("229", "2015-360T00:24"),
  ("230", "2016-007T20:47"),
  ("231", "2016-022T05:32"),
  ("232", "2016-038T03:08"),
  ("233", "2016-057T23:48"),
  ("234", "2016-081T21:40"),
  ("235", "2016-109T18:01"),
  ("236", "2016-141T13:11"),
  ("237", "2016-169T05:46"),
  ("238", "2016-193T03:50"),
  ("239", "2016-212T19:12"),
  ("240", "2016-226T14:22"),
  ("241", "2016-238T13:11"),
  ("242", "2016-250T12:11"),
  ("243", "2016-262T11:22"),
  ("244", "2016-273T05:34"),
  ("245", "2016-282T19:09"),
  ("246", "2016-292T08:50"),
  ("247", "2016-301T22:25"),
  ("248", "2016-311T11:57"),
  ("249", "2016-320T09:06"),
  ("250", "2016-328T08:16"),
  ("251", "2016-335T23:31"),
  ("252", "2016-343T03:39"),
  ("253", "2016-350T07:51"),
  ("254", "2016-357T11:51"),
  ("255", "2016-364T15:50"),
  ("256", "2017-005T19:44"),
  ("257", "2017-012T23:37"),
  ("258", "2017-020T03:29"),
  ("259", "2017-027T07:24"),
  ("260", "2017-034T11:33"),
  ("261", "2017-041T16:02"),
  ("262", "2017-048T20:24"),
  ("263", "2017-056T00:27"),
  ("264", "2017-063T04:32"),
  ("265", "2017-070T08:28"),
  ("266", "2017-077T12:22"),
  ("267", "2017-084T16:13"),
  ("268", "2017-091T20:05"),
  ("269", "2017-099T00:00"),
  ("270", "2017-106T04:02"),
  ("271", "2017-113T03:46"),
  ("272", "2017-119T14:22"),
  ("273", "2017-126T00:59"),
  ("274", "2017-132T11:30"),
  ("275", "2017-138T21:59"),
  ("276", "2017-145T08:50"),
  ("277", "2017-151T20:04"),
  ("278", "2017-158T07:18"),
  ("279", "2017-164T18:24"),
  ("280", "2017-171T05:26"),
  ("281", "2017-177T16:35"),
  ("282", "2017-184T03:54"),
  ("283", "2017-190T15:13"),
  ("284", "2017-197T02:21"),
  ("285", "2017-203T13:26"),
  ("286", "2017-210T00:33"),
  ("287", "2017-216T11:46"),
  ("288", "2017-222T22:56"),
  ("289", "2017-229T09:52"),
  ("290", "2017-235T20:50"),
  ("291", "2017-242T07:45"),
  ("292", "2017-248T18:41"),
  ("293", "2017-255T05:27"),
  ("end", "2017-260T00:00"))

# tuple of Titan encounter ID and closest approach SCET
cssTitan=(
  ("TA", "2004-300T15:30"),
  ("TB", "2004-348T11:38"),
  ("TC", "2005-014T11:11"),
  ("T3", "2005-046T06:57"),
  ("T4", "2005-090T20:05"),
  ("T5", "2005-106T19:11"),
  ("T6", "2005-234T08:53"),
  ("T7", "2005-250T08:11"),
  ("T8", "2005-301T04:15"),
  ("T9", "2005-360T18:59"),
  ("T10", "2006-015T11:41"),
  ("T11", "2006-058T08:25"),
  ("T12", "2006-078T00:05"),
  ("T13", "2006-120T20:58"),
  ("T14", "2006-140T12:18"),
  ("T15", "2006-183T09:20"),
  ("T16", "2006-203T00:25"),
  ("T17", "2006-250T20:16"),
  ("T18", "2006-266T18:58"),
  ("T19", "2006-282T17:30"),
  ("T20", "2006-298T15:58"),
  ("T21", "2006-346T11:41"),
  ("T22", "2006-362T10:05"),
  ("T23", "2007-013T08:38"),
  ("T24", "2007-029T07:15"),
  ("T25", "2007-053T03:12"),
  ("T26", "2007-069T01:49"),
  ("T27", "2007-085T00:23"),
  ("T28", "2007-100T22:58"),
  ("T29", "2007-116T21:32"),
  ("T30", "2007-132T20:09"),
  ("T31", "2007-148T18:51"),
  ("T32", "2007-164T17:46"),
  ("T33", "2007-180T16:59"),
  ("T34", "2007-200T01:11"),
  ("T35", "2007-243T06:32"),
  ("T36", "2007-275T04:42"),
  ("T37", "2007-323T00:47"),
  ("T38", "2007-339T00:06"),
  ("T39", "2007-354T22:57"),
  ("T40", "2008-005T21:30"),
  ("T41", "2008-053T17:32"),
  ("T42", "2008-085T14:27"),
  ("T43", "2008-133T10:01"),
  ("T44", "2008-149T08:24"),
  ("T45", "2008-213T02:13"),
  ("T46", "2008-308T17:35"),
  ("T47", "2008-324T15:56"),
  ("T48", "2008-340T14:25"),
  ("T49", "2008-356T12:59"),
  ("T50", "2009-038T08:50"),
  ("T51", "2009-086T04:43"),
  ("T52", "2009-094T01:47"),
  ("T53", "2009-110T00:20"),
  ("T54", "2009-125T22:54"),
  ("T55", "2009-141T21:26"),
  ("T56", "2009-157T20:00"),
  ("T57", "2009-173T18:32"),
  ("T58", "2009-189T17:04"),
  ("T59", "2009-205T15:34"),
  ("T60", "2009-221T14:03"),
  ("T61", "2009-237T12:51"),
  ("T62", "2009-285T08:36"),
  ("T63", "2009-346T01:03"),
  ("T64", "2009-362T00:16"),
  ("T65", "2010-012T23:10"),
  ("T66", "2010-028T22:28"),
  ("T67", "2010-095T15:50"),
  ("T68", "2010-140T03:24"),
  ("T69", "2010-156T02:26"),
  ("T70", "2010-172T01:27"),
  ("T71", "2010-188T00:22"),
  ("T72", "2010-267T18:38"),
  ("T73", "2010-315T13:37"),
  ("T74", "2011-049T16:04"),
  ("T75", "2011-109T05:00"),
  ("T76", "2011-128T22:53"),
  ("T77", "2011-171T18:32"),
  ("T78", "2011-255T02:50"),
  ("T79", "2011-347T20:11"),
  ("T80", "2012-002T15:13"),
  ("T81", "2012-030T13:39"),
  ("T82", "2012-050T08:43"),
  ("T83", "2012-143T01:10"),
  ("T84", "2012-159T00:07"),
  ("T85", "2012-206T20:03"),
  ("T86", "2012-270T14:35"),
  ("T87", "2012-318T10:22"),
  ("T88", "2012-334T08:56"),
  ("T89", "2013-048T01:56"),
  ("T90", "2013-095T21:43"),
  ("T91", "2013-143T17:32"),
  ("T92", "2013-191T13:21"),
  ("T93", "2013-207T11:56"),
  ("T94", "2013-255T07:43"),
  ("T95", "2013-287T04:56"),
  ("T96", "2013-335T00:41"),
  ("T97", "2014-001T21:59"),
  ("T98", "2014-033T19:12"),
  ("T99", "2014-065T16:26"),
  ("T100", "2014-097T13:41"),
  ("T101", "2014-137T16:12"),
  ("T102", "2014-169T13:28"),
  ("T103", "2014-201T10:40"),
  ("T104", "2014-233T08:09"),
  ("T105", "2014-265T05:23"),
  ("T106", "2014-297T02:40"),
  ("T107", "2014-344T22:26"),
  ("T108", "2015-011T19:48"),
  ("T109", "2015-043T17:08"),
  ("T110", "2015-075T14:29"),
  ("T111", "2015-127T22:50"),
  ("T112", "2015-188T08:09"),
  ("T113", "2015-271T21:37"),
  ("T114", "2015-317T05:46"),
  ("T115", "2016-016T02:20"),
  ("T116", "2016-032T01:00"),
  ("T117", "2016-047T23:49"),
  ("T118", "2016-095T19:42"),
  ("T119", "2016-127T16:54"),
  ("T120", "2016-159T14:06"),
  ("T121", "2016-207T09:58"),
  ("T122", "2016-223T08:30"),
  ("T123", "2016-271T04:16"),
  ("T124", "2016-318T23:55"),
  ("T125", "2016-334T22:14"),
  ("T126", "2017-112T06:08"))

# tuple of Enceladus encounter ID and closest approach SCET
cssEnceladus=(
  ("E0", "2005-048T03:30"),
  ("E1", "2005-068T09:08"),
  ("E2", "2005-195T19:55"),
  ("E3", "2008-072T19:06"),
  ("E4", "2008-224T21:06"),
  ("E5", "2008-283T19:06"),
  ("E6", "2008-305T17:14"),
  ("E7", "2009-306T07:41"),
  ("E8", "2009-325T02:09"),
  ("E9", "2010-118T00:10"),
  ("E10", "2010-138T06:04"),
  ("E11", "2010-225T22:30"),
  ("E12", "2010-334T11:53"),
  ("E13", "2010-355T01:08"),
  ("E14", "2011-274T13:52"),
  ("E15", "2011-292T09:22"),
  ("E16", "2011-310T04:58"),
  ("E17", "2012-087T18:30"),
  ("E18", "2012-105T14:01"),
  ("E19", "2012-123T09:31"),
  ("E20", "2015-287T10:41"),
  ("E21", "2015-301T15:22"),
  ("E22", "2015-353T17:49"))
