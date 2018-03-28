PDS_VERSION_ID          = PDS3
/************************/
/* File Characteristics */
/************************/
RECORD_TYPE             = FIXED_LENGTH
RECORD_BYTES            = 272
FILE_RECORDS            = %(index-rows)s
/************************/
/* Data object pointers */
/************************/
^INDEX_TABLE            = ("INDEX.TAB",2)
/******************/
/* Identification */
/******************/
VOLUME_ID               = %(vol-id)s
DATA_SET_ID             = %(datasets)s
PRODUCT_CREATION_TIME   = %(pub-date)s
START_TIME              = %(start-time)s
STOP_TIME               = %(stop-time)s
MISSION_NAME            = "CASSINI-HUYGENS"
SPACECRAFT_NAME         = "CASSINI ORBITER"
TARGET_NAME             = %(targets)s
MISSION_PHASE_NAME      = %(phases)s
DESCRIPTION             = "INDEX.TAB is an index of all of the PDS
  label files corresponding to all of the archived Cassini RPWS
  data on this volume.  The first line of the index file contains
  individual OBJECT names from the OBJECT=INDEX_TABLE that follows.
  Some of these names may be truncated to fit the defined column
  width."
SOFTWARE_VERSION_ID     = "P3.9a/L2.8a"
/*************************/
/* Data Object Structure */
/*************************/
OBJECT                  = INDEX_TABLE
  INTERCHANGE_FORMAT      = ASCII
  ROWS                    = %(num-labels)s
  ROW_BYTES               = 272
  COLUMNS                 = 9
  INDEX_TYPE              = SINGLE
  DESCRIPTION             = "The following fields are
    extracted from the individual label files on this
    and previous volumes."

  OBJECT                  = COLUMN
    NAME                    = "VOLUME_ID"
    DATA_TYPE               = CHARACTER
    START_BYTE              = 2
    BYTES                   = 11
    DESCRIPTION             = "Volume ID in the form CORPWS_0nnn.
      In the CUMINDEX.TAB this identifies the volume on which
      the indicated dataset resides."
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "STANDARD_DATA_PRODUCT_ID"
    DATA_TYPE               = CHARACTER
    START_BYTE              = 16
    BYTES                   = 20
    DESCRIPTION             = "The general data product name"
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "DATA_SET_ID"
    DATA_TYPE               = CHARACTER
    START_BYTE              = 39
    BYTES                   = 40
    DESCRIPTION             = "The data set ID from the label"
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "PRODUCT_ID"
    DATA_TYPE               = CHARACTER
    START_BYTE              = 82
    BYTES                   = 30
    DESCRIPTION             = "The data product ID"
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "START_TIME"
    DATA_TYPE               = TIME
    START_BYTE              = 115
    BYTES                   = 22
    DESCRIPTION             = "Spacecraft Event Time (SCET) of the
      begining of the period in the form yyyy-dddThh:mm:ss.sssZ"
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "STOP_TIME"
    DATA_TYPE               = TIME
    START_BYTE              = 140
    BYTES                   = 22
    DESCRIPTION             = "Spacecraft Event Time (SCET) of the
      end of the period in the form yyyy-dddThh:mm:ss.sssZ"
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "SPACECRAFT_CLOCK_START_COUNT"
    DATA_TYPE               = CHARACTER
    START_BYTE              = 165
    BYTES                   = 16
    DESCRIPTION             = "Spacecraft Clock (SCLK) of the
      begining of the period in the form p/ssssssssss:fff
        where:
          p indicates the partition number;
          ssssssssss is the SCLK second counter;
          fff is the SCLK fine counter (256 counts per second)"
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "FILE_SPECIFICATION_NAME"
    DATA_TYPE               = CHARACTER
    START_BYTE              = 184
    BYTES                   = 73
    DESCRIPTION             = "POSIX-compliant full path to the PDS label
      file that describes the binary file containing instrument data.
      The path is relative to the root of the archive volume."
  END_OBJECT              = COLUMN

  OBJECT                  = COLUMN
    NAME                    = "PRODUCT_CREATION_TIME"
    DATA_TYPE               = TIME
    START_BYTE              = 260
    BYTES                   = 10
    DESCRIPTION             = "Product creation date"
  END_OBJECT              = COLUMN

END_OBJECT              = INDEX_TABLE

END
