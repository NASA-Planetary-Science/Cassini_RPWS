##############################################################################
# Make sure they are using gmake on Solaris

ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif


include Posix.mak

