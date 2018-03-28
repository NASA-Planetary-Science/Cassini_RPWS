#!/usr/bin/env python

from distutils.core import setup

setup(
   name="rpw_pds",
	version="0.4",
	description="PDS volume generation utilities",
	author="Chris Piker",
	packages=['rpw_pds'],
	scripts=['scripts/vol_check']
)
	
