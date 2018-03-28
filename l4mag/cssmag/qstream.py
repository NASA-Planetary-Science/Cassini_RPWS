def header(fOut):
	sHeader='<stream dataset_id="parallel"/>\n'
	fOut.write('[00]%06d%s'%(len(sHeader),sHeader))

def frequencyDescriptor(fOut, iBandCount):
	sPacketDescriptor = '''
<packet>
	<qdataset id="frequency_0" rank="1">
		<properties>
			<property name="UNITS" type="units" value="hertz"/>
		</properties>
		<values encoding="ascii11" length="{0}"/>
	</qdataset>
</packet>
'''.format(iBandCount)
	fOut.write('[01]%06d%s'%(len(sPacketDescriptor),sPacketDescriptor))

def packetDescriptor(fOut, iBandCount, ascii=True):
	if not ascii:
		raise ValueError('I only support ascii right now')
	sPacketDescriptor = '''
<packet>
	<qdataset id="time_0" rank="1">
		<properties>
			<property name="UNITS" type="units" value="t2000"/>
		</properties>
		<values encoding="time24" length=""/>
	</qdataset>
	<qdataset id="parallel" rank="2">
		<properties>
			<property name="DEPEND_0" type="qdataset" value="time_0"/>
			<property name="DEPEND_1" type="qdataset" value="frequency_0"/>
			<property name="QUBE" type="Boolean" value="true"/>
		</properties>
		<values encoding="ascii11" length="{0}"
	</qdatset>
	<qdataset id="perpendicular" rank="2">
		<properties>
			<property name="DEPEND_0" type="qdataset" value="time_0"/>
			<property name="DEPEND_1" type="qdataset" value="frequency_0"/>
			<property name="QUBE" type="Boolean" value="true"/>
		</properties>
		<values encoding="ascii11" length="{0}"
	</qdatset>
</packet>
'''.format(iBandCount)
	fOut.write('[02]%06d%s'%(len(sPacketDescriptor),sPacketDescriptor))


