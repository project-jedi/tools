<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright © 2006 by Christian Rodemeyer (mailto:christian@atombrenner.de) -->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:xdt="http://www.w3.org/2005/02/xpath-datatypes">
	<xsl:output method="html" version="1.0" encoding="UTF-8" indent="no"/>
	<xsl:template match="/">
		<xsl:apply-templates select="//buildresults" />
	</xsl:template>

	<xsl:template match="buildresults">
		<table class="section-table" cellpadding="2" cellspacing="0" border="0" width="98%">
		<tr><td class="sectionheader">
			<xsl:apply-templates select="task/description" mode="header" />
      <xsl:if test="not(task/description)">Execution result</xsl:if>
		</td></tr>
			<xsl:apply-templates select="message" />
		</table>
	</xsl:template>

	<xsl:template match="task/description" mode="header" >
	  <xsl:value-of select="."/> 
	</xsl:template>
	
	<xsl:template match="message" >
		<tr>
			<td>
				<xsl:value-of select="."/>
			</td>
		</tr>
	</xsl:template>
	
</xsl:stylesheet>
