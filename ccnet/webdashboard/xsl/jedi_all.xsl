<?xml version="1.0"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

    <xsl:include href="jedi.xsl" />

  	<xsl:template match="/">
  		<xsl:apply-templates select="//JclInstall" mode="summary"/>
  		<xsl:apply-templates select="//JvclInstall" mode="summary"/>
  		<xsl:apply-templates select="//JclInstall" mode="log"/>
  		<xsl:apply-templates select="//JvclInstall" mode="log"/>
  	</xsl:template>
  
    <xsl:template match="JclInstall" mode="summary">
      <xsl:call-template name="JediInstall">
        <xsl:with-param name="InstallerName">JCL</xsl:with-param>
      </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="JclInstall" mode="log">
      <xsl:call-template name="JediInstallLog">
        <xsl:with-param name="InstallerName">JCL</xsl:with-param>
      </xsl:call-template>
    </xsl:template>
  
    <xsl:template match="JvclInstall" mode="summary">
      <xsl:call-template name="JediInstall">
        <xsl:with-param name="InstallerName">JVCL</xsl:with-param>
      </xsl:call-template>
    </xsl:template>
    
    <xsl:template match="JvclInstall" mode="log">
      <xsl:call-template name="JediInstallLog">
        <xsl:with-param name="InstallerName">JVCL</xsl:with-param>
      </xsl:call-template>
    </xsl:template>
</xsl:stylesheet>