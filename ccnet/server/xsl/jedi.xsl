<?xml version="1.0"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    
  	<xsl:template match="/">
  	</xsl:template>
    
    <xsl:output method="html"/>

    <xsl:template name="JediInstall">
      <xsl:param name="InstallerName" />
      <table class="section-table" cellpadding="2" cellspacing="0" border="0" width="98%">
    		<tr><td class="sectionheader">
          <xsl:value-of select="$InstallerName" />
          <xsl:text> Installer results</xsl:text>
    		</td></tr>
        <tr>
          <td>
            <table cellpadding="5" cellspacing="0" border="0" >
              <tr style="background-color: black; color: white;">
                <td>Target</td>
                <td>Target name</td>
                <td style="text-align: center;">Enabled</td>
                <td style="text-align: center;">Install attempted</td>
                <td style="text-align: center;">Success</td>
              </tr>
              <xsl:apply-templates select="Installation" mode="summary" />
            </table>
          </td>
        </tr>
      </table>
    </xsl:template>

    <xsl:template match="Installation" mode="summary">
      <tr>
        <td><xsl:value-of select="@Target" /></td>
        <td><xsl:value-of select="@TargetName" /></td>
        <td style="text-align: center;"><xsl:choose><xsl:when test="@Enabled != 0">Yes</xsl:when><xsl:otherwise>No</xsl:otherwise></xsl:choose></td>
        <td style="text-align: center;"><xsl:choose><xsl:when test="@InstallAttempted != 0">Yes</xsl:when><xsl:otherwise>No</xsl:otherwise></xsl:choose></td>
        <td style="text-align: center;"><xsl:choose><xsl:when test="@InstallSuccess != 0">Yes</xsl:when><xsl:otherwise>No</xsl:otherwise></xsl:choose></td>
      </tr>
    </xsl:template>

    <xsl:template name="JediInstallLog">
      <xsl:param name="InstallerName" />
      <table class="section-table" cellpadding="2" cellspacing="0" border="0" width="98%">
    		<tr><td class="sectionheader">
          <xsl:value-of select="$InstallerName" />
          <xsl:text> Installer log files</xsl:text>
    		</td></tr>
        <tr>
          <td>
            <table cellpadding="5" cellspacing="0" border="0" >
              <tr style="background-color: black; color: white;">
                <td>Target</td>
                <td>Target name</td>
                <td>Log content</td>
              </tr>
              <xsl:apply-templates select="Installation" mode="log" />
            </table>
          </td>
        </tr>
      </table>
    </xsl:template>
    
    <xsl:template match="Installation" mode="log">
      <tr>
        <td style="vertical-align: top;"><xsl:value-of select="@Target" /></td>
        <td style="vertical-align: top;"><xsl:value-of select="@TargetName" /></td>
        <td><xsl:apply-templates select="LogFile" /></td>
      </tr>
    </xsl:template>
    
    <xsl:template match="LogFile">
      <pre><xsl:value-of select="." /></pre>
    </xsl:template>
</xsl:stylesheet>