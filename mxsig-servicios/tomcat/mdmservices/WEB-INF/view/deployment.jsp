<%-- 
    Document   : memory_usage
    Created on : 29/08/2011, 04:16:01 PM
    Author     : miguel.figueroa
--%>

<%@page contentType="text/html" pageEncoding="UTF-8"%>
<%@ page import="java.util.jar.Manifest"%>
<%@ page import="java.util.jar.Attributes"%>
<%@ page import="java.io.InputStream"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Deployment version</title>
</head>
<body bgcolor="#0CA35F">
	<%
		ServletContext app = getServletConfig().getServletContext();
		InputStream inputStream = application.getResourceAsStream("/META-INF/MANIFEST.MF");
		Manifest manifest = new Manifest(inputStream);
		Attributes attributes = manifest.getMainAttributes();
		String buildNumber = attributes.getValue("Build-Number");
		String builTimestamp = attributes.getValue("Build-Timestamp");
		String buildUrl =attributes.getValue("Build-Url");
		out.println("<h3>Deployment</h3>");
		out.println("<br>Version: " + buildNumber);
		out.println("<br>Date: " + builTimestamp);
		out.println("<br>Url: " + "<a href=" + buildUrl +">"+ buildUrl + "</a>");
	%>
</body>
</html>
