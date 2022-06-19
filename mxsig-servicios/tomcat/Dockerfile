FROM mxsig/mxsig_docker_tomcat:v4

LABEL MxSIG INEGI

RUN cd /opt/tomcat/webapps/ && \
rm -rf mdmservices.war

RUN cd /opt/tomcat/webapps/ && \
rm -rf mdmservices

COPY mdmservices.war /opt/tomcat/webapps

COPY tomcat-users.xml /opt/tomcat/conf/

COPY web.xml /opt/tomcat/conf/

COPY setenv.sh /opt/tomcat/bin/

COPY geowebcache.xml /usr/local/data/geowebcache/

COPY solr-config /usr/local/instalacion/tomcat-solr/solr-config/

RUN useradd -m tomcat

RUN chown -R tomcat /opt/tomcat

RUN chown -R tomcat /usr/local

USER tomcat

EXPOSE 8080