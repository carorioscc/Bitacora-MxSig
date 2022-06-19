FROM postgres:11.8

ENV POSTGIS_MAJOR 2.5

# Set the locale
RUN sed -i -e 's/# es_MX.UTF-8 UTF-8/es_MX.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG es_MX.UTF-8
ENV LANGUAGE es_MX:es
ENV LC_ALL es_MX.UTF-8

ADD ./postgresql.conf /var/lib/pgsql/data/

RUN chown -v postgres:postgres /var/lib/pgsql/data/postgresql.conf

RUN apt-get update \
      && apt-get install -y postgresql-$PG_MAJOR-plr \
      && apt-cache showpkg postgresql-$PG_MAJOR-postgis-$POSTGIS_MAJOR \
      && apt-get install -y --no-install-recommends \
           postgresql-$PG_MAJOR-postgis-$POSTGIS_MAJOR \
           postgresql-$PG_MAJOR-postgis-$POSTGIS_MAJOR-scripts \
      && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('stratification', repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('RJSONIO', repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('bitops', repos='http://cran.rstudio.com/')" \    
    && R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/caTools/caTools_1.17.1.4.tar.gz', repos=NULL, type='source')" \
    && R -e "install.packages('gplots', repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('BAMMtools', repos='http://cran.rstudio.com/')"

RUN mkdir -p /docker-entrypoint-initdb.d
COPY ./R_functions.sql /docker-entrypoint-initdb.d/
COPY ./geomcoords.sql /docker-entrypoint-initdb.d/
COPY ./getlayertemamat_estatal.sql /docker-entrypoint-initdb.d/
COPY ./getlayertemamat_municipal.sql /docker-entrypoint-initdb.d/
COPY ./getx1x2coords.sql /docker-entrypoint-initdb.d/
COPY ./isnumeric.sql /docker-entrypoint-initdb.d/
COPY ./temamattype.sql /docker-entrypoint-initdb.d/
COPY ./jenks_json.sql /docker-entrypoint-initdb.d/
COPY ./daleniushodge2r_json.sql /docker-entrypoint-initdb.d/
COPY ./cuantiles2r_json.sql /docker-entrypoint-initdb.d/
COPY ./kmedias_json.sql /docker-entrypoint-initdb.d/
COPY ./neir_json.sql /docker-entrypoint-initdb.d/
COPY ./initdb-postgis.sh /docker-entrypoint-initdb.d/10_postgis.sh