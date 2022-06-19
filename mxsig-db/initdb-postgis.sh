#!/bin/bash

POSTGIS_SQL_PATH=`pg_config --sharedir`/usr/share/postgresql/11/contrib/postgis-2.5;
createdb -E UTF8 template_postgis;
createlang -d template_postgis plpgsql;
psql -d postgres -c "UPDATE pg_database SET datistemplate='true' \
  WHERE datname='template_postgis'"
psql -d template_postgis -f /usr/share/postgresql/11/contrib/postgis-2.5/postgis.sql -U postgres
psql -d template_postgis -f /usr/share/postgresql/11/contrib/postgis-2.5/spatial_ref_sys.sql -U postgres
psql -d template_postgis -f /usr/share/postgresql/11/contrib/postgis-2.5/legacy_minimal.sql -U postgres
psql -d template_postgis -f /usr/share/postgresql/11/contrib/postgis-2.5/legacy.sql -U postgres
psql -d template_postgis -f /usr/share/postgresql/11/contrib/postgis-2.5/postgis_comments.sql -U postgres

createdb mdm6data -U postgres --template=template_postgis --encoding=UTF8 --tablespace=pg_default --lc-collate=es_MX.UTF-8 --lc-ctype=es_MX.UTF-8
psql -d mdm6data -U postgres -c "CREATE EXTENSION plr"

psql -d mdm6data -U postgres -c "CREATE schema control;
CREATE schema datosestadisticos;
CREATE schema mdm;
GRANT ALL ON SCHEMA control TO postgres;
CREATE TABLE control.mibuffer
(
gid integer NOT NULL,
fechahora date,
the_geom geometry,
CONSTRAINT mibuffer_pki PRIMARY KEY (gid)
);
GRANT ALL ON TABLE control.mibuffer TO postgres;
CREATE SEQUENCE control.control_seq
INCREMENT 1
MINVALUE 1
MAXVALUE 9223372036854775807
START 1
CACHE 1;
GRANT ALL ON SEQUENCE control.control_seq TO postgres; "

psql -d mdm6data -U postgres -c "CREATE TABLE control.share
(
id serial NOT NULL,
json json,
CONSTRAINT pk_share PRIMARY KEY (id)
)
WITH (
OIDS=FALSE
);
GRANT ALL ON SEQUENCE control.control_seq TO postgres;"

psql -d mdm6data -U postgres -c "CREATE TABLE control.layer_stats
(
proyecto character varying(20),
sesion character varying,
capa character varying(50),
fecha time with time zone,
id serial NOT NULL,
ubicacion character varying,
tipo character varying,
fecha_anio timestamp with time zone DEFAULT now(),
CONSTRAINT layer_stats_pk PRIMARY KEY (id)
)
WITH (
OIDS=FALSE
);
ALTER TABLE control.layer_stats
OWNER TO postgres;
GRANT ALL ON SEQUENCE control.control_seq TO postgres;
"

psql -d mdm6data -U postgres -c "CREATE SCHEMA temas
GRANT ALL ON SCHEMA temas TO postgres;

CREATE TABLE temas.temamat
(
  id serial NOT NULL,
  definicion character varying,
  clausulawhen character varying,
  filtro character varying,
  signature character varying,
  tabla character varying,
  clausulawhenestratos character varying,
  proy character varying(50),
  variable character varying,
  CONSTRAINT temas_temace_pki PRIMARY KEY (id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE temas.temamat OWNER TO postgres;
GRANT ALL ON TABLE temas.temamat TO postgres;
"

psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/R_functions.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/temamattype.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/isnumeric.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/geomcoords.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/getlayertemamat_estatal.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/getlayertemamat_municipal.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/getx1x2coords.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/jenks_json.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/daleniushodge2r_json.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/cuantiles2r_json.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/kmedias_json.sql
psql -d mdm6data -U postgres -f /docker-entrypoint-initdb.d/neir_json.sql