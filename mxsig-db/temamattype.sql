-- Type: temamattype

-- DROP TYPE temamattype;

CREATE TYPE temamattype AS
   (clausulawhen character varying,
    filtro character varying,
    tabla character varying,
    variable character varying);
ALTER TYPE temamattype
  OWNER TO postgres;
