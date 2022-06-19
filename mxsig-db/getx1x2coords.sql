-- Function: getx1x2coords(geometry)

-- DROP FUNCTION getx1x2coords(geometry);

CREATE OR REPLACE FUNCTION getx1x2coords(geometry)
  RETURNS geomcoords AS
$BODY$
select cast(substring(cast(p[0] as varchar) from  $$([+-]?\d+\.\d+)$$) as numeric),
       cast(substring(cast(p[1] as varchar) from  $$([+-]?\d+\.\d+)$$) as numeric),
       astext($1) from box($1) as p
$BODY$
  LANGUAGE sql VOLATILE
  COST 100;
ALTER FUNCTION getx1x2coords(geometry)
  OWNER TO postgres;
GRANT EXECUTE ON FUNCTION getx1x2coords(geometry) TO postgres;